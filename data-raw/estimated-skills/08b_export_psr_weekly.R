# 08b_export_psr_weekly.R
# Compute and export weekly PSR snapshots for date-based player_psr() queries
#
# Generates skill estimates at weekly/monthly snapshot dates and applies PSR
# coefficients to produce opta_psr_weekly.parquet:
#   - Weekly snapshots for the last 2 years
#   - Monthly snapshots before that (back to 2015)
#
# Prerequisite: step 07 must have run (PSR coefficients in inst/extdata/)
#
# Outputs:
#   - pannadata/data/opta/opta_psr_weekly.parquet (uploaded to GitHub)

# 1. Setup ----

library(arrow)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-skills")
opta_dir  <- opta_data_dir()
dir.create(opta_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat(paste(rep("#", 70), collapse = ""), "\n")
cat("#  PSR WEEKLY SNAPSHOT EXPORT\n")
cat(paste(rep("#", 70), collapse = ""), "\n\n")

# 2. Check Prerequisites ----

coef_xg <- system.file("extdata", "psr_coefficients.csv", package = "panna")
coef_gd <- system.file("extdata", "gd_psr_coefficients.csv", package = "panna")
if (coef_xg == "" && coef_gd == "") {
  stop("PSR coefficients not found. Run step 07 (07_train_psr_model.R) first.")
}
psr_target <- if (coef_xg != "") "xg" else "goals"
# Within-position normalization (per-role skill means); display-only, BPM-style.
.psr_position_means <- if (exists("position_normalize") && !isTRUE(position_normalize)) {
  NULL
} else load_position_role_means()
if (psr_target == "goals") {
  cat("Note: xG coefficients not found, using goal-diff PSR instead.\n")
  cat("Re-run step 07 after fixing splint xG columns for xG-based PSR.\n\n")
}

slim_path <- file.path(cache_dir, "01_match_stats_slim.rds")
full_path  <- file.path(cache_dir, "01_match_stats.rds")
ms_path    <- if (file.exists(slim_path)) slim_path else full_path
if (!file.exists(ms_path)) {
  stop("Missing match stats cache. Run step 01 first.")
}

# 3. Load Data ----

cat("=== Loading Data ===\n")
cat(sprintf("  Match stats: %s\n", basename(ms_path)))
match_stats <- data.table::as.data.table(readRDS(ms_path))
if (!inherits(match_stats$match_date, "Date")) {
  match_stats[, match_date := as.Date(match_date)]
}
# Enrich with per-match xMetrics so the weekly PSR snapshots see the xG
# over-performance / gsaa skill features (same as steps 2/7). Without this the
# snapshot loop estimates skills from box-score-only stats and PSR is xG-blind.
match_stats <- enrich_match_stats_with_xmetrics(match_stats, verbose = FALSE)
gc(verbose = FALSE)
cat(sprintf("  Rows: %s | Date range: %s to %s\n",
            format(nrow(match_stats), big.mark = ","),
            min(match_stats$match_date),
            max(match_stats$match_date)))

decay_params_path <- file.path(cache_dir, "02b_decay_params.rds")
decay_params <- if (file.exists(decay_params_path)) {
  cat("  Using optimized decay params\n")
  readRDS(decay_params_path)
} else {
  cat("  Using default decay params\n")
  get_default_decay_params()
}

# 4. Define Snapshot Dates ----

cat("\n=== Defining Snapshot Dates ===\n")

today       <- Sys.Date()
min_history <- min(match_stats$match_date) + 365L  # Need 1yr history minimum
cutoff_weekly <- today - 2L * 365L                  # 2 years back = weekly

# Weekly for last 2 years, monthly (every 4 weeks) before that
recent_weekly  <- seq(cutoff_weekly, today, by = "7 days")
older_monthly  <- seq(min_history, cutoff_weekly - 1L, by = "28 days")
snapshot_dates <- sort(unique(c(older_monthly, recent_weekly)))
snapshot_dates <- snapshot_dates[snapshot_dates >= min_history]

cat(sprintf("  Total snapshot dates: %d (%s to %s)\n",
            length(snapshot_dates),
            min(snapshot_dates),
            max(snapshot_dates)))
cat(sprintf("  Weekly (last 2yr): %d | Monthly (older): %d\n",
            length(recent_weekly),
            length(older_monthly[older_monthly >= min_history])))

# --- Incremental update: reuse prior weekly parquet if available ----
#
# Most dates are stable week-to-week — re-computing every date on every weekly
# run is wasted work. Download the existing release parquet, keep rows for
# dates older than a "recompute buffer" (handles retroactive match updates),
# and compute only the missing dates.
#
# Failure policy: fall back to full rebuild ONLY when the asset doesn't exist
# yet (first run) or when PSR_FORCE_FULL_REBUILD=1 is set. Auth, network, or
# corruption errors halt the job — silently full-rebuilding on every failure
# mode would mask a persistent regression behind a 60-min elapsed-time delta.
message("\n=== Incremental Update Check ===")

recompute_buffer_days <- 28L   # Recompute the last 4 weeks (covers late results)
force_full_rebuild <- nzchar(Sys.getenv("PSR_FORCE_FULL_REBUILD"))

existing_parquet <- NULL
if (force_full_rebuild) {
  message("  PSR_FORCE_FULL_REBUILD set - skipping incremental check")
} else {
  # piggyback writes under the asset's original name, so download into a
  # dedicated subdir and read from <dir>/<asset>. Earlier versions of this
  # block referenced a non-existent suffix and silently skipped the fast path.
  dl_dir <- file.path(tempdir(), "psr_weekly_existing")
  dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)
  existing_path <- file.path(dl_dir, "opta_psr_weekly.parquet")
  if (file.exists(existing_path)) file.remove(existing_path)

  existing_parquet <- tryCatch({
    piggyback::pb_download(
      file = "opta_psr_weekly.parquet",
      repo = "peteowen1/pannadata",
      tag  = "opta-latest",
      dest = dl_dir,
      overwrite = TRUE
    )
    if (file.exists(existing_path)) {
      arrow::read_parquet(existing_path)
    } else {
      message("  Release exists but asset 'opta_psr_weekly.parquet' not in it - full rebuild")
      NULL
    }
  }, error = function(e) {
    msg <- conditionMessage(e)
    # piggyback surfaces missing assets with varied phrasings — accept any of
    # these as a legitimate "first run" signal and fall back silently.
    is_missing <- grepl("not found|404|no asset|unable to find",
                        msg, ignore.case = TRUE)
    if (is_missing) {
      message("  No existing parquet on release (first run) - full rebuild")
      return(NULL)
    }
    # Auth / network / corruption: fail loudly. Silent fallback here would
    # mask a 60-minute elapsed-time regression for days.
    stop(sprintf(
      "Failed to fetch existing opta_psr_weekly.parquet: %s\n  ",
      msg),
      "Refusing to silently fall back to full rebuild. ",
      "Set PSR_FORCE_FULL_REBUILD=1 to override if this is intentional.",
      call. = FALSE)
  })
}

keep_existing <- NULL
recompute_cutoff <- today - recompute_buffer_days
n_before <- length(snapshot_dates)
incremental_active <- FALSE

if (!is.null(existing_parquet) && "snapshot_date" %in% names(existing_parquet) &&
    nrow(existing_parquet) > 0) {
  existing_dt <- data.table::as.data.table(existing_parquet)
  existing_dt[, snapshot_date := as.Date(snapshot_date)]

  # Schema compatibility: all expected output columns must be present in the
  # existing parquet. Missing columns = schema drift, fall back with a loud
  # warning so the daily health check (which greps for Warning:) flags it.
  expected_cols <- c("snapshot_date", "player_id", "player_name",
                     "primary_position", "psr", "osr", "dsr", "weighted_90s")
  missing_cols <- setdiff(expected_cols, names(existing_dt))
  if (length(missing_cols) > 0) {
    warning(sprintf(
      "opta_psr_weekly.parquet schema mismatch: existing release missing [%s]. ",
      paste(missing_cols, collapse = ", ")),
      "Falling back to full rebuild. Investigate whether the schema changed ",
      "intentionally.", call. = FALSE)
  } else {
    existing_dates <- sort(unique(existing_dt$snapshot_date))
    # Keep rows strictly older than the recompute buffer
    keep_existing <- existing_dt[snapshot_date < recompute_cutoff]
    # Skip target snapshot_dates already covered by keep_existing
    already_covered <- snapshot_dates %in% keep_existing$snapshot_date
    snapshot_dates <- snapshot_dates[!already_covered]
    incremental_active <- TRUE

    message(sprintf("  Existing parquet: %s rows covering %d dates (%s to %s)",
                    format(nrow(existing_dt), big.mark = ","),
                    length(existing_dates),
                    min(existing_dates), max(existing_dates)))
    message(sprintf("  Recompute buffer: last %d days (dates >= %s)",
                    recompute_buffer_days, recompute_cutoff))
    message(sprintf("  Reused rows: %s (snapshot_date < %s)",
                    format(nrow(keep_existing), big.mark = ","),
                    recompute_cutoff))
    message(sprintf("  Dates to recompute: %d (down from %d)",
                    length(snapshot_dates), n_before))
  }
} else {
  message("  No valid existing data - computing all dates fresh")
}

# Safety net: the incremental filter may legitimately leave zero dates when
# everything is already covered outside the buffer. In that case, refresh
# today's snapshot so the release always has a current row. BUT if we had
# zero dates *before* incremental filtering that points to an upstream bug
# in snapshot date generation — refuse to publish rather than masking it.
if (length(snapshot_dates) == 0) {
  if (n_before == 0) {
    stop("snapshot_dates empty before incremental filtering - ",
         "check recent_weekly / older_monthly generation. ",
         "Refusing to publish a single-date parquet over the existing release.",
         call. = FALSE)
  }
  message("  All target dates already covered; refreshing today's snapshot only")
  snapshot_dates <- today
}

# 4b. Stream existing rows to disk to free RAM before the snapshot loop ----
#
# keep_existing is a 7-8M row data.table (~500MB). It's only consumed at the
# very end during the merge step, but holding it in memory for the entire
# snapshot loop steals headroom from per-iteration intermediates and trips
# the OOM ceiling on standard GHA runners. Stream it to a tempfile, free the
# in-memory reference, and read it back at merge time.
#
# Also capture metadata (column names, date count) needed for the schema-drift
# check and date-coverage assertion at the end so we can fully drop
# existing_parquet / existing_dt too.

existing_schema_cols <- NULL
existing_n_dates <- 0L

if (!is.null(keep_existing) && nrow(keep_existing) > 0) {
  existing_schema_cols <- names(keep_existing)
  if (!is.null(existing_parquet)) {
    existing_n_dates <- data.table::uniqueN(as.Date(existing_parquet$snapshot_date))
  }

  # Keep keep_existing in memory through the loop. Earlier versions streamed
  # it to a parquet file under cache_dir to free ~500 MB of RAM during the
  # snapshot loop, but the streamed file kept vanishing on GHA (v6 hit the
  # guard: file genuinely absent at merge time despite successful write —
  # leading-dot filename appears to be the trigger; visible-named chunks in
  # the same dir survived fine in v6). Now that the loop has gc(full=TRUE)
  # every iteration (added in v5), peak memory is bounded by ONE iter's
  # transients rather than 10 iters' worth — keep_existing's 500 MB fits
  # comfortably below the 7 GB ceiling without needing the disk round-trip.
  rm(existing_parquet)
  if (exists("existing_dt")) rm(existing_dt)
  gc(verbose = FALSE, full = TRUE)
  message(sprintf("  Holding keep_existing in memory: %s rows (~%s)",
                  format(nrow(keep_existing), big.mark = ","),
                  format(utils::object.size(keep_existing), units = "auto")))
}

# 5. Pre-Compute Shared Data (position multipliers + prior centers) ----
#
# Position multipliers and global prior centers change negligibly across dates
# when computed from the full dataset. Computing them once and passing via
# decay_params$position_multipliers / $prior_centers avoids re-running
# compute_position_multipliers() on every snapshot date (saves ~N_stats × 4
# which() calls × 231 dates = tens of thousands of vector scans).

cat("\n=== Pre-Computing Shared Priors ===\n")

# Auto-detect stat columns the same way estimate_player_skills() does
# Detect skill columns the SAME way the package estimator does. MUST catch
# `_per90` (the xG / finishing over-performance xMetrics + the 5 duel WOE), not
# just `_p90`, and union the registered PSR/GK skill lists. The old `_p90$`-only
# grep silently dropped every `_per90` feature from the weekly PSR — they were in
# the coefficient set but never estimated here. See psr.R
# .estimate_prematch_skills_batch (same `_p90$|_per90$` + skill-col union).
p90_cols   <- grep("_p90$|_per90$", names(match_stats), value = TRUE)
eff_cols   <- intersect(names(.classify_skill_stats()), names(match_stats))
reg_cols   <- tryCatch(union(.get_psr_skill_cols(), .get_gk_skill_cols()),
                       error = function(e) character(0))
stat_cols_all <- intersect(unique(c(p90_cols, eff_cols, reg_cols)), names(match_stats))
cat(sprintf("  Stat columns detected: %d\n", length(stat_cols_all)))

# Pre-sort by date so each date filter is a fast prefix scan
data.table::setorder(match_stats, match_date)
match_date_vec <- match_stats$match_date  # cached for binary search
cat(sprintf("  match_stats sorted by date\n"))

# --- Cross-league PSR offsets (transfer-graph calibration) ----
# PSR is built from box-score rates that barely vary by league, so strong
# players in weakly-connected leagues post inflated PSR. Step 06 estimates a
# per-league additive offset from player transfers (Big-5 anchored) and saves
# it; we load that table and add it so the weekly snapshots use the IDENTICAL
# calibration the seasonal/WC PSR uses. To look up each player's offset we
# attach their "current" league as of the snapshot date (max-minutes competition
# in the trailing window, falling back to all-time-before-d).
psr_offsets_path <- file.path(cache_dir, "psr_league_offsets.parquet")
psr_offsets <- if (file.exists(psr_offsets_path)) {
  arrow::read_parquet(psr_offsets_path)
} else NULL
psr_lg_col <- if ("competition" %in% names(match_stats)) "competition" else
              if ("league" %in% names(match_stats)) "league" else NULL
psr_apply_offsets <- !is.null(psr_offsets) && !is.null(psr_lg_col) &&
                     "total_minutes" %in% names(match_stats)
if (psr_apply_offsets) {
  # DECAY-WEIGHTED BLEND of league offsets (not a single primary league): each
  # game contributes its league's offset, weighted by PSR's OWN skill recency
  # (decay_params$rate, ~231-day half-life) so the blend matches how the smoothed
  # skills weight games. End-added per player below. This handles mid-season
  # movers correctly (a 60/40 split gets a blended discount, converging as the
  # window fills) instead of snapping to the single max-minutes league. Mirrors
  # the EPR decay-blend; PSR uses its shorter decay, not EPR's 900d.
  pl_src <- match_stats[!is.na(get(psr_lg_col)),
                        .(player_id, .lg = get(psr_lg_col),
                          match_date, total_minutes)]
  PSR_BLEND_LAMBDA <- decay_params$rate          # per-day; ~231d half-life
  blend_src <- merge(pl_src,
                     data.table::as.data.table(psr_offsets)[, .(.lg = league, .off = offset)],
                     by = ".lg", all.x = TRUE)
  blend_src[is.na(.off), .off := 0]              # leagues without an offset contribute 0
  .maxmd_psr <- as.numeric(max(blend_src$match_date))
  # gfac = exp(-lambda*(d - md))*mins; the exp(-lambda*d) common factor cancels in
  # the blend ratio, so precompute the max-date-shifted per-game weight once.
  blend_src[, gfac := exp(-PSR_BLEND_LAMBDA * (.maxmd_psr - as.numeric(match_date))) * total_minutes]
  blend_src[, woff := gfac * .off]
  data.table::setkey(blend_src, match_date)
  .blend_offset_asof <- function(d) {
    hist <- blend_src[match_date < d]
    if (!nrow(hist)) return(NULL)
    hist[, .(.boff = sum(woff) / sum(gfac)), by = player_id]
  }
  cat(sprintf("  PSR league offsets loaded (%d leagues); decay-weighted blend built (%s, lambda=%.4f ~ %d-day half-life)\n",
              nrow(psr_offsets), psr_lg_col, PSR_BLEND_LAMBDA, round(log(2) / PSR_BLEND_LAMBDA)))
} else {
  cat(sprintf("  NOTE: PSR league offsets %s -> not applied\n",
              if (is.null(psr_offsets)) "not found (run step 06 first)" else "disabled (no competition/minutes)"))
}

# Position multipliers from full dataset
pos_mults_precomp <- compute_position_multipliers(match_stats, stat_cols_all)
cat(sprintf("  Position multipliers computed\n"))

# Prior centers (minutes-weighted global mean per stat) from full dataset
wts_all   <- as.numeric(match_stats$total_minutes)
wts_all[is.na(wts_all)] <- 0
total_wt  <- sum(wts_all)
prior_centers_precomp <- vapply(stat_cols_all, function(sc) {
  v <- as.numeric(match_stats[[sc]])
  v[is.na(v)] <- 0
  if (total_wt > 0) sum(v * wts_all) / total_wt else 0
}, numeric(1))
cat(sprintf("  Prior centers computed\n\n"))

# Augment decay_params with pre-computed values so estimate_player_skills()
# skips recomputing these inside each loop iteration
decay_params_fast <- decay_params
decay_params_fast$position_multipliers <- pos_mults_precomp
decay_params_fast$prior_centers        <- prior_centers_precomp

# Pre-compute match_date exponentials: the decay weight
#   exp(-lam * (target_date - match_date))
# factors as
#   exp(lam * match_date) * exp(-lam * target_date)
# The first factor depends only on match_date (time-invariant across snapshot
# dates), so we compute it once per unique lambda and cache as columns on
# match_stats. Each snapshot iteration then replaces ~530K exp() calls per
# lambda with a single scalar multiply inside estimate_player_skills(). For
# full rebuilds (~200+ dates) this saves several minutes of exp() calls, but
# adds N_lambdas * N_rows * 8 bytes of columns to match_stats — on current
# data that's ~540MB, which OOM-kills the step on standard GHA runners when
# combined with the filtered copy `estimate_player_skills()` creates.
#
# For weekly incremental runs (~5 dates), the exp() savings are negligible
# (~5 seconds total) and not worth the memory. Threshold chosen so a normal
# weekly cron skips the precompute but a force-rebuild still gets the speed.
MEXP_PRECOMPUTE_MIN_DATES <- 50L

# Gate the precompute behind PSR_PRECOMPUTE_MEXP=1. The size-based threshold
# alone wasn't enough — a 100+ date catch-up run from a multi-day scrape outage
# triggers the precompute on a runner that can't fit it, OOMing at iteration 1.
# Default-off keeps GHA runs on the slow path (a few extra minutes of exp() calls,
# safely fits in 7GB). Local runs on bigger machines opt in for the speedup.
mexp_enabled <- nzchar(Sys.getenv("PSR_PRECOMPUTE_MEXP")) &&
                length(snapshot_dates) >= MEXP_PRECOMPUTE_MIN_DATES

if (mexp_enabled) {
  cat("\n=== Pre-Computing Decay Exponentials ===\n")
  mexp_start <- Sys.time()

  match_date_num <- as.numeric(match_stats$match_date)

  stat_lambdas_all <- vapply(stat_cols_all,
                             function(s) panna:::.resolve_lambda(s, decay_params_fast,
                                                                 panna:::.classify_skill_stats()),
                             numeric(1))
  all_lambdas <- unique(c(decay_params_fast$rate, stat_lambdas_all))
  all_lambdas <- all_lambdas[!is.na(all_lambdas)]

  precomputed_mexp <- list()
  for (j in seq_along(all_lambdas)) {
    lam <- all_lambdas[j]
    col_name <- sprintf(".mexp_%d", j)
    data.table::set(match_stats, j = col_name, value = exp(lam * match_date_num))
    precomputed_mexp[[as.character(lam)]] <- col_name
  }
  decay_params_fast$precomputed_match_exp <- precomputed_mexp

  cat(sprintf("  Cached exp() for %d unique lambdas in %.1fs\n",
              length(all_lambdas),
              as.numeric(difftime(Sys.time(), mexp_start, units = "secs"))))
} else {
  if (length(snapshot_dates) >= MEXP_PRECOMPUTE_MIN_DATES) {
    cat(sprintf("\n  Skipping mexp precompute (%d dates >= threshold %d, but PSR_PRECOMPUTE_MEXP not set). Set env var to enable; needs ~540MB extra RAM.\n",
                length(snapshot_dates), MEXP_PRECOMPUTE_MIN_DATES))
  } else {
    cat(sprintf("\n  Skipping mexp precompute (%d dates < threshold %d) — slow path is cheap at this size\n",
                length(snapshot_dates), MEXP_PRECOMPUTE_MIN_DATES))
  }
}

# 6. Compute PSR at Each Snapshot Date ----

cat("=== Computing PSR Snapshots ===\n\n")

# Stream each iteration's result to a per-date parquet file, then rbind at the
# end via arrow. Replaces an in-memory `psr_list` that retained every iteration
# until the end; combined with `rm()` of transients every iteration and a
# periodic `gc(full = TRUE)` every 10th iteration, this bounds *loop-retained
# growth* to ~1 iteration's worth (the fixed ~1.7GB baseline of `match_stats`
# + `precomputed_mexp` still persists across the loop by design). Fixes the
# persistent OOM documented in panna#63, which hit at ~date 1/105 when the
# recompute set grew well past the originally-assumed handful of dates.
# Streaming chunks. Three prior fix attempts failed:
#   v1: tempfile() under /tmp — disappeared mid-loop.
#   v2: dotfile dir under cache_dir — ALSO disappeared.
#   v3: non-dotfile dir under cache_dir, with diagnostic guards — confirmed
#       cwd is correct AND cache_dir parent exists, but the chunks SUBDIR
#       still gets wiped between dir.create and iter 1 of the loop.
#       Mechanism: still unconfirmed, but specifically targets subdirs.
# This version: don't create a subdirectory at all. Stream chunks as
# flat-named files directly under cache_dir — same level as
# psr_existing_chunk.parquet, which has proven to survive across the loop.
# Cleanup uses a name pattern instead of recursive unlink.
# Chunks named without leading dot. v4/v5 used ".psr_chunk_NNNNNN.parquet"
# but list.files() defaults to all.files=FALSE which silently skips hidden
# files — chunks were written successfully but the merge step's list.files
# returned 0 entries. The "vanishing files" we'd been chasing for 3 iterations
# was actually just our own filter omitting them. Drop the leading dot.
psr_chunk_prefix <- file.path(cache_dir, "psr_chunk_")
.cleanup_psr_chunks <- function() {
  files <- list.files(cache_dir,
                       pattern = "^psr_chunk_\\d+\\.parquet$",
                       full.names = TRUE)
  if (length(files) > 0) unlink(files, force = TRUE)
}
.cleanup_psr_chunks()  # remove residue from a prior crashed run, if any
on.exit(.cleanup_psr_chunks(), add = TRUE)
message(sprintf("  Streaming per-iteration chunks as flat files: %s*.parquet",
                psr_chunk_prefix))

n_success <- 0L
start_time <- Sys.time()

for (i in seq_along(snapshot_dates)) {
  d <- snapshot_dates[i]

  if (i %% 50 == 0 || i == 1L) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    rate    <- if (i > 1) elapsed / (i - 1) else 0
    eta_min <- round(rate * (length(snapshot_dates) - i) / 60, 1)
    cat(sprintf("  [%d/%d] %s  (%.0fs elapsed, ~%.1f min remaining)\n",
                i, length(snapshot_dates), d, elapsed, eta_min))
  }

  # Early-out if no history exists for this date. We used to pre-slice
  # match_stats with seq_len(cutoff) as a "fast prefix filter", but that copy
  # runs alongside `estimate_player_skills()`'s own internal
  # `dt[md < target_date]` filter — two near-full-size data.table copies live
  # simultaneously at ~1.7GB each. The internal filter alone is fast enough;
  # skipping the pre-slice drops peak memory by ~1.7GB.
  cutoff <- findInterval(as.numeric(d) - 1L, as.numeric(match_date_vec))
  if (cutoff < 1L) next

  skills <- tryCatch(
    estimate_player_skills(
      match_stats  = match_stats,
      decay_params = decay_params_fast,
      target_date  = d,
      min_weighted_90s = 3
    ),
    error = function(e) {
      cat(sprintf("  WARN: skills failed for %s: %s\n", d, e$message))
      NULL
    }
  )
  if (is.null(skills) || nrow(skills) == 0) { rm(skills); next }

  psr <- tryCatch(
    compute_player_psr(skills, center = TRUE, target = psr_target,
                       position_means = .psr_position_means),
    error = function(e) {
      cat(sprintf("  WARN: PSR failed for %s: %s\n", d, e$message))
      NULL
    }
  )
  if (is.null(psr) || nrow(psr) == 0) { rm(skills, psr); next }

  # Cross-league calibration: end-add the decay-weighted blend of league offsets
  # (full strength, split osr/dsr to preserve osr+dsr=psr) so weak-league players
  # aren't ranked globally and mid-season movers blend across leagues.
  if (psr_apply_offsets) {
    psr <- data.table::as.data.table(psr)
    bl <- .blend_offset_asof(d)
    if (!is.null(bl)) {
      psr <- merge(psr, bl, by = "player_id", all.x = TRUE)
      psr[is.na(.boff), .boff := 0]
      psr[, psr := psr + .boff]
      if (all(c("osr", "dsr") %in% names(psr))) {
        psr[, osr := osr + .boff / 2]
        psr[, dsr := dsr + .boff / 2]
      }
      psr[, .boff := NULL]
    }
  }

  psr[, snapshot_date := d]
  psr_slim <- psr[, .(snapshot_date, player_id, player_name,
                       primary_position, psr, osr, dsr, weighted_90s)]
  arrow::write_parquet(
    as.data.frame(psr_slim),
    paste0(psr_chunk_prefix, sprintf("%06d.parquet", i))
  )

  rm(skills, psr, psr_slim)
  # GC every iteration. The previous "every 10" cadence let ~10 iters of
  # transient allocations accumulate as unreachable-but-uncollected heap
  # — over 80+ iters this cumulatively pushed past 7GB on standard
  # GHA runners (v4 cancelled at iter 84/106). Per-iter cost is ~200ms
  # on a 2GB heap; ~20s total across 100 dates is a fair trade for
  # bounded memory.
  gc(verbose = FALSE, full = TRUE)
  n_success <- n_success + 1L
}

total_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("\nCompleted: %d / %d dates (%.0fs, %.1f sec/date)\n",
            n_success, length(snapshot_dates),
            total_secs, total_secs / max(n_success, 1)))

# Guard: always abort if ALL new dates failed. Having reusable existing rows
# is NOT a reason to continue — an incremental run that computes nothing new
# but re-publishes stale reused rows would silently stagnate the release.
if (n_success == 0) {
  stop("All snapshot dates failed - refusing to publish. ",
       "Reused rows alone would stagnate the release.", call. = FALSE)
}
# Warn on ANY failure (not just >50%). On incremental runs with 1-3 new
# dates, a single failure is a 33-100% failure rate and the old threshold
# logic would miss it entirely.
n_failed <- length(snapshot_dates) - n_success
if (n_failed > 0) {
  warning(sprintf("%d / %d snapshot dates failed. Investigate before next run.",
                  n_failed, length(snapshot_dates)), call. = FALSE)
}

# 7. Combine and Export ----

cat("\n=== Exporting ===\n")

# Read back all streamed per-date chunks. v4 used lapply() then rbindlist
# which holds 2x the total chunk bytes briefly (the list and the bound
# result both alive). With 106 chunks × ~5-10 MB plus existing residual
# memory from the loop, that pushed us over the 7GB ceiling — manifested
# as "operation cancelled" 15 sec after the loop's last iteration.
#
# Use arrow::open_dataset() instead, which lazy-reads the chunk files
# and materializes them in a single pass without holding all individual
# data.frames in memory simultaneously.
chunk_files <- list.files(cache_dir,
                          pattern = "^psr_chunk_\\d+\\.parquet$",
                          full.names = TRUE)
if (length(chunk_files) == 0) {
  stop("No PSR snapshot chunks were written - refusing to publish.",
       call. = FALSE)
}

# Force a full GC at the loop boundary so the merge starts clean.
gc(verbose = FALSE, full = TRUE)

new_psr <- data.table::as.data.table(
  arrow::open_dataset(chunk_files, format = "parquet") |>
    dplyr::collect()
)
message(sprintf("  Newly computed: %s rows across %d dates",
                format(nrow(new_psr), big.mark = ","),
                data.table::uniqueN(new_psr$snapshot_date)))

# Merge with reused rows from existing parquet (if any). keep_existing has
# been held in memory throughout the loop — was previously streamed to a
# parquet file to save RAM, but that file kept vanishing on GHA (mechanism
# unclear — see comments at the streaming-removed block above).
if (!is.null(keep_existing) && nrow(keep_existing) > 0) {
  # Drift check uses captured schema from start of script.
  added_cols   <- setdiff(names(new_psr), existing_schema_cols)
  dropped_cols <- setdiff(existing_schema_cols, names(new_psr))
  if (length(added_cols) > 0 || length(dropped_cols) > 0) {
    warning(sprintf(
      "Column drift between new and existing parquet (added: [%s], removed: [%s]). ",
      paste(added_cols,   collapse = ", "),
      paste(dropped_cols, collapse = ", ")),
      "Publishing new_psr only - rerun with PSR_FORCE_FULL_REBUILD=1 to ",
      "rebuild full history under the new schema.",
      call. = FALSE)
    weekly_psr <- new_psr
  } else {
    common_cols <- names(new_psr)
    weekly_psr <- data.table::rbindlist(
      list(keep_existing[, ..common_cols], new_psr[, ..common_cols]),
      use.names = TRUE, fill = TRUE
    )
    message(sprintf("  Reused rows:    %s rows from existing parquet",
                    format(nrow(keep_existing), big.mark = ",")))
    rm(keep_existing)
    gc(verbose = FALSE, full = TRUE)
  }
} else {
  weekly_psr <- new_psr
}

# Deduplicate on snapshot_date + player_id, keeping the newly computed rows.
# Since new rows are appended after keep_existing, fromLast=TRUE keeps the new
# version on any collision. The incremental filter already prevents overlap,
# so this is a safety net.
weekly_psr <- unique(weekly_psr, by = c("snapshot_date", "player_id"), fromLast = TRUE)
data.table::setorder(weekly_psr, snapshot_date, player_id)

# Hard assertions before we overwrite the release asset. A 0-row publish or
# a coverage regression is never acceptable — better to fail loudly than to
# silently break downstream consumers.
if (nrow(weekly_psr) == 0) {
  stop("Refusing to publish a 0-row opta_psr_weekly.parquet. ",
       "Both new computation and reused rows came back empty.",
       call. = FALSE)
}
if (incremental_active && existing_n_dates > 0L) {
  n_new_dates <- data.table::uniqueN(weekly_psr$snapshot_date)
  if (n_new_dates < existing_n_dates) {
    warning(sprintf(
      "Published snapshot date count decreased (%d -> %d). ",
      existing_n_dates, n_new_dates),
      "This should not happen on a normal incremental run - investigate ",
      "before trusting the release.", call. = FALSE)
  }
}

cat(sprintf("  Total rows:    %s\n", format(nrow(weekly_psr), big.mark = ",")))
cat(sprintf("  Unique players: %s\n", format(data.table::uniqueN(weekly_psr$player_id), big.mark = ",")))
cat(sprintf("  Unique dates:   %d\n", data.table::uniqueN(weekly_psr$snapshot_date)))

out_path <- file.path(opta_dir, "opta_psr_weekly.parquet")
arrow::write_parquet(as.data.frame(weekly_psr), out_path)
mb <- round(file.info(out_path)$size / 1024^2, 1)
cat(sprintf("  Written: %s (%s MB)\n", out_path, mb))

# 8. Upload to GitHub Release ----

cat("\n=== Uploading to GitHub Release ===\n")

repo <- "peteowen1/pannadata"
tag  <- "opta-latest"

# Upload toggle — set upload_psr <- FALSE before sourcing to generate the parquet
# locally for validation WITHOUT publishing to the live release (mirrors 10b's
# upload_game_logs). Default TRUE preserves the scheduled-workflow behaviour.
if (!exists("upload_psr", inherits = FALSE)) upload_psr <- TRUE

if (!isTRUE(upload_psr)) {
  cat(sprintf("\n  upload_psr = FALSE — wrote %s locally, NOT publishing.\n", out_path))
} else {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' required for upload.")
  }
  tryCatch({
    piggyback::pb_upload(file = out_path, repo = repo, tag = tag, overwrite = TRUE)
    cat(sprintf("  Uploaded opta_psr_weekly.parquet to %s (%s)\n", repo, tag))
  }, error = function(e) {
    stop(sprintf("Upload failed: %s. Weekly PSR not published.", e$message))
  })
}

cat("\n=== COMPLETE ===\n")
cat(sprintf("  opta_psr_weekly.parquet uploaded (%s MB, %d dates)\n",
            mb, data.table::uniqueN(weekly_psr$snapshot_date)))
