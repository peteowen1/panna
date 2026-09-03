## Build a weekly EPR cache via the regression method (with league FE +
## opponent strength + cross-league EPV calibration + data-chosen decay),
## mirroring opta_psr_weekly.parquet's structure.
##
## RUN FROM THE PACKAGE ROOT (cwd = panna/) — all input/output paths below are
## cwd-relative, not script-relative. Moved here from debug/keep/ 2026-06-23 so
## it's version-controlled and can run in CI (epr-weekly-snapshot.yml); the old
## location was gitignored, which is why EPR had no automated rebuild.
## INCREMENTAL: reuses snapshots older than a 28-day buffer from the existing
## opta_epr_weekly.parquet and recomputes only the recent window (~3 min vs
## ~25 min full). Set EPR_FORCE_FULL_REBUILD=1 to recompute every snapshot.
##
## Method: calculate_epr_regression() — for each snapshot date, fit a weighted
## ridge regression
##     epv_p90 ~ β_player + α_league_season + γ × opp_def_rating
## with exponential time-decay weights. β_player is the EPR.
##
## Cross-league calibration: the regression keeps the league-season FE (β_player
## is "above league-season mean"), then a per-league network offset is END-ADDED
## (apply_epr_league_offsets) to place each league on a single Big-5-equivalent
## scale — consistent with PSR, and at full strength (not discounted by per-player
## ridge shrinkage). See section 3b. Because the offset is now additive, a future
## offset-ONLY change is a cheap re-apply; but THIS migration changes the whole
## application (FE kept, tier off, end-add) so it needs one full rebuild
## (EPR_FORCE_FULL_REBUILD=1) to re-fit every snapshot.
##
## Inputs: per-season game_logs.parquet files + opta_lineups (for opponents)
##         + cache-opta/team_season_strength.parquet (for opp_def_rating)
## Output: opta_epr_weekly.parquet — one row per (player_id × snapshot_date)
##
## Publish: this script now uploads the parquet to opta-latest itself via
## vb_publish() (section 6b) -- epr-weekly-snapshot.yml no longer has a
## separate `gh release upload` step. Set upload_epr <- FALSE before sourcing
## to skip publishing (e.g. local test runs).
##
## League offsets are computed IN-RUN (no cached input) via build_league_network()
## — the same same-season co-occurrence estimator PSR uses — run on offensive and
## defensive EPV (see section 3b). Migrated 2026-06-23 off the legacy
## compute_league_offsets() (which over-swung on thin-bridge leagues like MLS).

suppressPackageStartupMessages({
  library(data.table); library(arrow); library(Matrix); library(glmnet)
  devtools::load_all()
})

t_log <- function(msg) cat(sprintf("[%s] %s\n",
                                     format(Sys.time(), "%H:%M:%S"), msg))
t_log("===== build_epr_weekly (regression method) =====")

cache_dir <- "data-raw/cache-predictions-opta"

## --- 1. Load and stack ALL available game_logs ---
## Loads every game_logs_*.parquet in the cache (2015-2016 -> current, plus
## Brazilian Serie A) so the EPR weekly cache spans the full history — the
## match-prediction pipeline (step 02) needs historical EPR to attach to
## training matches, not just the recent window.
t0 <- Sys.time()
files <- list.files(cache_dir, pattern = "^game_logs_.*\\.parquet$",
                    full.names = TRUE)
gl <- rbindlist(lapply(files, read_parquet),
                use.names = TRUE, fill = TRUE)
gl[, match_date := as.Date(sub("Z$","", match_date))]
gl <- gl[!is.na(epv_offensive_adj) & !is.na(epv_defensive_adj),
         .(player_id, player_name, match_id, match_date, league, season,
            team_id, minutes_played = total_minutes,
            epv_offensive = epv_offensive_adj,
            epv_defensive = epv_defensive_adj)]
gl[, season_end_year := fifelse(month(match_date) >= 7L,
                                 year(match_date) + 1L,
                                 year(match_date))]
t_log(sprintf("loaded game_logs in %.1fs: %s rows, %s players, %s matches",
              as.numeric(Sys.time()-t0, units="secs"),
              format(nrow(gl), big.mark=","),
              format(uniqueN(gl$player_id), big.mark=","),
              format(uniqueN(gl$match_id), big.mark=",")))

## --- 2. Build match → opponent lookup (once for all snapshots) ---
t0 <- Sys.time()
lu <- as.data.table(read_parquet("../pannadata/data/opta/opta_lineups.parquet"))
match_teams <- unique(lu[, .(match_id, team_id, team_name)])
match_pairs <- match_teams[, {
  if (.N == 2L) list(team_id = team_id, opp_team_id = rev(team_id),
                       opp_team_name = rev(team_name))
  else list(team_id = team_id, opp_team_id = NA_character_,
             opp_team_name = NA_character_)
}, by = match_id]
gl <- match_pairs[gl, on = c("match_id","team_id")]
t_log(sprintf("opponent lookup + join in %.1fs (NAs: %d/%d)",
              as.numeric(Sys.time()-t0, units="secs"),
              sum(is.na(gl$opp_team_id)), nrow(gl)))

## --- 3. Join opp_def_rating from team_season_strength ---
t0 <- Sys.time()
ts <- as.data.table(read_parquet("data-raw/cache-opta/team_season_strength.parquet"))
## Since panna#224 the file is keyed on team_id, so the team_name remap this
## used to do is gone -- it was the source of ~0% match rates in MLS / Liga MX /
## Saudi / Argentina, and its inline season parser had the same calendar-year
## bug (it read "2025" as season_end_year 2025 only by luck of the branch order).
if (!"team_id" %in% names(ts)) {
  cli::cli_abort(paste(
    "team_season_strength.parquet has no {.field team_id} column -- it predates",
    "panna#224. Regenerate with",
    "{.path data-raw/player-ratings-opta/07c_team_season_strength.R}."))
}
stopifnot(!anyDuplicated(ts, by = c("team_id", "season_end_year")))

n_before <- nrow(gl)
gl <- merge(gl, ts[, .(team_id, season_end_year,
                         opp_def_rating = def_rating,
                         opp_ts_coverage = coverage)],
             by.x = c("opp_team_id","season_end_year"),
             by.y = c("team_id","season_end_year"), all.x = TRUE)
stopifnot(nrow(gl) == n_before)   # a duplicated key would row-multiply, not error
n_na <- sum(is.na(gl$opp_def_rating))
## Fill with the season median, not 0: the ratings are not centred on 0, and a
## constant fill is absorbed by the league-season FE -- which is precisely how
## the control went inert for eight competitions without anything failing.
if (n_na > 0) {
  med <- gl[, .(.med = median(opp_def_rating, na.rm = TRUE)), by = season_end_year]
  gl <- merge(gl, med, by = "season_end_year", all.x = TRUE)
  gl[is.na(opp_def_rating), opp_def_rating := .med]
  gl[, .med := NULL]
  gl[is.na(opp_def_rating), opp_def_rating := 0]   # season with no ratings at all
}
stopifnot(!anyNA(gl$opp_def_rating))
t_log(sprintf("opp_def_rating joined on team_id in %.1fs: %d unmatched (%.1f%%), median-filled",
              as.numeric(Sys.time()-t0, units="secs"),
              n_na, 100*n_na/nrow(gl)))

## --- 3b. Per-league EPV calibration offsets (co-occurrence NETWORK, END-ADD) ---
## Estimated with build_league_network() — the SAME same-season co-occurrence
## estimator PSR uses (PSR <- PSV network) — run separately on offensive and
## defensive EPV to produce offset_off / offset_def, Big-5-anchored.
##
## APPLICATION = END-ADD, consistent with PSR (apply_psr_league_offsets): we run
## calculate_epr_regression with the league-season FE KEPT (league_offsets=NULL),
## so β_player is "above its league-season mean", then ADD offset_off/offset_def
## to epr_off/epr_def per player's as-of-date primary league (apply_epr_league_
## offsets). We do NOT shift y inside the regression: the offset is a LEAGUE-level
## quantity and must apply at full strength, not be discounted by each player's
## ridge shrinkage (which shifting-y-then-shrinking-β does — it pulls low-sample
## weak-league players back toward the GLOBAL mean, defeating the purpose). End-add
## shrinks each player toward their own LEAGUE prior — correct, and additive so an
## offset-only change is a cheap re-apply (no full rebuild), like PSR.
##
## Replaces the legacy UCL-anchored compute_league_offsets(), which over-swung on
## thin-bridge leagues (MLS -0.346 vs network -0.102, burying the whole league;
## CAFCL -0.169 from a 7-player chain). build_league_network needs `total_minutes`;
## gl renamed it to minutes_played.
##
## FLAT vs quality-aware — settled empirically (2026-06-23, same-season mover study,
## ~10.6k EPV / 10.5k PSV movers, harmonic-mean-of-minutes weights):
##   * The dramatic "elites get discounted less" signal was ~90% regression-to-the-
##     mean artifact (regressing weak-strong on strong). Gone under a clean proxy.
##   * Mean inflation is rock-solid (+0.041 EPV) -> the flat offset MAGNITUDE is right.
##   * Gap-controlled independent-proxy slope = +0.15 (EPV, real flat-track-bully:
##     elites inflate weak-league EPV MORE) but small (~0.03 across the quality
##     range) and points toward discounting elites *more* -> flat is conservative,
##     not harsh. PSR slope ~0 (flat). So a FLAT offset is the right, non-overfit call.
## FUTURE POLISH (documented, low priority): (a) optional gentle flat-track-bully
## tilt for EPR (+~0.03 to top-quality); (b) rescale offsets ~1.1-1.2x — the gap
## coefficient was 1.10 (EPV)/1.20 (PSV), i.e. the network mildly under-estimates
## the true league gap.
gl[, total_minutes := minutes_played]
off_net <- build_league_network(gl, value_col = "epv_offensive", verbose = FALSE)
def_net <- build_league_network(gl, value_col = "epv_defensive", verbose = FALSE)
league_offsets <- merge(off_net[, .(league, offset_off = offset)],
                        def_net[, .(league, offset_def = offset)],
                        by = "league", all = TRUE)
league_offsets[is.na(offset_off), offset_off := 0]
league_offsets[is.na(offset_def), offset_def := 0]
league_offsets[, offset_tot := offset_off + offset_def]
t_log(sprintf("EPV-network league offsets: %d leagues (Big-5-anchored, end-add)", nrow(league_offsets)))
print(league_offsets[order(offset_tot), .(league,
                       offset_off = round(offset_off, 3),
                       offset_def = round(offset_def, 3),
                       offset_tot = round(offset_tot, 3))])

## Per-player as-of-date DECAY-WEIGHTED BLEND of league offsets.
## Each game contributes its league's offset, weighted the SAME way the regression
## weights it (decay = 900d × minutes), then end-added outside the regression so it
## survives at full strength (an in-regression per-game y-shift gets washed out by
## the ridge — tested: MLS mean -0.003 vs -0.10). This is game-level adjustment
## realized at full strength, and it handles mid-season movers correctly (e.g. a
## 60% MLS / 40% EPL window gets a blended discount, not the single primary league).
##
## Efficiency: w_g = exp(-(d - md_g)/decay)·min_g; the exp(-d/decay) factor is
## common to numerator and denominator so it CANCELS in the blend ratio. We
## precompute the per-game factor gfac = exp((md_g - max_md)/decay)·min_g once
## (max-date-shifted for numerical safety), and each snapshot is just a weighted
## sum over games before d. NA-offset leagues contribute offset 0.
DECAY_BLEND_DAYS <- 900L                      # match the EPR-regression decay
glb <- merge(gl[!is.na(league), .(player_id, league, match_date, minutes_played)],
             league_offsets[, .(league, offset_off, offset_def)], by = "league", all.x = TRUE)
glb[is.na(offset_off), offset_off := 0]
glb[is.na(offset_def), offset_def := 0]
.maxmd <- as.numeric(max(glb$match_date))
glb[, gfac := exp((as.numeric(match_date) - .maxmd) / DECAY_BLEND_DAYS) * minutes_played]
glb[, c("woff", "wdef") := .(gfac * offset_off, gfac * offset_def)]
data.table::setkey(glb, match_date)
.blend_offset_asof <- function(d) {
  hist <- glb[match_date < d]
  if (!nrow(hist)) return(NULL)
  hist[, .(offset_off = sum(woff) / sum(gfac),
           offset_def = sum(wdef) / sum(gfac)), by = player_id]
}

## --- 4. Define snapshot dates (full history, weekly) ---
## Weekly snapshots across the whole game_logs history. Step 02 of the match-
## prediction pipeline takes the latest snapshot within each season_end_year,
## so coverage must span every training season — mirroring opta_psr_weekly.
end_date   <- max(gl$match_date)
start_date <- min(gl$match_date) + 90L   # skip first ~3 months (thin history)
snapshots  <- seq(start_date, end_date, by = "week")
t_log(sprintf("Building %d weekly snapshots: %s -> %s",
              length(snapshots), start_date, end_date))

out_path <- "../pannadata/data/opta/opta_epr_weekly.parquet"

## --- 4b. Incremental: reuse snapshots older than a recompute buffer ----
## Each snapshot is an independent calculate_epr_regression() fit filtered to
## `match_date < ref_date` (verified leak-free), so a snapshot older than the
## buffer is fully determined by matches that already existed last run — it is
## immutable and can be reused verbatim. We recompute only the recent window
## (covers new matches + late-arriving results) and any brand-new snapshot
## dates the advancing end_date introduced. ~25 min full rebuild -> ~3 min.
## Set EPR_FORCE_FULL_REBUILD=1 to force all 1346 snapshots (e.g. after a
## method/offset change that shifts historical values).
RECOMPUTE_BUFFER_DAYS <- 28L
force_full_epr <- nzchar(Sys.getenv("EPR_FORCE_FULL_REBUILD"))
keep_existing <- NULL
if (force_full_epr) {
  t_log("EPR_FORCE_FULL_REBUILD set — recomputing every snapshot")
} else if (file.exists(out_path)) {
  existing_epr <- tryCatch(as.data.table(read_parquet(out_path)),
                           error = function(e) { t_log(sprintf(
                             "could not read existing parquet (%s) — full rebuild",
                             conditionMessage(e))); NULL })
  if (!is.null(existing_epr) && "snapshot_date" %in% names(existing_epr) &&
      nrow(existing_epr) > 0) {
    existing_epr[, snapshot_date := as.Date(snapshot_date)]
    cutoff <- end_date - RECOMPUTE_BUFFER_DAYS
    keep_existing <- existing_epr[snapshot_date < cutoff]
    snapshots <- snapshots[snapshots >= cutoff]
    t_log(sprintf("Incremental: reuse %s rows (<%s), recompute %d snapshots (>=%s)",
                  format(nrow(keep_existing), big.mark = ","), cutoff,
                  length(snapshots), cutoff))
  } else {
    t_log("existing parquet unusable (no snapshot_date / empty) — full rebuild")
  }
} else {
  t_log("no existing parquet — full rebuild")
}

## --- 5. Run calculate_epr_regression() at each snapshot ---
## Decay 900 was chosen by held-out MSE search in build_epr_regression.R.
## All decays produced near-identical MSE so this is robust.
DECAY <- 900L
PRIOR_STRENGTH <- 5
ALPHA <- 0   # pure ridge — keeps all players, no zeroing

t0 <- Sys.time()
all_snaps <- vector("list", length(snapshots))
for (i in seq_along(snapshots)) {
  d <- snapshots[i]
  # FE-mode (league_offsets = NULL keeps the league-season FE; tier off, since
  # the network offset replaces the coarse tier interaction). β_player is then
  # "above league-season mean"; we end-add the network offset below.
  res <- calculate_epr_regression(
    gl, ref_date = d, decay = DECAY,
    prior_strength = PRIOR_STRENGTH, alpha = ALPHA,
    tier_interaction = FALSE, league_offsets = NULL,
    verbose = FALSE
  )
  if (nrow(res) > 0) {
    res <- as.data.table(res)
    bl <- .blend_offset_asof(d)                # decay-weighted blend offset per player
    if (!is.null(bl)) {
      res <- merge(res, bl, by = "player_id", all.x = TRUE)
      res[is.na(offset_off), offset_off := 0]
      res[is.na(offset_def), offset_def := 0]
      res[, epr_offensive := epr_offensive + offset_off]     # end-add (full strength)
      res[, epr_defensive := epr_defensive + offset_def]
      res[, epr := epr_offensive + epr_defensive]            # preserve epr = off + def
      res[, c("offset_off", "offset_def") := NULL]           # keep the slim schema
    }
    res[, snapshot_date := d]
    all_snaps[[i]] <- res
  }
  if (i %% 10 == 0) {
    el <- as.numeric(Sys.time() - t0, units = "secs")
    rate <- el / i
    eta <- rate * (length(snapshots) - i)
    cat(sprintf("  snapshot %d/%d (%s) — %.0fs elapsed, ~%.0fs remaining\n",
                i, length(snapshots), d, el, eta))
  }
}
new_epr <- rbindlist(all_snaps, use.names = TRUE, fill = TRUE)
n_new_snaps <- if (nrow(new_epr) > 0) uniqueN(new_epr$snapshot_date) else 0L
if (n_new_snaps == 0L) {
  stop("All recomputed snapshots failed (0 produced) — refusing to publish. ",
       "Reused rows alone would stagnate the EPR cache.", call. = FALSE)
}

## Merge reused (immutable) rows with freshly recomputed ones. Dedup on
## (snapshot_date, player_id) keeping the new rows on any overlap.
if (!is.null(keep_existing) && nrow(keep_existing) > 0) {
  common <- intersect(names(keep_existing), names(new_epr))
  epr_weekly <- rbindlist(list(keep_existing[, ..common], new_epr[, ..common]),
                          use.names = TRUE, fill = TRUE)
  epr_weekly <- unique(epr_weekly, by = c("snapshot_date", "player_id"),
                       fromLast = TRUE)
  t_log(sprintf("Merged: %s reused + %s recomputed -> %s rows",
                format(nrow(keep_existing), big.mark = ","),
                format(nrow(new_epr), big.mark = ","),
                format(nrow(epr_weekly), big.mark = ",")))
} else {
  epr_weekly <- new_epr
}
setorder(epr_weekly, snapshot_date, player_id)
t_log(sprintf("Done fitting %d snapshots in %.1f min",
              length(snapshots),
              as.numeric(Sys.time()-t0, units="mins")))
t_log(sprintf("Output: %d rows across %d snapshots, %d unique players",
              nrow(epr_weekly), uniqueN(epr_weekly$snapshot_date),
              uniqueN(epr_weekly$player_id)))

## --- 6. Save ---
write_parquet(epr_weekly, out_path)
t_log(sprintf("Saved -> %s (%.1f MB)",
              out_path, file.size(out_path) / 1e6))

## --- 6b. Publish to opta-latest (manifest-gated) ---
## vb_publish() (R/versebus.R) replaces epr-weekly-snapshot.yml's separate
## `gh release upload` step: hashes first, uploads with bounded retries,
## verifies the live asset list, then writes bus_manifest.json last -- same
## publish path as 08b_export_psr_weekly.R / 13_publish_release_data.R,
## unified onto opta-latest (Pete's decision 2026-07-14). Upload toggle
## mirrors 08b/10b's upload_psr/upload_game_logs -- set upload_epr <- FALSE
## before sourcing to build the parquet locally without publishing.
if (!exists("upload_epr")) upload_epr <- TRUE
if (!isTRUE(upload_epr)) {
  t_log("upload_epr = FALSE -- wrote parquet locally, NOT publishing.")
} else {
  manifest <- vb_publish(out_path, repo = "peteowen1/pannadata", tag = "opta-latest",
                         rows = c(opta_epr_weekly.parquet = nrow(epr_weekly)))
  t_log(sprintf("Published -> opta-latest (generation %s)", manifest$generation))
}

## --- 6. Latest snapshot sanity check ---
latest <- max(epr_weekly$snapshot_date)
cat(sprintf("\nLatest snapshot: %s\n", latest))
top10 <- epr_weekly[snapshot_date == latest][order(-epr)][1:10]
cat("Top 10 by EPR:\n")
print(top10[, .(player_name,
                 epr = round(epr, 3),
                 epr_off = round(epr_offensive, 3),
                 epr_def = round(epr_defensive, 3),
                 wt_games = round(wt_games, 1),
                 n_games)])
