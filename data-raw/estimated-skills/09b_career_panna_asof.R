# 09b_career_panna_asof.R
# As-of-date career-trait Panna snapshots: one career-Panna fit per reference date,
# so the match-prediction model (and the blog) can look up "how good was this player
# AS OF date D" — the point-in-time trait, never the in-season (leaky) season xRAPM.
#
# Each snapshot is LEAK-FREE: splints are filtered to match_date <= D before the fit
# (fit_career_rapm only DECAYS by date, it does NOT filter — a future match would get
# a >1 decay weight, i.e. up-weighted; see fit_career_rapm() docs). Splints older than
# PRUNE_YEARS are dropped (decay weight 0.5^8 ~= 0.004 at 8yr — negligible + much
# faster, since it shrinks the largest recent design matrices).
#
# Lambda is set from the sample-size formula (lambda = 16.67 * n_obs^-0.58, validated
# on pruned snapshots in data-raw/debug/_validate_lambda_pruned.R) instead of re-running
# cv.glmnet for every date.
#
# Inputs: cache-opta/03_splints.rds, cache-skills/03_skill_spm.rds (fallback/burn-in),
# cache-skills/03_skill_spm_asof.rds (expanding-window, preferred), opta_fixtures.parquet.
# Output: career_panna_asof.parquet (player_id, ref_date, panna, panna_offense,
# panna_defense, total_minutes), optionally uploaded to the ratings-data release.
#
# PRIOR caveat (RESOLVED 2026-07 — FABLE-ASOF-EXPERIMENTS.md sec 4, the promotion of this
# file's own header caveat that H3 called for): skill_spm used to be the as-of-NOW
# career-trait prior for every snapshot — a second-order leak via the shrinkage target
# (the first-order result-leak, the match being in the training splints, was already
# removed by the date filter above). Each snapshot now picks the expanding-window
# skill-SPM for its own reference year (03_skill_spm.R section 12 / R/spm_asof.R:
# fit_expanding_skill_spm(), trained ONLY on seasons before that year) when
# 03_skill_spm_asof.rds is present, closing that second-order leak too. Falls back to
# the all-history skill_spm (hindsight) with a loud warning if the as-of file is
# missing, so an older/partial cache degrades visibly rather than silently.

library(arrow)
library(data.table)
devtools::load_all()

cache_opta   <- file.path("data-raw", "cache-opta")
cache_skills <- file.path("data-raw", "cache-skills")
opta_dir     <- file.path("..", "pannadata", "data", "opta")

# ---- Config (override by assigning before sourcing) -------------------------
granularity <- if (exists("asof_granularity", inherits = FALSE)) asof_granularity else "monthly"  # "monthly"|"weekly"|"yearly"
halflife    <- if (exists("panna_halflife_days", inherits = FALSE)) panna_halflife_days else 365L
prune_years <- if (exists("asof_prune_years", inherits = FALSE)) asof_prune_years else 8L
# Plain exists() — see 09_career_panna.R: inherits = FALSE can't see a
# driver-set global through source(local = TRUE), silently skipping the upload.
upload      <- if (exists("upload_career_panna_asof")) upload_career_panna_asof else FALSE
resume      <- if (exists("asof_resume", inherits = FALSE)) asof_resume else TRUE

# Sample-size lambda formula (validated on pruned data; see _validate_lambda_pruned.R).
lambda_formula <- if (exists("asof_lambda_formula", inherits = FALSE)) asof_lambda_formula else
  function(n) 16.67 * n^(-0.58)

prune_days <- as.integer(prune_years) * 365L
out_path   <- if (exists("asof_out_path", inherits = FALSE)) asof_out_path else
  file.path(opta_dir, "career_panna_asof.parquet")

# ---- Inputs -----------------------------------------------------------------
splints_path <- file.path(cache_opta, "03_splints.rds")
spm_path     <- file.path(cache_skills, "03_skill_spm.rds")
fx_path      <- file.path(opta_dir, "opta_fixtures.parquet")
for (p in c(splints_path, spm_path, fx_path))
  if (!file.exists(p)) stop("Missing required input: ", p)

cat("\n=== Loading inputs ===\n")
sd <- readRDS(splints_path)
sd <- filter_bad_xg_data(sd, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = FALSE)$splint_data
skill_spm <- readRDS(spm_path)

# Expanding-window skill-SPM (FABLE-ASOF-EXPERIMENTS.md sec 4): a list keyed
# by cutoff year, each trained ONLY on seasons before that year. Optional —
# older/partial caches (or a fresh clone that hasn't run 03_skill_spm.R's
# asof section) fall back to the all-history `skill_spm` above, loudly.
skill_spm_asof_path <- if (exists("skill_spm_asof_path", inherits = FALSE)) skill_spm_asof_path else
  file.path(cache_skills, "03_skill_spm_asof.rds")
skill_spm_asof <- if (file.exists(skill_spm_asof_path)) readRDS(skill_spm_asof_path) else NULL
if (is.null(skill_spm_asof) || length(skill_spm_asof) == 0) {
  warning(paste(
    "No expanding-window skill-SPM found at", skill_spm_asof_path, "-- every",
    "snapshot will use the ALL-HISTORY (hindsight-contaminated) skill_spm prior.",
    "Run 03_skill_spm.R's asof section first for point-in-time weights",
    "(FABLE-ASOF-EXPERIMENTS.md sec 4)."), call. = FALSE)
  asof_years <- integer(0)
} else {
  asof_years <- sort(as.integer(names(skill_spm_asof)))
  cat(sprintf("  expanding-window skill-SPM: %d cutoff years available (%d-%d)\n",
              length(asof_years), min(asof_years), max(asof_years)))
}

# Pick the as-of skill-SPM for reference date D: the model trained on seasons
# strictly before season_end_year(D) -- an honest, point-in-time prior. Dates
# before the earliest available cutoff year use that earliest model
# (first-K burn-in, sec 4) rather than the all-history fit; only falls back
# to the hindsight `skill_spm` if the asof list is entirely missing.
.pick_skill_spm_asof <- function(D) {
  if (length(asof_years) == 0) return(skill_spm)
  ref_year <- .season_end_year_for_date(D)
  candidates <- asof_years[asof_years <= ref_year]
  chosen <- if (length(candidates) > 0) max(candidates) else min(asof_years)
  skill_spm_asof[[as.character(chosen)]]
}

fx <- as.data.table(read_parquet(fx_path))[, .(match_id, match_date = as.Date(match_date))][!is.na(match_date)]
# Bound the grid by SPLINT coverage (the RAPM era ~2015+), NOT the full fixtures
# history — opta_fixtures contains historical matches back to ~2000 that have no
# splints, so fitting before splint coverage is empty/meaningless.
splint_dates <- fx[match_id %in% unique(sd$splints$match_id), match_date]
rng <- range(splint_dates)
cat(sprintf("  splint matches: %d | fixtures: %d | splint date range: %s to %s\n",
            data.table::uniqueN(sd$splints$match_id), nrow(fx),
            as.character(rng[1]), as.character(rng[2])))

# ---- Reference-date grid ----------------------------------------------------
# First sensible date needs ~200d of data; cap at one month past the last match.
seq_by <- switch(granularity, monthly = "month", weekly = "week", yearly = "year",
                 stop("granularity must be monthly/weekly/yearly"))
start  <- as.Date(cut(rng[1] + 200, "month"))
ref_dates <- seq(start, as.Date(cut(rng[2] + 31, "month")), by = seq_by)
ref_dates <- ref_dates[ref_dates >= rng[1] + 200 & ref_dates <= rng[2] + 31]
# Smoke-test hook: cap to the most recent N dates (set asof_date_limit before sourcing).
if (exists("asof_date_limit", inherits = FALSE) && is.finite(asof_date_limit))
  ref_dates <- tail(ref_dates, asof_date_limit)
cat(sprintf("  granularity: %s | reference dates: %d (%s .. %s) | prune: >%dyr\n",
            granularity, length(ref_dates), as.character(min(ref_dates)),
            as.character(max(ref_dates)), prune_years))

# ---- Resume: skip dates already present in a partial output -----------------
done <- character(0)
if (isTRUE(resume) && file.exists(out_path)) {
  prev <- as.data.table(read_parquet(out_path))
  done <- as.character(unique(prev$ref_date))
  cat(sprintf("  resume: %d snapshots already in %s — skipping those dates\n",
              length(done), basename(out_path)))
} else {
  prev <- NULL
}

# ---- Build snapshots --------------------------------------------------------
cat(sprintf("\n=== Fitting %d as-of snapshots (fixed-lambda, no CV) ===\n",
            sum(!as.character(ref_dates) %in% done)))
out <- list()
for (i in seq_along(ref_dates)) {
  D <- ref_dates[i]
  if (as.character(D) %in% done) next
  lo <- D - prune_days
  keep <- fx[match_date <= D & match_date > lo, match_id]   # leak-free + prune
  sd2 <- sd
  sd2$splints <- sd$splints[sd$splints$match_id %in% keep, , drop = FALSE]
  if (!is.null(sd$match_info) && "match_id" %in% names(sd$match_info))
    sd2$match_info <- sd$match_info[sd$match_info$match_id %in% keep, , drop = FALSE]

  snapshot_skill_spm <- .pick_skill_spm_asof(D)
  t0 <- Sys.time()
  res <- tryCatch(
    fit_career_rapm(sd2, fx, skill_spm = snapshot_skill_spm, halflife_days = halflife,
                    reference_date = D, min_minutes = MIN_MINUTES_RAPM_FIT,
                    lambda_formula = lambda_formula),
    error = function(e) { message("  FIT FAILED ", D, ": ", conditionMessage(e)); NULL })
  if (is.null(res)) next

  r <- as.data.table(res$ratings)[
    , .(player_id, ref_date = D, panna, panna_offense, panna_defense, total_minutes)]
  out[[as.character(D)]] <- r
  cat(sprintf("[%d/%d] %s: %d players (%d matches), skill_spm cutoff_year=%s, %.1f min\n",
              i, length(ref_dates), as.character(D), nrow(r), length(keep),
              if (length(asof_years) > 0) snapshot_skill_spm$cutoff_year %||% "?" else "all-history",
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

if (!length(out)) { cat("\nNothing new to write (all dates already done).\n"); quit(save = "no") }

allr <- rbindlist(out)
if (!is.null(prev)) allr <- rbindlist(list(prev, allr), use.names = TRUE, fill = TRUE)
setorder(allr, ref_date, -panna)
write_parquet(as.data.frame(allr), out_path)
cat(sprintf("\nSAVED %s: %d rows across %d snapshots (%.1f MB)\n",
            out_path, nrow(allr), data.table::uniqueN(allr$ref_date),
            file.info(out_path)$size / 1048576))

if (isTRUE(upload)) {
  piggyback::pb_upload(out_path, repo = "peteowen1/pannadata", tag = "ratings-data",
                       name = "career_panna_asof.parquet", overwrite = TRUE)
  cat("Uploaded career_panna_asof.parquet to ratings-data release.\n")
} else {
  cat("(upload skipped — set upload_career_panna_asof <- TRUE before sourcing to publish)\n")
}
cat("=== COMPLETE ===\n")
