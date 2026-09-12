# 10b_backfill_game_logs.R
# Local backfill for per-season game_logs.parquet files.
#
# Runs 10b_export_game_logs.R over multiple historical seasons, producing
# game_logs_<season>.parquet for each. Designed to run locally — GHA runs
# the current season only on its weekly schedule.
#
# Usage (from panna/ directory):
#   Rscript data-raw/match-predictions-opta/10b_backfill_game_logs.R
#
# Or interactively:
#   devtools::load_all()
#   source("data-raw/match-predictions-opta/10b_backfill_game_logs.R")

# 1. Config ----

# Ensure panna is loaded (for load_opta_*, SPADL, EPV/WPA/PSV functions)
if (!"panna" %in% (.packages())) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(quiet = TRUE)
  } else {
    stop("devtools required. Run: install.packages('devtools')")
  }
}

# run_backfill() lives in pipeline_utils.R — source it if this is running
# standalone (outside run_predictions_opta.R, which sources it already).
if (!exists("run_backfill", mode = "function")) {
  source(file.path("data-raw", "pipeline_utils.R"))
}

# Seasons to backfill (2015-2016 onwards — matches xMetrics coverage).
# Override-safe: callers can set `game_log_seasons` before sourcing.
# envir=globalenv() hardening (2026-09-04, latent-risk finding after the
# upload_psr incident): this script is currently only ever invoked as a
# top-level entry point, never through an isolated source(local=TRUE), so
# these guards aren't live bugs today -- but matching the fix used
# elsewhere costs nothing and survives if that ever changes.
if (!exists("game_log_seasons", envir = globalenv(), inherits = FALSE)) {
  game_log_seasons <- c(
    "2015-2016", "2016-2017", "2017-2018", "2018-2019",
    "2019-2020", "2020-2021", "2021-2022", "2022-2023",
    "2023-2024", "2024-2025", "2025-2026"
  )
}

# Skip seasons whose parquet already exists. Set `force_rebuild <- TRUE`
# before sourcing to regenerate everything (e.g. after column-schema changes).
if (!exists("force_rebuild", envir = globalenv(), inherits = FALSE)) force_rebuild <- FALSE

# Upload to blog-latest release? Set FALSE for a dry run.
if (!exists("upload_game_logs")) upload_game_logs <- TRUE

# Use the skill-adjusted SPM priors (TRUE) vs raw Opta xRAPM priors (FALSE)
if (!exists("use_skill_ratings", envir = globalenv(), inherits = FALSE)) use_skill_ratings <- TRUE

# Season-level parallel workers. 1 = serial (safe default). Each worker
# duplicates the in-memory match_stats snapshot (~600 MB) + xgboost models,
# so 4 workers needs ~4 GB free RAM on top of the main process. On a 16 GB
# laptop, 2-4 workers is comfortable; tune based on free memory.
# Requires `future` and `future.apply` packages.
if (!exists("parallel_workers", envir = globalenv(), inherits = FALSE)) parallel_workers <- 1L

# Paths
cache_dir <- file.path("data-raw", "cache-predictions-opta")

# 2. Delegate to the shared backfill driver (pipeline_utils.R) ----
#
# Two paths inside run_backfill():
#   parallel_workers == 1  → single source of 10b (original behavior)
#   parallel_workers >  1  → workers build per-season in parallel (no upload),
#                            then main does a single alias + upload pass

run_backfill(
  export_script    = "data-raw/match-predictions-opta/10b_export_game_logs.R",
  seasons          = game_log_seasons,
  seasons_var      = "game_log_seasons",
  upload_var       = "upload_game_logs",
  build_var        = "build_game_logs",
  out_pattern      = "game_logs_%s.parquet",
  cache_dir        = cache_dir,
  force_rebuild    = force_rebuild,
  upload           = upload_game_logs,
  parallel_workers = parallel_workers,
  extra_worker_globals = list(use_skill_ratings = use_skill_ratings),
  label            = "Backfill"
)
