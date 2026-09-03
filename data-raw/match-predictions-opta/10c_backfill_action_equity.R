# 10c_backfill_action_equity.R
# Local backfill for per-season action_equity.parquet files.
#
# Runs 10c_export_equity.R over multiple historical seasons, producing
# action_equity_<season>.parquet for each. Designed to run locally.
#
# Usage (from panna/ directory):
#   Rscript data-raw/match-predictions-opta/10c_backfill_action_equity.R
#
# Or interactively:
#   devtools::load_all()
#   source("data-raw/match-predictions-opta/10c_backfill_action_equity.R")

# 1. Config ----

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

# Override-safe config (same pattern as 10b_backfill_game_logs.R).
if (!exists("equity_seasons", inherits = FALSE)) {
  equity_seasons <- c(
    "2015-2016", "2016-2017", "2017-2018", "2018-2019",
    "2019-2020", "2020-2021", "2021-2022", "2022-2023",
    "2023-2024", "2024-2025", "2025-2026"
  )
}

if (!exists("force_rebuild", inherits = FALSE))    force_rebuild <- FALSE
if (!exists("upload_equity"))    upload_equity <- TRUE
if (!exists("parallel_workers", inherits = FALSE)) parallel_workers <- 1L

cache_dir <- file.path("data-raw", "cache-predictions-opta")

# 2. Delegate to the shared backfill driver (pipeline_utils.R) ----

run_backfill(
  export_script    = "data-raw/match-predictions-opta/10c_export_equity.R",
  seasons          = equity_seasons,
  seasons_var      = "equity_seasons",
  upload_var       = "upload_equity",
  build_var        = "build_equity",
  out_pattern      = "action_equity_%s.parquet",
  cache_dir        = cache_dir,
  force_rebuild    = force_rebuild,
  upload           = upload_equity,
  parallel_workers = parallel_workers,
  extra_worker_globals = list(),
  label            = "Equity backfill"
)
