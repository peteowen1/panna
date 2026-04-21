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

# Seasons to backfill (2015-2016 onwards — matches xMetrics coverage).
# Override-safe: callers can set `game_log_seasons` before sourcing.
if (!exists("game_log_seasons", inherits = FALSE)) {
  game_log_seasons <- c(
    "2015-2016", "2016-2017", "2017-2018", "2018-2019",
    "2019-2020", "2020-2021", "2021-2022", "2022-2023",
    "2023-2024", "2024-2025", "2025-2026"
  )
}

# Skip seasons whose parquet already exists. Set `force_rebuild <- TRUE`
# before sourcing to regenerate everything (e.g. after column-schema changes).
if (!exists("force_rebuild", inherits = FALSE)) force_rebuild <- FALSE

# Upload to blog-latest release? Set FALSE for a dry run.
if (!exists("upload_game_logs", inherits = FALSE)) upload_game_logs <- TRUE

# Use the skill-adjusted SPM priors (TRUE) vs raw Opta xRAPM priors (FALSE)
if (!exists("use_skill_ratings", inherits = FALSE)) use_skill_ratings <- TRUE

# Paths
cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# 2. Filter out already-built seasons ----

if (!isTRUE(force_rebuild)) {
  existing <- vapply(game_log_seasons, function(s) {
    file.exists(file.path(cache_dir, sprintf("game_logs_%s.parquet", s)))
  }, logical(1))
  if (any(existing)) {
    message(sprintf("Skipping already-built seasons: %s",
                    paste(game_log_seasons[existing], collapse = ", ")))
    message("  (set force_rebuild <- TRUE to rebuild)")
    game_log_seasons <- game_log_seasons[!existing]
  }
}

if (length(game_log_seasons) == 0) {
  message("\nAll seasons already built. Nothing to do.")
  message("Set force_rebuild <- TRUE at the top of this script to rebuild.")
  return(invisible(NULL))
}

message(sprintf("\n=== Backfill plan ==="))
message(sprintf("  Seasons to build: %s", paste(game_log_seasons, collapse = ", ")))
message(sprintf("  Upload: %s", if (isTRUE(upload_game_logs)) "yes" else "no (dry run)"))
message(sprintf("  Cache dir: %s", cache_dir))

# Future optimisation: season-level parallelism.
#   The 11 seasons are fully independent. future.apply::future_lapply with
#   plan(multisession, workers = 4) would give ~3-4x wall-time reduction on
#   multi-core machines. Memory cost: each worker loads its own copy of
#   models + `all_match_stats` (~600 MB). Check free RAM before enabling.
#   Implementation sketch:
#     future::plan(future::multisession, workers = parallel_workers)
#     future.apply::future_lapply(game_log_seasons, function(s) {
#       devtools::load_all(".", quiet = TRUE)
#       game_log_seasons <- s
#       source("data-raw/match-predictions-opta/10b_export_game_logs.R",
#              local = TRUE)
#     })
#
# 3. Delegate to 10b ----

t0 <- Sys.time()
source("data-raw/match-predictions-opta/10b_export_game_logs.R", local = FALSE)
elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
message(sprintf("\nBackfill complete in %.1f min", elapsed))
