# run_pipeline_opta.R
# Master script to run the Opta-based panna ratings pipeline
#
# Parallel pipeline to run_pipeline.R (FBref) using Opta data sources.
# Uses pre-trained xG/xPass models to score SPADL shots, then feeds
# into the same RAPM/SPM/xRAPM infrastructure.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----
# Use if (!exists(...)) so test scripts can override before sourcing

# LEAGUES TO INCLUDE
if (!exists("leagues")) leagues <- c(
  "ENG", "ESP", "GER", "ITA", "FRA",       # Big 5
  "NED", "POR", "TUR", "ENG2", "SCO",      # Extended domestic
  "UCL", "UEL", "UECL",                     # European comps
  "WC", "EURO"                               # International
)

# SEASONS (NULL = all available, or specify like c("2024-2025"))
# For incremental rebuilds, set to the changed season(s):
#   seasons <- c("2025-2026")  # Only rebuild current season
# Note: Steps 04+ (RAPM/SPM/xRAPM) run on ALL data regardless,
# since RAPM is cross-season by design. Only steps 01-03 benefit.
if (!exists("seasons")) seasons <- NULL

# MINIMUM SEASON (skip data before this season, NULL = no filter)
if (!exists("min_season")) min_season <- "2013-2014"

# ENRICH SPM WITH xMETRICS FEATURES (xG/xA/xPass per-90)
if (!exists("use_xmetrics_features")) use_xmetrics_features <- TRUE

# START FROM STEP (skip earlier steps that are already cached)
# Set before sourcing: start_step <- 3  # resume from splints
if (!exists("start_step")) start_step <- 1

# WHICH STEPS TO RUN (auto-populated from start_step)
if (!exists("run_steps")) {
  run_steps <- list(
    step_01_load_data        = start_step <= 1,
    step_02_data_processing  = start_step <= 2,
    step_03_splint_creation  = start_step <= 3,
    step_04_rapm             = start_step <= 4,
    step_05_spm              = start_step <= 5,
    step_06_xrapm            = start_step <= 6,
    step_07_seasonal_ratings = start_step <= 7,
    step_08_panna_ratings    = start_step <= 8,
    step_09_export_ratings   = start_step <= 9
  )
}

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh
if (!exists("force_rebuild_from")) force_rebuild_from <- NULL

# 3. Helper Functions ----

source("data-raw/pipeline_utils.R")

# Wrapper that passes run_steps from the pipeline environment.
# Forces a full GC after each step so the next step starts with clean memory.
# Without this, R's lazy GC leaves the previous step's local-frame variables
# (combined_lineups, raw_opta_data, processed_data, etc.) holding multi-GB of
# unreachable-but-allocated heap, which pushes step 3's readRDS over the
# 7GB ceiling on standard ubuntu-latest runners.
run_step_opta <- function(step_name, step_num, code_block) {
  result <- run_step(step_name, step_num, code_block, run_steps)
  gc(verbose = FALSE, full = TRUE)
  result
}

# 4. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache-opta")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Handle force rebuild
handle_force_rebuild(force_rebuild_from, cache_dir)

pipeline_start <- Sys.time()
step_results <- list()
pipeline_failed <- FALSE

# Wrapper that updates pipeline_failed in parent env
check_step <- function(step_num, step_name) {
  if (check_critical_step(step_num, step_name, step_results)) {
    pipeline_failed <<- TRUE
    return(TRUE)
  }
  FALSE
}

message("\n")
message(paste(rep("#", 70), collapse = ""))
message("#")
message("#   OPTA PANNA RATINGS PIPELINE")
message("#")
message(sprintf("#   Leagues: %s", paste(leagues, collapse = ", ")))
message(sprintf("#   Seasons: %s", if (is.null(seasons)) "All available" else paste(seasons, collapse = ", ")))
message(sprintf("#   Min season: %s", if (is.null(min_season)) "None" else min_season))
message(sprintf("#   xMetrics enrichment: %s", use_xmetrics_features))
message(sprintf("#   Start from step: %d", start_step))
message(sprintf("#   Force rebuild from: %s",
                if (is.null(force_rebuild_from)) "None (use cache)" else force_rebuild_from))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 5. Step 1: Load Opta Data ----

step_results[[1]] <- run_step_opta("load_data", 1, function() {
  source("data-raw/player-ratings-opta/01_load_opta_data.R", local = TRUE)
})
check_step(1, "load_data")

# 6. Step 2: Data Processing ----

if (!isTRUE(pipeline_failed)) {
  step_results[[2]] <- run_step_opta("data_processing", 2, function() {
    source("data-raw/player-ratings-opta/02_data_processing.R", local = TRUE)
  })
  check_step(2, "data_processing")
}

# 7. Step 3: Splint Creation ----

if (!isTRUE(pipeline_failed)) {
  step_results[[3]] <- run_step_opta("splint_creation", 3, function() {
    source("data-raw/player-ratings-opta/03_splint_creation.R", local = TRUE)
  })
  check_step(3, "splint_creation")
}

# 8. Step 4: RAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[4]] <- run_step_opta("rapm", 4, function() {
    source("data-raw/player-ratings-opta/04_rapm.R", local = TRUE)
  })
  check_step(4, "rapm")
}

# 9. Step 5: SPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[5]] <- run_step_opta("spm", 5, function() {
    source("data-raw/player-ratings-opta/05_spm.R", local = TRUE)
  })
  check_step(5, "spm")
}

# 10. Step 6: xRAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[6]] <- run_step_opta("xrapm", 6, function() {
    source("data-raw/player-ratings-opta/06_xrapm.R", local = TRUE)
  })
  check_step(6, "xrapm")
}

# 11. Step 7: Seasonal Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[7]] <- run_step_opta("seasonal_ratings", 7, function() {
    source("data-raw/player-ratings-opta/07_seasonal_ratings.R", local = TRUE)
  })
  check_step(7, "seasonal_ratings")
}

# 12. Step 8: Final Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[8]] <- run_step_opta("panna_ratings", 8, function() {
    source("data-raw/player-ratings-opta/08_panna_ratings.R", local = TRUE)
  })
  check_step(8, "panna_ratings")
}

# 13. Step 9: Export Ratings ----

# Skip export if pipeline failed
if (isTRUE(pipeline_failed)) {
  message("\nSkipping export: upstream step failed")
  step_results[[9]] <- list(step = 9, name = "export_ratings", status = "SKIPPED",
                            duration_secs = 0, duration_formatted = "0.0 seconds")
} else {
  step_results[[9]] <- run_step_opta("export_ratings", 9, function() {
    source("data-raw/player-ratings-opta/09_export_ratings.R", local = TRUE)
  })
}

# 14. Summary ----

print_pipeline_summary(step_results, pipeline_start, "OPTA PIPELINE")

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "08_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.csv")))
message("  - peteowen1/pannadata releases: seasonal_xrapm.parquet, seasonal_spm.parquet")

message("\nDone!")
