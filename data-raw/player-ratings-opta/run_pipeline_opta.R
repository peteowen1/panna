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
if (!exists("seasons")) seasons <- NULL

# MINIMUM SEASON (skip data before this season, NULL = no filter)
if (!exists("min_season")) min_season <- "2013-2014"

# ENRICH SPM WITH xMETRICS FEATURES (xG/xA/xPass per-90)
if (!exists("use_xmetrics_features")) use_xmetrics_features <- TRUE

# WHICH STEPS TO RUN
if (!exists("run_steps")) {
  run_steps <- list(
    step_01_load_data        = TRUE,
    step_02_data_processing  = TRUE,
    step_03_splint_creation  = TRUE,
    step_04_rapm             = TRUE,
    step_05_spm              = TRUE,
    step_06_xrapm            = TRUE,
    step_07_seasonal_ratings = TRUE,
    step_08_panna_ratings    = TRUE,
    step_09_export_ratings   = TRUE
  )
}

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh
if (!exists("force_rebuild_from")) force_rebuild_from <- NULL

# 3. Helper Functions ----

run_step <- function(step_name, step_num, code_block) {
  step_key <- sprintf("step_%02d_%s", step_num, step_name)
  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %d: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_num, step_name))
    return(NULL)
  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %d: %s",
                  format(Sys.time(), "%H:%M:%S"), step_num, step_name))
  message(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  start_time <- Sys.time()
  result <- tryCatch({
    code_block()
    "SUCCESS"
  }, error = function(e) {
    message(sprintf("ERROR: %s", e$message))
    "FAILED"
  })
  end_time <- Sys.time()

  duration <- difftime(end_time, start_time, units = "secs")

  list(
    step = step_num,
    name = step_name,
    status = result,
    duration_secs = as.numeric(duration),
    duration_formatted = format_duration(as.numeric(duration))
  )
}

# format_duration() is defined in R/utils.R

# 4. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache-opta")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Handle force rebuild
if (!is.null(force_rebuild_from) && force_rebuild_from >= 1 && force_rebuild_from <= 9) {
  cache_files <- list(
    "1" = c("01_raw_data.rds", "01_config.rds"),
    "2" = "02_processed_data.rds",
    "3" = "03_splints.rds",
    "4" = "04_rapm.rds",
    "5" = "05_spm.rds",
    "6" = "06_xrapm.rds",
    "7" = c("07_seasonal_ratings.rds", "seasonal_spm.csv", "seasonal_rapm.csv", "seasonal_xrapm.csv"),
    "8" = c("08_panna.rds", "panna_ratings.csv"),
    "9" = character(0)
  )

  files_to_delete <- unlist(cache_files[as.character(force_rebuild_from:9)])
  deleted <- 0
  for (f in files_to_delete) {
    fpath <- file.path(cache_dir, f)
    if (file.exists(fpath)) {
      file.remove(fpath)
      deleted <- deleted + 1
    }
  }
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %d onwards\n",
                  deleted, force_rebuild_from))
}

pipeline_start <- Sys.time()
step_results <- list()
pipeline_failed <- FALSE

# Check if a critical step failed and halt downstream steps
check_critical_step <- function(step_num, step_name) {
  result <- step_results[[step_num]]
  if (!is.null(result) && result$status == "FAILED") {
    pipeline_failed <<- TRUE
    message(sprintf("\nCRITICAL: Step %d (%s) failed — halting downstream steps.", step_num, step_name))
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
message(sprintf("#   Force rebuild from: %s",
                if (is.null(force_rebuild_from)) "None (use cache)" else force_rebuild_from))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 5. Step 1: Load Opta Data ----

step_results[[1]] <- run_step("load_data", 1, function() {
  source("data-raw/player-ratings-opta/01_load_opta_data.R", local = TRUE)
})

# 6. Step 2: Data Processing ----

step_results[[2]] <- run_step("data_processing", 2, function() {
  source("data-raw/player-ratings-opta/02_data_processing.R", local = TRUE)
})

# 7. Step 3: Splint Creation ----

step_results[[3]] <- run_step("splint_creation", 3, function() {
  source("data-raw/player-ratings-opta/03_splint_creation.R", local = TRUE)
})

# 8. Step 4: RAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[4]] <- run_step("rapm", 4, function() {
    source("data-raw/player-ratings-opta/04_rapm.R", local = TRUE)
  })
  check_critical_step(4, "rapm")
}

# 9. Step 5: SPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[5]] <- run_step("spm", 5, function() {
    source("data-raw/player-ratings-opta/05_spm.R", local = TRUE)
  })
  check_critical_step(5, "spm")
}

# 10. Step 6: xRAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[6]] <- run_step("xrapm", 6, function() {
    source("data-raw/player-ratings-opta/06_xrapm.R", local = TRUE)
  })
  check_critical_step(6, "xrapm")
}

# 11. Step 7: Seasonal Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[7]] <- run_step("seasonal_ratings", 7, function() {
    source("data-raw/player-ratings-opta/07_seasonal_ratings.R", local = TRUE)
  })
}

# 12. Step 8: Final Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[8]] <- run_step("panna_ratings", 8, function() {
    source("data-raw/player-ratings-opta/08_panna_ratings.R", local = TRUE)
  })
}

# 13. Step 9: Export Ratings ----

# Skip export if pipeline failed or step 7 failed
if (isTRUE(pipeline_failed) ||
    (!is.null(step_results[[7]]) && step_results[[7]]$status == "FAILED")) {
  message("\nSkipping export: upstream step failed")
  step_results[[9]] <- list(step = 9, name = "export_ratings", status = "SKIPPED",
                            duration_secs = 0, duration_formatted = "0.0 seconds")
} else {
  step_results[[9]] <- run_step("export_ratings", 9, function() {
    source("data-raw/player-ratings-opta/09_export_ratings.R", local = TRUE)
  })
}

# 14. Summary ----

pipeline_end <- Sys.time()
total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

message("\n")
message(paste(rep("=", 70), collapse = ""))
message("OPTA PIPELINE COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nStep Summary:")
message(sprintf("%-25s %-10s %s", "Step", "Status", "Duration"))
message(paste(rep("-", 50), collapse = ""))

for (result in step_results) {
  if (!is.null(result)) {
    message(sprintf("%-25s %-10s %s",
                    result$name,
                    result$status,
                    result$duration_formatted))
  }
}

message(paste(rep("-", 50), collapse = ""))
message(sprintf("%-25s %-10s %s", "TOTAL", "", format_duration(as.numeric(total_duration))))

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "08_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.csv")))
message("  - peteowen1/pannadata releases: seasonal_xrapm.parquet, seasonal_spm.parquet")

message("\nDone!")
