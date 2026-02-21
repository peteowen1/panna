# run_skills_pipeline.R
# Master script for the estimated skills pipeline
#
# Produces skill-based Panna ratings by replacing raw season averages
# with decay-weighted career skill estimates as SPM inputs.
#
# Prerequisites: Opta RAPM pipeline (run_pipeline_opta.R) must have been
# run first to generate cache-opta/03_splints.rds and 04_rapm.rds.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----
# Use if (!exists(...)) so test scripts can override before sourcing

if (!exists("leagues")) leagues <- c(
  "ENG", "ESP", "GER", "ITA", "FRA",
  "NED", "POR", "TUR", "ENG2", "SCO",
  "UCL", "UEL", "UECL",
  "WC", "EURO"
)

if (!exists("seasons")) seasons <- NULL
if (!exists("min_season")) min_season <- "2013-2014"
if (!exists("use_xmetrics_features")) use_xmetrics_features <- TRUE

# Which steps to run
if (!exists("n_cores")) n_cores <- 1  # Parallel cores for optimization

if (!exists("run_steps")) {
  run_steps <- list(
    step_01_compute_match_stats    = TRUE,
    step_02_estimate_skills        = TRUE,
    step_02b_optimize_params       = TRUE,   # Joint 2D optimization (prior + lambda), faster with n_cores
    step_03_skill_spm              = TRUE,
    step_04_skill_xrapm            = TRUE,
    step_05_skill_panna_ratings    = TRUE,
    step_06_seasonal_skill_ratings = TRUE     # Seasonal ratings for match predictions
  )
}

if (!exists("force_rebuild_from")) force_rebuild_from <- NULL

# 3. Helper Functions ----

run_step <- function(step_name, step_num, code_block) {
  step_key <- sprintf("step_%s_%s",
                       if (grepl("b$", as.character(step_num))) step_num
                       else sprintf("%02d", as.numeric(step_num)),
                       step_name)
  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %s: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_num, step_name))
    return(NULL)
  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %s: %s",
                  format(Sys.time(), "%H:%M:%S"), step_num, step_name))
  message(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  start_time <- Sys.time()
  if (pipeline_failed) {
    message("  SKIPPED (previous step failed)")
    return(list(step = step_num, name = step_name, status = "SKIPPED",
                duration_secs = 0, duration_formatted = "0.0 seconds"))
  }

  result <- tryCatch({
    code_block()
    "SUCCESS"
  }, error = function(e) {
    message(sprintf("ERROR: %s", e$message))
    pipeline_failed <<- TRUE
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

format_duration <- function(secs) {
  if (secs < 60) sprintf("%.1f seconds", secs)
  else if (secs < 3600) sprintf("%.1f minutes", secs / 60)
  else sprintf("%.1f hours", secs / 3600)
}

# 4. Initialize ----

cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Handle force rebuild
if (!is.null(force_rebuild_from)) {
  cache_files <- list(
    "1" = c("01_match_stats.rds", "01_config.rds"),
    "2" = "02_skill_features.rds",
    "2b" = "02b_decay_params.rds",
    "3" = "03_skill_spm.rds",
    "4" = "04_skill_xrapm.rds",
    "5" = c("05_skill_panna.rds", "skill_panna_ratings.csv"),
    "6" = c("06_seasonal_ratings.rds", "seasonal_skill_xrapm.csv")
  )

  steps_to_clear <- names(cache_files)
  # Keep only steps >= force_rebuild_from
  if (is.numeric(force_rebuild_from)) {
    # Compare numerically where possible
    numeric_steps <- suppressWarnings(as.numeric(steps_to_clear))
    steps_to_clear <- steps_to_clear[!is.na(numeric_steps) & numeric_steps >= force_rebuild_from]
  }

  files_to_delete <- unlist(cache_files[steps_to_clear])
  deleted <- 0
  for (f in files_to_delete) {
    fpath <- file.path(cache_dir, f)
    if (file.exists(fpath)) {
      file.remove(fpath)
      deleted <- deleted + 1
    }
  }
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %s onwards\n",
                  deleted, force_rebuild_from))
}

# Check prerequisites
opta_cache <- file.path("data-raw", "cache-opta")
required_files <- c("03_splints.rds", "04_rapm.rds")
missing <- required_files[!file.exists(file.path(opta_cache, required_files))]
if (length(missing) > 0) {
  stop(sprintf(
    "Missing Opta pipeline prerequisites: %s\nRun run_pipeline_opta.R first.",
    paste(missing, collapse = ", ")
  ))
}

pipeline_start <- Sys.time()
step_results <- list()
pipeline_failed <- FALSE

message("\n")
message(paste(rep("#", 70), collapse = ""))
message("#")
message("#   ESTIMATED SKILLS PIPELINE")
message("#")
message(sprintf("#   Leagues: %s", paste(leagues, collapse = ", ")))
message(sprintf("#   Seasons: %s", if (is.null(seasons)) "All available" else paste(seasons, collapse = ", ")))
message(sprintf("#   Min season: %s", if (is.null(min_season)) "None" else min_season))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 5. Step 1: Compute Match-Level Stats ----

step_results[[1]] <- run_step("compute_match_stats", 1, function() {
  source("data-raw/estimated-skills/01_compute_match_stats.R", local = TRUE)
})

# 6. Step 2: Estimate Skills ----

step_results[[2]] <- run_step("estimate_skills", 2, function() {
  source("data-raw/estimated-skills/02_estimate_skills.R", local = TRUE)
})

# 7. Step 2b: Optimize Params (optional) ----

step_results[[3]] <- run_step("optimize_params", "2b", function() {
  source("data-raw/estimated-skills/02b_optimize_params.R", local = TRUE)
})

# 8. Step 3: Skill SPM ----

step_results[[4]] <- run_step("skill_spm", 3, function() {
  source("data-raw/estimated-skills/03_skill_spm.R", local = TRUE)
})

# 9. Step 4: Skill xRAPM ----

step_results[[5]] <- run_step("skill_xrapm", 4, function() {
  source("data-raw/estimated-skills/04_skill_xrapm.R", local = TRUE)
})

# 10. Step 5: Skill Panna Ratings ----

step_results[[6]] <- run_step("skill_panna_ratings", 5, function() {
  source("data-raw/estimated-skills/05_skill_panna_ratings.R", local = TRUE)
})

# 11. Step 6: Seasonal Skill Ratings ----

step_results[[7]] <- run_step("seasonal_skill_ratings", 6, function() {
  source("data-raw/estimated-skills/06_seasonal_skill_ratings.R", local = TRUE)
})

# 12. Summary ----

pipeline_end <- Sys.time()
total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

message("\n")
message(paste(rep("=", 70), collapse = ""))
message("ESTIMATED SKILLS PIPELINE COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nStep Summary:")
message(sprintf("%-30s %-10s %s", "Step", "Status", "Duration"))
message(paste(rep("-", 55), collapse = ""))

for (result in step_results) {
  if (!is.null(result)) {
    message(sprintf("%-30s %-10s %s",
                    result$name, result$status, result$duration_formatted))
  }
}

message(paste(rep("-", 55), collapse = ""))
message(sprintf("%-30s %-10s %s", "TOTAL", "", format_duration(as.numeric(total_duration))))

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "05_skill_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "skill_panna_ratings.csv")))
message(sprintf("  - %s", file.path(cache_dir, "06_seasonal_ratings.rds")))

message("\nDone!")
