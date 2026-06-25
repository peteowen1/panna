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

# Must match run_pipeline_opta.R's league set so skills coverage tracks the
# rated pool — otherwise box-score stats (opta_skills.parquet) miss whole
# competitions the model rates (A_League/CAF_CL/Belgian/etc.).
# Canonical rating/display set, shared with step 03 / RAPM / 10b (constants.R).
if (!exists("leagues")) leagues <- PANNA_RATING_LEAGUES

if (!exists("seasons")) seasons <- NULL
if (!exists("min_season")) min_season <- "2013-2014"
if (!exists("use_xmetrics_features")) use_xmetrics_features <- TRUE

# Skills = continuous career trait, NOT season-gated (CLAUDE_TODO_CONTINUOUS_SKILLS.md).
# Gate on CAREER sample (decay-weighted weighted_90s), not per-season minutes:
# drops the old 450-min publishing filter that hid players with rich history but
# low current-season minutes (e.g. F. Chiesa). Consumed by step 02 (feeds
# skill-SPM/PSR + export). Coverage of the rated pool: 80% -> 85.6%.
if (!exists("min_minutes_spm")) min_minutes_spm <- 0     # per-season minutes floor removed (was 450)
if (!exists("min_career_w90")) min_career_w90 <- 3       # career-sample INCLUSION gate (decay-weighted 90s)

# Which steps to run
if (!exists("n_cores")) n_cores <- 1  # Parallel cores for optimization

# START FROM STEP (skip earlier steps that are already cached)
# Set before sourcing: start_step <- 3  # resume from skill SPM
# Use "2b" or 2.5 for the optimize step
if (!exists("start_step")) start_step <- 1

# Normalize lettered steps to numeric for comparison: "2b" -> 2.5, "8b" -> 8.5
start_num <- switch(as.character(start_step),
  "2b" = 2.5,
  "8b" = 8.5,
  as.numeric(start_step)
)

if (!exists("run_steps")) {
  run_steps <- list(
    step_01_compute_match_stats    = start_num <= 1,
    step_02_estimate_skills        = start_num <= 2,
    step_02b_optimize_params       = start_num <= 2.5,
    step_03_skill_spm              = start_num <= 3,
    step_04_skill_xrapm            = start_num <= 4,
    step_05_skill_panna_ratings    = start_num <= 5,
    step_06_seasonal_skill_ratings = start_num <= 6,
    step_07_train_psr_model        = start_num <= 7,
    step_08_export_skills          = start_num <= 8,
    step_08b_export_psr_weekly     = start_num <= 8.5,
    step_09_career_panna           = start_num <= 9
  )
}

if (!exists("force_rebuild_from")) force_rebuild_from <- NULL

# 3. Shared Pipeline Utilities ----

source("data-raw/pipeline_utils.R")

# Wrapper that passes run_steps and pipeline_failed from this scope
run_skills_step <- function(step_name, step_num, code_block) {
  result <- run_step(step_name, step_num, code_block, run_steps, pipeline_failed)
  if (!is.null(result) && identical(result$status, "FAILED")) {
    pipeline_failed <<- TRUE
  }
  result
}

# 4. Initialize ----

cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Handle force rebuild
skills_cache_files <- list(
  "1" = c("01_match_stats.rds", "01_config.rds"),
  "2" = "02_skill_features.rds",
  "2b" = "02b_decay_params.rds",
  "3" = "03_skill_spm.rds",
  "4" = "04_skill_xrapm.rds",
  "5" = c("05_skill_panna.rds", "skill_panna_ratings.csv"),
  "6" = c("06_seasonal_ratings.rds", "seasonal_skill_xrapm.csv"),
  "7" = "07_psr_model.rds",
  "8" = character(0),
  "8b" = character(0),
  "9" = character(0)
)
clear_cache_files(force_rebuild_from, cache_dir, skills_cache_files, max_step = 8)

# Check prerequisites (only if steps that need them are enabled)
opta_cache <- file.path("data-raw", "cache-opta")
needs_opta_cache <- any(vapply(c("step_03_skill_spm", "step_04_skill_xrapm",
                                  "step_05_skill_panna_ratings",
                                  "step_06_seasonal_skill_ratings"),
                                function(s) isTRUE(run_steps[[s]]), logical(1)))
if (needs_opta_cache) {
  required_files <- c("03_splints.rds", "04_rapm.rds")
  missing <- required_files[!file.exists(file.path(opta_cache, required_files))]
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing Opta pipeline prerequisites: %s\nRun run_pipeline_opta.R first.",
      paste(missing, collapse = ", ")
    ))
  }
  # Check freshness of Opta caches (warn if >2 weeks old)
  for (f in required_files) {
    meta_path <- paste0(file.path(opta_cache, f), ".meta.json")
    if (file.exists(meta_path)) {
      meta <- jsonlite::fromJSON(meta_path)
      if (!is.null(meta$written_at)) {
        written <- as.POSIXct(meta$written_at, format = "%Y-%m-%dT%H:%M:%S%z")
        age_days <- as.numeric(difftime(Sys.time(), written, units = "days"))
        if (!is.na(age_days) && age_days > 14) {
          warning(sprintf("Opta cache %s is %.0f days old. Consider re-running Opta pipeline.", f, age_days))
        }
      }
    }
  }
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
message(sprintf("#   Start from step: %s", start_step))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 5. Step 1: Compute Match-Level Stats ----

step_results[[1]] <- run_skills_step("compute_match_stats", 1, function() {
  source("data-raw/estimated-skills/01_compute_match_stats.R", local = TRUE)
})

# 6. Step 2: Estimate Skills ----

step_results[[2]] <- run_skills_step("estimate_skills", 2, function() {
  source("data-raw/estimated-skills/02_estimate_skills.R", local = TRUE)
})

# 7. Step 2b: Optimize Params (optional) ----

step_results[[3]] <- run_skills_step("optimize_params", "2b", function() {
  source("data-raw/estimated-skills/02b_optimize_params.R", local = TRUE)
})

# 8. Step 3: Skill SPM ----

step_results[[4]] <- run_skills_step("skill_spm", 3, function() {
  source("data-raw/estimated-skills/03_skill_spm.R", local = TRUE)
})

# 9. Step 4: Skill xRAPM ----

step_results[[5]] <- run_skills_step("skill_xrapm", 4, function() {
  source("data-raw/estimated-skills/04_skill_xrapm.R", local = TRUE)
})

# 10. Step 5: Skill Panna Ratings ----

step_results[[6]] <- run_skills_step("skill_panna_ratings", 5, function() {
  source("data-raw/estimated-skills/05_skill_panna_ratings.R", local = TRUE)
})

# 11. Step 6: Seasonal Skill Ratings ----

step_results[[7]] <- run_skills_step("seasonal_skill_ratings", 6, function() {
  source("data-raw/estimated-skills/06_seasonal_skill_ratings.R", local = TRUE)
})

# 12. Step 7: Train PSR Model ----

step_results[[8]] <- run_skills_step("train_psr_model", 7, function() {
  source("data-raw/estimated-skills/07_train_psr_model.R", local = TRUE)
})

# 13. Step 8: Export Skills ----

step_results[[9]] <- run_skills_step("export_skills", 8, function() {
  source("data-raw/estimated-skills/08_export_skills.R", local = TRUE)
})

# 14. Step 8b: Export Weekly PSR Snapshots ----

step_results[[10]] <- run_skills_step("export_psr_weekly", "8b", function() {
  source("data-raw/estimated-skills/08b_export_psr_weekly.R", local = TRUE)
})

# Career-trait Panna (decay-weighted multi-season xRAPM) — needs cache-opta splints +
# the step-03 skill-SPM, so it runs last. Uploads career_panna.parquet to ratings-data
# when upload_career_panna <- TRUE (CI sets it). See CLAUDE_TODO_CAREER_PANNA.md.
step_results[[11]] <- run_skills_step("career_panna", 9, function() {
  source("data-raw/estimated-skills/09_career_panna.R", local = TRUE)
})

# 15. Summary ----

print_pipeline_summary(step_results, pipeline_start, "ESTIMATED SKILLS PIPELINE")

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "05_skill_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "skill_panna_ratings.csv")))
message(sprintf("  - %s", file.path(cache_dir, "06_seasonal_ratings.rds")))
message("  - pannadata/data/opta/opta_skills.parquet (uploaded to GitHub)")
message("  - pannadata/data/opta/opta_psr_weekly.parquet (uploaded to GitHub)")

message("\nDone!")
