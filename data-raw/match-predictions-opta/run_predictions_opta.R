# run_predictions_opta.R
# Master script to run the Opta match prediction pipeline
#
# Predicts match outcomes (W/D/L) and goal counts using player ratings
# (RAPM/SPM/xRAPM) aggregated to team level, rolling form features,
# and Elo ratings. Two-step model: XGBoost Poisson for goals, then
# XGBoost multinomial for outcome probabilities.
#
# Prerequisites: Run the Opta RAPM pipeline first to generate
# seasonal player ratings (cache-opta/07_seasonal_ratings.rds).

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

# MINIMUM SEASON (skip data before this season)
if (!exists("min_season")) min_season <- "2013-2014"

# USE SKILL-BASED RATINGS (from estimated skills pipeline)
if (!exists("use_skill_ratings")) use_skill_ratings <- TRUE

# WHICH STEPS TO RUN
if (!exists("run_steps")) {
  run_steps <- list(
    step_01_build_fixture_results    = TRUE,
    step_02_player_ratings_to_team   = TRUE,
    step_02b_team_skill_features     = TRUE,   # Team-level skill aggregations
    step_03_team_rolling_features    = TRUE,
    step_04_build_match_dataset      = TRUE,
    step_05_fit_goals_model          = TRUE,
    step_06_fit_outcome_model        = TRUE,
    step_07_predict_fixtures         = TRUE,
    step_08_evaluate_model           = TRUE,
    step_09_upload_predictions       = FALSE,  # Opt-in: upload to GitHub
    step_10_export_blog_data         = FALSE   # Opt-in: export blog parquets
  )
}

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh
if (!exists("force_rebuild_from")) force_rebuild_from <- NULL

# 3. Helper Functions ----

run_step <- function(step_name, step_num, code_block) {
  # Support both numeric (2) and string ("2b") step numbers
  step_label <- as.character(step_num)
  if (is.numeric(step_num)) {
    step_key <- sprintf("step_%02d_%s", step_num, step_name)
  } else {
    padded <- sub("^(\\d)([a-z])", "0\\1\\2", as.character(step_num))
    step_key <- sprintf("step_%s_%s", padded, step_name)
  }
  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %s: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_label, step_name))
    return(NULL)
  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %s: %s",
                  format(Sys.time(), "%H:%M:%S"), step_label, step_name))
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

format_duration <- function(secs) {
  if (secs < 60) {
    sprintf("%.1f seconds", secs)
  } else if (secs < 3600) {
    sprintf("%.1f minutes", secs / 60)
  } else {
    sprintf("%.1f hours", secs / 3600)
  }
}

# 4. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Handle force rebuild
if (!is.null(force_rebuild_from) && force_rebuild_from >= 1 && force_rebuild_from <= 10) {
  cache_files <- list(
    "1" = "01_fixture_results.rds",
    "2" = "02_team_ratings.rds",
    "2b" = "02b_team_skill_features.rds",
    "3" = "03_rolling_features.rds",
    "4" = "04_match_dataset.rds",
    "5" = "05_goals_model.rds",
    "6" = "06_outcome_model.rds",
    "7" = c("07_predictions.rds", "predictions.csv", "predictions.parquet"),
    "8" = "08_evaluation.rds",
    "9" = character(0),
    "10" = c("panna_ratings.parquet", "match_predictions.parquet")
  )

  # Build list of steps to clear: numeric steps >= force_rebuild_from + fractional steps
  steps_to_clear <- as.character(force_rebuild_from:10)
  # Include fractional steps (e.g., "2b") when their parent step is being rebuilt
  fractional_steps <- setdiff(names(cache_files), as.character(1:10))
  for (fs in fractional_steps) {
    parent <- as.numeric(sub("[a-z]+$", "", fs))
    if (!is.na(parent) && parent >= force_rebuild_from) {
      steps_to_clear <- c(steps_to_clear, fs)
    }
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
  force_rebuild <- TRUE
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %d onwards\n",
                  deleted, force_rebuild_from))
} else {
  force_rebuild <- FALSE
}

pipeline_start <- Sys.time()
step_results <- list()

message("\n")
message(paste(rep("#", 70), collapse = ""))
message("#")
message("#   OPTA MATCH PREDICTION PIPELINE")
message("#")
message(sprintf("#   Leagues: %s", paste(leagues, collapse = ", ")))
message(sprintf("#   Seasons: %s", if (is.null(seasons)) "All available" else paste(seasons, collapse = ", ")))
message(sprintf("#   Min season: %s", if (is.null(min_season)) "None" else min_season))
message(sprintf("#   Skill ratings: %s", use_skill_ratings))
message(sprintf("#   Force rebuild from: %s",
                if (is.null(force_rebuild_from)) "None (use cache)" else force_rebuild_from))
message("#")
message(paste(rep("#", 70), collapse = ""))

# 5. Step 1: Build Fixture Results ----

step_results[[1]] <- run_step("build_fixture_results", 1, function() {
  source("data-raw/match-predictions-opta/01_build_fixture_results.R", local = TRUE)
})

# 6. Step 2: Player Ratings to Team ----

step_results[[2]] <- run_step("player_ratings_to_team", 2, function() {
  source("data-raw/match-predictions-opta/02_player_ratings_to_team.R", local = TRUE)
})

# 6b. Step 2b: Team Skill Features ----

step_results[["2b"]] <- run_step("team_skill_features", "2b", function() {
  source("data-raw/match-predictions-opta/02b_team_skill_features.R", local = TRUE)
})

# 7. Step 3: Team Rolling Features ----

step_results[[3]] <- run_step("team_rolling_features", 3, function() {
  source("data-raw/match-predictions-opta/03_team_rolling_features.R", local = TRUE)
})

# 8. Step 4: Build Match Dataset ----

step_results[[4]] <- run_step("build_match_dataset", 4, function() {
  source("data-raw/match-predictions-opta/04_build_match_dataset.R", local = TRUE)
})

# 9. Step 5: Fit Goals Model ----

step_results[[5]] <- run_step("fit_goals_model", 5, function() {
  source("data-raw/match-predictions-opta/05_fit_goals_model.R", local = TRUE)
})

# 10. Step 6: Fit Outcome Model ----

step_results[[6]] <- run_step("fit_outcome_model", 6, function() {
  source("data-raw/match-predictions-opta/06_fit_outcome_model.R", local = TRUE)
})

# 11. Step 7: Predict Fixtures ----

step_results[[7]] <- run_step("predict_fixtures", 7, function() {
  source("data-raw/match-predictions-opta/07_predict_fixtures.R", local = TRUE)
})

# 12. Step 8: Evaluate Model ----

step_results[[8]] <- run_step("evaluate_model", 8, function() {
  source("data-raw/match-predictions-opta/08_evaluate_model.R", local = TRUE)
})

# 13. Step 9: Upload Predictions ----

step_results[[9]] <- run_step("upload_predictions", 9, function() {
  source("data-raw/match-predictions-opta/09_upload_predictions.R", local = TRUE)
})

# 14. Step 10: Export Blog Data ----

step_results[[10]] <- run_step("export_blog_data", 10, function() {
  source("data-raw/match-predictions-opta/10_export_blog_data.R", local = TRUE)
})

# 15. Summary ----

pipeline_end <- Sys.time()
total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

message("\n")
message(paste(rep("=", 70), collapse = ""))
message("MATCH PREDICTION PIPELINE COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message("\nStep Summary:")
message(sprintf("%-35s %-10s %s", "Step", "Status", "Duration"))
message(paste(rep("-", 60), collapse = ""))

for (result in step_results) {
  if (!is.null(result)) {
    message(sprintf("%-35s %-10s %s",
                    result$name,
                    result$status,
                    result$duration_formatted))
  }
}

message(paste(rep("-", 60), collapse = ""))
message(sprintf("%-35s %-10s %s", "TOTAL", "", format_duration(as.numeric(total_duration))))

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "07_predictions.rds")))
message(sprintf("  - %s", file.path(cache_dir, "predictions.csv")))
message(sprintf("  - %s", file.path(cache_dir, "predictions.parquet")))
message(sprintf("  - %s", file.path(cache_dir, "08_evaluation.rds")))
if (isTRUE(run_steps$step_10_export_blog_data)) {
  message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.parquet")))
  message(sprintf("  - %s", file.path(cache_dir, "match_predictions.parquet")))
}

message("\nDone!")
