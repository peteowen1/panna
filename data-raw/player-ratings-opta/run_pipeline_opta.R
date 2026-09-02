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

# LEAGUES TO INCLUDE — canonical rating/display set (PANNA_RATING_LEAGUES) +
# bridge comps for cross-league connectivity. Shared with step 03 / skills / 10b.
if (!exists("leagues", inherits = FALSE)) leagues <- c(PANNA_RATING_LEAGUES, PANNA_BRIDGE_LEAGUES)

# SEASONS (NULL = all available, or specify like c("2024-2025"))
# For incremental rebuilds, set to the changed season(s):
#   seasons <- c("2025-2026")  # Only rebuild current season
# Note: Steps 04+ (RAPM/SPM/xRAPM) run on ALL data regardless,
# since RAPM is cross-season by design. Only steps 01-03 benefit.
if (!exists("seasons", inherits = FALSE)) seasons <- NULL

# MINIMUM SEASON (skip data before this season, NULL = no filter)
if (!exists("min_season", inherits = FALSE)) min_season <- "2013-2014"

# ENRICH SPM WITH xMETRICS FEATURES (xG/xA/xPass per-90)
if (!exists("use_xmetrics_features", inherits = FALSE)) use_xmetrics_features <- TRUE

# START FROM STEP (skip earlier steps that are already cached)
# Set before sourcing: start_step <- 3  # resume from splints
if (!exists("start_step", inherits = FALSE)) start_step <- 1

# WHICH STEPS TO RUN (auto-populated from start_step)
if (!exists("run_steps", inherits = FALSE)) {
  run_steps <- list(
    step_01_load_data        = start_step <= 1,
    step_02_data_processing  = start_step <= 2,
    step_03_splint_creation  = start_step <= 3,
    step_04_rapm             = start_step <= 4,
    step_05_spm              = start_step <= 5,
    # Fractional step (panna#173), same idiom as the skills pipeline's
    # step_08b_export_psr_weekly = start_num <= 8.5: not a separate opt-in
    # boolean, just gated by start_step like every other step here. Reads
    # cache-opta/05_spm.rds (no refit), so it belongs right after step 5.
    step_05b_export_spm_coefficients = start_step <= 5.5,
    step_06_xrapm            = start_step <= 6,
    step_07_seasonal_ratings = start_step <= 7,
    # Fractional step (panna#224), same idiom as 05b above. Reads
    # cache-opta/07_seasonal_ratings.rds (no refit) plus the lineups feed, so it
    # belongs right after step 7. Produces the opponent-quality control consumed
    # by the skills pipeline's 07_train_psr_model.R and by build_epr_weekly.R.
    # It lived in debug/keep/ and belonged to no pipeline until 2026-09-02,
    # which is exactly why it went 3.5 months stale and silently disabled the
    # opponent adjustment in eight competitions.
    step_07c_team_season_strength = start_step <= 7.5,
    step_08_panna_ratings    = start_step <= 8,
    step_09_export_ratings   = start_step <= 9
  )
}

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh
if (!exists("force_rebuild_from", inherits = FALSE)) force_rebuild_from <- NULL

# 3. Helper Functions ----

source("data-raw/pipeline_utils.R")

# Each step runs in its OWN fresh R subprocess via the shared
# run_step_isolated() (pipeline_utils.R) — see its docs for the memory
# rationale (panna#87/#128) and the child environment contract. Config is
# snapshotted ONCE here (after all config + force-rebuild handling, before
# any step) and reloaded into each subprocess's globalenv.
#
# NB since the 2026-07-17 dedup the child also does library(dplyr) +
# devtools::load_all() unconditionally (previously only the predictions
# pipeline's child did) — step scripts' own load_all headers are now
# belt-and-braces, not load-bearing. That asymmetry was the WS-3 bug.
.write_pipeline_config <- function() {
  write_isolated_config(file.path(cache_dir, ".pipeline_config.rds"))
}

run_step_opta <- function(step_name, step_num, code_block) {
  run_step_isolated(
    step_name, step_num, code_block,
    cfg_path = file.path("data-raw", "cache-opta", ".pipeline_config.rds"),
    run_steps = run_steps
  )
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

# Snapshot config for the isolated step subprocesses (after all config + the
# force-rebuild handling above, before any step runs).
.write_pipeline_config()

# Wrapper that updates pipeline_failed in parent env. Takes the step_results
# INDEX (append position), not the "true" step number/label -- those diverge
# once a fractional step (5b) is inserted between numbered ones. Uses
# check_critical_step()'s 1-arg form so the CRITICAL message reads the true
# step/name off the stored result itself rather than trusting a second,
# separately-typed literal to stay in sync with the index.
check_step <- function(idx) {
  if (check_critical_step(step_results[[idx]])) {
    pipeline_failed <<- TRUE
    return(TRUE)
  }
  FALSE
}

print_pipeline_banner("OPTA PANNA RATINGS PIPELINE", c(
  sprintf("Leagues: %s", paste(leagues, collapse = ", ")),
  sprintf("Seasons: %s", if (is.null(seasons)) "All available" else paste(seasons, collapse = ", ")),
  sprintf("Min season: %s", if (is.null(min_season)) "None" else min_season),
  sprintf("xMetrics enrichment: %s", use_xmetrics_features),
  sprintf("Start from step: %d", start_step),
  sprintf("Force rebuild from: %s",
          if (is.null(force_rebuild_from)) "None (use cache)" else force_rebuild_from)
))

# 5. Step 1: Load Opta Data ----

step_results[[1]] <- run_step_opta("load_data", 1, function() {
  source("data-raw/player-ratings-opta/01_load_opta_data.R", local = TRUE)
})
check_step(1)

# 6. Step 2: Data Processing ----

if (!isTRUE(pipeline_failed)) {
  step_results[[2]] <- run_step_opta("data_processing", 2, function() {
    source("data-raw/player-ratings-opta/02_data_processing.R", local = TRUE)
  })
  check_step(2)
}

# 7. Step 3: Splint Creation ----

if (!isTRUE(pipeline_failed)) {
  step_results[[3]] <- run_step_opta("splint_creation", 3, function() {
    source("data-raw/player-ratings-opta/03_splint_creation.R", local = TRUE)
  })
  check_step(3)
}

# 8. Step 4: RAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[4]] <- run_step_opta("rapm", 4, function() {
    source("data-raw/player-ratings-opta/04_rapm.R", local = TRUE)
  })
  check_step(4)
}

# 9. Step 5: SPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[5]] <- run_step_opta("spm", 5, function() {
    source("data-raw/player-ratings-opta/05_spm.R", local = TRUE)
  })
  check_step(5)
}

# 9b. Step 5b: Export SPM Coefficients (panna#173) ----
# Blog-parity export (see export_spm_coefficients_csv() in R/spm_model.R):
# writes inst/extdata/spm{,_osr,_dsr}_coefficients.csv from the model step 5
# just cached. Cheap (no refit), so — like the skills pipeline's PSR
# coefficient write inside its step 7 — it just runs whenever step 5 does,
# rather than living behind its own opt-in toggle. Not registered in a
# publish_files accumulator: neither does the PSR/OSR/DSR equivalent (those
# are committed package data under inst/extdata, not GitHub-Release-published
# pipeline output).

if (!isTRUE(pipeline_failed)) {
  step_results[[6]] <- run_step_opta("export_spm_coefficients", "5b", function() {
    source("data-raw/player-ratings-opta/05b_export_spm_coefficients.R", local = TRUE)
  })
  check_step(6)
}

# 10. Step 6: xRAPM ----

if (!isTRUE(pipeline_failed)) {
  step_results[[7]] <- run_step_opta("xrapm", 6, function() {
    source("data-raw/player-ratings-opta/06_xrapm.R", local = TRUE)
  })
  check_step(7)
}

# 11. Step 7: Seasonal Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[8]] <- run_step_opta("seasonal_ratings", 7, function() {
    source("data-raw/player-ratings-opta/07_seasonal_ratings.R", local = TRUE)
  })
  check_step(8)
}

# 11b. Step 7c: Team-Season Strength (opponent-quality control) ----

if (!isTRUE(pipeline_failed)) {
  step_results[[9]] <- run_step_opta("team_season_strength", "7c", function() {
    source("data-raw/player-ratings-opta/07c_team_season_strength.R", local = TRUE)
  })
  check_step(9)
}

# 12. Step 8: Final Ratings ----

if (!isTRUE(pipeline_failed)) {
  step_results[[10]] <- run_step_opta("panna_ratings", 8, function() {
    source("data-raw/player-ratings-opta/08_panna_ratings.R", local = TRUE)
  })
  check_step(10)
}

# 13. Step 9: Export Ratings ----

# Skip export if pipeline failed
if (isTRUE(pipeline_failed)) {
  message("\nSkipping export: upstream step failed")
  step_results[[11]] <- list(step = 9, name = "export_ratings", status = "SKIPPED",
                            duration_secs = 0, duration_formatted = "0.0 seconds")
} else {
  step_results[[11]] <- run_step_opta("export_ratings", 9, function() {
    source("data-raw/player-ratings-opta/09_export_ratings.R", local = TRUE)
  })
  # Last step, but check_step still matters: it sets pipeline_failed, which the
  # workflow's `if (isTRUE(pipeline_failed)) quit(status = 1)` reads — without
  # this, a failed export prints FAILED in the summary yet the job stays green
  # (exactly how ratings-data went silently stale 2026-06-11 → 2026-07-17).
  check_step(11)
}

# 14. Summary ----

print_pipeline_summary(step_results, pipeline_start, "OPTA PIPELINE")

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "08_panna.rds")))
message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.csv")))
message("  - peteowen1/pannadata releases: seasonal_xrapm.parquet, seasonal_spm.parquet")

message("\nDone!")
