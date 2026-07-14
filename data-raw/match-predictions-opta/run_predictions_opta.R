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
# International qualifiers + Nations League + friendlies added 2026-05-28
# so the Elo iteration sees national-team form beyond just WC/EURO finals.
# Previously, teams like Norway (whose entire 2026 qualifying campaign
# happens in UEFA_WC_Qualifiers) sat at the 1500 initial Elo; the model
# was making predictions on top of effectively zero national-team
# strength evidence for most countries.
if (!exists("leagues", inherits = FALSE)) leagues <- c(
  "ENG", "ESP", "GER", "ITA", "FRA",       # Big 5
  "NED", "POR", "TUR", "ENG2", "SCO",      # Extended domestic
  "UCL", "UEL", "UECL",                     # European club comps
  # International tournaments
  "WC", "EURO", "AFCON", "COPA", "GOLD", "ACUP", "GULF",
  # International qualifiers + Nations League + friendlies
  "WCQ_UEFA", "WCQ_CONMEBOL", "WCQ_CAF", "WCQ_AFC",
  "EUROQ", "AFCONQ", "ACUPQ", "NL", "INTL_FR"
)

# SEASONS (NULL = all available, or specify like c("2024-2025"))
if (!exists("seasons", inherits = FALSE)) seasons <- NULL

# MINIMUM SEASON (skip data before this season)
if (!exists("min_season", inherits = FALSE)) min_season <- "2013-2014"

# USE SKILL-BASED RATINGS (from estimated skills pipeline)
if (!exists("use_skill_ratings", inherits = FALSE)) use_skill_ratings <- TRUE

# WHICH STEPS TO RUN
if (!exists("run_steps", inherits = FALSE)) {
  run_steps <- list(
    step_01_build_fixture_results    = TRUE,
    step_01b_refresh_wc2026_squads   = FALSE,  # Opt-in: rebuild announced-squads EM from Wikipedia (GHA enables)
    step_02_player_ratings_to_team   = TRUE,
    step_02b_team_skill_features     = TRUE,   # Team-level skill aggregations
    step_03_team_rolling_features    = TRUE,
    step_04_build_match_dataset      = TRUE,
    step_05_fit_goals_model          = TRUE,
    step_06_fit_outcome_model        = TRUE,
    step_07_predict_fixtures         = TRUE,
    step_08_evaluate_model           = TRUE,
    step_09_upload_predictions       = FALSE,  # Opt-in: upload to GitHub
    step_10_export_blog_data         = FALSE,  # Opt-in: export blog parquets
    step_10b_export_game_logs        = FALSE,  # Opt-in: export per-match value metrics
    step_10c_export_equity           = FALSE,  # Opt-in: export per-action EPV equity
    step_10d_export_shootout_wpa     = FALSE,  # Opt-in: export per-player penalty-shootout WPA
    step_11_simulate_wc2026          = FALSE,  # Opt-in: simulate the 2026 World Cup
    step_12_export_wc2026_blog       = FALSE,  # Opt-in: export WC2026 blog data
    step_12b_snapshot_wc_minutes     = FALSE,  # Opt-in: archive dated minutes snapshot + diff
    step_12c_snapshot_wc_strength    = FALSE,  # Opt-in: archive dated team-strength (ELO+p_champ) snapshot + diff
    step_13_publish_release_data     = FALSE   # Opt-in: single gated publish of predictions-latest + blog-latest (PA5/H-TORN)
  )
}

# FORCE REBUILD FROM STEP
# Set to a step number to clear cache and rebuild from that step onwards
# NULL = normal run (use cache), 1 = full refresh
if (!exists("force_rebuild_from", inherits = FALSE)) force_rebuild_from <- NULL

# 3. Shared Pipeline Utilities ----

source("data-raw/pipeline_utils.R")

# Critical step check: set pipeline_failed flag to skip downstream steps
check_pred_critical <- function(result) {
  if (check_critical_step(result)) {
    pipeline_failed <<- TRUE
  }
}

# 4. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Handle force rebuild
pred_cache_files <- list(
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
  "10" = c("panna_ratings.parquet", "match_predictions.parquet", "season_standings.parquet"),
  "10b" = "game_logs.parquet",
  "10c" = "action_equity.parquet"
)
clear_cache_files(force_rebuild_from, cache_dir, pred_cache_files, max_step = 10)
force_rebuild <- !is.null(force_rebuild_from) && force_rebuild_from >= 1

# The per-league SPADL cache (SPADL_CACHE_DIR = data-raw/cache/epv/spadl) is a
# SEPARATE store that clear_cache_files() above does NOT touch. Steps 10b/10c
# consume it via get_or_build_spadl(). A stale SPADL built before an events
# backfill silently caps coverage even after the events grow (the 2026-06
# Championship 536/557 case: cloud events were 557 but a 536-match cached SPADL
# survived). Any forced rebuild re-runs 10b/10c, so drop the SPADL cache too —
# get_or_build_spadl() then rebuilds it from current events.
if (!is.null(force_rebuild_from)) {
  spadl_files <- list.files(SPADL_CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (length(spadl_files) > 0) {
    unlink(spadl_files)
    message(sprintf("Cleared %d SPADL cache file(s) from %s (forced rebuild)",
                    length(spadl_files), SPADL_CACHE_DIR))
  }
}

pipeline_start <- Sys.time()
step_results <- list()
pipeline_failed <- FALSE

# Publish-candidate accumulator for step 13 (ECOSYSTEM-FIX-PLAN.md PA5 /
# panna H-TORN). Steps 09/10/10b/10c/10d/12 write LOCAL outputs only and
# register the files they want published THIS run here (respecting each
# step's own "did I actually rewrite this" logic, e.g. 10b/10c's
# mirror_alias) via `publish_files$<tag> <<- c(publish_files$<tag>, ...)` --
# superassignment finds this pre-declared global binding by walking up the
# enclosing scope from each step's `source(..., local = TRUE)` frame. Step 13
# is the ONE place either tag actually gets uploaded, via vb_publish()
# (hash -> upload -> verify -> manifest LAST), so predictions-latest and
# blog-latest either both advance together this run or neither does.
publish_files <- list(predictions_latest = character(0), blog_latest = character(0))

print_pipeline_banner("OPTA MATCH PREDICTION PIPELINE", c(
  sprintf("Leagues: %s", paste(leagues, collapse = ", ")),
  sprintf("Seasons: %s", if (is.null(seasons)) "All available" else paste(seasons, collapse = ", ")),
  sprintf("Min season: %s", if (is.null(min_season)) "None" else min_season),
  sprintf("Skill ratings: %s", use_skill_ratings),
  sprintf("Force rebuild from: %s",
          if (is.null(force_rebuild_from)) "None (use cache)" else force_rebuild_from)
))

# 5. Step 1: Build Fixture Results ----

step_results[[1]] <- run_pipeline_step("build_fixture_results", 1, function() {
  source("data-raw/match-predictions-opta/01_build_fixture_results.R", local = TRUE)
})
check_pred_critical(step_results[[1]])

# 5b. Step 1b: Refresh WC2026 Announced Squads ----
# Runs after step 01 because the derived-squad path reads
# 01_fixture_results.rds for the WC2026 team list.

step_results[["1b"]] <- run_pipeline_step("refresh_wc2026_squads", "1b", function() {
  source("data-raw/match-predictions-opta/01b_refresh_wc2026_squads.R", local = TRUE)
})
# 1b is non-critical — 01b itself falls back to the predictions-cache copy,
# and step 02 falls back to last-XI weighting if the parquet is absent.

# 6. Step 2: Player Ratings to Team ----

step_results[[2]] <- run_pipeline_step("player_ratings_to_team", 2, function() {
  source("data-raw/match-predictions-opta/02_player_ratings_to_team.R", local = TRUE)
})
check_pred_critical(step_results[[2]])

# 6b. Step 2b: Team Skill Features ----

step_results[["2b"]] <- run_pipeline_step("team_skill_features", "2b", function() {
  source("data-raw/match-predictions-opta/02b_team_skill_features.R", local = TRUE)
})
# 2b is optional — don't abort if it fails

# 7. Step 3: Team Rolling Features ----

step_results[[3]] <- run_pipeline_step("team_rolling_features", 3, function() {
  source("data-raw/match-predictions-opta/03_team_rolling_features.R", local = TRUE)
})
check_pred_critical(step_results[[3]])

# 8. Step 4: Build Match Dataset ----

step_results[[4]] <- run_pipeline_step("build_match_dataset", 4, function() {
  source("data-raw/match-predictions-opta/04_build_match_dataset.R", local = TRUE)
})
check_pred_critical(step_results[[4]])

# 9. Step 5: Fit Goals Model ----

step_results[[5]] <- run_pipeline_step("fit_goals_model", 5, function() {
  source("data-raw/match-predictions-opta/05_fit_goals_model.R", local = TRUE)
})
check_pred_critical(step_results[[5]])

# 10. Step 6: Fit Outcome Model ----

step_results[[6]] <- run_pipeline_step("fit_outcome_model", 6, function() {
  source("data-raw/match-predictions-opta/06_fit_outcome_model.R", local = TRUE)
})
check_pred_critical(step_results[[6]])

# 11. Step 7: Predict Fixtures ----

step_results[[7]] <- run_pipeline_step("predict_fixtures", 7, function() {
  source("data-raw/match-predictions-opta/07_predict_fixtures.R", local = TRUE)
})

# 12. Step 8: Evaluate Model ----

step_results[[8]] <- run_pipeline_step("evaluate_model", 8, function() {
  source("data-raw/match-predictions-opta/08_evaluate_model.R", local = TRUE)
})

# 13. Step 9: Upload Predictions ----

step_results[[9]] <- run_pipeline_step("upload_predictions", 9, function() {
  source("data-raw/match-predictions-opta/09_upload_predictions.R", local = TRUE)
})

# 14. Step 10: Export Blog Data ----

step_results[[10]] <- run_pipeline_step("export_blog_data", 10, function() {
  source("data-raw/match-predictions-opta/10_export_blog_data.R", local = TRUE)
})

# 14b. Step 10b: Export Game Logs ----

step_results[["10b"]] <- run_pipeline_step("export_game_logs", "10b", function() {
  source("data-raw/match-predictions-opta/10b_export_game_logs.R", local = TRUE)
})

# 14c. Step 10c: Export Equity ----

step_results[["10c"]] <- run_pipeline_step("export_equity", "10c", function() {
  source("data-raw/match-predictions-opta/10c_export_equity.R", local = TRUE)
})

# 14c2. Step 10d: Export Shootout WPA ----

step_results[["10d"]] <- run_pipeline_step("export_shootout_wpa", "10d", function() {
  source("data-raw/match-predictions-opta/10d_export_shootout_wpa.R", local = TRUE)
})

# 14d. Step 11: Simulate WC 2026 ----

step_results[[11]] <- run_pipeline_step("simulate_wc2026", 11, function() {
  source("data-raw/match-predictions-opta/11_simulate_wc2026.R", local = TRUE)
})

# 14e. Step 12: Export WC 2026 Blog Data ----

step_results[[12]] <- run_pipeline_step("export_wc2026_blog", 12, function() {
  source("data-raw/match-predictions-opta/12_export_wc2026_blog.R", local = TRUE)
})

# 14f. Step 12b: Snapshot WC 2026 Minutes ----
# Archive a dated copy of wc2026_squads.parquet to the wc2026-minutes-history
# release and diff it against the previous snapshot (group-stage drift tracking).
# Runs after step 12, which writes the squads file this reads.

step_results[["12b"]] <- run_pipeline_step("snapshot_wc_minutes", "12b", function() {
  source("data-raw/match-predictions-opta/12b_snapshot_wc_minutes.R", local = TRUE)
})

# 14g. Step 12c: Snapshot WC 2026 Team Strength ----
# Archive a dated copy of wc2026_team_strength.parquet (ELO + p_champ + ratings)
# to the wc2026-strength-history release and diff it against the previous
# snapshot (tournament ELO/champion-odds drift tracking). Runs after step 12,
# which writes the team_strength file this reads.

step_results[["12c"]] <- run_pipeline_step("snapshot_wc_strength", "12c", function() {
  source("data-raw/match-predictions-opta/12c_snapshot_wc_strength.R", local = TRUE)
})

# 14h. Step 13: Publish predictions-latest + blog-latest (gated, manifest-last) ----
# Runs after every build step so publish_files is fully populated. A failure
# here (e.g. one tag's vb_publish aborting) is caught by run_pipeline_step() like
# any other step -- it sets pipeline_failed, the workflow's
# `quit(status = 1)` stops the job, and the "Trigger blog data build"
# workflow step (gated `if: success()`) never fires a predictions-complete
# dispatch against a torn release.

step_results[[13]] <- run_pipeline_step("publish_release_data", 13, function() {
  source("data-raw/match-predictions-opta/13_publish_release_data.R", local = TRUE)
})

# 15. Summary ----

print_pipeline_summary(step_results, pipeline_start, "MATCH PREDICTION PIPELINE", col_width = 35)

message("\nOutput files:")
message(sprintf("  - %s", file.path(cache_dir, "07_predictions.rds")))
message(sprintf("  - %s", file.path(cache_dir, "predictions.csv")))
message(sprintf("  - %s", file.path(cache_dir, "predictions.parquet")))
message(sprintf("  - %s", file.path(cache_dir, "08_evaluation.rds")))
if (isTRUE(run_steps$step_10_export_blog_data)) {
  message(sprintf("  - %s", file.path(cache_dir, "panna_ratings.parquet")))
  message(sprintf("  - %s", file.path(cache_dir, "match_predictions.parquet")))
}
if (isTRUE(run_steps$step_10b_export_game_logs)) {
  message(sprintf("  - %s", file.path(cache_dir, "game_logs.parquet")))
}
if (isTRUE(run_steps$step_10c_export_equity)) {
  message(sprintf("  - %s", file.path(cache_dir, "action_equity.parquet")))
}
if (isTRUE(run_steps$step_10d_export_shootout_wpa)) {
  message(sprintf("  - %s", file.path(cache_dir, "shootout_wpa.parquet")))
}
if (isTRUE(run_steps$step_11_simulate_wc2026)) {
  message(sprintf("  - %s", file.path(cache_dir, "wc2026_bt_ratings.parquet")))
  message(sprintf("  - %s", file.path(cache_dir, "wc2026_simulation.parquet")))
  message(sprintf("  - %s", file.path(cache_dir, "wc2026_group_expectations.parquet")))
}
if (isTRUE(run_steps$step_12_export_wc2026_blog)) {
  message(sprintf("  - %s", file.path(cache_dir, "wc2026_predictions.parquet")))
  message(sprintf("  - %s", file.path(cache_dir, "wc2026_team_strength.parquet")))
  message("  - (wc2026_*.parquet uploaded to blog-latest)")
}

message("\nDone!")
