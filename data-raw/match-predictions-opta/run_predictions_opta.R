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
    step_07b_snapshot_predictions    = FALSE,  # Opt-in: archive dated predictions.parquet snapshot (panna#178)
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

# 3b. Isolated-step helper (steps 2/2b only) ----
# Steps 2 (player_ratings_to_team) and 2b (team_skill_features) load
# 01_match_stats.rds -- 1.9M rows x 421 cols, ~5.9GB in R heap -- just to
# filter it down to the handful of players in upcoming fixtures. Confirmed
# root cause of predictions-pipeline.yml dev failures 2026-07-16 (3/5
# dispatches died with exit 143 / "operation was canceled" right after this
# load; RSS checkpoint showed ~9.1GB heap BEFORE the skill-estimate
# computation even starts). Unlike run_pipeline_opta.R, this pipeline runs
# all steps in ONE shared session, so whatever these two steps allocate
# becomes a permanent floor for every step after them too (step 2b alone
# pushed a successful run from 2.7GB to 11.6GB that never came back down).
# Isolate just these two in their own callr subprocess (same technique as
# run_pipeline_opta.R's run_step_opta(), panna#87/#128) so their peak is
# fully released to the OS on exit. Safe here because neither step writes
# to publish_files or any other cross-step in-memory state -- both
# communicate solely via their own .rds cache file. Do NOT reuse this for
# steps that DO rely on in-memory state crossing step boundaries
# (09/10/10b/10c/10d/12 all write to publish_files via `<<-`; step 13 reads
# it -- none of that would survive a subprocess boundary).
.pred_isolated_cfg_path <- file.path("data-raw", "cache-predictions-opta",
                                     ".step_isolated_config.rds")

.write_pred_isolated_config <- function() {
  # publish_files is a cross-step `<<-` accumulator (declared at top level,
  # written by steps 09/10/10b/10c/10d/12, read by step 13) -- excluded here
  # even though steps 2/2b don't currently write to it, so a future edit
  # that adds a publish_files write to either isolated step fails loudly
  # (object not found in the child) instead of silently losing the write
  # across the subprocess boundary.
  write_isolated_config(.pred_isolated_cfg_path, exclude = "publish_files")
}

# Shared isolation core lives in pipeline_utils.R (run_step_isolated) since the
# 2026-07-17 dedup — this wrapper adds the per-call config snapshot (this
# pipeline mutates config between steps, unlike opta's once-up-front snapshot)
# and the inline pipeline_failed propagation this orchestrator uses.
run_pred_step_isolated <- function(step_name, step_num, code_block) {
  .write_pred_isolated_config()
  result <- run_step_isolated(step_name, step_num, code_block,
                              cfg_path = .pred_isolated_cfg_path,
                              run_steps = run_steps,
                              pipeline_failed = pipeline_failed)
  if (!is.null(result) && identical(result$status, "FAILED")) {
    pipeline_failed <<- TRUE
  }
  result
}

# 3c. Non-fatal step helper (WC 2026 steps 11/12/12b/12c) ----
# run_pipeline_step() sets pipeline_failed on ANY failure, which makes every
# later step -- including step 13, the ONLY publish -- print
# "SKIPPED (previous step failed)". That is right for the model chain: a torn
# 07 must never reach predictions-latest. It is wrong for the World Cup
# branch, which is a side output nothing else consumes.
#
# On 2026-08-13 step 11 aborted in build_knockout_lookup() and took step 13
# with it. The predictions themselves were fine and sat in the cache for eight
# days while the daily run reported FAILED and published nothing. Isolate the
# blast radius: these steps still report FAILED in the summary and are still
# visible in the log, they just don't gate the publish.
#
# Deliberately NOT applied to steps 09/10/10b/10c/10d: those register files in
# publish_files, so half of one failing IS a reason to hold the release back.
# Step 12 registers too, but only at its very end (12_export_wc2026_blog.R:465,
# after every wc2026_*.parquet is written), so a mid-step failure registers
# nothing and step 13 has nothing torn to publish.
#
# Non-fatal must not mean invisible. The workflow's exit code is driven by
# `pipeline_failed`, so a WC step failing for a REAL reason (a bug, not "the
# tournament ended") would otherwise leave a green tick and a job summary that
# still claims blog data was uploaded -- a recurring failure nobody would see
# without opening the raw log. Each failure is recorded to a marker file that
# the workflow's summary step reads and turns into a GitHub warning
# annotation. The run stays green on purpose; it just stops being silent.
.NONFATAL_MARKER <- ".nonfatal_step_failures"

run_pred_step_optional <- function(step_name, step_num, code_block) {
  result <- run_step(step_name, step_num, code_block, run_steps, pipeline_failed)
  if (!is.null(result) && identical(result$status, "FAILED")) {
    message(sprintf(
      "  NOTE: step %s is non-fatal -- the pipeline continues and step 13 still publishes.",
      as.character(step_num)))
    cat(sprintf("step %s (%s)
", as.character(step_num), step_name),
        file = file.path(cache_dir, .NONFATAL_MARKER), append = TRUE)
  }
  result
}

# 4. Initialize Pipeline ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

# Start each run with no non-fatal failures recorded. Cloud runs check out
# fresh so this is a no-op there; locally the cache directory persists, and a
# marker left over from yesterday would report a failure that did not happen.
unlink(file.path(cache_dir, .NONFATAL_MARKER))

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
# Isolated (see run_pred_step_isolated() above) -- loads the 5.9GB
# match_stats cache and was the confirmed OOM source in this pipeline.

step_results[[2]] <- run_pred_step_isolated("player_ratings_to_team", 2, function() {
  source("data-raw/match-predictions-opta/02_player_ratings_to_team.R", local = TRUE)
})
check_pred_critical(step_results[[2]])

# 6b. Step 2b: Team Skill Features ----
# Isolated for the same reason -- the successful 2026-07-15 run showed RSS
# jump from 2.7GB to 11.6GB across this one step, a floor the rest of the
# pipeline then carried for the remaining ~10 steps.

step_results[["2b"]] <- run_pred_step_isolated("team_skill_features", "2b", function() {
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

# 11b. Step 7b: Snapshot Predictions ----
# Archive a dated copy of predictions.parquet to the predictions-history
# release (panna#178). Runs after step 7, which writes the cache file this
# reads.

step_results[["7b"]] <- run_pipeline_step("snapshot_predictions", "7b", function() {
  source("data-raw/match-predictions-opta/07b_snapshot_predictions.R", local = TRUE)
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

# 14c-bis. World Cup liveness gate (steps 11/12b/12c) ----
# Steps 11, 12b and 12c simulate/snapshot a tournament that is still being
# played. Once the final is done there is nothing left to simulate, and the
# WC branch does not just become pointless -- it becomes wrong, then it
# becomes fatal:
#
#   * Pointless: step 11 filters predictions to WC2026 and, post-final, got
#     zero rows. It went on to fit Bradley-Terry ratings and simulate 10,000
#     tournaments off a knockout lookup alone, publishing champion odds for a
#     finished competition. The log said "Simulating WC2026 from 0
#     group-stage predictions across 0 teams" and the step reported SUCCESS.
#   * Fatal: bda139d (2026-08-12) added an invariant check to
#     build_knockout_lookup() -- a team's aggregates must be constant across
#     its own WC rows, since the lookup reads row [1]. That holds while every
#     WC row is an unplayed fixture carrying one as-of snapshot. Played rows
#     carry per-match as-of aggregates, so from the next run (2026-08-13) it
#     aborted on Argentina. The guard is correct; the branch should not have
#     been running at all.
#
# step 12 is deliberately NOT in this gate (it was, and that was the bug --
# panna#194 review). Step 12 EXPORTS; it does not simulate. Half of it
# (sections 2/2b of 12_export_wc2026_blog.R) reads only 07_predictions.rds
# and is exactly as valid post-tournament as pre- -- wc2026_predictions.parquet
# and wc_history_predictions.parquet are how the blog browses a FINISHED World
# Cup. The other half reads step 11's outputs and stops running the moment
# step 11 does (12_export_wc2026_blog.R gates those sections itself, on
# whether wc2026_simulation.parquet et al. exist -- not on tournament
# liveness), so disabling step 12 here bought no safety and cost the blog its
# post-final match-history export.
#
# Decide from the data, not the calendar: any WC2026 row still marked
# "fixture" means matches remain. 07_predictions.rds is the right source --
# small (~53k x 15), already written by step 7, and it carries the played /
# fixture status directly. 04_match_dataset.rds would answer the same question
# but is ~47MB, and this pipeline has a history of memory cliffs (panna#128).
#
# Known edge, accepted: unresolved knockout slots sit in the data as
# blank-team placeholders with status "fixture", and neither this nor step 11
# counts them (they cannot be simulated). So in a window where every resolved
# match is played but the bracket has not filled in, this reads 0 and skips
# the WC steps for those runs, resuming by itself once the draw lands. These steps are idempotent weekly refreshes, so a skipped
# run costs a stale WC tab for a day -- against eight days of the whole
# pipeline publishing nothing, which is what the alternative cost.
.wc_steps <- c("step_11_simulate_wc2026",
               "step_12b_snapshot_wc_minutes", "step_12c_snapshot_wc_strength")
if (any(vapply(.wc_steps, function(k) isTRUE(run_steps[[k]]), logical(1)))) {
  .wc_remaining <- .wc2026_fixtures_remaining(cache_dir)
  # Say what happened either way. Silently overriding a workflow's explicit
  # TRUE is the same failure shape as the bug above: it looks like it worked.
  if (is.na(.wc_remaining)) {
    message(sprintf(
      "\n[%s] WC2026 gate: cannot read %s -- leaving steps 11/12b/12c as configured.",
      format(Sys.time(), "%H:%M:%S"),
      file.path(cache_dir, "07_predictions.rds")))
  } else if (.wc_remaining > 0L) {
    message(sprintf(
      "\n[%s] WC2026 gate: %d fixture(s) remaining -- steps 11/12b/12c will run.",
      format(Sys.time(), "%H:%M:%S"), .wc_remaining))
  } else {
    message(sprintf(paste0(
      "\n[%s] WC2026 gate: 0 unplayed WC2026 fixtures in 07_predictions.rds.\n",
      "  The tournament is over -- DISABLING steps 11/12b/12c for this run.\n",
      "  (Simulating a finished tournament produced champion odds off zero\n",
      "  group-stage predictions, and aborts in build_knockout_lookup().)\n",
      "  Step 12 still runs -- it exports match history/predictions, not a\n",
      "  simulation, and its own step-11-dependent sections self-skip."),
      format(Sys.time(), "%H:%M:%S")))
    for (.k in .wc_steps) run_steps[[.k]] <- FALSE
  }
}

# 14d. Step 11: Simulate WC 2026 ----
# Non-fatal (run_pred_step_optional): a WC failure must never block step 13.

step_results[[11]] <- run_pred_step_optional("simulate_wc2026", 11, function() {
  source("data-raw/match-predictions-opta/11_simulate_wc2026.R", local = TRUE)
})

# 14e. Step 12: Export WC 2026 Blog Data ----

step_results[[12]] <- run_pred_step_optional("export_wc2026_blog", 12, function() {
  source("data-raw/match-predictions-opta/12_export_wc2026_blog.R", local = TRUE)
})

# 14f. Step 12b: Snapshot WC 2026 Minutes ----
# Archive a dated copy of wc2026_squads.parquet to the wc2026-minutes-history
# release and diff it against the previous snapshot (group-stage drift tracking).
# Runs after step 12, which writes the squads file this reads.

step_results[["12b"]] <- run_pred_step_optional("snapshot_wc_minutes", "12b", function() {
  source("data-raw/match-predictions-opta/12b_snapshot_wc_minutes.R", local = TRUE)
})

# 14g. Step 12c: Snapshot WC 2026 Team Strength ----
# Archive a dated copy of wc2026_team_strength.parquet (ELO + p_champ + ratings)
# to the wc2026-strength-history release and diff it against the previous
# snapshot (tournament ELO/champion-odds drift tracking). Runs after step 12,
# which writes the team_strength file this reads.

step_results[["12c"]] <- run_pred_step_optional("snapshot_wc_strength", "12c", function() {
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
