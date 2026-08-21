# Tests for 12_export_wc2026_blog.R surviving an absent step-11 simulation.
#
# panna#194 (2026-08-21 regression, review fix): the WC2026 liveness gate in
# run_predictions_opta.R disabled step 12 alongside steps 11/12b/12c once the
# tournament ended. That was wrong -- step 12's sections 2/2b (match
# predictions) read ONLY 07_predictions.rds and are exactly as valid
# post-tournament as pre- (they're how the blog browses a finished World
# Cup). Only sections 3, 4, 5 and 5c read step 11's outputs
# (wc2026_simulation.parquet, wc2026_group_expectations.parquet,
# wc2026_bt_ratings.parquet) directly, via bare read_parquet() calls that
# hard-error if those files are absent.
#
# The fix: step 12 stays enabled always; 12_export_wc2026_blog.R itself now
# computes a `.wc11_available` flag from whether step 11's three output files
# exist, and gates sections 3-5c and 8 on that flag instead of ever hard-
# erroring. This test proves the absent-simulation path: with only
# 07_predictions.rds in the cache (no wc2026_simulation.parquet et al.),
# sourcing the step must not error, must still publish the match-prediction
# exports, and must NOT publish the step-11-dependent ones.
#
# local_no_reload() stubs devtools::load_all() before sourcing the script --
# see test-publish-gating.R's copy of this helper for the full rationale
# (the script's own load_all() header reloads the package namespace
# mid-suite, discarding this test's mocks and breaking every test file that
# runs after it). Defined again here rather than shared across files because
# testthat does not guarantee one test file's top-level helpers are visible
# to another's.
local_no_reload <- function(env = parent.frame()) {
  if (!requireNamespace("devtools", quietly = TRUE)) return(invisible(NULL))
  testthat::local_mocked_bindings(
    load_all = function(...) invisible(NULL),
    .package = "devtools",
    .env = env
  )
}

.wc12_preds_fixture <- function() {
  data.frame(
    league = WC2026_LEAGUE,
    season = WC2026_SEASON_LABEL,
    match_date = as.Date("2026-06-15") + 0:1,
    home_team = c("Mexico", "Czechia"),
    away_team = c("Czechia", "Mexico"),
    prob_H = c(0.45, 0.30),
    prob_D = c(0.30, 0.35),
    prob_A = c(0.25, 0.35),
    pred_home_goals = c(1.6, 1.1),
    pred_away_goals = c(1.0, 1.4),
    predicted_result = c("H", "A"),
    stringsAsFactors = FALSE
  )
}

test_that("step 12 exports match predictions when step 11 has not run", {
  skip_if_not_installed("arrow")
  local_no_reload()

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "12_export_wc2026_blog.R")
  skip_if_not(file.exists(script), "12_export_wc2026_blog.R not found")

  cache_dir <- withr::local_tempdir()
  saveRDS(.wc12_preds_fixture(), file.path(cache_dir, "07_predictions.rds"))
  # Deliberately absent: wc2026_simulation.parquet, wc2026_group_expectations.
  # parquet, wc2026_bt_ratings.parquet -- exactly the state step 11 leaves a
  # cache in when it has never run (or, before this fix, when the liveness
  # gate disabled it).

  expect_no_error(source(script, local = TRUE))

  # Step-11-INdependent exports: must be written.
  expect_true(file.exists(file.path(cache_dir, "wc2026_predictions.parquet")))
  expect_true(file.exists(file.path(cache_dir, "wc_history_predictions.parquet")))

  preds <- arrow::read_parquet(file.path(cache_dir, "wc2026_predictions.parquet"))
  expect_equal(nrow(preds), 2L)
  expect_setequal(preds$home_team, c("Mexico", "Czechia"))

  hist <- arrow::read_parquet(file.path(cache_dir, "wc_history_predictions.parquet"))
  expect_equal(nrow(hist), 2L)

  # Step-11-dependent exports: must NOT be written -- there is nothing to
  # publish, and (pre-fix) getting here at all meant the script had already
  # hard-errored on a missing wc2026_simulation.parquet.
  expect_false(file.exists(file.path(cache_dir, "wc2026_simulation.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_groups.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_team_strength.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_squads.parquet")))
})

test_that("step 12's .wc11_available flag requires all three step-11 outputs, not just one", {
  skip_if_not_installed("arrow")
  local_no_reload()

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "12_export_wc2026_blog.R")
  skip_if_not(file.exists(script), "12_export_wc2026_blog.R not found")

  cache_dir <- withr::local_tempdir()
  saveRDS(.wc12_preds_fixture(), file.path(cache_dir, "07_predictions.rds"))
  # Only ONE of the three step-11 outputs present (a torn/partial step-11
  # run) -- must still be treated as unavailable, not spuriously "ready".
  arrow::write_parquet(data.frame(team = "Mexico", p_champ = 0.01),
                       file.path(cache_dir, "wc2026_simulation.parquet"))

  expect_no_error(source(script, local = TRUE))
  expect_true(file.exists(file.path(cache_dir, "wc2026_predictions.parquet")))
  # wc2026_simulation.parquet exists going in, but section 3 must not have
  # re-run against a partial step-11 output (no wc2026_team_strength.parquet,
  # which only section 5 -- gated on the same flag -- would produce).
  expect_false(file.exists(file.path(cache_dir, "wc2026_team_strength.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_squads.parquet")))
})


test_that("all three step-11 files present but no commit marker is still 'not ready'", {
  # The case file.exists() alone cannot see, and the one that actually bites.
  # Step 11 writes wc2026_bt_ratings.parquet BEFORE simulate_world_cup() and
  # the other two AFTER. If the simulation throws, the cache is left with a
  # FRESH bt file beside STALE sim files -- all three present. Section 5 would
  # then merge two vintages into one published wc2026_team_strength.parquet.
  # Step 11 has been non-fatal since panna#194, so this is routine, not rare.
  skip_if_not_installed("arrow")
  local_no_reload()

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "12_export_wc2026_blog.R")
  skip_if_not(file.exists(script), "12_export_wc2026_blog.R not found")

  cache_dir <- withr::local_tempdir()
  saveRDS(.wc12_preds_fixture(), file.path(cache_dir, "07_predictions.rds"))
  for (f in c("wc2026_simulation.parquet", "wc2026_group_expectations.parquet",
              "wc2026_bt_ratings.parquet")) {
    arrow::write_parquet(data.frame(team = "Mexico", p_champ = 0.01, rating = 1),
                         file.path(cache_dir, f))
  }
  # No .wc11_outputs_complete marker: no single run produced this set.

  expect_no_error(source(script, local = TRUE))
  expect_true(file.exists(file.path(cache_dir, "wc2026_predictions.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_team_strength.parquet")))
  expect_false(file.exists(file.path(cache_dir, "wc2026_squads.parquet")))
})

test_that("a skipped simulation is not re-stamped with this run's build_id", {
  # build_id is a per-run stamp, not a data vintage. Stamping a file the run
  # did not rewrite claims a freshness that isn't there, and gives two
  # different vintages the same id -- which is exactly the signal the blog's
  # detectMixedBuild() reads to spot a torn publish.
  skip_if_not_installed("arrow")
  local_no_reload()

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "12_export_wc2026_blog.R")
  skip_if_not(file.exists(script), "12_export_wc2026_blog.R not found")

  cache_dir <- withr::local_tempdir()
  saveRDS(.wc12_preds_fixture(), file.path(cache_dir, "07_predictions.rds"))
  sim_path <- file.path(cache_dir, "wc2026_simulation.parquet")
  arrow::write_parquet(data.frame(team = "Mexico", p_champ = 0.01,
                                  build_id = "PRIOR-RUN"), sim_path)

  expect_no_error(source(script, local = TRUE))

  # Untouched: same build_id it went in with.
  expect_identical(arrow::read_parquet(sim_path)$build_id, "PRIOR-RUN")
  # And the files this run DID write carry a real stamp.
  fresh <- arrow::read_parquet(file.path(cache_dir, "wc2026_predictions.parquet"))
  expect_true("build_id" %in% names(fresh))
  expect_false(any(fresh$build_id == "PRIOR-RUN"))
})
