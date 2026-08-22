# Tests for 04b_export_match_features.R.
#
# This step exists because 04_match_dataset.rds is a build artifact the GHA
# runner discards, which left panna#190 and panna#192 both unable to check what
# the model actually saw for a fixture. The value of the export is entirely in
# WHICH columns it carries, so that is what these tests pin.

local_no_reload <- function(env = parent.frame()) {
  if (!requireNamespace("devtools", quietly = TRUE)) return(invisible(NULL))
  testthat::local_mocked_bindings(
    load_all = function(...) invisible(NULL), .package = "devtools", .env = env)
}

.mf_fixture <- function(dir) {
  n <- 40L
  md <- data.frame(
    match_id = sprintf("m%03d", seq_len(n)), match_date = Sys.Date() + seq_len(n),
    league = "ENG", season = "2026-2027", season_end_year = 2027L,
    split = rep(c("train", "fixture"), length.out = n),
    match_status = rep(c("Played", "Fixture"), length.out = n),
    home_team = "A", away_team = "B",
    home_goals = 1L, away_goals = 2L,
    home_sum_panna = 1, away_sum_panna = 2,
    home_elo = 1500, away_elo = 1400, elo_diff = 100, panna_diff = -1,
    home_avg_psr = 0.5, away_avg_psr = 0.4,
    diff_form = 0.1, some_other_col = 9,
    stringsAsFactors = FALSE)
  saveRDS(md, file.path(dir, "04_match_dataset.rds"))
  saveRDS(list(feature_cols = c("home_sum_panna", "away_sum_panna", "home_elo",
                                "away_elo", "elo_diff", "panna_diff",
                                "home_avg_psr", "away_avg_psr", "diff_form",
                                "some_other_col")),
          file.path(dir, "05_goals_model.rds"))
  dir
}

.run_04b <- function(cache_dir, publish = TRUE) {
  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "04b_export_match_features.R")
  testthat::skip_if_not(file.exists(script), "04b script not found")
  if (publish) {
    assign("publish_files",
           list(predictions_latest = character(0), blog_latest = character(0)),
           envir = globalenv())
  } else if (exists("publish_files", envir = globalenv())) {
    rm("publish_files", envir = globalenv())
  }
  # The step must run inside a closure whose enclosing chain reaches globalenv,
  # exactly as run_pipeline_step() wraps it -- that is what lets the script's
  # `publish_files$... <<-` find the accumulator. Sourcing at top level instead
  # makes `<<-` skip globalenv and error, so this test would prove nothing.
  runner <- function() source(script, local = TRUE)
  environment(runner) <- list2env(list(cache_dir = cache_dir, script = script),
                                  parent = globalenv())
  runner()
  invisible(NULL)
}

test_that("04b exports the strength features and the actuals, and nothing else", {
  skip_if_not_installed("arrow")
  local_no_reload()
  d <- .mf_fixture(withr::local_tempdir())
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  .run_04b(d)
  out <- arrow::read_parquet(file.path(d, "match_features.parquet"))

  expect_equal(nrow(out), 40L)
  # Strength features: the whole point of the export.
  expect_true(all(c("home_sum_panna", "away_sum_panna", "home_elo", "away_elo",
                    "home_avg_psr", "away_avg_psr") %in% names(out)))
  # Derived differentials are deliberately dropped: every *_diff is exactly
  # home_<base> - away_<base>, both sides are here, and a consumer can
  # recompute it. Verified on the first published build (elo_diff ==
  # home_elo - away_elo for 100.0% of 58,780 rows). This is the only
  # departure from mirroring 07_predict_fixtures.R's guard set, and it is
  # safe only because it is derivable -- if this assertion is ever relaxed to
  # cover a NON-derived column, that is a bug, not a tidy-up.
  expect_false(any(c("elo_diff", "panna_diff") %in% names(out)))
  expect_true(all(c("home_elo", "away_elo") %in% names(out)))
  # Actuals: without these the export cannot be calibrated against outcomes,
  # which is most of why panna#192 wanted it.
  expect_true(all(c("home_goals", "away_goals") %in% names(out)))
  # Identity: needed to join back to predictions.parquet on match_id.
  expect_true(all(c("match_id", "league", "season", "split") %in% names(out)))
  # A feature that is neither strength nor identity must not ride along --
  # otherwise this quietly becomes a full match-dataset dump.
  expect_false("some_other_col" %in% names(out))
})

test_that("04b registers exactly one file for publish, and only when asked", {
  skip_if_not_installed("arrow")
  local_no_reload()
  d <- .mf_fixture(withr::local_tempdir())
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  .run_04b(d)
  pf <- get("publish_files", envir = globalenv())
  expect_length(pf$predictions_latest, 1L)
  expect_match(pf$predictions_latest, "match_features[.]parquet$")
  # It is a predictions diagnostic, not blog data.
  expect_length(pf$blog_latest, 0L)
})

test_that("04b aborts rather than shipping an empty diagnostic", {
  # A strength subset that matched nothing would still write a valid parquet
  # and still report SUCCESS -- the exact "looks like it worked" shape this
  # repo keeps getting bitten by. It must fail loudly instead.
  skip_if_not_installed("arrow")
  local_no_reload()
  d <- withr::local_tempdir()
  .mf_fixture(d)
  saveRDS(list(feature_cols = c("nothing_matches_here", "nor_this")),
          file.path(d, "05_goals_model.rds"))
  expect_error(.run_04b(d, publish = FALSE), "strength feature columns")
})
