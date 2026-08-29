# Regression test for the Elo split-identity bug found verifying panna#201:
# `01_build_fixture_results.R`'s team-name canonicalization block computed a
# team_id -> canonical-name map from `results_clean` (played matches), but
# only applied it to `fixtures_clean` (upcoming fixtures) -- `results_clean`
# itself kept whatever raw name spelling each row's lineup happened to carry.
# Real-world instances found live in `team_strength.csv` (2026-08-29): the
# same team_id showing up as both "Angers"/"Angers SCO", "Gaziantep"/
# "Gaziantep FK", "Bournemouth"/"AFC Bournemouth", etc. Both
# 03_team_rolling_features.R and 12d_export_domestic_team_strength.R call
# compute_match_elos() directly on `01_fixture_results.rds`'s home_team/
# away_team strings, so an uncanonicalized name split fragments that team's
# match history into two separate (and both wrong) Elo ratings -- in
# 03_team_rolling_features.R this is the rating that feeds live predictions.
#
# Uses the same source()-with-stubbed-loaders pattern as
# test-fixture-results-missing-events.R (panna#166).

test_that("results_clean's own home/away team names are canonicalized by team_id (Elo split-identity fix)", {
  script_path <- testthat::test_path(
    "..", "..", "data-raw", "match-predictions-opta", "01_build_fixture_results.R"
  )
  if (!file.exists(script_path)) {
    testthat::skip("01_build_fixture_results.R not available (data-raw/ excluded from R CMD check tarball)")
  }
  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)

  test_league <- "AFCON"
  test_season <- "2021 Cameroon"

  # Team "h1" appears as "Angers" in 2 matches and "Angers SCO" in 1 --
  # majority-vote canonical name is "Angers". Away team "a1" is spelled
  # consistently so it's an unaffected control.
  list_opta_seasons <- function(league, ...) {
    if (identical(league, test_league)) test_season else character(0)
  }
  load_opta_lineups <- function(league, season, ...) {
    data.frame(
      match_id = c("m1", "m1", "m2", "m2", "m3", "m3"),
      team_id = c("h1", "a1", "h1", "a1", "h1", "a1"),
      team_name = c("Angers", "Away FC", "Angers", "Away FC", "Angers SCO", "Away FC"),
      team_position = c("Home", "Away", "Home", "Away", "Home", "Away"),
      match_date = c("2024-01-01", "2024-01-01", "2024-01-08", "2024-01-08",
                      "2024-01-15", "2024-01-15"),
      is_starter = TRUE,
      stringsAsFactors = FALSE
    )
  }
  load_opta_events <- function(league, season, ...) {
    cli::cli_abort(
      sprintf("No data found for %s season %s.", league, season),
      class = "vb_error_absent"
    )
  }
  load_opta_fixtures <- function(league, season, status = NULL, ...) {
    data.frame(
      match_id = c("m1", "m2", "m3"),
      home_score = c(2, 1, 0),
      away_score = c(1, 1, 0),
      match_status = c("Played", "Played", "Played"),
      match_date = c("2024-01-01", "2024-01-08", "2024-01-15"),
      stringsAsFactors = FALSE
    )
  }

  scratch <- withr::local_tempdir()
  withr::local_dir(scratch)

  testthat::local_mocked_bindings(
    load_all = function(...) invisible(NULL),
    .package = "devtools"
  )

  leagues <- test_league
  seasons <- NULL
  min_season <- NULL
  force_rebuild <- TRUE

  suppressWarnings(suppressMessages(
    source(script_path, local = TRUE)
  ))

  expect_true(exists("fixture_results", inherits = FALSE))
  expect_setequal(fixture_results$match_id, c("m1", "m2", "m3"))

  # The bug: m3's home_team stayed "Angers SCO" because results_clean was
  # never rewritten, only fixtures_clean was. Fixed: every row for team_id
  # "h1" reports the same canonical name.
  by_id <- fixture_results[order(fixture_results$match_id), ]
  expect_equal(unique(by_id$home_team), "Angers")
  expect_equal(unique(by_id$away_team), "Away FC")
})
