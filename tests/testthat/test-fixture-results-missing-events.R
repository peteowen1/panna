# Tests for panna#166: a missing opta_events.parquet table for one (league,
# season) must not discard that season's matches from 01_build_fixture_results.R.
#
# Root cause (panna#166 diagnosis): load_opta_events() throws vb_error_absent
# BEFORE load_opta_fixtures() is reached inside the per-(league, season) loop
# (data-raw/match-predictions-opta/01_build_fixture_results.R, section 5). The
# whole per-season block is wrapped in an outer tryCatch that only message()s,
# so the season silently vanishes on every run even though fixtures + lineups
# are present and complete -- exactly the AFCON "2021 Cameroon" case (52
# fixtures, 2,320 lineup rows, 86,748 raw events, 0 rows in the derived
# opta_events.parquet).
#
# This test sources the real pipeline script with the 4 Opta loader functions
# it calls replaced by local stubs, relying on plain R lexical scoping (the
# stubs are defined in the same environment the script is sourced into, so
# calls resolve to them ahead of the package namespace) rather than
# testthat::local_mocked_bindings() -- the script's own `devtools::load_all()`
# call partway through would otherwise be free to invalidate a namespace-level
# mock.

test_that("a season with fixtures+lineups but no events still yields matches (panna#166)", {
  script_path <- testthat::test_path(
    "..", "..", "data-raw", "match-predictions-opta", "01_build_fixture_results.R"
  )
  if (!file.exists(script_path)) {
    testthat::skip("01_build_fixture_results.R not available (data-raw/ excluded from R CMD check tarball)")
  }
  # Resolve to an absolute path BEFORE the working directory changes below --
  # test_path() can return a path relative to the pre-withr::local_dir() cwd.
  script_path <- normalizePath(script_path, winslash = "/", mustWork = TRUE)

  # Use a real league code ("AFCON") -- the script calls to_opta_league()
  # directly (not one of the 4 stubbed loaders below), which validates
  # against the OPTA_LEAGUES map and rejects an invented code.
  # "2021 Cameroon" mirrors the real panna#166 season string.
  test_league <- "AFCON"
  test_season <- "2021 Cameroon"

  # --- Stubs: fixtures + lineups complete, events absent (vb_error_absent) ---
  list_opta_seasons <- function(league, ...) {
    if (identical(league, test_league)) test_season else character(0)
  }
  load_opta_lineups <- function(league, season, ...) {
    data.frame(
      match_id = c("m1", "m1", "m2", "m2"),
      team_id = c("h1", "a1", "h1", "a1"),
      team_name = c("Home FC", "Away FC", "Home FC", "Away FC"),
      team_position = c("Home", "Away", "Home", "Away"),
      match_date = c("2024-01-01", "2024-01-01", "2024-01-08", "2024-01-08"),
      is_starter = TRUE,
      stringsAsFactors = FALSE
    )
  }
  load_opta_events <- function(league, season, ...) {
    # Reproduces the real failure mode for AFCON "2021 Cameroon": the
    # consolidated events table exists but has 0 rows for this (league,
    # season), and load_opta_table() classifies that as vb_error_absent
    # (verified interactively against R/opta_loaders.R:691-700).
    cli::cli_abort(
      sprintf("No data found for %s season %s.", league, season),
      class = "vb_error_absent"
    )
  }
  load_opta_fixtures <- function(league, season, status = NULL, ...) {
    # Called from 3 sites in the script with different arg sets (section 5,
    # 5b override, section 7 fixture-load) -- one stub signature covers all.
    data.frame(
      match_id = c("m1", "m2"),
      home_score = c(2, 0),
      away_score = c(1, 0),
      match_status = c("Played", "Played"),
      match_date = c("2024-01-01", "2024-01-08"),
      stringsAsFactors = FALSE
    )
  }

  # Isolate cache_dir / rapm_cache (both relative, hardcoded paths inside the
  # script) in a scratch directory so the test can't read or clobber the real
  # repo's data-raw/cache-*/ contents.
  scratch <- withr::local_tempdir()
  withr::local_dir(scratch)

  # The script's own `devtools::load_all()` call (line 26) resolves paths
  # from the CURRENT working directory, which is now the scratch dir with no
  # DESCRIPTION file. The package is already loaded for this test run, so
  # that reload is a no-op we can safely skip.
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
  expect_setequal(fixture_results$match_id, c("m1", "m2"))
  expect_equal(sum(fixture_results$match_status == "Played"), 2L)
  # Scores must come from fixtures (the primary source), not be dropped or
  # fabricated as 0-0 by the empty-events fallback.
  m1 <- fixture_results[fixture_results$match_id == "m1", ]
  expect_equal(m1$home_goals, 2L)
  expect_equal(m1$away_goals, 1L)
})
