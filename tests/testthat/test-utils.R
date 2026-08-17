# Tests for utility functions

test_that("safe_divide handles division by zero", {
  expect_equal(safe_divide(10, 2), 5)
  # Default is 0 (safe for stat tables and model features)
  expect_equal(safe_divide(10, 0), 0)
  # Explicit default = NA for contexts where unknown is preferred
  expect_true(is.na(safe_divide(10, 0, default = NA_real_)))
  expect_equal(safe_divide(c(10, 20), c(2, 0)), c(5, 0))
  expect_equal(safe_divide(c(10, 20), c(2, 0), default = NA_real_), c(5, NA))
})

test_that("safe_divide preserves input NAs", {
  expect_true(is.na(safe_divide(NA, 5)))
  expect_true(is.na(safe_divide(5, NA)))
  # NA input + zero denominator: input NA takes precedence
  expect_true(is.na(safe_divide(NA, 0)))
  expect_equal(safe_divide(c(10, NA, 5), c(2, 3, 0)), c(5, NA, 0))
  expect_equal(safe_divide(c(10, NA, 5), c(2, 3, 0), default = NA_real_), c(5, NA, NA))
})

test_that("validate_seasons accepts valid seasons", {
  expect_true(validate_seasons("2023-2024"))
  expect_true(validate_seasons(c("2022-2023", "2023-2024")))
})

test_that("validate_seasons rejects invalid seasons", {
  expect_error(validate_seasons("2016-2017"), "2017-2018")
  expect_error(validate_seasons("2023"), "YYYY-YYYY")
  expect_error(validate_seasons("2023-2025"), "start year \\+ 1")
})

test_that("validate_seasons respects min_year and source_name", {
  expect_true(validate_seasons("2013-2014", min_year = 2013, source_name = "Opta"))
  expect_error(validate_seasons("2012-2013", min_year = 2013, source_name = "Opta"), "Opta")
})

test_that("create_match_id creates unique identifiers", {
  id <- create_match_id("2023-2024", "2024-01-01", "Arsenal", "Liverpool")
  expect_type(id, "character")
  expect_true(grepl("2023-2024", id))
  expect_true(grepl("Arsenal", id))
})

test_that("per_90 calculates correctly", {
  expect_equal(per_90(10, 90), 10)
  expect_equal(per_90(10, 45), 20)
  expect_equal(per_90(0, 90), 0)
  # Zero minutes returns 0 (not NA) — per_90 feeds into model matrices
  expect_equal(per_90(10, 0), 0)
})

test_that("clean_player_name normalizes case and whitespace", {
  # Basic normalization
  expect_equal(clean_player_name("Kylian Mbappé"), "kylianmbappé")
  expect_equal(clean_player_name("kylian mbappé"), "kylianmbappé")
  expect_equal(clean_player_name("KYLIAN MBAPPÉ"), "kylianmbappé")

  # Multiple spaces
  expect_equal(clean_player_name("Mohamed  Salah"), "mohamedsalah")

  # Leading/trailing whitespace
  expect_equal(clean_player_name("  Kylian Mbappé"), "kylianmbappé")
  expect_equal(clean_player_name("Kylian Mbappé  "), "kylianmbappé")
  expect_equal(clean_player_name("   kylian Mbappé   "), "kylianmbappé")

  # Non-breaking spaces (U+00A0) - common from HTML scraping
  expect_equal(clean_player_name("\u00A0\u00A0\u00A0lionel Messi"), "lionelmessi")
  expect_equal(clean_player_name("Lionel\u00A0Messi"), "lionelmessi")

  # Handles vectors
  result <- clean_player_name(c("Lionel Messi", "lionel messi"))
  expect_equal(result[1], result[2])
})


# =============================================================================
# Tests for constants
# =============================================================================

test_that("match structure constants have correct values", {
  expect_equal(MINUTES_PER_MATCH, 90L)
  expect_equal(PLAYERS_PER_TEAM, 11L)
  expect_equal(HALFTIME_MINUTE, 45L)
})

test_that("model default constants have correct values", {
  expect_equal(MIN_MINUTES_RAPM, 90L)
  expect_equal(MIN_MINUTES_SPM, 450L)
  expect_equal(MIN_MINUTES_FEATURES, 180L)
  expect_equal(MIN_GAMES_FOR_PADDING, 10L)
  expect_equal(MIN_SHOTS_FOR_FINISHING, 20L)
  expect_equal(MIN_WEIGHT_DURATION, 0.01)
})

test_that("statistical constants have correct values", {
  expect_equal(BETA_PRIOR_ALPHA, 5L)
  expect_equal(CONFIDENCE_LEVEL, 0.95)
})

test_that("sequence estimation constants have correct values", {
  expect_equal(TOUCHES_PER_SEQUENCE, 5L)
  expect_equal(MIN_SEQUENCES_PER_MATCH, 20L)
})

test_that("xG model bounds have correct values", {
  expect_equal(XG_MIN, 0.01)
  expect_equal(XG_MAX, 0.75)
  expect_true(XG_MIN < XG_MAX)
})


# =============================================================================
# Tests for build_where_clause()
# =============================================================================

test_that("build_where_clause handles single filter", {
  result <- build_where_clause(list(league = "ENG"))
  expect_equal(result, "WHERE league = 'ENG'")
})

test_that("build_where_clause handles multiple filters", {
  result <- build_where_clause(list(league = "ENG", season = "2023-2024"))
  expect_equal(result, "WHERE league = 'ENG' AND season = '2023-2024'")
})

test_that("build_where_clause handles NULL values", {
  result <- build_where_clause(list(league = NULL, season = "2023-2024"))
  expect_equal(result, "WHERE season = '2023-2024'")

  result2 <- build_where_clause(list(league = NULL, season = NULL))
  expect_equal(result2, "")
})

test_that("build_where_clause handles empty list", {
  expect_equal(build_where_clause(list()), "")
  expect_equal(build_where_clause(NULL), "")
})

test_that("build_where_clause handles numeric values", {
  result <- build_where_clause(list(min_goals = 5))
  expect_equal(result, "WHERE min_goals = 5")
})

test_that("build_where_clause respects prefix parameter", {
  result <- build_where_clause(list(league = "ENG"), prefix = FALSE)
  expect_equal(result, "league = 'ENG'")

  result2 <- build_where_clause(list(league = "ENG", season = "2023-2024"), prefix = FALSE)
  expect_equal(result2, "league = 'ENG' AND season = '2023-2024'")
})

test_that("build_where_clause handles multi-value IN clause", {
  result <- build_where_clause(list(league = c("ENG", "ESP")))
  expect_match(result, "IN")
  expect_match(result, "'ENG'")
  expect_match(result, "'ESP'")
})



# =============================================================================
# Tests for validate_dataframe()
# =============================================================================

test_that("validate_dataframe accepts valid data frames", {
  df <- data.frame(player_name = "Messi", minutes = 90)
  expect_true(validate_dataframe(df))
  expect_true(validate_dataframe(df, required_cols = c("player_name", "minutes")))
})

test_that("validate_dataframe rejects NULL input", {
  expect_error(
    validate_dataframe(NULL),
    "cannot be NULL"
  )
})

test_that("validate_dataframe rejects non-data.frame input", {
  expect_error(
    validate_dataframe(list(a = 1, b = 2)),
    "must be a data frame"
  )
  expect_error(
    validate_dataframe("not a df"),
    "must be a data frame"
  )
})

test_that("validate_dataframe rejects empty data frames", {
  empty_df <- data.frame()
  expect_error(
    validate_dataframe(empty_df),
    "at least 1 row"
  )
})

test_that("validate_dataframe respects min_rows parameter", {
  df <- data.frame(x = 1:3)
  expect_true(validate_dataframe(df, min_rows = 3))
  expect_error(
    validate_dataframe(df, min_rows = 5),
    "at least 5 rows"
  )
})

test_that("validate_dataframe checks required columns", {
  df <- data.frame(player_name = "Messi", minutes = 90)
  expect_error(
    validate_dataframe(df, required_cols = "nonexistent"),
    "missing required column"
  )
  expect_error(
    validate_dataframe(df, required_cols = c("player_name", "goals")),
    "Missing.*goals"
  )
})

test_that("validate_dataframe uses custom arg_name in errors", {
  expect_error(
    validate_dataframe(NULL, arg_name = "my_data"),
    "my_data.*cannot be NULL"
  )
})


# =============================================================================
# Tests for extract_season_end_year()
# =============================================================================

test_that("extract_season_end_year handles standard and tournament formats", {
  expect_equal(extract_season_end_year("2023-2024"), 2024)
  expect_equal(extract_season_end_year("2018 Russia"), 2018)
  expect_true(is.na(extract_season_end_year("garbage")))
})

test_that("extract_season_end_year is vectorized", {
  # Was scalar-only until 2026-08-17: the `is.na(season) || !nzchar(season)`
  # guard is a hard error on length > 1 under R >= 4.3, so every one of the
  # ~40 call sites had to remember a vapply() wrapper and one that forgot was
  # a crash, not a wrong number.
  expect_equal(
    extract_season_end_year(c("2023-2024", "2018 Russia", "Intl_Friendlies_2024")),
    c(2024, 2018, 2024)
  )
  # NA/empty entries must not poison their neighbours.
  expect_equal(
    extract_season_end_year(c("2023-2024", NA, "", "garbage", "2020")),
    c(2024, NA, NA, NA, 2020)
  )
})

test_that("extract_season_end_year is type-stable on empty input", {
  # sapply(character(0), f) returns a LIST, which is how an empty season set
  # turns a downstream comparison into an error instead of an empty result.
  out <- extract_season_end_year(character(0))
  expect_type(out, "double")
  expect_length(out, 0)
})

test_that("extract_season_end_year covers all three label formats", {
  # The three formats that share one end year -- the trap documented in
  # panna/CLAUDE.md ("Season subsetting"): European "YYYY-YYYY", calendar-year
  # leagues "YYYY", and tournaments "YYYY Country" must all map to 2026.
  expect_equal(
    extract_season_end_year(c("2025-2026", "2026", "2026 Canada-Mexico-USA")),
    c(2026, 2026, 2026)
  )
  # Trailing-year format used by the intl-friendlies scrape.
  expect_equal(extract_season_end_year("Intl_Friendlies_2026"), 2026)
})

test_that(".season_end_year_for_date accepts a vector of dates", {
  # This composition passed a vectorized result straight into the scalar-only
  # helper -- a latent crash for any multi-date caller.
  out <- .season_end_year_for_date(as.Date(c("2025-09-01", "2026-03-01")))
  expect_length(out, 2)
  expect_true(all(is.finite(out)))
})


# ===========================================================================
# .log_rss — OS-level memory checkpoint (panna#128/#133 diagnostic)
# ===========================================================================

test_that(".log_rss prints a checkpoint line and returns RSS invisibly", {
  msgs <- capture_messages(res <- withVisible(.log_rss("unit-test")))
  expect_false(res$visible)
  # Numeric MB on Linux/mac, NA_real_ on Windows — both are valid returns
  expect_true(is.numeric(res$value) || is.na(res$value))
  expect_true(any(grepl("[rss] unit-test:", msgs, fixed = TRUE)))
  expect_true(any(grepl("R heap=", msgs)))
  # On the platform that matters (GHA Linux), RSS must be a real number
  if (file.exists("/proc/self/status")) {
    expect_true(is.finite(res$value) && res$value > 0)
  }
})

test_that(".log_rss respects verbose = FALSE", {
  msgs <- capture_messages(.log_rss("quiet", verbose = FALSE))
  expect_length(msgs, 0)
})
