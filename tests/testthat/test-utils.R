# Tests for utility functions

test_that("safe_divide handles division by zero", {
  expect_equal(safe_divide(10, 2), 5)
  expect_equal(safe_divide(10, 0), 0)
  # NA_real_ is returned when default = NA (since result is numeric)
  expect_true(is.na(safe_divide(10, 0, default = NA)))
  expect_equal(safe_divide(c(10, 20), c(2, 0)), c(5, 0))
})

test_that("validate_seasons accepts valid seasons", {
  expect_true(validate_seasons("2023-2024"))
  expect_true(validate_seasons(c("2022-2023", "2023-2024")))
})

test_that("validate_seasons rejects invalid seasons", {
  expect_error(validate_seasons("2016-2017"), "2017-18")
  expect_error(validate_seasons("2023"), "YYYY-YYYY")
  expect_error(validate_seasons("2023-2025"), "start year \\+ 1")
})

test_that("standardize_player_names cleans names", {
  expect_equal(
    standardize_player_names("  MOHAMED SALAH  "),
    "Mohamed Salah"
  )
})

test_that("standardize_team_names maps common variations", {
  expect_equal(standardize_team_names("Man Utd"), "Manchester United")
  expect_equal(standardize_team_names("Spurs"), "Tottenham Hotspur")
  expect_equal(standardize_team_names("Wolves"), "Wolverhampton Wanderers")
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

test_that("extract_fbref_player_id extracts 8-char hex ID", {
  # Valid FBref hrefs
  expect_equal(
    extract_fbref_player_id("/players/d080ed5e/Kylian-Mbappe"),
    "d080ed5e"
  )
  expect_equal(
    extract_fbref_player_id("/players/abc12345/Some-Player"),
    "abc12345"
  )

  # Invalid hrefs return NA
  expect_true(is.na(extract_fbref_player_id("/some/other/path")))
  expect_true(is.na(extract_fbref_player_id("not-a-url")))

  # Handles vectors
  hrefs <- c("/players/d080ed5e/Player1", "/players/abc12345/Player2")
  result <- extract_fbref_player_id(hrefs)
  expect_equal(result, c("d080ed5e", "abc12345"))
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


# =============================================================================
# Tests for standardize_data_columns()
# =============================================================================

test_that("standardize_data_columns renames columns correctly", {
  df <- data.frame(squad = "Arsenal", min = 90, player = "Saka")
  result <- standardize_data_columns(df, list(
    team = c("squad"),
    minutes = c("min"),
    player_name = c("player")
  ))

  expect_true("team" %in% names(result))
  expect_true("minutes" %in% names(result))
  expect_true("player_name" %in% names(result))
  expect_false("squad" %in% names(result))
  expect_false("min" %in% names(result))
  expect_false("player" %in% names(result))
})

test_that("standardize_data_columns preserves existing canonical names", {
  df <- data.frame(team = "Arsenal", squad = "should_be_ignored")
  result <- standardize_data_columns(df, list(
    team = c("squad", "team_name")
  ))

  expect_true("team" %in% names(result))
  expect_true("squad" %in% names(result))  # Not renamed, team already exists
  expect_equal(result$team, "Arsenal")
})

test_that("standardize_data_columns handles multiple alternatives", {
  # First alternative found
  df1 <- data.frame(squad = "Arsenal")
  result1 <- standardize_data_columns(df1, list(team = c("squad", "team_name")))
  expect_true("team" %in% names(result1))

  # Second alternative found
  df2 <- data.frame(team_name = "Chelsea")
  result2 <- standardize_data_columns(df2, list(team = c("squad", "team_name")))
  expect_true("team" %in% names(result2))
})

test_that("standardize_data_columns handles NULL and non-dataframe input", {
  expect_null(standardize_data_columns(NULL, list(team = "squad")))
  expect_equal(standardize_data_columns("not a df", list(team = "squad")), "not a df")
})

test_that("standardize_data_columns works with data.table", {
  skip_if_not_installed("data.table")

  dt <- data.table::data.table(squad = "Arsenal", min = 90)
  result <- standardize_data_columns(dt, list(
    team = c("squad"),
    minutes = c("min")
  ))

  expect_true("team" %in% names(result))
  expect_true("minutes" %in% names(result))
  expect_s3_class(result, "data.table")
})


# =============================================================================
# Tests for default_column_map()
# =============================================================================

test_that("default_column_map returns expected structure", {
  map <- default_column_map()

  expect_type(map, "list")
  expect_true("team" %in% names(map))
  expect_true("player_name" %in% names(map))
  expect_true("minutes" %in% names(map))
  expect_true("position" %in% names(map))
})

test_that("default_column_map contains common variations", {
  map <- default_column_map()

  expect_true("squad" %in% map$team)
  expect_true("player" %in% map$player_name)
  expect_true("min" %in% map$minutes)
  expect_true("pos" %in% map$position)
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
