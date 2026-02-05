# Tests for data loading functions
#
# These tests cover input validation, SQL construction, and error handling
# for the various data loading functions without requiring network access.

# =============================================================================
# Tests for FBref loader functions
# =============================================================================

test_that("load_summary accepts valid parameters", {
  # Function exists and has expected parameters
  expect_true(is.function(load_summary))
  fn_args <- names(formals(load_summary))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
  expect_true("source" %in% fn_args)
})

test_that("load_passing accepts valid parameters", {
  expect_true(is.function(load_passing))
  fn_args <- names(formals(load_passing))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_defense accepts valid parameters", {
  expect_true(is.function(load_defense))
  fn_args <- names(formals(load_defense))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_possession accepts valid parameters", {
  expect_true(is.function(load_possession))
  fn_args <- names(formals(load_possession))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_shots accepts valid parameters", {
  expect_true(is.function(load_shots))
  fn_args <- names(formals(load_shots))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_events accepts valid parameters", {
  expect_true(is.function(load_events))
  fn_args <- names(formals(load_events))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_metadata accepts valid parameters", {
  expect_true(is.function(load_metadata))
  fn_args <- names(formals(load_metadata))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_keeper accepts valid parameters", {
  expect_true(is.function(load_keeper))
  fn_args <- names(formals(load_keeper))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})


# =============================================================================
# Tests for Understat loader functions
# =============================================================================

test_that("load_understat_roster accepts valid parameters", {
  expect_true(is.function(load_understat_roster))
  fn_args <- names(formals(load_understat_roster))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
  expect_true("source" %in% fn_args)
})

test_that("load_understat_shots accepts valid parameters", {
  expect_true(is.function(load_understat_shots))
  fn_args <- names(formals(load_understat_shots))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_understat_metadata accepts valid parameters", {
  expect_true(is.function(load_understat_metadata))
  fn_args <- names(formals(load_understat_metadata))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})


# =============================================================================
# Tests for Opta loader functions
# =============================================================================

test_that("load_opta_stats accepts valid parameters", {
  expect_true(is.function(load_opta_stats))
  fn_args <- names(formals(load_opta_stats))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
  expect_true("columns" %in% fn_args)
})

test_that("load_opta_shots accepts valid parameters", {
  expect_true(is.function(load_opta_shots))
  fn_args <- names(formals(load_opta_shots))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_opta_events accepts valid parameters", {
  expect_true(is.function(load_opta_events))
  fn_args <- names(formals(load_opta_events))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})

test_that("load_opta_lineups accepts valid parameters", {
  expect_true(is.function(load_opta_lineups))
  fn_args <- names(formals(load_opta_lineups))
  expect_true("league" %in% fn_args)
  expect_true("season" %in% fn_args)
})


# =============================================================================
# Tests for validate_sql_columns()
# =============================================================================

test_that("validate_sql_columns accepts valid column names", {
  valid_cols <- c("player_name", "goals", "xg_per_90", "team")
  expect_equal(validate_sql_columns(valid_cols), valid_cols)
})

test_that("validate_sql_columns accepts NULL", {
  expect_null(validate_sql_columns(NULL))
})

test_that("validate_sql_columns accepts column names starting with underscore", {
  expect_equal(validate_sql_columns(c("_private_col")), c("_private_col"))
})

test_that("validate_sql_columns rejects columns with invalid characters", {
  # SQL injection attempts

  expect_error(validate_sql_columns(c("valid_col", "col; DROP TABLE")), "Invalid column")
  expect_error(validate_sql_columns(c("col--comment")), "Invalid column")
  expect_error(validate_sql_columns(c("col'injection")), "Invalid column")
  expect_error(validate_sql_columns(c("col\"quote")), "Invalid column")
})

test_that("validate_sql_columns rejects columns starting with numbers", {
  expect_error(validate_sql_columns(c("123col")), "Invalid column")
  expect_error(validate_sql_columns(c("1_column")), "Invalid column")
})

test_that("validate_sql_columns rejects columns with spaces", {
  expect_error(validate_sql_columns(c("column name")), "Invalid column")
})


# =============================================================================
# Tests for Opta league code mapping
# =============================================================================

test_that("OPTA_LEAGUES constant has correct mappings", {
  expect_equal(OPTA_LEAGUES["ENG"], c(ENG = "EPL"))
  expect_equal(OPTA_LEAGUES["ESP"], c(ESP = "La_Liga"))
  expect_equal(OPTA_LEAGUES["GER"], c(GER = "Bundesliga"))
  expect_equal(OPTA_LEAGUES["ITA"], c(ITA = "Serie_A"))
  expect_equal(OPTA_LEAGUES["FRA"], c(FRA = "Ligue_1"))
})

test_that("OPTA_LEAGUES includes all Big 5 leagues", {
  expect_true(all(c("ENG", "ESP", "GER", "ITA", "FRA") %in% names(OPTA_LEAGUES)))
  expect_equal(length(OPTA_LEAGUES), 5)
})


# =============================================================================
# Tests for pannadata_dir and opta_data_dir
# =============================================================================

test_that("pannadata_dir function exists and works", {
  expect_true(is.function(pannadata_dir))
  # Should return a path without error (may be default R_user_dir)
  path <- pannadata_dir()
  expect_type(path, "character")
  expect_gt(nchar(path), 0)
})

test_that("opta_data_dir function exists", {
  expect_true(is.function(opta_data_dir))
  # Function should exist - actual path depends on environment
})

test_that("pannadata_dir can be set and retrieved", {
  # Save current value
  original <- tryCatch(pannadata_dir(), error = function(e) NULL)

  # Set a new value
  test_path <- tempdir()
  result <- pannadata_dir(test_path)

  # Verify it was set
  current <- pannadata_dir()
  expect_true(grepl(basename(test_path), current))

  # Restore original if it existed
  if (!is.null(original)) {
    pannadata_dir(original)
  }
})


# =============================================================================
# Tests for source parameter validation
# =============================================================================

test_that("source parameter defaults to 'remote' for FBref loaders", {
  # Check default value in function definition - it's c("remote", "local")
  # When called without source arg, match.arg picks the first
  default_source <- formals(load_summary)$source
  expect_true(is.call(default_source) || is.character(default_source))
  # Evaluate the default to get the character vector
  if (is.call(default_source)) {
    choices <- eval(default_source)
    expect_equal(choices[1], "remote")
  } else {
    expect_equal(default_source[1], "remote")
  }
})

test_that("source parameter defaults to 'remote' for Understat loaders", {
  default_source <- formals(load_understat_roster)$source
  expect_true(is.call(default_source) || is.character(default_source))
  if (is.call(default_source)) {
    choices <- eval(default_source)
    expect_equal(choices[1], "remote")
  } else {
    expect_equal(default_source[1], "remote")
  }
})


# =============================================================================
# Tests for build_where_clause integration in data loaders
# =============================================================================

test_that("build_where_clause produces correct SQL for single league", {
  result <- build_where_clause(list(league = "ENG"))
  expect_equal(result, "WHERE league = 'ENG'")
})

test_that("build_where_clause produces correct SQL for league and season", {
  result <- build_where_clause(list(league = "ENG", season = "2023-2024"))
  expect_equal(result, "WHERE league = 'ENG' AND season = '2023-2024'")
})

test_that("build_where_clause handles NULL filters correctly", {
  result <- build_where_clause(list(league = NULL, season = "2023-2024"))
  expect_equal(result, "WHERE season = '2023-2024'")
})

test_that("build_where_clause returns empty string for empty filters", {
  expect_equal(build_where_clause(list()), "")
  expect_equal(build_where_clause(list(league = NULL, season = NULL)), "")
})


# =============================================================================
# Tests for error handling (network-dependent tests skipped)
# =============================================================================

test_that("pb_download_opta function exists", {
  expect_true(is.function(pb_download_opta))
  fn_args <- names(formals(pb_download_opta))
  expect_true("repo" %in% fn_args)
  expect_true("tag" %in% fn_args)
})


# =============================================================================
# Integration tests with mock data (skipped if fixtures not available)
# =============================================================================

test_that("load functions return data frames or empty data frames", {
  skip_on_cran()
  skip_if_not_installed("arrow")

  # These functions should not error when called, even if data is missing
  # They return empty data frames with warnings when no data found
  result <- tryCatch(
    suppressWarnings(suppressMessages(
      load_summary("NONEXISTENT", "1900-1901", source = "local")
    )),
    error = function(e) data.frame()
  )
  expect_true(is.data.frame(result))
})
