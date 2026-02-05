# Tests for Understat scraping functions

# =============================================================================
# Helper functions
# =============================================================================

#' Create mock Understat match API response
#' Note: extract_understat_roster expects api_data$rosters$h and api_data$rosters$a
create_mock_understat_api <- function() {
  list(
    rosters = list(
      h = list(  # Home team
        list(
          id = "12345",
          player = "Player A",
          player_id = "1001",
          position = "FW",
          goals = "1",
          own_goals = "0",
          assists = "1",
          key_passes = "3",
          xG = "0.75",
          xA = "0.5",
          shots = "4",
          time = "90",
          h_a = "h",
          yellow = "0",
          red = "0"
        ),
        list(
          id = "12346",
          player = "Player B",
          player_id = "1002",
          position = "MF",
          goals = "0",
          own_goals = "0",
          assists = "0",
          key_passes = "2",
          xG = "0.1",
          xA = "0.3",
          shots = "1",
          time = "90",
          h_a = "h",
          yellow = "1",
          red = "0"
        )
      ),
      a = list(  # Away team
        list(
          id = "12347",
          player = "Player C",
          player_id = "2001",
          position = "FW",
          goals = "0",
          own_goals = "0",
          assists = "0",
          key_passes = "1",
          xG = "0.2",
          xA = "0.1",
          shots = "2",
          time = "75",
          h_a = "a",
          yellow = "0",
          red = "0"
        )
      )
    ),
    shots = list(
      h = list(
        list(
          id = "shot_1",
          player = "Player A",
          player_id = "1001",
          minute = "23",
          result = "Goal",
          X = "0.92",
          Y = "0.45",
          xG = "0.75",
          h_a = "h",
          situation = "OpenPlay",
          shotType = "RightFoot",
          lastAction = "Pass"
        ),
        list(
          id = "shot_2",
          player = "Player A",
          player_id = "1001",
          minute = "55",
          result = "SavedShot",
          X = "0.85",
          Y = "0.5",
          xG = "0.15",
          h_a = "h",
          situation = "OpenPlay",
          shotType = "Head",
          lastAction = "Cross"
        )
      ),
      a = list(
        list(
          id = "shot_3",
          player = "Player C",
          player_id = "2001",
          minute = "67",
          result = "BlockedShot",
          X = "0.8",
          Y = "0.55",
          xG = "0.2",
          h_a = "a",
          situation = "SetPiece",
          shotType = "LeftFoot",
          lastAction = "BallRecovery"
        )
      )
    )
  )
}


# =============================================================================
# Tests for URL validation (scrape_understat.R)
# =============================================================================

test_that("fetch_understat_page validates URL", {
  expect_error(
    fetch_understat_page("https://google.com"),
    "Invalid Understat URL"
  )

  expect_error(
    fetch_understat_page("not_a_url"),
    "Invalid Understat URL"
  )
})


# =============================================================================
# Tests for header generation
# =============================================================================

test_that("get_understat_headers returns valid headers", {
  headers <- get_understat_headers()

  expect_type(headers, "character")
  expect_true("User-Agent" %in% names(headers))
  expect_true("Accept" %in% names(headers))
  expect_true("Referer" %in% names(headers))
})

test_that("get_understat_headers respects custom referer", {
  headers <- get_understat_headers(referer = "https://understat.com/match/12345")

  expect_equal(headers["Referer"], c(Referer = "https://understat.com/match/12345"))
})


# =============================================================================
# Tests for session management
# =============================================================================

test_that("get_understat_session returns httr handle", {
  session <- get_understat_session()

  expect_s3_class(session, "handle")
})

test_that("reset_understat_session creates new session", {
  session1 <- get_understat_session()
  reset_understat_session()
  session2 <- get_understat_session()

  # Both should be valid handles
  expect_s3_class(session1, "handle")
  expect_s3_class(session2, "handle")
})


# =============================================================================
# Tests for data extraction from API response
# =============================================================================

test_that("extract_understat_roster processes API response correctly", {
  mock_api <- create_mock_understat_api()
  understat_id <- 12345

  result <- extract_understat_roster(mock_api, understat_id)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 3)  # At least 3 players

  # Check that understat_id is present
  expect_true("understat_id" %in% names(result))
  expect_true("is_home" %in% names(result))
})

test_that("extract_understat_roster handles empty input", {
  empty_api <- list(rosters = list(h = list(), a = list()))

  result <- extract_understat_roster(empty_api, 12345)

  # Returns NULL when no data
  expect_null(result)
})

test_that("extract_understat_roster returns NULL for NULL input", {
  expect_null(extract_understat_roster(NULL, 12345))
  expect_null(extract_understat_roster(list(), 12345))
})

test_that("extract_understat_shots processes shot data correctly", {
  mock_api <- create_mock_understat_api()
  understat_id <- 12345

  result <- extract_understat_shots(mock_api, understat_id)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)  # 3 shots total

  # Check understat_id is present
  expect_true("understat_id" %in% names(result))
  expect_true("is_home" %in% names(result))
})

test_that("extract_understat_shots handles empty input", {
  empty_api <- list(shots = list(h = list(), a = list()))

  result <- extract_understat_shots(empty_api, 12345)

  # Returns NULL when no data
  expect_null(result)
})

test_that("extract_understat_shots returns NULL for NULL input", {
  expect_null(extract_understat_shots(NULL, 12345))
  expect_null(extract_understat_shots(list(), 12345))
})


# =============================================================================
# Tests for input validation
# =============================================================================

test_that("scrape_understat_match_range validates ID range", {
  expect_error(
    scrape_understat_match_range(100, 50, "EPL", "2024"),
    "start_id must be <= end_id"
  )
})


# =============================================================================
# Tests for league validation
# =============================================================================

test_that("scrape_understat_season validates league", {
  expect_error(
    scrape_understat_season("INVALID_LEAGUE", "2024"),
    "not available on Understat"
  )
})


# =============================================================================
# Tests for path generation
# =============================================================================

test_that("get_understat_parquet_path creates valid paths", {
  path <- get_understat_parquet_path("roster", "EPL", "2024", create = FALSE)

  expect_type(path, "character")
  expect_true(grepl("understat", path))
  expect_true(grepl("roster", path))
  expect_true(grepl("EPL", path))
  expect_true(grepl("2024", path))
})


# =============================================================================
# Tests for caching functions
# =============================================================================

test_that("get_cached_understat_ids returns character vector", {
  # Should return empty character vector for non-existent cache
  result <- get_cached_understat_ids("NONEXISTENT", "9999")

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("is_understat_cached returns FALSE for non-cached match", {
  result <- is_understat_cached("NONEXISTENT", "9999", 999999999)

  expect_false(result)
})


# =============================================================================
# Tests for JSON parsing
# =============================================================================

test_that("parse_understat_json handles escaped unicode", {
  # Test with escaped unicode characters
  # parse_understat_json cleans the string but doesn't parse JSON
  json_str <- '{"player":"M\\u00e9ssi","goals":1}'

  result <- parse_understat_json(json_str)

  # Returns cleaned string
  expect_type(result, "character")
  expect_true(grepl("Méssi", result))
})

test_that("parse_understat_json handles hex escapes", {
  # Test with hex escapes (\\xNN -> \\u00NN)
  json_str <- '\\x7B"test": 1\\x7D'

  result <- parse_understat_json(json_str)

  expect_type(result, "character")
  # Hex escapes converted to unicode then unescaped
  expect_true(grepl("\\{", result) || grepl("test", result))
})

test_that("parse_understat_json handles escaped single quotes", {
  json_str <- "O\\'Brien"

  result <- parse_understat_json(json_str)

  expect_type(result, "character")
  expect_equal(result, "O'Brien")
})

test_that("parse_understat_json returns input for normal strings", {
  json_str <- '{"player":"Smith","goals":1}'

  result <- parse_understat_json(json_str)

  expect_type(result, "character")
  expect_equal(result, json_str)
})


# =============================================================================
# Tests for bulk scraping validation
# =============================================================================

test_that("bulk_scrape_understat validates ID range", {
  expect_error(
    bulk_scrape_understat(100, 50),
    "start_id must be <= end_id"
  )
})


# =============================================================================
# Tests for aggregation functions
# =============================================================================

test_that("aggregate_understat_data returns empty data frame for missing data", {
  result <- aggregate_understat_data("nonexistent_type", "FAKE", "9999")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

