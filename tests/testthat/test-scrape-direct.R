# Tests for direct FBref scraping functions

test_that("get_fbref_headers returns valid headers", {
  headers <- get_fbref_headers()

  expect_type(headers, "character")
  expect_true("User-Agent" %in% names(headers))
  expect_true("Accept" %in% names(headers))
  expect_true("sec-ch-ua" %in% names(headers))
})


test_that("make_match_filename creates valid filenames", {
  # Function now only takes fbref_id (hierarchical structure uses dir path for league/season)
  filename <- make_match_filename("abc12345")

  expect_equal(filename, "abc12345.rds")

  # Handles special characters
  filename_special <- make_match_filename("abc/123")
  expect_false(grepl("/", filename_special))
})


test_that("cache directory functions work", {
  # Get base cache dir (now uses pannadata_dir which defaults differently)
  cache_dir <- get_fbref_match_cache_dir(create = FALSE)
  expect_type(cache_dir, "character")

  # Get table-specific cache dir - hierarchical structure
  cache_dir_summary <- get_fbref_match_cache_dir("summary", "ENG", "2024-2025", create = FALSE)
  expect_true(grepl("summary", cache_dir_summary))
  expect_true(grepl("ENG", cache_dir_summary))
  expect_true(grepl("2024-2025", cache_dir_summary))
})


test_that("save and load match table work correctly", {
  skip_on_cran()

  # Create test data
  test_data <- data.frame(
    player = c("Test Player 1", "Test Player 2"),
    goals = c(1, 0),
    stringsAsFactors = FALSE
  )

  # Save - hierarchical structure: {table_type}/{league}/{season}/{id}.rds
  save_match_table(test_data, "TEST", "2024-2025", "test1234", "summary")

  # Load
  loaded <- load_match_table("TEST", "2024-2025", "test1234", "summary")

  expect_equal(test_data, loaded)

  # Check cache detection - requires metadata with tables_available field
  # For now just check that nonexistent returns FALSE
  expect_false(is_match_cached("TEST", "2024-2025", "nonexist", "summary"))

  # Cleanup - remove the whole test directory structure
  cache_dir <- get_fbref_match_cache_dir("summary", "TEST", "2024-2025", create = FALSE)
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
  }
  # Also try to clean up parent directories if empty
  parent_dir <- dirname(cache_dir)
  if (dir.exists(parent_dir) && length(list.files(parent_dir)) == 0) {
    unlink(parent_dir, recursive = TRUE)
  }
})


test_that("list_cached_matches returns correct structure", {
  # Should return empty data frame when no cache
  cached <- list_cached_matches("nonexistent_type")

  expect_s3_class(cached, "data.frame")
  expect_equal(names(cached), c("league", "season", "fbref_id"))
  expect_equal(nrow(cached), 0)
})


test_that("fetch_match_page validates URL", {
  expect_error(
    fetch_match_page("https://google.com"),
    "Invalid FBref match URL"
  )
})


test_that("scrape_fbref_matches validates inputs", {
  expect_error(
    scrape_fbref_matches(character(0), "ENG", "2024-2025"),
    "No match URLs provided"
  )

  expect_error(
    scrape_fbref_matches("https://fbref.com/en/matches/abc123/test"),
    "league and season are required"
  )
})


# Integration tests (skip by default, require network)
test_that("fetch_match_page works with real URL", {
  skip_on_cran()
  skip_if_offline()
  skip("Manual test - requires network access to FBref")

  url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"
  page <- fetch_match_page(url)

  expect_false(is.null(page))
  expect_s3_class(page, "xml_document")
})


test_that("full scrape workflow works", {
  skip_on_cran()
  skip_if_offline()
  skip("Manual test - requires network access to FBref")

  url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"

  data <- scrape_fbref_matches(
    url,
    league = "ESP",
    season = "2025-2026",
    delay = 5,
    use_cache = FALSE,
    verbose = FALSE
  )

  expect_type(data, "list")
  expect_true("metadata" %in% names(data))
  expect_true("summary" %in% names(data))

  # Check metadata
  expect_s3_class(data$metadata, "data.frame")
  expect_equal(data$metadata$fbref_id, "12c8079e")

  # Check summary stats
  if (!is.null(data$summary)) {
    expect_s3_class(data$summary, "data.frame")
    expect_true("player" %in% names(data$summary))
    expect_true("team" %in% names(data$summary))
  }
})
