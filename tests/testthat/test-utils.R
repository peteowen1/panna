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
