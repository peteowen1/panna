# Tests for compare_players.R

# Helper: create mock xMetrics data matching .load_opta_xmetrics_data() output
mock_xmetrics_data <- function() {
  data.frame(
    player_name = c("Mohamed Salah", "Mohamed Salah", "Erling Haaland", "Erling Haaland"),
    player_id = c("p1", "p1", "p2", "p2"),
    team_name = c("Liverpool", "Liverpool", "Man City", "Man City"),
    minutes = c(90, 85, 90, 70),
    goals = c(1, 0, 2, 1),
    xg = c(0.5, 0.3, 1.2, 0.6),
    npxg = c(0.5, 0.3, 1.0, 0.4),
    shots = c(3, 2, 5, 3),
    assists = c(0, 1, 0, 0),
    xa = c(0.2, 0.4, 0.1, 0.05),
    key_passes = c(2, 3, 1, 1),
    passes_completed = c(40, 35, 25, 30),
    passes_attempted = c(50, 45, 35, 40),
    xpass = c(42, 37, 27, 32),
    xpass_overperformance = c(2.0, 1.5, -0.5, 0.3),
    progressive_passes = c(3, 4, 1, 2),
    chain_goals = c(1, 1, 2, 1),
    chain_shots = c(3, 2, 5, 3),
    chain_xg = c(0.5, 0.3, 1.2, 0.6),
    chain_passes = c(5, 4, 3, 3),
    chain_carries = c(2, 3, 4, 2),
    tackles = c(1, 2, 0, 1),
    interceptions = c(0, 1, 0, 0),
    clearances = c(0, 0, 0, 0),
    aerial_won = c(1, 0, 3, 2),
    aerial_lost = c(0, 1, 1, 0),
    touches = c(50, 45, 35, 30),
    carries = c(15, 12, 10, 8),
    progressive_carries = c(3, 2, 2, 1),
    stringsAsFactors = FALSE
  )
}


test_that("compare_players errors with no players", {
  expect_error(compare_players(character(0)), "at least one")
})

test_that("compare_players returns empty frame for no data", {
  local_mocked_bindings(
    .load_opta_xmetrics_data = function(...) NULL
  )
  expect_warning(
    result <- compare_players("Nonexistent Player"),
    "No xMetrics data"
  )
  expect_equal(nrow(result), 0)
})

test_that("compare_players finds players by partial name", {
  local_mocked_bindings(
    .load_opta_xmetrics_data = function(...) mock_xmetrics_data()
  )

  result <- compare_players(c("Salah", "Haaland"))
  expect_equal(nrow(result), 2)
  expect_true("Mohamed Salah" %in% result$player_name)
  expect_true("Erling Haaland" %in% result$player_name)
})

test_that("compare_players aggregates stats correctly", {
  local_mocked_bindings(
    .load_opta_xmetrics_data = function(...) mock_xmetrics_data()
  )

  result <- compare_players("Salah")
  expect_equal(nrow(result), 1)
  # Salah: 90 + 85 = 175 minutes
  expect_equal(result$minutes, 175)
  # Salah: 1 + 0 = 1 goal
  expect_equal(result$goals, 1)
})

test_that("compare_players returns per-90 columns", {
  local_mocked_bindings(
    .load_opta_xmetrics_data = function(...) mock_xmetrics_data()
  )

  result <- compare_players("Haaland")
  # Check that per-90 columns exist
  p90_cols <- grep("_p90$", names(result), value = TRUE)
  expect_true(length(p90_cols) > 0)
})

test_that("compare_players warns for unmatched player", {
  local_mocked_bindings(
    .load_opta_xmetrics_data = function(...) mock_xmetrics_data()
  )

  expect_warning(
    result <- compare_players("Completely Unknown Name"),
    "No data found"
  )
  expect_equal(nrow(result), 0)
})
