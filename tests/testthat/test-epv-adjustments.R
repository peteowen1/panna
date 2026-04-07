# Tests for EPV position centering and opponent adjustment (epv_adjustments.R)

adjust_epv_for_position <- panna:::adjust_epv_for_position
adjust_epv_for_opponents <- panna:::adjust_epv_for_opponents
get_player_positions <- panna:::get_player_positions

# =============================================================================
# Position centering tests
# =============================================================================

test_that("adjust_epv_for_position centers credit by position group", {
  dt <- data.table::data.table(
    player_id = c("p1", "p2", "p3", "p4"),
    match_id = rep("m1", 4),
    position = c("Defender", "Defender", "Midfielder", "Midfielder"),
    total_credit = c(0.1, 0.3, 0.5, 0.7)
  )

  result <- adjust_epv_for_position(dt, credit_cols = "total_credit")

  expect_true("total_credit_adj" %in% names(result))

  # Defender mean = 0.2, so p1 adj = -0.1, p2 adj = +0.1
  expect_equal(result[player_id == "p1"]$total_credit_adj, -0.1)
  expect_equal(result[player_id == "p2"]$total_credit_adj, 0.1)

  # Midfielder mean = 0.6, so p3 adj = -0.1, p4 adj = +0.1
  expect_equal(result[player_id == "p3"]$total_credit_adj, -0.1)
  expect_equal(result[player_id == "p4"]$total_credit_adj, 0.1)
})

test_that("adjust_epv_for_position works with multiple credit columns", {
  dt <- data.table::data.table(
    player_id = c("p1", "p2"),
    match_id = rep("m1", 2),
    position = c("Defender", "Defender"),
    epv_total = c(0.4, 0.6),
    epv_offensive = c(0.1, 0.3)
  )

  result <- adjust_epv_for_position(dt, credit_cols = c("epv_total", "epv_offensive"))

  expect_true(all(c("epv_total_adj", "epv_offensive_adj") %in% names(result)))
  # Both centered around their position mean
  expect_equal(sum(result$epv_total_adj), 0)
  expect_equal(sum(result$epv_offensive_adj), 0)
})

test_that("adjust_epv_for_position requires position column", {
  dt <- data.table::data.table(
    player_id = "p1", match_id = "m1", total_credit = 0.5
  )
  expect_error(adjust_epv_for_position(dt), "position")
})

# =============================================================================
# Opponent adjustment tests
# =============================================================================

test_that("adjust_epv_for_opponents adds adjustment columns", {
  dt <- data.table::data.table(
    player_id = rep(c("p1", "p2", "p3", "p4"), each = 3),
    match_id = rep(c("m1", "m2", "m3"), 4),
    team_id = rep(c(101, 101, 101, 102, 102, 102, 101, 101, 101, 102, 102, 102)),
    match_date = rep(as.Date("2024-01-01") + c(0, 7, 14), 4),
    minutes_played = rep(90, 12),
    total_credit = rnorm(12, 0.5, 0.2)
  )

  result <- adjust_epv_for_opponents(dt, credit_col = "total_credit")

  expect_true("opp_adjustment" %in% names(result))
  expect_true("player_opp_adj" %in% names(result))
  expect_equal(nrow(result), nrow(dt))
})

test_that("adjust_epv_for_opponents first match has zero adjustment", {
  # With only 2 matches, first match should have 0 opponent profile
  dt <- data.table::data.table(
    player_id = c("p1", "p2", "p1", "p2"),
    match_id = c("m1", "m1", "m2", "m2"),
    team_id = c(101, 102, 101, 102),
    match_date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-08", "2024-01-08")),
    minutes_played = c(90, 90, 90, 90),
    total_credit = c(0.5, 0.3, 0.7, 0.2)
  )

  result <- adjust_epv_for_opponents(dt, credit_col = "total_credit")

  # First match: no prior data → adjustment should be 0
  first_match <- result[match_id == "m1"]
  expect_true(all(first_match$opp_adjustment == 0))
})

test_that("adjust_epv_for_opponents validates required columns", {
  dt <- data.table::data.table(
    player_id = "p1", match_id = "m1", total_credit = 0.5
  )
  expect_error(adjust_epv_for_opponents(dt), "Missing columns")
})

# =============================================================================
# get_player_positions tests
# =============================================================================

test_that("get_player_positions returns most common position", {
  lineups <- data.table::data.table(
    player_id = c("p1", "p1", "p1", "p1", "p2"),
    position = c("Defender", "Defender", "Midfielder", "Defender", "Striker"),
    match_id = c("m1", "m2", "m3", "m4", "m1")
  )

  result <- get_player_positions(lineups)

  expect_equal(result[player_id == "p1"]$position, "Defender")  # 3 DEF vs 1 MID
  expect_equal(result[player_id == "p2"]$position, "Striker")
})

test_that("get_player_positions merges Wing Back into Defender", {
  lineups <- data.table::data.table(
    player_id = "p1",
    position = "Wing Back",
    match_id = "m1"
  )

  result <- get_player_positions(lineups)

  expect_equal(result$position, "Defender")
})
