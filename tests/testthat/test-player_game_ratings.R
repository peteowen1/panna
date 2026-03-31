# Tests for build_player_game_ratings() and aggregate_season_ratings()

test_that("build_player_game_ratings merges EPV, WPA, PSV", {
  epv <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    team_id = c("t1", "t1"),
    match_id = c("m1", "m1"),
    minutes_played = c(90, 90),
    epv_total = c(0.5, 0.3),
    epv_offensive = c(0.4, 0.2),
    epv_defensive = c(0.1, 0.1),
    stringsAsFactors = FALSE
  )

  wpa <- data.frame(
    player_id = c("p1", "p2"),
    match_id = c("m1", "m1"),
    wpa_total = c(0.08, 0.03),
    wpa_as_actor = c(0.05, 0.02),
    wpa_as_receiver = c(0.03, 0.01),
    stringsAsFactors = FALSE
  )

  psv <- data.frame(
    player_id = c("p1", "p2"),
    match_id = c("m1", "m1"),
    psv = c(0.6, 0.2),
    osv = c(0.4, 0.15),
    dsv = c(0.2, 0.05),
    stringsAsFactors = FALSE
  )

  result <- build_player_game_ratings(epv, wpa, psv)

  expect_equal(nrow(result), 2)
  expect_true("epv_total" %in% names(result))
  expect_true("wpa_total" %in% names(result))
  expect_true("psv" %in% names(result))
  expect_true("panna_value" %in% names(result))

  # panna_value is a weighted combination of EPV and PSV using package constants
  alice <- result[result$player_id == "p1", ]
  expect_equal(alice$panna_value,
               PANNA_EPR_WEIGHT * 0.5 + PANNA_PSR_WEIGHT * 0.6)

  # Higher EPV+PSV player should have higher panna_value
  bob <- result[result$player_id == "p2", ]
  expect_gt(alice$panna_value, bob$panna_value)
})

test_that("build_player_game_ratings works without WPA/PSV", {
  epv <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    team_id = "t1",
    match_id = "m1",
    minutes_played = 90,
    epv_total = 0.5,
    epv_offensive = 0.4,
    epv_defensive = 0.1,
    stringsAsFactors = FALSE
  )

  result <- build_player_game_ratings(epv)

  expect_equal(nrow(result), 1)
  expect_true("panna_value" %in% names(result))
  # No PSV → panna_value uses EPV only (PSV defaults to 0)
  expect_equal(result$panna_value, PANNA_EPR_WEIGHT * 0.5)
})

test_that("build_player_game_ratings computes panna_value_p90", {
  epv <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    team_id = "t1",
    match_id = "m1",
    minutes_played = 45,
    epv_total = 0.5,
    epv_offensive = 0.4,
    epv_defensive = 0.1,
    stringsAsFactors = FALSE
  )

  result <- build_player_game_ratings(epv)

  expect_true("panna_value_p90" %in% names(result))
  # p90 should be double the raw value for a 45-min appearance
  expect_equal(result$panna_value_p90, result$panna_value / (45 / 90))
})

test_that("build_player_game_ratings custom weights override defaults", {
  epv <- data.frame(
    player_id = "p1", player_name = "Alice", team_id = "t1",
    match_id = "m1", minutes_played = 90,
    epv_total = 1.0, epv_offensive = 0.8, epv_defensive = 0.2,
    stringsAsFactors = FALSE
  )

  psv <- data.frame(
    player_id = "p1", match_id = "m1", psv = 2.0,
    stringsAsFactors = FALSE
  )

  # Custom weights should be respected
  result <- build_player_game_ratings(epv, player_game_psv = psv,
                                       epv_weight = 0.7, psv_weight = 0.3)
  expect_equal(result$panna_value, 0.7 * 1.0 + 0.3 * 2.0)
})

test_that("blend weight constants sum to 1", {
  expect_equal(PANNA_EPR_WEIGHT + PANNA_PSR_WEIGHT, 1.0)
  expect_true(PANNA_EPR_WEIGHT >= 0 && PANNA_EPR_WEIGHT <= 1)
  expect_true(PANNA_PSR_WEIGHT >= 0 && PANNA_PSR_WEIGHT <= 1)
})

test_that("aggregate_season_ratings sums and averages correctly", {
  game_ratings <- data.frame(
    player_id = rep("p1", 3),
    player_name = rep("Alice", 3),
    match_id = c("m1", "m2", "m3"),
    season = rep("2024", 3),
    minutes_played = c(90, 45, 90),
    epv_total = c(0.5, 0.3, 0.4),
    panna_value = c(0.4, 0.2, 0.3),
    stringsAsFactors = FALSE
  )

  result <- aggregate_season_ratings(game_ratings)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_games, 3)
  expect_equal(result$total_minutes, 225)
  expect_equal(result$epv_total, 1.2)  # 0.5 + 0.3 + 0.4
  expect_equal(result$panna_value, 0.9)  # 0.4 + 0.2 + 0.3

  # Per-90: 1.2 / (225/90) = 0.48
  expect_equal(result$epv_total_p90, 1.2 / (225 / 90))
})
