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
  expect_true("piero_value" %in% names(result))

  # piero_value is a weighted combination of EPV and PSV using package constants
  alice <- result[result$player_id == "p1", ]
  expect_equal(alice$piero_value,
               PANNA_EPR_WEIGHT * 0.5 + PANNA_PSR_WEIGHT * 0.6)

  # Higher EPV+PSV player should have higher piero_value
  bob <- result[result$player_id == "p2", ]
  expect_gt(alice$piero_value, bob$piero_value)
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
  expect_true("piero_value" %in% names(result))
  # No PSV -> piero_value is NA, not EPV with PSV taken as 0 (review R007)
  expect_true(is.na(result$piero_value))
  expect_false(result$has_psv)
  expect_false(result$has_wpa)
})

test_that("a player-game with no PSV row keeps NA psv and piero (R007)", {
  epv <- data.frame(
    player_id = c("p1", "p2"), player_name = c("Alice", "Bob"),
    team_id = "t1", match_id = "m1", minutes_played = c(90, 5),
    epv_total = c(0.5, 0.05), epv_offensive = c(0.4, 0.05), epv_defensive = c(0.1, 0),
    stringsAsFactors = FALSE
  )
  wpa <- data.frame(player_id = "p1", match_id = "m1", wpa_total = 0.08,
                    stringsAsFactors = FALSE)
  psv <- data.frame(player_id = "p1", match_id = "m1", psv = 0.6, osv = 0.4,
                    dsv = 0.2, stringsAsFactors = FALSE)
  result <- build_player_game_ratings(epv, wpa, psv)
  bob <- result[result$player_id == "p2", ]
  expect_true(all(is.na(c(bob$psv, bob$osv, bob$dsv, bob$wpa_total,
                          bob$piero_value, bob$piero_value_p90))))
  expect_false(bob$has_psv)
  expect_false(bob$has_wpa)
  expect_equal(bob$epv_total, 0.05)
  alice <- result[result$player_id == "p1", ]
  expect_true(alice$has_psv && alice$has_wpa)
  expect_equal(alice$piero_value, PANNA_EPR_WEIGHT * 0.5 + PANNA_PSR_WEIGHT * 0.6)
  # NA piero sorts last within the match
  expect_identical(result$player_id, c("p1", "p2"))
})

test_that("build_player_game_ratings computes piero_value_p90", {
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

  expect_true("piero_value_p90" %in% names(result))
  # p90 should be double the raw value for a 45-min appearance
  expect_equal(result$piero_value_p90, result$piero_value / (45 / 90))
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
  expect_equal(result$piero_value, 0.7 * 1.0 + 0.3 * 2.0)
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
    piero_value = c(0.4, 0.2, 0.3),
    stringsAsFactors = FALSE
  )

  result <- aggregate_season_ratings(game_ratings)

  expect_equal(nrow(result), 1)
  expect_equal(result$n_games, 3)
  expect_equal(result$total_minutes, 225)
  expect_equal(result$epv_total, 1.2)  # 0.5 + 0.3 + 0.4
  expect_equal(result$piero_value, 0.9)  # 0.4 + 0.2 + 0.3

  # Per-90: 1.2 / (225/90) = 0.48
  expect_equal(result$epv_total_p90, 1.2 / (225 / 90))
})

test_that("aggregate_season_ratings keeps missing PSV out of totals and per-90 (R007)", {
  gr <- data.frame(
    player_id = c("p1", "p1", "p2"), player_name = c("Alice", "Alice", "Bob"),
    match_id = c("m1", "m2", "m1"), season = "2024",
    minutes_played = c(90, 10, 5),
    epv_total = c(0.5, 0.1, 0.05), psv = c(0.6, NA, NA),
    piero_value = c(0.55, NA, NA), stringsAsFactors = FALSE
  )
  res <- aggregate_season_ratings(gr)
  a <- res[res$player_id == "p1", ]
  b <- res[res$player_id == "p2", ]
  expect_equal(a$psv, 0.6)
  expect_equal(a$psv_p90, 0.6)                 # divides by the 90 PSV minutes, not 100
  expect_equal(a$epv_total_p90, 0.6 / (100 / 90))
  expect_true(is.na(b$psv) && is.na(b$piero_value) && is.na(b$psv_p90))
  expect_identical(res$player_id, c("p1", "p2"))   # NA piero sorts last
})
