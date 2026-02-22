# Tests for match prediction functions (Elo + lineup skill aggregation)


# =============================================================================
# Elo Rating System Tests
# =============================================================================

test_that("update_elo is zero-sum", {
  result <- update_elo(1500, 1500, home_goals = 2, away_goals = 1)
  total_before <- 1500 + 1500
  total_after <- result$new_home_elo + result$new_away_elo
  expect_equal(total_after, total_before, tolerance = 1e-10)
})

test_that("update_elo: home win increases home Elo", {
  result <- update_elo(1500, 1500, home_goals = 1, away_goals = 0)
  expect_true(result$new_home_elo > 1500)
  expect_true(result$new_away_elo < 1500)
})

test_that("update_elo: draw between equals causes small changes", {
  result <- update_elo(1500, 1500, home_goals = 1, away_goals = 1)
  # Home has advantage built in, so a draw means home slightly underperformed
  home_change <- abs(result$new_home_elo - 1500)
  expect_true(home_change < 20)  # small update
})

test_that("update_elo: goal difference multiplier amplifies large margins", {
  result_1goal <- update_elo(1500, 1500, home_goals = 1, away_goals = 0)
  result_3goal <- update_elo(1500, 1500, home_goals = 3, away_goals = 0)

  change_1 <- result_1goal$new_home_elo - 1500
  change_3 <- result_3goal$new_home_elo - 1500

  expect_true(change_3 > change_1)
})

test_that("update_elo remains zero-sum across many scenarios", {
  scenarios <- list(
    list(1600, 1400, 0, 0),
    list(1200, 1800, 3, 1),
    list(1500, 1500, 0, 2),
    list(1700, 1300, 1, 1)
  )

  for (s in scenarios) {
    result <- update_elo(s[[1]], s[[2]], s[[3]], s[[4]])
    total_before <- s[[1]] + s[[2]]
    total_after <- result$new_home_elo + result$new_away_elo
    expect_equal(total_after, total_before, tolerance = 1e-10)
  }
})


test_that("compute_match_elos returns correct structure", {
  results <- data.frame(
    match_id = paste0("m", 1:4),
    match_date = as.Date("2024-01-01") + c(0, 7, 14, 21),
    home_team = c("Team A", "Team B", "Team A", "Team C"),
    away_team = c("Team B", "Team C", "Team C", "Team A"),
    home_goals = c(2, 1, 0, 3),
    away_goals = c(1, 1, 1, 0)
  )

  elos <- compute_match_elos(results)

  expect_true(is.data.frame(elos))
  expect_equal(nrow(elos), 4)
  expect_true(all(c("match_id", "home_elo", "away_elo", "elo_diff") %in% names(elos)))

  # First match: all teams start at 1500
  expect_equal(elos$home_elo[1], 1500)
  expect_equal(elos$away_elo[1], 1500)
})


test_that("compute_match_elos handles NA goals (unplayed matches)", {
  results <- data.frame(
    match_id = paste0("m", 1:3),
    match_date = as.Date("2024-01-01") + c(0, 7, 14),
    home_team = c("Team A", "Team A", "Team A"),
    away_team = c("Team B", "Team B", "Team B"),
    home_goals = c(1, NA, 2),
    away_goals = c(0, NA, 1)
  )

  elos <- compute_match_elos(results)

  # Match 2 has NA goals, so Elos shouldn't change between match 2 and 3's pre-match
  # Match 1 updates Elo; match 2 records but doesn't update; match 3 uses same pre-match Elo as match 2
  expect_equal(elos$home_elo[2], elos$home_elo[3])
})


test_that("compute_match_elos: pre-match Elo reflects prior results", {
  results <- data.frame(
    match_id = c("m1", "m2"),
    match_date = as.Date("2024-01-01") + c(0, 7),
    home_team = c("Team A", "Team A"),
    away_team = c("Team B", "Team B"),
    home_goals = c(3, 0),
    away_goals = c(0, 0)
  )

  elos <- compute_match_elos(results)

  # After a 3-0 home win, Team A's Elo for match 2 should be above 1500
  expect_true(elos$home_elo[2] > 1500)
  expect_true(elos$away_elo[2] < 1500)
})


# =============================================================================
# aggregate_lineup_skills Tests
# =============================================================================

test_that("aggregate_lineup_skills returns correct structure", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  skills <- data.frame(
    player_name = paste0("Player_", 1:22),
    goals_p90 = runif(22, 0, 1),
    tackles_won_p90 = runif(22, 0, 3)
  )

  result <- aggregate_lineup_skills(lineups, skills,
                                     attacking_stats = "goals_p90",
                                     defensive_stats = "tackles_won_p90")

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)  # one match
  expect_true("home_sk_att_goals" %in% names(result))
  expect_true("away_sk_def_tackles_won" %in% names(result))
})


test_that("aggregate_lineup_skills computes correct means", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  skills <- data.frame(
    player_name = paste0("Player_", 1:22),
    goals_p90 = c(rep(1.0, 11), rep(0.5, 11))  # home=1.0, away=0.5
  )

  result <- aggregate_lineup_skills(lineups, skills,
                                     attacking_stats = "goals_p90",
                                     defensive_stats = character(0))

  expect_equal(result$home_sk_att_goals, 1.0, tolerance = 1e-10)
  expect_equal(result$away_sk_att_goals, 0.5, tolerance = 1e-10)
})


test_that("aggregate_lineup_skills warns about unmatched players", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  # Only provide skills for half the players
  skills <- data.frame(
    player_name = paste0("Player_", 1:11),
    goals_p90 = rep(1.0, 11)
  )

  expect_warning(
    aggregate_lineup_skills(lineups, skills,
                             attacking_stats = "goals_p90",
                             defensive_stats = character(0)),
    "no matching"
  )
})


test_that("aggregate_lineup_skills composite is mean-of-means", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  # Set up skills so stat1 mean = 2.0 and stat2 mean = 4.0 for home team
  skills <- data.frame(
    player_name = paste0("Player_", 1:22),
    goals_p90 = c(rep(2.0, 11), rep(1.0, 11)),
    shots_p90 = c(rep(4.0, 11), rep(1.0, 11))
  )

  result <- aggregate_lineup_skills(lineups, skills,
                                     attacking_stats = c("goals_p90", "shots_p90"),
                                     defensive_stats = character(0))

  # Mean-of-means: (2.0 + 4.0) / 2 = 3.0 (not pooled mean of all player-stat values)
  expect_equal(result$home_sk_att_composite, 3.0, tolerance = 1e-10)
})


test_that("aggregate_lineup_skills does not mutate input", {
  skills_orig <- data.frame(
    player_name = paste0("Player_", 1:22),
    goals_p90 = runif(22)
  )

  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  names_before <- names(skills_orig)
  nrow_before <- nrow(skills_orig)

  aggregate_lineup_skills(lineups, skills_orig,
                           attacking_stats = "goals_p90",
                           defensive_stats = character(0))

  expect_equal(names(skills_orig), names_before)
  expect_equal(nrow(skills_orig), nrow_before)
  # Verify no clean_name column was added to the original
  expect_false("clean_name" %in% names(skills_orig))
})


test_that("aggregate_lineup_skills computes defensive composite and differentials", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  skills <- data.frame(
    player_name = paste0("Player_", 1:22),
    goals_p90 = c(rep(2.0, 11), rep(1.0, 11)),
    tackles_won_p90 = c(rep(3.0, 11), rep(1.0, 11))
  )

  result <- aggregate_lineup_skills(lineups, skills,
                                     attacking_stats = "goals_p90",
                                     defensive_stats = "tackles_won_p90")

  expect_true("home_sk_def_tackles_won" %in% names(result))
  expect_true("away_sk_def_tackles_won" %in% names(result))
  expect_true("sk_att_diff" %in% names(result))
  expect_true("sk_def_diff" %in% names(result))
  expect_equal(result$sk_att_diff, 2.0 - 1.0, tolerance = 1e-10)
  expect_equal(result$sk_def_diff, 3.0 - 1.0, tolerance = 1e-10)
})


test_that("aggregate_lineup_skills errors with no matching stat columns", {
  lineups <- data.frame(
    match_id = rep("m1", 22),
    player_name = paste0("Player_", 1:22),
    team_name = rep(c("Home FC", "Away FC"), each = 11),
    team_position = rep(c("home", "away"), each = 11),
    is_starter = TRUE
  )

  skills <- data.frame(
    player_name = paste0("Player_", 1:22),
    unrelated_col = runif(22)
  )

  expect_error(
    aggregate_lineup_skills(lineups, skills,
                             attacking_stats = "nonexistent_p90",
                             defensive_stats = character(0)),
    "No skill stat columns found"
  )
})
