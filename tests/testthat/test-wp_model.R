# Tests for wp_model.R

test_that("create_wp_features builds correct features", {
  spadl <- data.frame(
    match_id = rep("m1", 4),
    team_id = c("t1", "t1", "t2", "t1"),
    player_id = c("p1", "p1", "p2", "p1"),
    player_name = c("Alice", "Alice", "Bob", "Alice"),
    action_type = c("pass", "shot", "pass", "shot"),
    result = c("success", "fail", "success", "success"),
    time_seconds = c(100, 500, 1000, 2000),
    period_id = c(1L, 1L, 1L, 2L),
    stringsAsFactors = FALSE
  )

  match_results <- data.frame(
    match_id = "m1",
    home_team_id = "t1",
    away_team_id = "t2",
    home_goals = 1,
    away_goals = 0,
    stringsAsFactors = FALSE
  )

  result <- create_wp_features(spadl, match_results)

  expect_true("time_remaining" %in% names(result))
  expect_true("score_diff" %in% names(result))
  expect_true("is_home" %in% names(result))
  expect_true("is_second_half" %in% names(result))
  expect_true("wp_label" %in% names(result))

  # wp_label is possession-POV (see R/wp_model.R:122-125): the home team
  # won here, so t1's actions (possession=home) label 1 and t2's action
  # (possession=away, lost) labels 0. Earlier home-POV semantics had
  # expected all-1 which only held when all actions were home-team's.
  expect_equal(result$wp_label[result$team_id == "t1"], c(1, 1, 1))
  expect_equal(result$wp_label[result$team_id == "t2"], 0)

  # is_home: t1 actions = 1, t2 actions = 0
  expect_equal(result$is_home[result$team_id == "t1"], c(1L, 1L, 1L))
  expect_equal(result$is_home[result$team_id == "t2"], 0L)

  # time_remaining should decrease
  expect_true(all(diff(result$time_remaining) <= 0))
})

test_that("create_wp_features tracks score state correctly", {
  # Two goals: home scores at action 2, away scores at action 4
  spadl <- data.frame(
    match_id = rep("m1", 5),
    team_id = c("t1", "t1", "t2", "t2", "t1"),
    player_id = c("p1", "p1", "p2", "p2", "p1"),
    player_name = c("A", "A", "B", "B", "A"),
    action_type = c("pass", "shot", "pass", "shot", "pass"),
    result = c("success", "success", "success", "success", "success"),
    time_seconds = c(100, 500, 1000, 2000, 3000),
    period_id = c(1L, 1L, 1L, 2L, 2L),
    stringsAsFactors = FALSE
  )

  match_results <- data.frame(
    match_id = "m1", home_team_id = "t1", away_team_id = "t2",
    home_goals = 1, away_goals = 1,
    stringsAsFactors = FALSE
  )

  result <- create_wp_features(spadl, match_results)

  # Score diff before each action (cumulative goals BEFORE this action):
  # Action 1 (pass): 0-0 → 0
  # Action 2 (home goal): 0-0 → 0 (goal counted after)
  # Action 3 (pass): 1-0 → 1
  # Action 4 (away goal): 1-0 → 1 (goal counted after)
  # Action 5 (pass): 1-1 → 0
  expect_equal(result$score_diff, c(0, 0, 1, 1, 0))

  # Draw → label = 0.5
  expect_equal(result$wp_label[1], 0.5)
})

test_that("create_wp_features errors without home_teams or match_results", {
  spadl <- data.frame(
    match_id = "m1", team_id = "t1", player_id = "p1", player_name = "A",
    action_type = "pass", result = "success", time_seconds = 100,
    period_id = 1L, stringsAsFactors = FALSE
  )

  expect_error(create_wp_features(spadl), "must be provided")
})


# Tests for wp_credit.R

test_that("assign_wpa_credit splits WPA between actor and receiver", {
  spadl <- data.frame(
    match_id = rep("m1", 3),
    player_id = c("p1", "p1", "p2"),
    player_name = c("A", "A", "B"),
    team_id = c("t1", "t1", "t2"),
    wpa = c(0.1, 0.2, -0.05),
    receiver_player_id = c("p2", NA, NA),
    receiver_player_name = c("B", NA, NA),
    stringsAsFactors = FALSE
  )

  result <- assign_wpa_credit(spadl, actor_share = 0.5)

  expect_true("wpa_actor" %in% names(result))
  expect_true("wpa_receiver" %in% names(result))

  # Action 1: pass with receiver → 50/50 split
  expect_equal(result$wpa_actor[1], 0.05)
  expect_equal(result$wpa_receiver[1], 0.05)

  # Action 2: no receiver → 100% to actor
  expect_equal(result$wpa_actor[2], 0.2)
  expect_equal(result$wpa_receiver[2], 0)

  # Action 3: no receiver → 100% to actor
  expect_equal(result$wpa_actor[3], -0.05)
})

test_that("aggregate_player_game_wpa groups by match_id", {
  spadl <- data.frame(
    match_id = c("m1", "m1", "m2", "m2"),
    player_id = c("p1", "p1", "p1", "p1"),
    player_name = c("A", "A", "A", "A"),
    team_id = c("t1", "t1", "t1", "t1"),
    wpa = c(0.1, 0.2, 0.05, -0.1),
    wpa_actor = c(0.1, 0.2, 0.05, -0.1),
    wpa_receiver = c(0, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_wpa(spadl)

  expect_equal(nrow(result), 2)  # 2 matches

  m1 <- result[result$match_id == "m1", ]
  expect_equal(m1$wpa_as_actor, 0.3)
  expect_equal(m1$wpa_total, 0.3)

  m2 <- result[result$match_id == "m2", ]
  expect_equal(m2$wpa_as_actor, -0.05)
})

test_that("aggregate_player_game_wpa computes per-90", {
  spadl <- data.frame(
    match_id = "m1",
    player_id = "p1",
    player_name = "Alice",
    team_id = "t1",
    wpa = 0.1,
    wpa_actor = 0.1,
    wpa_receiver = 0,
    stringsAsFactors = FALSE
  )

  lineups <- data.frame(
    player_id = "p1",
    match_id = "m1",
    minutes_played = 45,
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_wpa(spadl, lineups = lineups)

  expect_true("wpa_total_p90" %in% names(result))
  # WPA total = 0.1, 45 mins, per-90 = 0.1 / (45/90) = 0.2
  expect_equal(result$wpa_total_p90, 0.2)
})

test_that("aggregate_player_game_wpa handles receiver WPA", {
  spadl <- data.frame(
    match_id = c("m1", "m1"),
    player_id = c("p1", "p1"),
    player_name = c("A", "A"),
    team_id = c("t1", "t1"),
    wpa = c(0.1, 0.2),
    wpa_actor = c(0.05, 0.1),
    wpa_receiver = c(0.05, 0.1),
    receiver_player_id = c("p2", "p2"),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_wpa(spadl)

  # p2 should appear with receiver WPA
  p2 <- result[result$player_id == "p2", ]
  expect_equal(nrow(p2), 1)
  expect_equal(p2$wpa_as_receiver, 0.15)  # 0.05 + 0.10
})

test_that("aggregate_player_game_wpa tracks max WPA", {
  spadl <- data.frame(
    match_id = rep("m1", 3),
    player_id = rep("p1", 3),
    player_name = rep("A", 3),
    team_id = rep("t1", 3),
    wpa = c(0.01, -0.3, 0.05),
    wpa_actor = c(0.01, -0.3, 0.05),
    wpa_receiver = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_wpa(spadl)
  # Max by absolute value: -0.3
  expect_equal(result$max_wpa, -0.3)
})
