# Tests for aggregate_player_game_epv()

test_that("aggregate_player_game_epv groups by match_id", {
  # Synthetic SPADL: 2 matches, 2 players each

  spadl <- data.frame(
    player_id = c("p1", "p1", "p2", "p2", "p1", "p1", "p2", "p2"),
    player_name = c("Alice", "Alice", "Bob", "Bob", "Alice", "Alice", "Bob", "Bob"),
    team_id = c("t1", "t1", "t1", "t1", "t1", "t1", "t1", "t1"),
    match_id = c("m1", "m1", "m1", "m1", "m2", "m2", "m2", "m2"),
    action_type = c("pass", "shot", "tackle", "pass", "pass", "pass", "shot", "take_on"),
    player_credit = c(0.1, 0.3, -0.05, 0.15, 0.2, 0.1, 0.4, 0.08),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl)

  # Should have 4 rows: 2 players x 2 matches

  expect_equal(nrow(result), 4)

  # Check Alice in m1: actor credit = 0.1 + 0.3 = 0.4
  alice_m1 <- result[result$player_id == "p1" & result$match_id == "m1", ]
  expect_equal(alice_m1$epv_as_actor, 0.4)
  expect_equal(alice_m1$n_actions, 2L)
  expect_equal(alice_m1$epv_passing, 0.1)
  expect_equal(alice_m1$epv_shooting, 0.3)

  # Check Alice in m2: actor credit = 0.2 + 0.1 = 0.3

  alice_m2 <- result[result$player_id == "p1" & result$match_id == "m2", ]
  expect_equal(alice_m2$epv_as_actor, 0.3)
  expect_equal(alice_m2$epv_passing, 0.3)
})

test_that("aggregate_player_game_epv handles receiver credit", {
  spadl <- data.frame(
    player_id = c("p1", "p1"),
    player_name = c("Alice", "Alice"),
    team_id = c("t1", "t1"),
    match_id = c("m1", "m1"),
    action_type = c("pass", "pass"),
    player_credit = c(0.1, 0.2),
    receiver_player_id = c("p2", "p2"),
    receiver_player_name = c("Bob", "Bob"),
    receiver_credit = c(0.05, 0.08),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl)

  bob <- result[result$player_id == "p2" & result$match_id == "m1", ]
  expect_equal(bob$epv_as_receiver, 0.13)
})

test_that("aggregate_player_game_epv computes offensive/defensive split", {
  spadl <- data.frame(
    player_id = rep("p1", 4),
    player_name = rep("Alice", 4),
    team_id = rep("t1", 4),
    match_id = rep("m1", 4),
    action_type = c("pass", "shot", "take_on", "tackle"),
    player_credit = c(0.1, 0.3, 0.05, -0.02),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl)
  expect_equal(nrow(result), 1)

  # Offensive = passing + shooting + dribbling = 0.1 + 0.3 + 0.05
  expect_equal(result$epv_offensive, 0.45)

  # Defensive = defending + duel_blame = -0.02 + 0

  expect_equal(result$epv_defensive, -0.02)
})

test_that("aggregate_player_game_epv computes per-90 with lineups", {
  spadl <- data.frame(
    player_id = c("p1", "p1"),
    player_name = c("Alice", "Alice"),
    team_id = c("t1", "t1"),
    match_id = c("m1", "m1"),
    action_type = c("pass", "shot"),
    player_credit = c(0.1, 0.2),
    stringsAsFactors = FALSE
  )

  lineups <- data.frame(
    player_id = "p1",
    match_id = "m1",
    minutes_played = 45,
    position = "MID",
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl, lineups = lineups)
  expect_true("minutes_played" %in% names(result))
  expect_true("epv_total_p90" %in% names(result))

  # EPV total = 0.3, minutes = 45, per-90 = 0.3 / (45/90) = 0.6
  expect_equal(result$epv_total_p90, 0.6)
})

test_that("aggregate_player_game_epv handles opponent credit", {
  spadl <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    team_id = c("t1", "t2"),
    match_id = c("m1", "m1"),
    action_type = c("take_on", "tackle"),
    player_credit = c(0.1, -0.05),
    opponent_player_id = c("p2", "p1"),
    opponent_player_name = c("Bob", "Alice"),
    opponent_credit = c(-0.08, 0.03),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl)

  # Bob gets duel_blame = -0.08 (from Alice's take_on)
  bob <- result[result$player_id == "p2" & result$match_id == "m1", ]
  expect_equal(bob$epv_duel_blame, -0.08)

  # Alice gets duel_blame = 0.03 (from Bob's tackle — positive, she won)
  alice <- result[result$player_id == "p1" & result$match_id == "m1", ]
  expect_equal(alice$epv_duel_blame, 0.03)
})

test_that("aggregate_player_game_epv position centering works", {
  spadl <- data.frame(
    player_id = c("p1", "p2", "p3"),
    player_name = c("Alice", "Bob", "Charlie"),
    team_id = c("t1", "t1", "t1"),
    match_id = c("m1", "m1", "m1"),
    action_type = c("pass", "pass", "tackle"),
    player_credit = c(0.3, 0.1, -0.05),
    stringsAsFactors = FALSE
  )

  lineups <- data.frame(
    player_id = c("p1", "p2", "p3"),
    match_id = c("m1", "m1", "m1"),
    minutes_played = c(90, 90, 90),
    position = c("MID", "MID", "DEF"),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_game_epv(spadl, lineups = lineups,
                                       position_center = TRUE)
  expect_true("epv_adj" %in% names(result))

  # Two MIDs: Alice=0.3, Bob=0.1 → mean=0.2
  # epv_adj for Alice = 0.3 - 0.2 = 0.1
  alice <- result[result$player_id == "p1", ]
  expect_equal(alice$epv_adj, 0.1)
})
