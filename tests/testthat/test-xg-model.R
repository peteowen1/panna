# Tests for xG model functions

test_that(".create_shot_features builds correct features from coordinates", {
  features <- .create_shot_features(
    x = c(90, 95, 50),
    y = c(50, 30, 50)
  )

  expect_s3_class(features, "data.frame")
  expect_equal(nrow(features), 3)
  expect_true(all(c("x", "y", "distance_to_goal", "angle_to_goal",
                     "in_penalty_area", "in_six_yard_box",
                     "is_header", "is_right_foot", "is_left_foot",
                     "is_open_play", "is_big_chance") %in% names(features)))
  # Defaults when no bodypart/situation provided

  expect_equal(features$is_header, c(0L, 0L, 0L))
  expect_equal(features$is_open_play, c(1L, 1L, 1L))
})

test_that(".create_shot_features handles bodypart detection consistently", {
  # FBref-style body_part values
  features_fbref <- .create_shot_features(
    x = c(90, 90, 90),
    y = c(50, 50, 50),
    bodypart = c("Head", "Right Foot", "Left Foot")
  )
  expect_equal(features_fbref$is_header, c(1L, 0L, 0L))
  expect_equal(features_fbref$is_right_foot, c(0L, 1L, 0L))
  expect_equal(features_fbref$is_left_foot, c(0L, 0L, 1L))

  # SPADL-style bodypart values
  features_spadl <- .create_shot_features(
    x = c(90, 90, 90),
    y = c(50, 50, 50),
    bodypart = c("head", "foot_right", "foot_left")
  )
  expect_equal(features_spadl$is_header, c(1L, 0L, 0L))
  expect_equal(features_spadl$is_right_foot, c(0L, 1L, 0L))
  expect_equal(features_spadl$is_left_foot, c(0L, 0L, 1L))
})

test_that("prepare_shots_for_xg validates required columns", {
  expect_error(
    prepare_shots_for_xg(data.frame(x = 1, y = 1)),
    "Missing required columns"
  )
})

test_that("prepare_shots_for_xg creates valid features", {
  shots <- data.frame(
    match_id = rep("m1", 5),
    player_id = paste0("p", 1:5),
    player_name = paste0("Player ", 1:5),
    x = c(90, 95, 85, 92, 88),
    y = c(50, 40, 60, 45, 55),
    is_goal = c(1, 0, 0, 1, 0),
    body_part = c("Head", "Right Foot", "Left Foot", "Right Foot", "Head"),
    situation = c("Open Play", "Set Piece", "Corner", "Open Play", "Direct Free Kick"),
    big_chance = c(1, 0, 0, 1, 0)
  )

  features <- prepare_shots_for_xg(shots)

  expect_s3_class(features, "data.frame")
  expect_equal(nrow(features), 5)
  expect_true(all(features$distance_to_goal > 0))
  expect_true(all(features$angle_to_goal >= 0))
  expect_equal(sum(features$is_goal), 2)
  expect_equal(features$is_header, c(1L, 0L, 0L, 0L, 1L))
  expect_equal(features$is_open_play, c(1L, 0L, 0L, 1L, 0L))
})

test_that("fit_xg_model trains a model with predictions in [0,1]", {
  skip_if_not_installed("xgboost")

  # Create mock shot data
  set.seed(42)
  n <- 200
  shots <- data.frame(
    match_id = rep("m1", n),
    player_id = paste0("p", seq_len(n)),
    player_name = paste0("Player ", seq_len(n)),
    x = runif(n, 70, 100),
    y = runif(n, 20, 80),
    is_goal = rbinom(n, 1, 0.1),
    body_part = sample(c("Head", "Right Foot", "Left Foot"), n, replace = TRUE),
    situation = sample(c("Open Play", "Set Piece"), n, replace = TRUE, prob = c(0.8, 0.2)),
    big_chance = rbinom(n, 1, 0.15)
  )

  features <- prepare_shots_for_xg(shots)
  model <- fit_xg_model(features, nrounds = 20, nfolds = 2, verbose = 0)

  expect_s3_class(model, "xg_model")
  expect_true(!is.null(model$model))
  expect_true(!is.null(model$panna_metadata$feature_cols))

  # Predictions should be probabilities
  preds <- predict_xg(model, features)
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("predict_xg handles missing features with zero-fill", {
  skip_if_not_installed("xgboost")

  set.seed(42)
  n <- 100
  shots <- data.frame(
    match_id = rep("m1", n),
    player_id = paste0("p", seq_len(n)),
    player_name = paste0("Player ", seq_len(n)),
    x = runif(n, 70, 100),
    y = runif(n, 20, 80),
    is_goal = rbinom(n, 1, 0.1)
  )

  features <- prepare_shots_for_xg(shots)
  model <- fit_xg_model(features, nrounds = 10, nfolds = 2, verbose = 0)

  # Predict with minimal features (missing bodypart/situation columns)
  minimal <- data.frame(
    x = c(90, 95),
    y = c(50, 40),
    distance_to_goal = c(10, 6),
    angle_to_goal = c(0.5, 0.3)
  )

  preds <- predict_xg(model, minimal)
  expect_length(preds, 2)
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("derive_xa assigns assists correctly", {
  # Create minimal SPADL-like data with a goal preceded by a pass
  spadl <- data.frame(
    match_id = rep("m1", 5),
    action_id = 1:5,
    period_id = rep(1L, 5),
    team_id = c("t1", "t1", "t1", "t1", "t2"),
    player_id = c("p1", "p2", "p3", "p2", "p4"),
    player_name = c("Player 1", "Player 2", "Player 3", "Player 2", "Player 4"),
    action_type = c("pass", "pass", "shot", "pass", "shot"),
    result = c("success", "success", "success", "success", "fail"),
    bodypart = rep("foot_right", 5),
    start_x = c(50, 70, 90, 60, 85),
    start_y = c(50, 50, 50, 50, 50),
    end_x = c(70, 90, 100, 85, 90),
    end_y = c(50, 50, 50, 50, 50),
    xg = c(0, 0, 0.3, 0, 0.1),
    stringsAsFactors = FALSE
  )

  result <- derive_xa(spadl)
  expect_true("xa" %in% names(result))
  # The assist (pass before goal) should have non-zero xA
  goal_idx <- which(result$action_type == "shot" & result$result == "success")
  expect_true(length(goal_idx) > 0)
  # Player 2's pass (action_id=2) before the goal (action_id=3) should get xA
  passer_row <- result[result$action_id == 2, ]
  expect_equal(passer_row$xa, 0.3)
  expect_equal(passer_row$is_assist, 1L)
})

test_that("extract_shots_from_spadl identifies shots", {
  spadl <- data.frame(
    match_id = rep("m1", 6),
    action_id = 1:6,
    team_id = rep("t1", 6),
    player_id = c("p1", "p1", "p2", "p2", "p3", "p3"),
    player_name = c("A", "A", "B", "B", "C", "C"),
    action_type = c("pass", "shot", "pass", "shot", "dribble", "shot"),
    result = c("success", "success", "success", "fail", "success", "fail"),
    bodypart = rep("foot_right", 6),
    start_x = c(50, 90, 60, 88, 70, 92),
    start_y = rep(50, 6),
    end_x = c(90, 100, 88, 95, 88, 98),
    end_y = rep(50, 6),
    xg = c(0, 0.3, 0, 0.15, 0, 0.2),
    time_seconds = c(600, 660, 1200, 1260, 1800, 1860),
    stringsAsFactors = FALSE
  )

  lineups <- data.frame(
    match_id = "m1",
    team_id = rep("t1", 3),
    team_name = rep("Team A", 3),
    player_id = c("p1", "p2", "p3"),
    player_name = c("A", "B", "C"),
    minutes_played = c(90, 90, 90),
    stringsAsFactors = FALSE
  )

  shots <- extract_shots_from_spadl(spadl, lineups)
  expect_s3_class(shots, "data.frame")
  expect_equal(nrow(shots), 3)
  expect_true(all(c("xg", "is_goal", "player_id") %in% names(shots)))
})

test_that("aggregate_player_xmetrics computes per-player stats", {
  spadl <- data.frame(
    match_id = rep("m1", 8),
    action_id = 1:8,
    period_id = rep(1L, 8),
    team_id = rep("t1", 8),
    player_id = c("p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2"),
    player_name = c("A", "A", "A", "A", "B", "B", "B", "B"),
    action_type = c("pass", "shot", "pass", "pass", "pass", "shot", "pass", "shot"),
    result = c("success", "success", "success", "fail", "success", "fail", "success", "fail"),
    bodypart = rep("foot_right", 8),
    start_x = c(50, 90, 60, 70, 55, 88, 65, 92),
    start_y = rep(50, 8),
    end_x = c(90, 100, 70, 75, 88, 95, 85, 98),
    end_y = rep(50, 8),
    xg = c(0, 0.3, 0, 0, 0, 0.15, 0, 0.2),
    xa = c(0.25, 0, 0, 0, 0, 0, 0.1, 0),
    is_key_pass = c(1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L),
    is_assist = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    xpass = c(0.8, NA, 0.7, 0.6, 0.75, NA, 0.85, NA),
    opta_type_id = c(1L, 13L, 1L, 1L, 1L, 15L, 1L, 14L),
    time_seconds = c(300, 600, 900, 1200, 1500, 1800, 2100, 2400),
    stringsAsFactors = FALSE
  )

  lineups <- data.frame(
    match_id = "m1",
    team_id = rep("t1", 2),
    team_name = rep("Team A", 2),
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    minutes_played = c(90, 90),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_xmetrics(spadl, lineups, min_minutes = 0)
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2)
  expect_true("xg" %in% names(result) || "total_xg" %in% names(result))
})
