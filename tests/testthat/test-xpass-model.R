# Tests for xPass model functions

test_that("create_pass_features computes expected features", {
  dt <- data.table::data.table(
    start_x = c(50, 30, 80),
    start_y = c(50, 20, 60),
    end_x = c(70, 50, 90),
    end_y = c(50, 40, 55),
    bodypart = c("foot_right", "head", "foot_left")
  )

  result <- create_pass_features(dt)

  expect_true("pass_distance" %in% names(result))
  expect_true("is_forward" %in% names(result))
  expect_true("is_progressive" %in% names(result))
  expect_true("is_headed_pass" %in% names(result))
  expect_true(all(result$pass_distance > 0))
  expect_equal(result$is_headed_pass, c(0L, 1L, 0L))
})

test_that("prepare_passes_for_xpass filters to passes only", {
  spadl <- data.frame(
    match_id = rep("m1", 6),
    action_id = 1:6,
    player_id = rep("p1", 6),
    player_name = rep("Player 1", 6),
    team_id = rep("t1", 6),
    action_type = c("pass", "pass", "shot", "pass", "dribble", "pass"),
    result = c("success", "fail", "success", "success", "success", "fail"),
    bodypart = rep("foot_right", 6),
    start_x = c(50, 40, 90, 60, 70, 30),
    start_y = rep(50, 6),
    end_x = c(70, 55, 100, 80, 75, 50),
    end_y = rep(50, 6),
    stringsAsFactors = FALSE
  )

  features <- prepare_passes_for_xpass(spadl)

  expect_s3_class(features, "data.frame")
  expect_equal(nrow(features), 4)  # Only passes
  expect_true("completed" %in% names(features))
  expect_equal(features$completed, c(1L, 0L, 1L, 0L))
})

test_that("fit_xpass_model trains with predictions in [0,1]", {
  skip_if_not_installed("xgboost")

  set.seed(42)
  n <- 300
  spadl <- data.frame(
    match_id = rep("m1", n),
    action_id = seq_len(n),
    player_id = sample(paste0("p", 1:5), n, replace = TRUE),
    player_name = sample(paste0("Player ", 1:5), n, replace = TRUE),
    team_id = rep("t1", n),
    action_type = rep("pass", n),
    result = sample(c("success", "fail"), n, replace = TRUE, prob = c(0.75, 0.25)),
    bodypart = sample(c("foot_right", "foot_left", "head"), n, replace = TRUE),
    start_x = runif(n, 10, 90),
    start_y = runif(n, 10, 90),
    end_x = runif(n, 10, 100),
    end_y = runif(n, 10, 90),
    stringsAsFactors = FALSE
  )

  features <- prepare_passes_for_xpass(spadl)
  model <- fit_xpass_model(features, nrounds = 20, nfolds = 2, verbose = 0)

  expect_s3_class(model, "xpass_model")
  expect_true(!is.null(model$panna_metadata$feature_cols))

  preds <- predict_xpass(model, features)
  expect_true(all(preds >= 0 & preds <= 1))
})

test_that("predict_xpass handles missing features", {
  skip_if_not_installed("xgboost")

  set.seed(42)
  n <- 200
  spadl <- data.frame(
    match_id = rep("m1", n),
    action_id = seq_len(n),
    player_id = rep("p1", n),
    player_name = rep("Player 1", n),
    team_id = rep("t1", n),
    action_type = rep("pass", n),
    result = sample(c("success", "fail"), n, replace = TRUE, prob = c(0.75, 0.25)),
    bodypart = rep("foot_right", n),
    start_x = runif(n, 10, 90),
    start_y = runif(n, 10, 90),
    end_x = runif(n, 10, 100),
    end_y = runif(n, 10, 90),
    stringsAsFactors = FALSE
  )

  features <- prepare_passes_for_xpass(spadl)
  model <- fit_xpass_model(features, nrounds = 10, nfolds = 2, verbose = 0)

  # Predict with minimal features
  minimal <- data.frame(
    start_x = c(50, 60),
    start_y = c(50, 40),
    end_x = c(70, 80),
    end_y = c(50, 50)
  )

  preds <- predict_xpass(model, minimal)
  expect_length(preds, 2)
  expect_true(all(preds >= 0 & preds <= 1))
})
