# Tests for SPM model functions

# Helper to create minimal player stats for SPM testing
create_test_player_stats <- function(n_players = 30) {
  set.seed(42)
  player_ids <- paste0("player_", seq_len(n_players))

  data.frame(
    player_id = player_ids,
    player_name = paste("Player", LETTERS[((seq_len(n_players) - 1) %% 26) + 1]),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    n_matches = sample(10:38, n_players, replace = TRUE),
    # Per-90 stats
    goals_p90 = runif(n_players, 0, 0.8),
    assists_p90 = runif(n_players, 0, 0.5),
    xg_p90 = runif(n_players, 0, 0.7),
    npxg_p90 = runif(n_players, 0, 0.6),
    xa_p90 = runif(n_players, 0, 0.4),
    shots_p90 = runif(n_players, 0.5, 4),
    shots_on_target_p90 = runif(n_players, 0.2, 2),
    sca_p90 = runif(n_players, 1, 5),
    gca_p90 = runif(n_players, 0.1, 0.8),
    tackles_p90 = runif(n_players, 0.5, 4),
    interceptions_p90 = runif(n_players, 0.3, 2.5),
    blocks_p90 = runif(n_players, 0.2, 2),
    clearances_p90 = runif(n_players, 0, 5),
    progressive_passes_p90 = runif(n_players, 1, 8),
    progressive_carries_p90 = runif(n_players, 0.5, 5),
    touches_p90 = runif(n_players, 30, 90),
    npxg_plus_xa_p90 = runif(n_players, 0, 0.8),
    stringsAsFactors = FALSE
  )
}

# Helper to create SPM training data (player stats + RAPM)
create_test_spm_train_data <- function(n_players = 30) {
  stats <- create_test_player_stats(n_players)

  # Add mock RAPM ratings (target variable)
  # RAPM correlates with offensive stats minus defensive load
  stats$rapm <- 0.5 * stats$npxg_p90 +
    0.3 * stats$xa_p90 +
    0.2 * stats$gca_p90 -
    0.1 * stats$tackles_p90 +
    rnorm(n_players, 0, 0.2)

  stats
}


test_that("fit_spm_model returns valid glmnet object", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)

  model <- fit_spm_model(
    train_data,
    predictor_cols = NULL,  # Use default p90 cols
    alpha = 0.5,
    nfolds = 3
  )

  # Check model structure
  expect_true(inherits(model, "cv.glmnet"))
  expect_true("panna_metadata" %in% names(model))
  expect_equal(model$panna_metadata$type, "spm")

  # Has predictor columns recorded
  expect_true(length(model$panna_metadata$predictor_cols) > 0)
})


test_that("fit_spm_model respects weight_by_minutes parameter", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)

  model_weighted <- fit_spm_model(
    train_data,
    alpha = 0.5,
    nfolds = 3,
    weight_by_minutes = TRUE
  )

  model_unweighted <- fit_spm_model(
    train_data,
    alpha = 0.5,
    nfolds = 3,
    weight_by_minutes = FALSE
  )

  # Both valid models
  expect_true(inherits(model_weighted, "cv.glmnet"))
  expect_true(inherits(model_unweighted, "cv.glmnet"))

  # Metadata records weighting
  expect_true(model_weighted$panna_metadata$weight_by_minutes)
  expect_false(model_unweighted$panna_metadata$weight_by_minutes)
})


test_that("calculate_spm_ratings produces predictions for all players", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)
  model <- fit_spm_model(train_data, alpha = 0.5, nfolds = 3)

  # Predict on same data
  ratings <- calculate_spm_ratings(train_data, model)

  # Check output
  expect_true(is.data.frame(ratings))
  expect_true("spm" %in% names(ratings))
  expect_true("player_id" %in% names(ratings))
  expect_equal(nrow(ratings), 50)

  # SPM values are numeric
  expect_type(ratings$spm, "double")
})


test_that("extract_spm_coefficients returns feature weights", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)
  model <- fit_spm_model(train_data, alpha = 0.5, nfolds = 3)

  coefs <- extract_spm_coefficients(model)

  # Check structure
  expect_true(is.numeric(coefs))
  expect_true(length(coefs) > 0)
  expect_true("(Intercept)" %in% names(coefs))
})


test_that("get_spm_feature_importance returns top features", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)
  model <- fit_spm_model(train_data, alpha = 0.5, nfolds = 3)

  importance <- get_spm_feature_importance(model, n = 5)

  # Check structure
  expect_true(is.data.frame(importance))
  expect_true("feature" %in% names(importance))
  expect_true("coefficient" %in% names(importance))
  expect_true("abs_coef" %in% names(importance))

  # Returns at most n features
  expect_lte(nrow(importance), 5)

  # Sorted by absolute coefficient
  if (nrow(importance) > 1) {
    expect_true(all(diff(importance$abs_coef) <= 0))
  }
})


test_that("validate_spm_prediction calculates metrics", {
  skip_if_not_installed("glmnet")

  train_data <- create_test_spm_train_data(n_players = 50)
  model <- fit_spm_model(train_data, alpha = 0.5, nfolds = 3)

  # Create SPM ratings
  spm_ratings <- calculate_spm_ratings(train_data, model)
  spm_ratings$total_minutes <- train_data$total_minutes

  # Create "actual" RAPM
  rapm_ratings <- data.frame(
    player_id = train_data$player_id,
    rapm = train_data$rapm
  )

  validation <- validate_spm_prediction(spm_ratings, rapm_ratings)

  # Check structure
  expect_true(is.list(validation))
  expect_true("r_squared" %in% names(validation))
  expect_true("rmse" %in% names(validation))
  expect_true("correlation" %in% names(validation))
  expect_true("n_players" %in% names(validation))

  # Metrics are reasonable (may be NA if variance is zero)
  expect_true(is.na(validation$r_squared) || (validation$r_squared >= -Inf && validation$r_squared <= 1))
  expect_true(validation$rmse >= 0)
  expect_true(is.na(validation$correlation) || (validation$correlation >= -1 && validation$correlation <= 1))
})


test_that("fit_spm_xgb returns valid model structure", {
  skip_if_not_installed("xgboost")

  train_data <- create_test_spm_train_data(n_players = 50)

  model <- fit_spm_xgb(
    train_data,
    nfolds = 3,
    max_depth = 3,
    nrounds = 50,
    early_stopping_rounds = 10,
    verbose = 0
  )

  # Check structure
  expect_true(is.list(model))
  expect_true("model" %in% names(model))
  expect_true("panna_metadata" %in% names(model))
  expect_equal(model$panna_metadata$type, "spm_xgb")

  # Has best iteration
  expect_true("best_nrounds" %in% names(model))
  expect_true(model$best_nrounds > 0)
})


test_that("calculate_spm_ratings_xgb produces predictions", {
  skip_if_not_installed("xgboost")

  train_data <- create_test_spm_train_data(n_players = 50)

  model <- fit_spm_xgb(
    train_data,
    nfolds = 3,
    max_depth = 3,
    nrounds = 50,
    verbose = 0
  )

  ratings <- calculate_spm_ratings_xgb(train_data, model)

  # Check output
  expect_true(is.data.frame(ratings))
  expect_true("spm" %in% names(ratings))
  expect_equal(nrow(ratings), 50)
})


test_that("calculate_spm_blend combines model predictions", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("xgboost")

  train_data <- create_test_spm_train_data(n_players = 50)

  model_glmnet <- fit_spm_model(train_data, alpha = 0.5, nfolds = 3)
  model_xgb <- fit_spm_xgb(train_data, nfolds = 3, max_depth = 3, nrounds = 50, verbose = 0)

  blend <- calculate_spm_blend(train_data, model_glmnet, model_xgb, weight_glmnet = 0.5)

  # Check output
  expect_true(is.data.frame(blend))
  expect_true("spm" %in% names(blend))
  expect_true("spm_glmnet" %in% names(blend))
  expect_true("spm_xgb" %in% names(blend))

  # Blend is weighted average
  expected_spm <- 0.5 * blend$spm_glmnet + 0.5 * blend$spm_xgb
  expect_equal(blend$spm, expected_spm, tolerance = 1e-10)
})


test_that("build_prior_vector creates aligned prior", {
  player_mapping <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("Alice", "Bob", "Charlie", "Diana"),
    stringsAsFactors = FALSE
  )

  spm_data <- data.frame(
    player_name = c("Alice", "Charlie"),
    offense_spm = c(0.5, -0.3),
    stringsAsFactors = FALSE
  )

  prior <- build_prior_vector(spm_data, "offense_spm", player_mapping)

  # Check structure
  expect_true(is.numeric(prior))
  expect_equal(length(prior), 4)

  # Has correct values
  expect_equal(unname(prior["p1"]), 0.5)
  expect_equal(unname(prior["p3"]), -0.3)

  # Unmatched players get default (0)
  expect_equal(unname(prior["p2"]), 0)
  expect_equal(unname(prior["p4"]), 0)
})


test_that("create_spm_prior handles data frame input", {
  player_mapping <- data.frame(
    player_id = c("p1", "p2", "p3"),
    player_name = c("Alice", "Bob", "Charlie"),
    stringsAsFactors = FALSE
  )

  spm_df <- data.frame(
    player_name = c("Alice", "Bob"),
    spm = c(0.5, -0.2),
    stringsAsFactors = FALSE
  )

  prior <- create_spm_prior(spm_df, player_mapping)

  # Check structure
  expect_true(is.numeric(prior))
  expect_equal(length(prior), 3)

  # Matched players have correct values
  expect_equal(unname(prior["p1"]), 0.5)
  expect_equal(unname(prior["p2"]), -0.2)

  # Unmatched get default
  expect_equal(unname(prior["p3"]), 0)
})
