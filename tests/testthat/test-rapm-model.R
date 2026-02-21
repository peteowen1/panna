# Tests for RAPM model functions

# Helper to create minimal RAPM data structure for testing
create_test_rapm_data <- function(n_splints = 50, n_players = 20) {
  set.seed(42)
  # Create player IDs
  player_ids <- paste0("player_", seq_len(n_players))

  # Create design matrix: 2 rows per splint (home/away perspective)
  n_rows <- n_splints * 2

  # Player columns: offense and defense for each player
  n_player_cols <- n_players * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_player_cols)

  # Randomly assign ~10 players per splint (5 home, 5 away)
  for (i in seq_len(n_splints)) {
    # Home attacking row
    home_players <- sample(seq_len(n_players), 5)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 5)

    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2

    # Home attacking: home players on offense (+1), away on defense (+1)
    for (p in home_players) {
      X_players[row_home, p] <- 1  # offense
    }
    for (p in away_players) {
      X_players[row_home, n_players + p] <- 1  # defense
    }

    # Away attacking: swap roles
    for (p in away_players) {
      X_players[row_away, p] <- 1  # offense
    }
    for (p in home_players) {
      X_players[row_away, n_players + p] <- 1  # defense
    }
  }

  # Add covariate columns
  covariates <- c("is_home", "goal_diff")
  X_covariates <- matrix(
    c(rep(c(1, 0), n_splints), runif(n_rows, -2, 2)),
    nrow = n_rows
  )

  # Combine
  X <- cbind(X_players, X_covariates)

  # Column names
  player_cols_off <- paste0(player_ids, "_off")
  player_cols_def <- paste0(player_ids, "_def")
  colnames(X) <- c(player_cols_off, player_cols_def, covariates)

  # Target: xGF per 90
  y <- rnorm(n_rows, mean = 1.5, sd = 0.5)

  # Weights based on duration
  weights <- runif(n_rows, 5, 30)

  # Minutes per player
  minutes <- sample(500:3000, n_players, replace = TRUE)

  # Player mapping (includes total_minutes)
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("Player", LETTERS[seq_len(n_players)]),
    total_minutes = minutes,
    stringsAsFactors = FALSE
  )

  list(
    X = X,
    y = y,
    weights = weights,
    player_ids = player_ids,
    player_mapping = player_mapping,
    covariate_cols = covariates,
    covariate_names = covariates,  # Alias for production code
    n_players = n_players,
    total_minutes = stats::setNames(minutes, player_ids)
  )
}


# =============================================================================
# Shared fixtures: fit once, reuse across tests
# =============================================================================

# Base RAPM model (shared by tests 1-3)
rapm_data_basic <- create_test_rapm_data(n_splints = 30, n_players = 15)
rapm_model_basic <- if (requireNamespace("glmnet", quietly = TRUE)) {
  fit_rapm(rapm_data_basic, alpha = 0, nfolds = 3, parallel = FALSE)
}

# xRAPM model (shared by tests 4-5)
xrapm_fixture <- if (requireNamespace("glmnet", quietly = TRUE)) {
  set.seed(42)
  offense_prior <- stats::setNames(
    rnorm(15, 0, 0.5),
    rapm_data_basic$player_ids
  )
  defense_prior <- stats::setNames(
    rnorm(15, 0, 0.3),
    rapm_data_basic$player_ids
  )
  model <- fit_rapm_with_prior(
    rapm_data_basic,
    offense_prior = offense_prior,
    defense_prior = defense_prior,
    alpha = 0,
    nfolds = 3
  )
  list(model = model, offense_prior = offense_prior, defense_prior = defense_prior)
}


test_that("fit_rapm returns valid model structure", {
  skip_if_not_installed("glmnet")

  model <- rapm_model_basic

  # Check model structure
  expect_true(inherits(model, "cv.glmnet"))
  expect_true("panna_metadata" %in% names(model))
  expect_equal(model$panna_metadata$type, "rapm")
  expect_equal(model$panna_metadata$n_player_cols, 30)  # 15 players * 2 (off/def)

  # Lambda values exist
  expect_true(length(model$lambda.min) > 0)
  expect_true(length(model$lambda.1se) > 0)
})


test_that("extract_rapm_ratings returns player ratings", {
  skip_if_not_installed("glmnet")

  ratings <- extract_rapm_ratings(rapm_model_basic)

  # Check output structure
  expect_true(is.data.frame(ratings))
  expect_true("player_id" %in% names(ratings))
  expect_true("player_name" %in% names(ratings))
  expect_true("rapm" %in% names(ratings))
  expect_true("offense" %in% names(ratings))
  expect_true("defense" %in% names(ratings))

  # All players included
  expect_equal(nrow(ratings), 15)

  # Ratings are numeric
  expect_type(ratings$rapm, "double")
  expect_type(ratings$offense, "double")
  expect_type(ratings$defense, "double")
})


test_that("get_covariate_effects extracts covariate effects", {
  skip_if_not_installed("glmnet")

  covariates <- get_covariate_effects(rapm_model_basic)

  # Check structure - returns named vector

  expect_true(is.numeric(covariates))
  expect_true(!is.null(names(covariates)))

  # Contains expected covariates
  expect_true("is_home" %in% names(covariates))
  expect_true("goal_diff" %in% names(covariates))
})


test_that("fit_rapm_with_prior accepts and uses prior", {
  skip_if_not_installed("glmnet")

  model <- xrapm_fixture$model

  # Check model structure
  expect_true(inherits(model, "cv.glmnet"))

  expect_true("panna_metadata" %in% names(model))
  expect_equal(model$panna_metadata$type, "xrapm")

  # Prior info stored
  expect_true("offense_prior" %in% names(model$panna_metadata))
  expect_true("defense_prior" %in% names(model$panna_metadata))
})


test_that("extract_xrapm_ratings includes deviation from prior", {
  skip_if_not_installed("glmnet")

  ratings <- extract_xrapm_ratings(xrapm_fixture$model)

  # Check output structure
  expect_true(is.data.frame(ratings))
  expect_true("xrapm" %in% names(ratings))
  expect_true("off_prior" %in% names(ratings))
  expect_true("def_prior" %in% names(ratings))
  expect_true("off_deviation" %in% names(ratings))
  expect_true("def_deviation" %in% names(ratings))

  # All players included
  expect_equal(nrow(ratings), 15)
})


test_that("RAPM ratings sum to approximately zero", {
  skip_if_not_installed("glmnet")

  rapm_data <- create_test_rapm_data(n_splints = 50, n_players = 20)
  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)
  ratings <- extract_rapm_ratings(model)

  # Ridge regularization should center ratings near zero
  mean_rapm <- mean(ratings$rapm)
  expect_true(abs(mean_rapm) < 0.5)  # Relatively centered
})


test_that("fit_rapm handles use_weights parameter", {
  skip_if_not_installed("glmnet")

  rapm_data <- create_test_rapm_data(n_splints = 30, n_players = 15)

  # With weights
  model_weighted <- fit_rapm(rapm_data, alpha = 0, nfolds = 3,
                             use_weights = TRUE, parallel = FALSE)

  # Without weights
  model_unweighted <- fit_rapm(rapm_data, alpha = 0, nfolds = 3,
                               use_weights = FALSE, parallel = FALSE)

  # Both should produce valid models
  expect_true(inherits(model_weighted, "cv.glmnet"))
  expect_true(inherits(model_unweighted, "cv.glmnet"))

  # Coefficients may differ
  coef_weighted <- as.vector(coef(model_weighted, s = "lambda.min"))
  coef_unweighted <- as.vector(coef(model_unweighted, s = "lambda.min"))

  # Not all coefficients should be identical
  expect_false(all(coef_weighted == coef_unweighted))
})
