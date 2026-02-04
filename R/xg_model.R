# xG (Expected Goals) Model for EPV
#
# Builds an xG model from Opta shot data since Opta doesn't provide xG values.
# Uses XGBoost to predict goal probability from shot location and context.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
NULL


#' Prepare Shot Data for xG Model
#'
#' Prepares Opta shot event data with features needed for xG modeling.
#'
#' @param shot_events Data frame from load_opta_shot_events()
#'
#' @return Data frame with xG features:
#'   \itemize{
#'     \item x, y: Shot coordinates (normalized)
#'     \item distance_to_goal: Distance from shot to goal center
#'     \item angle_to_goal: Visible angle to goal
#'     \item is_header: Binary indicator for headed shots
#'     \item is_big_chance: Binary indicator for big chances
#'     \item is_penalty: Binary indicator for penalties
#'     \item is_direct_freekick: Binary for direct free kicks
#'     \item shot_type_*: One-hot encoded shot types
#'     \item is_goal: Target variable (1 = goal)
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' shots <- load_opta_shot_events("ENG", "2024-2025")
#' shot_features <- prepare_shots_for_xg(shots)
#' }
prepare_shots_for_xg <- function(shot_events) {
  if (is.null(shot_events) || nrow(shot_events) == 0) {
    cli::cli_abort("No shot events provided")
  }

  cli::cli_alert_info("Preparing {format(nrow(shot_events), big.mark=',')} shots for xG modeling...")

  # Ensure required columns
  required <- c("x", "y", "is_goal")
  if (!all(required %in% names(shot_events))) {
    missing <- setdiff(required, names(shot_events))
    cli::cli_abort("Missing required columns: {paste(missing, collapse=', ')}")
  }

  # Create features
  features <- data.frame(
    match_id = shot_events$match_id,
    event_id = if ("event_id" %in% names(shot_events)) shot_events$event_id else seq_len(nrow(shot_events)),
    player_id = shot_events$player_id,
    player_name = shot_events$player_name,
    x = shot_events$x,
    y = shot_events$y,
    stringsAsFactors = FALSE
  )

  # Distance and angle to goal
  features$distance_to_goal <- calculate_distance_to_goal(features$x, features$y)
  features$angle_to_goal <- calculate_angle_to_goal(features$x, features$y)

  # Location features
  features$in_penalty_area <- as.integer(is_in_penalty_area(features$x, features$y))
  features$in_six_yard_box <- as.integer(features$x > 94 & features$y > 37 & features$y < 63)

  # Body part (if available)
  if ("body_part" %in% names(shot_events)) {
    features$is_header <- as.integer(grepl("head", tolower(shot_events$body_part)))
    features$is_right_foot <- as.integer(grepl("right", tolower(shot_events$body_part)))
    features$is_left_foot <- as.integer(grepl("left", tolower(shot_events$body_part)))
  } else {
    features$is_header <- 0L
    features$is_right_foot <- 0L
    features$is_left_foot <- 0L
  }

  # Situation (if available)
  if ("situation" %in% names(shot_events)) {
    features$is_open_play <- as.integer(grepl("open", tolower(shot_events$situation)))
    features$is_set_piece <- as.integer(grepl("set", tolower(shot_events$situation)))
    features$is_corner <- as.integer(grepl("corner", tolower(shot_events$situation)))
    features$is_penalty <- as.integer(grepl("penalty", tolower(shot_events$situation)))
    features$is_direct_freekick <- as.integer(grepl("free", tolower(shot_events$situation)))
  } else {
    features$is_open_play <- 1L
    features$is_set_piece <- 0L
    features$is_corner <- 0L
    features$is_penalty <- 0L
    features$is_direct_freekick <- 0L
  }

  # Big chance (if available)
  if ("big_chance" %in% names(shot_events)) {
    features$is_big_chance <- as.integer(shot_events$big_chance)
  } else {
    features$is_big_chance <- 0L
  }

  # Shot type from type_id (if available)
  if ("type_id" %in% names(shot_events)) {
    features$shot_saved <- as.integer(shot_events$type_id == 13)
    features$shot_post <- as.integer(shot_events$type_id == 14)
    features$shot_miss <- as.integer(shot_events$type_id == 15)
  }

  # Target variable
  features$is_goal <- as.integer(shot_events$is_goal)

  # Handle missing values
  numeric_cols <- sapply(features, is.numeric)
  features[numeric_cols] <- lapply(features[numeric_cols], function(x) {
    ifelse(is.na(x), 0, x)
  })

  cli::cli_alert_success("Prepared shot features: {sum(features$is_goal)} goals from {nrow(features)} shots")

  features
}


#' Fit xG Model using XGBoost
#'
#' Trains an XGBoost classifier to predict goal probability from shot features.
#' Uses cross-validation to find optimal number of boosting rounds.
#'
#' @param shot_features Data frame from prepare_shots_for_xg()
#' @param exclude_penalties Whether to exclude penalties from training (default TRUE)
#' @param nfolds Number of CV folds (default 5)
#' @param max_depth Maximum tree depth (default 6)
#' @param eta Learning rate (default 0.05)
#' @param subsample Row subsampling (default 0.8)
#' @param colsample_bytree Column subsampling (default 0.8)
#' @param nrounds Maximum boosting rounds (default 500)
#' @param early_stopping_rounds Early stopping patience (default 50)
#' @param verbose Print progress (0=silent, 1=progress)
#'
#' @return List with:
#'   \itemize{
#'     \item model: Fitted XGBoost model
#'     \item cv_result: Cross-validation results
#'     \item importance: Feature importance
#'     \item calibration: Calibration metrics
#'     \item panna_metadata: Model metadata
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' shots <- load_opta_shot_events("ENG", "2024-2025")
#' shot_features <- prepare_shots_for_xg(shots)
#' xg_model <- fit_xg_model(shot_features)
#' }
fit_xg_model <- function(shot_features,
                          exclude_penalties = TRUE,
                          nfolds = 5,
                          max_depth = 6,
                          eta = 0.05,
                          subsample = 0.8,
                          colsample_bytree = 0.8,
                          nrounds = 500,
                          early_stopping_rounds = 50,
                          verbose = 1) {

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. Install with: install.packages('xgboost')")
  }

  # Filter out penalties if requested
  if (exclude_penalties && "is_penalty" %in% names(shot_features)) {
    shot_features <- shot_features[shot_features$is_penalty == 0, ]
    cli::cli_alert_info("Excluded penalties, {nrow(shot_features)} shots remaining")
  }

  # Define feature columns
  feature_cols <- c(
    "x", "y", "distance_to_goal", "angle_to_goal",
    "in_penalty_area", "in_six_yard_box",
    "is_header", "is_right_foot", "is_left_foot",
    "is_open_play", "is_set_piece", "is_corner", "is_direct_freekick",
    "is_big_chance"
  )

  # Use available features
  available_features <- intersect(feature_cols, names(shot_features))
  if (length(available_features) < 3) {
    cli::cli_abort("Insufficient features available for xG model")
  }

  cli::cli_alert_info("Fitting xG model with {length(available_features)} features on {nrow(shot_features)} shots...")

  # Prepare data
  X <- as.matrix(shot_features[, available_features, drop = FALSE])
  y <- shot_features$is_goal

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  # XGBoost parameters for binary classification
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = max_depth,
    eta = eta,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = 10  # More conservative for probability estimation
  )

  # Cross-validation
  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nfold = nfolds,
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose,
    print_every_n = 50
  )

  # Get best iteration
  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || length(best_nrounds) == 0) {
    eval_log <- cv_result$evaluation_log
    best_nrounds <- which.min(eval_log$test_logloss_mean)
  }
  best_logloss <- cv_result$evaluation_log$test_logloss_mean[best_nrounds]

  cli::cli_alert_info("XGBoost CV: best iteration = {best_nrounds}, CV LogLoss = {round(best_logloss, 4)}")

  # Fit final model
  final_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Get predictions for calibration
  y_pred <- stats::predict(final_model, dtrain)

  # Calculate calibration metrics
  calibration <- calculate_xg_calibration(y, y_pred)

  # Feature importance
  importance <- xgboost::xgb.importance(
    feature_names = available_features,
    model = final_model
  )

  # Summary statistics
  cli::cli_alert_success(paste0(
    "xG model fit complete. ",
    "LogLoss: ", round(best_logloss, 4), ", ",
    "Mean xG: ", round(mean(y_pred), 4), ", ",
    "Actual rate: ", round(mean(y), 4)
  ))

  result <- list(
    model = final_model,
    cv_result = cv_result,
    importance = importance,
    calibration = calibration,
    best_nrounds = best_nrounds,
    best_logloss = best_logloss,
    panna_metadata = list(
      type = "xg_model",
      feature_cols = available_features,
      n_shots = length(y),
      n_goals = sum(y),
      goal_rate = mean(y),
      params = params,
      exclude_penalties = exclude_penalties
    )
  )

  class(result) <- c("xg_model", "list")
  result
}


#' Calculate xG Model Calibration
#'
#' Assesses how well predicted probabilities match actual goal rates.
#'
#' @param actual Binary vector of actual outcomes (0/1)
#' @param predicted Predicted probabilities
#' @param n_bins Number of calibration bins (default 10)
#'
#' @return List with calibration metrics
#' @keywords internal
calculate_xg_calibration <- function(actual, predicted, n_bins = 10) {
  # Create bins
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bins <- cut(predicted, breaks = breaks, include.lowest = TRUE, labels = FALSE)

  # Calculate actual rate per bin
  calibration_df <- stats::aggregate(
    data.frame(actual = actual, predicted = predicted),
    by = list(bin = bins),
    FUN = function(x) c(mean = mean(x), n = length(x))
  )

  # Flatten
  cal_data <- data.frame(
    bin = calibration_df$bin,
    actual_rate = calibration_df$actual[, "mean"],
    predicted_rate = calibration_df$predicted[, "mean"],
    n_shots = calibration_df$actual[, "n"]
  )

  # Calibration error (weighted by bin size)
  weights <- cal_data$n_shots / sum(cal_data$n_shots)
  calibration_error <- sum(weights * abs(cal_data$actual_rate - cal_data$predicted_rate))

  # Brier score
  brier_score <- mean((predicted - actual)^2)

  list(
    calibration_data = cal_data,
    calibration_error = calibration_error,
    brier_score = brier_score,
    mean_predicted = mean(predicted),
    mean_actual = mean(actual)
  )
}


#' Predict xG for Shots
#'
#' Applies trained xG model to predict goal probability for shots.
#'
#' @param xg_model Fitted xG model from fit_xg_model()
#' @param shot_features Data frame with shot features (same format as training)
#'
#' @return Vector of xG predictions (probabilities)
#'
#' @export
#' @examples
#' \dontrun{
#' xg_model <- fit_xg_model(training_shots)
#' new_shots <- prepare_shots_for_xg(new_shot_events)
#' xg_pred <- predict_xg(xg_model, new_shots)
#' }
predict_xg <- function(xg_model, shot_features) {
  feature_cols <- xg_model$panna_metadata$feature_cols

  # Ensure all required columns exist
  missing_cols <- setdiff(feature_cols, names(shot_features))
  if (length(missing_cols) > 0) {
    # Add missing columns as zeros
    for (col in missing_cols) {
      shot_features[[col]] <- 0
    }
  }

  # Prepare prediction matrix
  X <- as.matrix(shot_features[, feature_cols, drop = FALSE])
  X[is.na(X)] <- 0

  # Predict
  xg_pred <- stats::predict(xg_model$model, X)

  xg_pred
}


#' Add xG to SPADL Actions
#'
#' Adds xG predictions to shot actions in SPADL data.
#'
#' @param spadl_actions SPADL actions data frame
#' @param xg_model Fitted xG model
#'
#' @return SPADL actions with xg column added for shots
#' @export
add_xg_to_spadl <- function(spadl_actions, xg_model) {
  # Initialize xG column
  spadl_actions$xg <- 0

  # Find shots
  shot_idx <- spadl_actions$action_type == "shot"

  if (sum(shot_idx) == 0) {
    cli::cli_warn("No shots found in SPADL actions")
    return(spadl_actions)
  }

  # Prepare shot features
  shots <- spadl_actions[shot_idx, ]

  # Use is_big_chance from SPADL if available (from Opta qualifier 214)
  is_big_chance <- if ("is_big_chance" %in% names(shots)) {
    as.integer(shots$is_big_chance)
  } else {
    0L
  }

  shot_features <- data.frame(
    x = shots$start_x,
    y = shots$start_y,
    distance_to_goal = calculate_distance_to_goal(shots$start_x, shots$start_y),
    angle_to_goal = calculate_angle_to_goal(shots$start_x, shots$start_y),
    in_penalty_area = as.integer(is_in_penalty_area(shots$start_x, shots$start_y)),
    in_six_yard_box = as.integer(shots$start_x > 94 & shots$start_y > 37 & shots$start_y < 63),
    is_header = as.integer(shots$bodypart == "head"),
    is_right_foot = 0L,
    is_left_foot = 0L,
    is_open_play = 1L,
    is_set_piece = 0L,
    is_corner = 0L,
    is_direct_freekick = 0L,
    is_big_chance = is_big_chance,
    stringsAsFactors = FALSE
  )

  # Predict xG
  xg_pred <- predict_xg(xg_model, shot_features)

  # Add to SPADL
  spadl_actions$xg[shot_idx] <- xg_pred

  cli::cli_alert_success("Added xG to {sum(shot_idx)} shots (mean xG: {round(mean(xg_pred), 3)})")

  spadl_actions
}


#' Load Pre-trained xG Model
#'
#' Loads xG model from saved RDS file or downloads from GitHub releases.
#'
#' @param path Path to model RDS file. If NULL, attempts to download from releases.
#'
#' @return Fitted xG model
#' @export
load_xg_model <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) {
    model <- readRDS(path)
    cli::cli_alert_success("Loaded xG model from {path}")
    return(model)
  }

  # Try default location in pannadata (Opta-specific models)
  default_path <- file.path(pannadata_dir(), "models", "opta", "xg_model.rds")
  if (file.exists(default_path)) {
    model <- readRDS(default_path)
    cli::cli_alert_success("Loaded xG model from {default_path}")
    return(model)
  }

  cli::cli_abort(c(
    "xG model not found.",
    "i" = "Train a new model with fit_xg_model() or download with pb_download_epv_models()"
  ))
}


#' Save xG Model
#'
#' Saves trained xG model to RDS file.
#'
#' @param xg_model Fitted xG model from fit_xg_model()
#' @param path Path to save model. If NULL, saves to pannadata/models/
#'
#' @return Invisibly returns the path
#' @export
save_xg_model <- function(xg_model, path = NULL) {
  if (is.null(path)) {
    model_dir <- file.path(pannadata_dir(), "models", "opta")
    dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(model_dir, "xg_model.rds")
  }

  saveRDS(xg_model, path)
  cli::cli_alert_success("Saved xG model to {path}")

  invisible(path)
}


#' Validate xG Model Performance
#'
#' Evaluates xG model on held-out test data.
#'
#' @param xg_model Fitted xG model
#' @param test_shots Shot features for validation
#'
#' @return List with validation metrics
#' @export
validate_xg_model <- function(xg_model, test_shots) {
  # Predict
  xg_pred <- predict_xg(xg_model, test_shots)
  actual <- test_shots$is_goal

  # Calibration
  calibration <- calculate_xg_calibration(actual, xg_pred)

  # Log loss
  eps <- 1e-15
  xg_pred_clipped <- pmax(pmin(xg_pred, 1 - eps), eps)
  logloss <- -mean(actual * log(xg_pred_clipped) + (1 - actual) * log(1 - xg_pred_clipped))

  # AUC (if pROC available)
  auc <- NA
  if (requireNamespace("pROC", quietly = TRUE)) {
    roc_obj <- pROC::roc(actual, xg_pred, quiet = TRUE)
    auc <- pROC::auc(roc_obj)
  }

  results <- list(
    n_shots = length(actual),
    n_goals = sum(actual),
    goal_rate = mean(actual),
    mean_xg = mean(xg_pred),
    logloss = logloss,
    brier_score = calibration$brier_score,
    calibration_error = calibration$calibration_error,
    auc = auc,
    calibration_data = calibration$calibration_data
  )

  cli::cli_alert_success(paste0(
    "xG validation: LogLoss=", round(logloss, 4),
    ", Brier=", round(calibration$brier_score, 4),
    if (!is.na(auc)) paste0(", AUC=", round(auc, 3)) else ""
  ))

  results
}
