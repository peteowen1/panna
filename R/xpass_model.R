# xPass (Expected Pass Completion) Model
#
# Predicts pass completion probability for credit assignment in EPV.
# When a pass is completed, credit is split between passer and receiver
# based on pass difficulty: harder passes give more credit to passer.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT .SD .N := fifelse as.data.table
NULL


#' Create pass features (internal helper)
#'
#' Shared feature creation logic for xPass model.
#'
#' @param dt data.table of passes
#'
#' @return data.table with features added
#' @keywords internal
create_pass_features <- function(dt) {
  # Calculate all features vectorized
  dt[, `:=`(
    pass_distance = sqrt((end_x - start_x)^2 + (end_y - start_y)^2),
    pass_angle = atan2(end_y - start_y, end_x - start_x),
    is_forward = as.integer((end_x - start_x) > 0),
    is_backward = as.integer((end_x - start_x) < -5),
    is_lateral = as.integer(abs(end_x - start_x) < 5)
  )]

  # Progressive pass (moves ball significantly closer to goal)
  dt[, `:=`(
    dist_before = sqrt((100 - start_x)^2 + (50 - start_y)^2),
    dist_after = sqrt((100 - end_x)^2 + (50 - end_y)^2)
  )]
  dt[, is_progressive := as.integer(dist_after < dist_before * 0.75)]

  # Zone features
  dt[, `:=`(
    start_zone = (pmin(pmax(floor(start_x / 33.34), 0), 2)) * 3 +
                  pmin(pmax(floor(start_y / 33.34), 0), 2) + 1,
    end_zone = (pmin(pmax(floor(end_x / 33.34), 0), 2)) * 3 +
                pmin(pmax(floor(end_y / 33.34), 0), 2) + 1
  )]

  # Cross-field and destination indicators
  dt[, `:=`(
    crosses_midfield = as.integer((start_x < 50 & end_x >= 50) | (start_x >= 50 & end_x < 50)),
    into_penalty_area = as.integer(!(start_x > 83 & start_y > 21 & start_y < 79) &
                                    (end_x > 83 & end_y > 21 & end_y < 79)),
    into_final_third = as.integer(start_x <= 67 & end_x > 67),
    ends_in_box = as.integer(end_x > 83 & end_y > 21 & end_y < 79),
    start_in_own_third = as.integer(start_x < 33),
    start_in_mid_third = as.integer(start_x >= 33 & start_x < 67),
    start_in_final_third = as.integer(start_x >= 67),
    end_dist_to_goal = sqrt((100 - end_x)^2 + (50 - end_y)^2),
    end_angle_to_goal = abs(atan2(end_y - 44, 100 - end_x) - atan2(end_y - 56, 100 - end_x)),
    is_short_pass = as.integer(pass_distance < 15),
    is_medium_pass = as.integer(pass_distance >= 15 & pass_distance < 30),
    is_long_pass = as.integer(pass_distance >= 30)
  )]

  # Body part (if available)
  if ("bodypart" %in% names(dt)) {
    dt[, is_headed_pass := as.integer(bodypart == "head")]
  } else {
    dt[, is_headed_pass := 0L]
  }

  dt
}


#' Prepare Pass Data for xPass Model
#'
#' Creates features for pass completion probability modeling from SPADL actions.
#' Optimized with data.table for fast processing.
#'
#' @param spadl_actions SPADL actions data frame with pass events
#'
#' @return Data frame with pass features:
#'   \itemize{
#'     \item start_x, start_y: Pass origin coordinates
#'     \item end_x, end_y: Pass destination coordinates
#'     \item pass_distance: Euclidean distance
#'     \item pass_angle: Angle of pass relative to goal direction
#'     \item is_forward: Pass moves toward opponent goal
#'     \item is_progressive: Significant forward movement
#'     \item start_zone, end_zone: Pitch zones
#'     \item crosses_midfield: Pass crosses halfway line
#'     \item into_penalty_area: Pass ends in penalty area
#'     \item into_final_third: Pass ends in final third
#'     \item pass_type_*: Pass type indicators
#'     \item completed: Target variable (1 = successful pass)
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' spadl <- convert_opta_to_spadl(events)
#' pass_features <- prepare_passes_for_xpass(spadl)
#' }
prepare_passes_for_xpass <- function(spadl_actions) {
  dt <- data.table::as.data.table(spadl_actions)

  # Filter to passes only
  passes <- dt[action_type == "pass"]

  if (nrow(passes) == 0) {
    cli::cli_abort("No passes found in SPADL actions")
  }

  cli::cli_alert_info("Preparing {format(nrow(passes), big.mark=',')} passes for xPass modeling...")

  # Create pass features using shared helper
  passes <- create_pass_features(passes)

  # Target variable
  passes[, completed := as.integer(result == "success")]

  # Select feature columns
  feature_cols <- c("match_id", "action_id", "player_id", "player_name", "team_id",
                     "start_x", "start_y", "end_x", "end_y",
                     "pass_distance", "pass_angle",
                     "is_forward", "is_backward", "is_lateral", "is_progressive",
                     "start_zone", "end_zone",
                     "crosses_midfield", "into_penalty_area", "into_final_third", "ends_in_box",
                     "start_in_own_third", "start_in_mid_third", "start_in_final_third",
                     "end_dist_to_goal", "end_angle_to_goal",
                     "is_short_pass", "is_medium_pass", "is_long_pass",
                     "is_headed_pass", "completed")
  features <- passes[, ..feature_cols]

  # Handle missing/infinite values (vectorized)
  numeric_cols <- names(features)[sapply(features, is.numeric)]
  for (col in numeric_cols) {
    data.table::set(features, which(is.na(features[[col]]) | is.infinite(features[[col]])), col, 0)
  }

  completion_rate <- mean(features$completed)
  cli::cli_alert_success("Prepared pass features: {round(completion_rate * 100, 1)}% completion rate")

  as.data.frame(features)
}


#' Fit xPass Model using XGBoost
#'
#' Trains an XGBoost classifier to predict pass completion probability.
#'
#' @param pass_features Data frame from prepare_passes_for_xpass()
#' @param nfolds Number of CV folds (default 5)
#' @param max_depth Maximum tree depth (default 6)
#' @param eta Learning rate (default 0.05)
#' @param subsample Row subsampling (default 0.8)
#' @param colsample_bytree Column subsampling (default 0.8)
#' @param nrounds Maximum boosting rounds (default 500)
#' @param early_stopping_rounds Early stopping patience (default 50)
#' @param verbose Print progress (0=silent, 1=progress)
#'
#' @return List with fitted model and metadata
#'
#' @export
#' @examples
#' \dontrun{
#' passes <- prepare_passes_for_xpass(spadl)
#' xpass_model <- fit_xpass_model(passes)
#' }
fit_xpass_model <- function(pass_features,
                             nfolds = 5,
                             max_depth = 6,
                             eta = 0.1,
                             subsample = 0.8,
                             colsample_bytree = 0.8,
                             nrounds = 500,
                             early_stopping_rounds = 50,
                             verbose = 1) {

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. Install with: install.packages('xgboost')")
  }

  # Feature columns
  feature_cols <- c(
    "start_x", "start_y", "end_x", "end_y",
    "pass_distance", "pass_angle",
    "is_forward", "is_backward", "is_lateral", "is_progressive",
    "start_zone", "end_zone",
    "crosses_midfield", "into_penalty_area", "into_final_third", "ends_in_box",
    "start_in_own_third", "start_in_mid_third", "start_in_final_third",
    "end_dist_to_goal", "end_angle_to_goal",
    "is_short_pass", "is_medium_pass", "is_long_pass",
    "is_headed_pass"
  )

  available_features <- intersect(feature_cols, names(pass_features))

  cli::cli_alert_info("Fitting xPass model with {length(available_features)} features on {nrow(pass_features)} passes...")

  # Prepare data
  X <- as.matrix(pass_features[, available_features, drop = FALSE])
  y <- pass_features$completed

  # Remove incomplete cases
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  # XGBoost parameters
  params <- list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    max_depth = max_depth,
    eta = eta,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    min_child_weight = 5
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

  # Feature importance
  importance <- xgboost::xgb.importance(
    feature_names = available_features,
    model = final_model
  )

  cli::cli_alert_success(paste0(
    "xPass model fit complete. ",
    "LogLoss: ", round(best_logloss, 4), ", ",
    "Mean xPass: ", round(mean(y_pred), 4), ", ",
    "Actual rate: ", round(mean(y), 4)
  ))

  result <- list(
    model = final_model,
    cv_result = cv_result,
    importance = importance,
    best_nrounds = best_nrounds,
    best_logloss = best_logloss,
    panna_metadata = list(
      type = "xpass_model",
      feature_cols = available_features,
      n_passes = length(y),
      completion_rate = mean(y),
      params = params
    )
  )

  class(result) <- c("xpass_model", "list")
  result
}


#' Predict Pass Completion Probability
#'
#' Applies trained xPass model to predict completion probability for passes.
#'
#' @param xpass_model Fitted xPass model from fit_xpass_model()
#' @param pass_features Data frame with pass features
#'
#' @return Vector of xPass predictions (probabilities)
#'
#' @export
predict_xpass <- function(xpass_model, pass_features) {
  feature_cols <- xpass_model$panna_metadata$feature_cols

  # Ensure all required columns exist
  missing_cols <- setdiff(feature_cols, names(pass_features))
  for (col in missing_cols) {
    pass_features[[col]] <- 0
  }

  # Prepare prediction matrix
  X <- as.matrix(pass_features[, feature_cols, drop = FALSE])
  X[is.na(X)] <- 0

  # Predict
  xpass_pred <- stats::predict(xpass_model$model, X)

  xpass_pred
}


#' Add xPass to SPADL Actions
#'
#' Adds pass completion probability to pass actions in SPADL data.
#'
#' @param spadl_actions SPADL actions data frame
#' @param xpass_model Fitted xPass model
#'
#' @return SPADL actions with xpass column added for passes
#' @export
add_xpass_to_spadl <- function(spadl_actions, xpass_model) {
  # Initialize xPass column
  spadl_actions$xpass <- NA_real_

  # Find passes
  pass_idx <- spadl_actions$action_type == "pass"

  if (sum(pass_idx) == 0) {
    cli::cli_warn("No passes found in SPADL actions")
    return(spadl_actions)
  }

  # Prepare pass features
  passes <- spadl_actions[pass_idx, ]
  pass_features <- prepare_passes_for_xpass_minimal(passes)

  # Predict xPass
  xpass_pred <- predict_xpass(xpass_model, pass_features)

  # Add to SPADL
  spadl_actions$xpass[pass_idx] <- xpass_pred

  cli::cli_alert_success("Added xPass to {sum(pass_idx)} passes (mean xPass: {round(mean(xpass_pred), 3)})")

  spadl_actions
}


#' Prepare Minimal Pass Features
#'
#' Creates minimal features for xPass prediction from SPADL data.
#' Used internally when features aren't already prepared.
#' Optimized with data.table for fast processing.
#'
#' @param passes SPADL pass actions
#'
#' @return Data frame with pass features
#' @keywords internal
prepare_passes_for_xpass_minimal <- function(passes) {
  dt <- data.table::as.data.table(passes)

  # Create pass features using shared helper
  dt <- create_pass_features(dt)

  # Select feature columns
  feature_cols <- c("start_x", "start_y", "end_x", "end_y",
                     "pass_distance", "pass_angle",
                     "is_forward", "is_backward", "is_lateral", "is_progressive",
                     "start_zone", "end_zone",
                     "crosses_midfield", "into_penalty_area", "into_final_third", "ends_in_box",
                     "start_in_own_third", "start_in_mid_third", "start_in_final_third",
                     "end_dist_to_goal", "end_angle_to_goal",
                     "is_short_pass", "is_medium_pass", "is_long_pass",
                     "is_headed_pass")
  features <- dt[, ..feature_cols]

  # Handle NAs/Inf (vectorized)
  numeric_cols <- names(features)[sapply(features, is.numeric)]
  for (col in numeric_cols) {
    data.table::set(features, which(is.na(features[[col]]) | is.infinite(features[[col]])), col, 0)
  }

  as.data.frame(features)
}


#' Calculate Pass Credit Split
#'
#' Splits EPV credit between passer and receiver based on pass difficulty.
#' Harder passes (lower xPass) give more credit to the passer.
#'
#' @param pass_value Total EPV value of the pass
#' @param xpass Predicted pass completion probability
#'
#' @return List with passer_credit and receiver_credit
#'
#' @export
#' @examples
#' # Easy pass (80% completion): receiver gets most credit
#' split_pass_credit(0.1, 0.8)
#'
#' # Difficult pass (30% completion): passer gets most credit
#' split_pass_credit(0.1, 0.3)
split_pass_credit <- function(pass_value, xpass) {
  # Clip xpass to avoid extreme splits
  xpass <- pmax(pmin(xpass, 0.95), 0.05)

  # Passer credit: proportional to difficulty (1 - xpass)
  # Receiver credit: proportional to ease (xpass)
  passer_credit <- pass_value * (1 - xpass)
  receiver_credit <- pass_value * xpass

  list(
    passer_credit = passer_credit,
    receiver_credit = receiver_credit
  )
}


#' Load Pre-trained xPass Model
#'
#' Loads xPass model from saved RDS file.
#'
#' @param path Path to model RDS file. If NULL, uses default location.
#'
#' @return Fitted xPass model
#' @export
load_xpass_model <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) {
    model <- readRDS(path)
    cli::cli_alert_success("Loaded xPass model from {path}")
    return(model)
  }

  default_path <- file.path(pannadata_dir(), "models", "opta", "xpass_model.rds")
  if (file.exists(default_path)) {
    model <- readRDS(default_path)
    cli::cli_alert_success("Loaded xPass model from {default_path}")
    return(model)
  }

  cli::cli_abort(c(
    "xPass model not found.",
    "i" = "Train a new model with fit_xpass_model() or download with pb_download_epv_models()"
  ))
}


#' Save xPass Model
#'
#' Saves trained xPass model to RDS file.
#'
#' @param xpass_model Fitted xPass model
#' @param path Path to save. If NULL, saves to pannadata/models/opta/
#'
#' @return Invisibly returns the path
#' @export
save_xpass_model <- function(xpass_model, path = NULL) {
  if (is.null(path)) {
    model_dir <- file.path(pannadata_dir(), "models", "opta")
    dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(model_dir, "xpass_model.rds")
  }

  saveRDS(xpass_model, path)
  cli::cli_alert_success("Saved xPass model to {path}")

  invisible(path)
}
