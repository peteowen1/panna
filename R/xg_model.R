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

  # Validate coordinate range
  if (any(shot_events$x < 0 | shot_events$x > 100, na.rm = TRUE) ||
      any(shot_events$y < 0 | shot_events$y > 100, na.rm = TRUE)) {
    cli::cli_warn("Some x/y coordinates are outside the expected [0, 100] range.")
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
  features$in_six_yard_box <- as.integer(
    features$x > SIX_YARD_X_MIN & features$y > SIX_YARD_Y_MIN & features$y < SIX_YARD_Y_MAX
  )

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
#' @keywords internal
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
    in_six_yard_box = as.integer(
      shots$start_x > SIX_YARD_X_MIN & shots$start_y > SIX_YARD_Y_MIN & shots$start_y < SIX_YARD_Y_MAX
    ),
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


#' Derive Expected Assists (xA) from SPADL Actions
#'
#' For each shot in SPADL data, checks if the preceding action was a completed
#' pass by a teammate. If so, credits the passer with the shot's xG as xA.
#'
#' @param spadl_actions SPADL actions data frame with xG column already added.
#'   Must contain: action_id, action_type, result, team_id, player_id,
#'   player_name, match_id, xg.
#'
#' @return The same SPADL data frame with additional columns:
#'   \itemize{
#'     \item xa: Expected assists value (shot's xG credited to passer)
#'     \item is_key_pass: Whether this pass led to a shot
#'     \item is_assist: Whether this pass led to a goal
#'   }
#'
#' @keywords internal
derive_xa <- function(spadl_actions) {
  dt <- data.table::as.data.table(spadl_actions)

  # Initialize columns
  dt[, `:=`(xa = 0, is_key_pass = 0L, is_assist = 0L)]

  # For each shot, look at the previous action in the same match/period
  dt[, prev_action_type := data.table::shift(action_type, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_result := data.table::shift(result, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_team_id := data.table::shift(team_id, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_player_id := data.table::shift(player_id, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_player_name := data.table::shift(player_name, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_action_id := data.table::shift(action_id, 1, type = "lag"), by = .(match_id, period_id)]

  # A key pass is a completed pass by same team immediately before a shot
  # NAs arise from shift() on first action per match/period — replace with FALSE
  shot_mask <- dt$action_type == "shot"
  key_pass_mask <- shot_mask &
    dt$prev_action_type == "pass" &
    dt$prev_result == "success" &
    dt$prev_team_id == dt$team_id &
    dt$prev_player_id != dt$player_id  # different player
  key_pass_mask[is.na(key_pass_mask)] <- FALSE

  # Credit the passer with the shot's xG as xA
  # We need to mark the PASS row, not the shot row
  if (sum(key_pass_mask) > 0) {
    key_pass_ids <- dt$prev_action_id[key_pass_mask]
    key_pass_xg <- dt$xg[key_pass_mask]
    shot_is_goal <- dt$result[key_pass_mask] == "success"

    # Match by action_id and match_id for safety
    key_pass_match_ids <- dt$match_id[key_pass_mask]

    for (i in seq_along(key_pass_ids)) {
      row_idx <- which(dt$action_id == key_pass_ids[i] & dt$match_id == key_pass_match_ids[i])
      if (length(row_idx) == 1) {
        data.table::set(dt, row_idx, "xa", key_pass_xg[i])
        data.table::set(dt, row_idx, "is_key_pass", 1L)
        if (shot_is_goal[i]) {
          data.table::set(dt, row_idx, "is_assist", 1L)
        }
      }
    }
  }

  # Clean up temp columns
  dt[, c("prev_action_type", "prev_result", "prev_team_id",
         "prev_player_id", "prev_player_name", "prev_action_id") := NULL]

  n_key_passes <- sum(dt$is_key_pass)
  total_xa <- sum(dt$xa)
  cli::cli_alert_success("Derived xA: {n_key_passes} key passes, total xA = {round(total_xa, 1)}")

  as.data.frame(dt)
}


#' Aggregate Player-Level xG/xA/xPass Metrics
#'
#' Aggregates SPADL-level predictions to player-season totals.
#' Produces shooting, assisting, and passing metrics per player.
#'
#' @param spadl SPADL actions data frame with xg, xa, xpass columns.
#' @param lineups Lineup data from load_opta_lineups() for minutes played.
#' @param min_minutes Minimum minutes for inclusion (default 0, no filter).
#'
#' @return Data frame with one row per player containing:
#'   \itemize{
#'     \item \strong{Identity}: player_id, player_name, team_name, minutes
#'     \item \strong{Shooting}: shots, goals, xg, npxg, goals_minus_xg, xg_per90
#'     \item \strong{Assisting}: key_passes, assists, xa, xa_per90
#'     \item \strong{Passing}: passes_attempted, passes_completed, sum_xpass,
#'       xpass_overperformance, xpass_avg
#'   }
#'
#' @export
aggregate_player_xmetrics <- function(spadl, lineups, min_minutes = 0) {
  dt <- data.table::as.data.table(spadl)

  # --- Shooting stats ---
  shots_dt <- dt[action_type == "shot"]
  if (nrow(shots_dt) > 0) {
    # Handle is_penalty column potentially missing
    if (!"is_penalty" %in% names(shots_dt)) {
      shots_dt[, is_penalty := 0L]
    }
    shooting <- shots_dt[, .(
      shots = .N,
      shots_on_target = sum(opta_type_id %in% c(13L, 16L), na.rm = TRUE),
      goals = sum(result == "success", na.rm = TRUE),
      penalty_goals = sum(result == "success" & is_penalty == 1L, na.rm = TRUE),
      xg = sum(xg, na.rm = TRUE),
      npxg = sum(data.table::fifelse(is.na(is_penalty) | is_penalty == 0L, xg, 0), na.rm = TRUE)
    ), by = .(player_id, player_name, team_id)]
    shooting[, npgoals := goals - penalty_goals]
  } else {
    shooting <- data.table::data.table(
      player_id = character(), player_name = character(), team_id = character(),
      shots = integer(), shots_on_target = integer(), goals = integer(),
      penalty_goals = integer(), xg = numeric(), npxg = numeric(), npgoals = integer()
    )
  }

  # --- Assisting stats ---
  passes_dt <- dt[action_type == "pass"]
  if (nrow(passes_dt) > 0) {
    assisting <- passes_dt[, .(
      key_passes = sum(is_key_pass, na.rm = TRUE),
      assists = sum(is_assist, na.rm = TRUE),
      xa = sum(xa, na.rm = TRUE)
    ), by = .(player_id, player_name, team_id)]
  } else {
    assisting <- data.table::data.table(
      player_id = character(), player_name = character(), team_id = character(),
      key_passes = integer(), assists = integer(), xa = numeric()
    )
  }

  # --- Passing stats ---
  if (nrow(passes_dt) > 0 && "xpass" %in% names(passes_dt)) {
    passing <- passes_dt[, .(
      passes_attempted = .N,
      passes_completed = sum(result == "success", na.rm = TRUE),
      sum_xpass = sum(xpass, na.rm = TRUE)
    ), by = .(player_id, player_name, team_id)]
    passing[, xpass_overperformance := passes_completed - sum_xpass]
    passing[, xpass_avg := sum_xpass / passes_attempted]
  } else {
    passing <- data.table::data.table(
      player_id = character(), player_name = character(), team_id = character(),
      passes_attempted = integer(), passes_completed = integer(),
      sum_xpass = numeric(), xpass_overperformance = numeric(), xpass_avg = numeric()
    )
  }

  # --- Minutes from lineups ---
  lineup_dt <- data.table::as.data.table(lineups)
  minutes_df <- lineup_dt[, .(
    minutes = sum(minutes_played, na.rm = TRUE),
    team_name = team_name[1]
  ), by = .(player_id, player_name, team_id)]

  # --- Merge all ---
  result <- shooting[minutes_df, on = c("player_id", "player_name", "team_id")]
  result <- assisting[result, on = c("player_id", "player_name", "team_id")]
  result <- passing[result, on = c("player_id", "player_name", "team_id")]

  # Fill NAs with 0
  num_cols <- c("shots", "shots_on_target", "goals", "penalty_goals", "npgoals",
                "xg", "npxg", "key_passes", "assists", "xa",
                "passes_attempted", "passes_completed", "sum_xpass",
                "xpass_overperformance", "xpass_avg")
  for (col in num_cols) {
    if (col %in% names(result)) {
      data.table::set(result, which(is.na(result[[col]])), col, 0)
    }
  }

  # --- Derived stats ---
  result[, `:=`(
    goals_minus_xg = goals - xg,
    xg_per90 = round(xg / minutes * 90, 2),
    xa_per90 = round(xa / minutes * 90, 2),
    xpass_overperformance_per90 = round(xpass_overperformance / minutes * 90, 2)
  )]

  # Handle Inf/NaN from 0-minute players
  inf_cols <- c("xg_per90", "xa_per90", "xpass_overperformance_per90")
  for (col in inf_cols) {
    data.table::set(result, which(is.infinite(result[[col]]) | is.nan(result[[col]])), col, 0)
  }

  # Filter by min_minutes

  if (min_minutes > 0) {
    result <- result[minutes >= min_minutes]
  }

  # Sort by minutes
  data.table::setorder(result, -minutes)

  cli::cli_alert_success("Aggregated xmetrics for {nrow(result)} players")

  as.data.frame(result)
}


#' Extract Shots from xG-Scored SPADL for Splint Pipeline
#'
#' Extracts shot rows from SPADL data that has been scored with xG predictions,
#' and formats them for the splint creation pipeline. This bridges the xMetrics
#' infrastructure (SPADL + xG model) with the RAPM/splint pipeline.
#'
#' @param spadl SPADL actions data frame with \code{xg} and \code{is_penalty} columns
#'   (from \code{add_xg_to_spadl()} and penalty detection).
#' @param lineups Lineup data from \code{load_opta_lineups()} for team_name mapping.
#'
#' @return Data frame with one row per shot containing:
#'   \itemize{
#'     \item match_id, minute, team, player_id, player_name
#'     \item xg: Model-predicted xG (penalties overridden to 0.76)
#'     \item is_goal: Whether the shot resulted in a goal
#'     \item is_penalty: Whether the shot was a penalty
#'   }
#'
#' @keywords internal
extract_shots_from_spadl <- function(spadl, lineups) {
  if (is.null(spadl) || nrow(spadl) == 0) {
    cli::cli_warn("No SPADL actions provided")
    return(data.frame(
      match_id = character(0), minute = numeric(0), team = character(0),
      player_id = character(0), player_name = character(0),
      xg = numeric(0), is_goal = logical(0), is_penalty = logical(0)
    ))
  }

  # Filter to shots only

  shots <- spadl[spadl$action_type == "shot", ]

  if (nrow(shots) == 0) {
    cli::cli_warn("No shots found in SPADL data")
    return(data.frame(
      match_id = character(0), minute = numeric(0), team = character(0),
      player_id = character(0), player_name = character(0),
      xg = numeric(0), is_goal = logical(0), is_penalty = logical(0)
    ))
  }

  # Build team_id -> team_name lookup from lineups
  if (!is.null(lineups) && "team_name" %in% names(lineups) && "team_id" %in% names(lineups)) {
    team_lookup <- unique(lineups[, c("team_id", "team_name")])
    team_lookup <- stats::setNames(team_lookup$team_name, as.character(team_lookup$team_id))
  } else {
    team_lookup <- NULL
  }

  # Map team_id to team_name
  team_names <- if (!is.null(team_lookup)) {
    unname(team_lookup[as.character(shots$team_id)])
  } else {
    as.character(shots$team_id)
  }

  # Handle is_penalty column
  is_pen <- if ("is_penalty" %in% names(shots)) {
    as.logical(shots$is_penalty)
  } else {
    rep(FALSE, nrow(shots))
  }

  result <- data.frame(
    match_id = shots$match_id,
    minute = floor(shots$time_seconds / 60),
    team = team_names,
    player_id = shots$player_id,
    player_name = shots$player_name,
    xg = shots$xg,
    is_goal = shots$result == "success",
    is_penalty = is_pen,
    stringsAsFactors = FALSE
  )

  cli::cli_alert_success(
    "Extracted {nrow(result)} shots from SPADL (mean xG: {round(mean(result$xg, na.rm = TRUE), 3)})"
  )

  result
}
