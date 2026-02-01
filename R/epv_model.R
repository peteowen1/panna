# EPV (Expected Possession Value) Model - Multinomial Approach
#
# Uses a single multinomial XGBoost model to predict:
#   - P(possession team scores next goal this half)
#   - P(opponent scores next goal this half)
#   - P(nobody scores this half)
#
# EPV = P(team_scores) - P(opponent_scores), bounded [-1, +1]
#
# Credit assignment handles possession changes with perspective flipping.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT setorder shift .SD .N .I := fifelse as.data.table
NULL


# =============================================================================
# LABEL CREATION
# =============================================================================

#' Create Next Goal Labels for EPV Model
#'
#' Determines who scores the next goal in each half for each action.
#' Labels: 0 = possession team scores next, 1 = opponent scores next, 2 = nobody scores
#'
#' @param spadl_actions SPADL actions with team_id, period_id, match_id
#'
#' @return Data frame with next_goal_label column added
#'
#' @export
create_next_goal_labels <- function(spadl_actions) {
  cli::cli_alert_info("Creating next goal labels...")

  dt <- data.table::as.data.table(spadl_actions)
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Find goals (shots with success)
  goals_dt <- dt[action_type == "shot" & result == "success", .(
    match_id,
    period_id,
    goal_time = time_seconds,
    goal_team = team_id
  )]

  # Sort goals by time within each match-period
  data.table::setorder(goals_dt, match_id, period_id, goal_time)

  # Use non-equi rolling join to find next goal for each action

  # This is much faster than nested for loops
  dt[, action_time := time_seconds]

  # Join: for each action, find the first goal where goal_time > action_time
  # in the same match-period
  result <- goals_dt[dt,
    on = .(match_id, period_id, goal_time > action_time),
    mult = "first",
    .(match_id = i.match_id,
      period_id = i.period_id,
      action_id = i.action_id,
      team_id = i.team_id,
      next_goal_team = x.goal_team)
  ]

  # Merge back to main data
  dt <- merge(dt, result[, .(match_id, action_id, next_goal_team)],
              by = c("match_id", "action_id"), all.x = TRUE)

  # Create label based on who scores next
  # 0 = possession team scores, 1 = opponent scores, 2 = nobody scores
  dt[, next_goal_label := fifelse(
    is.na(next_goal_team),
    2L,  # Nobody scores (no more goals this half)
    fifelse(next_goal_team == team_id, 0L, 1L)
  )]

  # Cleanup
  dt[, c("action_time", "next_goal_team") := NULL]

  # Summary stats
  label_counts <- table(dt$next_goal_label)
  cli::cli_alert_success(paste0(
    "Labels created: team_scores=", label_counts["0"],
    ", opponent_scores=", label_counts["1"],
    ", nobody_scores=", label_counts["2"]
  ))

  as.data.frame(dt)
}


#' Create Next xG Labels for EPV Model
#'
#' Determines the xG of the next shot in each half for each action.
#' Positive xG = team's shot, Negative xG = opponent's shot, 0 = no more shots.
#' This provides more signal than binary goal outcomes.
#'
#' @param spadl_actions SPADL actions with team_id, period_id, match_id
#' @param xg_values Optional named vector or data frame with xG values per action_id.
#'   If NULL, uses chain_xg from spadl_actions if available, otherwise estimates.
#'
#' @return Data frame with next_xg_label column added
#'
#' @export
create_next_xg_labels <- function(spadl_actions, xg_values = NULL) {
  cli::cli_alert_info("Creating next xG labels...")

  dt <- data.table::as.data.table(spadl_actions)
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Get xG values for shots
  if (!is.null(xg_values)) {
    if (is.data.frame(xg_values)) {
      xg_df <- data.table::as.data.table(xg_values)
      if (all(c("match_id", "action_id", "xg") %in% names(xg_df))) {
        dt <- merge(dt, xg_df[, .(match_id, action_id, shot_xg = xg)],
                     by = c("match_id", "action_id"), all.x = TRUE)
      }
    }
  } else if ("chain_xg" %in% names(dt)) {
    # Use chain_xg for shots
    dt[action_type == "shot", shot_xg := chain_xg]
  }

  # If no xG available, estimate from position (simple model)
  if (!"shot_xg" %in% names(dt)) {
    cli::cli_alert_info("No xG values found, estimating from position...")
    dt[action_type == "shot", shot_xg := estimate_simple_xg(start_x, start_y)]
  }

  # Fill NA xG for non-shots
  dt[is.na(shot_xg), shot_xg := 0]

  # Find all shots with their xG
  shots_dt <- dt[action_type == "shot", .(
    match_id,
    period_id,
    shot_time = time_seconds,
    shot_team = team_id,
    shot_xg = shot_xg
  )]

  data.table::setorder(shots_dt, match_id, period_id, shot_time)

  # Use non-equi rolling join to find next shot for each action
  # This is much faster than nested for loops
  dt[, action_time := time_seconds]

  # Join: for each action, find the first shot where shot_time > action_time
  result <- shots_dt[dt,
    on = .(match_id, period_id, shot_time > action_time),
    mult = "first",
    .(match_id = i.match_id,
      period_id = i.period_id,
      action_id = i.action_id,
      team_id = i.team_id,
      next_shot_team = x.shot_team,
      next_shot_xg = x.shot_xg)
  ]

  # Merge back to main data
  dt <- merge(dt, result[, .(match_id, action_id, next_shot_team, next_shot_xg)],
              by = c("match_id", "action_id"), all.x = TRUE)

  # Create label: positive xG for team's shot, negative for opponent's, 0 for none
  dt[, next_xg_label := fifelse(
    is.na(next_shot_xg),
    0,  # No more shots this half
    fifelse(next_shot_team == team_id, next_shot_xg, -next_shot_xg)
  )]

  # Cleanup
  dt[, c("action_time", "next_shot_team", "next_shot_xg") := NULL]

  # Cleanup
  if ("shot_xg" %in% names(dt)) {
    dt[, shot_xg := NULL]
  }

  # Summary stats
  mean_xg <- mean(dt$next_xg_label, na.rm = TRUE)
  n_positive <- sum(dt$next_xg_label > 0, na.rm = TRUE)
  n_negative <- sum(dt$next_xg_label < 0, na.rm = TRUE)
  n_zero <- sum(dt$next_xg_label == 0, na.rm = TRUE)

  cli::cli_alert_success(paste0(
    "xG labels created: mean=", round(mean_xg, 4),
    ", team_shots=", n_positive,
    ", opponent_shots=", n_negative,
    ", no_shots=", n_zero
  ))

  as.data.frame(dt)
}


#' Estimate Simple xG from Position
#'
#' Simple xG estimation based on distance and angle to goal.
#' Used as fallback when no xG model is available.
#'
#' @param x X coordinate (0-100 scale, attacking right)
#' @param y Y coordinate (0-100 scale)
#'
#' @return Estimated xG values
#' @keywords internal
estimate_simple_xg <- function(x, y) {
  # Distance to center of goal
  dist_to_goal <- sqrt((100 - x)^2 + (50 - y)^2)

  # Angle to goal (using goal posts at y = 44 and y = 56)
  angle <- abs(atan2(y - 44, 100 - x) - atan2(y - 56, 100 - x))

  # Simple logistic model (approximate)
  # Higher xG for closer shots with wider angles
  logit <- 2 - 0.1 * dist_to_goal + 5 * angle
  xg <- 1 / (1 + exp(-logit))

  # Cap between 0.01 and 0.95
  pmin(pmax(xg, 0.01), 0.95)
}


# =============================================================================
# MODEL TRAINING
# =============================================================================

#' Fit EPV Model
#'
#' Trains an XGBoost model to predict expected possession value.
#' Supports two methods:
#' - "goal": Multinomial classification (who scores next: team/opponent/nobody)
#' - "xg": Regression on signed xG of next shot (+team, -opponent, 0 if none)
#'
#' @param features Data frame from create_epv_features()
#' @param labels Data frame with labels (next_goal_label for "goal", next_xg_label for "xg")
#' @param method Either "goal" (multinomial) or "xg" (regression). Default "goal".
#' @param nfolds Number of CV folds (default 5)
#' @param max_depth Maximum tree depth (default 6)
#' @param eta Learning rate (default 0.05)
#' @param subsample Row subsampling (default 0.8)
#' @param colsample_bytree Column subsampling (default 0.8)
#' @param nrounds Maximum boosting rounds (default 1000)
#' @param early_stopping_rounds Early stopping patience (default 50)
#' @param verbose Print progress (default 1)
#' @param ... Arguments passed to deprecated wrappers (for internal use)
#'
#' @return Fitted EPV model with metadata
#'
#' @export
fit_epv_model <- function(features,
                           labels,
                           method = c("goal", "xg"),
                           nfolds = 5,
                           max_depth = 6,
                           eta = 0.05,
                           subsample = 0.8,
                           colsample_bytree = 0.8,
                           nrounds = 1000,
                           early_stopping_rounds = 50,
                           verbose = 1) {

  method <- match.arg(method)

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. Install with: install.packages('xgboost')")
  }

  # Get feature columns
  feature_cols <- get_epv_feature_cols(include_sequence = TRUE, n_prev = 3)
  available_cols <- intersect(feature_cols, names(features))

  if (length(available_cols) < 5) {
    cli::cli_abort("Insufficient features available for EPV model")
  }

  # Determine label column based on method
  label_col <- if (method == "goal") "next_goal_label" else "next_xg_label"

  cli::cli_alert_info("Fitting EPV {method} model with {length(available_cols)} features...")

  # Get labels - either from labels parameter or already in features
  if (label_col %in% names(features)) {
    data <- features
  } else if (label_col %in% names(labels)) {
    data <- merge(features, labels[, c("match_id", "action_id", label_col)],
                   by = c("match_id", "action_id"), all.x = TRUE)
  } else {
    cli::cli_abort("{label_col} column not found in features or labels")
  }

  # Prepare matrices
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data[[label_col]]

  # Remove rows where target is NA (XGBoost handles missing features natively)
  valid_y <- !is.na(y)
  X <- X[valid_y, , drop = FALSE]
  y <- y[valid_y]

  # Method-specific setup
  if (method == "goal") {
    # Multinomial classification
    class_counts <- table(y)
    cli::cli_alert_info(paste0(
      "Training on ", length(y), " actions: ",
      "team_scores=", class_counts["0"], " (", round(class_counts["0"]/length(y)*100, 1), "%), ",
      "opponent_scores=", class_counts["1"], " (", round(class_counts["1"]/length(y)*100, 1), "%), ",
      "nobody_scores=", class_counts["2"], " (", round(class_counts["2"]/length(y)*100, 1), "%)"
    ))

    params <- list(
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      num_class = 3,
      max_depth = max_depth,
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      min_child_weight = 10
    )
    metric_name <- "test_mlogloss_mean"

  } else {
    # xG regression
    n_positive <- sum(y > 0)
    n_negative <- sum(y < 0)
    n_zero <- sum(y == 0)
    cli::cli_alert_info(paste0(
      "Training on ", length(y), " actions: ",
      "team_shots=", n_positive, " (", round(n_positive/length(y)*100, 1), "%), ",
      "opponent_shots=", n_negative, " (", round(n_negative/length(y)*100, 1), "%), ",
      "no_shots=", n_zero, " (", round(n_zero/length(y)*100, 1), "%)"
    ))

    params <- list(
      objective = "reg:squarederror",
      eval_metric = "rmse",
      max_depth = max_depth,
      eta = eta,
      subsample = subsample,
      colsample_bytree = colsample_bytree,
      min_child_weight = 10
    )
    metric_name <- "test_rmse_mean"
  }

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

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

  # Best iteration
  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || length(best_nrounds) == 0) {
    eval_log <- cv_result$evaluation_log
    best_nrounds <- which.min(eval_log[[metric_name]])
  }
  best_metric <- cv_result$evaluation_log[[metric_name]][best_nrounds]

  metric_label <- if (method == "goal") "mlogloss" else "rmse"
  cli::cli_alert_info("Best iteration: {best_nrounds}, CV {metric_label}: {round(best_metric, 4)}")

  # Fit final model
  final_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Feature importance
  importance <- xgboost::xgb.importance(
    feature_names = available_cols,
    model = final_model
  )

  # Build metadata based on method
  if (method == "goal") {
    model_type <- "epv_multinomial"
    distribution <- as.list(prop.table(class_counts))
  } else {
    model_type <- "epv_xg_regression"
    distribution <- list(
      mean_xg = mean(y),
      sd_xg = stats::sd(y),
      pct_positive = mean(y > 0),
      pct_negative = mean(y < 0),
      pct_zero = mean(y == 0)
    )
  }

  result <- list(
    model = final_model,
    cv_result = cv_result,
    importance = importance,
    best_nrounds = best_nrounds,
    best_metric = best_metric,
    method = method,
    panna_metadata = list(
      type = model_type,
      method = method,
      feature_cols = available_cols,
      n_actions = length(y),
      distribution = distribution,
      params = params
    )
  )

  class(result) <- c("epv_model", "list")

  cli::cli_alert_success("EPV {method} model complete")

  result
}


# =============================================================================
# EPV PREDICTION AND CALCULATION
# =============================================================================

#' Predict EPV Values
#'
#' Gets predictions from EPV model. For "goal" method returns multinomial
#' probabilities, for "xg" method returns expected next xG value.
#'
#' @param model EPV model from fit_epv_model()
#' @param features EPV features
#'
#' @return For "goal": data frame with p_team_scores, p_opponent_scores, p_nobody_scores
#'         For "xg": data frame with expected_xg
#' @keywords internal
predict_epv_probs <- function(model, features) {
  feature_cols <- model$panna_metadata$feature_cols
  method <- model$method %||% "goal"  # Default to goal for backwards compatibility

  # Ensure columns exist
  missing_cols <- setdiff(feature_cols, names(features))
  for (col in missing_cols) {
    features[[col]] <- 0
  }

  X <- as.matrix(features[, feature_cols, drop = FALSE])
  X[is.na(X)] <- 0

  if (method == "goal") {
    # Multinomial: returns matrix of probabilities (n_samples x 3)
    probs <- stats::predict(model$model, X, reshape = TRUE)

    data.frame(
      p_team_scores = probs[, 1],
      p_opponent_scores = probs[, 2],
      p_nobody_scores = probs[, 3]
    )
  } else {
    # xG regression: returns single value (expected next xG with sign)
    preds <- stats::predict(model$model, X)

    data.frame(
      expected_xg = preds
    )
  }
}


#' Calculate Action EPV Values
#'
#' Computes EPV for each action. Supports two methods:
#' - "goal": EPV = P(team_scores) - P(opponent_scores), bounded -1 to +1
#' - "xg": EPV = expected xG of next shot (already signed)
#'
#' Handles possession changes by flipping perspective:
#' - Same team: delta = EPV_after - EPV_before
#' - Team change: delta = (-EPV_after) - EPV_before
#'
#' @param spadl_actions SPADL actions data frame (with team_id, possession_change)
#' @param features EPV features from create_epv_features()
#' @param epv_model Fitted EPV model from fit_epv_model()
#'
#' @return SPADL actions with EPV columns added:
#'   \itemize{
#'     \item epv: EPV at this state
#'     \item epv_delta: Change in EPV (with perspective handling)
#'     \item For "goal" method: p_team_scores, p_opponent_scores, p_nobody_scores
#'     \item For "xg" method: expected_xg
#'   }
#'
#' @export
calculate_action_epv <- function(spadl_actions, features, epv_model) {
  cli::cli_alert_info("Calculating EPV for {nrow(spadl_actions)} actions...")

  method <- epv_model$method %||% "goal"

  # Get predictions
  preds <- predict_epv_probs(epv_model, features)

  # Convert to data.table
  dt <- data.table::as.data.table(spadl_actions)

  if (method == "goal") {
    # Multinomial: add probability columns
    dt[, `:=`(
      p_team_scores = preds$p_team_scores,
      p_opponent_scores = preds$p_opponent_scores,
      p_nobody_scores = preds$p_nobody_scores
    )]

    # EPV = P(team_scores) - P(opponent_scores), bounded [-1, +1]
    dt[, epv := p_team_scores - p_opponent_scores]
  } else {
    # xG regression: expected_xg is already signed
    dt[, expected_xg := preds$expected_xg]

    # EPV is directly the prediction
    dt[, epv := expected_xg]
  }

  # Sort by match and action_id
  data.table::setorder(dt, match_id, action_id)

  # Calculate lagged values
  dt[, `:=`(
    prev_epv = shift(epv, 1, type = "lag"),
    prev_team_id = shift(team_id, 1, type = "lag")
  ), by = match_id]

  # Calculate delta with perspective handling
  # If same team: delta = EPV_after - EPV_before
  # If team change: delta = (-EPV_after) - EPV_before
  #   Because from previous team's perspective, new team's positive EPV is negative
  dt[, epv_delta := fifelse(
    is.na(prev_team_id),
    0,  # First action in match
    fifelse(
      team_id == prev_team_id,
      epv - prev_epv,  # Same team: simple difference
      (-epv) - prev_epv  # Team change: flip new team's EPV
    )
  )]

  # Override delta for goals - terminal value is 1.0 (certainty of scoring)
  # For a goal, delta = 1 - prev_epv_from_scoring_team_perspective
  #
  # prev_epv is from the PREVIOUS action's team's perspective, so we need to
  # flip it when there's a team change (possession_change)
  #
  # Same team:   delta = 1 - prev_epv (prev_epv already from our view)
  # Team change: delta = 1 - (-prev_epv) = 1 + prev_epv (flip to our view)
  dt[action_type == "shot" & result == "success",
     epv_delta := fifelse(
       is.na(prev_team_id) | team_id == prev_team_id,
       1 - prev_epv,      # Same team: prev_epv is from our perspective
       1 + prev_epv       # Team change: flip prev_epv to our perspective
     )]

  # Handle own goals (is_own_goal from Opta qualifier 28)
  # Own goals have negative terminal value: opponent achieved max value
  # delta = -1 - prev_epv_from_own_goal_scorer_team_perspective
  if ("is_own_goal" %in% names(dt)) {
    dt[action_type == "shot" & result == "success" & is_own_goal == TRUE,
       epv_delta := fifelse(
         is.na(prev_team_id) | team_id == prev_team_id,
         -1 - prev_epv,     # Same team
         -1 + prev_epv      # Team change
       )]
  }

  # Handle xG-based shot valuation if xg column exists
  # At shots, use xG as the expected terminal value (before outcome is known)
  # This applies to non-goal shots where we use expected value instead of outcome
  # Same perspective handling as goals
  if ("xg" %in% names(dt)) {
    dt[action_type == "shot" & result == "fail" & !is.na(xg),
       epv_delta := fifelse(
         is.na(prev_team_id) | team_id == prev_team_id,
         xg - prev_epv,      # Same team
         xg + prev_epv       # Team change: flip prev_epv
       )]
  }

  # Cleanup temporary columns
  dt[, c("prev_epv", "prev_team_id") := NULL]

  # Summary stats
  cli::cli_alert_success(paste0(
    "EPV calculated (", method, " method). Mean EPV: ", round(mean(dt$epv, na.rm = TRUE), 4),
    ", Mean delta: ", round(mean(dt$epv_delta, na.rm = TRUE), 6),
    ", SD delta: ", round(stats::sd(dt$epv_delta, na.rm = TRUE), 4)
  ))

  as.data.frame(dt)
}


# =============================================================================
# CREDIT ASSIGNMENT
# =============================================================================

#' Assign EPV Credit with Turnover Handling
#'
#' Assigns EPV credit/blame to players. For turnovers (possession changes),
#' splits the value between the player who lost the ball and who gained it.
#'
#' @param spadl_with_epv SPADL actions with EPV values from calculate_action_epv()
#' @param xpass_model Optional xPass model for pass difficulty weighting
#'
#' @return SPADL actions with credit columns added:
#'   \itemize{
#'     \item player_credit: EPV credit to acting player
#'     \item receiver_credit: EPV credit to receiver (for turnovers, this is positive)
#'     \item xpass: Pass completion probability (for passes)
#'   }
#'
#' @export
assign_epv_credit <- function(spadl_with_epv, xpass_model = NULL) {
  cli::cli_alert_info("Assigning EPV credit...")

  dt <- data.table::as.data.table(spadl_with_epv)

  # Initialize credit columns
  dt[, `:=`(
    player_credit = epv_delta,  # Default: acting player gets full delta
    receiver_credit = 0,
    xpass = NA_real_
  )]

  # Check if we have possession_change column
  has_possession_info <- "possession_change" %in% names(dt)

  if (!has_possession_info) {
    cli::cli_warn("No possession_change column - cannot split turnover credit")
    return(as.data.frame(dt))
  }

  # For turnovers (possession_change == TRUE), split credit between
  # the player who lost the ball and the receiver who gained it
  # The delta is negative for the acting player's team
  turnover_idx <- which(dt$possession_change == TRUE & !is.na(dt$epv_delta))

  if (length(turnover_idx) > 0) {
    # For turnovers, split the negative value:
    # - Acting player (who lost ball) gets some blame
    # - Receiver (who gained ball) gets credit (positive)
    # The total delta is: player_credit + receiver_credit = epv_delta
    # But receiver_credit should be from receiver's perspective (positive for them)

    # Default 50/50 split for turnovers
    dt[turnover_idx, `:=`(
      player_credit = epv_delta * 0.5,  # Blame for losing possession
      receiver_credit = -epv_delta * 0.5  # Credit for gaining (negate because delta is negative)
    )]
  }

  # For passes, use xPass model to weight credit split if available
  if (!is.null(xpass_model)) {
    pass_idx <- which(dt$action_type == "pass")

    if (length(pass_idx) > 0) {
      passes <- dt[pass_idx, ]
      pass_features <- prepare_passes_for_xpass_minimal(as.data.frame(passes))
      xpass_pred <- predict_xpass(xpass_model, pass_features)

      dt[pass_idx, xpass := xpass_pred]

      # For successful passes (no possession change):
      # Use base split with xPass adjustment for pass difficulty
      # Base: 60% passer, 40% receiver
      # Adjustment: harder passes (low xpass) shift more credit to passer
      #
      # passer_share = base_passer + adjustment * (1 - xpass)
      # With base_passer=0.5 and adjustment=0.3:
      #   Easy pass (xpass=0.9): passer gets 53%, receiver 47%
      #   Hard pass (xpass=0.5): passer gets 65%, receiver 35%
      #   Very hard (xpass=0.2): passer gets 74%, receiver 26%
      success_pass_idx <- which(dt$action_type == "pass" &
                                  dt$result == "success" &
                                  dt$possession_change == FALSE)

      if (length(success_pass_idx) > 0) {
        base_passer_share <- 0.5
        difficulty_adjustment <- 0.3
        dt[success_pass_idx, `:=`(
          passer_share = base_passer_share + difficulty_adjustment * (1 - xpass)
        )]
        dt[success_pass_idx, `:=`(
          player_credit = epv_delta * passer_share,
          receiver_credit = epv_delta * (1 - passer_share)
        )]
        dt[, passer_share := NULL]  # Clean up temp column
      }

      # For failed passes (turnovers):
      # Use xPass to determine blame split
      # Higher xPass (should have completed) = more blame on passer
      # Lower xPass (risky pass) = less blame, more credit to interceptor
      #
      # passer_blame_share: how much of the negative delta the passer takes
      # With base=0.5 and adjustment=0.3:
      #   Easy pass missed (xpass=0.9): passer takes 77% blame
      #   Hard pass missed (xpass=0.5): passer takes 65% blame
      #   Very hard missed (xpass=0.2): passer takes 56% blame
      failed_pass_idx <- which(dt$action_type == "pass" &
                                 (dt$result == "fail" | dt$possession_change == TRUE))

      if (length(failed_pass_idx) > 0) {
        base_blame_share <- 0.5
        xpass_adjustment <- 0.3
        dt[failed_pass_idx, `:=`(
          passer_blame = base_blame_share + xpass_adjustment * xpass
        )]
        dt[failed_pass_idx, `:=`(
          player_credit = epv_delta * passer_blame,  # Passer takes blame (delta is negative)
          receiver_credit = -epv_delta * (1 - passer_blame)  # Interceptor gets credit
        )]
        dt[, passer_blame := NULL]  # Clean up temp column
      }
    }
  }

  n_turnovers <- sum(dt$possession_change == TRUE, na.rm = TRUE)
  cli::cli_alert_success("Assigned credit for {nrow(dt)} actions ({n_turnovers} turnovers)")

  as.data.frame(dt)
}


#' Split Pass Credit (Legacy Helper)
#'
#' Splits pass EPV between passer and receiver based on difficulty.
#'
#' @param pass_value Vector of pass EPV values
#' @param xpass Vector of pass completion probabilities
#'
#' @return List with passer_credit and receiver_credit vectors
#' @keywords internal
split_pass_credit <- function(pass_value, xpass) {
  # Passer gets credit proportional to difficulty (1 - xpass)
  # Receiver gets credit proportional to pass probability (xpass)
  list(
    passer_credit = pass_value * (1 - xpass),
    receiver_credit = pass_value * xpass
  )
}


# =============================================================================
# PLAYER AGGREGATION
# =============================================================================

#' Aggregate Player EPV Metrics
#'
#' Summarizes EPV by player, calculating total and per-90 metrics.
#' Properly attributes receiver credit to the actual receivers.
#'
#' @param spadl_with_epv SPADL actions with EPV and credit columns
#' @param lineups Optional lineup data for minutes played
#' @param min_minutes Minimum minutes for inclusion (default 450)
#'
#' @return Data frame with player EPV statistics:
#'   \itemize{
#'     \item player_id, player_name, team_id
#'     \item n_actions, total_minutes (if available)
#'     \item epv_total: Total EPV contribution
#'     \item epv_p90: EPV per 90 minutes
#'     \item epv_as_actor: EPV from own actions (player_credit)
#'     \item epv_as_receiver: EPV from receiving (successful passes + turnovers won)
#'     \item epv_passing, epv_shooting, epv_dribbling, epv_defending
#'   }
#'
#' @export
aggregate_player_epv <- function(spadl_with_epv, lineups = NULL, min_minutes = 450) {
  cli::cli_alert_info("Aggregating player EPV metrics...")

  dt <- data.table::as.data.table(spadl_with_epv)

  # Actor credit: sum of player_credit for each player's own actions
  actor_credit <- dt[, .(
    epv_as_actor = sum(player_credit, na.rm = TRUE),
    n_actions = .N
  ), by = .(player_id, player_name, team_id)]

  # Receiver credit: sum of receiver_credit attributed to receiver_player_id
  if ("receiver_player_id" %in% names(dt) && "receiver_credit" %in% names(dt)) {
    receiver_dt <- dt[!is.na(receiver_player_id) & !is.na(receiver_credit)]

    if (nrow(receiver_dt) > 0) {
      receiver_credit <- receiver_dt[, .(
        epv_as_receiver = sum(receiver_credit, na.rm = TRUE)
      ), by = .(receiver_player_id, receiver_player_name)]

      data.table::setnames(receiver_credit,
                            c("receiver_player_id", "receiver_player_name"),
                            c("player_id", "player_name"))
    } else {
      receiver_credit <- data.table::data.table(
        player_id = character(0),
        player_name = character(0),
        epv_as_receiver = numeric(0)
      )
    }
  } else {
    receiver_credit <- data.table::data.table(
      player_id = character(0),
      player_name = character(0),
      epv_as_receiver = numeric(0)
    )
  }

  # Merge actor and receiver credit
  player_epv <- merge(actor_credit, receiver_credit,
                       by = c("player_id", "player_name"), all = TRUE)
  player_epv[is.na(epv_as_actor), epv_as_actor := 0]
  player_epv[is.na(epv_as_receiver), epv_as_receiver := 0]
  player_epv[is.na(n_actions), n_actions := 0L]

  # Total EPV = actor + receiver credit
  player_epv[, epv_total := epv_as_actor + epv_as_receiver]

  # EPV by action type
  action_epv <- calculate_action_type_epv(spadl_with_epv)
  player_epv <- merge(player_epv, action_epv, by = c("player_id", "player_name"), all.x = TRUE)

  # Add minutes if lineups provided
  if (!is.null(lineups) && "minutes_played" %in% names(lineups)) {
    dt_lineups <- data.table::as.data.table(lineups)
    minutes_by_player <- dt_lineups[, .(total_minutes = sum(minutes_played, na.rm = TRUE)),
                                      by = .(player_id, player_name)]

    player_epv <- merge(player_epv, minutes_by_player,
                         by = c("player_id", "player_name"), all.x = TRUE)

    # Calculate per-90
    player_epv[, mins_per_90 := total_minutes / 90]

    epv_cols <- names(player_epv)[grepl("^epv_", names(player_epv))]
    for (col in epv_cols) {
      p90_col <- paste0(col, "_p90")
      player_epv[, (p90_col) := fifelse(mins_per_90 > 0, get(col) / mins_per_90, NA_real_)]
    }
    player_epv[, mins_per_90 := NULL]

    # Filter by minutes
    player_epv <- player_epv[total_minutes >= min_minutes]
  }

  # Replace NAs with 0 in numeric columns
  numeric_cols <- names(player_epv)[sapply(player_epv, is.numeric)]
  for (col in numeric_cols) {
    data.table::set(player_epv, which(is.na(player_epv[[col]])), col, 0)
  }

  # Sort by total EPV
  data.table::setorder(player_epv, -epv_total)

  cli::cli_alert_success("Aggregated EPV for {nrow(player_epv)} players")

  as.data.frame(player_epv)
}


#' Calculate EPV by Action Type
#'
#' @param spadl_with_epv SPADL actions with EPV
#'
#' @return Data frame with EPV totals by action type per player
#' @keywords internal
calculate_action_type_epv <- function(spadl_with_epv) {
  dt <- data.table::as.data.table(spadl_with_epv)

  # Use player_credit if available, otherwise epv_delta or epv
  credit_col <- if ("player_credit" %in% names(dt)) "player_credit"
                else if ("epv_delta" %in% names(dt)) "epv_delta"
                else "epv"

  # Aggregate passing
  passing_epv <- dt[action_type == "pass",
                     .(epv_passing = sum(get(credit_col), na.rm = TRUE)),
                     by = .(player_id, player_name)]

  # Shooting
  shooting_epv <- dt[action_type == "shot",
                      .(epv_shooting = sum(get(credit_col), na.rm = TRUE)),
                      by = .(player_id, player_name)]

  # Dribbling
  dribbling_epv <- dt[action_type == "take_on",
                       .(epv_dribbling = sum(get(credit_col), na.rm = TRUE)),
                       by = .(player_id, player_name)]

  # Defending
  defending_epv <- dt[action_type %in% c("tackle", "interception", "clearance", "aerial", "ball_recovery"),
                       .(epv_defending = sum(get(credit_col), na.rm = TRUE)),
                       by = .(player_id, player_name)]

  # Merge all together
  result <- passing_epv
  result <- merge(result, shooting_epv, by = c("player_id", "player_name"), all = TRUE)
  result <- merge(result, dribbling_epv, by = c("player_id", "player_name"), all = TRUE)
  result <- merge(result, defending_epv, by = c("player_id", "player_name"), all = TRUE)

  as.data.frame(result)
}


# =============================================================================
# MODEL PERSISTENCE
# =============================================================================

#' Save EPV Model
#'
#' Saves trained EPV model to disk.
#'
#' @param epv_model EPV model from fit_epv_model()
#' @param path Directory to save model. If NULL, uses pannadata/models/
#'
#' @return Invisibly returns path
#' @export
save_epv_model <- function(epv_model, path = NULL) {
  if (is.null(path)) {
    path <- file.path(pannadata_dir(), "models")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  model_path <- file.path(path, "epv_model.rds")
  saveRDS(epv_model, model_path)

  cli::cli_alert_success("Saved EPV model to {model_path}")

  invisible(model_path)
}


#' Load EPV Model
#'
#' Loads pre-trained EPV model from disk.
#'
#' @param path Directory containing model. If NULL, uses pannadata/models/
#'
#' @return EPV model
#' @export
load_epv_model <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(pannadata_dir(), "models")
  }

  model_path <- file.path(path, "epv_model.rds")

  if (!file.exists(model_path)) {
    cli::cli_abort(c(
      "EPV model not found at {model_path}",
      "i" = "Train model with fit_epv_model()",
      "i" = "Or download with pb_download_epv_models()"
    ))
  }

  epv_model <- readRDS(model_path)

  cli::cli_alert_success("Loaded EPV model from {model_path}")

  epv_model
}


#' Download EPV Models from GitHub Releases
#'
#' Downloads pre-trained EPV models from GitHub releases.
#'
#' @param repo GitHub repository (default: peteowen1/pannadata)
#' @param tag Release tag (default: epv-models)
#' @param dest Destination directory. If NULL, uses pannadata/models/
#'
#' @return Invisibly returns path to models
#' @export
pb_download_epv_models <- function(repo = "peteowen1/pannadata",
                                    tag = "epv-models",
                                    dest = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("piggyback package required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- file.path(pannadata_dir(), "models")
  }
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)

  cli::cli_alert_info("Downloading EPV models from {repo} ({tag})...")

  model_files <- c(
    "xg_model.rds",
    "xpass_model.rds",
    "epv_model.rds"
  )

  for (f in model_files) {
    tryCatch({
      piggyback::pb_download(
        file = f,
        repo = repo,
        tag = tag,
        dest = dest,
        overwrite = TRUE
      )
      cli::cli_alert_success("Downloaded {f}")
    }, error = function(e) {
      cli::cli_warn("Failed to download {f}: {e$message}")
    })
  }

  cli::cli_alert_success("EPV models downloaded to {dest}")

  invisible(dest)
}


# =============================================================================
# VALIDATION
# =============================================================================

#' Validate EPV Model
#'
#' Checks model calibration and that EPV values are bounded.
#'
#' @param spadl_with_epv SPADL actions with EPV values
#'
#' @return List with validation statistics
#' @export
validate_epv_model <- function(spadl_with_epv) {
  cli::cli_alert_info("Validating EPV model...")

  # Check EPV bounds
  epv_range <- range(spadl_with_epv$epv, na.rm = TRUE)
  bounded <- epv_range[1] >= -1.01 & epv_range[2] <= 1.01  # Small tolerance

  # Sum of deltas
  total_delta <- sum(spadl_with_epv$epv_delta, na.rm = TRUE)

  # Actual goals
  goals <- if ("result" %in% names(spadl_with_epv) && "action_type" %in% names(spadl_with_epv)) {
    sum(spadl_with_epv$action_type == "shot" &
          spadl_with_epv$result == "success", na.rm = TRUE)
  } else NA

  # Match-level stats
  n_matches <- length(unique(spadl_with_epv$match_id))

  results <- list(
    total_actions = nrow(spadl_with_epv),
    epv_range = epv_range,
    epv_bounded = bounded,
    total_epv_delta = total_delta,
    actual_goals = goals,
    mean_epv = mean(spadl_with_epv$epv, na.rm = TRUE),
    mean_epv_delta = mean(spadl_with_epv$epv_delta, na.rm = TRUE),
    sd_epv_delta = stats::sd(spadl_with_epv$epv_delta, na.rm = TRUE),
    n_matches = n_matches
  )

  cli::cli_alert_success(paste0(
    "Validation: EPV range [", round(epv_range[1], 3), ", ", round(epv_range[2], 3), "]",
    ", Bounded: ", bounded,
    ", Mean delta: ", round(results$mean_epv_delta, 4)
  ))

  results
}


# =============================================================================
# LEGACY COMPATIBILITY (deprecated, will be removed)
# =============================================================================

#' @rdname fit_epv_model
#' @export
fit_epv_scoring_model <- function(...) {
  cli::cli_warn("fit_epv_scoring_model() is deprecated. Use fit_epv_model() instead.")
  fit_epv_model(...)
}

#' @rdname fit_epv_model
#' @export
fit_epv_conceding_model <- function(...) {
  cli::cli_warn("fit_epv_conceding_model() is deprecated. Use fit_epv_model() instead.")
  fit_epv_model(...)
}

#' Create EPV Labels (Legacy)
#'
#' Creates labels in the old format for backward compatibility.
#'
#' @param spadl_actions SPADL actions
#'
#' @return Data frame with scores_this_possession and concedes_next_possession
#' @keywords internal
create_epv_labels_legacy <- function(spadl_actions) {
  cli::cli_alert_info("Creating EPV labels (legacy format)...")

  dt <- data.table::as.data.table(spadl_actions)

  # Extract from chain outcome columns if available
  if ("chain_ends_in_goal" %in% names(dt)) {
    dt[, scores_this_possession := as.integer(chain_ends_in_goal)]
  } else {
    dt[, scores_this_possession := 0L]
  }

  if ("opponent_scores_next" %in% names(dt)) {
    dt[, concedes_next_possession := as.integer(opponent_scores_next)]
  } else {
    dt[, concedes_next_possession := 0L]
  }

  as.data.frame(dt[, .(match_id, action_id, scores_this_possession, concedes_next_possession)])
}

#' Assign Pass Credit (Legacy)
#'
#' Legacy function for backward compatibility. Use assign_epv_credit() instead.
#'
#' @param spadl_with_epv SPADL with EPV
#' @param xpass_model xPass model
#'
#' @return SPADL with credit columns
#' @export
assign_pass_credit <- function(spadl_with_epv, xpass_model) {
  cli::cli_warn("assign_pass_credit() is deprecated. Use assign_epv_credit() instead.")
  assign_epv_credit(spadl_with_epv, xpass_model)
}
