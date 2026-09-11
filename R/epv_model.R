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

#' Find the next event after each action via non-equi rolling join
#'
#' Core helper for label creation. For each action, finds the first matching
#' event (goal, shot, etc.) that occurs after it in the same match-period.
#' Uses data.table non-equi join for O(n log n) performance.
#'
#' @param dt data.table of SPADL actions (must have match_id, period_id,
#'   time_seconds, action_id, team_id)
#' @param events_dt data.table of target events. Must have columns:
#'   match_id, period_id, event_time, event_team
#'
#' @return dt with next_event_team column added (NA if no subsequent event)
#' @keywords internal
.find_next_event <- function(dt, events_dt) {
  events_dt <- data.table::copy(events_dt)
  data.table::setorder(events_dt, match_id, period_id, event_time)
  dt[, action_time := time_seconds]

  result <- events_dt[dt,
    on = .(match_id, period_id, event_time > action_time),
    mult = "first",
    .(match_id = i.match_id,
      action_id = i.action_id,
      next_event_team = x.event_team)
  ]

  dt <- result[dt, on = c("match_id", "action_id")]
  dt[, action_time := NULL]
  dt
}


#' Find the next event with extra columns via non-equi rolling join
#'
#' Like \code{.find_next_event()} but also carries through an extra numeric
#' column (e.g., shot_xg) from the events table.
#'
#' @param dt data.table of SPADL actions
#' @param events_dt data.table of target events. Must have columns:
#'   match_id, period_id, event_time, event_team, plus \code{extra_col}
#' @param extra_col Name of the additional column to carry through
#'
#' @return dt with next_event_team and next_\{extra_col\} columns added
#' @keywords internal
.find_next_event_with_value <- function(dt, events_dt, extra_col) {
  # Copy to avoid mutating caller's data.table (setorder + setnames modify in place)
  events_dt <- data.table::copy(events_dt)
  data.table::setorder(events_dt, match_id, period_id, event_time)
  dt[, action_time := time_seconds]

  # Rename the extra column to a known name for the join
  data.table::setnames(events_dt, extra_col, "extra_value")

  result <- events_dt[dt,
    on = .(match_id, period_id, event_time > action_time),
    mult = "first",
    .(match_id = i.match_id,
      action_id = i.action_id,
      next_event_team = x.event_team,
      next_extra_value = x.extra_value)
  ]

  next_col_name <- paste0("next_", extra_col)
  data.table::setnames(result, "next_extra_value", next_col_name)

  dt <- result[dt, on = c("match_id", "action_id")]
  dt[, action_time := NULL]
  dt
}


#' Create Next Goal Labels for EPV Model
#'
#' Determines who scores the next goal in each half for each action.
#' Labels: 0 = possession team scores next, 1 = opponent scores next, 2 = nobody scores
#'
#' @param spadl_actions SPADL actions with team_id, period_id, match_id
#'
#' @return Data frame with next_goal_label column added
#'
#' @keywords internal
create_next_goal_labels <- function(spadl_actions) {
  cli::cli_alert_info("Creating next goal labels...")

  dt <- data.table::as.data.table(spadl_actions)
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Find goals (shots with success)
  goals_dt <- dt[action_type == "shot" & result == "success", .(
    match_id,
    period_id,
    event_time = time_seconds,
    event_team = team_id
  )]

  # Rolling join to find next goal for each action
  dt <- .find_next_event(dt, goals_dt)

  # Create label: 0 = team scores, 1 = opponent scores, 2 = nobody scores
  dt[, next_goal_label := fifelse(
    is.na(next_event_team),
    2L,
    fifelse(next_event_team == team_id, 0L, 1L)
  )]
  dt[, next_event_team := NULL]

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
#' @keywords internal
create_next_xg_labels <- function(spadl_actions, xg_values = NULL) {
  cli::cli_alert_info("Creating next xG labels...")

  dt <- data.table::as.data.table(spadl_actions)
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Get xG values for shots
  if (!is.null(xg_values)) {
    if (is.data.frame(xg_values)) {
      xg_df <- data.table::as.data.table(xg_values)
      if (all(c("match_id", "action_id", "xg") %in% names(xg_df))) {
        dt <- xg_df[, .(match_id, action_id, shot_xg = xg)][dt, on = c("match_id", "action_id")]
      }
    }
  } else if ("chain_xg" %in% names(dt) && any(!is.na(dt$chain_xg))) {
    dt[action_type == "shot", shot_xg := chain_xg]
  }

  # If no xG available (or all NA), estimate from position (simple model)
  if (!"shot_xg" %in% names(dt) || all(is.na(dt$shot_xg))) {
    cli::cli_alert_info("No xG values found, estimating from position...")
    dt[action_type == "shot", shot_xg := estimate_simple_xg(start_x, start_y)]
  }

  # Fill NA xG for non-shots
  dt[is.na(shot_xg), shot_xg := 0]

  # Find all shots with their xG
  shots_dt <- dt[action_type == "shot", .(
    match_id,
    period_id,
    event_time = time_seconds,
    event_team = team_id,
    shot_xg = shot_xg
  )]

  # Rolling join to find next shot + its xG for each action
  dt <- .find_next_event_with_value(dt, shots_dt, "shot_xg")

  # Create label: positive xG for team's shot, negative for opponent's, 0 for none
  dt[, next_xg_label := fifelse(
    is.na(next_shot_xg),
    0,
    fifelse(next_event_team == team_id, next_shot_xg, -next_shot_xg)
  )]

  # Cleanup
  dt[, c("next_event_team", "next_shot_xg") := NULL]
  if ("shot_xg" %in% names(dt)) dt[, shot_xg := NULL]

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
#' Calibrated to produce ~11% mean xG (matching real shot distributions):
#' - 6 yard box (dist ~6): xG ~ 0.35-0.40
#' - Penalty spot (dist ~12): xG ~ 0.15-0.20
#' - Edge of box (dist ~18): xG ~ 0.06-0.10
#' - Long range (dist ~30): xG ~ 0.02-0.03
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

  # Calibrated logistic model to produce ~11% mean xG
  # Lower intercept (-2.8) and stronger distance penalty (-0.12)
  logit <- -2.8 - 0.12 * dist_to_goal + 3.0 * angle
  xg <- 1 / (1 + exp(-logit))

  # Cap between 0.01 and 0.75
  pmin(pmax(xg, 0.01), 0.75)
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
#' @param eta Learning rate (default 0.1)
#' @param subsample Row subsampling (default 0.8)
#' @param colsample_bytree Column subsampling (default 0.8)
#' @param nrounds Maximum boosting rounds (default 1000)
#' @param early_stopping_rounds Early stopping patience (default 50)
#' @param verbose Print progress (default 1)
#'
#' @return Fitted EPV model with metadata
#'
#' @family epv
#' @export
fit_epv_model <- function(features,
                           labels,
                           method = c("goal", "xg"),
                           nfolds = 5,
                           max_depth = 6,
                           eta = 0.1,
                           subsample = 0.8,
                           colsample_bytree = 0.8,
                           nrounds = 1000,
                           early_stopping_rounds = 50,
                           verbose = 1) {

  method <- match.arg(method)

  # Validate inputs
  validate_dataframe(features, required_cols = c("match_id", "action_id"), arg_name = "features")

  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. Install with: install.packages('xgboost')")
  }


  # Get feature columns -- check simple features first, then full
  simple_available <- intersect(EPV_SIMPLE_FEATURE_COLS, names(features))
  if (length(simple_available) >= length(EPV_SIMPLE_FEATURE_COLS)) {
    available_cols <- simple_available
  } else {
    feature_cols <- get_epv_feature_cols(include_sequence = TRUE,
                                         n_prev = EPV_N_PREV)
    available_cols <- intersect(feature_cols, names(features))
  }

  if (length(available_cols) < 5) {
    cli::cli_abort("Insufficient features available for EPV model")
  }

  # Determine label column based on method
  label_col <- if (method == "goal") "next_goal_label" else "next_xg_label"

  cli::cli_alert_info("Fitting EPV {method} model with {length(available_cols)} features...")

  # Convert to data.table for fast operations
  dt <- data.table::as.data.table(features)

  # Get labels - either from labels parameter or already in features
  if (label_col %in% names(dt)) {
    # Labels already in features, nothing to do
  } else if (label_col %in% names(labels)) {
    # Fast keyed join instead of merge
    labels_dt <- data.table::as.data.table(labels)[, c("match_id", "action_id", label_col), with = FALSE]
    data.table::setkeyv(labels_dt, c("match_id", "action_id"))
    data.table::setkeyv(dt, c("match_id", "action_id"))
    dt[labels_dt, (label_col) := get(paste0("i.", label_col))]
  } else {
    cli::cli_abort("{label_col} column not found in features or labels")
  }

  # Filter valid rows first (before matrix conversion)
  y_vec <- dt[[label_col]]
  valid_idx <- which(!is.na(y_vec))
  y <- y_vec[valid_idx]

  # Extract feature matrix directly using data.table (faster than as.matrix on data.frame)
  X <- as.matrix(dt[valid_idx, ..available_cols])

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
      params = params,
      leagues_trained = if ("league_id" %in% available_cols)
        names(EPV_LEAGUE_MAP) else NULL
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

  # --- Feature-contract guard (the "can't silently revert to the wrong model") ---
  # A simple-mode EPV model whose feature_cols differ from what the current code
  # emits (EPV_SIMPLE_FEATURE_COLS) is a VERSION MISMATCH — e.g. the pre-overhaul
  # 17-feature model (dx/dy/result_success) loaded against 14-feature code. The
  # 0-fill below would then silently score every action with those features = 0,
  # producing garbage (this shipped inflated EPV 2026-06-21). Refuse to score.
  if (identical(model$panna_metadata$feature_mode %||% NA_character_, "simple")) {
    extra <- setdiff(feature_cols, EPV_SIMPLE_FEATURE_COLS)
    if (length(extra) > 0) {
      cli::cli_abort(c(
        "EPV model feature-contract MISMATCH — refusing to score with a stale/wrong model.",
        "x" = "Model expects {length(feature_cols)} features incl. {.val {extra}} the current code does NOT emit.",
        "i" = "Code emits {length(EPV_SIMPLE_FEATURE_COLS)} (EPV_SIMPLE_FEATURE_COLS) -- this looks like the pre-overhaul model.",
        "i" = "Use the clean model: pass {.code epv_model_override} or republish the clean model as default (see MODELS.md)."
      ))
    }
  }

  # Convert to data.table for fast column operations
  dt <- data.table::as.data.table(features)

  # Add missing columns (vectorized, not loop)
  missing_cols <- setdiff(feature_cols, names(dt))
  if (length(missing_cols) > 0) {
    dt[, (missing_cols) := 0]
  }

  # Extract feature matrix efficiently
  X <- as.matrix(dt[, ..feature_cols])

  # Replace NA with 0 using nafill on each column (faster than X[is.na(X)] <- 0)
  # But for matrix, direct replacement is still needed - use vectorized approach
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
#' @param xg_model Optional pre-trained xG model from fit_xg_model(). If NULL,
#'   attempts to load from pannadata/data/opta/models/xg_model.rds. Falls back to
#'   position-based estimate if no model available.
#' @param league League code (e.g., "ENG") for league-aware EPV features.
#'   Only used when feature_mode is "simple". If NULL, defaults to 0 (unknown).
#' @param season Season label for this league (e.g. "2025-2026", "2026",
#'   "2026 Canada-Mexico-USA"), forwarded to \code{add_xg_to_spadl()}. Required
#'   when the xG model carries a season term; that model aborts rather than
#'   score without it.
#' @param shot_lookup Shot events keyed by (\code{match_id}, \code{event_id})
#'   with \code{body_part} and \code{situation}, forwarded to
#'   \code{add_xg_to_spadl()}. Without it six xG features are constant 0 and the
#'   xG feeding EPV is skewed roughly 6%. Build it with
#'   \code{load_opta_shot_events(league, season)}, not by reading the parquet
#'   directly.
#'
#' @return SPADL actions with EPV columns added:
#'   \itemize{
#'     \item epv: EPV at this state
#'     \item epv_delta: Change in EPV (with perspective handling)
#'     \item xg: For shots, the xG value (from model or estimated)
#'     \item For "goal" method: p_team_scores, p_opponent_scores, p_nobody_scores
#'     \item For "xg" method: expected_xg
#'   }
#'
#' @keywords internal
calculate_action_epv <- function(spadl_actions, features = NULL, epv_model, xg_model = NULL,
                                  league = NULL, season = NULL, shot_lookup = NULL) {
  cli::cli_alert_info("Calculating EPV for {nrow(spadl_actions)} actions...")

  # Try to load pre-trained Opta xG model if not provided
  if (is.null(xg_model)) {
    xg_model <- tryCatch({
      load_xg_model()
    }, error = function(e) {
      cli::cli_warn("No pre-trained Opta xG model found. Using position-based estimate for shots.")
      NULL
    })
  }

  # Apply xG model to add xg column for shots (if model available). `season` and
  # `shot_lookup` are forwarded because without them the xG scored here is
  # skewed: a season-aware model refuses to score without a season, and without
  # the lookup SPADL supplies neither body part (its `bodypart` says "foot" for
  # every shot) nor situation, leaving six features constant 0. See
  # add_xg_to_spadl(). This function feeds EPV, WPA, game logs and equity, so
  # the skew reached all of them.
  if (!is.null(xg_model)) {
    spadl_actions <- add_xg_to_spadl(spadl_actions, xg_model, season = season,
                                     shot_lookup = shot_lookup)
  }

  method <- epv_model$method %||% "goal"

  # For simple models, create features from the SPADL actions directly
  feature_mode <- epv_model$panna_metadata$feature_mode %||% "full"
  if (feature_mode == "simple") {
    features <- create_epv_features_simple(spadl_actions, league = league)
  }

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

  # Note: Aerial actions are handled as stationary duel actions in SPADL
  # (end coordinates set to start coordinates via merge_duel_rows).

  # Sort by match and action_id
  data.table::setorder(dt, match_id, action_id)

  # Calculate lead values (next action's EPV)
  # EPV measures state BEFORE action, so delta = next_epv - current_epv
  dt[, `:=`(
    next_epv = shift(epv, 1, type = "lead"),
    next_team_id = shift(team_id, 1, type = "lead")
  ), by = match_id]

  # Calculate delta with perspective handling
  # If same team: delta = next_epv - current_epv
  # If team change: delta = (-next_epv) - current_epv
  #   Because from current team's perspective, next team's positive EPV is negative
  dt[, epv_delta := fifelse(
    is.na(next_team_id),
    0,  # Last action in match
    fifelse(
      team_id == next_team_id,
      next_epv - epv,      # Same team: simple difference
      (-next_epv) - epv    # Team change: flip next team's EPV
    )
  )]

  # ==========================================================================
  # SHOT EPV OVERRIDE
  #
  # The EPV model predicts "who scores next goal this half", which causes
  # shot actions to have artificially LOW EPV because:
  #   - Most shots miss
  #   - After a miss, opponent often gets possession
  #   - So model learns is_shot=1 -> low P(team_scores_next)
  #
  # FIX: Override shot EPV with xG, then calculate delta based on actual
  # outcome vs expectation:
  #   - Goal: delta = 1 - xG (credit for converting)
  #   - Miss: delta = 0 - xG (blame for missing)
  # ==========================================================================

  # Save original model EPV for debugging if needed
  dt[action_type == "shot", epv_model := epv]

  # Override shot EPV with xG if available, otherwise estimate from position
  if ("xg" %in% names(dt)) {
    # Use provided xG
    n_with_xg <- sum(dt$action_type == "shot" & !is.na(dt$xg))
    dt[action_type == "shot" & !is.na(xg), epv := xg]

    # For shots without xG, estimate from position
    n_estimated <- sum(dt$action_type == "shot" & is.na(dt$xg))
    if (n_estimated > 0) {
      dt[action_type == "shot" & is.na(xg),
         epv := estimate_simple_xg(start_x, start_y)]
    }

    cli::cli_alert_info("Shot EPV: {n_with_xg} from xG, {n_estimated} estimated from position")
  } else {
    # No xG column - estimate all shots from position
    n_shots <- sum(dt$action_type == "shot")
    dt[action_type == "shot", epv := estimate_simple_xg(start_x, start_y)]
    cli::cli_alert_info("Estimated shot EPV from position for {n_shots} shots (no xG column)")
  }

  # Restore own goals to model EPV -- xG is meaningless for deflections.
  # The xG model sees start_x=3 (near own goal) and predicts 0.97, but
  # that's the xG of a deliberate shot from 3m out, not a deflection.
  if ("is_own_goal" %in% names(dt)) {
    n_og <- sum(dt$action_type == "shot" & dt$is_own_goal == TRUE, na.rm = TRUE)
    if (n_og > 0) {
      dt[action_type == "shot" & is_own_goal == TRUE, epv := epv_model]
      cli::cli_alert_info("Restored {n_og} own goals to model EPV (xG not applicable)")
    }
  }

  # For goals: delta = 1 - epv (where epv is now xG if available)
  # This gives credit = 1 - xG for scoring
  dt[action_type == "shot" & result == "success",
     epv_delta := 1 - epv]

  # Handle own goals (is_own_goal from Opta qualifier 28)
  # Own goals have negative terminal value: opponent achieved max value
  # delta = -1 - epv (where epv is now model EPV, not xG)
  if ("is_own_goal" %in% names(dt)) {
    dt[action_type == "shot" & result == "success" & is_own_goal == TRUE,
       epv_delta := -1 - epv]
  }

  # For missed shots: delta = 0 - epv (where epv is now xG or estimated)
  # This gives blame = -xG for missing
  # (epv was already overridden with xG or estimated above)
  dt[action_type == "shot" & result == "fail",
     epv_delta := 0 - epv]

  # ==========================================================================
  # FIX DELTA FOR ROWS PRECEDING SHOTS
  #
  # The original delta calculation used the model's low shot EPV (~0.07).
  # Now that shots have xG-based EPV, recalculate delta for the row before
  # each shot so assists and key passes get proper credit.
  # ==========================================================================

  # Recalculate next_epv now that shot EPV is fixed
  dt[, next_epv_fixed := shift(epv, 1, type = "lead"), by = match_id]
  dt[, next_team_fixed := shift(team_id, 1, type = "lead"), by = match_id]
  dt[, next_action := shift(action_type, 1, type = "lead"), by = match_id]

  # Update delta for rows that precede shots (usually passes/assists)
  dt[next_action == "shot" & !is.na(next_epv_fixed), epv_delta := fifelse(
    team_id == next_team_fixed,
    next_epv_fixed - epv,      # Same team: simple difference
    (-next_epv_fixed) - epv    # Team change: flip (rare for shots)
  )]

  # Cleanup temporary columns
  dt[, c("next_epv_fixed", "next_team_fixed", "next_action") := NULL]

  # Mark actions where possession changes to next team
  # This is used for turnover credit assignment
  dt[, possession_change := fifelse(
    is.na(next_team_id),
    FALSE,
    team_id != next_team_id
  )]

  # Cleanup temporary columns (keep possession_change for credit assignment)
  dt[, c("next_epv", "next_team_id") := NULL]

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
#' For merged duels (Aerials, Take On vs Tackle), assigns zero-sum credit
#' where winner gain = loser loss.
#'
#' @param spadl_with_epv SPADL actions with EPV values from calculate_action_epv()
#' @param xpass_model Optional xPass model for pass difficulty weighting
#'
#' @return SPADL actions with credit columns added:
#'   \itemize{
#'     \item player_credit: EPV credit to acting player
#'     \item receiver_credit: EPV credit to receiver (for turnovers, this is positive)
#'     \item opponent_credit: EPV credit to duel loser (negative, via opponent_player_id)
#'     \item xpass: Pass completion probability (for passes)
#'   }
#'
#' @family epv
#' @export
assign_epv_credit <- function(spadl_with_epv, xpass_model = NULL) {
  cli::cli_alert_info("Assigning EPV credit...")

  dt <- data.table::as.data.table(spadl_with_epv)

  # Initialize credit columns
  dt[, `:=`(
    player_credit = epv_delta,  # Default: acting player gets full delta
    receiver_credit = 0,
    opponent_credit = NA_real_, # For duel losers (opponent_player_id)
    xpass = NA_real_
  )]

  # Check if we have possession_change column
  has_possession_info <- "possession_change" %in% names(dt)

  if (!has_possession_info) {
    cli::cli_warn("No possession_change column - cannot split turnover credit")
    return(as.data.frame(dt))
  }

  # Handle credit assignment based on action type and possession context
  #
  # With lead-based delta calculation:
  # - epv_delta = next_epv - current_epv (same team) or (-next_epv) - current_epv (team change)
  # - For turnovers (possession_change == TRUE), the delta is negative for the acting player
  #   because the next action benefits the opposing team
  # - The acting player naturally gets blame (negative delta)
  # - The receiver (next team) gets credit by flipping the sign
  #
  # Defensive actions (tackle, interception, ball_recovery, etc.) that win the ball
  # will have the turnover captured by the PREVIOUS player's action (the attacker who lost it)
  # The defender's subsequent action starts fresh with their team's EPV

  # For turnovers (possession_change == TRUE), split credit between loser and gainer
  # EXCLUDE:
  # - Goals: terminal actions where scorer gets full credit, kickoff taker not involved
  # - Half-time boundary: receiver in period 2 not involved in period 1 play
  #
  # Detect half-time boundary by checking if next action is in a different period
  dt[, next_period_id := shift(period_id, 1, type = "lead"), by = match_id]
  dt[, is_period_boundary := !is.na(next_period_id) & period_id != next_period_id]

  turnover_idx <- which(dt$possession_change == TRUE &
                        !is.na(dt$epv_delta) &
                        !(dt$action_type == "shot" & dt$result == "success") &
                        dt$is_period_boundary == FALSE)

  if (length(turnover_idx) > 0) {
    # For turnovers, split credit between actor and receiver
    # But only if delta is NEGATIVE (actor lost value)
    #
    # If delta is POSITIVE (actor's team gained value despite losing possession,
    # e.g., winning a corner/foul in good position), actor gets credit and
    # receiver gets 0 (shouldn't be blamed for taking the next action)
    dt[turnover_idx, `:=`(
      player_credit = fifelse(
        epv_delta < 0,
        epv_delta * EPV_TURNOVER_BLAME_SHARE,    # Negative: share blame
        epv_delta                                  # Positive: actor gets full credit
      ),
      receiver_credit = fifelse(
        epv_delta < 0,
        -epv_delta * EPV_TURNOVER_BLAME_SHARE,   # Negative: receiver gets credit for gaining
        0                                          # Positive: receiver not blamed
      )
    )]
  }

  # Handle failed shots by type (requires opta_type_id from SPADL conversion)
  # - Type 13 (Missed): No receiver credit (off target)
  # - Type 14 (Post): No receiver credit (hit woodwork)
  # - Type 15 (Saved): Give defender/GK credit for the save
  if ("opta_type_id" %in% names(dt)) {

    # For saved shots (type 15), give defender/GK credit for the save
    # Only apply if receiver is on DIFFERENT team (excludes rebounds where shooter takes next action)
    saved_shot_idx <- which(dt$action_type == "shot" &
                            dt$result == "fail" &
                            dt$opta_type_id == 15 &
                            !is.na(dt$receiver_player_id) &
                            !is.na(dt$receiver_team_id) &
                            dt$team_id != dt$receiver_team_id)  # Receiver on opposing team

    if (length(saved_shot_idx) > 0) {
      # Shooter takes blame for not scoring (delta is typically negative)
      # Defender/GK gets positive credit for preventing the goal
      # Use 50/50 split like turnovers
      dt[saved_shot_idx, `:=`(
        player_credit = epv_delta * EPV_TURNOVER_BLAME_SHARE,      # Shooter blame
        receiver_credit = -epv_delta * EPV_TURNOVER_BLAME_SHARE    # Defender credit
      )]
    }

    n_saved <- length(saved_shot_idx)
    if (n_saved > 0) {
      cli::cli_alert_info("Applied save credit to {n_saved} saved shots")
    }

    # For missed shots (type 13) and posts (type 14), no receiver credit
    # The shot missed the target entirely, so no defender made a save
    # Override any turnover logic that may have been applied
    missed_shot_idx <- which(dt$action_type == "shot" &
                              dt$result == "fail" &
                              dt$opta_type_id %in% c(13, 14))  # Missed or Post

    if (length(missed_shot_idx) > 0) {
      # Shooter takes full blame, no defender credit
      dt[missed_shot_idx, `:=`(
        player_credit = epv_delta,  # Full blame to shooter
        receiver_credit = 0         # No defender involved
      )]
    }

    n_missed <- length(missed_shot_idx)
    if (n_missed > 0) {
      cli::cli_alert_info("Reset {n_missed} missed/post shots (no defender credit)")
    }
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
      #
      # IMPORTANT: Only share POSITIVE deltas with receiver. For negative deltas
      # (backward/safety passes), the passer chose to play safe - the receiver
      # shouldn't be blamed for receiving a backpass (e.g., keeper receiving
      # backpasses shouldn't accumulate negative EPV).
      #
      # POSITION SCALING: Passes from deep positions (near own goal) get
      # scaled down because EPV improvement from own box is "expected" for routine
      # passes. A keeper's short pass to a defender shouldn't get more credit than
      # a midfielder's creative through ball just because EPV delta is higher.
      #
      # Coordinate system: x=0 is own goal, x=100 is attacking goal
      # Scale factor: EPV_POSITION_SCALE_MIN at x=0 (own goal), ramping to 1.0 at x=EPV_POSITION_RAMP_X
      # This reduces credit for routine GK/defender distribution while maintaining
      # full credit for passes in advanced positions.
      #
      # passer_share = EPV_BASE_PASSER_SHARE + EPV_PASS_DIFFICULTY_ADJUSTMENT * (1 - xpass)
      success_pass_idx <- which(dt$action_type == "pass" &
                                  dt$result == "success" &
                                  dt$possession_change == FALSE)

      if (length(success_pass_idx) > 0) {
        # Position-based scaling: reduce credit for routine deep passes
        # x=0 (own goal): scale=EPV_POSITION_SCALE_MIN, x=EPV_POSITION_RAMP_X: scale=1.0
        dt[success_pass_idx, `:=`(
          position_scale = pmin(1.0, EPV_POSITION_SCALE_MIN +
            (1 - EPV_POSITION_SCALE_MIN) * (start_x / EPV_POSITION_RAMP_X)),
          passer_share = EPV_BASE_PASSER_SHARE + EPV_PASS_DIFFICULTY_ADJUSTMENT * (1 - xpass)
        )]

        # For positive delta (progressive passes): split credit between passer and receiver
        # For negative delta (backward/sideways passes): small penalty (20% of delta)
        #   Keeping possession is valuable, but backward passes do lose some value.
        #   Apply 80% discount to avoid over-penalizing safe passes.
        backward_penalty <- 0.2  # Only 20% of normal penalty for backward passes

        dt[success_pass_idx, `:=`(
          player_credit = fifelse(
            epv_delta >= 0,
            epv_delta * passer_share * position_scale,              # Positive: share credit
            epv_delta * position_scale * backward_penalty           # Negative: small penalty
          ),
          receiver_credit = fifelse(
            epv_delta >= 0,
            epv_delta * (1 - passer_share) * position_scale,  # Positive: share credit
            0                                                  # Negative: receiver not blamed
          )
        )]
        dt[, c("passer_share", "position_scale") := NULL]  # Clean up temp columns
      }

      # For failed passes (turnovers):
      # Use xPass to determine blame split, but only for NEGATIVE delta
      # Higher xPass (should have completed) = more blame on passer
      # Lower xPass (risky pass) = less blame, more credit to interceptor
      #
      # For positive delta (rare: failed pass still benefited team), passer gets full credit
      #
      # Position scaling: failed passes from deep positions (near own goal) get LESS
      # scaling reduction since mistakes from deep ARE costly and should be penalized.
      # Coordinate system: x=0 is own goal, x=100 is attacking goal
      # Scale: 0.6 at x=0 (own goal), 1.0 at x=EPV_POSITION_RAMP_X and beyond
      #
      # passer_blame_share: how much of the negative delta the passer takes
      # passer_blame = EPV_BASE_PASSER_SHARE + EPV_PASS_DIFFICULTY_ADJUSTMENT * xpass
      # Exclude period boundaries - receiver in next half not involved in this play
      failed_pass_idx <- which(dt$action_type == "pass" &
                                 (dt$result == "fail" | dt$possession_change == TRUE) &
                                 dt$is_period_boundary == FALSE)

      if (length(failed_pass_idx) > 0) {
        # Less aggressive scaling for failed passes (mistakes from deep are still costly)
        # x=0 (own goal): scale=0.6, x=EPV_POSITION_RAMP_X: scale=1.0
        failed_scale_min <- 0.6
        dt[failed_pass_idx, `:=`(
          position_scale = pmin(1.0, failed_scale_min +
            (1 - failed_scale_min) * (start_x / EPV_POSITION_RAMP_X)),
          passer_blame = EPV_BASE_PASSER_SHARE + EPV_PASS_DIFFICULTY_ADJUSTMENT * xpass
        )]

        # Only split for negative delta (actual loss of value)
        # For positive delta (rare), passer gets full credit, receiver gets 0
        # For deep positions (x < 20), reduce penalty further (routine distribution)
        dt[failed_pass_idx, `:=`(
          player_credit = fifelse(
            epv_delta < 0,
            fifelse(
              start_x < 20,
              epv_delta * passer_blame * position_scale * 0.5,  # Deep: halve the penalty
              epv_delta * passer_blame * position_scale          # Advanced: full penalty
            ),
            epv_delta  # Positive: passer gets full credit
          ),
          receiver_credit = fifelse(
            epv_delta < 0,
            fifelse(
              start_x < 20,
              -epv_delta * (1 - passer_blame) * position_scale * 0.5,  # Deep: halve
              -epv_delta * (1 - passer_blame) * position_scale          # Advanced: full
            ),
            0  # Positive: receiver not blamed
          )
        )]
        dt[, c("passer_blame", "position_scale") := NULL]
      }
    }
  }

  # Cleanup temporary columns
  dt[, c("next_period_id", "is_period_boundary") := NULL]


  # POSITION SCALING FOR OFFENSIVE ACTIONS FROM DEEP
  #
  # Apply position scaling to reduce credit for "routine" offensive actions
  # performed from deep positions (near own goal). The EPV model correctly
  # identifies that game state improves when ball moves upfield, but we don't
  # want to over-credit routine actions like keeper pick-ups or punts.
  #
  # Offensive actions (scaled): keeper_pick_up, keeper_punch, other, ball_touch
  # Defensive actions (NOT scaled): keeper_save, tackle, interception, clearance,
  #                                  ball_recovery, aerial, foul
  #
  # Coordinate system: x=0 is own goal, x=100 is attacking goal
  # Scale: EPV_POSITION_SCALE_MIN at x=0, ramping to 1.0 at x=EPV_POSITION_RAMP_X
  # This matches the pass position scaling for consistency.
  #
  offensive_action_types <- c("keeper_pick_up", "keeper_punch", "other",
                              "ball_touch", "take_on")

  # Only scale actions from deep positions (x < EPV_POSITION_RAMP_X) that are offensive
  # and haven't already been handled (passes are already scaled above)
  offensive_deep_idx <- which(dt$action_type %in% offensive_action_types &
                               dt$start_x < EPV_POSITION_RAMP_X &
                               !is.na(dt$player_credit))

  if (length(offensive_deep_idx) > 0) {
    # Apply same position scaling formula as passes
    dt[offensive_deep_idx, position_scale := pmin(1.0, EPV_POSITION_SCALE_MIN +
      (1 - EPV_POSITION_SCALE_MIN) * (start_x / EPV_POSITION_RAMP_X))]
    dt[offensive_deep_idx, player_credit := player_credit * position_scale]
    dt[, position_scale := NULL]

    cli::cli_alert_info("Applied position scaling to {length(offensive_deep_idx)} offensive actions from deep")
  }


  # BOOST DEFENSIVE ACTION CREDIT
  #
  # Defensive actions (clearance, interception, tackle, ball_recovery) are

  # undervalued relative to their importance. These actions prevent goals
  # and regain possession but the EPV delta often doesn't capture their
  # full value. Apply a multiplier to boost their credit.
  #
  # Note: keeper_save already gets proper credit through the save handling above.
  #
  # Note: aerials are handled as stationary duels in SPADL but excluded from
  # defensive boost since their EPV contribution is captured through duel outcomes
  defensive_action_types <- c("clearance", "interception", "tackle", "ball_recovery")

  defensive_idx <- which(dt$action_type %in% defensive_action_types &
                          !is.na(dt$player_credit))

  if (length(defensive_idx) > 0) {
    dt[defensive_idx, player_credit := player_credit * EPV_DEFENSIVE_BOOST]
    cli::cli_alert_info("Boosted {length(defensive_idx)} defensive actions by {EPV_DEFENSIVE_BOOST}x")
  }


  # DUEL CREDIT: Zero-sum for merged duel rows
  #
  # Merged duels (Aerial, Take On vs Tackle) have opponent_player_id populated
  # with the loser's identity. Make duels zero-sum: winner gain = loser loss.
  # This is one of the few opportunities to assign negative credit directly.
  #
  # Note: Don't use abs() - the EPV delta already encodes position value.
  # Positive delta = winner gained value, negative = won but lost value (rare).
  if ("opponent_player_id" %in% names(dt)) {
    duel_idx <- which(!is.na(dt$opponent_player_id) & !is.na(dt$epv_delta))

    if (length(duel_idx) > 0) {
      # Winner: half the value change (already has player_credit set)
      # Loser: opposite (zero-sum)
      dt[duel_idx, `:=`(
        player_credit = epv_delta * EPV_TURNOVER_BLAME_SHARE,
        opponent_credit = -epv_delta * EPV_TURNOVER_BLAME_SHARE
      )]
      cli::cli_alert_info("Applied zero-sum duel credit to {length(duel_idx)} merged duels")
    }
  }

  n_turnovers <- sum(dt$possession_change == TRUE, na.rm = TRUE)
  n_duels <- sum(!is.na(dt$opponent_player_id), na.rm = TRUE)
  cli::cli_alert_success("Assigned credit for {nrow(dt)} actions ({n_turnovers} turnovers, {n_duels} duels)")

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
#'     \item epv_duel_blame: EPV from losing duels (negative, summed from opponent_credit)
#'     \item epv_passing, epv_shooting, epv_dribbling, epv_defending
#'   }
#'
#' @keywords internal
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

  # Full outer join: actor + receiver credit
  player_epv <- receiver_credit[actor_credit, on = c("player_id", "player_name")]
  unmatched_receivers <- receiver_credit[!actor_credit, on = c("player_id", "player_name")]
  if (nrow(unmatched_receivers) > 0) {
    player_epv <- data.table::rbindlist(list(player_epv, unmatched_receivers), fill = TRUE)
  }
  player_epv[is.na(epv_as_actor), epv_as_actor := 0]
  player_epv[is.na(epv_as_receiver), epv_as_receiver := 0]
  player_epv[is.na(n_actions), n_actions := 0L]

  # Opponent credit: sum opponent_credit (duel blame) to the opponent player
  # This is negative credit for losing duels (Aerials, Take On vs Tackle, etc.)
  if ("opponent_player_id" %in% names(dt) && "opponent_credit" %in% names(dt)) {
    opponent_dt <- dt[!is.na(opponent_player_id) & !is.na(opponent_credit)]

    if (nrow(opponent_dt) > 0) {
      opponent_blame <- opponent_dt[, .(
        epv_duel_blame = sum(opponent_credit, na.rm = TRUE)
      ), by = .(opponent_player_id, opponent_player_name)]

      data.table::setnames(opponent_blame,
                            c("opponent_player_id", "opponent_player_name"),
                            c("player_id", "player_name"))

      # Full outer join: player_epv + opponent_blame
      unmatched_opponents <- opponent_blame[!player_epv, on = c("player_id", "player_name")]
      player_epv <- opponent_blame[player_epv, on = c("player_id", "player_name")]
      if (nrow(unmatched_opponents) > 0) {
        player_epv <- data.table::rbindlist(list(player_epv, unmatched_opponents), fill = TRUE)
      }
      # Fill NAs for opponent-only players (they have no actor/receiver credit)
      player_epv[is.na(epv_as_actor), epv_as_actor := 0]
      player_epv[is.na(epv_as_receiver), epv_as_receiver := 0]
      player_epv[is.na(epv_duel_blame), epv_duel_blame := 0]
      player_epv[is.na(n_actions), n_actions := 0L]

      cli::cli_alert_info("Aggregated duel blame for {nrow(opponent_blame)} players")
    } else {
      player_epv[, epv_duel_blame := 0]
    }
  } else {
    player_epv[, epv_duel_blame := 0]
  }

  # Total EPV = actor + receiver credit + duel blame (negative for losers)
  player_epv[, epv_total := epv_as_actor + epv_as_receiver + epv_duel_blame]

  # EPV by action type
  action_epv <- calculate_action_type_epv(spadl_with_epv)
  player_epv <- data.table::as.data.table(action_epv)[player_epv, on = c("player_id", "player_name")]

  # Add minutes if lineups provided
  if (!is.null(lineups) && "minutes_played" %in% names(lineups)) {
    dt_lineups <- data.table::as.data.table(lineups)
    minutes_by_player <- dt_lineups[, .(total_minutes = sum(minutes_played, na.rm = TRUE)),
                                      by = .(player_id, player_name)]

    player_epv <- minutes_by_player[player_epv, on = c("player_id", "player_name")]

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

  # Defending (aerials excluded -- their value is captured through duel outcomes)
  defending_epv <- dt[action_type %in% c("tackle", "interception", "clearance", "ball_recovery"),
                       .(epv_defending = sum(get(credit_col), na.rm = TRUE)),
                       by = .(player_id, player_name)]

  # Full outer join all action type EPVs
  all_players <- unique(data.table::rbindlist(list(
    passing_epv[, .(player_id, player_name)],
    shooting_epv[, .(player_id, player_name)],
    dribbling_epv[, .(player_id, player_name)],
    defending_epv[, .(player_id, player_name)]
  )))
  result <- passing_epv[all_players, on = c("player_id", "player_name")]
  result <- shooting_epv[result, on = c("player_id", "player_name")]
  result <- dribbling_epv[result, on = c("player_id", "player_name")]
  result <- defending_epv[result, on = c("player_id", "player_name")]

  as.data.frame(result)
}


# =============================================================================
# PER-GAME PLAYER EPV
# =============================================================================

#' Aggregate Player EPV Per Game
#'
#' Like \code{\link{aggregate_player_epv}} but groups by \code{(player_id, match_id)}
#' to produce one row per player per match. Includes offensive/defensive
#' decomposition, per-90 rates, and optional position-centering.
#'
#' @param spadl_with_epv SPADL actions with EPV and credit columns from
#'   \code{\link{assign_epv_credit}}.
#' @param lineups Optional lineup data with \code{player_id}, \code{match_id},
#'   \code{minutes_played}, and optionally \code{position}.
#' @param position_center Logical; subtract position-group mean per season
#'   to produce \code{epv_adj} columns. Requires lineups with \code{position}.
#'   Default \code{FALSE}.
#'
#' @return A data.table with one row per player per match:
#'   \describe{
#'     \item{player_id, player_name, team_id, match_id}{Identifiers}
#'     \item{n_actions}{Number of SPADL actions by this player in this match}
#'     \item{epv_total}{Total EPV = actor + receiver + duel_blame}
#'     \item{epv_offensive}{Offensive EPV = passing + shooting + dribbling +
#'       attacking-third aerials + receiver credit}
#'     \item{epv_defensive}{Defensive EPV = defending + keeping + mid/defensive
#'       aerials + duel_blame (keeper handling and defensive headers are
#'       defensive). offensive + defensive == epv_total always.}
#'     \item{epv_as_actor, epv_as_receiver, epv_duel_blame}{Credit source breakdown}
#'     \item{epv_passing}{Outfield passing + ball touches}
#'     \item{epv_shooting}{Shot credit (xG-weighted)}
#'     \item{epv_dribbling}{Ground take-on attempts}
#'     \item{epv_aerial}{Aerial duel credit (winner + / loser via duel_blame)}
#'     \item{epv_keeping}{Goalkeeping: saves, pick-ups, claims, punches}
#'     \item{epv_defending}{Outfield defending: tackles, interceptions,
#'       clearances, ball recoveries, fouls won, dispossessed events}
#'     \item{epv_duel_blame}{Negative credit for losing duels (aerials, take-on
#'       vs tackle). Part of \code{epv_defensive}; exported so that roll-up is
#'       auditable.}
#'     \item{epv_aerial_att}{The attacking-third share of \code{epv_aerial}
#'       (\code{start_x > 67}). \code{epv_defensive} contains
#'       \code{epv_aerial - epv_aerial_att}, so without this column the defensive
#'       roll-up cannot be reconstructed from the exported components.}
#'     \item{minutes_played}{Minutes played (if lineups provided)}
#'     \item{epv_p90, epv_offensive_p90, ...}{Per-90 rates (if lineups provided)}
#'     \item{epv_adj}{Position-centered EPV (if \code{position_center = TRUE})}
#'   }
#'
#' @family epr
#' @export
aggregate_player_game_epv <- function(spadl_with_epv, lineups = NULL,
                                       position_center = FALSE) {
  dt <- data.table::as.data.table(spadl_with_epv)

  # --- Actor credit per player per match ---
  actor_credit <- dt[, .(
    epv_as_actor = sum(player_credit, na.rm = TRUE),
    n_actions = .N
  ), by = .(player_id, player_name, team_id, match_id)]

  # --- Receiver credit per player per match ---
  if ("receiver_player_id" %in% names(dt) && "receiver_credit" %in% names(dt)) {
    receiver_dt <- dt[!is.na(receiver_player_id) & !is.na(receiver_credit)]
    if (nrow(receiver_dt) > 0) {
      receiver_credit <- receiver_dt[, .(
        epv_as_receiver = sum(receiver_credit, na.rm = TRUE)
      ), by = .(receiver_player_id, receiver_player_name, match_id)]
      data.table::setnames(receiver_credit,
                            c("receiver_player_id", "receiver_player_name"),
                            c("player_id", "player_name"))
    } else {
      receiver_credit <- data.table::data.table(
        player_id = character(0), player_name = character(0),
        match_id = character(0), epv_as_receiver = numeric(0))
    }
  } else {
    receiver_credit <- data.table::data.table(
      player_id = character(0), player_name = character(0),
      match_id = character(0), epv_as_receiver = numeric(0))
  }

  # Join actor + receiver
  player_epv <- merge(actor_credit, receiver_credit,
                       by = c("player_id", "player_name", "match_id"),
                       all = TRUE)
  player_epv[is.na(epv_as_actor), epv_as_actor := 0]
  player_epv[is.na(epv_as_receiver), epv_as_receiver := 0]
  player_epv[is.na(n_actions), n_actions := 0L]
  # Fill team_id for receiver-only rows
  if (any(is.na(player_epv$team_id))) {
    pid_team <- unique(dt[, .(player_id, team_id)])
    player_epv[is.na(team_id), team_id := pid_team[.SD, team_id,
                                                     on = "player_id"]]
  }

  # --- Opponent credit (duel blame) per player per match ---
  if ("opponent_player_id" %in% names(dt) && "opponent_credit" %in% names(dt)) {
    opponent_dt <- dt[!is.na(opponent_player_id) & !is.na(opponent_credit)]
    if (nrow(opponent_dt) > 0) {
      opponent_blame <- opponent_dt[, .(
        epv_duel_blame = sum(opponent_credit, na.rm = TRUE)
      ), by = .(opponent_player_id, opponent_player_name, match_id)]
      data.table::setnames(opponent_blame,
                            c("opponent_player_id", "opponent_player_name"),
                            c("player_id", "player_name"))
      player_epv <- merge(player_epv, opponent_blame,
                           by = c("player_id", "player_name", "match_id"),
                           all = TRUE)
      player_epv[is.na(epv_as_actor), epv_as_actor := 0]
      player_epv[is.na(epv_as_receiver), epv_as_receiver := 0]
      player_epv[is.na(epv_duel_blame), epv_duel_blame := 0]
      player_epv[is.na(n_actions), n_actions := 0L]
    } else {
      player_epv[, epv_duel_blame := 0]
    }
  } else {
    player_epv[, epv_duel_blame := 0]
  }

  # --- Total EPV ---
  player_epv[, epv_total := epv_as_actor + epv_as_receiver + epv_duel_blame]

  # --- Action-type decomposition per match ---
  credit_col <- if ("player_credit" %in% names(dt)) "player_credit"
                else if ("epv_delta" %in% names(dt)) "epv_delta"
                else "epv"

  # Action-type buckets. Keeper actions and aerial duels are split into their own
  # components so `epv_passing` means outfield passing and `epv_dribbling` means
  # ground take-ons -- without this split, GKs dominated `epv_passing` and target
  # strikers dominated `epv_dribbling` via aerial wins.
  #
  # `keeper_save` moved from `epv_defending` to `epv_keeping` on 2026-09-02.
  # This is PRESENTATIONAL ONLY: both buckets sit inside the defensive roll-up
  # below, so offensive + defensive == epv_total is unchanged and no player's
  # headline EPV / EPV(ADJ) moves. It was previously bucketed as defending on the
  # grounds that a save suppresses opponent EPV, but that is true of every
  # defensive action and left the two columns misnamed: `epv_keeping` held only
  # handling (pick-up / claim / punch) and so measured almost nothing --
  # calibrated against goal difference it read R^2 = 0.0001 -- while shot-stopping,
  # the substance of goalkeeping, sat under `epv_defending` alongside outfield
  # tackles. After the move each column means what it says: `epv_keeping` is
  # goalkeeping, `epv_defending` is outfield defending.
  action_types <- list(
    epv_passing   = c("pass", "ball_touch"),
    epv_shooting  = "shot",
    epv_dribbling = "take_on",
    epv_aerial    = "aerial",
    epv_keeping   = c("keeper_pick_up", "keeper_claim", "keeper_punch",
                      "keeper_save"),
    epv_defending = c("tackle", "interception", "clearance", "ball_recovery",
                      "foul", "dispossessed")
  )
  for (col_name in names(action_types)) {
    at <- action_types[[col_name]]
    at_dt <- dt[action_type %in% at, .(
      val = sum(get(credit_col), na.rm = TRUE)
    ), by = .(player_id, match_id)]
    data.table::setnames(at_dt, "val", col_name)
    player_epv <- merge(player_epv, at_dt, by = c("player_id", "match_id"),
                         all.x = TRUE)
    player_epv[is.na(get(col_name)), (col_name) := 0]
  }

  # Offensive vs defensive roll-up. The split is presentational only —
  # offensive + defensive == epv_total regardless of bucketing — so re-bucketing
  # never changes a player's headline EPV / EPV(ADJ).
  #   - Keeper handling (pick-up/claim/punch) is DEFENSIVE: it ends opponent
  #     attacks, consistent with keeper_save already being in epv_defending.
  #   - Aerials are SPLIT by pitch location: attacking-third wins (flick-ons,
  #     headers toward goal, start_x > 67) are offensive; mid/defensive-third
  #     clearances are defensive. Unknown-location aerials default to defensive.
  # The displayed epv_aerial / epv_keeping columns stay as TOTALS; only the
  # offensive/defensive roll-up splits them.
  if ("start_x" %in% names(dt)) {
    aerial_att <- dt[action_type == "aerial" & start_x > 67, .(
      epv_aerial_att = sum(get(credit_col), na.rm = TRUE)
    ), by = .(player_id, match_id)]
    player_epv <- merge(player_epv, aerial_att, by = c("player_id", "match_id"),
                         all.x = TRUE)
    player_epv[is.na(epv_aerial_att), epv_aerial_att := 0]
  } else {
    # No location available -> all aerials default to defensive (epv_aerial_att=0).
    player_epv[, epv_aerial_att := 0]
  }

  player_epv[, `:=`(
    epv_offensive = epv_passing + epv_shooting + epv_dribbling +
                    epv_aerial_att + epv_as_receiver,
    epv_defensive = epv_defending + epv_keeping +
                    (epv_aerial - epv_aerial_att) + epv_duel_blame
  )]
  ## epv_aerial_att is KEPT (was dropped as an "internal split helper" until
  ## 2026-09-02). Without it the defensive roll-up cannot be reconstructed from
  ## the exported components: `epv_aerial` ships as a TOTAL while epv_defensive
  ## contains only `epv_aerial - epv_aerial_att`. That gap made epv_defensive
  ## un-auditable from published data and caused a component of an inversion
  ## (panna#228) to be attributed to the wrong term -- the aerial total was
  ## calibrated when the defensive half was the quantity of interest.

  # --- Join lineups for minutes and per-90 ---
  if (!is.null(lineups) && "minutes_played" %in% names(lineups)) {
    dt_lineups <- data.table::as.data.table(lineups)
    key_cols <- intersect(c("player_id", "match_id"), names(dt_lineups))
    if (length(key_cols) == 2) {
      mins <- dt_lineups[, .(minutes_played = sum(minutes_played, na.rm = TRUE)),
                          by = .(player_id, match_id)]
      # Carry position if available
      if ("position" %in% names(dt_lineups)) {
        pos <- dt_lineups[, .(position = position[1]), by = .(player_id, match_id)]
        mins <- merge(mins, pos, by = c("player_id", "match_id"), all.x = TRUE)
      }
      player_epv <- merge(player_epv, mins, by = c("player_id", "match_id"),
                           all.x = TRUE)

      # Per-90 rates
      epv_cols <- grep("^epv_", names(player_epv), value = TRUE)
      epv_cols <- setdiff(epv_cols, grep("_p90$|_adj$", epv_cols, value = TRUE))
      mins_safe <- pmax(player_epv$minutes_played, 1, na.rm = TRUE)
      for (col in epv_cols) {
        p90_col <- paste0(col, "_p90")
        data.table::set(player_epv, j = p90_col,
                         value = player_epv[[col]] / (mins_safe / 90))
      }
    }
  }

  # --- Position centering ---
  if (isTRUE(position_center) && "position" %in% names(player_epv)) {
    # Map to broad groups
    player_epv[, pos_group := data.table::fcase(
      grepl("GK|Goalkeeper", position, ignore.case = TRUE), "GK",
      grepl("DEF|Back|CB|LB|RB|WB", position, ignore.case = TRUE), "DEF",
      grepl("MID|CM|DM|AM|Wing", position, ignore.case = TRUE), "MID",
      grepl("FWD|Forward|Striker|CF|ST", position, ignore.case = TRUE), "FWD",
      default = "MID"
    )]
    adj_cols <- c("epv_total", "epv_offensive", "epv_defensive")
    for (col in adj_cols) {
      adj_name <- sub("^epv_", "epv_", paste0(col, "_adj"))
      if (col == "epv_total") adj_name <- "epv_adj"
      else adj_name <- paste0(col, "_adj")
      player_epv[, (adj_name) := get(col) - mean(get(col), na.rm = TRUE),
                  by = pos_group]
    }
    player_epv[, pos_group := NULL]
  }

  # Fill remaining NAs in numeric columns
  num_cols <- names(player_epv)[vapply(player_epv, is.numeric, logical(1))]
  for (col in num_cols) {
    data.table::set(player_epv, which(is.na(player_epv[[col]])), col, 0)
  }

  data.table::setorder(player_epv, match_id, -epv_total)
  player_epv[]
}


# =============================================================================
# MODEL PERSISTENCE
# =============================================================================

#' Save EPV Model
#'
#' Saves trained EPV model to disk.
#'
#' @param epv_model EPV model from fit_epv_model()
#' @param path Directory to save model. If NULL, uses pannadata/data/opta/models/
#'
#' @return Invisibly returns path
#' @family epv
#' @export
save_epv_model <- function(epv_model, path = NULL) {
  if (is.null(path)) {
    path <- file.path(opta_data_dir(), "models")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  model_path <- file.path(path, "epv_model.rds")
  saveRDS(epv_model, model_path)

  cli::cli_alert_success("Saved EPV model to {model_path}")

  invisible(model_path)
}


#' Report which model file was loaded, with date + staleness warning
#'
#' The model-loader fallback chains (explicit path → pannamodels → local) used to
#' announce only the source, not the file DATE — so a silent fallback to a stale
#' model (the 2026-06-21 inflated-EPV incident) looked identical to a correct
#' load in the logs. This always prints the resolved file's modification date and
#' WARNS if it's older than \code{max_age_days}, so a stale model is visible.
#'
#' @param model_label e.g. "EPV", "WP".
#' @param model_path Resolved file path, or NULL for package-provided models.
#' @param source Human label for where it came from.
#' @param max_age_days Warn above this age (default 14).
#' @keywords internal
.report_model_provenance <- function(model_label, model_path, source,
                                     max_age_days = 14) {
  if (is.null(model_path) || !file.exists(model_path)) {
    cli::cli_alert_success(
      "Loaded {model_label} model from {source} (date unknown -- pass an explicit path/override to pin the version)")
    return(invisible())
  }
  mtime <- file.info(model_path)$mtime
  age <- as.numeric(difftime(Sys.time(), mtime, units = "days"))
  line <- sprintf("Loaded %s model from %s [%s, modified %s, %.0f days old]",
                  model_label, basename(model_path), source,
                  format(mtime, "%Y-%m-%d"), age)
  if (isTRUE(age > max_age_days)) {
    cli::cli_warn(c(
      line,
      "!" = paste("This model is >{round(max_age_days)} days old — confirm it's the",
                  "intended (latest) version. For game-logs, pass",
                  "{.code epv_model_override}/{.code wp_model_override} to pin the",
                  "post-overhaul models (see MODELS.md).")
    ))
  } else {
    cli::cli_alert_success(line)
  }
  invisible()
}

#' Load EPV Model
#'
#' Loads a pre-trained EPV model, trying an explicit path first, then the
#' \code{pannamodels} package, then falling back to the local pannadata models
#' directory. Reports the resolved source and file date via
#' \code{\link{.report_model_provenance}}.
#'
#' @param path Directory containing model. If NULL, uses pannadata/data/opta/models/
#'
#' @return EPV model
#' @family epv
#' @export
load_epv_model <- function(path = NULL) {
  # Try explicit path first
  if (!is.null(path)) {
    model_path <- file.path(path, "epv_model.rds")
    if (file.exists(model_path)) {
      .report_model_provenance("EPV", model_path, "explicit path")
      return(readRDS(model_path))
    }
  }

  # Try pannamodels package (preferred)
  if (requireNamespace("pannamodels", quietly = TRUE)) {
    model <- tryCatch(
      pannamodels::load_panna_model("epv_model", verbose = FALSE),
      error = function(e) {
        cli::cli_alert_info("pannamodels failed: {e$message}. Trying local path.")
        NULL
      }
    )
    if (!is.null(model)) {
      .report_model_provenance("EPV", NULL, "pannamodels package")
      return(model)
    }
  }

  # Fall back to local pannadata path
  default_path <- file.path(opta_data_dir(), "models", "epv_model.rds")
  if (file.exists(default_path)) {
    .report_model_provenance("EPV", default_path, "local pannadata fallback")
    return(readRDS(default_path))
  }

  cli::cli_abort(c(
    "EPV model not found.",
    "i" = "Install pannamodels: devtools::install_github('peteowen1/pannamodels')",
    "i" = "Or download with pb_download_epv_models()"
  ))
}


#' Download EPV Models from GitHub Releases
#'
#' Downloads pre-trained EPV models from GitHub releases.
#'
#' @param repo GitHub repository (default: peteowen1/pannadata)
#' @param tag Release tag (default: epv-models)
#' @param dest Destination directory. If NULL, uses pannadata/data/opta/models/
#'
#' @return Invisibly returns path to models
#' @family epv
#' @export
pb_download_epv_models <- function(repo = "peteowen1/pannadata",
                                    tag = "epv-models",
                                    dest = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("piggyback package required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- file.path(opta_data_dir(), "models")
  }
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)

  cli::cli_alert_info("Downloading EPV models from {repo} ({tag})...")

  model_files <- c(
    "xg_model.rds",
    "xpass_model.rds",
    "epv_model.rds"
  )

  failed <- character(0)
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
      failed <<- c(failed, f)
      cli::cli_warn("Failed to download {f}: {e$message}")
    })
  }

  if (length(failed) > 0) {
    cli::cli_warn("{length(failed)}/{length(model_files)} model files failed to download: {paste(failed, collapse = ', ')}")
  } else {
    cli::cli_alert_success("EPV models downloaded to {dest}")
  }

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
#' @keywords internal
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
