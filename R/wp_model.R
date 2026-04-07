# Win Probability Model for Soccer
# =================================
# Predicts P(home_win), P(draw), P(away_win) from in-match game state.
# Draw is valued at 0.5, so WPA measures contribution toward match points.
#
# WP = P(home_win) + 0.5 * P(draw)   (from home team's perspective)
#
# This follows the torpverse pattern (R/add_variables.R) adapted for soccer's
# 3-outcome structure and low-scoring nature.


# ============================================================================
# Feature engineering
# ============================================================================

#' Create win probability features from SPADL actions
#'
#' Builds the game-state feature set at each action for WP model training
#' and prediction. Features capture score state, expected goals state,
#' time remaining, and team strength indicators.
#'
#' @param spadl_with_epv SPADL actions with EPV. Must contain: \code{match_id},
#'   \code{team_id}, \code{time_seconds}, \code{period_id}.
#' @param match_results Data.frame with \code{match_id}, \code{home_team_id},
#'   \code{away_team_id}, \code{home_goals}, \code{away_goals} for training
#'   labels. If NULL, labels are not added (prediction mode).
#' @param home_teams Optional data.frame with \code{match_id}, \code{home_team_id}
#'   to determine home/away. If NULL, derived from match_results.
#'
#' @return A data.table with one row per action, containing WP features
#'   and optionally training labels.
#'
#' @export
create_wp_features <- function(spadl_with_epv, match_results = NULL,
                                home_teams = NULL) {
  dt <- data.table::as.data.table(spadl_with_epv)
  data.table::setorder(dt, match_id, period_id, time_seconds)

  # Determine home team per match

  if (!is.null(home_teams)) {
    ht <- data.table::as.data.table(home_teams)
  } else if (!is.null(match_results)) {
    ht <- data.table::as.data.table(match_results)[, .(match_id, home_team_id)]
  } else {
    cli::cli_abort("Either {.arg match_results} or {.arg home_teams} must be provided")
  }
  dt[ht, home_team_id := i.home_team_id, on = "match_id"]

  # Determine is_home for each action
  dt[, is_home := as.integer(team_id == home_team_id)]

  # --- Time features ---
  # Total match seconds: 90 min = 5400 sec (we cap at this)
  dt[, time_remaining := pmax(0, (5400 - time_seconds)) / 5400]
  dt[, time_elapsed_frac := pmin(time_seconds / 5400, 1)]

  # --- Score state ---
  # Detect goals: action_type == "shot" & result == "success" in SPADL
  dt[, is_goal := as.integer(action_type == "shot" & result == "success")]

  # Cumulative goals per team per match
  dt[, home_goal := as.integer(is_goal == 1L & is_home == 1L)]
  dt[, away_goal := as.integer(is_goal == 1L & is_home == 0L)]
  dt[, cum_home_goals := cumsum(home_goal) - home_goal, by = match_id]
  dt[, cum_away_goals := cumsum(away_goal) - away_goal, by = match_id]
  dt[, score_diff := cum_home_goals - cum_away_goals]  # from home perspective

  # --- xG state ---
  # Cumulative xG differential (if xg column available)
  if ("xg" %in% names(dt)) {
    dt[, home_xg_action := data.table::fifelse(is_home == 1L & action_type == "shot",
                                                xg, 0)]
    dt[, away_xg_action := data.table::fifelse(is_home == 0L & action_type == "shot",
                                                xg, 0)]
    dt[, cum_home_xg := cumsum(home_xg_action), by = match_id]
    dt[, cum_away_xg := cumsum(away_xg_action), by = match_id]
    dt[, xg_diff := cum_home_xg - cum_away_xg]
  } else {
    cli::cli_warn("xg column not found — WP model will operate without xG differential")
    dt[, xg_diff := 0]
  }

  # --- Red card state (if available) ---
  if ("red_card" %in% names(dt)) {
    dt[, home_red := as.integer(red_card == 1L & is_home == 1L)]
    dt[, away_red := as.integer(red_card == 1L & is_home == 0L)]
    dt[, cum_home_reds := cumsum(home_red), by = match_id]
    dt[, cum_away_reds := cumsum(away_red), by = match_id]
    dt[, red_card_diff := cum_home_reds - cum_away_reds]
  } else {
    dt[, red_card_diff := 0L]
  }

  # --- Period ---
  dt[, is_second_half := as.integer(period_id == 2L)]

  # --- Add training labels ---
  if (!is.null(match_results)) {
    mr <- data.table::as.data.table(match_results)
    # Outcome: 1 = home win, 0.5 = draw, 0 = away win
    mr[, wp_label := data.table::fcase(
      home_goals > away_goals, 1,
      home_goals == away_goals, WP_DRAW_VALUE,
      home_goals < away_goals, 0
    )]
    dt[mr, wp_label := i.wp_label, on = "match_id"]
  }

  # Select feature columns
  feature_cols <- c("match_id", "team_id", "player_id", "player_name",
                     "action_type", "time_seconds", "period_id",
                     "time_remaining", "time_elapsed_frac",
                     "score_diff", "xg_diff", "red_card_diff",
                     "is_home", "is_second_half", "is_goal")
  if ("wp_label" %in% names(dt)) feature_cols <- c(feature_cols, "wp_label")

  # Keep additional columns needed for WPA computation
  keep_cols <- intersect(
    c(feature_cols, "result", "home_team_id",
      "receiver_player_id", "receiver_player_name",
      "player_credit", "epv_delta"),
    names(dt)
  )

  dt[, ..keep_cols]
}


#' Train a win probability model
#'
#' Fits an XGBoost model to predict match outcome (home win / draw / away win)
#' from in-match game state features. Uses \code{WP_DRAW_VALUE} (0.5) for draws
#' so the model predicts "expected points fraction" for the home team.
#'
#' @param wp_features Output of \code{\link{create_wp_features}} with
#'   \code{wp_label} column.
#' @param nrounds Number of XGBoost boosting rounds (default 200).
#' @param max_depth Maximum tree depth (default 4).
#' @param eta Learning rate (default 0.05).
#' @param ... Additional parameters passed to \code{xgboost::xgb.train()}.
#'
#' @return A list with:
#'   \describe{
#'     \item{model}{Trained xgboost model object}
#'     \item{feature_names}{Character vector of feature column names}
#'   }
#'
#' @export
train_wp_model <- function(wp_features, nrounds = 200L, max_depth = 4L,
                            eta = 0.05, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} required for WP model training")
  }

  dt <- data.table::as.data.table(wp_features)

  if (!"wp_label" %in% names(dt)) {
    cli::cli_abort("wp_features must contain {.val wp_label} column (from create_wp_features with match_results)")
  }

  feature_names <- c("time_remaining", "score_diff", "xg_diff",
                      "red_card_diff", "is_home", "is_second_half")
  feature_names <- intersect(feature_names, names(dt))

  mat <- as.matrix(dt[, ..feature_names])
  label <- dt$wp_label

  # Remove rows with NA labels
  valid <- !is.na(label)
  mat <- mat[valid, , drop = FALSE]
  label <- label[valid]

  dtrain <- xgboost::xgb.DMatrix(data = mat, label = label)

  params <- list(
    objective = "reg:squarederror",
    max_depth = max_depth,
    eta = eta,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 50,
    ...
  )

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    verbose = 0
  )

  cli::cli_alert_success("Trained WP model on {sum(valid)} actions ({length(unique(dt$match_id[valid]))} matches)")

  list(model = model, feature_names = feature_names)
}


#' Predict win probability
#'
#' Scores each action's game state with win probability using a trained model.
#'
#' @param wp_model Output of \code{\link{train_wp_model}}.
#' @param wp_features SPADL features from \code{\link{create_wp_features}}.
#'
#' @return Numeric vector of win probabilities (home team perspective),
#'   same length as \code{nrow(wp_features)}.
#'
#' @export
predict_wp <- function(wp_model, wp_features) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} required for WP prediction")
  }

  dt <- data.table::as.data.table(wp_features)
  feature_names <- wp_model$feature_names
  mat <- as.matrix(dt[, ..feature_names])

  preds <- predict(wp_model$model, mat)

  # Clamp to [0, 1]
  pmax(pmin(preds, 1), 0)
}


#' Add win probability and WPA to SPADL data
#'
#' Adds \code{wp} (win probability at each action from home perspective) and
#' \code{wpa} (win probability added by each action, sign-adjusted for the
#' acting team) columns.
#'
#' WPA is computed as the change in win probability caused by each action.
#' For home team actions: \code{wpa = wp_after - wp_before}.
#' For away team actions: \code{wpa = (1 - wp_after) - (1 - wp_before) = wp_before - wp_after}.
#'
#' @param wp_features SPADL features with WP model features.
#' @param wp_model Trained WP model from \code{\link{train_wp_model}}.
#'
#' @return The input data.table with added \code{wp} and \code{wpa} columns.
#'
#' @export
add_wp_vars <- function(wp_features, wp_model) {
  dt <- data.table::as.data.table(wp_features)

  # Predict WP at each action (home team perspective)
  dt[, wp := predict_wp(wp_model, dt)]

  # WPA = change in WP from this action to the next
  dt[, wp_next := data.table::shift(wp, type = "lead"), by = match_id]

  # Last action in match: WPA based on final outcome
  if ("wp_label" %in% names(dt)) {
    dt[is.na(wp_next), wp_next := wp_label]
  } else {
    dt[is.na(wp_next), wp_next := wp]  # no change for last action
  }

  # WPA from home team perspective
  dt[, wpa_home := wp_next - wp]

  # Adjust sign for acting team: home team gets positive, away gets negative
  dt[, wpa := data.table::fifelse(is_home == 1L, wpa_home, -wpa_home)]

  # Center WPA per match so it sums to zero (removes WP model calibration bias)
  dt[, wpa := wpa - mean(wpa, na.rm = TRUE), by = match_id]

  # Clean up
  dt[, c("wp_next", "wpa_home") := NULL]

  dt
}


# ============================================================================
# Model persistence
# ============================================================================

#' Save WP model
#'
#' @param wp_model WP model from \code{\link{train_wp_model}}.
#' @param path Directory to save. If NULL, uses \code{pannadata/data/opta/models/}.
#'
#' @return Invisibly returns the file path.
#' @export
save_wp_model <- function(wp_model, path = NULL) {
  if (is.null(path)) {
    path <- file.path(opta_data_dir(), "models")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  model_path <- file.path(path, "wp_model.rds")
  saveRDS(wp_model, model_path)

  cli::cli_alert_success("Saved WP model to {model_path}")
  invisible(model_path)
}


#' Load WP model
#'
#' @param path Directory to load from. If NULL, uses \code{pannadata/data/opta/models/}.
#'
#' @return WP model list (model + feature_names).
#' @export
load_wp_model <- function(path = NULL) {
  if (is.null(path)) {
    path <- file.path(opta_data_dir(), "models")
  }

  model_path <- file.path(path, "wp_model.rds")
  if (!file.exists(model_path)) {
    cli::cli_abort(c(
      "WP model not found at {.file {model_path}}",
      "i" = "Run {.file data-raw/epv/05_train_wp_model.R} to train the model."
    ))
  }

  readRDS(model_path)
}
