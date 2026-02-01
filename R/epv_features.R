# EPV Feature Engineering (Optimized with data.table)
#
# Creates game state features for EPV model training and prediction.
# Features capture location, action sequences, and match context.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT setorder shift .SD .N :=
NULL

# =============================================================================
# Shared Feature Constants (Single source of truth)
# =============================================================================

# Base location features
EPV_LOCATION_FEATURES <- c(
  "x", "y", "distance_to_goal", "angle_to_goal", "zone_id",
  "in_penalty_area", "in_final_third", "in_own_third", "in_mid_third",
  "y_left", "y_center", "y_right"
)

# Movement features
EPV_MOVEMENT_FEATURES <- c("dx", "dy", "move_distance", "dist_delta")

# Action type base features
EPV_ACTION_BASE_FEATURES <- c("result_success", "is_foot", "is_head")

# Action types for one-hot encoding
EPV_ACTION_TYPES <- c("pass", "shot", "take_on", "tackle", "interception",
                      "clearance", "aerial", "foul", "ball_recovery")

# Action types included in sequence features
EPV_SEQUENCE_ACTION_TYPES <- c("pass", "shot", "take_on", "tackle", "interception")

# Chain/context features
EPV_CHAIN_FEATURES <- c("seconds_since_chain_start", "action_in_chain")
EPV_CONTEXT_FEATURES <- c("time_normalized", "period_id")


#' Create Game State Features for EPV
#'
#' Builds comprehensive features for each action including location,
#' sequence context, and match situation. Optimized with data.table.
#'
#' @param spadl_actions SPADL actions with chain assignments
#' @param n_prev Number of previous actions to include (default 3)
#'
#' @return Data frame with EPV features
#'
#' @export
create_epv_features <- function(spadl_actions, n_prev = 3) {

  if (is.null(spadl_actions) || nrow(spadl_actions) == 0) {
    cli::cli_abort("No SPADL actions provided for feature creation")
  }

 cli::cli_alert_info("Creating EPV features for {format(nrow(spadl_actions), big.mark=',')} actions...")

  # Convert to data.table for performance
  dt <- data.table::as.data.table(spadl_actions)

  # Sort by match, period, time, action_id
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # =========================================================================
  # LOCATION FEATURES (vectorized)
  # =========================================================================
  dt[, `:=`(
    x = start_x,
    y = start_y,
    distance_to_goal = sqrt((100 - start_x)^2 + (50 - start_y)^2),
    # Angle to goal: visible angle between goal posts (y=44 and y=56)
    # atan2(dy, dx) where dy = post_y - player_y, dx = distance to goal line
    angle_to_goal = abs(atan2((50 + 6) - start_y, pmax(100 - start_x, 0.1)) -
                        atan2((50 - 6) - start_y, pmax(100 - start_x, 0.1))),
    zone_id = (pmin(pmax(floor(start_x / 33.34), 0), 2)) * 3 +
              pmin(pmax(floor(start_y / 33.34), 0), 2) + 1,
    in_penalty_area = as.integer(start_x > 83 & start_y > 21 & start_y < 79),
    in_final_third = as.integer(start_x > 67),
    in_own_third = as.integer(start_x < 33),
    in_mid_third = as.integer(start_x >= 33 & start_x < 67),
    y_left = as.integer(start_y < 33),
    y_center = as.integer(start_y >= 33 & start_y < 67),
    y_right = as.integer(start_y >= 67)
  )]

  # =========================================================================
  # ACTION TYPE FEATURES (vectorized)
  # =========================================================================
  dt[, `:=`(
    result_success = as.integer(result == "success"),
    is_foot = as.integer(bodypart == "foot"),
    is_head = as.integer(bodypart == "head"),
    is_pass = as.integer(action_type == "pass"),
    is_shot = as.integer(action_type == "shot"),
    is_take_on = as.integer(action_type == "take_on"),
    is_tackle = as.integer(action_type == "tackle"),
    is_interception = as.integer(action_type == "interception"),
    is_clearance = as.integer(action_type == "clearance"),
    is_aerial = as.integer(action_type == "aerial"),
    is_foul = as.integer(action_type == "foul"),
    is_ball_recovery = as.integer(action_type == "ball_recovery")
  )]

  # =========================================================================
  # MOVEMENT FEATURES (vectorized)
  # =========================================================================
  dt[, `:=`(
    dx = end_x - start_x,
    dy = end_y - start_y,
    move_distance = sqrt((end_x - start_x)^2 + (end_y - start_y)^2),
    dist_delta = sqrt((100 - end_x)^2 + (50 - end_y)^2) -
                 sqrt((100 - start_x)^2 + (50 - start_y)^2)
  )]

  # =========================================================================
  # SEQUENCE FEATURES (optimized with data.table shift)
  # =========================================================================
  cli::cli_alert_info("Adding sequence features (n_prev={n_prev})...")

  # Action type to numeric for lagging
  action_type_map <- c(
    "pass" = 1, "shot" = 2, "take_on" = 3, "tackle" = 4,
    "interception" = 5, "clearance" = 6, "aerial" = 7,
    "foul" = 8, "ball_recovery" = 9, "other" = 0
  )
  dt[, action_type_num := action_type_map[action_type]]
  dt[is.na(action_type_num), action_type_num := 0]

  # Create lagged features within each match using data.table shift
  for (lag in 1:n_prev) {
    suffix <- paste0("_prev", lag)

    dt[, (paste0("result", suffix)) := shift(result_success, lag, type = "lag"), by = match_id]
    dt[, (paste0("dx", suffix)) := shift(dx, lag, type = "lag"), by = match_id]
    dt[, (paste0("dy", suffix)) := shift(dy, lag, type = "lag"), by = match_id]
    dt[, (paste0("action_type_num", suffix)) := shift(action_type_num, lag, type = "lag"), by = match_id]

    # Same team indicator
    dt[, (paste0("team_id_prev", lag)) := shift(team_id, lag, type = "lag"), by = match_id]
    dt[, (paste0("same_team", suffix)) := as.integer(team_id == get(paste0("team_id_prev", lag)))]
    dt[, (paste0("team_id_prev", lag)) := NULL]

    # One-hot encode previous action types (using shared constant)
    for (atype in EPV_SEQUENCE_ACTION_TYPES) {
      type_num <- action_type_map[atype]
      dt[, (paste0("is_", atype, suffix)) := as.integer(get(paste0("action_type_num", suffix)) == type_num)]
    }
  }

  # =========================================================================
  # CHAIN/POSSESSION FEATURES
  # =========================================================================
  if ("chain_id" %in% names(dt)) {
    if ("chain_start_time" %in% names(dt)) {
      dt[, seconds_since_chain_start := time_seconds - chain_start_time]
    }
  }

  # =========================================================================
  # CONTEXT FEATURES
  # =========================================================================
  dt[, `:=`(
    time_normalized = pmin(time_seconds / (45 * 60), 1)
  )]

  # =========================================================================
  # CLEANUP
  # =========================================================================
  # Remove temporary columns
  dt[, action_type_num := NULL]
  for (lag in 1:n_prev) {
    dt[, (paste0("action_type_num_prev", lag)) := NULL]
  }

  # Replace NAs with 0 for numeric columns
  numeric_cols <- names(dt)[sapply(dt, is.numeric)]
  for (col in numeric_cols) {
    data.table::set(dt, which(is.na(dt[[col]])), col, 0)
  }

  cli::cli_alert_success("Created {ncol(dt)} features for {nrow(dt)} actions")

  # Convert back to data.frame for compatibility
  as.data.frame(dt)
}


#' Create Location-Only Features
#'
#' Creates a minimal feature set based only on location.
#'
#' @param x X coordinates
#' @param y Y coordinates
#'
#' @return Data frame with location features
#' @export
create_location_features <- function(x, y) {
  data.frame(
    x = x,
    y = y,
    distance_to_goal = sqrt((100 - x)^2 + (50 - y)^2),
    # Angle to goal: visible angle between goal posts (y=44 and y=56)
    angle_to_goal = abs(atan2(56 - y, pmax(100 - x, 0.1)) - atan2(44 - y, pmax(100 - x, 0.1))),
    zone_id = (pmin(pmax(floor(x / 33.34), 0), 2)) * 3 +
              pmin(pmax(floor(y / 33.34), 0), 2) + 1,
    in_penalty_area = as.integer(x > 83 & y > 21 & y < 79),
    in_final_third = as.integer(x > 67),
    in_own_third = as.integer(x < 33),
    in_mid_third = as.integer(x >= 33 & x < 67),
    y_left = as.integer(y < 33),
    y_center = as.integer(y >= 33 & y < 67),
    y_right = as.integer(y >= 67),
    stringsAsFactors = FALSE
  )
}


#' Get EPV Model Feature Columns
#'
#' Returns the list of feature columns used for EPV model training.
#' Uses shared constants to ensure consistency with create_epv_features().
#'
#' @param include_sequence Whether to include sequence features (default TRUE)
#' @param n_prev Number of previous actions for sequence features (default 3)
#'
#' @return Character vector of feature column names
#' @export
get_epv_feature_cols <- function(include_sequence = TRUE, n_prev = 3) {
  # Build from shared constants (single source of truth)
  cols <- c(
    EPV_LOCATION_FEATURES,
    EPV_MOVEMENT_FEATURES,
    EPV_ACTION_BASE_FEATURES,
    paste0("is_", EPV_ACTION_TYPES),
    EPV_CHAIN_FEATURES,
    EPV_CONTEXT_FEATURES
  )

  # Sequence features
  if (include_sequence) {
    for (lag in 1:n_prev) {
      suffix <- paste0("_prev", lag)
      cols <- c(cols, paste0("result", suffix))
      cols <- c(cols, paste0("dx", suffix))
      cols <- c(cols, paste0("dy", suffix))
      cols <- c(cols, paste0("same_team", suffix))
      cols <- c(cols, paste0("is_", EPV_SEQUENCE_ACTION_TYPES, suffix))
    }
  }

  cols
}


#' Create EPV Training Labels
#'
#' Creates target labels for EPV model training from chain outcomes.
#'
#' @param spadl_with_outcomes SPADL actions with chain outcome labels
#'
#' @return Data frame with action_id and target columns
#'
#' @export
create_epv_labels <- function(spadl_with_outcomes) {
  labels <- data.frame(
    match_id = spadl_with_outcomes$match_id,
    action_id = spadl_with_outcomes$action_id,
    stringsAsFactors = FALSE
  )

  if ("chain_ends_in_goal" %in% names(spadl_with_outcomes)) {
    labels$scores_this_possession <- spadl_with_outcomes$chain_ends_in_goal
  } else {
    cli::cli_warn("chain_ends_in_goal not found, setting to 0")
    labels$scores_this_possession <- 0L
  }

  if ("opponent_scores_next" %in% names(spadl_with_outcomes)) {
    labels$concedes_next_possession <- spadl_with_outcomes$opponent_scores_next
  } else {
    labels$concedes_next_possession <- 0L
  }

  labels
}


#' Calculate Possession Velocity
#'
#' Computes the speed of ball progression in a possession chain.
#' Optimized with data.table.
#'
#' @param spadl_with_chains SPADL actions with chain information
#'
#' @return SPADL actions with velocity features added
#' @export
add_possession_velocity <- function(spadl_with_chains) {
  if (!"chain_id" %in% names(spadl_with_chains)) {
    cli::cli_warn("No chain_id found, skipping velocity calculation")
    return(spadl_with_chains)
  }

  dt <- data.table::as.data.table(spadl_with_chains)

  # Calculate chain-level stats efficiently
  chain_stats <- dt[, .(
    x_first = start_x[1],
    x_last = start_x[.N],
    time_first = time_seconds[1],
    time_last = time_seconds[.N]
  ), by = .(match_id, chain_id)]

  chain_stats[, `:=`(
    x_progression = x_last - x_first,
    chain_duration = time_last - time_first
  )]

  chain_stats[, possession_velocity := fifelse(
    chain_duration > 0,
    x_progression / chain_duration,
    0
  )]

  # Merge back
  dt <- merge(dt, chain_stats[, .(match_id, chain_id, possession_velocity, x_progression, chain_duration)],
              by = c("match_id", "chain_id"), all.x = TRUE, sort = FALSE)

  # Restore original order
  data.table::setorder(dt, match_id, action_id)

  as.data.frame(dt)
}


#' Normalize Features for Model
#'
#' Applies standardization to numeric features.
#'
#' @param features Feature data frame
#' @param feature_cols Columns to normalize
#' @param means Optional pre-computed means
#' @param sds Optional pre-computed standard deviations
#'
#' @return List with normalized features, means, and sds
#' @export
normalize_epv_features <- function(features, feature_cols = NULL, means = NULL, sds = NULL) {
  if (is.null(feature_cols)) {
    feature_cols <- names(features)[sapply(features, is.numeric)]
    feature_cols <- setdiff(feature_cols, c("match_id", "action_id", "team_id", "player_id", "chain_id"))
  }

  available_cols <- intersect(feature_cols, names(features))

  if (is.null(means)) {
    means <- sapply(features[, available_cols, drop = FALSE], mean, na.rm = TRUE)
  }
  if (is.null(sds)) {
    sds <- sapply(features[, available_cols, drop = FALSE], stats::sd, na.rm = TRUE)
    sds[sds == 0] <- 1
  }

  # Vectorized standardization
  dt <- data.table::as.data.table(features)
  for (col in available_cols) {
    if (col %in% names(means) && col %in% names(sds)) {
      data.table::set(dt, j = col, value = (dt[[col]] - means[col]) / sds[col])
    }
  }

  list(
    features = as.data.frame(dt),
    means = means,
    sds = sds,
    feature_cols = available_cols
  )
}
