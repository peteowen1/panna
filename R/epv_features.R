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

# How many preceding actions the sequence features look back over. Single
# source of truth: create_epv_features() builds them and get_epv_feature_cols()
# names them, and the two MUST agree or the model asks for a feature set the
# builder never produced. Previously both hardcoded 3 independently.
EPV_N_PREV <- 3L

#' Validate an n_prev argument (internal helper)
#'
#' Guards the sequence-feature lookback. The dangerous value is 0: the loops
#' below used \code{1:n_prev}, and \code{1:0} is \code{c(1, 0)}, so asking for
#' "no sequence features" instead produced a \code{_prev0} column set where
#' \code{shift(x, 0)} is the identity -- i.e. the CURRENT action's outcome
#' leaking in as a "previous action" feature. Target leakage, silently.
#' @keywords internal
.check_n_prev <- function(n_prev) {
  if (!is.numeric(n_prev) || length(n_prev) != 1L || is.na(n_prev) ||
      n_prev < 0 || n_prev != as.integer(n_prev)) {
    cli::cli_abort(c(
      "{.arg n_prev} must be a single non-negative whole number.",
      "x" = "Got {.val {n_prev}}."
    ))
  }
  invisible(as.integer(n_prev))
}

# Chain/context features
EPV_CHAIN_FEATURES <- c("seconds_since_chain_start", "action_in_chain")
EPV_CONTEXT_FEATURES <- c("time_normalized", "period_id")


#' Create Game State Features for EPV
#'
#' Builds comprehensive features for each action including location,
#' sequence context, and match situation. Optimized with data.table.
#'
#' @param spadl_actions SPADL actions with chain assignments
#' @param n_prev Number of previous actions to include (default
#'   \code{EPV_N_PREV}). Must be a non-negative whole number.
#'
#' @return Data frame with EPV features
#'
#' @keywords internal
create_epv_features <- function(spadl_actions, n_prev = EPV_N_PREV) {

  if (is.null(spadl_actions) || nrow(spadl_actions) == 0) {
    cli::cli_abort("No SPADL actions provided for feature creation")
  }
  .check_n_prev(n_prev)

 cli::cli_alert_info("Creating EPV features for {format(nrow(spadl_actions), big.mark=',')} actions...")

  # Convert to data.table for performance
  dt <- data.table::as.data.table(spadl_actions)

  # Note: Aerial actions are handled as stationary duel actions in
  # convert_opta_to_spadl() (end coordinates set to start coordinates).

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
  # SEQUENCE FEATURES (optimized with data.table shift - batch operations)
  # =========================================================================
  cli::cli_alert_info("Adding sequence features (n_prev={n_prev})...")

  # Action type to numeric for lagging
  action_type_map <- c(
    "pass" = 1L, "shot" = 2L, "take_on" = 3L, "tackle" = 4L,
    "interception" = 5L, "clearance" = 6L, "aerial" = 7L,
    "foul" = 8L, "ball_recovery" = 9L, "other" = 0L
  )
  dt[, action_type_num := action_type_map[action_type]]
  dt[is.na(action_type_num), action_type_num := 0L]

  # Pre-compute all lagged team_ids at once for same_team calculation
  for (lag in seq_len(n_prev)) {
    dt[, (paste0("team_id_prev", lag)) := shift(team_id, lag, type = "lag"), by = match_id]
  }

  # Create all lagged features in one grouped operation per match
  # This is much faster than multiple separate shift calls
  lag_cols_base <- c("result_success", "dx", "dy", "action_type_num")

  for (lag in seq_len(n_prev)) {
    suffix <- paste0("_prev", lag)
    new_cols <- paste0(lag_cols_base, suffix)

    # Single shift operation for all base columns at this lag
    dt[, (new_cols) := lapply(.SD, function(x) shift(x, lag, type = "lag")),
       by = match_id, .SDcols = lag_cols_base]

    # Same team indicator (vectorized)
    prev_team_col <- paste0("team_id_prev", lag)
    dt[, (paste0("same_team", suffix)) := as.integer(team_id == get(prev_team_col))]
  }

  # One-hot encode previous action types (vectorized for all lags and types)
  atype_nums <- action_type_map[EPV_SEQUENCE_ACTION_TYPES]
  for (lag in seq_len(n_prev)) {
    suffix <- paste0("_prev", lag)
    action_num_col <- paste0("action_type_num", suffix)
    action_nums <- dt[[action_num_col]]

    for (i in seq_along(EPV_SEQUENCE_ACTION_TYPES)) {
      atype <- EPV_SEQUENCE_ACTION_TYPES[i]
      dt[, (paste0("is_", atype, suffix)) := as.integer(action_nums == atype_nums[i])]
    }
  }

  # Cleanup temp team_id columns
  for (lag in seq_len(n_prev)) {
    dt[, (paste0("team_id_prev", lag)) := NULL]
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
  temp_cols <- c("action_type_num",
                 paste0("action_type_num_prev", seq_len(n_prev)))
  dt[, (temp_cols) := NULL]

  # Replace NAs with 0 for numeric columns using setnafill (much faster)
  numeric_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
  if (length(numeric_cols) > 0) {
    data.table::setnafill(dt, fill = 0, cols = numeric_cols)
  }

  cli::cli_alert_success("Created {ncol(dt)} features for {nrow(dt)} actions")

  # Convert back to data.frame for compatibility
  as.data.frame(dt)
}


# Simple feature column names (single source of truth)
# PRE-ACTION STATE ONLY. `dx`/`dy` (end-displacement) and `result_success` were
# dropped 2026-06-18: they leak the action's OUTCOME into what is meant to be the
# state-BEFORE value (see epv_model.R:611 "EPV measures state BEFORE action"). The
# contamination inflated e.g. a corner delivery to ~0.20 (the model "knew" it
# succeeded into the box) when the pre-action corner *state* is ~0.07. EPV = the
# value of the position you are in, not what the action achieved; the action's
# credit comes from epv_delta = lead(epv) - epv. Prototype A/B (7.45M actions):
# ordering fixed (box 0.095 > corner 0.071, was inverted 0.125 < 0.197),
# held-out RMSE unchanged (0.1943 vs 0.1936).
EPV_SIMPLE_FEATURE_COLS <- c(
  "start_x", "start_y", "distance_to_goal", "angle_to_goal",
  "time_remaining", "is_extra_time", "time_in_half_remaining",
  "prev_x", "prev_y", "prev_dx", "prev_dy",
  "same_team_prev", "action_cat",
  "league_id"
)

# Action type encoding for simple features
EPV_SIMPLE_ACTION_MAP <- c(
  "pass" = 1L, "cross" = 2L, "shot" = 3L, "take_on" = 4L,
  "tackle" = 5L, "interception" = 6L, "clearance" = 7L,
  "aerial" = 8L, "foul" = 9L, "ball_recovery" = 10L
)

# League encoding for simple features (0 = unknown)
EPV_LEAGUE_MAP <- c(
  "ENG" = 1L, "ESP" = 2L, "GER" = 3L, "ITA" = 4L, "FRA" = 5L,
  "NED" = 6L, "POR" = 7L, "TUR" = 8L, "ENG2" = 9L, "SCO" = 10L,
  "UCL" = 11L, "UEL" = 12L, "UECL" = 13L, "WC" = 14L, "EURO" = 15L
)


#' Create Simple EPV Features
#'
#' Builds a 14-feature PRE-ACTION STATE set for EPV prediction: spatial location,
#' time remaining (per-match regulation/ET denominator, #94), extra-time
#' indicator, previous-action (arrival) context, action type, and league
#' identity. Outcome features (end-displacement dx/dy, result_success) are
#' deliberately excluded so EPV is the value of the state BEFORE the action,
#' not what the action achieved (the action's value = lead(epv) - epv).
#'
#' @param spadl_actions SPADL actions data frame
#' @param league League code (e.g., "ENG"). If NULL, uses \code{league} column
#'   from spadl_actions if present, otherwise defaults to 0 (unknown).
#'
#' @return Data frame with 16 EPV features plus match_id and action_id
#' @keywords internal
create_epv_features_simple <- function(spadl_actions, league = NULL) {
  if (is.null(spadl_actions) || nrow(spadl_actions) == 0) {
    cli::cli_abort("No SPADL actions provided for feature creation")
  }

  cli::cli_alert_info("Creating simple EPV features for {format(nrow(spadl_actions), big.mark=',')} actions...")

  dt <- data.table::as.data.table(spadl_actions)
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Spatial
  dt[, distance_to_goal := sqrt((100 - start_x)^2 + (50 - start_y)^2)]
  dt[, angle_to_goal := abs(atan2(56 - start_y, pmax(100 - start_x, 0.1)) -
                            atan2(44 - start_y, pmax(100 - start_x, 0.1)))]

  # Movement
  dt[, dx := end_x - start_x]
  dt[, dy := end_y - start_y]

  # Time remaining in match (1 = kickoff, 0 = full time).
  # Issue #94: Opta time_seconds = minute*60 + second is match-CUMULATIVE (the
  # clock runs 45->90+ across the break and into ET, not reset per half), so the
  # old flat 45*60 = 2700 denominator clamped EVERY second-half and ET event to
  # time_remaining == 0 — the model was time-blind for the entire 2nd half/ET.
  # Fix mirrors the already-shipped WP per-match denominator (wp_model.R ~L94):
  # decide per match_id whether the match reached extra time (any period 3/4) and
  # use EXTRA_TIME_SECONDS (7200) if so, else REGULATION_SECONDS (5400). Decided
  # PER MATCH on purpose — a per-EVENT cap would make time_remaining jump at the
  # 90' boundary (regulation rows 5400 vs ET rows 7200 in the same match);
  # is_extra_time below carries the ET signal instead. Shootout actions
  # (period_id >= 5) are dropped upstream in convert_opta_to_spadl().
  dt[, match_reached_et := any(period_id %in% OPTA_EXTRA_TIME_PERIODS), by = match_id]
  dt[, match_seconds := data.table::fifelse(match_reached_et,
                                            EXTRA_TIME_SECONDS, REGULATION_SECONDS)]
  dt[, time_remaining := 1 - pmin(time_seconds / match_seconds, 1)]
  # Per-event ET indicator (model feature): 1 for periods 3-4, else 0. Without
  # it, a flat regulation denominator would re-clamp ET events to ~0; mirrors the
  # WP model's is_extra_time feature so the model can learn ET-specific dynamics.
  dt[, is_extra_time := as.integer(period_id %in% OPTA_EXTRA_TIME_PERIODS)]
  dt[, c("match_reached_et", "match_seconds") := NULL]

  # PROTOTYPE (next retrain cycle — requires retrain + lockstep worker update to
  # ship): time remaining in the CURRENT half (sawtooth — 1 at each kickoff, 0 at
  # each whistle). Whole-match time_remaining above ramps only toward FULL time,
  # so at 45' it sits at ~0.5 (mid-range) and the end-of-FIRST-half wind-down is
  # invisible to it. Empirically (labeled-chunk check 2026-06-18) next-shot value
  # drops ~40% in 1st-half stoppage — a real signal the per-match clock misses.
  # Resets per period so both 45' and 90' whistles register. Regulation halves =
  # REGULATION_SECONDS/2 (2700s); ET halves = (EXTRA_TIME_SECONDS-REGULATION)/2
  # (900s). Stoppage time clamps to 0 (in-half elapsed > nominal half length).
  .reg_half <- REGULATION_SECONDS / 2
  .et_half  <- (EXTRA_TIME_SECONDS - REGULATION_SECONDS) / 2
  dt[, .half_start := data.table::fcase(
    period_id == 1L, 0,
    period_id == 2L, .reg_half,
    period_id == 3L, REGULATION_SECONDS,
    period_id == 4L, REGULATION_SECONDS + .et_half,
    default = 0)]
  dt[, .half_len := data.table::fcase(
    period_id %in% c(1L, 2L), .reg_half,
    period_id %in% OPTA_EXTRA_TIME_PERIODS, .et_half,
    default = .reg_half)]
  dt[, time_in_half_remaining := 1 - pmin(pmax((time_seconds - .half_start) / .half_len, 0), 1)]
  dt[, c(".half_start", ".half_len") := NULL]

  # Previous action context
  dt[, prev_x := shift(start_x, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, prev_y := shift(start_y, 1, type = "lag"), by = .(match_id, period_id)]
  dt[is.na(prev_x), prev_x := start_x]
  dt[is.na(prev_y), prev_y := start_y]
  dt[, prev_dx := start_x - prev_x]
  dt[, prev_dy := start_y - prev_y]

  # Possession continuity
  dt[, prev_team := shift(team_id, 1, type = "lag"), by = .(match_id, period_id)]
  dt[, same_team_prev := as.integer(!is.na(prev_team) & team_id == prev_team)]
  dt[, prev_team := NULL]

  # Action type (integer-encoded)
  dt[, action_cat := EPV_SIMPLE_ACTION_MAP[action_type]]
  dt[is.na(action_cat), action_cat := 0L]

  # Result
  dt[, result_success := as.integer(result == "success")]

  # League identity (integer-encoded)
  if (!is.null(league) && is.character(league) && length(league) == 1) {
    dt[, league_id := as.integer(EPV_LEAGUE_MAP[league] %||% 0L)]
  } else if ("league" %in% names(dt)) {
    mapped <- as.integer(EPV_LEAGUE_MAP[dt$league])
    dt[, league_id := data.table::fifelse(is.na(mapped), 0L, mapped)]
  } else {
    dt[, league_id := 0L]
  }

  # Keep only feature columns + identifiers
  keep_cols <- c("match_id", "action_id", EPV_SIMPLE_FEATURE_COLS)
  out <- dt[, ..keep_cols]

  # Fill NAs with 0
  numeric_cols <- EPV_SIMPLE_FEATURE_COLS
  data.table::setnafill(out, fill = 0, cols = numeric_cols)

  cli::cli_alert_success("Created {length(EPV_SIMPLE_FEATURE_COLS)} simple features for {nrow(out)} actions")

  as.data.frame(out)
}


#' Create Location-Only Features
#'
#' Creates a minimal feature set based only on location.
#'
#' @param x X coordinates
#' @param y Y coordinates
#'
#' @return Data frame with location features
#' @keywords internal
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
    y_right = as.integer(y >= 67)
  )
}


#' Get EPV Model Feature Columns
#'
#' Returns the list of feature columns used for EPV model training.
#' Uses shared constants to ensure consistency with create_epv_features().
#'
#' @param include_sequence Whether to include sequence features (default TRUE)
#' @param n_prev Number of previous actions for sequence features (default
#'   \code{EPV_N_PREV}; must match whatever \code{create_epv_features()} was
#'   given, or the model asks for columns the builder never produced)
#'
#' @return Character vector of feature column names
#' @keywords internal
get_epv_feature_cols <- function(include_sequence = TRUE, n_prev = EPV_N_PREV) {
  .check_n_prev(n_prev)
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
    for (lag in seq_len(n_prev)) {
      suffix <- paste0("_prev", lag)
      cols <- c(cols, paste0("result_success", suffix))
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
#' @keywords internal
create_epv_labels <- function(spadl_with_outcomes) {
  labels <- data.frame(
    match_id = spadl_with_outcomes$match_id,
    action_id = spadl_with_outcomes$action_id
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
#' @keywords internal
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
  dt <- chain_stats[, .(match_id, chain_id, possession_velocity, x_progression, chain_duration)][dt, on = c("match_id", "chain_id")]

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
#' @keywords internal
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
