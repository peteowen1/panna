# Possession Chain Functions for EPV Model (Optimized with data.table)
#
# Groups consecutive actions by the same team into possession chains.
# Each chain represents a continuous period of possession that ends when
# the opponent gains control, the ball goes out of play, or a goal is scored.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT setorder shift .SD .N .I := fifelse uniqueN as.data.table
NULL


#' Create Possession Chains from SPADL Actions
#'
#' Groups consecutive actions by the same team into possession chains.
#' Optimized with data.table for fast processing of large datasets.
#'
#' A chain ends when:
#' - Opponent gains possession (successful tackle/interception)
#' - Ball goes out of play (foul, throw-in situation)
#' - Goal is scored
#' - Period ends
#' - Large time gap (>30 seconds)
#'
#' @param spadl_actions Data frame in SPADL format from convert_opta_to_spadl()
#'
#' @return Data frame with chain assignments added
#'
#' @export
create_possession_chains <- function(spadl_actions) {
  if (is.null(spadl_actions) || nrow(spadl_actions) == 0) {
    cli::cli_abort("No SPADL actions provided for chain creation")
  }

  cli::cli_alert_info("Creating possession chains from {format(nrow(spadl_actions), big.mark=',')} actions...")

  # Convert to data.table
  dt <- data.table::as.data.table(spadl_actions)

  # Ensure team_id is character for consistent comparison
  dt[, team_id := as.character(team_id)]

  # Sort by match, period, time
  data.table::setorder(dt, match_id, period_id, time_seconds, action_id)

  # Create lagged values for chain break detection
  # Using separate statements to avoid potential issues
  dt[, prev_team_id := shift(team_id, 1, type = "lag", fill = NA_character_), by = match_id]
  dt[, prev_period_id := shift(period_id, 1, type = "lag", fill = NA_integer_), by = match_id]
  dt[, prev_time := shift(time_seconds, 1, type = "lag", fill = NA_real_), by = match_id]
  dt[, prev_action_type := shift(action_type, 1, type = "lag", fill = NA_character_), by = match_id]
  dt[, prev_result := shift(result, 1, type = "lag", fill = NA_character_), by = match_id]

  # Compute chain_break in a single vectorized operation
  # A chain breaks when:
  # 1. First action in match (prev_team_id is NA)
  # 2. Team changes
  # 3. Period changes
  # 4. After a goal (shot with success)
  # 5. After a foul
  # 6. Time gap > 30 seconds
  dt[, chain_break := (
    is.na(prev_team_id) |                                    # First action in match
    (!is.na(prev_team_id) & team_id != prev_team_id) |       # Team change
    (!is.na(prev_period_id) & period_id != prev_period_id) | # Period change
    (prev_action_type == "shot" & prev_result == "success") | # After goal
    (prev_action_type == "foul") |                           # After foul
    (!is.na(prev_time) & (time_seconds - prev_time) > CHAIN_TIME_GAP_SECONDS) # Time gap
  )]

  # Handle NA values in logical expression (set to TRUE if NA, meaning chain break)
  dt[is.na(chain_break), chain_break := TRUE]

  # Create chain_id using cumsum of breaks within each match
  dt[, chain_id := cumsum(chain_break), by = match_id]

  # Add chain metadata
  dt[, chain_team_id := team_id]
  dt[, action_in_chain := seq_len(.N), by = .(match_id, chain_id)]

  # Add chain start time
  dt[, chain_start_time := time_seconds[1], by = .(match_id, chain_id)]

  # Cleanup temporary columns
  dt[, c("prev_team_id", "prev_period_id", "prev_time", "prev_action_type",
         "prev_result", "chain_break") := NULL]

  n_chains <- dt[, data.table::uniqueN(paste(match_id, chain_id))]
  n_matches <- dt[, data.table::uniqueN(match_id)]

  cli::cli_alert_success("Created {format(n_chains, big.mark=',')} possession chains across {n_matches} matches")

  as.data.frame(dt)
}


#' Classify Chain Outcomes
#'
#' Determines the outcome of each possession chain (goal, shot, turnover, etc.)
#' Optimized with data.table.
#'
#' @param spadl_with_chains SPADL actions with chain_id from create_possession_chains()
#'
#' @return Data frame of chain-level statistics
#'
#' @keywords internal
classify_chain_outcomes <- function(spadl_with_chains) {
  if (!"chain_id" %in% names(spadl_with_chains)) {
    cli::cli_abort("Input must have chain_id column. Run create_possession_chains() first.")
  }

  cli::cli_alert_info("Classifying chain outcomes...")

  dt <- data.table::as.data.table(spadl_with_chains)

  # Aggregate chain-level info efficiently
  chain_summary <- dt[, .(
    team_id = team_id[1],
    chain_start_time = time_seconds[1],
    chain_end_time = time_seconds[.N],
    n_actions = .N,
    last_action_type = action_type[.N],
    last_action_result = result[.N],
    has_shot = any(action_type == "shot"),
    has_goal = any(action_type == "shot" & result == "success")
  ), by = .(match_id, chain_id)]

  # Classify outcomes based on last action
  chain_summary[, outcome := "turnover"]
  chain_summary[last_action_type == "shot" & last_action_result == "success", outcome := "goal"]
  chain_summary[last_action_type == "shot" & last_action_result != "success", outcome := "shot"]
  chain_summary[last_action_type == "foul", outcome := "foul"]
  chain_summary[last_action_type == "clearance", outcome := "out_of_play"]

  # Update for chains with shots that didn't end in shot
  chain_summary[has_shot & outcome == "turnover", outcome := "shot"]

  # Count shots in chain
  shots_per_chain <- dt[action_type == "shot", .(shots_in_chain = .N), by = .(match_id, chain_id)]
  chain_summary <- shots_per_chain[chain_summary, on = c("match_id", "chain_id")]
  chain_summary[is.na(shots_in_chain), shots_in_chain := 0L]

  # Binary goal indicator
  chain_summary[, ends_in_goal := as.integer(outcome == "goal")]

  # Add xG if available in source data
  if ("xg" %in% names(dt)) {
    chain_xg <- dt[, .(chain_xg = sum(xg, na.rm = TRUE)), by = .(match_id, chain_id)]
    chain_summary <- chain_xg[chain_summary, on = c("match_id", "chain_id")]
  } else {
    chain_summary[, chain_xg := NA_real_]
  }

  # Cleanup
  chain_summary[, c("last_action_type", "last_action_result", "has_shot", "has_goal") := NULL]

  n_goals <- sum(chain_summary$ends_in_goal)
  cli::cli_alert_success("Classified {nrow(chain_summary)} chains: {n_goals} ended in goals")

  as.data.frame(chain_summary)
}


#' Add Opponent's Next Chain Outcome
#'
#' For EPV conceding model, we need to know if the opponent scores on their
#' next possession after we lose the ball. Optimized with data.table.
#'
#' @param chain_outcomes Data frame from classify_chain_outcomes()
#'
#' @return Chain outcomes with next_chain_goal column added
#' @keywords internal
add_next_chain_outcome <- function(chain_outcomes) {
  cli::cli_alert_info("Adding opponent next chain outcomes...")

  dt <- data.table::as.data.table(chain_outcomes)
  data.table::setorder(dt, match_id, chain_start_time)

  # Get next chain's team and goal within each match
  dt[, `:=`(
    next_team_id = shift(team_id, 1, type = "lead"),
    next_ends_in_goal = shift(ends_in_goal, 1, type = "lead")
  ), by = match_id]

  # Next opponent chain goal: 1 if next chain is by different team and they score
  dt[, next_opponent_chain_goal := fifelse(
    !is.na(next_team_id) & next_team_id != team_id & next_ends_in_goal == 1L,
    1L, 0L
  )]

  # Cleanup
  dt[, c("next_team_id", "next_ends_in_goal") := NULL]

  cli::cli_alert_success("Added next opponent chain outcomes")

  as.data.frame(dt)
}


#' Label Actions with Chain Outcomes
#'
#' Adds chain outcome labels to each action for EPV model training.
#' Optimized with data.table merge.
#'
#' @param spadl_with_chains SPADL actions with chain assignments
#' @param chain_outcomes Chain-level outcomes from classify_chain_outcomes()
#'
#' @return SPADL actions with outcome labels
#'
#' @keywords internal
label_actions_with_outcomes <- function(spadl_with_chains, chain_outcomes) {
  cli::cli_alert_info("Labeling actions with chain outcomes...")

  dt_actions <- data.table::as.data.table(spadl_with_chains)
  dt_outcomes <- data.table::as.data.table(chain_outcomes)

  # Select relevant outcome columns
  outcome_cols <- c("match_id", "chain_id", "ends_in_goal", "chain_xg",
                     "next_opponent_chain_goal", "outcome")
  outcome_cols <- intersect(outcome_cols, names(dt_outcomes))

  # Merge outcomes to actions
  result <- dt_outcomes[, ..outcome_cols][dt_actions, on = c("match_id", "chain_id")]

  # Rename for clarity
  if ("ends_in_goal" %in% names(result)) {
    data.table::setnames(result, "ends_in_goal", "chain_ends_in_goal")
  }
  if ("next_opponent_chain_goal" %in% names(result)) {
    data.table::setnames(result, "next_opponent_chain_goal", "opponent_scores_next")
  }
  if ("outcome" %in% names(result)) {
    data.table::setnames(result, "outcome", "chain_outcome")
  }

  # Create shot indicator
  result[, chain_ends_in_shot := as.integer(chain_outcome %in% c("goal", "shot"))]

  # Restore order
  data.table::setorder(result, match_id, action_id)

  cli::cli_alert_success("Labeled {format(nrow(result), big.mark=',')} actions with chain outcomes")

  as.data.frame(result)
}


#' Calculate Chain Statistics
#'
#' Computes summary statistics about possession chains in the dataset.
#'
#' @param chain_outcomes Data frame from classify_chain_outcomes()
#'
#' @return List with chain statistics
#' @keywords internal
calculate_chain_stats <- function(chain_outcomes) {
  dt <- data.table::as.data.table(chain_outcomes)

  stats <- list(
    total_chains = nrow(dt),
    chains_with_shots = sum(dt$shots_in_chain > 0, na.rm = TRUE),
    chains_with_goals = sum(dt$ends_in_goal, na.rm = TRUE),
    mean_actions_per_chain = mean(dt$n_actions, na.rm = TRUE),
    median_actions_per_chain = stats::median(dt$n_actions, na.rm = TRUE),
    mean_duration = mean(dt$chain_end_time - dt$chain_start_time, na.rm = TRUE),
    outcome_dist = as.list(prop.table(table(dt$outcome))),
    goal_rate = mean(dt$ends_in_goal, na.rm = TRUE),
    shot_rate = sum(dt$shots_in_chain > 0, na.rm = TRUE) / nrow(dt)
  )

  stats
}
