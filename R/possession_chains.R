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
#' @family possession chains
#' @export
#' @examples
#' \dontrun{
#' spadl <- convert_opta_to_spadl(opta_events)
#' chains <- create_possession_chains(spadl)
#' head(chains[, c("match_id", "chain_id", "action_type", "team_id")])
#' }
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

  # Keeper-rebound transparency (pannadata#76).
  # A keeper save/punch (a DEFLECTION) carries the DEFENDING team's team_id, so a
  # shot → save → rebound sequence flips attacking → defending → attacking and
  # the raw team-change rule below would split ONE possession into THREE chains
  # (the 1-event save mislabelled a turnover). Treat these actions as transparent
  # to possession tracking: they must neither force a new chain on the team-change
  # clause NOR advance the running "previous team". We do this by computing an
  # *effective* team that is NA on keeper-rebound rows and LOCF-filled with the
  # prior attacker's team within each match — so keeper rows inherit the
  # attacker's team (no spurious split) and the NEXT action compares against the
  # attacker, not the keeper. If the original attacker regains the ball the chain
  # continues; if the defending team genuinely wins it, their next non-keeper
  # action still has team_id != prev_team and triggers a real split, preserving
  # turnovers.
  # SET CHOSEN FROM DATA (2026-06-18): only deflections that leave the ball live.
  #   keeper_save  ~50% of occurrences are stranded 1-event chains, 100% preceded
  #                by a shot -> genuine rebound/contest. FOLD.
  #   keeper_punch ~50% stranded, 98% preceded by a cross/pass -> contest. FOLD.
  #   keeper_claim only 4% stranded -> a clean CATCH is a real change of
  #                possession (the keeper's team keeps it 96% of the time), so it
  #                is NOT folded — folding it would mis-attribute the catch to the
  #                attacking chain. EXCLUDED.
  #   keeper_pick_up (1%) likewise a settled GK possession. EXCLUDED.
  # Also narrower than the blog-side build_chains_ci (which folds Opta type 50 /
  # SPADL `dispossessed`) — that's a genuine turnover, not a GK deflection, and
  # folding it would corrupt the EPV training labels. See pannadata#<contest> for
  # the broader contest-vs-possession idea (outcome/qualifier-driven).
  keeper_rebound_types <- c("keeper_save", "keeper_punch")
  dt[, effective_team_id := team_id]
  dt[action_type %in% keeper_rebound_types, effective_team_id := NA_character_]
  # Carry the last non-keeper team forward within each match (character-safe
  # LOCF — data.table::nafill() does not accept character vectors on all
  # versions). cummax() over the non-NA mask points each row at the most recent
  # populated value; rows before any populated value (e.g. a match opening on a
  # keeper rebound) stay NA and are handled by the first-action break below.
  dt[, effective_team_id := {
    filled_idx <- cummax((!is.na(effective_team_id)) * seq_len(.N))
    fifelse(filled_idx == 0L, NA_character_, effective_team_id[filled_idx])
  }, by = match_id]
  # Compare each row against the carried-forward attacker team. Keeper-rebound
  # rows now match the prior attacker (so they don't start a chain); genuine
  # opponent actions still differ and split.
  dt[, prev_team_id := shift(effective_team_id, 1, type = "lag", fill = NA_character_), by = match_id]
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
  # Team-change comparison uses effective_team_id (keeper-rebound rows inherit
  # the prior attacker's team via LOCF above) so a save/claim/punch/block does
  # not split the attacking possession — only a genuine opponent action does
  # (pannadata#76).
  dt[, chain_break := (
    is.na(prev_team_id) |                                    # First action in match
    (!is.na(prev_team_id) & effective_team_id != prev_team_id) | # Team change
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
  dt[, c("effective_team_id", "prev_team_id", "prev_period_id", "prev_time",
         "prev_action_type", "prev_result", "chain_break") := NULL]

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


#' Summarize Match-Level Possession Chains
#'
#' Aggregates possession chain data at the match-team level, providing
#' chain counts, success rates, territory metrics, and duration stats.
#'
#' @param spadl_with_chains SPADL actions with chain_id and chain outcome columns
#'   (from \code{create_possession_chains()} + \code{label_actions_with_outcomes()}).
#'
#' @return Data frame with one row per team per match containing:
#'   \itemize{
#'     \item \code{match_id}, \code{team_id}: Identifiers
#'     \item \code{total_chains}: Number of possession chains
#'     \item \code{chains_with_shot}, \code{chains_with_goal}: Chain outcomes
#'     \item \code{avg_chain_length}: Mean actions per chain
#'     \item \code{avg_chain_duration}: Mean chain duration (seconds)
#'     \item \code{territory_pct}: Percentage of chains reaching final third (x > 66)
#'     \item \code{chain_xg}: Sum of xG across all chains (if available)
#'     \item \code{possession_pct}: Team's share of total chains in the match
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' spadl <- convert_opta_to_spadl(events)
#' spadl <- create_possession_chains(spadl)
#' outcomes <- classify_chain_outcomes(spadl)
#' spadl <- label_actions_with_outcomes(spadl, outcomes)
#' match_chains <- summarize_match_chains(spadl)
#' }
summarize_match_chains <- function(spadl_with_chains) {
  if (!"chain_id" %in% names(spadl_with_chains)) {
    cli::cli_abort("Input must have chain_id column. Run create_possession_chains() first.")
  }

  dt <- data.table::as.data.table(spadl_with_chains)

  # Per-chain summary first
  chain_level <- dt[, .(
    team_id = team_id[1],
    n_actions = .N,
    duration = time_seconds[.N] - time_seconds[1],
    has_shot = any(action_type == "shot"),
    has_goal = any(action_type == "shot" & result == "success"),
    max_x = max(start_x, na.rm = TRUE),
    chain_xg = sum(xg[action_type == "shot"], na.rm = TRUE)
  ), by = .(match_id, chain_id)]

  # Aggregate to match-team level
  result <- chain_level[, .(
    total_chains = .N,
    chains_with_shot = sum(has_shot),
    chains_with_goal = sum(has_goal),
    avg_chain_length = round(mean(n_actions), 1),
    avg_chain_duration = round(mean(duration), 1),
    territory_pct = round(sum(max_x > CHAIN_FINAL_THIRD_X) / .N * 100, 1),
    chain_xg = round(sum(chain_xg), 2)
  ), by = .(match_id, team_id)]

  # Add possession percentage (team chains / total chains in match)
  match_totals <- result[, .(match_total = sum(total_chains)), by = match_id]
  result <- match_totals[result, on = "match_id"]
  result[, possession_pct := round(total_chains / match_total * 100, 1)]
  result[, match_total := NULL]

  data.table::setorder(result, match_id, -total_chains)

  as.data.frame(result)
}


#' Summarize Player-Level Possession Chains
#'
#' Aggregates how each player contributes to possession chains within a match.
#' Tracks chain involvement, starts, finishes, and progressive contributions.
#'
#' @param spadl_with_chains SPADL actions with chain_id and chain outcome columns
#'   (from \code{create_possession_chains()} + \code{label_actions_with_outcomes()}).
#'
#' @return Data frame with one row per player per match containing:
#'   \itemize{
#'     \item \code{match_id}, \code{player_id}, \code{player_name}, \code{team_id}
#'     \item \code{chains_involved}: Unique chains the player participated in
#'     \item \code{chain_starts}: Chains where player had the first action
#'     \item \code{chain_finishes}: Chains where player had the last action before outcome
#'     \item \code{progressive_chains}: Chains where player advanced ball >25 units forward
#'     \item \code{key_chain_actions}: Actions in chains ending in shot/goal
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' spadl <- convert_opta_to_spadl(events)
#' spadl <- create_possession_chains(spadl)
#' outcomes <- classify_chain_outcomes(spadl)
#' spadl <- label_actions_with_outcomes(spadl, outcomes)
#' player_chains <- summarize_player_chains(spadl)
#' }
summarize_player_chains <- function(spadl_with_chains) {
  if (!"chain_id" %in% names(spadl_with_chains)) {
    cli::cli_abort("Input must have chain_id column. Run create_possession_chains() first.")
  }

  dt <- data.table::as.data.table(spadl_with_chains)

  # Per player-chain: track contribution metrics
  player_chain <- dt[, .(
    is_first = any(action_in_chain == 1L),
    is_last = any(action_in_chain == max(action_in_chain)),
    x_progression = max(start_x, na.rm = TRUE) - min(start_x, na.rm = TRUE),
    in_successful_chain = any(chain_outcome %in% c("shot", "goal"), na.rm = TRUE),
    n_actions = .N
  ), by = .(match_id, chain_id, player_id, player_name, team_id)]

  # Aggregate to player-match level
  result <- player_chain[, .(
    chains_involved = .N,
    chain_starts = sum(is_first),
    chain_finishes = sum(is_last),
    progressive_chains = sum(x_progression > CHAIN_PROGRESSIVE_THRESHOLD),
    key_chain_actions = sum(n_actions[in_successful_chain])
  ), by = .(match_id, player_id, player_name, team_id)]

  data.table::setorder(result, match_id, -chains_involved)

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
