# Feature engineering functions for panna package
#
# Creates rate statistics (per-100-sequences) and applies
# Bayesian padding for low-minute players.

#' Calculate team possession sequences
#'
#' Estimates the number of possession sequences per match for each team.
#' A sequence is defined as a period of continuous possession.
#'
#' @param stats Processed summary stats with touches and passes
#'
#' @return Data frame with team sequences per match
#' @keywords internal
calculate_team_sequences <- function(stats) {
  # Estimate sequences from team-level possession indicators

  # Approximation: sequences ~ (total_touches + turnovers) / avg_touches_per_sequence

  dt <- data.table::as.data.table(stats)
  team_stats <- dt[, .(total_touches = sum(touches, na.rm = TRUE)),
                   by = .(match_id, team, is_home)]

  # Estimate sequences: rough approximation
  # Typical match has ~100-150 sequences per team
  # Approximate: 1 sequence per ~5 touches
  team_stats[, estimated_sequences := pmax(MIN_SEQUENCES_PER_MATCH,
                                           total_touches / TOUCHES_PER_SEQUENCE)]
  as.data.frame(team_stats)
}


#' Convert statistics to per-100-sequences rate
#'
#' Normalizes counting statistics to a per-100-sequences basis,
#' similar to basketball's per-100-possessions.
#'
#' @param player_stats Data frame of player match stats
#' @param team_sequences Data frame of team sequences per match
#' @param stat_cols Character vector of columns to convert
#'
#' @return Data frame with rate statistics
#' @keywords internal
calculate_per_100_sequences <- function(player_stats, team_sequences, stat_cols = NULL) {
  # Join team sequences
  seq_cols <- intersect(c("match_id", "team", "estimated_sequences"), names(team_sequences))
  data <- merge(player_stats, team_sequences[, seq_cols, drop = FALSE],
                by = c("match_id", "team"), all.x = TRUE)

  if (is.null(stat_cols)) {
    # Default stat columns to convert (snake_case from janitor::clean_names())
    stat_cols <- c("gls", "ast", "sh", "so_t", "x_g", "npx_g", "x_ag",
                   "touches", "tkl", "int", "blocks", "sca", "gca",
                   "cmp", "prg_p", "carries", "prg_c")
    stat_cols <- intersect(stat_cols, names(data))
  }

  # Calculate per-100-sequence rates
  for (col in stat_cols) {
    if (col %in% names(data)) {
      new_col <- paste0(col, "_p100")
      data[[new_col]] <- safe_divide(data[[col]] * 100, data$estimated_sequences)
    }
  }

  data
}


#' Calculate player finishing modifier
#'
#' Calculates a player's finishing ability relative to xG.
#' Used to adjust xG predictions in splint analysis.
#'
#' @param shooting Processed shooting data
#' @param min_shots Minimum shots required (default 20)
#'
#' @return Data frame with finishing modifier per player
#' @keywords internal
calculate_finishing_modifier <- function(shooting, min_shots = 20) {
  dt <- data.table::as.data.table(shooting)
  dt <- dt[is_penalty == FALSE]
  player_shooting <- dt[, .(
    total_shots = .N,
    total_goals = sum(is_goal, na.rm = TRUE),
    total_xg = sum(xg, na.rm = TRUE)
  ), by = player_name]
  player_shooting <- player_shooting[total_shots >= min_shots]
  # Goals over/under xG
  player_shooting[, goals_minus_xg := total_goals - total_xg]
  # Finishing ratio (with Bayesian shrinkage toward 1.0)
  # Use beta prior: success rate with prior toward league average
  player_shooting[, finishing_modifier := (total_goals + 5) / (total_xg + 5)]

  as.data.frame(player_shooting)
}


#' Create offensive features
#'
#' Builds attacking-related rate statistics for SPM model.
#'
#' @param stats Processed player stats with rate columns
#'
#' @return Data frame with offensive features
#' @keywords internal
create_offensive_features <- function(stats) {
  # Ensure we have the necessary columns (snake_case)
  offensive_cols <- c(
    "gls_p100", "x_g_p100", "npx_g_p100",
    "sh_p100", "so_t_p100",
    "ast_p100", "x_ag_p100",
    "sca_p100", "gca_p100",
    "prg_p_p100", "prg_c_p100",
    "carries_p100"
  )

  available_cols <- intersect(offensive_cols, names(stats))

  if (length(available_cols) == 0) {
    cli::cli_warn("No offensive feature columns found")
    return(stats)
  }

  keep_cols <- intersect(c("match_id", "team", "player_name", available_cols), names(stats))
  stats[, keep_cols, drop = FALSE]
}


#' Create defensive features
#'
#' Builds defensive-related rate statistics for SPM model.
#'
#' @param stats Processed player stats with rate columns
#'
#' @return Data frame with defensive features
#' @keywords internal
create_defensive_features <- function(stats) {
  # Defensive columns (snake_case)
  defensive_cols <- c(
    "tkl_p100", "int_p100", "blocks_p100",
    "tkl_won_p100", "clr_p100"
  )

  available_cols <- intersect(defensive_cols, names(stats))

  if (length(available_cols) == 0) {
    cli::cli_warn("No defensive feature columns found")
    return(stats)
  }

  keep_cols <- intersect(c("match_id", "team", "player_name", available_cols), names(stats))
  stats[, keep_cols, drop = FALSE]
}


#' Apply Bayesian padding to statistics
#'
#' Regresses statistics toward population mean for players with few games.
#' This prevents extreme ratings from small sample sizes.
#'
#' @param player_stats Data frame with player statistics
#' @param stat_cols Character vector of columns to pad
#' @param min_games Games required for full weight (default 10)
#' @param weight_col Column containing games/minutes played
#'
#' @return Data frame with Bayesian-padded statistics
#' @keywords internal
apply_bayesian_padding <- function(player_stats, stat_cols, min_games = 10,
                                    weight_col = "n_games") {
  if (!weight_col %in% names(player_stats)) {
    cli::cli_warn("Weight column {.val {weight_col}} not found, skipping padding.")
    return(player_stats)
  }

  # Calculate population means
  pop_means <- sapply(stat_cols, function(col) {
    if (col %in% names(player_stats)) {
      mean(player_stats[[col]], na.rm = TRUE)
    } else {
      NA
    }
  })

  # Apply shrinkage
  for (col in stat_cols) {
    if (col %in% names(player_stats) && !is.na(pop_means[col])) {
      n_games <- player_stats[[weight_col]]
      weight <- pmin(n_games / min_games, 1)

      player_stats[[col]] <- weight * player_stats[[col]] +
        (1 - weight) * pop_means[col]
    }
  }

  player_stats
}


#' Aggregate player season stats
#'
#' Combines match-level stats into season aggregates.
#'
#' @param match_stats Data frame of match-level player stats
#' @param rate_cols Columns with rate statistics
#' @param count_cols Columns with counting statistics
#'
#' @return Data frame with season-level player stats
#' @keywords internal
aggregate_player_season_stats <- function(match_stats, rate_cols = NULL, count_cols = NULL) {
  dt <- data.table::as.data.table(match_stats)

  # Sum counting stats per group
  if (!is.null(count_cols) && length(count_cols) > 0) {
    count_cols <- intersect(count_cols, names(dt))
    for (col in count_cols) {
      new_col <- paste0(col, "_total")
      dt[, (new_col) := sum(get(col), na.rm = TRUE), by = .(player_name, team)]
    }
  }

  # Weight-average rate stats (by minutes or appearances)
  if (!is.null(rate_cols) && length(rate_cols) > 0) {
    rate_cols <- intersect(rate_cols, names(dt))
    for (col in rate_cols) {
      new_col <- paste0(col, "_avg")
      dt[, (new_col) := mean(get(col), na.rm = TRUE), by = .(player_name, team)]
    }
  }

  # Add game counts
  as.data.frame(dt[, .(n_games = .N), by = .(player_name, team)])
}


#' Create full player feature matrix
#'
#' Builds the complete feature matrix for SPM model.
#'
#' @param processed_data List of processed data
#' @param min_minutes Minimum minutes for inclusion
#'
#' @return Data frame with all player features
#' @export
create_player_feature_matrix <- function(processed_data, min_minutes = 180) {
  # Get summary stats
  stats <- processed_data$stats_summary

  if (is.null(stats) || nrow(stats) == 0) {
    cli::cli_warn("No summary stats available for feature matrix")
    return(NULL)
  }

  # Calculate team sequences
  team_seq <- calculate_team_sequences(stats)

  # Convert to per-100-sequences
  rate_stats <- calculate_per_100_sequences(stats, team_seq)

  # Aggregate to player level
  dt <- data.table::as.data.table(rate_stats)
  p100_cols <- names(dt)[grepl("_p100$", names(dt))]
  agg_exprs <- c(
    list(n_games = quote(.N), total_minutes = quote(sum(min, na.rm = TRUE))),
    lapply(stats::setNames(p100_cols, p100_cols), function(col) {
      bquote(mean(.(as.name(col)), na.rm = TRUE))
    })
  )
  player_stats <- dt[, eval(as.call(c(quote(list), agg_exprs))), by = player_name]
  player_stats <- player_stats[total_minutes >= min_minutes]
  player_stats <- as.data.frame(player_stats)

  # Apply Bayesian padding
  rate_cols <- names(player_stats)[grepl("_p100$", names(player_stats))]
  player_stats <- apply_bayesian_padding(player_stats, rate_cols, min_games = 10)

  player_stats
}
