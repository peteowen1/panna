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

  team_stats <- stats %>%
    dplyr::group_by(.data$match_id, .data$team, .data$is_home) %>%
    dplyr::summarise(
      total_touches = sum(.data$touches, na.rm = TRUE),
      .groups = "drop"
    )

  # Estimate sequences: rough approximation
  # Typical match has ~100-150 sequences per team
  team_stats %>%
    dplyr::mutate(
      # Approximate: 1 sequence per ~5 touches
      estimated_sequences = pmax(MIN_SEQUENCES_PER_MATCH, .data$total_touches / TOUCHES_PER_SEQUENCE)
    )
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
  data <- player_stats %>%
    dplyr::left_join(
      team_sequences %>%
        dplyr::select(match_id, team, estimated_sequences),
      by = c("match_id", "team")
    )

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
  player_shooting <- shooting %>%
    dplyr::filter(!.data$is_penalty) %>%
    dplyr::group_by(.data$player_name) %>%
    dplyr::summarise(
      total_shots = dplyr::n(),
      total_goals = sum(.data$is_goal, na.rm = TRUE),
      total_xg = sum(.data$xg, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$total_shots >= min_shots) %>%
    dplyr::mutate(
      # Goals over/under xG
      goals_minus_xg = .data$total_goals - .data$total_xg,
      # Finishing ratio (with Bayesian shrinkage toward 1.0)
      # Use beta prior: success rate with prior toward league average
      finishing_modifier = (.data$total_goals + 5) / (.data$total_xg + 5)
    )

  player_shooting
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
    warning("No offensive feature columns found")
    return(stats)
  }

  stats %>%
    dplyr::select(
      .data$match_id, .data$team, .data$player_name,
      dplyr::any_of(available_cols)
    )
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
    warning("No defensive feature columns found")
    return(stats)
  }

  stats %>%
    dplyr::select(
      .data$match_id, .data$team, .data$player_name,
      dplyr::any_of(available_cols)
    )
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
    warning(paste("Weight column", weight_col, "not found, skipping padding"))
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
  # Get season from match_id or join with match data
  season_stats <- match_stats %>%
    dplyr::group_by(.data$player_name, .data$team)

  # Sum counting stats
  if (!is.null(count_cols) && length(count_cols) > 0) {
    count_cols <- intersect(count_cols, names(match_stats))
    for (col in count_cols) {
      season_stats <- season_stats %>%
        dplyr::mutate(!!paste0(col, "_total") := sum(.data[[col]], na.rm = TRUE))
    }
  }

  # Weight-average rate stats (by minutes or appearances)
  if (!is.null(rate_cols) && length(rate_cols) > 0) {
    rate_cols <- intersect(rate_cols, names(match_stats))
    for (col in rate_cols) {
      season_stats <- season_stats %>%
        dplyr::mutate(!!paste0(col, "_avg") := mean(.data[[col]], na.rm = TRUE))
    }
  }

  # Add game counts
  season_stats %>%
    dplyr::summarise(
      n_games = dplyr::n(),
      .groups = "drop"
    )
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
    warning("No summary stats available for feature matrix")
    return(NULL)
  }

  # Calculate team sequences
  team_seq <- calculate_team_sequences(stats)

  # Convert to per-100-sequences
  rate_stats <- calculate_per_100_sequences(stats, team_seq)

  # Aggregate to player level
  player_stats <- rate_stats %>%
    dplyr::group_by(.data$player_name) %>%
    dplyr::summarise(
      n_games = dplyr::n(),
      total_minutes = sum(.data$min, na.rm = TRUE),
      dplyr::across(dplyr::ends_with("_p100"), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$total_minutes >= min_minutes)

  # Apply Bayesian padding
  rate_cols <- names(player_stats)[grepl("_p100$", names(player_stats))]
  player_stats <- apply_bayesian_padding(player_stats, rate_cols, min_games = 10)

  player_stats
}
