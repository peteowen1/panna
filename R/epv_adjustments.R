# EPV Position Centering and Opponent Adjustment
#
# Position centering: subtractive adjustment comparing each player to their
# positional peers (using Opta positions). Adapted from torpverse pattern.
#
# Opponent adjustment: residual-based. Measures how much teams over/underperform
# their season average when facing each opponent. Uses decay-weighted rolling
# profiles with Bayesian shrinkage. Distributed to players by minutes share.

#' @importFrom data.table data.table as.data.table setorder shift .SD .N :=
#'   fifelse fcase
NULL


# =============================================================================
# POSITION CENTERING
# =============================================================================

#' Adjust Player EPV Credits for Position
#'
#' Subtracts the positional mean from each EPV component, so players are
#' compared to peers at the same position rather than across all positions.
#' Uses Opta lineup positions (Goalkeeper, Defender, Defensive Midfielder,
#' Midfielder, Attacking Midfielder, Striker). Wing Back merged into Defender.
#'
#' @param player_match data.table with columns: player_id, match_id, position,
#'   and one or more credit columns to center
#' @param credit_cols Character vector of column names to position-center.
#'   Defaults to "total_credit".
#'
#' @return Same data.table with `{col}_adj` columns added for each credit col
#' @keywords internal
adjust_epv_for_position <- function(player_match, credit_cols = "total_credit") {
  if (!"position" %in% names(player_match)) {
    cli::cli_abort("player_match must have a 'position' column")
  }

  dt <- data.table::as.data.table(player_match)

  # Compute position means
  pos_means <- dt[!is.na(position), lapply(.SD, mean, na.rm = TRUE),
    by = position, .SDcols = credit_cols]

  # Rename to _pos_mean
  mean_cols <- paste0(credit_cols, "_pos_mean")
  data.table::setnames(pos_means, credit_cols, mean_cols)

  # Join and subtract

  dt <- pos_means[dt, on = "position"]

  for (i in seq_along(credit_cols)) {
    adj_col <- paste0(credit_cols[i], "_adj")
    dt[, (adj_col) := get(credit_cols[i]) - get(mean_cols[i])]
  }

  # Players with NA position get no adjustment (adj = raw value)
  n_na_pos <- sum(is.na(dt$position))
  if (n_na_pos > 0) {
    for (i in seq_along(credit_cols)) {
      adj_col <- paste0(credit_cols[i], "_adj")
      dt[is.na(position), (adj_col) := get(credit_cols[i])]
    }
  }

  # Clean up temp columns
  dt[, (mean_cols) := NULL]

  dt
}


#' Get Player Positions from Opta Lineups
#'
#' Extracts each player's most common starting position across a season.
#' Wing Back is merged into Defender. Substitutes without a starting
#' position are assigned one based on their average pitch x-coordinate.
#'
#' @param lineups data.table from load_opta_lineups()
#' @param spadl_actions Optional SPADL actions for fallback position assignment
#'   based on average x-coordinate
#'
#' @return data.table with player_id and position columns
#' @keywords internal
get_player_positions <- function(lineups, spadl_actions = NULL) {
  dt <- data.table::as.data.table(lineups)

  # Merge Wing Back into Defender (too few for standalone group)
  dt[position == "Wing Back", position := "Defender"]

  # Most common starting position per player (exclude Substitute)
  starter_pos <- dt[position != "Substitute", .N, by = .(player_id, position)]
  data.table::setorder(starter_pos, player_id, -N)
  player_pos <- starter_pos[, .SD[1], by = player_id][, .(player_id, position)]

  # Fallback for players who only appeared as subs
  if (!is.null(spadl_actions)) {
    missing_ids <- setdiff(
      unique(data.table::as.data.table(spadl_actions)$player_id),
      player_pos$player_id
    )
    if (length(missing_ids) > 0) {
      avg_x <- data.table::as.data.table(spadl_actions)[
        player_id %in% missing_ids,
        .(avg_x = mean(start_x, na.rm = TRUE)), by = player_id
      ]
      avg_x[, position := data.table::fcase(
        avg_x < 15, "Goalkeeper",
        avg_x < 35, "Defender",
        avg_x < 55, "Midfielder",
        avg_x < 70, "Attacking Midfielder",
        default = "Striker"
      )]
      player_pos <- data.table::rbindlist(
        list(player_pos, avg_x[, .(player_id, position)]),
        use.names = TRUE
      )
    }
  }

  player_pos
}


# =============================================================================
# OPPONENT ADJUSTMENT
# =============================================================================

#' Adjust Player EPV Credits for Opponent Strength
#'
#' Residual-based opponent adjustment. For each match, computes how much
#' a team over/underperformed their season average, then attributes that
#' residual to the opponent they faced. Uses decay-weighted rolling profiles
#' with Bayesian shrinkage toward league average (0).
#'
#' Adjustment is distributed to players by minutes share (proportional to
#' playing time within the match).
#'
#' @param player_match data.table with columns: player_id, match_id, team_id,
#'   match_date, minutes_played, total_credit (or column specified by credit_col)
#' @param credit_col Name of the credit column to use for team totals.
#'   Default "total_credit".
#' @param lambda_decay Exponential decay rate for opponent profiles.
#'   Default EPV_OPP_LAMBDA_DECAY (0.003, ~231-day half-life).
#' @param prior_games Pseudo-games for Bayesian shrinkage toward league avg.
#'   Default EPV_OPP_PRIOR_GAMES (2).
#'
#' @return Same data.table with opp_adjustment and player_opp_adj columns added
#' @keywords internal
adjust_epv_for_opponents <- function(player_match,
                                      credit_col = "total_credit",
                                      lambda_decay = EPV_OPP_LAMBDA_DECAY,
                                      prior_games = EPV_OPP_PRIOR_GAMES) {

  dt <- data.table::as.data.table(player_match)

  required <- c("player_id", "match_id", "team_id", "match_date",
                 "minutes_played", credit_col)
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns: {paste(missing, collapse = ', ')}")
  }

  # -- Team-match credit totals --
  team_match <- dt[, .(
    team_credit = sum(get(credit_col), na.rm = TRUE),
    team_total_mins = sum(minutes_played, na.rm = TRUE)
  ), by = .(match_id, team_id, match_date)]

  # Get opponent team_id (requires exactly 2 teams per match)
  match_teams <- team_match[, .(team_id = team_id), by = match_id]
  teams_per_match <- match_teams[, .N, by = match_id]
  bad_matches <- teams_per_match[N != 2]$match_id
  if (length(bad_matches) > 0) {
    cli::cli_warn("{length(bad_matches)} match(es) have != 2 teams - excluding from opponent adjustment")
    team_match <- team_match[!match_id %in% bad_matches]
    match_teams <- match_teams[!match_id %in% bad_matches]
    dt <- dt[!match_id %in% bad_matches]
  }
  match_teams[, opp_team_id := rev(team_id), by = match_id]
  team_match <- match_teams[, .(match_id, team_id, opp_team_id)][
    team_match, on = .(match_id, team_id)]

  # -- Residuals: team over/underperformance vs own average --
  team_season_avg <- team_match[, .(team_avg = mean(team_credit)), by = team_id]
  team_match[team_season_avg, team_avg := i.team_avg, on = "team_id"]
  team_match[, residual := team_credit - team_avg]

  # -- Rolling causal opponent profiles from residuals --
  data.table::setorder(team_match, opp_team_id, match_date)
  team_match[, match_date_num := as.numeric(as.Date(match_date))]

  .compute_rolling_profile <- function(sub_dt) {
    n <- nrow(sub_dt)
    profile <- numeric(n)
    for (i in seq_len(n)) {
      if (i == 1L) {
        profile[i] <- 0
      } else {
        prior <- sub_dt[1:(i - 1)]
        days_since <- sub_dt$match_date_num[i] - prior$match_date_num
        weights <- exp(-lambda_decay * days_since)
        wt_sum <- sum(weights)
        weighted_avg <- sum(weights * prior$residual) / wt_sum
        profile[i] <- (wt_sum * weighted_avg) / (wt_sum + prior_games)
      }
    }
    profile
  }

  team_match[, opp_profile := .compute_rolling_profile(.SD), by = opp_team_id]

  # Adjustment = -profile (tough opponent with negative residual -> positive boost)
  team_match[, opp_adjustment := -opp_profile]

  # -- Distribute to players by minutes share --
  dt <- team_match[, .(match_id, team_id, opp_team_id, opp_adjustment,
    team_total_mins)][dt, on = .(match_id, team_id)]

  dt[, mins_share := minutes_played / pmax(team_total_mins, 1)]
  dt[, player_opp_adj := opp_adjustment * mins_share]

  # Clean up
  dt[, c("team_total_mins", "mins_share") := NULL]

  dt
}
