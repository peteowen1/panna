# FBref Player Statistics Functions
#
# User-facing functions for aggregating FBref player statistics across matches.
# Each function focuses on a specific stat category with ~15 metrics max.


#' FBref Player Summary Statistics
#'
#' Aggregate offensive statistics from FBref data. Returns totals and per-90 rates.
#' Includes xG metrics from StatsBomb.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#'   NULL returns all players.
#' @param league Character. Filter by league code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Filter by season (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion (default 450).
#' @param by_team Logical. If TRUE, aggregate by player and team separately.
#'   If FALSE (default), aggregate across all teams and show most common team.
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with columns: player, team, matches, minutes,
#'   goals, assists, shots, shots_on_target, xg, npxg, xag, sca, gca,
#'   goals_minus_xg, goals_per90, xg_per90, npxg_per90, xag_per90
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get all players with 450+ minutes
#' player_fbref_summary(league = "ENG", season = "2024-2025")
#'
#' # Get specific player
#' player_fbref_summary(player = "Salah", league = "ENG")
#'
#' # Group by team (useful for players who transferred)
#' player_fbref_summary(league = "ENG", season = "2024-2025", by_team = TRUE)
#' }
player_fbref_summary <- function(player = NULL,
                                  league = NULL,
                                  season = NULL,
                                  min_minutes = 450,
                                  by_team = FALSE,
                                  source = c("remote", "local")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load data
  data <- load_summary(league = league, season = season, source = source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No data found for specified filters")
    return(data.frame())
  }

  # Filter by player if specified (case-insensitive partial match)
  if (!is.null(player)) {
    data <- data[grepl(player, data$player, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Aggregate by player (and optionally team)
  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = as.numeric(min),
        goals = as.numeric(gls),
        assists = as.numeric(ast),
        shots = as.numeric(sh),
        shots_on_target = as.numeric(so_t),
        xg = as.numeric(x_g),
        npxg = as.numeric(npx_g),
        xag = as.numeric(x_ag),
        sca = as.numeric(sca),
        gca = as.numeric(gca)
      ) ~ player + team,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = as.numeric(min),
        goals = as.numeric(gls),
        assists = as.numeric(ast),
        shots = as.numeric(sh),
        shots_on_target = as.numeric(so_t),
        xg = as.numeric(x_g),
        npxg = as.numeric(npx_g),
        xag = as.numeric(x_ag),
        sca = as.numeric(sca),
        gca = as.numeric(gca)
      ) ~ player,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    # Get most frequent team per player
    team_mode <- stats::aggregate(
      team ~ player,
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)
  }

  # Calculate derived stats
  result$goals_minus_xg <- round(result$goals - result$xg, 2)
  result$goals_per90 <- round(per_90(result$goals, result$minutes), 2)
  result$xg_per90 <- round(per_90(result$xg, result$minutes), 2)
  result$npxg_per90 <- round(per_90(result$npxg, result$minutes), 2)
  result$xag_per90 <- round(per_90(result$xag, result$minutes), 2)

  # Apply min_minutes filter (unless specific player)
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  # Sort by minutes and reorder columns
  result <- result[order(-result$minutes), ]
  col_order <- c(
    "player", "team", "matches", "minutes",
    "goals", "assists", "shots", "shots_on_target",
    "xg", "npxg", "xag", "sca", "gca",
    "goals_minus_xg", "goals_per90", "xg_per90", "npxg_per90", "xag_per90"
  )
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' FBref Player Passing Statistics
#'
#' Aggregate passing statistics from FBref data. Returns totals and per-90 rates.
#'
#' @inheritParams player_fbref_summary
#'
#' @return Data frame with columns: player, team, matches, minutes,
#'   passes_completed, passes_attempted, pass_pct, progressive_passes,
#'   key_passes, passes_into_final_third, passes_into_penalty_area, crosses,
#'   passes_per90, progressive_passes_per90, key_passes_per90
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_fbref_passing(league = "ENG", season = "2024-2025")
#' }
player_fbref_passing <- function(player = NULL,
                                  league = NULL,
                                  season = NULL,
                                  min_minutes = 450,
                                  by_team = FALSE,
                                  source = c("remote", "local")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load data
  data <- load_passing(league = league, season = season, source = source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No data found for specified filters")
    return(data.frame())
  }

  # Filter by player if specified
  if (!is.null(player)) {
    data <- data[grepl(player, data$player, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Need minutes - load summary to get it
  summary_data <- load_summary(league = league, season = season, source = source)

  if (by_team) {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player + team,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[3] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        passes_completed = as.numeric(cmp),
        passes_attempted = as.numeric(att),
        progressive_passes = as.numeric(prg_p),
        key_passes = as.numeric(kp),
        passes_into_final_third = as.numeric(x1_3),
        passes_into_penalty_area = as.numeric(ppa),
        crosses = as.numeric(crs_pa)
      ) ~ player + team,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = c("player", "team")]
    data.table::setDF(result)
  } else {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[2] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        passes_completed = as.numeric(cmp),
        passes_attempted = as.numeric(att),
        progressive_passes = as.numeric(prg_p),
        key_passes = as.numeric(kp),
        passes_into_final_third = as.numeric(x1_3),
        passes_into_penalty_area = as.numeric(ppa),
        crosses = as.numeric(crs_pa)
      ) ~ player,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)

    # Get most frequent team per player
    team_mode <- stats::aggregate(
      team ~ player,
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)
  }

  # Calculate derived stats
  result$pass_pct <- round(safe_divide(result$passes_completed * 100, result$passes_attempted), 1)
  result$passes_per90 <- round(per_90(result$passes_completed, result$minutes), 1)
  result$progressive_passes_per90 <- round(per_90(result$progressive_passes, result$minutes), 2)
  result$key_passes_per90 <- round(per_90(result$key_passes, result$minutes), 2)

  # Apply min_minutes filter (unless specific player)
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  # Sort by minutes and reorder columns
  result <- result[order(-result$minutes), ]
  col_order <- c(
    "player", "team", "matches", "minutes",
    "passes_completed", "passes_attempted", "pass_pct", "progressive_passes",
    "key_passes", "passes_into_final_third", "passes_into_penalty_area", "crosses",
    "passes_per90", "progressive_passes_per90", "key_passes_per90"
  )
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' FBref Player Defense Statistics
#'
#' Aggregate defensive statistics from FBref data. Returns totals and per-90 rates.
#'
#' @inheritParams player_fbref_summary
#'
#' @return Data frame with columns: player, team, matches, minutes,
#'   tackles, tackles_won, interceptions, blocks, clearances, errors,
#'   tackles_per90, interceptions_per90, blocks_per90, tackle_win_pct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_fbref_defense(league = "ENG", season = "2024-2025")
#' }
player_fbref_defense <- function(player = NULL,
                                  league = NULL,
                                  season = NULL,
                                  min_minutes = 450,
                                  by_team = FALSE,
                                  source = c("remote", "local")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load data
  data <- load_defense(league = league, season = season, source = source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No data found for specified filters")
    return(data.frame())
  }

  # Filter by player if specified
  if (!is.null(player)) {
    data <- data[grepl(player, data$player, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Need minutes - load summary to get it
  summary_data <- load_summary(league = league, season = season, source = source)

  if (by_team) {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player + team,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[3] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        tackles = as.numeric(tkl),
        tackles_won = as.numeric(tkl_w),
        interceptions = as.numeric(int),
        blocks = as.numeric(blocks),
        clearances = as.numeric(clr),
        errors = as.numeric(err)
      ) ~ player + team,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = c("player", "team")]
    data.table::setDF(result)
  } else {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[2] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        tackles = as.numeric(tkl),
        tackles_won = as.numeric(tkl_w),
        interceptions = as.numeric(int),
        blocks = as.numeric(blocks),
        clearances = as.numeric(clr),
        errors = as.numeric(err)
      ) ~ player,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)

    # Get most frequent team per player
    team_mode <- stats::aggregate(
      team ~ player,
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)
  }

  # Calculate derived stats
  result$tackles_per90 <- round(per_90(result$tackles, result$minutes), 2)
  result$interceptions_per90 <- round(per_90(result$interceptions, result$minutes), 2)
  result$blocks_per90 <- round(per_90(result$blocks, result$minutes), 2)
  result$tackle_win_pct <- round(safe_divide(result$tackles_won * 100, result$tackles), 1)

  # Apply min_minutes filter (unless specific player)
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  # Sort by minutes and reorder columns
  result <- result[order(-result$minutes), ]
  col_order <- c(
    "player", "team", "matches", "minutes",
    "tackles", "tackles_won", "interceptions", "blocks", "clearances", "errors",
    "tackles_per90", "interceptions_per90", "blocks_per90", "tackle_win_pct"
  )
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' FBref Player Keeper Statistics
#'
#' Aggregate goalkeeper statistics from FBref data. Returns totals and rates.
#'
#' @inheritParams player_fbref_summary
#'
#' @return Data frame with columns: player, team, matches, minutes,
#'   shots_on_target_against, saves, goals_against, clean_sheets,
#'   save_pct, goals_against_per90, clean_sheet_pct
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_fbref_keeper(league = "ENG", season = "2024-2025")
#' }
player_fbref_keeper <- function(player = NULL,
                                 league = NULL,
                                 season = NULL,
                                 min_minutes = 450,
                                 by_team = FALSE,
                                 source = c("remote", "local")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load keeper data
  data <- load_keeper(league = league, season = season, source = source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No keeper data found for specified filters")
    return(data.frame())
  }

  # Filter by player if specified
  if (!is.null(player)) {
    data <- data[grepl(player, data$player, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Need minutes - load summary to get it (filter for GK position)
  summary_data <- load_summary(league = league, season = season, source = source)
  summary_data <- summary_data[grepl("GK", summary_data$pos, ignore.case = TRUE), ]

  # Calculate clean sheets from the data (goals_against == 0)
  data$clean_sheet <- as.numeric(data$ga) == 0

  if (by_team) {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player + team,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[3] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        shots_on_target_against = as.numeric(so_ta),
        saves = as.numeric(saves),
        goals_against = as.numeric(ga),
        clean_sheets = as.numeric(clean_sheet)
      ) ~ player + team,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = c("player", "team")]
    data.table::setDF(result)
  } else {
    minutes_df <- stats::aggregate(
      as.numeric(min) ~ player,
      data = summary_data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(minutes_df)[2] <- "minutes"

    result <- stats::aggregate(
      cbind(
        matches = 1,
        shots_on_target_against = as.numeric(so_ta),
        saves = as.numeric(saves),
        goals_against = as.numeric(ga),
        clean_sheets = as.numeric(clean_sheet)
      ) ~ player,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    result <- data.table::as.data.table(minutes_df)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)

    # Get most frequent team per player
    team_mode <- stats::aggregate(
      team ~ player,
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)
  }

  # Calculate derived stats
  result$save_pct <- round(safe_divide(result$saves * 100, result$shots_on_target_against), 1)
  result$goals_against_per90 <- round(per_90(result$goals_against, result$minutes), 2)
  result$clean_sheet_pct <- round(safe_divide(result$clean_sheets * 100, result$matches), 1)

  # Apply min_minutes filter (unless specific player)
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  # Sort by minutes and reorder columns
  result <- result[order(-result$minutes), ]
  col_order <- c(
    "player", "team", "matches", "minutes",
    "shots_on_target_against", "saves", "goals_against", "clean_sheets",
    "save_pct", "goals_against_per90", "clean_sheet_pct"
  )
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}
