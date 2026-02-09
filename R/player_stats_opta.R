# Opta Player Statistics Functions
#
# User-facing functions for aggregating Opta player statistics across matches.
# Includes summary, passing, defense, possession, keeper, shots, set pieces,
# and xG/xPass from trained models.


# Internal helper to load Opta data, handling NULL league
.load_opta_data <- function(league, season, source) {
  data <- if (is.null(league)) {
    # Load all Big 5 leagues (load_opta_big5 only supports local)
    load_opta_big5(season = season)
  } else {
    load_opta_stats(league = league, season = season, source = source)
  }
  # Convert to base data.frame for stats::aggregate compatibility
  as.data.frame(data)
}

# Internal helper to load Opta xmetrics data
.load_opta_xmetrics_data <- function(league, season, source) {
  data <- if (is.null(league)) {
    # Load all Big 5 leagues
    results <- lapply(names(OPTA_LEAGUES), function(lg) {
      tryCatch({
        df <- load_opta_xmetrics(lg, season)
        df$league <- lg
        df
      }, error = function(e) NULL)
    })
    rbindlist(Filter(Negate(is.null), results), use.names = TRUE, fill = TRUE)
  } else {
    load_opta_xmetrics(league = league, season = season)
  }
  as.data.frame(data)
}


#' Opta Player Summary Statistics
#'
#' Aggregate basic statistics from Opta data. Returns totals and per-90 rates.
#' Note: Opta does not have xG data.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Season (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion (default 450).
#' @param by_team Logical. If TRUE, aggregate by player and team separately.
#' @param source Character. "local" (default) or "remote".
#'
#' @return Data frame with player summary statistics.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' player_opta_summary(league = "ENG", season = "2024-2025")
#' }
player_opta_summary <- function(player = NULL,
                                 league = NULL,
                                 season = NULL,
                                 min_minutes = 450,
                                 by_team = FALSE,
                                 source = c("local", "remote")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load data
  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found for specified filters")
    return(data.frame())
  }

  # Filter by player if specified
  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Define columns to aggregate (using Opta column names)
  # Use coalesce to handle missing columns
  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        goals = get_col(data, "goals"),
        assists = get_col(data, "goalAssist"),
        shots = get_col(data, "totalScoringAtt"),
        shots_on_target = get_col(data, "ontargetScoringAtt"),
        big_chances_created = get_col(data, "bigChanceCreated"),
        big_chances_scored = get_col(data, "bigChanceScored"),
        big_chances_missed = get_col(data, "bigChanceMissed")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        goals = get_col(data, "goals"),
        assists = get_col(data, "goalAssist"),
        shots = get_col(data, "totalScoringAtt"),
        shots_on_target = get_col(data, "ontargetScoringAtt"),
        big_chances_created = get_col(data, "bigChanceCreated"),
        big_chances_scored = get_col(data, "bigChanceScored"),
        big_chances_missed = get_col(data, "bigChanceMissed")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    # Get most frequent team
    team_mode <- stats::aggregate(
      team_name ~ player_name,
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate per-90 rates
  result$goals_per90 <- round(per_90(result$goals, result$minutes), 2)
  result$assists_per90 <- round(per_90(result$assists, result$minutes), 2)
  result$shots_per90 <- round(per_90(result$shots, result$minutes), 2)

  # Apply min_minutes filter
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  # Sort and reorder
  result <- result[order(-result$minutes), ]
  col_order <- c("player", "team", "matches", "minutes",
                 "goals", "assists", "shots", "shots_on_target",
                 "big_chances_created", "big_chances_scored", "big_chances_missed",
                 "goals_per90", "assists_per90", "shots_per90")
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' Opta Player Passing Statistics
#'
#' Aggregate passing statistics from Opta data.
#'
#' @inheritParams player_opta_summary
#'
#' @return Data frame with player passing statistics.
#'
#' @export
player_opta_passing <- function(player = NULL,
                                 league = NULL,
                                 season = NULL,
                                 min_minutes = 450,
                                 by_team = FALSE,
                                 source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        passes_completed = get_col(data, "accuratePass"),
        passes_attempted = get_col(data, "totalPass"),
        long_balls_completed = get_col(data, "accurateLongBalls"),
        long_balls_attempted = get_col(data, "totalLongBalls"),
        crosses_completed = get_col(data, "accurateCross"),
        crosses_attempted = get_col(data, "totalCross"),
        through_balls_completed = get_col(data, "accurateThroughBall"),
        through_balls_attempted = get_col(data, "totalThroughBall"),
        key_passes = get_col(data, "totalAttAssist"),
        final_third_passes = get_col(data, "successfulFinalThirdPasses")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        passes_completed = get_col(data, "accuratePass"),
        passes_attempted = get_col(data, "totalPass"),
        long_balls_completed = get_col(data, "accurateLongBalls"),
        long_balls_attempted = get_col(data, "totalLongBalls"),
        crosses_completed = get_col(data, "accurateCross"),
        crosses_attempted = get_col(data, "totalCross"),
        through_balls_completed = get_col(data, "accurateThroughBall"),
        through_balls_attempted = get_col(data, "totalThroughBall"),
        key_passes = get_col(data, "totalAttAssist"),
        final_third_passes = get_col(data, "successfulFinalThirdPasses")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate rates
  result$pass_pct <- round(safe_divide(result$passes_completed * 100, result$passes_attempted), 1)
  result$passes_per90 <- round(per_90(result$passes_completed, result$minutes), 1)
  result$key_passes_per90 <- round(per_90(result$key_passes, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Opta Player Defense Statistics
#'
#' Aggregate defensive statistics from Opta data.
#'
#' @inheritParams player_opta_summary
#'
#' @return Data frame with player defensive statistics.
#'
#' @export
player_opta_defense <- function(player = NULL,
                                 league = NULL,
                                 season = NULL,
                                 min_minutes = 450,
                                 by_team = FALSE,
                                 source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        tackles = get_col(data, "totalTackle"),
        tackles_won = get_col(data, "wonTackle"),
        interceptions = get_col(data, "interception"),
        blocks = get_col(data, "outfielderBlock"),
        clearances = get_col(data, "totalClearance"),
        ball_recoveries = get_col(data, "ballRecovery"),
        duels_won = get_col(data, "duelWon"),
        duels_lost = get_col(data, "duelLost"),
        aerials_won = get_col(data, "aerialWon"),
        aerials_lost = get_col(data, "aerialLost"),
        poss_won_def3rd = get_col(data, "possWonDef3rd"),
        poss_won_mid3rd = get_col(data, "possWonMid3rd")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        tackles = get_col(data, "totalTackle"),
        tackles_won = get_col(data, "wonTackle"),
        interceptions = get_col(data, "interception"),
        blocks = get_col(data, "outfielderBlock"),
        clearances = get_col(data, "totalClearance"),
        ball_recoveries = get_col(data, "ballRecovery"),
        duels_won = get_col(data, "duelWon"),
        duels_lost = get_col(data, "duelLost"),
        aerials_won = get_col(data, "aerialWon"),
        aerials_lost = get_col(data, "aerialLost"),
        poss_won_def3rd = get_col(data, "possWonDef3rd"),
        poss_won_mid3rd = get_col(data, "possWonMid3rd")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate rates
  result$tackles_per90 <- round(per_90(result$tackles, result$minutes), 2)
  result$interceptions_per90 <- round(per_90(result$interceptions, result$minutes), 2)
  result$tackle_win_pct <- round(safe_divide(result$tackles_won * 100, result$tackles), 1)
  result$aerial_win_pct <- round(safe_divide(result$aerials_won * 100, result$aerials_won + result$aerials_lost), 1)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Opta Player Possession Statistics
#'
#' Aggregate possession and carrying statistics from Opta data.
#'
#' @inheritParams player_opta_summary
#'
#' @return Data frame with player possession statistics.
#'
#' @export
player_opta_possession <- function(player = NULL,
                                    league = NULL,
                                    season = NULL,
                                    min_minutes = 450,
                                    by_team = FALSE,
                                    source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        touches = get_col(data, "touches"),
        touches_final_third = get_col(data, "touchesInFinalThird"),
        touches_opp_box = get_col(data, "touchesInOppBox"),
        carries = get_col(data, "carries"),
        progressive_carries = get_col(data, "progressiveCarries"),
        final_third_entries = get_col(data, "finalThirdEntries"),
        pen_area_entries = get_col(data, "penAreaEntries"),
        dispossessed = get_col(data, "dispossessed"),
        turnovers = get_col(data, "turnover"),
        times_tackled = get_col(data, "timesTackled")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        touches = get_col(data, "touches"),
        touches_final_third = get_col(data, "touchesInFinalThird"),
        touches_opp_box = get_col(data, "touchesInOppBox"),
        carries = get_col(data, "carries"),
        progressive_carries = get_col(data, "progressiveCarries"),
        final_third_entries = get_col(data, "finalThirdEntries"),
        pen_area_entries = get_col(data, "penAreaEntries"),
        dispossessed = get_col(data, "dispossessed"),
        turnovers = get_col(data, "turnover"),
        times_tackled = get_col(data, "timesTackled")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate rates
  result$touches_per90 <- round(per_90(result$touches, result$minutes), 1)
  result$progressive_carries_per90 <- round(per_90(result$progressive_carries, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Opta Player Keeper Statistics
#'
#' Aggregate goalkeeper statistics from Opta data.
#'
#' @inheritParams player_opta_summary
#'
#' @return Data frame with columns: player, team, matches, minutes,
#'   saves, saves_ibox, saves_obox, goals_conceded, goals_conceded_ibox,
#'   shots_conceded_ibox, shots_conceded_obox, clean_sheets, diving_saves,
#'   high_claims, punches, big_chance_saves, shots_conceded,
#'   save_pct, goals_against_per90, shots_conceded_per90, clean_sheet_pct
#'
#' @export
player_opta_keeper <- function(player = NULL,
                                league = NULL,
                                season = NULL,
                                min_minutes = 450,
                                by_team = FALSE,
                                source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  # Filter for goalkeepers
  data <- data[grepl("Goalkeeper", data$position, ignore.case = TRUE), ]

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        saves = get_col(data, "saves"),
        saves_ibox = get_col(data, "savedIbox"),
        saves_obox = get_col(data, "savedObox"),
        goals_conceded = get_col(data, "goalsConceded"),
        goals_conceded_ibox = get_col(data, "goalsConcededIbox"),
        shots_conceded_ibox = get_col(data, "attemptsConcededIbox"),
        shots_conceded_obox = get_col(data, "attemptsConcededObox"),
        clean_sheets = get_col(data, "cleanSheet"),
        diving_saves = get_col(data, "divingSave"),
        high_claims = get_col(data, "goodHighClaim"),
        punches = get_col(data, "punches"),
        big_chance_saves = get_col(data, "bigChanceSaves")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        saves = get_col(data, "saves"),
        saves_ibox = get_col(data, "savedIbox"),
        saves_obox = get_col(data, "savedObox"),
        goals_conceded = get_col(data, "goalsConceded"),
        goals_conceded_ibox = get_col(data, "goalsConcededIbox"),
        shots_conceded_ibox = get_col(data, "attemptsConcededIbox"),
        shots_conceded_obox = get_col(data, "attemptsConcededObox"),
        clean_sheets = get_col(data, "cleanSheet"),
        diving_saves = get_col(data, "divingSave"),
        high_claims = get_col(data, "goodHighClaim"),
        punches = get_col(data, "punches"),
        big_chance_saves = get_col(data, "bigChanceSaves")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate derived stats
  result$shots_conceded <- result$shots_conceded_ibox + result$shots_conceded_obox
  shots_on_target_against <- result$saves + result$goals_conceded
  result$save_pct <- round(safe_divide(result$saves * 100, shots_on_target_against), 1)
  result$goals_against_per90 <- round(per_90(result$goals_conceded, result$minutes), 2)
  result$shots_conceded_per90 <- round(per_90(result$shots_conceded, result$minutes), 2)
  result$clean_sheet_pct <- round(safe_divide(result$clean_sheets * 100, result$matches), 1)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Opta Player Shooting Statistics
#'
#' Aggregate shooting statistics from Opta data. Includes shot volume,
#' shot locations, finishing by body part, and big chances.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Season string (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion.
#' @param by_team Logical. If TRUE, aggregate by player and team.
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with shooting statistics.
#'
#' @export
player_opta_shots <- function(player = NULL,
                               league = NULL,
                               season = NULL,
                               min_minutes = 450,
                               by_team = FALSE,
                               source = c("remote", "local")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        shots_inside_box = get_col(data, "attemptsIbox"),
        shots_outside_box = get_col(data, "attemptsObox"),
        shots_on_target = get_col(data, "ontargetScoringAtt"),
        shots_off_target = get_col(data, "shotOffTarget"),
        shots_blocked = get_col(data, "blockedScoringAtt"),
        goals = get_col(data, "goals"),
        goals_inside_box = get_col(data, "attIboxGoal"),
        goals_outside_box = get_col(data, "attOboxGoal"),
        headed_goals = get_col(data, "attHdGoal"),
        left_foot_goals = get_col(data, "attLfGoal"),
        right_foot_goals = get_col(data, "attRfGoal"),
        penalty_goals = get_col(data, "attPenGoal"),
        big_chances_scored = get_col(data, "bigChanceScored"),
        big_chances_missed = get_col(data, "bigChanceMissed"),
        hit_woodwork = get_col(data, "hitWoodwork")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        shots_inside_box = get_col(data, "attemptsIbox"),
        shots_outside_box = get_col(data, "attemptsObox"),
        shots_on_target = get_col(data, "ontargetScoringAtt"),
        shots_off_target = get_col(data, "shotOffTarget"),
        shots_blocked = get_col(data, "blockedScoringAtt"),
        goals = get_col(data, "goals"),
        goals_inside_box = get_col(data, "attIboxGoal"),
        goals_outside_box = get_col(data, "attOboxGoal"),
        headed_goals = get_col(data, "attHdGoal"),
        left_foot_goals = get_col(data, "attLfGoal"),
        right_foot_goals = get_col(data, "attRfGoal"),
        penalty_goals = get_col(data, "attPenGoal"),
        big_chances_scored = get_col(data, "bigChanceScored"),
        big_chances_missed = get_col(data, "bigChanceMissed"),
        hit_woodwork = get_col(data, "hitWoodwork")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate derived stats
  result$total_shots <- result$shots_inside_box + result$shots_outside_box
  result$conversion_rate <- round(safe_divide(result$goals, result$total_shots) * 100, 1)
  result$shot_accuracy <- round(safe_divide(result$shots_on_target, result$total_shots) * 100, 1)
  result$goals_per90 <- round(per_90(result$goals, result$minutes), 2)
  result$shots_per90 <- round(per_90(result$total_shots, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$goals, -result$total_shots), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Opta Player xG and xA Statistics
#'
#' Aggregate shooting and assisting statistics with xG/xA from our trained
#' Opta xG model. Requires pre-computed xmetrics parquet files.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Season (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion (default 450).
#' @param by_team Logical. If TRUE, aggregate by player and team separately.
#' @param source Character. "local" (default) or "remote".
#'
#' @return Data frame with xG/xA statistics per player.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Top xG in EPL
#' player_opta_xg(league = "ENG", season = "2024-2025")
#'
#' # Specific player
#' player_opta_xg("B. Saka", league = "ENG")
#' }
player_opta_xg <- function(player = NULL,
                            league = NULL,
                            season = NULL,
                            min_minutes = 450,
                            by_team = FALSE,
                            source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_xmetrics_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta xmetrics data found. Run 03_calculate_player_xmetrics.R first.")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        minutes = get_col(data, "minutes"),
        shots = get_col(data, "shots"),
        shots_on_target = get_col(data, "shots_on_target"),
        goals = get_col(data, "goals"),
        npgoals = get_col(data, "npgoals"),
        xg = get_col(data, "xg"),
        npxg = get_col(data, "npxg"),
        key_passes = get_col(data, "key_passes"),
        assists = get_col(data, "assists"),
        xa = get_col(data, "xa")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        minutes = get_col(data, "minutes"),
        shots = get_col(data, "shots"),
        shots_on_target = get_col(data, "shots_on_target"),
        goals = get_col(data, "goals"),
        npgoals = get_col(data, "npgoals"),
        xg = get_col(data, "xg"),
        npxg = get_col(data, "npxg"),
        key_passes = get_col(data, "key_passes"),
        assists = get_col(data, "assists"),
        xa = get_col(data, "xa")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(
      team_name ~ player_name, data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Derived stats
  result$goals_minus_xg <- round(result$goals - result$xg, 2)
  result$xg_per90 <- round(per_90(result$xg, result$minutes), 2)
  result$npxg_per90 <- round(per_90(result$npxg, result$minutes), 2)
  result$xa_per90 <- round(per_90(result$xa, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$xg), ]
  col_order <- c("player", "team", "minutes",
                 "shots", "shots_on_target", "goals", "npgoals",
                 "xg", "npxg", "goals_minus_xg",
                 "xg_per90", "npxg_per90",
                 "key_passes", "assists", "xa", "xa_per90")
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' Opta Player xPass Statistics
#'
#' Aggregate passing statistics with xPass (expected pass completion) from
#' our trained model. Shows passing volume, accuracy, and overperformance.
#'
#' @inheritParams player_opta_xg
#'
#' @return Data frame with xPass statistics per player.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Top xPass overperformers
#' player_opta_xpass(league = "ENG", season = "2024-2025")
#'
#' # Specific player
#' player_opta_xpass("B. Saka", league = "ENG")
#' }
player_opta_xpass <- function(player = NULL,
                               league = NULL,
                               season = NULL,
                               min_minutes = 450,
                               by_team = FALSE,
                               source = c("local", "remote")) {
  source <- match.arg(source)

  data <- .load_opta_xmetrics_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta xmetrics data found. Run 03_calculate_player_xmetrics.R first.")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        minutes = get_col(data, "minutes"),
        passes_attempted = get_col(data, "passes_attempted"),
        passes_completed = get_col(data, "passes_completed"),
        sum_xpass = get_col(data, "sum_xpass"),
        xpass_overperformance = get_col(data, "xpass_overperformance")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        minutes = get_col(data, "minutes"),
        passes_attempted = get_col(data, "passes_attempted"),
        passes_completed = get_col(data, "passes_completed"),
        sum_xpass = get_col(data, "sum_xpass"),
        xpass_overperformance = get_col(data, "xpass_overperformance")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(
      team_name ~ player_name, data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Derived stats
  result$pass_pct <- round(safe_divide(result$passes_completed * 100, result$passes_attempted), 1)
  result$xpass_overperformance_per90 <- round(per_90(result$xpass_overperformance, result$minutes), 2)
  result$xpass_avg <- round(safe_divide(result$sum_xpass, result$passes_attempted), 3)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$xpass_overperformance), ]
  col_order <- c("player", "team", "minutes",
                 "passes_attempted", "passes_completed", "pass_pct",
                 "sum_xpass", "xpass_overperformance", "xpass_overperformance_per90",
                 "xpass_avg")
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' Opta Player Set Piece Statistics
#'
#' Aggregate set piece statistics from Opta data. Includes corners,
#' free kicks, penalties, and set piece involvement.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Season string (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion.
#' @param by_team Logical. If TRUE, aggregate by player and team.
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with set piece statistics.
#'
#' @export
player_opta_setpiece <- function(player = NULL,
                                  league = NULL,
                                  season = NULL,
                                  min_minutes = 450,
                                  by_team = FALSE,
                                  source = c("remote", "local")) {
  source <- match.arg(source)

  data <- .load_opta_data(league, season, source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player_name, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        corners_taken = get_col(data, "cornerTaken"),
        corners_accurate = get_col(data, "accurateCornersIntobox"),
        corners_won = get_col(data, "wonCorners"),
        freekicks_taken = get_col(data, "attFreekickTotal"),
        freekick_goals = get_col(data, "attFreekickGoal"),
        freekick_on_target = get_col(data, "attFreekickTarget"),
        freekick_crosses = get_col(data, "freekickCross"),
        freekick_crosses_accurate = get_col(data, "accurateFreekickCross"),
        penalties_won = get_col(data, "penaltyWon"),
        penalties_conceded = get_col(data, "penaltyConceded"),
        setpiece_goals = get_col(data, "attSetpiece"),
        setpiece_assists = get_col(data, "goalAssistSetplay"),
        deadball_assists = get_col(data, "goalAssistDeadball"),
        throws_total = get_col(data, "totalThrows"),
        throws_accurate = get_col(data, "accurateThrows")
      ) ~ player_name + team_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1:2] <- c("player", "team")
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "minsPlayed"),
        corners_taken = get_col(data, "cornerTaken"),
        corners_accurate = get_col(data, "accurateCornersIntobox"),
        corners_won = get_col(data, "wonCorners"),
        freekicks_taken = get_col(data, "attFreekickTotal"),
        freekick_goals = get_col(data, "attFreekickGoal"),
        freekick_on_target = get_col(data, "attFreekickTarget"),
        freekick_crosses = get_col(data, "freekickCross"),
        freekick_crosses_accurate = get_col(data, "accurateFreekickCross"),
        penalties_won = get_col(data, "penaltyWon"),
        penalties_conceded = get_col(data, "penaltyConceded"),
        setpiece_goals = get_col(data, "attSetpiece"),
        setpiece_assists = get_col(data, "goalAssistSetplay"),
        deadball_assists = get_col(data, "goalAssistDeadball"),
        throws_total = get_col(data, "totalThrows"),
        throws_accurate = get_col(data, "accurateThrows")
      ) ~ player_name,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[1] <- "player"

    team_mode <- stats::aggregate(team_name ~ player_name, data = data,
                                   FUN = function(x) names(which.max(table(x))))
    names(team_mode) <- c("player", "team")
    result <- merge(result, team_mode, by = "player", all.x = TRUE)
  }

  # Calculate derived stats
  result$corner_accuracy <- round(safe_divide(result$corners_accurate, result$corners_taken) * 100, 1)
  result$throw_accuracy <- round(safe_divide(result$throws_accurate, result$throws_total) * 100, 1)
  result$corners_per90 <- round(per_90(result$corners_taken, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$corners_taken, -result$freekicks_taken), ]
  rownames(result) <- NULL
  as.data.frame(result)
}
