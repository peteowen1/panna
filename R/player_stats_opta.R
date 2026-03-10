# Opta Player Statistics Functions
#
# User-facing functions for aggregating Opta player statistics across matches.
# Includes summary, passing, defense, possession, keeper, shots, set pieces,
# and xG/xPass from trained models.


# Internal helper to load Opta data, handling NULL league
.load_opta_data <- function(league, season, source) {
  data <- if (is.null(league)) {
    load_opta_big5(season = season, source = source)
  } else {
    load_opta_stats(league = league, season = season, source = source)
  }
  # Convert to base data.frame for stats::aggregate compatibility
  as.data.frame(data)
}

# Internal helper to load Opta xmetrics data
# Note: source param is accepted for API consistency but xmetrics only supports local
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


# Shared helper for all player_opta_* aggregation functions.
# col_spec: named list mapping output_name -> opta_column_name
# derive_fn: function(result) that adds derived metrics (per-90 rates, etc.)
# col_order: character vector of final column order
# loader: function(league, season, source) for data loading (default .load_opta_data)
.aggregate_opta_player_stats <- function(player, league, season, min_minutes,
                                          by_team, source, col_spec, derive_fn,
                                          col_order, loader = .load_opta_data) {
  source <- match.arg(source, c("remote", "local"))
  validate_min_minutes(min_minutes)

  data <- loader(league, season, source)
  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Opta data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(tolower(player), tolower(data$player_name), fixed = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  # Build aggregation data.table from col_spec
  agg_dt <- data.table::data.table(
    player = data$player_name,
    team = data$team_name,
    matches = 1
  )
  for (nm in names(col_spec)) {
    data.table::set(agg_dt, j = nm, value = .get_col(data, col_spec[[nm]]))
  }
  stat_cols_agg <- c("matches", names(col_spec))

  if (by_team) {
    result <- agg_dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
                     by = .(player, team), .SDcols = stat_cols_agg]
  } else {
    result <- agg_dt[, lapply(.SD, function(x) sum(x, na.rm = TRUE)),
                     by = .(player), .SDcols = stat_cols_agg]
    team_mode <- agg_dt[, .(team = names(which.max(table(team)))), by = player]
    result <- team_mode[result, on = "player"]
  }
  data.table::setDF(result)

  # Apply custom derived metrics
  result <- derive_fn(result)

  # Filter by min_minutes
  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  col_order <- intersect(col_order, names(result))
  result <- result[, col_order]
  rownames(result) <- NULL

  as.data.frame(result)
}


#' Opta Player Summary Statistics
#'
#' Aggregate basic statistics from Opta data. Returns totals and per-90 rates.
#' Note: This uses raw Opta match stats (no xG). For xG/xA, use \code{player_opta_xg()}.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA).
#' @param season Character. Season (e.g., "2024-2025").
#' @param min_minutes Integer. Minimum minutes for inclusion (default 450).
#' @param by_team Logical. If TRUE, aggregate by player and team separately.
#' @param source Character. "remote" (default) or "local".
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
                                 source = c("remote", "local")) {
  .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed", goals = "goals", assists = "goalAssist",
      shots = "totalScoringAtt", shots_on_target = "ontargetScoringAtt",
      big_chances_created = "bigChanceCreated",
      big_chances_scored = "bigChanceScored",
      big_chances_missed = "bigChanceMissed"
    ),
    derive_fn = function(r) {
      r$goals_per90 <- round(per_90(r$goals, r$minutes), 2)
      r$assists_per90 <- round(per_90(r$assists, r$minutes), 2)
      r$shots_per90 <- round(per_90(r$shots, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "goals", "assists", "shots", "shots_on_target",
                  "big_chances_created", "big_chances_scored", "big_chances_missed",
                  "goals_per90", "assists_per90", "shots_per90")
  )
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
                                 source = c("remote", "local")) {
  .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      passes_completed = "accuratePass", passes_attempted = "totalPass",
      long_balls_completed = "accurateLongBalls", long_balls_attempted = "totalLongBalls",
      crosses_completed = "accurateCross", crosses_attempted = "totalCross",
      through_balls_completed = "accurateThroughBall", through_balls_attempted = "totalThroughBall",
      key_passes = "totalAttAssist",
      final_third_passes = "successfulFinalThirdPasses"
    ),
    derive_fn = function(r) {
      r$pass_pct <- round(safe_divide(r$passes_completed * 100, r$passes_attempted), 1)
      r$passes_per90 <- round(per_90(r$passes_completed, r$minutes), 1)
      r$key_passes_per90 <- round(per_90(r$key_passes, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "passes_completed", "passes_attempted",
                  "long_balls_completed", "long_balls_attempted",
                  "crosses_completed", "crosses_attempted",
                  "through_balls_completed", "through_balls_attempted",
                  "key_passes", "final_third_passes",
                  "pass_pct", "passes_per90", "key_passes_per90")
  )
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
                                 source = c("remote", "local")) {
  .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      tackles = "totalTackle", tackles_won = "wonTackle",
      interceptions = "interception", blocks = "outfielderBlock",
      clearances = "totalClearance", ball_recoveries = "ballRecovery",
      duels_won = "duelWon", duels_lost = "duelLost",
      aerials_won = "aerialWon", aerials_lost = "aerialLost",
      poss_won_def3rd = "possWonDef3rd", poss_won_mid3rd = "possWonMid3rd"
    ),
    derive_fn = function(r) {
      r$tackles_per90 <- round(per_90(r$tackles, r$minutes), 2)
      r$interceptions_per90 <- round(per_90(r$interceptions, r$minutes), 2)
      r$tackle_win_pct <- round(safe_divide(r$tackles_won * 100, r$tackles), 1)
      r$aerial_win_pct <- round(safe_divide(r$aerials_won * 100, r$aerials_won + r$aerials_lost), 1)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "tackles", "tackles_won", "interceptions", "blocks",
                  "clearances", "ball_recoveries",
                  "duels_won", "duels_lost", "aerials_won", "aerials_lost",
                  "poss_won_def3rd", "poss_won_mid3rd",
                  "tackles_per90", "interceptions_per90",
                  "tackle_win_pct", "aerial_win_pct")
  )
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
                                    source = c("remote", "local")) {
  .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      touches = "touches", touches_final_third = "touchesInFinalThird",
      touches_opp_box = "touchesInOppBox",
      carries = "carries", progressive_carries = "progressiveCarries",
      final_third_entries = "finalThirdEntries", pen_area_entries = "penAreaEntries",
      dispossessed = "dispossessed", turnovers = "turnover",
      times_tackled = "timesTackled"
    ),
    derive_fn = function(r) {
      r$touches_per90 <- round(per_90(r$touches, r$minutes), 1)
      r$progressive_carries_per90 <- round(per_90(r$progressive_carries, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "touches", "touches_final_third", "touches_opp_box",
                  "carries", "progressive_carries",
                  "final_third_entries", "pen_area_entries",
                  "dispossessed", "turnovers", "times_tackled",
                  "touches_per90", "progressive_carries_per90")
  )
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
                                source = c("remote", "local")) {
  # Custom loader that filters for goalkeepers
  gk_loader <- function(league, season, source) {
    data <- .load_opta_data(league, season, source)
    if (!is.null(data) && nrow(data) > 0) {
      data <- data[grepl("Goalkeeper", data$position, ignore.case = TRUE), ]
    }
    data
  }

  .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      saves = "saves", saves_ibox = "savedIbox", saves_obox = "savedObox",
      goals_conceded = "goalsConceded", goals_conceded_ibox = "goalsConcededIbox",
      shots_conceded_ibox = "attemptsConcededIbox", shots_conceded_obox = "attemptsConcededObox",
      clean_sheets = "cleanSheet",
      diving_saves = "divingSave", high_claims = "goodHighClaim",
      punches = "punches", big_chance_saves = "bigChanceSaves"
    ),
    derive_fn = function(r) {
      r$shots_conceded <- r$shots_conceded_ibox + r$shots_conceded_obox
      shots_on_target_against <- r$saves + r$goals_conceded
      r$save_pct <- round(safe_divide(r$saves * 100, shots_on_target_against), 1)
      r$goals_against_per90 <- round(per_90(r$goals_conceded, r$minutes), 2)
      r$shots_conceded_per90 <- round(per_90(r$shots_conceded, r$minutes), 2)
      r$clean_sheet_pct <- round(safe_divide(r$clean_sheets * 100, r$matches), 1)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "saves", "saves_ibox", "saves_obox",
                  "goals_conceded", "goals_conceded_ibox",
                  "shots_conceded_ibox", "shots_conceded_obox",
                  "clean_sheets", "diving_saves", "high_claims",
                  "punches", "big_chance_saves", "shots_conceded",
                  "save_pct", "goals_against_per90",
                  "shots_conceded_per90", "clean_sheet_pct"),
    loader = gk_loader
  )
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
  result <- .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      shots_inside_box = "attemptsIbox", shots_outside_box = "attemptsObox",
      shots_on_target = "ontargetScoringAtt", shots_off_target = "shotOffTarget",
      shots_blocked = "blockedScoringAtt",
      goals = "goals", goals_inside_box = "attIboxGoal", goals_outside_box = "attOboxGoal",
      headed_goals = "attHdGoal", left_foot_goals = "attLfGoal",
      right_foot_goals = "attRfGoal", penalty_goals = "attPenGoal",
      big_chances_scored = "bigChanceScored", big_chances_missed = "bigChanceMissed",
      hit_woodwork = "hitWoodwork"
    ),
    derive_fn = function(r) {
      r$total_shots <- r$shots_inside_box + r$shots_outside_box
      r$conversion_rate <- round(safe_divide(r$goals, r$total_shots) * 100, 1)
      r$shot_accuracy <- round(safe_divide(r$shots_on_target, r$total_shots) * 100, 1)
      r$goals_per90 <- round(per_90(r$goals, r$minutes), 2)
      r$shots_per90 <- round(per_90(r$total_shots, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "shots_inside_box", "shots_outside_box",
                  "shots_on_target", "shots_off_target", "shots_blocked",
                  "goals", "goals_inside_box", "goals_outside_box",
                  "headed_goals", "left_foot_goals", "right_foot_goals",
                  "penalty_goals", "big_chances_scored", "big_chances_missed",
                  "hit_woodwork", "total_shots",
                  "conversion_rate", "shot_accuracy", "goals_per90", "shots_per90")
  )

  # Re-sort by goals and total shots (helper sorts by minutes)
  if (nrow(result) > 0) {
    result <- result[order(-result$goals, -result$total_shots), ]
    rownames(result) <- NULL
  }
  result
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
#' @param source Character. "remote" (default) or "local".
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
                            source = c("remote", "local")) {
  result <- .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minutes",
      shots = "shots", shots_on_target = "shots_on_target",
      goals = "goals", npgoals = "npgoals",
      xg = "xg", npxg = "npxg",
      key_passes = "key_passes", assists = "assists", xa = "xa"
    ),
    derive_fn = function(r) {
      r$goals_minus_xg <- round(r$goals - r$xg, 2)
      r$xg_per90 <- round(per_90(r$xg, r$minutes), 2)
      r$npxg_per90 <- round(per_90(r$npxg, r$minutes), 2)
      r$xa_per90 <- round(per_90(r$xa, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "minutes",
                  "shots", "shots_on_target", "goals", "npgoals",
                  "xg", "npxg", "goals_minus_xg",
                  "xg_per90", "npxg_per90",
                  "key_passes", "assists", "xa", "xa_per90"),
    loader = .load_opta_xmetrics_data
  )

  # Re-sort by xg (helper sorts by minutes)
  if (nrow(result) > 0) {
    result <- result[order(-result$xg), ]
    rownames(result) <- NULL
  }
  result
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
                               source = c("remote", "local")) {
  result <- .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minutes",
      passes_attempted = "passes_attempted", passes_completed = "passes_completed",
      sum_xpass = "sum_xpass", xpass_overperformance = "xpass_overperformance"
    ),
    derive_fn = function(r) {
      r$pass_pct <- round(safe_divide(r$passes_completed * 100, r$passes_attempted), 1)
      r$xpass_overperformance_per90 <- round(per_90(r$xpass_overperformance, r$minutes), 2)
      r$xpass_avg <- round(safe_divide(r$sum_xpass, r$passes_attempted), 3)
      r
    },
    col_order = c("player", "team", "minutes",
                  "passes_attempted", "passes_completed", "pass_pct",
                  "sum_xpass", "xpass_overperformance", "xpass_overperformance_per90",
                  "xpass_avg"),
    loader = .load_opta_xmetrics_data
  )

  # Re-sort by xpass overperformance (helper sorts by minutes)
  if (nrow(result) > 0) {
    result <- result[order(-result$xpass_overperformance), ]
    rownames(result) <- NULL
  }
  result
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
  result <- .aggregate_opta_player_stats(
    player = player, league = league, season = season,
    min_minutes = min_minutes, by_team = by_team, source = source,
    col_spec = list(
      minutes = "minsPlayed",
      corners_taken = "cornerTaken", corners_accurate = "accurateCornersIntobox",
      corners_won = "wonCorners",
      freekicks_taken = "attFreekickTotal", freekick_goals = "attFreekickGoal",
      freekick_on_target = "attFreekickTarget",
      freekick_crosses = "freekickCross", freekick_crosses_accurate = "accurateFreekickCross",
      penalties_won = "penaltyWon", penalties_conceded = "penaltyConceded",
      setpiece_goals = "attSetpiece", setpiece_assists = "goalAssistSetplay",
      deadball_assists = "goalAssistDeadball",
      throws_total = "totalThrows", throws_accurate = "accurateThrows"
    ),
    derive_fn = function(r) {
      r$corner_accuracy <- round(safe_divide(r$corners_accurate, r$corners_taken) * 100, 1)
      r$throw_accuracy <- round(safe_divide(r$throws_accurate, r$throws_total) * 100, 1)
      r$corners_per90 <- round(per_90(r$corners_taken, r$minutes), 2)
      r
    },
    col_order = c("player", "team", "matches", "minutes",
                  "corners_taken", "corners_accurate", "corners_won",
                  "freekicks_taken", "freekick_goals", "freekick_on_target",
                  "freekick_crosses", "freekick_crosses_accurate",
                  "penalties_won", "penalties_conceded",
                  "setpiece_goals", "setpiece_assists", "deadball_assists",
                  "throws_total", "throws_accurate",
                  "corner_accuracy", "throw_accuracy", "corners_per90")
  )

  # Re-sort by corners and freekicks (helper sorts by minutes)
  if (nrow(result) > 0) {
    result <- result[order(-result$corners_taken, -result$freekicks_taken), ]
    rownames(result) <- NULL
  }
  result
}
