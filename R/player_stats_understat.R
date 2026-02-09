# Understat Player Statistics Functions
#
# User-facing functions for aggregating Understat player statistics,
# plus the multi-source player_profile() function.


#' Understat Player Summary Statistics
#'
#' Aggregate statistics from Understat data. Includes Understat's xG model.
#'
#' @param player Character. Player name to filter.
#' @param league Character. League code (ENG, ESP, GER, ITA, FRA, RUS).
#' @param season Character. Season in Understat format (e.g., "2024").
#' @param min_minutes Integer. Minimum minutes for inclusion.
#' @param by_team Logical. If TRUE, aggregate by player and team.
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with player statistics including xG, xA, xGChain, xGBuildup.
#'
#' @export
player_understat_summary <- function(player = NULL,
                                      league = NULL,
                                      season = NULL,
                                      min_minutes = 450,
                                      by_team = FALSE,
                                      source = c("remote", "local")) {
  source <- match.arg(source)
  validate_min_minutes(min_minutes)

  # Load Understat roster data
  data <- load_understat_roster(league = league, season = season, source = source)

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_warn("No Understat data found")
    return(data.frame())
  }

  if (!is.null(player)) {
    data <- data[grepl(player, data$player, ignore.case = TRUE), ]
    if (nrow(data) == 0) {
      cli::cli_warn("No data found for player: {player}")
      return(data.frame())
    }
  }

  get_col <- function(df, col) {
    if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
  }

  # Determine team column (Understat data has team_id, not team)
  team_col <- if ("team" %in% names(data)) "team" else "team_id"

  if (by_team) {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "time"),
        goals = get_col(data, "goals"),
        assists = get_col(data, "assists"),
        shots = get_col(data, "shots"),
        key_passes = get_col(data, "key_passes"),
        xg = get_col(data, "x_g"),
        xa = get_col(data, "x_a"),
        xg_chain = get_col(data, "x_g_chain"),
        xg_buildup = get_col(data, "x_g_buildup")
      ) ~ player + data[[team_col]],
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )
    names(result)[2] <- "team"
  } else {
    result <- stats::aggregate(
      cbind(
        matches = 1,
        minutes = get_col(data, "time"),
        goals = get_col(data, "goals"),
        assists = get_col(data, "assists"),
        shots = get_col(data, "shots"),
        key_passes = get_col(data, "key_passes"),
        xg = get_col(data, "x_g"),
        xa = get_col(data, "x_a"),
        xg_chain = get_col(data, "x_g_chain"),
        xg_buildup = get_col(data, "x_g_buildup")
      ) ~ player,
      data = data,
      FUN = function(x) sum(x, na.rm = TRUE),
      na.action = na.pass
    )

    team_mode <- stats::aggregate(
      data[[team_col]] ~ player, data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    names(team_mode)[2] <- "team"
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = "player"]
    data.table::setDF(result)
  }

  # Calculate derived stats
  result$goals_minus_xg <- round(result$goals - result$xg, 2)
  result$goals_per90 <- round(per_90(result$goals, result$minutes), 2)
  result$xg_per90 <- round(per_90(result$xg, result$minutes), 2)
  result$xa_per90 <- round(per_90(result$xa, result$minutes), 2)

  if (is.null(player)) {
    result <- result[result$minutes >= min_minutes, ]
  }

  result <- result[order(-result$minutes), ]
  rownames(result) <- NULL
  as.data.frame(result)
}


#' Player Profile - Combined Statistics Across Sources
#'
#' Combines key statistics from FBref, Opta, and Understat into a single
#' consolidated data frame with one row per player-season-league. Merges
#' the most useful columns from each source.
#'
#' @param player Character. Player name to filter (case-insensitive partial match).
#'   Required.
#' @param league Character. Filter by league code (ENG, ESP, GER, ITA, FRA).
#'   NULL returns all leagues.
#' @param season Character. Filter by FBref season format (e.g., "2024-2025").
#'   NULL returns all seasons.
#' @param min_minutes Integer. Minimum minutes for inclusion (default 450).
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with one row per player-season-league containing:
#'   \itemize{
#'     \item \strong{Identity}: player, team, league, season, minutes
#'     \item \strong{FBref}: goals, assists, xg, npxg, xag, sca, gca,
#'       passes_completed, pass_pct, progressive_passes, key_passes
#'     \item \strong{Opta}: progressive_carries, final_third_entries,
#'       pen_area_entries, big_chances_created (when available)
#'     \item \strong{Understat}: xg_chain, xg_buildup (when available)
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Full profile for a player
#' player_profile("Saka", league = "ENG")
#'
#' # Specific season
#' player_profile("Salah", league = "ENG", season = "2024-2025")
#'
#' # All Big 5 leagues
#' player_profile("Mbappe")
#' }
player_profile <- function(player,
                           league = NULL,
                           season = NULL,
                           min_minutes = 450,
                           source = c("remote", "local")) {
  source <- match.arg(source)

  if (is.null(player) || !nzchar(player)) {
    cli::cli_abort("{.arg player} is required for {.fn player_profile}.")
  }

  # -- FBref Summary --
  fbref_sum <- tryCatch(
    player_fbref_summary(
      player = player, league = league, season = season,
      min_minutes = 0, source = source
    ),
    error = function(e) data.frame()
  )

  if (is.null(fbref_sum) || nrow(fbref_sum) == 0) {
    cli::cli_warn("No FBref data found for player: {player}")
    return(data.frame())
  }

  # Start with FBref summary as the base
  profile <- fbref_sum[, c("player", "team", "matches", "minutes",
                            "goals", "assists", "xg", "npxg", "xag",
                            "sca", "gca", "goals_per90", "xg_per90",
                            "npxg_per90", "xag_per90"), drop = FALSE]

  # -- FBref Passing --
  fbref_pass <- tryCatch(
    player_fbref_passing(
      player = player, league = league, season = season,
      min_minutes = 0, source = source
    ),
    error = function(e) data.frame()
  )

  if (!is.null(fbref_pass) && nrow(fbref_pass) > 0) {
    pass_cols <- fbref_pass[, c("player", "passes_completed", "pass_pct",
                                "progressive_passes", "key_passes",
                                "passes_into_final_third",
                                "passes_into_penalty_area",
                                "progressive_passes_per90",
                                "key_passes_per90"), drop = FALSE]
    profile <- data.table::as.data.table(pass_cols)[data.table::as.data.table(profile), on = "player"]
    data.table::setDF(profile)
  }

  # -- Opta: derive initial-format name ("Bukayo Saka" -> "B. Saka") --
  opta_player <- player
  name_parts <- strsplit(trimws(player), "\\s+")[[1]]
  if (length(name_parts) >= 2) {
    opta_player <- paste0(substr(name_parts[1], 1, 1), ". ",
                          paste(name_parts[-1], collapse = " "))
  }

  # -- Opta Possession (if available) --
  opta_poss <- tryCatch({
    player_opta_possession(
      player = opta_player, league = league, season = season,
      min_minutes = 0, source = "local"
    )
  }, error = function(e) data.frame())

  if (!is.null(opta_poss) && nrow(opta_poss) > 0) {
    opta_cols <- opta_poss[, c("progressive_carries",
                                "final_third_entries", "pen_area_entries",
                                "progressive_carries_per90"), drop = FALSE]
    profile <- cbind(profile, opta_cols)
  }

  # -- Opta Summary (big chances) --
  opta_sum <- tryCatch(
    player_opta_summary(
      player = opta_player, league = league, season = season,
      min_minutes = 0, source = "local"
    ),
    error = function(e) data.frame()
  )

  if (!is.null(opta_sum) && nrow(opta_sum) > 0) {
    opta_sum_cols <- opta_sum[, c("big_chances_created",
                                   "big_chances_scored"), drop = FALSE]
    profile <- cbind(profile, opta_sum_cols)
  }

  # -- Understat (xGChain, xGBuildup) --
  understat <- tryCatch({
    # Convert FBref season to Understat format: "2024-2025" -> "2024"
    us_season <- if (!is.null(season)) substr(season, 1, 4) else NULL
    us_league <- league  # Same codes for Big 5
    player_understat_summary(
      player = player, league = us_league, season = us_season,
      min_minutes = 0, source = source
    )
  }, error = function(e) data.frame())

  if (!is.null(understat) && nrow(understat) > 0) {
    us_cols <- understat[, c("player", "xg_chain", "xg_buildup"), drop = FALSE]
    profile <- data.table::as.data.table(us_cols)[data.table::as.data.table(profile), on = "player"]
    data.table::setDF(profile)
  }

  # Apply min_minutes filter
  profile <- profile[profile$minutes >= min_minutes, , drop = FALSE]

  # Sort by minutes descending
  profile <- profile[order(-profile$minutes), ]
  rownames(profile) <- NULL

  as.data.frame(profile)
}
