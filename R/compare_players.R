# Player Comparison Functions
#
# Side-by-side comparison of players across multiple stat categories.

#' Compare Two or More Players
#'
#' Pulls stats from multiple sources (xG, xPass, chains, defense, possession)
#' and presents a consolidated side-by-side comparison. All stats are per-90
#' where applicable.
#'
#' @param players Character vector of player names (case-insensitive partial match).
#' @param league Character. League code to filter (NULL for all leagues).
#' @param season Character. Season to filter (NULL for all seasons combined).
#' @param min_minutes Numeric. Minimum minutes for inclusion (default 0 when
#'   players are specified).
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with one row per player and columns grouped by category:
#'   identity, shooting, creating, passing, chains, defending, possession.
#'
#' @family player statistics
#' @export
#' @examples
#' \dontrun{
#' compare_players(c("Salah", "Mbapp\u00e9", "Haaland"))
#' compare_players(c("B. Saka", "Lamine Yamal"), league = "ESP", season = "2025-2026")
#' compare_players(c("V. van Dijk", "W. Saliba"), league = "ENG", season = "2024-2025")
#' }
compare_players <- function(players,
                             league = NULL,
                             season = NULL,
                             min_minutes = 0,
                             source = c("remote", "local")) {
  source <- match.arg(source)

  if (length(players) < 1) {
    cli::cli_abort("Provide at least one player name.")
  }

  # Load xMetrics data once (shared by xG, xPass, chains)
  xm_data <- .load_opta_xmetrics_data(league, season, source)
  if (is.null(xm_data) || nrow(xm_data) == 0) {
    cli::cli_warn("No xMetrics data found")
    return(data.frame())
  }


  # Filter to requested players using word-boundary matching
  # "Saka" matches "B. Saka" but NOT "Wan-Bissaka"
  player_mask <- rep(FALSE, nrow(xm_data))
  lower_names <- tolower(xm_data$player_name)
  for (p in players) {
    # Try exact surname/name match with word boundaries first
    pattern <- paste0("\\b", tolower(p), "\\b")
    boundary_match <- grepl(pattern, lower_names, perl = TRUE)
    if (any(boundary_match)) {
      player_mask <- player_mask | boundary_match
    } else {
      # Fallback to substring for partial names (e.g., accented characters)
      player_mask <- player_mask | grepl(tolower(p), lower_names, fixed = TRUE)
    }
  }
  xm_data <- xm_data[player_mask, ]

  if (nrow(xm_data) == 0) {
    cli::cli_warn("No data found for players: {paste(players, collapse=', ')}")
    return(data.frame())
  }

  # Aggregate across seasons/leagues per player
  dt <- data.table::as.data.table(xm_data)
  result <- dt[, .(
    team = names(which.max(table(team_name))),
    minutes = sum(minutes, na.rm = TRUE),
    # Shooting
    goals = sum(goals, na.rm = TRUE),
    xg = sum(xg, na.rm = TRUE),
    npxg = sum(npxg, na.rm = TRUE),
    shots = sum(shots, na.rm = TRUE),
    # Creating
    assists = sum(assists, na.rm = TRUE),
    xa = sum(xa, na.rm = TRUE),
    key_passes = sum(key_passes, na.rm = TRUE),
    # Passing
    passes_completed = sum(passes_completed, na.rm = TRUE),
    passes_attempted = sum(passes_attempted, na.rm = TRUE),
    xpass_overperformance = sum(xpass_overperformance, na.rm = TRUE)
  ), by = .(player_name)]

  # Add chain stats if available
  has_chains <- "chains_involved" %in% names(dt)
  if (has_chains) {
    chain_agg <- dt[, .(
      chains_involved = sum(chains_involved, na.rm = TRUE),
      successful_chains = sum(successful_chains, na.rm = TRUE),
      chain_goals = sum(chain_goals, na.rm = TRUE),
      chain_starts = sum(chain_starts, na.rm = TRUE),
      chain_xg = sum(chain_xg, na.rm = TRUE)
    ), by = .(player_name)]
    result <- chain_agg[result, on = "player_name"]
  }

  data.table::setDF(result)

  # Filter by min_minutes
  if (min_minutes > 0) {
    result <- result[result$minutes >= min_minutes, ]
  }

  if (nrow(result) == 0) {
    cli::cli_warn("No players meet minimum minutes threshold ({min_minutes})")
    return(data.frame())
  }

  # Derive per-90 rates
  result$goals_p90 <- round(per_90(result$goals, result$minutes), 2)
  result$xg_p90 <- round(per_90(result$xg, result$minutes), 2)
  result$npxg_p90 <- round(per_90(result$npxg, result$minutes), 2)
  result$xa_p90 <- round(per_90(result$xa, result$minutes), 2)
  result$shots_p90 <- round(per_90(result$shots, result$minutes), 1)
  result$pass_pct <- round(safe_divide(result$passes_completed * 100,
                                        result$passes_attempted), 1)
  result$xpass_op_p90 <- round(per_90(result$xpass_overperformance, result$minutes), 2)

  if (has_chains) {
    result$chains_p90 <- round(per_90(result$chains_involved, result$minutes), 1)
    result$chain_shot_pct <- round(safe_divide(result$successful_chains * 100,
                                               result$chains_involved), 1)
    result$chain_goal_pct <- round(safe_divide(result$chain_goals * 100,
                                               result$chains_involved), 1)
  }

  # Select output columns
  out_cols <- c("player_name", "team", "minutes",
                "goals", "xg_p90", "npxg_p90", "shots_p90",
                "assists", "xa_p90", "key_passes",
                "pass_pct", "xpass_op_p90")
  if (has_chains) {
    out_cols <- c(out_cols, "chains_p90", "chain_shot_pct", "chain_goal_pct")
  }
  out_cols <- intersect(out_cols, names(result))

  result <- result[order(-result$minutes), out_cols]
  rownames(result) <- NULL

  result
}
