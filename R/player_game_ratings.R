# Unified Player Game Ratings
# ============================
# One row per player per match with all value metrics: EPV, WPA, PSV/OSV/DSV,
# and a combined "panna value" rating.
#
# Mirrors torpverse R/player_game_ratings.R but adapted for soccer's metric set.


#' Build unified player game ratings
#'
#' Merges per-game EPV, WPA, and PSV into a single data.table with one row
#' per player per match. Computes a combined \code{panna_value} blending
#' EPV and PSV contributions.
#'
#' @param player_game_epv Per-game EPV from \code{\link{aggregate_player_game_epv}}.
#' @param player_game_wpa Per-game WPA from \code{\link{aggregate_player_game_wpa}}.
#'   Optional; WPA columns are NA if not provided.
#' @param player_game_psv Per-game PSV from \code{\link{calculate_psv_components}}.
#'   Optional; PSV columns are NA if not provided.
#' @param epv_weight Weight for EPV in combined panna_value (default
#'   \code{PANNA_EPR_WEIGHT}).
#' @param psv_weight Weight for PSV in combined panna_value (default
#'   \code{PANNA_PSR_WEIGHT}).
#'
#' @return A data.table with one row per player per match:
#'   \describe{
#'     \item{player_id, player_name, team_id, match_id}{Identifiers}
#'     \item{minutes_played, position}{From lineups (if available)}
#'     \item{epv_total, epv_offensive, epv_defensive}{EPV components}
#'     \item{epv_p90}{EPV per 90 minutes}
#'     \item{wpa_total, wpa_as_actor, wpa_as_receiver}{WPA components}
#'     \item{wpa_p90}{WPA per 90 minutes}
#'     \item{psv, osv, dsv}{Player Stat Value with O/D decomposition}
#'     \item{panna_value}{Combined: epv_weight * epv_total + psv_weight * psv}
#'     \item{panna_value_p90}{Combined per 90 minutes}
#'   }
#'
#' @export
build_player_game_ratings <- function(player_game_epv,
                                       player_game_wpa = NULL,
                                       player_game_psv = NULL,
                                       epv_weight = PANNA_EPR_WEIGHT,
                                       psv_weight = PANNA_PSR_WEIGHT) {
  result <- data.table::as.data.table(player_game_epv)

  # Select core EPV columns
  epv_cols <- intersect(
    c("player_id", "player_name", "team_id", "match_id",
      "minutes_played", "position", "n_actions",
      "epv_total", "epv_offensive", "epv_defensive",
      "epv_passing", "epv_shooting", "epv_dribbling", "epv_defending",
      "epv_as_actor", "epv_as_receiver", "epv_duel_blame",
      "epv_total_p90", "epv_offensive_p90", "epv_defensive_p90",
      "epv_adj"),
    names(result)
  )
  result <- result[, ..epv_cols]

  # Rename epv_total_p90 to epv_p90 for brevity
  if ("epv_total_p90" %in% names(result)) {
    data.table::setnames(result, "epv_total_p90", "epv_p90")
  }

  # --- Merge WPA ---
  if (!is.null(player_game_wpa)) {
    wpa <- data.table::as.data.table(player_game_wpa)
    wpa_cols <- intersect(
      c("player_id", "match_id", "wpa_total", "wpa_as_actor",
        "wpa_as_receiver", "max_wpa",
        "wpa_total_p90", "wpa_as_actor_p90", "wpa_as_receiver_p90",
        "wpa_adj"),
      names(wpa)
    )
    wpa <- wpa[, ..wpa_cols]
    if ("wpa_total_p90" %in% names(wpa)) {
      data.table::setnames(wpa, "wpa_total_p90", "wpa_p90")
    }
    result <- merge(result, wpa, by = c("player_id", "match_id"),
                     all.x = TRUE)
  }

  # --- Merge PSV ---
  if (!is.null(player_game_psv)) {
    psv <- data.table::as.data.table(player_game_psv)
    psv_cols <- intersect(
      c("player_id", "match_id", "psv", "psv_raw", "osv", "dsv"),
      names(psv)
    )
    psv <- psv[, ..psv_cols]
    result <- merge(result, psv, by = c("player_id", "match_id"),
                     all.x = TRUE)
  }

  # --- Combined panna_value ---
  epv_val <- if ("epv_total" %in% names(result)) result$epv_total else 0
  psv_val <- if ("psv" %in% names(result)) {
    data.table::fifelse(is.na(result$psv), 0, result$psv)
  } else 0

  result[, panna_value := epv_weight * epv_val + psv_weight * psv_val]

  # Per-90 panna_value
  if ("minutes_played" %in% names(result)) {
    mins_safe <- pmax(result$minutes_played, 1, na.rm = TRUE)
    result[, panna_value_p90 := panna_value / (mins_safe / 90)]
  }

  # Fill NAs in numeric columns
  num_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  for (col in num_cols) {
    data.table::set(result, which(is.na(result[[col]])), col, 0)
  }

  data.table::setorder(result, match_id, -panna_value)
  result[]
}


#' Aggregate player game ratings to season level
#'
#' Summarizes per-game ratings to one row per player per season, with
#' total and per-90 averages for all value metrics.
#'
#' @param game_ratings Output of \code{\link{build_player_game_ratings}}.
#' @param season_col Column name containing season identifier (default
#'   \code{"season"}). If not present, all data is treated as one season.
#'
#' @return A data.table with one row per player (per season), containing
#'   summed totals and minutes-weighted per-90 averages.
#'
#' @export
aggregate_season_ratings <- function(game_ratings, season_col = "season") {

  dt <- data.table::as.data.table(game_ratings)

  group_cols <- "player_id"
  if (season_col %in% names(dt)) group_cols <- c(group_cols, season_col)

  # Value metric columns to aggregate
  value_cols <- intersect(
    c("epv_total", "epv_offensive", "epv_defensive",
      "wpa_total", "psv", "osv", "dsv", "panna_value"),
    names(dt)
  )

  # Aggregate
  result <- dt[, c(
    list(
      player_name = player_name[1],
      n_games = .N,
      total_minutes = sum(minutes_played, na.rm = TRUE)
    ),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = group_cols, .SDcols = value_cols]

  # Per-90 rates
  mins_safe <- pmax(result$total_minutes, 1)
  for (col in value_cols) {
    p90_col <- paste0(col, "_p90")
    data.table::set(result, j = p90_col,
                     value = result[[col]] / (mins_safe / 90))
  }

  data.table::setorder(result, -panna_value)
  result[]
}
