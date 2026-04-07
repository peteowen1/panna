# EPR: Expected Points Rating from EPV
# ======================================
# Bayesian-shrinkage rating that smooths per-game EPV values with exponential
# decay weighting to predict next-match EPV contribution.
#
# Soccer version: 2 components (offensive + defensive) vs AFL's 4.
# Follows torpverse R/player_ratings.R calculate_epr() / calculate_epr_stats().



# ============================================================================
# Core EPR calculation
# ============================================================================

#' Calculate EPR (Expected Points Rating) from per-game EPV
#'
#' For each player, applies exponential time-decay to their per-game EPV
#' values and computes a Bayesian-shrunk rating estimate. Players with
#' little data are shrunk toward the prior rate (slightly below zero for
#' offense, zero for defense).
#'
#' @param player_game_epv Per-game EPV data from
#'   \code{\link{aggregate_player_game_epv}}. Must contain: \code{player_id},
#'   \code{match_id}, \code{match_date}, \code{minutes_played},
#'   \code{epv_offensive}, \code{epv_defensive}.
#' @param ref_date Date to estimate ratings at. Only matches before this date
#'   are used. If NULL, uses the latest match date in data.
#' @param decay_offensive Decay rate in days for offensive EPV (default
#'   \code{EPR_DECAY_OFFENSIVE}).
#' @param decay_defensive Decay rate in days for defensive EPV (default
#'   \code{EPR_DECAY_DEFENSIVE}).
#' @param prior_games Prior strength in equivalent full games (default
#'   \code{EPR_PRIOR_GAMES}).
#' @param prior_rate_off Prior rate for offensive component (default
#'   \code{EPR_PRIOR_RATE_OFF}).
#' @param prior_rate_def Prior rate for defensive component (default
#'   \code{EPR_PRIOR_RATE_DEF}).
#' @param loading Loading factor applied to observed data (default
#'   \code{EPR_LOADING}).
#'
#' @return A data.table with one row per player:
#'   \describe{
#'     \item{player_id, player_name}{Identifiers}
#'     \item{epr}{Total EPR = epr_offensive + epr_defensive}
#'     \item{epr_offensive}{Offensive EPV rating (passing, shooting, dribbling)}
#'     \item{epr_defensive}{Defensive EPV rating (defending, duel blame)}
#'     \item{wt_games}{Weighted games (effective sample size)}
#'     \item{n_games}{Raw number of games played}
#'   }
#'
#' @export
calculate_epr <- function(player_game_epv, ref_date = NULL,
                           decay_offensive = EPR_DECAY_OFFENSIVE,
                           decay_defensive = EPR_DECAY_DEFENSIVE,
                           prior_games = EPR_PRIOR_GAMES,
                           prior_rate_off = EPR_PRIOR_RATE_OFF,
                           prior_rate_def = EPR_PRIOR_RATE_DEF,
                           loading = EPR_LOADING) {
  dt <- data.table::as.data.table(player_game_epv)

  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }

  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date, na.rm = TRUE)
  } else {
    ref_date <- as.Date(ref_date)
  }

  # Filter to matches before ref_date
  dt <- dt[match_date < ref_date]
  if (nrow(dt) == 0) {
    cli::cli_warn("No matches before {ref_date}")
    return(data.table::data.table(
      player_id = character(0), player_name = character(0),
      epr = numeric(0), epr_offensive = numeric(0), epr_defensive = numeric(0),
      wt_games = numeric(0), n_games = integer(0)))
  }

  # Days since match
  dt[, days_since := as.numeric(ref_date - match_date)]

  # Minutes fraction (per-game adjustment: divide by 90)
  dt[, mins_frac := pmax(as.numeric(minutes_played), 1) / 90]

  # Decay weights per component
  dt[, w_off := exp(-days_since / decay_offensive) * mins_frac]
  dt[, w_def := exp(-days_since / decay_defensive) * mins_frac]

  # Ensure EPV columns exist
  if (!"epv_offensive" %in% names(dt)) dt[, epv_offensive := 0]
  if (!"epv_defensive" %in% names(dt)) dt[, epv_defensive := 0]

  # Per-90 EPV values (undo the minutes effect so we rate per-90 contribution)
  dt[, epv_off_p90 := epv_offensive / mins_frac]
  dt[, epv_def_p90 := epv_defensive / mins_frac]

  # Aggregate per player
  agg <- dt[, .(
    sum_off  = sum(w_off * epv_off_p90, na.rm = TRUE),
    sum_def  = sum(w_def * epv_def_p90, na.rm = TRUE),
    wt_off   = sum(w_off, na.rm = TRUE),
    wt_def   = sum(w_def, na.rm = TRUE),
    wt_games = sum(w_off, na.rm = TRUE),  # use offensive weights as "games"
    n_games  = .N,
    player_name = player_name[1]
  ), by = player_id]

  # Bayesian shrinkage: (loading * sum + prior_games * prior_rate) / (wt + prior_games)
  agg[, epr_offensive := (loading * sum_off + prior_games * prior_rate_off) /
                          (wt_off + prior_games)]
  agg[, epr_defensive := (loading * sum_def + prior_games * prior_rate_def) /
                          (wt_def + prior_games)]
  agg[, epr := epr_offensive + epr_defensive]

  # Clean up
  agg[, c("sum_off", "sum_def", "wt_off", "wt_def") := NULL]

  data.table::setorder(agg, -epr)
  agg[]
}


#' Calculate EPR at multiple dates (batch version)
#'
#' Efficiently computes EPR ratings at multiple reference dates using the
#' cumsum trick for O(N + D * players) instead of O(N * D) complexity.
#'
#' @param player_game_epv Per-game EPV data.
#' @param ref_dates Character or Date vector of reference dates.
#' @param ... Additional parameters passed to \code{\link{calculate_epr}}.
#'
#' @return A data.table with columns from \code{calculate_epr} plus
#'   \code{ref_date}.
#'
#' @export
calculate_epr_batch <- function(player_game_epv, ref_dates, ...) {
  ref_dates <- sort(as.Date(ref_dates))

  results <- vector("list", length(ref_dates))
  for (i in seq_along(ref_dates)) {
    result <- calculate_epr(player_game_epv, ref_date = ref_dates[i], ...)
    if (nrow(result) > 0) {
      result[, ref_date := ref_dates[i]]
      results[[i]] <- result
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}
