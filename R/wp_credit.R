# WPA Credit Assignment for Soccer
# ==================================
# Splits Win Probability Added between actor and receiver, then aggregates
# per player per game. Mirrors torpverse R/wp_credit.R adapted for SPADL.

# Default WPA credit split: 50% to actor, 50% to receiver
WPA_ACTOR_SHARE <- 0.5


#' Assign WPA credit to players
#'
#' Splits WPA between the acting player and receiver (for passes). When there
#' is no receiver (shots, clearances, etc.), the actor gets 100% of WPA.
#'
#' @param spadl_with_wpa SPADL actions with \code{wpa}, \code{player_id},
#'   and optionally \code{receiver_player_id} columns.
#' @param actor_share Fraction of WPA credited to the actor (0-1).
#'   Default \code{WPA_ACTOR_SHARE} (0.5). Receiver gets \code{1 - actor_share}.
#'
#' @return The input data.table with added columns:
#'   \describe{
#'     \item{wpa_actor}{WPA credited to the acting player}
#'     \item{wpa_receiver}{WPA credited to the receiver (0 if no receiver)}
#'   }
#'
#' @export
assign_wpa_credit <- function(spadl_with_wpa, actor_share = WPA_ACTOR_SHARE) {
  dt <- data.table::as.data.table(spadl_with_wpa)

  if (!"wpa" %in% names(dt)) {
    cli::cli_abort("{.arg spadl_with_wpa} must contain a {.val wpa} column")
  }

  has_receiver <- "receiver_player_id" %in% names(dt)

  if (has_receiver) {
    dt[, has_recv := !is.na(receiver_player_id) & receiver_player_id != player_id]
    dt[, wpa_actor := data.table::fifelse(has_recv, actor_share * wpa, wpa)]
    dt[, wpa_receiver := data.table::fifelse(has_recv, (1 - actor_share) * wpa, 0)]
    dt[, has_recv := NULL]
  } else {
    dt[, wpa_actor := wpa]
    dt[, wpa_receiver := 0]
  }

  dt
}


#' Aggregate player WPA per game
#'
#' Produces one row per player per match with total WPA, actor WPA, and
#' receiver WPA. Optionally computes per-90 rates and position-centered
#' values.
#'
#' @param spadl_with_wpa SPADL actions with WPA credit columns from
#'   \code{\link{assign_wpa_credit}}.
#' @param lineups Optional lineup data with \code{player_id}, \code{match_id},
#'   \code{minutes_played}, and optionally \code{position}.
#' @param position_center Logical. Subtract position-group mean per season
#'   to produce \code{wpa_adj}. Default \code{FALSE}.
#'
#' @return A data.table with one row per player per match:
#'   \describe{
#'     \item{player_id, player_name, team_id, match_id}{Identifiers}
#'     \item{wpa_total}{Total WPA = actor + receiver}
#'     \item{wpa_as_actor}{WPA from own actions}
#'     \item{wpa_as_receiver}{WPA from receiving}
#'     \item{n_wpa_actions}{Number of actions with non-zero WPA}
#'     \item{max_wpa}{Largest single-action WPA (peak moment)}
#'     \item{wpa_p90, wpa_as_actor_p90, wpa_as_receiver_p90}{Per-90 rates}
#'     \item{wpa_adj}{Position-centered WPA (if position_center = TRUE)}
#'   }
#'
#' @export
aggregate_player_game_wpa <- function(spadl_with_wpa, lineups = NULL,
                                       position_center = FALSE) {
  dt <- data.table::as.data.table(spadl_with_wpa)

  if (!"wpa_actor" %in% names(dt)) {
    cli::cli_abort("Run {.fn assign_wpa_credit} first to add wpa_actor/wpa_receiver columns")
  }

  # --- Actor WPA per player per match ---
  actor_agg <- dt[, .(
    wpa_as_actor = sum(wpa_actor, na.rm = TRUE),
    n_wpa_actions = sum(wpa != 0, na.rm = TRUE),
    max_wpa = if (.N > 0) wpa[which.max(abs(wpa))] else 0
  ), by = .(player_id, player_name, team_id, match_id)]

  # --- Receiver WPA per player per match ---
  if ("receiver_player_id" %in% names(dt)) {
    recv_dt <- dt[!is.na(receiver_player_id) & wpa_receiver != 0]
    if (nrow(recv_dt) > 0) {
      recv_agg <- recv_dt[, .(
        wpa_as_receiver = sum(wpa_receiver, na.rm = TRUE)
      ), by = .(receiver_player_id, match_id)]
      data.table::setnames(recv_agg, "receiver_player_id", "player_id")
    } else {
      recv_agg <- data.table::data.table(
        player_id = character(0), match_id = character(0),
        wpa_as_receiver = numeric(0))
    }
  } else {
    recv_agg <- data.table::data.table(
      player_id = character(0), match_id = character(0),
      wpa_as_receiver = numeric(0))
  }

  # Join actor + receiver
  result <- merge(actor_agg, recv_agg, by = c("player_id", "match_id"),
                   all = TRUE)
  result[is.na(wpa_as_actor), wpa_as_actor := 0]
  result[is.na(wpa_as_receiver), wpa_as_receiver := 0]
  result[is.na(n_wpa_actions), n_wpa_actions := 0L]
  result[is.na(max_wpa), max_wpa := 0]

  # Total WPA
  result[, wpa_total := wpa_as_actor + wpa_as_receiver]

  # --- Join lineups for minutes and per-90 ---
  if (!is.null(lineups) && "minutes_played" %in% names(lineups)) {
    dt_lineups <- data.table::as.data.table(lineups)
    key_cols <- intersect(c("player_id", "match_id"), names(dt_lineups))
    if (length(key_cols) == 2) {
      mins <- dt_lineups[, .(minutes_played = sum(minutes_played, na.rm = TRUE)),
                          by = .(player_id, match_id)]
      if ("position" %in% names(dt_lineups)) {
        pos <- dt_lineups[, .(position = position[1]), by = .(player_id, match_id)]
        mins <- merge(mins, pos, by = c("player_id", "match_id"), all.x = TRUE)
      }
      result <- merge(result, mins, by = c("player_id", "match_id"),
                       all.x = TRUE)

      # Per-90 rates
      wpa_cols <- c("wpa_total", "wpa_as_actor", "wpa_as_receiver")
      mins_safe <- pmax(result$minutes_played, 1, na.rm = TRUE)
      for (col in wpa_cols) {
        p90_col <- paste0(col, "_p90")
        data.table::set(result, j = p90_col,
                         value = result[[col]] / (mins_safe / 90))
      }
    }
  }

  # --- Position centering ---
  if (isTRUE(position_center) && "position" %in% names(result)) {
    result[, pos_group := data.table::fcase(
      grepl("GK|Goalkeeper", position, ignore.case = TRUE), "GK",
      grepl("DEF|Back|CB|LB|RB|WB", position, ignore.case = TRUE), "DEF",
      grepl("MID|CM|DM|AM|Wing", position, ignore.case = TRUE), "MID",
      grepl("FWD|Forward|Striker|CF|ST", position, ignore.case = TRUE), "FWD",
      default = "MID"
    )]
    result[, wpa_adj := wpa_total - mean(wpa_total, na.rm = TRUE),
            by = pos_group]
    result[, pos_group := NULL]
  }

  # Fill NAs
  num_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  for (col in num_cols) {
    data.table::set(result, which(is.na(result[[col]])), col, 0)
  }

  data.table::setorder(result, match_id, -wpa_total)
  result[]
}
