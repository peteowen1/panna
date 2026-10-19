# WPA Credit Assignment for Soccer
# ==================================
# Splits Win Probability Added between actor and receiver, then aggregates
# per player per game. Mirrors torpverse R/wp_credit.R adapted for SPADL.


#' Assign WPA credit to players
#'
#' Splits WPA between the acting player and receiver (for passes). When there
#' is no receiver (shots, clearances, etc.), the actor gets 100% of WPA.
#'
#' Goals (successful shots, own goals included) and the last action of each
#' period keep 100% for the actor: the "receiver" there is just the next
#' kick-off taker, who was not involved in the play.
#'
#' Cross-team note: WPA is sign-flipped per actor's team in
#' \code{\link{add_wp_vars}} so positive wpa always means "good for my team".
#' When the next-action's player is on the OPPOSITE team (turnover, save,
#' interception, opp shot), passing the receiver \code{(1 - actor_share) * wpa}
#' unchanged would credit the receiver in the actor's team perspective -- i.e.
#' a goalkeeper saving a shot would inherit the shooter's negative WPA. We
#' sign-flip on cross-team transitions so the receiver is credited in their
#' own team's perspective (mirrors AFL pattern in
#' worker/src/ep-model.js scoreChainRows lines 289-333 and afl/match.qmd:600).
#'
#' @param spadl_with_wpa SPADL actions with \code{wpa}, \code{player_id},
#'   \code{team_id}, and optionally \code{receiver_player_id} +
#'   \code{receiver_team_id} columns. If \code{receiver_player_id} is present,
#'   \code{receiver_team_id} is required (error otherwise). \code{action_type},
#'   \code{result}, \code{period_id} enable the goal / period-end exclusions.
#' @param actor_share Fraction of WPA credited to the actor (0-1).
#'   Default \code{WPA_ACTOR_SHARE} (0.5). Receiver gets \code{1 - actor_share}.
#'
#' @return The input data.table with added columns:
#'   \describe{
#'     \item{wpa_actor}{WPA credited to the acting player}
#'     \item{wpa_receiver}{WPA credited to the receiver (0 if no receiver),
#'       sign-flipped on cross-team transitions}
#'   }
#'
#' @family win probability
#' @export
assign_wpa_credit <- function(spadl_with_wpa, actor_share = WPA_ACTOR_SHARE) {
  dt <- data.table::as.data.table(spadl_with_wpa)

  if (!"wpa" %in% names(dt)) {
    cli::cli_abort("{.arg spadl_with_wpa} must contain a {.val wpa} column")
  }

  has_receiver <- "receiver_player_id" %in% names(dt)
  has_receiver_team <- "receiver_team_id" %in% names(dt)

  if (has_receiver && !has_receiver_team) {
    cli::cli_abort(c(
      "{.arg spadl_with_wpa} has {.field receiver_player_id} but no {.field receiver_team_id}.",
      "i" = "Without it the cross-team sign flip cannot run and every receiver would silently be treated as a teammate (review R011).",
      "i" = "{.fn create_wp_features} keeps {.field receiver_team_id}; make sure the column is carried through."
    ))
  }

  if (has_receiver) {
    dt[, has_recv := !is.na(receiver_player_id) & receiver_player_id != player_id]

    # Terminal actions keep 100% for the actor (review R012), same definitions
    # as assign_epv_credit(): a goal (successful shot, own goals included --
    # the next action is the conceding side's kick-off) and the last action of
    # a period (the next action is the next period's kick-off).
    is_goal <- if (all(c("action_type", "result") %in% names(dt))) {
      dt$action_type %in% "shot" & dt$result %in% "success"
    } else {
      rep(FALSE, nrow(dt))
    }
    is_period_end <- rep(FALSE, nrow(dt))
    if (all(c("period_id", "match_id") %in% names(dt))) {
      dt[, .next_period := data.table::shift(period_id, 1L, type = "lead"),
         by = match_id]
      is_period_end <- !is.na(dt$.next_period) & dt$period_id != dt$.next_period
      dt[, .next_period := NULL]
    }
    dt[, has_recv := has_recv & !is_goal & !is_period_end]
    dt[, wpa_actor := data.table::fifelse(has_recv, actor_share * wpa, wpa)]

    # Cross-team sign flip on receiver credit. pos_team = +1 for same-team
    # (legitimate pass completion), -1 for cross-team (turnover/save) -- flips
    # the receiver's portion back into their own team's perspective.
    dt[, pos_team := data.table::fifelse(
      !is.na(receiver_team_id) & team_id == receiver_team_id, 1L, -1L)]
    dt[, wpa_receiver := data.table::fifelse(
      has_recv, pos_team * (1 - actor_share) * wpa, 0)]
    dt[, c("has_recv", "pos_team") := NULL]
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
#' @family win probability
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
    # which.max(abs(wpa)) on an all-NA group returns integer(0), so
    # wpa[which.max(...)] is length-0 there -- inconsistent with the length-1
    # result from every other group and errors data.table's j (L2, 2026-07-08
    # review). Guard on all-NA explicitly rather than relying on .N > 0
    # (which is always TRUE for a non-empty group regardless of NAs).
    max_wpa = if (all(is.na(wpa))) 0 else wpa[which.max(abs(wpa))]
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
    result[, pos_group := .position_role4_any(
      position, if ("position_side" %in% names(result)) position_side else NULL)]
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
