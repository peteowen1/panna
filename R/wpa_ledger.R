# WPA as a net ledger (panna#273).
# ===========================================================================
# The per-action WPA credit (assign_wpa_credit()) pays actors and receivers but
# never debits the side that conceded, so both teams' players can total several
# wins in one match (Liverpool 2026-27: both sides +1.3 to +5.0 in every match,
# losses included). This runs the same per-action change in win probability
# through the net goals engine instead, so each team's players sum to its result
# against an even start: win +0.5, draw 0, loss -0.5. That is torp's
# `wpa_neutral` (torp#243), agreed with Pete 2026-11-09: everyone starts at 50/50,
# and the net goals engine routes the credit unchanged.

#' Build the WPA net ledger
#'
#' Each team's players sum to its result minus `start` (win = 1, draw = 0.5,
#' loss = 0): +0.5, 0 or -0.5 with the default even start. The two teams in a
#' match cancel.
#'
#' The engine is [ng_build_ledger()] in its team convention, with the goals-only
#' rules switched off:
#' * `shot_chain` and `shot_aftermath` restart value from 0 goals after a shot,
#'   which has no meaning in win probability;
#' * the xGOT split prices a shot's strike as `xgot - xG` **in goals**
#'   (`R/epv_net_goals.R`, `strike <- xgt - xg0`), so on a win probability row
#'   it would hand the shooter and the keeper amounts in the wrong units. Shots
#'   take the single-step rule, as torp's plain configuration does.
#'
#' The engine conserves each match's own total, which for un-centred WPA is the
#' result minus the opening win chance (the model's first action). The
#' difference between that opening and `start` -- about +/-0.04 per team, the
#' home advantage in the model -- is spread across each team's players by
#' minutes and reported as `wpa_start`.
#'
#' @param actions SPADL actions as the net goals ledger takes them
#'   (`match_id`, `action_id`, `team_id`, `player_id`, `action_type`, `result`,
#'   `period_id`, `time_seconds`, and optionally `xpass`, `receiver_player_id`,
#'   `receiver_team_id`). Any `epv_delta` or `xgot` column is ignored.
#' @param wp_actions One row per action: `match_id`, `action_id`, `team_id`,
#'   `wp` (the acting team's win chance) and `wpa` (its change, in the acting
#'   team's frame, NOT centred: `add_wp_vars(..., centre = FALSE)`).
#' @param fixtures `match_id`, `home_team_id`, `away_team_id`, `home_score`,
#'   `away_score`.
#' @param lineups Lineups as [ng_spread_pools()] and [ng_player_game()] take them.
#' @param adj Optional output of [ng_build_adjacency()].
#' @param start The even start every team is measured from. Default 0.5.
#' @param dacts_share Passed to [ng_spread_pools()]. Default 0 (flat pools).
#' @param tol Largest allowed gap between a team's total and its target, and
#'   between the engine's match total and (result - opening win chance).
#' @param verbose Print a summary. Default `TRUE`.
#'
#' @return One row per player-match: `wpa_neutral` (the ledger value),
#'   `wpa_offence`, `wpa_defence`, `wpa_start` (`wpa_offence + wpa_defence +
#'   wpa_start = wpa_neutral`), torp's three channels `wpa_own` (on-ball acts),
#'   `wpa_won` (balls won and shots stopped, where the feed names him) and
#'   `wpa_shared` (his minutes' share of the team pools plus `wpa_start`), with
#'   `wpa_own + wpa_won + wpa_shared = wpa_neutral`, and one `wpa_<role>`
#'   column per engine role.
#'   Attribute `targets`: per team-match, its result, target, opening win chance
#'   and the gap after the build.
#' @family win probability
#' @export
wpa_build_ledger <- function(actions, wp_actions, fixtures, lineups, adj = NULL,
                             start = 0.5, dacts_share = 0, tol = 1e-6,
                             verbose = TRUE) {
  a <- data.table::as.data.table(actions)
  w <- data.table::as.data.table(wp_actions)
  fx <- data.table::as.data.table(fixtures)
  need_w <- c("match_id", "action_id", "team_id", "wp", "wpa")
  miss <- setdiff(need_w, names(w))
  if (length(miss)) cli::cli_abort("{.arg wp_actions} is missing {.val {miss}}.")
  need_f <- c("match_id", "home_team_id", "away_team_id", "home_score", "away_score")
  miss <- setdiff(need_f, names(fx))
  if (length(miss)) cli::cli_abort("{.arg fixtures} is missing {.val {miss}}.")
  if (!is.numeric(start) || length(start) != 1L || start <= 0 || start >= 1) {
    cli::cli_abort("{.arg start} must be one number between 0 and 1, not {.val {start}}.")
  }

  a <- data.table::copy(a)
  # Goals-unit inputs are dropped (see above), and so are any fixture columns the
  # engine merges in itself, which would otherwise come back as .x / .y.
  for (col in intersect(c("epv_delta", "xgot", "epv", "shot_xg", "home_team_id",
                          "away_team_id", "is_home"), names(a))) a[, (col) := NULL]
  a[w, epv_delta := i.wpa, on = c("match_id", "action_id")]
  n_miss <- sum(is.na(a$epv_delta))
  if (n_miss > 0) {
    cli::cli_abort(c(
      "{n_miss} action{?s} have no WPA in {.arg wp_actions}.",
      "i" = "Every action needs one: a missing change would leave its value booked to nobody."))
  }

  # Each match: result, opening win chance (home frame), and the identity the
  # engine conserves. Centred WPA (add_wp_vars()'s default) breaks the identity,
  # and the ledger would then quietly spread the centring offset as "start".
  fx <- fx[match_id %in% unique(a$match_id)]
  fx[, res_home := data.table::fifelse(home_score > away_score, 1,
                    data.table::fifelse(home_score < away_score, 0, 0.5))]
  data.table::setorder(a, match_id, action_id)
  wo <- w[a[, .(match_id, action_id)], on = c("match_id", "action_id")]
  wo[fx, home_team_id := i.home_team_id, on = "match_id"]
  op <- wo[, .(open_home = data.table::fifelse(team_id[1] == home_team_id[1], wp[1], 1 - wp[1]),
               sum_home = sum(data.table::fifelse(team_id == home_team_id, wpa, -wpa))),
           by = match_id]
  fx[op, `:=`(open_home = i.open_home, sum_home = i.sum_home), on = "match_id"]
  if (anyNA(fx$open_home)) {
    cli::cli_abort("{sum(is.na(fx$open_home))} fixture{?s} have no actions; drop them from {.arg fixtures}.")
  }
  fx[, identity_gap := sum_home - (res_home - open_home)]
  bad <- fx[abs(identity_gap) > tol]
  if (nrow(bad)) {
    cli::cli_abort(c(
      "WPA does not add up to (result - opening win chance) in {nrow(bad)} match{?es}, largest gap {signif(max(abs(bad$identity_gap)), 3)}.",
      "i" = "Pass un-centred WPA: {.code add_wp_vars(..., centre = FALSE)}. The default centres each match to sum to zero.",
      "i" = "First: {.val {bad$match_id[1]}}."))
  }

  pay <- ng_build_ledger(a, adj = adj, fixtures = fx[, .(match_id, home_team_id, away_team_id)],
                         lineups = lineups, convention = "team", shot_chain = FALSE,
                         shot_aftermath = FALSE, verbose = FALSE)
  pay <- ng_spread_pools(pay, a, lineups, dacts_share = dacts_share, verbose = FALSE)
  pg <- data.table::as.data.table(ng_player_game(pay, lineups, verbose = FALSE))

  data.table::setnames(pg, c("net_goals", "epv_offensive", "epv_defensive"),
                       c("wpa_neutral", "wpa_offence", "wpa_defence"))
  rc <- grep("^ng_", names(pg), value = TRUE)
  data.table::setnames(pg, rc, sub("^ng_", "wpa_", rc))

  # The even start: move each team from its opening to `start`, by minutes.
  tg <- rbind(fx[, .(match_id, team_id = home_team_id, result = res_home, open = open_home)],
              fx[, .(match_id, team_id = away_team_id, result = 1 - res_home, open = 1 - open_home)])
  tg[, target := result - start]
  pg[, .mins := pmax(data.table::fifelse(is.na(minutes_played), 0, minutes_played), 0)]
  pg[tg, .shift := i.open - start, on = c("match_id", "team_id")]
  pg[, .tot_mins := sum(.mins), by = .(match_id, team_id)]
  n_nomins <- pg[.tot_mins == 0, data.table::uniqueN(paste(match_id, team_id))]
  if (n_nomins > 0) {
    cli::cli_abort("{n_nomins} team-match{?es} have no lineup minutes, so the start term cannot be spread.")
  }
  pg[, wpa_start := .shift * .mins / .tot_mins]
  pg[, wpa_neutral := wpa_neutral + wpa_start]
  pg[, c(".mins", ".shift", ".tot_mins") := NULL]

  # torp's three channels (.np_v4_channels(), torp#263), not offence/defence.
  # Offence/defence puts every ball a team loses in its players' offence and
  # every ball it wins back in their defence, so on ENG 2026-27 a team's
  # offence averaged -1.95 a match and 94% of player-matches were negative
  # (Pete, 2026-11-11). Here the unnamed team share is its own channel:
  #   own    -- what the player did on the ball (actor, receiver, shooter, rebound)
  #   won    -- what the feed names him for winning or stopping (role "defender")
  #   shared -- his minutes' share of the team pools, plus the even start
  roles <- sub("^wpa_", "", grep("^wpa_", names(pg), value = TRUE))
  roles <- setdiff(roles, c("neutral", "offence", "defence", "start"))
  shared_r <- intersect(roles, c("pool_off_spread", "pool_def_spread"))
  won_r <- intersect(roles, "defender")
  own_r <- setdiff(roles, c(shared_r, won_r))
  rsum <- function(r) if (length(r)) rowSums(as.matrix(pg[, paste0("wpa_", r), with = FALSE]), na.rm = TRUE) else 0
  pg[, `:=`(wpa_own = rsum(own_r), wpa_won = rsum(won_r), wpa_shared = rsum(shared_r) + wpa_start)]
  ch_gap <- max(abs(pg$wpa_own + pg$wpa_won + pg$wpa_shared - pg$wpa_neutral))
  if (!is.finite(ch_gap) || ch_gap > tol) {
    cli::cli_abort("WPA ledger: own + won + shared misses the total by {signif(ch_gap, 3)}; roles seen: {.val {roles}}.")
  }

  tt <- pg[, .(total = sum(wpa_neutral)), by = .(match_id, team_id)]
  tg[tt, total := i.total, on = c("match_id", "team_id")]
  tg[, gap := total - target]
  worst <- max(abs(tg$gap), na.rm = TRUE)
  if (anyNA(tg$total) || worst > tol) {
    cli::cli_abort(c(
      "WPA ledger does not conserve: largest team gap {signif(worst, 3)} ({sum(is.na(tg$total))} team-match{?es} with no players).",
      "i" = "Each team's players must sum to its result minus {start}."))
  }
  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste0(
      "WPA ledger: {format(nrow(pg), big.mark = ',')} player-match row{?s} over ",
      "{data.table::uniqueN(pg$match_id)} match{?es}; every team within {signif(worst, 2)} of ",
      "its result minus {start}; opening win chance {round(min(fx$open_home), 3)}-{round(max(fx$open_home), 3)} (home)."))
  }
  data.table::setattr(pg, "targets", tg[])
  pg[]
}
