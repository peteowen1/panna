# Shot context: what was known about a shot BEFORE it was struck, read from the
# full Opta event stream. One function for training (data-raw/epv/xg-vnext) and
# scoring (add_xg_to_spadl / add_xgot_to_spadl), so the two cannot drift -- the
# divergent-construction failure that once made SPM xG-blind and cost the season
# term a 70-minute rebuild. Design and evidence: pannaverse/docs/plans/XG-VNEXT-2026-09.md.

# Shot tags the xG v5 / xGOT v3 models read: fast break, 1 on 1, individual play,
# intentional assist, lob. All are recorded before the shot is struck. The
# outcome-revealing tags (230/231, 217, 468 and the goal-only / save-only
# families) are never computed here -- see the plan's qualifier table. Big
# chance (214) reaches the models through SPADL's is_big_chance, as before.
.SHOT_CONTEXT_TAGS <- c("23", "89", "215", "154", "117")
# Pass tags read off the assist: cross, through ball, pull back, chipped, lay-off,
# flick-on, long ball, free kick, corner, throw-in.
.SHOT_ASSIST_TAGS <- c("2", "4", "195", "155", "156", "168", "1", "5", "6", "107")
# Event types on which a side has the ball: when did the other team last have it?
.SHOT_ONBALL_TYPES <- c(1L, 3L, 12L, 13L, 14L, 15L, 16L, 61L, 50L)

.has_q <- function(json, id) grepl(sprintf('"%s":', id), json, fixed = TRUE)
.num_q <- function(json, id) {
  suppressWarnings(as.numeric(sub(sprintf('.*"%s":"?([-0-9.]+).*', id), "\\1",
                                  ifelse(.has_q(json, id), json, NA))))
}

#' Pre-shot context for every shot in an Opta event stream
#'
#' For each shot (types 13-16, periods 1-4): its pre-shot tags, the assist (the
#' last pass by the shooting team tagged 210 in the 20 seconds before), seconds
#' since the other team last had the ball and completed passes since, whether it
#' follows another shot within 5 seconds, and the score before it (own goals,
#' qualifier 28, count for the other side). Events are ordered by period,
#' minute, second, then event_id.
#'
#' @param events Opta events for whole matches: `match_id`, `event_id`,
#'   `type_id`, `team_id`, `period_id`, `minute`, `second`, `outcome`, `x`, `y`,
#'   `qualifier_json`. Pass every event of each match, not a filtered subset:
#'   possession and score are read from the events around the shot.
#' @return data.table, one row per shot: `match_id`, `event_id` and the context
#'   columns (`q<tag>`, `has_assist`, `a<tag>`, `a_len`, `a_x`, `a_y`,
#'   `poss_secs`, `poss_passes`, `rebound`, `goals_for`, `goals_against`,
#'   `score_diff`, `minute`).
#' @keywords internal
.shot_context <- function(events) {
  need <- c("match_id", "event_id", "type_id", "team_id", "period_id", "minute", "second",
            "outcome", "x", "y", "qualifier_json")
  miss <- setdiff(need, names(events))
  if (length(miss)) cli::cli_abort("Shot context needs {.field {miss}} on the events.")
  ev <- data.table::as.data.table(events)[period_id %in% 1:4,
          .(match_id = as.character(match_id), event_id = as.character(event_id), type_id = as.integer(type_id),
            ev_ord = as.numeric(event_id), team_id, period_id, minute, second, outcome, x, y, qualifier_json)]
  # tie-break within a second on the NUMERIC event id (as text, "1316229790" sorts
  # before "469459429" and 0.6% of shots get the wrong assist / possession)
  data.table::setorder(ev, match_id, period_id, minute, second, ev_ord)
  ev[, `:=`(idx = .I, t = minute * 60 + second)]
  shots <- ev[type_id %in% 13:16]
  if (!nrow(shots)) return(data.table::data.table())
  teams <- ev[, .(teams = list(unique(team_id))), by = match_id]
  other_of <- function(tm, a) setdiff(a, tm)[1]
  shots[teams, on = "match_id", other := mapply(other_of, team_id, i.teams)]
  for (id in .SHOT_CONTEXT_TAGS)
    data.table::set(shots, j = paste0("q", id), value = as.integer(.has_q(shots$qualifier_json, id)))
  shots[, j_idx := idx - 0.5]

  # assist: last pass tagged 210 by the shooting team, within 20 s
  ap <- ev[type_id == 1L & .has_q(qualifier_json, "210"),
           .(match_id, team_id, a_idx = idx, a_t = t, a_period = period_id, a_x = x, a_y = y, qualifier_json)]
  for (id in .SHOT_ASSIST_TAGS)
    data.table::set(ap, j = paste0("a", id), value = as.integer(.has_q(ap$qualifier_json, id)))
  ap[, a_len := .num_q(qualifier_json, "212")][, qualifier_json := NULL]
  a_cols <- c(paste0("a", .SHOT_ASSIST_TAGS), "a_len", "a_x", "a_y", "a_t", "a_period")
  asst <- ap[shots[, .(match_id, team_id, j_idx)], on = .(match_id, team_id, a_idx = j_idx), roll = Inf]
  for (cc in a_cols) data.table::set(shots, j = cc, value = asst[[cc]])
  shots[, has_assist := !is.na(a_t) & a_period == period_id & (t - a_t) <= 20]
  a_vals <- c(paste0("a", .SHOT_ASSIST_TAGS), "a_len", "a_x", "a_y")
  shots[has_assist == FALSE, (a_vals) := NA]
  shots[, c("a_t", "a_period") := NULL]

  # possession: when did the other team last have the ball? (a rolling join
  # returns the LOOKUP's value in the join column, so the matched row's own
  # position rides in o_pos)
  ob <- ev[type_id %in% .SHOT_ONBALL_TYPES, .(match_id, team_id, o_idx = idx, o_pos = idx, o_t = t, o_period = period_id)]
  po <- ob[shots[, .(match_id, team_id = other, j_idx)], on = .(match_id, team_id, o_idx = j_idx), roll = Inf]
  shots[, `:=`(opp_idx = po$o_pos, opp_t = po$o_t, opp_period = po$o_period)]
  shots[, poss_secs := data.table::fifelse(!is.na(opp_t) & opp_period == period_id, t - opp_t, NA_real_)]
  pc <- ev[type_id == 1L & outcome == 1L, .(match_id, team_id, p_idx = idx)]
  pc[, n := seq_len(.N), by = .(match_id, team_id)]
  n_at <- function(key_idx) pc[data.table::data.table(match_id = shots$match_id, team_id = shots$team_id, p_idx = key_idx),
                               on = .(match_id, team_id, p_idx), roll = Inf]$n
  shots[, poss_passes := data.table::fcoalesce(n_at(j_idx), 0L) -
          data.table::fcoalesce(n_at(data.table::fcoalesce(as.numeric(opp_idx), 0) + 0.5), 0L)]
  shots[is.na(poss_secs), poss_passes := NA]

  # rebound: another shot in the same match and period in the 5 s before
  ps <- shots[, .(match_id, r_idx = idx, r_t = t, r_period = period_id)]
  rb <- ps[shots[, .(match_id, j_idx)], on = .(match_id, r_idx = j_idx), roll = Inf]
  shots[, rebound := as.integer(!is.na(rb$r_t) & rb$r_period == period_id & (t - rb$r_t) <= 5)]

  # score before the shot
  g <- shots[type_id == 16L, .(match_id, g_idx = idx, team_id, og = .has_q(qualifier_json, "28"))]
  g[teams, on = "match_id", scorer := ifelse(og, mapply(other_of, team_id, i.teams), team_id)]
  sb <- merge(shots[, .(match_id, event_id, team_id, other, j_idx)], g[, .(match_id, g_idx, scorer)],
              by = "match_id", allow.cartesian = TRUE)
  sb <- sb[g_idx < j_idx, .(goals_for = sum(scorer == team_id), goals_against = sum(scorer == other)),
           by = .(match_id, event_id)]
  shots[sb, on = .(match_id, event_id), `:=`(goals_for = i.goals_for, goals_against = i.goals_against)]
  shots[is.na(goals_for), `:=`(goals_for = 0L, goals_against = 0L)]
  shots[, score_diff := goals_for - goals_against]

  keep <- c("match_id", "event_id", paste0("q", .SHOT_CONTEXT_TAGS), "has_assist", a_vals,
            "poss_secs", "poss_passes", "rebound", "goals_for", "goals_against", "score_diff", "minute")
  shots[, ..keep]
}

#' Each shooter's earlier foot shots, for the weak-foot input
#'
#' For every (player, match): how many foot shots the player took in EARLIER
#' matches (by match date; same-day matches excluded) and how many of those were
#' right-footed. `foot_share` for a shot is then the share of those earlier foot
#' shots taken with the foot used for this shot.
#'
#' @param shot_events Opta shot events with `player_id`, `match_id`,
#'   `body_part` (RightFoot / LeftFoot / Head / ...) and `match_date`.
#' @return data.table: `player_id`, `match_id`, `r_prev`, `n_prev`.
#' @keywords internal
.shot_foot_history <- function(shot_events) {
  s <- data.table::as.data.table(shot_events)
  if ("is_own_goal" %in% names(s)) s <- s[!(is_own_goal %in% TRUE)]   # as in training (prepare_shots_for_xg)
  s <- s[
    !is.na(match_date) & !is.na(player_id) & body_part %in% c("RightFoot", "LeftFoot"),
    .(player_id, match_id = as.character(match_id), match_date = as.Date(match_date), right = as.integer(body_part == "RightFoot"))]
  d <- s[, .(r = sum(right), n = .N), by = .(player_id, match_date)][order(player_id, match_date)]
  d[, `:=`(r_prev = cumsum(r) - r, n_prev = cumsum(n) - n), by = player_id]
  m <- unique(s[, .(player_id, match_id, match_date)])
  out <- m[d, on = .(player_id, match_date), nomatch = NULL][, .(player_id, match_id, r_prev, n_prev)]
  unique(out, by = c("player_id", "match_id"))
}

# Minimum earlier foot shots before foot_share is trusted (below it, NA: the
# model learned "unknown" on the 33% of training shots without enough history).
SHOT_FOOT_MIN <- 10L

.foot_share <- function(is_header, is_right_foot, r_prev, n_prev, min_n = SHOT_FOOT_MIN) {
  n_prev <- as.numeric(n_prev)
  data.table::fifelse(is_header == 0 & data.table::fcoalesce(n_prev, 0) >= min_n,
                      data.table::fifelse(is_right_foot == 1, r_prev / n_prev, 1 - r_prev / n_prev), NA_real_)
}

# Every model input this file can supply. A model whose feature_cols include any
# of them needs the context at scoring time (xG v5, xGOT v3).
.SHOT_CONTEXT_FEATURES <- c(paste0("q", .SHOT_CONTEXT_TAGS), "rebound", "has_assist",
                            paste0("a", .SHOT_ASSIST_TAGS), "a_len", "a_x", "a_y",
                            "poss_secs", "poss_passes", "score_diff", "minute", "foot_share")

#' Add the pre-shot context a model needs to its shot features
#'
#' @param features Shot feature frame, one row per shot in `shots` (same order).
#' @param shots SPADL shot rows: `match_id`, `original_event_id`, `player_id`.
#' @param need Context columns the model reads (from its feature_cols).
#' @param events Full Opta events for these matches (see `.shot_context()`).
#'   Not needed when `shots` already carries every column in `need`.
#' @param foot_history `.shot_foot_history()` output; needed for `foot_share`.
#' @param what Model name for messages.
#' @return `features` with the `need` columns added. Missing values stay NA:
#'   the models were trained with NA meaning "no assist" / "too few earlier
#'   shots", and read it that way (predict_xg honours `na_is_missing`).
#' @keywords internal
.add_shot_context_features <- function(features, shots, need, events = NULL, foot_history = NULL, what = "xG") {
  ctx_need <- setdiff(need, "foot_share")
  if (length(ctx_need)) {
    if (all(ctx_need %in% names(shots))) {
      for (cc in ctx_need) features[[cc]] <- as.numeric(shots[[cc]])
    } else {
      if (is.null(events)) {
        cli::cli_abort(c(
          "This {what} model reads pre-shot context ({length(ctx_need)} inputs) but no {.arg events} were given.",
          "x" = "Scoring without them would put every shot on the wrong branch of the model.",
          "i" = "Pass the full Opta events for these matches: {.code events = load_opta_match_events(league, season)}."
        ))
      }
      cx <- .shot_context(events)
      idx <- match(paste(shots$match_id, shots$original_event_id), paste(cx$match_id, cx$event_id))
      hit <- mean(!is.na(idx))
      if (!is.finite(hit) || hit < 0.95) {
        cli::cli_abort(c(
          "{what}: pre-shot context found for only {round(100 * hit, 1)}% of {nrow(shots)} shots.",
          "x" = "The events given do not cover these shots (wrong league/season, or a filtered event set)."
        ))
      }
      for (cc in ctx_need) features[[cc]] <- as.numeric(cx[[cc]][idx])
    }
  }
  if ("foot_share" %in% need) {
    if (is.null(foot_history)) {
      cli::cli_abort(c(
        "This {what} model reads the shooter's weak foot but no {.arg foot_history} was given.",
        "i" = "Build it once from the shot history: {.code .shot_foot_history(shot_events)}."
      ))
    }
    fh <- data.table::as.data.table(foot_history)
    j <- match(paste(shots$player_id, shots$match_id), paste(fh$player_id, fh$match_id))
    features$foot_share <- .foot_share(features$is_header, features$is_right_foot, fh$r_prev[j], fh$n_prev[j])
  }
  features
}

# Penalty value for a model and season: the model's own by-season table when it
# carries one (earlier-seasons pooled rate, xg-vnext), else PENALTY_XG as before.
.penalty_xg_for <- function(xg_model, season = NULL) {
  tab <- xg_model$panna_metadata$penalty_xg_by_season
  if (is.null(tab) || is.null(season)) return(PENALTY_XG)
  yr <- suppressWarnings(as.integer(extract_season_end_year(season)))
  yrs <- as.integer(names(tab))
  ok <- !is.na(yr) & yrs <= yr
  if (!any(ok)) return(unname(tab[which.min(yrs)]))
  unname(tab[which.max(ifelse(ok, yrs, -Inf))])
}
