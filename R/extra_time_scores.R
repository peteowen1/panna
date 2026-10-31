#' Score after extra time, and the penalty-shootout winner, from Opta events
#'
#' `home_goals` / `away_goals` in the match results are the 90-minute score
#' (Opta's matchstats). For a knockout match settled in extra time that
#' under-reports the result (2014 final: 0-0, actual 1-0 aet). This adds the
#' extra-time goals (`period_id` 3 and 4) to the 90-minute score and reads the
#' shootout (`period_id` 5) for the winner.
#'
#' Own goals are credited to the side that benefits: Opta tags an own goal's
#' `team_id` with the scorer's own team and marks it with qualifier 28, so the
#' credited side is the opponent. The same rule reproduces the fixtures score
#' for 899 of 900 World Cup matches from 90-minute goal events alone.
#'
#' @param events Match events with `match_id`, `type_id`, `team_id`,
#'   `period_id` and `qualifier_json`. Goals are `type_id == 16`.
#' @param scores One row per match with `match_id`, `home_team_id`,
#'   `away_team_id`, `home_goals`, `away_goals` (the 90-minute score).
#' @param events_match_ids Match ids that have any events at all. Defaults to
#'   those in `events`; pass it when `events` was pre-filtered to goals and
#'   late periods, so a 0-0 match is not mistaken for a match without events.
#'
#' @return A data frame with one row per `scores` row: `match_id`,
#'   `home_goals_aet`, `away_goals_aet` (equal to the 90-minute score when the
#'   match has events but none in period 3 or 4; `NA` when the match has no
#'   events at all, since extra time cannot then be ruled out, or when either
#'   team id is missing, since no goal can then be credited to a side) and
#'   `shootout_winner` (`"home"`, `"away"`, or `NA` when there was no shootout).
#' @keywords internal
#' @noRd
derive_extra_time_scores <- function(events, scores,
                                     events_match_ids = events$match_id) {
  n <- nrow(scores)
  hg <- as.numeric(scores$home_goals)
  ag <- as.numeric(scores$away_goals)
  out <- data.frame(match_id = scores$match_id, home_goals_aet = hg,
                    away_goals_aet = ag, shootout_winner = NA_character_,
                    stringsAsFactors = FALSE)
  if (n == 0L) return(out)

  idx <- match(events$match_id, scores$match_id)
  keep <- !is.na(idx)
  events <- events[keep, , drop = FALSE]
  idx <- idx[keep]
  home_id <- scores$home_team_id[idx]
  away_id <- scores$away_team_id[idx]

  has_events <- scores$match_id %in% events_match_ids

  is_goal <- events$type_id == 16L
  own_goal <- grepl("\"28\":", events$qualifier_json, fixed = TRUE)
  credited <- ifelse(own_goal,
                     ifelse(events$team_id == home_id, away_id, home_id),
                     events$team_id)

  # Goals per match credited to the home / away side among the selected events.
  count_side <- function(sel) {
    list(home = as.numeric(tabulate(idx[which(sel & credited == home_id)], nbins = n)),
         away = as.numeric(tabulate(idx[which(sel & credited == away_id)], nbins = n)))
  }

  et <- count_side(is_goal & events$period_id %in% 3:4)
  out$home_goals_aet <- hg + et$home
  out$away_goals_aet <- ag + et$away
  # Without both team ids no goal can be credited to a side, so the counts
  # above read 0 (or one-sided): NA, not a copy of the 90-minute score.
  no_ids <- is.na(scores$home_team_id) | is.na(scores$away_team_id)
  out$home_goals_aet[!has_events | no_ids] <- NA_real_
  out$away_goals_aet[!has_events | no_ids] <- NA_real_

  # Shootout goals are credited to the taker's team (no own goals in a shootout).
  so <- count_side(is_goal & events$period_id == 5L)
  out$shootout_winner <- ifelse(so$home > so$away, "home",
                         ifelse(so$away > so$home, "away", NA_character_))
  out$shootout_winner[no_ids] <- NA_character_
  out
}

#' Add `home_goals_aet`, `away_goals_aet` and `shootout_winner` to predictions
#'
#' Leagues that cannot have extra time (`PANNA_DOMESTIC_LEAGUES`) get the
#' 90-minute score and no shootout winner without loading any events. For the
#' rest, events are loaded per league season and fed to
#' [derive_extra_time_scores()]. A league season whose events fail to load
#' keeps `NA` aet columns (with a warning) rather than a possibly wrong copy of
#' the 90-minute score.
#'
#' @param predictions Data frame with `match_id`, `league`, `season`,
#'   `home_goals`, `away_goals`.
#' @param fixtures Data frame with `match_id`, `home_team_id`, `away_team_id`
#'   (the team ids are not in the predictions table).
#' @param load_events Function `(league, season)` returning that league
#'   season's match events.
#' @return `predictions` with the three columns appended.
#' @keywords internal
#' @noRd
add_extra_time_scores <- function(predictions, fixtures,
                                  load_events = function(league, season) {
                                    load_opta_match_events(
                                      league, season = season,
                                      columns = c("match_id", "type_id", "team_id",
                                                  "period_id", "qualifier_json"))
                                  }) {
  predictions$home_goals_aet <- as.numeric(predictions$home_goals)
  predictions$away_goals_aet <- as.numeric(predictions$away_goals)
  predictions$shootout_winner <- NA_character_

  played <- !is.na(predictions$home_goals) & !is.na(predictions$away_goals)
  et_capable <- played & !predictions$league %in% PANNA_DOMESTIC_LEAGUES
  ids <- fixtures[!duplicated(fixtures$match_id),
                  c("match_id", "home_team_id", "away_team_id"), drop = FALSE]

  groups <- unique(predictions[et_capable, c("league", "season"), drop = FALSE])
  for (g in seq_len(nrow(groups))) {
    lg <- groups$league[g]
    ssn <- groups$season[g]
    rows <- which(et_capable & predictions$league == lg & predictions$season == ssn)
    ev <- tryCatch(load_events(lg, ssn), error = function(e) {
      cli::cli_warn("Extra-time scores for {lg} {ssn}: {conditionMessage(e)}; aet columns left NA")
      NULL
    })
    if (is.null(ev)) {
      predictions$home_goals_aet[rows] <- NA_real_
      predictions$away_goals_aet[rows] <- NA_real_
      next
    }
    ev <- as.data.frame(ev)
    with_events <- unique(ev$match_id)
    ev <- ev[which(ev$type_id == 16L | ev$period_id >= 3L), , drop = FALSE]
    sc <- merge(predictions[rows, c("match_id", "home_goals", "away_goals")],
                ids, by = "match_id", all.x = TRUE, sort = FALSE)
    d <- derive_extra_time_scores(ev, sc, events_match_ids = with_events)
    m <- match(predictions$match_id[rows], d$match_id)
    predictions$home_goals_aet[rows] <- d$home_goals_aet[m]
    predictions$away_goals_aet[rows] <- d$away_goals_aet[m]
    predictions$shootout_winner[rows] <- d$shootout_winner[m]
  }
  predictions
}
