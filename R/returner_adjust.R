#' Returning-player adjustment to Panna
#'
#' Players coming back after a spell with no minutes deliver less than their
#' Panna says for their first few games back (panna#497). Across 8,404 returns
#' after 60-179 days out, their team's xG difference with them on the pitch fell
#' 0.032 per 90 short of their Panna relative to same-rated regulars. A shrink
#' that grows with time away over-corrected players back from 180+ days. A flat
#' discount, tuned on returns before 2024-07-01, did best on later ones: keep
#' 85% of the player's lead over replacement level. Test loss fell from 0.0022
#' to 0.0009, and regulars were unchanged
#' (`pannaverse/docs/reviews/panna-validity-2026-11/shrink_shapes_holdout.R`).
#'
#' The shortfall reads as a return dip (fitness) rather than an out-of-date
#' rating, so this is applied where a current-form number is wanted (the team
#' page, match predictions). It is never applied to the career rating itself.
#'
#' @param panna Numeric vector of Panna ratings.
#' @param replacement Replacement-level Panna, length 1 or the same length as
#'   `panna`.
#' @param returning Logical vector, the same length as `panna`: is the player
#'   returning? See [returner_status()].
#' @param keep Share of the lead over replacement kept while returning.
#' @return `panna` with returning players pulled towards `replacement`. `NA`
#'   inputs stay `NA`.
#' @concept panna ratings
#' @export
#' @examples
#' returner_adjusted_panna(c(0.12, 0.12), -0.11, c(TRUE, FALSE))
returner_adjusted_panna <- function(panna, replacement, returning, keep = 0.85) {
  stopifnot(is.numeric(panna), is.numeric(replacement), is.logical(returning),
            length(returning) == length(panna),
            length(replacement) %in% c(1L, length(panna)),
            is.numeric(keep), length(keep) == 1L, keep >= 0, keep <= 1)
  adj <- replacement + (panna - replacement) * keep
  ifelse(returning %in% TRUE, adj, panna)
}

#' Is a player returning from a spell with no minutes?
#'
#' A player is returning on `as_of` when either:
#' \itemize{
#'   \item he is still out: no minutes for at least `gap_days` before `as_of`,
#'     while his last club played at least `club_matches` matches since; or
#'   \item he is back: his last such gap ended before `as_of`, and he has played
#'     fewer than `window_minutes` minutes since it ended. The gap counts only if
#'     the club he came back to had played at least `club_matches` matches in it
#'     and he had played for that club before the gap (so a transfer debut is
#'     not a return).
#' }
#' This is the same return rule as the panna#497 validity tests
#' (`respell()` / `build_hist()` in pannaverse
#' `docs/reviews/panna-validity-2026-11/panna_validity_common.R`), which is what
#' the 0.85 in [returner_adjusted_panna()] was tuned on. Minutes count every
#' competition in `lineups`, one row per (match, player), capped at
#' `minutes_cap` (the lineups hold a few corrupt values in the tens of thousands). A player who retired or left the
#' covered competitions stays "still out" indefinitely (19.7% of the 100,378
#' players seen since 2024, as at 2026-11-09), so filter to the players you
#' are showing or predicting for before reading the share.
#'
#' @param lineups data.frame with `player_id`, `team_id`, `match_id`,
#'   `match_date` (Date or ISO string) and `minutes_played`. Rows on or after
#'   `as_of` are ignored, so the result is as-at.
#' @param as_of Date the status is wanted for.
#' @param gap_days,club_matches,window_minutes The return rule.
#' @param minutes_cap Minutes above this in one match are treated as this.
#' @return data.table with one row per player seen before `as_of`: `player_id`,
#'   `returning` (logical), `days_out` (length of the current gap if he is still
#'   out, else of his last qualifying gap; NA if none) and `minutes_since_return`
#'   (0 while still out, even if he had an earlier return; NA if he never had a
#'   qualifying gap).
#' @concept panna ratings
#' @export
returner_status <- function(lineups, as_of, gap_days = 60L, club_matches = 8L,
                            window_minutes = 900, minutes_cap = 120) {
  need <- c("player_id", "team_id", "match_id", "match_date", "minutes_played")
  miss <- setdiff(need, names(lineups))
  if (length(miss)) cli::cli_abort("{.arg lineups} is missing {.val {miss}}.")
  as_of <- as.Date(as_of)
  L <- data.table::as.data.table(lineups)[, need, with = FALSE]
  L[, md := as.Date(substr(as.character(match_date), 1, 10))]
  L <- L[!is.na(md) & md < as_of]
  if (!nrow(L)) return(data.table::data.table(player_id = character(), returning = logical(),
                                              days_out = numeric(), minutes_since_return = numeric()))
  L <- unique(L, by = c("match_id", "player_id"))
  L[, mins := pmin(pmax(data.table::fcoalesce(as.numeric(minutes_played), 0), 0), minutes_cap)]
  # club calendar: distinct matches per club, cumulative by date
  Tc <- unique(L[, .(team_id, match_id, md)])[, .(nm = .N), by = .(team_id, md)]
  data.table::setorder(Tc, team_id, md); Tc[, cum := cumsum(nm), by = team_id]
  cal <- function(tid, t) {
    r <- Tc[data.table::data.table(team_id = tid, md = t), on = .(team_id, md), roll = TRUE, x.cum]
    data.table::fifelse(is.na(r), 0, r)
  }
  A <- L[mins > 0, .(mins = sum(mins), team_id = team_id[which.max(mins)]), by = .(player_id, md)]
  data.table::setorder(A, player_id, md)
  fpt <- A[, .(first_pt = min(md)), by = .(player_id, team_id)]   # first appearance for each club
  A[, prev_md := data.table::shift(md), by = player_id]
  A[, gap := as.numeric(md - prev_md)]
  cand <- A[!is.na(gap) & gap >= gap_days]
  cand <- merge(cand, fpt, by = c("player_id", "team_id"))
  # matches of the club he CAME BACK TO during the gap, and he must have played for it before (as respell())
  cand[, cm := cal(team_id, md - 1) - cal(team_id, prev_md)]
  ret <- cand[cm >= club_matches & first_pt <= prev_md, .(player_id, ret_md = md, days_out = gap)]
  last_ret <- ret[, .SD[which.max(ret_md)], by = player_id]
  # minutes since the last return (that return's match included)
  A <- merge(A, last_ret, by = "player_id", all.x = TRUE)
  since <- A[!is.na(ret_md) & md >= ret_md, .(minutes_since_return = sum(mins)), by = player_id]
  # still out now
  last <- A[, .(last_md = max(md), last_team = team_id[which.max(md)]), by = player_id]
  last[, out_days := as.numeric(as_of - last_md)]
  last[, out_cm := cal(last_team, as_of - 1) - cal(last_team, last_md)]
  out <- merge(last[, .(player_id, out_days, out_cm)], last_ret[, .(player_id, days_out)], by = "player_id", all.x = TRUE)
  out <- merge(out, since, by = "player_id", all.x = TRUE)
  out[, still_out := out_days >= gap_days & out_cm >= club_matches]
  out[, back := !is.na(days_out) & data.table::fcoalesce(minutes_since_return, 0) < window_minutes]
  out[, returning := still_out | back]
  out[still_out == TRUE, `:=`(days_out = out_days, minutes_since_return = 0)]
  out[, .(player_id, returning, days_out, minutes_since_return)]
}
