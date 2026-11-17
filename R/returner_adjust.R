#' Returning-player adjustment to Panna
#'
#' Players coming back from an absence deliver less than their Panna says for
#' their first games back (panna#497). The rule has no cliffs (Pete,
#' 2026-11-17): each player carries a "rust" value between 0 and 1 (see
#' [returner_rust()]), and his Panna keeps `1 - max_discount * rust` of its
#' lead over replacement level. With the defaults that is 0.85 after missing 4
#' club matches and 0.70 after 8 or more, fading back to 1 over his first 900
#' minutes back. Chosen on a hold-out: returns before 2024-07-01 to fit, later
#' ones to score; test loss 0.00090 against 0.00087 for the earlier flat rule
#' (difference -0.00007 to +0.00015), no adjustment 0.0022
#' (`pannaverse/docs/reviews/panna-validity-2026-11/shrink_smooth_holdout.R`, run 5).
#'
#' The shortfall reads as a return dip (fitness) rather than an out-of-date
#' rating, so this is for a current-form number (the team page). It is never
#' applied to the career rating itself, and match predictions do not use it
#' (out-of-sample error unchanged, 1.0915 -> 1.0914 on 675 matches).
#'
#' @param panna Numeric vector of Panna ratings.
#' @param replacement Replacement-level Panna, length 1 or the same length as
#'   `panna`.
#' @param rust Numeric vector in \[0, 1\], the same length as `panna`; NA counts as 0.
#' @param max_discount Share of the lead over replacement removed at rust 1.
#' @return `panna` pulled towards `replacement` by `max_discount * rust`. `NA`
#'   ratings stay `NA`.
#' @concept panna ratings
#' @export
#' @examples
#' returner_adjusted_panna(c(0.12, 0.12), -0.11, c(1, 0))
returner_adjusted_panna <- function(panna, replacement, rust, max_discount = 0.30) {
  stopifnot(is.numeric(panna), is.numeric(replacement), is.numeric(rust),
            length(rust) == length(panna),
            length(replacement) %in% c(1L, length(panna)),
            is.numeric(max_discount), length(max_discount) == 1L, max_discount >= 0, max_discount <= 1)
  r <- pmin(pmax(data.table::fcoalesce(as.numeric(rust), 0), 0), 1)
  replacement + (panna - replacement) * (1 - max_discount * r)
}

#' How much "rust" a player carries from time out
#'
#' Rust rises with the club matches a player missed and drains as he plays:
#' \itemize{
#'   \item at each appearance, the club matches his previous club played between
#'     his last appearance and this one, `m`, give
#'     `s(m) = (m / centre)^k / (1 + (m / centre)^k)`, an S-curve (0 for no
#'     missed matches, 0.5 at `centre`, close to 1 well past it). Counting
#'     club matches, not days, so the summer break adds nothing;
#'   \item rust before the match is `max(s(m), rust carried)`;
#'   \item after the match it drops by `minutes / drain_minutes`, never below 0.
#' }
#' On `as_of`, a player still out carries `max(s(matches missed so far), rust
#' carried)`. Minutes count every competition in `lineups`, one row per
#' (match, player), capped at `minutes_cap` (the lineups hold a few corrupt
#' values in the tens of thousands). A player who retired or left the covered
#' competitions keeps accruing missed club matches, so filter to the players
#' you are showing before reading any share.
#'
#' @param lineups data.frame with `player_id`, `team_id`, `match_id`,
#'   `match_date` (Date or ISO string) and `minutes_played`. Rows on or after
#'   `as_of` are ignored, so the result is as-at.
#' @param as_of Date the rust is wanted for.
#' @param centre,k S-curve centre (club matches missed) and steepness.
#' @param drain_minutes Minutes back after which the rust from one absence is gone.
#' @param minutes_cap Minutes above this in one match are treated as this.
#' @return data.table, one row per player seen before `as_of`: `player_id`,
#'   `rust`, and for the absence behind it `club_matches_missed`, `days_out` and
#'   `minutes_since_return` (0 while still out; all three NA when rust is 0).
#' @concept panna ratings
#' @export
returner_rust <- function(lineups, as_of, centre = 4, k = 6, drain_minutes = 900, minutes_cap = 120) {
  need <- c("player_id", "team_id", "match_id", "match_date", "minutes_played")
  miss <- setdiff(need, names(lineups))
  if (length(miss)) cli::cli_abort("{.arg lineups} is missing {.val {miss}}.")
  as_of <- as.Date(as_of)
  L <- data.table::as.data.table(lineups)[, need, with = FALSE]
  L[, md := as.Date(substr(as.character(match_date), 1, 10))]
  L <- L[!is.na(md) & md < as_of]
  if (!nrow(L)) return(data.table::data.table(player_id = character(), rust = numeric(), club_matches_missed = numeric(),
                                              days_out = numeric(), minutes_since_return = numeric()))
  L <- unique(L, by = c("match_id", "player_id"))
  L[, mins := pmin(pmax(data.table::fcoalesce(as.numeric(minutes_played), 0), 0), minutes_cap)]
  Tc <- unique(L[, .(team_id, match_id, md)])[, .(nm = .N), by = .(team_id, md)]
  data.table::setorder(Tc, team_id, md); Tc[, cum := cumsum(nm), by = team_id]
  cal <- function(tid, t) {
    r <- Tc[data.table::data.table(team_id = tid, md = t), on = .(team_id, md), roll = TRUE, x.cum]
    data.table::fifelse(is.na(r), 0, r)
  }
  sfun <- function(m) { z <- (pmax(m, 0) / centre)^k; z / (1 + z) }
  A <- L[mins > 0, .(mins = sum(mins), team_id = team_id[which.max(mins)]), by = .(player_id, md)]
  data.table::setorder(A, player_id, md)
  A[, prev_md := data.table::shift(md), by = player_id]
  A[, prev_team := data.table::shift(team_id), by = player_id]
  A[, cm := data.table::fifelse(is.na(prev_md), 0, cal(prev_team, md - 1) - cal(prev_team, prev_md))]
  sv <- sfun(A$cm); pid <- match(A$player_id, unique(A$player_id)); mn <- A$mins; n <- nrow(A)
  ra_v <- numeric(n); jump <- logical(n); last <- 0L; ra <- 0
  for (i in seq_len(n)) {
    if (pid[i] != last) { ra <- 0; last <- pid[i]; rb <- 0 } else { jump[i] <- sv[i] > ra; rb <- max(sv[i], ra) }
    ra <- max(0, rb - mn[i] / drain_minutes); ra_v[i] <- ra
  }
  A[, `:=`(r_after = ra_v, jump = jump)]
  A[, episode := cumsum(jump), by = player_id]
  ep <- A[episode > 0, .(ep_cm = cm[1], ep_days = as.numeric(md[1] - prev_md[1]), ep_mins = sum(mins)),
          by = .(player_id, episode)][, .SD[.N], by = player_id]
  out <- A[, .SD[.N], by = player_id][, .(player_id, last_md = md, last_team = team_id, r_after)]
  out[, cm_now := cal(last_team, as_of - 1) - cal(last_team, last_md)]
  out[, s_now := sfun(cm_now)]
  out[, rust := pmax(s_now, r_after)]
  out <- merge(out, ep[, .(player_id, ep_cm, ep_days, ep_mins)], by = "player_id", all.x = TRUE)
  out[, still_out := s_now >= r_after & s_now > 0.01]
  out[, club_matches_missed := data.table::fifelse(still_out, cm_now, ep_cm)]
  out[, days_out := data.table::fifelse(still_out, as.numeric(as_of - last_md), ep_days)]
  out[, minutes_since_return := data.table::fifelse(still_out, 0, ep_mins)]
  out[rust <= 0, `:=`(club_matches_missed = NA_real_, days_out = NA_real_, minutes_since_return = NA_real_)]
  out[, .(player_id, rust, club_matches_missed, days_out, minutes_since_return)]
}
