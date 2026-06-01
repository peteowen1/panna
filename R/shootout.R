# Penalty Shootout Win Probability
# =================================
# Exact (non-simulated) win probability for a penalty shootout, given per-kick
# conversion rates. NOT a statistical model — a closed-form calculation. Two
# independent binomials over the remaining regulation kicks, plus a geometric
# sudden-death tiebreak. Works from the start of the shootout OR from any
# mid-shootout state, so it also yields per-kick live win probability (and
# hence per-kick WPA via successive differences).
#
# Convention: "team A" is the team that takes the FIRST kick of the shootout.
# Returns P(team A ultimately wins the shootout).


#' Penalty-shootout win probability (exact, binomial)
#'
#' Computes \eqn{P(\text{team A wins the shootout})} exactly, from the start of
#' the shootout or from any mid-shootout state. No simulation: the regulation
#' phase is two independent binomials over the kicks each team has left, and a
#' level result after regulation is resolved by the sudden-death geometric
#' series.
#'
#' Early termination (stopping kicks once the result is locked) never changes
#' the winner, so it needs no special handling — \eqn{P(\text{A scores more of
#' its remaining kicks})} already accounts for it.
#'
#' Note: with equal conversion rates this returns exactly \code{0.5} — a fair
#' shootout has no structural first-kicker advantage. The empirical ~55-60\%
#' first-kicker edge is a behavioural (scoreboard-pressure) effect that an
#' i.i.d. per-kick model deliberately does not encode.
#'
#' @param p_a,p_b Per-kick conversion probability for team A (first kicker) and
#'   team B. Default \code{PENALTY_SHOOTOUT_CONVERSION} (0.75, the empirical
#'   rate) for both. Pass team-specific rates to get non-50/50 odds.
#' @param score_a,score_b Kicks each team has already scored. Default 0.
#' @param kicks_a,kicks_b Kicks each team has already taken. Default 0.
#' @param n_regulation Kicks per team in the regulation phase. Default 5.
#'
#' @return Numeric scalar in \code{[0, 1]}: P(team A wins).
#' @export
#' @examples
#' shootout_win_prob()                       # 0.5 (equal skill, start)
#' shootout_win_prob(p_a = 0.80, p_b = 0.70) # team A favoured
#' # Live WP after A has scored its 1st kick and B is about to take its 1st:
#' shootout_win_prob(score_a = 1, score_b = 0, kicks_a = 1, kicks_b = 0)
shootout_win_prob <- function(p_a = PENALTY_SHOOTOUT_CONVERSION,
                              p_b = PENALTY_SHOOTOUT_CONVERSION,
                              score_a = 0L, score_b = 0L,
                              kicks_a = 0L, kicks_b = 0L,
                              n_regulation = 5L) {
  stopifnot(p_a >= 0, p_a <= 1, p_b >= 0, p_b <= 1,
            score_a <= kicks_a, score_b <= kicks_b)

  # P(A wins a level sudden-death round eventually): A wins a round iff A scores
  # and B misses; B wins iff A misses and B scores; else the round repeats.
  a_round <- p_a * (1 - p_b)
  b_round <- (1 - p_a) * p_b
  sd_a <- if (a_round + b_round == 0) 0.5 else a_round / (a_round + b_round)

  # Resolve a fully-decided level/led state at a round boundary.
  decided <- function(sa, sb) if (sa > sb) 1 else if (sb > sa) 0 else sd_a

  ra <- n_regulation - kicks_a
  rb <- n_regulation - kicks_b

  # --- Regulation phase (either team still has regulation kicks) ---
  # Remaining kicks are independent binomials; convolve onto current scores.
  if (ra > 0 || rb > 0) {
    ra <- max(0L, ra); rb <- max(0L, rb)
    da <- stats::dbinom(0:ra, ra, p_a)
    db <- stats::dbinom(0:rb, rb, p_b)
    final_a <- score_a + 0:ra
    final_b <- score_b + 0:rb
    joint <- outer(da, db)
    diff  <- outer(final_a, final_b, "-")
    return(sum(joint[diff > 0]) + sum(joint[diff == 0]) * sd_a)
  }

  # --- Sudden death (both teams past regulation) ---
  # A kicks first each round, so well-formed states are kicks_a == kicks_b
  # (round boundary) or kicks_a == kicks_b + 1 (A has kicked, B to respond).
  if (kicks_a == kicks_b) {
    return(decided(score_a, score_b))
  }
  if (kicks_a == kicks_b + 1L) {
    win_if_b_scores <- decided(score_a, score_b + 1L)
    win_if_b_misses <- decided(score_a, score_b)
    return(p_b * win_if_b_scores + (1 - p_b) * win_if_b_misses)
  }
  # kicks_b == kicks_a + 1: start of a round, A about to kick (rare to be asked,
  # but handle for completeness).
  if (kicks_b == kicks_a + 1L) {
    win_if_a_scores <- decided(score_a + 1L, score_b)
    win_if_a_misses <- decided(score_a, score_b)
    return(p_a * win_if_a_scores + (1 - p_a) * win_if_a_misses)
  }

  cli::cli_abort(c(
    "Malformed sudden-death state: kicks_a = {kicks_a}, kicks_b = {kicks_b}.",
    "i" = "In sudden death the two kick counts differ by at most 1."
  ))
}


#' Per-kick win probability and WPA for one shootout
#'
#' Scores every kick of a single penalty shootout: the live win probability
#' after each kick (from \code{\link{shootout_win_prob}}) and the WPA that kick
#' produced — the change in the kicking team's win probability, credited to the
#' kicker. This is purely successive differences of the win-prob function; no
#' separate model.
#'
#' WPA sign convention: positive = the kick helped the KICKER's team. A scored
#' kick is a small positive (a 0.75 conversion is largely "priced in"); a miss
#' is a larger negative (the surprising outcome moves WP more). Sudden-death
#' kicks swing far harder (±0.3-0.4) than early regulation kicks (±0.05).
#'
#' Keep shootout WPA in its OWN column — never add it to open-play WPA. A single
#' sudden-death kick (~±0.4) would swamp a whole match of open-play events
#' (~±0.05 each).
#'
#' Taker vs keeper attribution: a missed kick's negative WPA is split by HOW it
#' missed. A keeper-SAVED miss (\code{type_id == 15}) is partly the keeper's
#' doing, so \code{keeper_save_share} of the (negative) WPA is re-credited as a
#' POSITIVE \code{keeper_wpa} for the opposing team's keeper, and the taker
#' keeps the rest. An off-target miss (skied/post, \code{type_id} 13/14) is all
#' on the taker — no keeper involvement. Scored kicks and the taker portion stay
#' in \code{shootout_wpa}. If \code{type_id} is absent, every miss is treated as
#' all-taker (the simple default) and \code{keeper_wpa} is all zero.
#'
#' @param kicks A data.frame/data.table of one match's shootout kicks, already
#'   filtered to shot-outcome events (\code{type_id} in 16/15/14/13) in
#'   \code{period_id >= 5}, with columns \code{team_id}, \code{scored}
#'   (1 = goal), optionally \code{type_id} (to split saved misses), and
#'   pre-sorted into the order the kicks were taken. The team of the first row
#'   is treated as the first kicker ("A").
#' @param p_a,p_b Per-kick conversion rates. Default
#'   \code{PENALTY_SHOOTOUT_CONVERSION} (0.75) for both.
#' @param keeper_save_share Fraction of a SAVED miss's WPA attributed to the
#'   saving keeper (re-credited positively to the defending team). Default 0.5.
#'   Set 0 to keep all blame on the taker (old behaviour).
#' @param n_regulation Regulation kicks per team. Default 5.
#'
#' @return The input as a data.table with added columns:
#'   \describe{
#'     \item{wp_first_kicker}{P(first-kicking team wins) AFTER this kick}
#'     \item{shootout_wpa}{WPA credited to the TAKER's team (+ = helped taker).
#'       For a saved miss, this is reduced by the keeper's share.}
#'     \item{keeper_wpa}{Positive WPA credited to the SAVING keeper's team on a
#'       saved miss (\code{type_id == 15}); 0 otherwise. Belongs to the team
#'       that did NOT take the kick.}
#'   }
#' @export
score_shootout_kicks <- function(kicks,
                                 p_a = PENALTY_SHOOTOUT_CONVERSION,
                                 p_b = PENALTY_SHOOTOUT_CONVERSION,
                                 keeper_save_share = 0.5,
                                 n_regulation = 5L) {
  dt <- data.table::as.data.table(kicks)
  if (nrow(dt) == 0L) {
    dt[, c("wp_first_kicker", "shootout_wpa", "keeper_wpa") := numeric(0)]
    return(dt[])
  }
  if (!all(c("team_id", "scored") %in% names(dt))) {
    cli::cli_abort("{.arg kicks} must have {.val team_id} and {.val scored} columns")
  }
  has_type <- "type_id" %in% names(dt)

  first_team <- dt$team_id[1]
  is_a <- dt$team_id == first_team

  ka <- kb <- sa <- sb <- 0L
  wp_after   <- numeric(nrow(dt))
  taker_wpa  <- numeric(nrow(dt))
  keeper_wpa <- numeric(nrow(dt))
  prev_wp    <- shootout_win_prob(p_a, p_b, 0L, 0L, 0L, 0L, n_regulation)  # 0.5 at equal p

  for (i in seq_len(nrow(dt))) {
    scored_i <- as.integer(dt$scored[i])
    if (is_a[i]) { ka <- ka + 1L; sa <- sa + scored_i }
    else         { kb <- kb + 1L; sb <- sb + scored_i }

    w <- shootout_win_prob(p_a, p_b, sa, sb, ka, kb, n_regulation)  # P(A wins) after kick
    wp_after[i] <- w

    # WPA from the KICKER's perspective. w is always P(A wins); the change in
    # P(A wins) is A's WPA, and the taker's WPA is signed to their own team.
    delta_a <- w - prev_wp
    kicker_wpa <- if (is_a[i]) delta_a else -delta_a
    prev_wp <- w

    # Split a keeper-saved miss: type_id 15 = saved. The save is good for the
    # DEFENDING team, so keeper_save_share of the (negative) taker WPA is moved
    # out as a positive keeper_wpa for the other team; the taker keeps the rest.
    saved <- has_type && scored_i == 0L && dt$type_id[i] == 15L
    if (saved) {
      keeper_wpa[i] <- -keeper_save_share * kicker_wpa   # -(neg) = positive
      taker_wpa[i]  <- (1 - keeper_save_share) * kicker_wpa
    } else {
      taker_wpa[i]  <- kicker_wpa
    }
  }

  dt[, wp_first_kicker := wp_after]
  dt[, shootout_wpa := taker_wpa]
  dt[, keeper_wpa := keeper_wpa]
  dt[]
}


#' Aggregate shootout WPA per player across matches
#'
#' Runs \code{\link{score_shootout_kicks}} on every match's shootout kicks, then
#' rolls the result up to one row per player, combining their TAKER WPA (own
#' kicks) with their KEEPER WPA (saves they made facing the other team's kicks).
#'
#' Keeper resolution: a saved kick's \code{keeper_wpa} belongs to the defending
#' team but the shot event names only the taker. We resolve the specific keeper
#' by joining \code{lineups} — the opposing team's goalkeeper in that match (the
#' \code{position == "Goalkeeper"} player who was on the pitch at the shootout;
#' if a match lists several, the one with the most minutes, i.e. the end-of-match
#' keeper who actually faced the kicks). If no lineups are supplied, keeper WPA
#' is still summed at team level but cannot be attributed to a player and is
#' dropped from the per-player total (reported separately as
#' \code{unattributed_keeper_wpa}).
#'
#' @param kicks_all A data.frame/data.table of shootout kicks across one or more
#'   matches: shot-outcome events (\code{type_id} 16/15/14/13, \code{period_id
#'   >= 5}) with \code{match_id}, \code{team_id}, \code{player_id},
#'   \code{player_name}, \code{scored}, and orderable \code{minute}/\code{second}
#'   (or pre-sorted within match).
#' @param lineups Optional lineup table with \code{match_id}, \code{team_id},
#'   \code{player_id}, \code{player_name}, \code{position},
#'   \code{minutes_played} — used to resolve the saving keeper per match.
#' @param keeper_save_share Passed to \code{\link{score_shootout_kicks}}.
#'   Default 0.5.
#' @param n_regulation Regulation kicks per team. Default 5.
#'
#' @return A data.table, one row per player, with: \code{player_id},
#'   \code{player_name}, \code{kicks_taken}, \code{kicks_scored},
#'   \code{taker_wpa} (sum over own kicks), \code{keeper_wpa} (sum over saves
#'   made), \code{shootout_wpa_total} (\code{taker_wpa + keeper_wpa}).
#' @export
aggregate_shootout_wpa <- function(kicks_all, lineups = NULL,
                                   keeper_save_share = 0.5,
                                   n_regulation = 5L) {
  dt <- data.table::as.data.table(kicks_all)
  req <- c("match_id", "team_id", "player_id", "scored")
  if (!all(req %in% names(dt))) {
    cli::cli_abort("{.arg kicks_all} must contain: {.val {req}}")
  }
  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      player_id = character(0), player_name = character(0),
      kicks_taken = integer(0), kicks_scored = integer(0),
      taker_wpa = numeric(0), keeper_wpa = numeric(0),
      shootout_wpa_total = numeric(0)))
  }
  if (!"player_name" %in% names(dt)) dt[, player_name := player_id]

  # Drop kicks with no resolvable taker (blank/NA player_id) — a data gap, not
  # a player; ranking it would put a phantom at the top of the leaderboard.
  n_before <- nrow(dt)
  dt <- dt[!is.na(player_id) & player_id != ""]
  if (nrow(dt) < n_before) {
    cli::cli_warn("Dropped {n_before - nrow(dt)} shootout kick(s) with missing player_id.")
  }
  if (nrow(dt) == 0L) {
    return(data.table::data.table(
      player_id = character(0), player_name = character(0),
      kicks_taken = integer(0), kicks_scored = integer(0),
      taker_wpa = numeric(0), keeper_wpa = numeric(0),
      shootout_wpa_total = numeric(0)))
  }

  # Order within each match if time columns are present.
  ord <- intersect(c("period_id", "minute", "second", "event_id"), names(dt))
  if (length(ord) > 0) data.table::setorderv(dt, c("match_id", ord))

  # Score each match's kicks.
  scored <- dt[, score_shootout_kicks(.SD, keeper_save_share = keeper_save_share,
                                      n_regulation = n_regulation),
               by = match_id]

  # --- Taker WPA: sum each player's own kicks ---
  taker <- scored[, .(kicks_taken = .N,
                      kicks_scored = sum(scored, na.rm = TRUE),
                      taker_wpa = sum(shootout_wpa, na.rm = TRUE)),
                  by = .(player_id, player_name)]

  # --- Keeper WPA: per (match, defending team), resolve to the keeper ---
  saves <- scored[keeper_wpa != 0]
  keeper_tbl <- data.table::data.table(
    player_id = character(0), player_name = character(0), keeper_wpa = numeric(0))
  unattributed <- 0
  if (nrow(saves) > 0) {
    # Defending team = the team that did NOT take the saved kick.
    def <- saves[, .(keeper_wpa = sum(keeper_wpa)), by = .(match_id, kicker_team = team_id)]
    if (!is.null(lineups)) {
      lu <- data.table::as.data.table(lineups)
      gks <- lu[position == "Goalkeeper",
                .(player_id, player_name, minutes_played), by = .(match_id, team_id)]
      # one keeper per (match, team): the one who played the most minutes
      data.table::setorder(gks, match_id, team_id, -minutes_played)
      gks <- gks[, .SD[1], by = .(match_id, team_id)]
      # join: defending team in match -> its keeper
      def <- merge(def, gks, by.x = c("match_id", "kicker_team"),
                   by.y = c("match_id", "team_id"),
                   all.x = TRUE)
      # NB: kicker_team is the TAKER's team; the keeper is the OTHER team's GK.
      # Re-resolve: we need the keeper of the team that did NOT kick. Build a
      # per-match team->gk map and pick the non-kicking team.
      def[, player_id := NULL][, player_name := NULL][, minutes_played := NULL]
      teams_per_match <- gks[, .(match_id, team_id, player_id, player_name)]
      def <- merge(def, teams_per_match, by = "match_id", allow.cartesian = TRUE)
      def <- def[team_id != kicker_team]   # keeper's team != taker's team
      keeper_tbl <- def[, .(keeper_wpa = sum(keeper_wpa)), by = .(player_id, player_name)]
    } else {
      unattributed <- sum(def$keeper_wpa)
    }
  }

  # --- Combine taker + keeper per player ---
  out <- merge(taker, keeper_tbl, by = c("player_id", "player_name"), all = TRUE)
  out[is.na(kicks_taken), kicks_taken := 0L]
  out[is.na(kicks_scored), kicks_scored := 0L]
  out[is.na(taker_wpa), taker_wpa := 0]
  out[is.na(keeper_wpa), keeper_wpa := 0]
  out[, shootout_wpa_total := taker_wpa + keeper_wpa]
  data.table::setorder(out, -shootout_wpa_total)

  if (unattributed != 0) {
    attr(out, "unattributed_keeper_wpa") <- unattributed
    cli::cli_warn(c(
      "No lineups supplied: {round(unattributed, 3)} of keeper WPA could not be ",
      "attributed to a player and is omitted from per-player totals."))
  }
  out[]
}
