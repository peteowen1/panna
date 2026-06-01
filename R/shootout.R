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
#' @param kicks A data.frame/data.table of one match's shootout kicks, already
#'   filtered to shot-outcome events (\code{type_id} in 16/15/14/13) in
#'   \code{period_id >= 5}, with columns \code{team_id}, \code{scored}
#'   (1 = goal), and pre-sorted into the order the kicks were taken. The team
#'   of the first row is treated as the first kicker ("A").
#' @param p_a,p_b Per-kick conversion rates. Default
#'   \code{PENALTY_SHOOTOUT_CONVERSION} (0.75) for both.
#' @param n_regulation Regulation kicks per team. Default 5.
#'
#' @return The input as a data.table with added columns:
#'   \describe{
#'     \item{wp_first_kicker}{P(first-kicking team wins) AFTER this kick}
#'     \item{shootout_wpa}{WPA credited to this kick's team (+ = helped kicker)}
#'   }
#' @export
score_shootout_kicks <- function(kicks,
                                 p_a = PENALTY_SHOOTOUT_CONVERSION,
                                 p_b = PENALTY_SHOOTOUT_CONVERSION,
                                 n_regulation = 5L) {
  dt <- data.table::as.data.table(kicks)
  if (nrow(dt) == 0L) {
    dt[, c("wp_first_kicker", "shootout_wpa") := numeric(0)]
    return(dt[])
  }
  if (!all(c("team_id", "scored") %in% names(dt))) {
    cli::cli_abort("{.arg kicks} must have {.val team_id} and {.val scored} columns")
  }

  first_team <- dt$team_id[1]
  is_a <- dt$team_id == first_team

  ka <- kb <- sa <- sb <- 0L
  wp_after <- numeric(nrow(dt))
  prev_wp  <- shootout_win_prob(p_a, p_b, 0L, 0L, 0L, 0L, n_regulation)  # 0.5 at equal p
  wpa      <- numeric(nrow(dt))

  for (i in seq_len(nrow(dt))) {
    scored_i <- as.integer(dt$scored[i])
    if (is_a[i]) { ka <- ka + 1L; sa <- sa + scored_i }
    else         { kb <- kb + 1L; sb <- sb + scored_i }

    w <- shootout_win_prob(p_a, p_b, sa, sb, ka, kb, n_regulation)  # P(A wins) after kick
    wp_after[i] <- w

    # WPA from the KICKER's perspective. w is always P(A wins); the change in
    # P(A wins) is A's WPA, and B's WPA is the negation (zero-sum).
    delta_a <- w - prev_wp
    wpa[i]  <- if (is_a[i]) delta_a else -delta_a
    prev_wp <- w
  }

  dt[, wp_first_kicker := wp_after]
  dt[, shootout_wpa := wpa]
  dt[]
}
