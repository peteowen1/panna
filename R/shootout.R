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
