# Tests for the FIFA-2026-bracket World Cup simulator

# Slot eligibility lists in R32 match order (74, 77, 79, 80, 81, 82, 85, 87)
wc_slot_cands <- function() {
  lapply(c("ABCDF", "CDFGH", "CEFHI", "EHIJK", "BEFIJ", "AEHIJ", "EFGIJ",
           "DEIJL"),
         function(s) match(strsplit(s, "")[[1]], LETTERS))
}

test_that("third-place allocation is valid for all 495 combinations", {
  cands <- wc_slot_cands()
  alloc <- panna:::build_third_allocation(cands)
  combos <- utils::combn(12L, 8L)
  for (ci in seq_len(ncol(combos))) {
    qual <- combos[, ci]
    row <- alloc[sum(2L^(qual - 1L)) + 1L, ]
    expect_false(anyNA(row))
    expect_setequal(row, qual)                      # all 8 qualified used once
    for (s in seq_along(cands)) {
      expect_true(row[s] %in% cands[[s]])           # slot eligibility respected
    }
  }
})

test_that("K and L thirds are forced to their only slots", {
  cands <- wc_slot_cands()
  alloc <- panna:::build_third_allocation(cands)
  qual <- match(c("A", "B", "C", "D", "G", "K", "L", "E"), LETTERS)
  row <- alloc[sum(2L^(qual - 1L)) + 1L, ]
  expect_equal(row[4], match("K", LETTERS))         # M80 (W-L vs 3rd)
  expect_equal(row[8], match("L", LETTERS))         # M87 (W-K vs 3rd)
})

test_that("head-to-head breaks an overall points/GD/GF tie", {
  # T1 and T2 both finish 6 pts, GD +1, GF 2 — but T1 beat T2.
  # Matches (local slots): T1>T2 1-0, T3>T1 1-0, T1>T4 1-0,
  #                        T2>T3 1-0, T2>T4 1-0, T4>T3 1-0
  m_a <- c(1L, 3L, 1L, 2L, 2L, 4L)
  m_b <- c(2L, 1L, 4L, 3L, 4L, 3L)
  g_a <- c(1L, 1L, 1L, 1L, 1L, 1L)
  g_b <- c(0L, 0L, 0L, 0L, 0L, 0L)
  p <- c(6L, 6L, 3L, 3L)
  d <- c(1L, 1L, -1L, -1L)
  f <- c(2L, 2L, 1L, 1L)
  for (i in 1:50) {
    ord <- panna:::rank_group_h2h(p, d, f, stats::runif(4), m_a, m_b, g_a, g_b)
    expect_equal(ord[1], 1L)   # T1 above T2 every time, regardless of tbk
    expect_equal(ord[2], 2L)
  }
})

test_that("deterministic strengths produce the official bracket paths", {
  # 48 teams; group g (A..L) holds strength ranks g, 12+g, 24+g, 36+g, so
  # every group winner is rank g, runner-up 12+g, third 24+g. Lower rank
  # always beats higher rank with certainty and no draws.
  teams <- sprintf("T%02d", 1:48)
  groups <- data.frame(
    group = rep(LETTERS[1:12], times = 4),
    team  = teams,
    stringsAsFactors = FALSE
  )

  pairs <- t(utils::combn(teams, 2))
  rank1 <- as.integer(substr(pairs[, 1], 2, 3))
  rank2 <- as.integer(substr(pairs[, 2], 2, 3))
  probs <- data.frame(
    t1 = pairs[, 1], t2 = pairs[, 2],
    p_t1 = as.numeric(rank1 < rank2),
    p_draw = 0,
    p_t2 = as.numeric(rank1 > rank2),
    lambda_t1 = ifelse(rank1 < rank2, 2, 0.2),
    lambda_t2 = ifelse(rank1 > rank2, 2, 0.2),
    stringsAsFactors = FALSE
  )
  knockout <- list(
    probs = probs,
    team_elo = stats::setNames(rep(1500, 48), teams)
  )
  predictions <- data.frame(home_team = character(0), away_team = character(0),
                            prob_H = numeric(0), prob_D = numeric(0),
                            prob_A = numeric(0), pred_home_goals = numeric(0),
                            pred_away_goals = numeric(0))

  sim <- simulate_world_cup(predictions, groups, knockout,
                            n_sims = 40L, elo_k = 0, verbose = FALSE)
  s <- sim$summary

  # T01 (Group A winner) is on the M79 -> M92 -> M99 -> M102 side;
  # T04 (Group D winner) is the strongest team on the M101 side.
  # T02/T03 share T01's half, so they can never reach the final.
  expect_equal(s$p_champ[s$team == "T01"], 100)
  expect_equal(s$p_final[s$team == "T04"], 100)
  expect_equal(s$p_final[s$team == "T02"], 0)
  expect_equal(s$p_SF[s$team == "T02"], 100)    # T02 falls to T01 in SF102
  expect_equal(sim$bracket, "fifa2026")

  # Group winners by construction
  gt <- sim$group_table
  expect_equal(gt$pos1[gt$team == "T01"], 100)
  expect_equal(gt$pos3[gt$team == "T25"], 100)
})

test_that("scoreline tables sample the true conditional Poisson margin", {
  # Guards the fix for the goal-difference bias: scorelines must come from the
  # independent-Poisson joint pmf restricted to the drawn outcome's region,
  # not from independent draws patched to agree. The old patching rule snapped
  # wins to the smallest consistent margin (P(1-goal win) 0.70 vs a true 0.44)
  # and made every draw the HOME team's Poisson draw doubled. Group GD is a
  # FIFA tiebreak, so both biases reached the published advancement numbers.
  l1 <- 1.8; l2 <- 1.1
  sc <- panna:::build_scoreline_tables(l1, l2)

  g <- 0:8
  p1 <- c(stats::dpois(0:7, l1), 1 - sum(stats::dpois(0:7, l1)))
  p2 <- c(stats::dpois(0:7, l2), 1 - sum(stats::dpois(0:7, l2)))
  J  <- outer(p1, p2)
  GD <- outer(g, g, "-")

  draw_region <- function(cum, gg1, gg2, n) {
    k <- findInterval(stats::runif(n), cum) + 1L
    gg1[k] - gg2[k]
  }
  set.seed(11)
  n <- 200000L
  gd_w <- draw_region(sc$win_cum[, 1],  sc$win_g1,  sc$win_g2,  n)
  gd_d <- draw_region(sc$draw_cum[, 1], sc$draw_g1, sc$draw_g2, n)
  gd_l <- draw_region(sc$loss_cum[, 1], sc$loss_g1, sc$loss_g2, n)

  # Regions are respected exactly -- a sampled scoreline can never contradict
  # the outcome the model drew.
  expect_true(all(gd_w > 0))
  expect_true(all(gd_d == 0))
  expect_true(all(gd_l < 0))

  # And the margin within each region matches the analytic conditional mean.
  expect_equal(mean(gd_w), sum(J[GD > 0] * GD[GD > 0]) / sum(J[GD > 0]),
               tolerance = 0.02)
  expect_equal(mean(gd_l), sum(J[GD < 0] * GD[GD < 0]) / sum(J[GD < 0]),
               tolerance = 0.02)

  # Draws use BOTH lambdas, not the home lambda twice.
  lvl <- sc$draw_g1[findInterval(stats::runif(n), sc$draw_cum[, 1]) + 1L]
  expect_equal(mean(lvl), sum(diag(J) * g) / sum(diag(J)), tolerance = 0.02)
})

test_that("build_scoreline_tables rejects unusable lambdas", {
  expect_error(panna:::build_scoreline_tables(c(1.2, NA), c(1.0, 1.0)),
               "must be finite")
  expect_error(panna:::build_scoreline_tables(c(1.2, 1.1), 1.0),
               "same length")
})

test_that("a knockout pair with no expected goals aborts by name", {
  teams <- sprintf("U%02d", 1:48)
  groups <- data.frame(group = rep(LETTERS[1:12], times = 4), team = teams,
                       stringsAsFactors = FALSE)
  prs <- t(utils::combn(teams, 2))
  probs <- data.frame(t1 = prs[, 1], t2 = prs[, 2],
                      p_t1 = 0.4, p_draw = 0.2, p_t2 = 0.4,
                      lambda_t1 = 1.5, lambda_t2 = 1.2,
                      stringsAsFactors = FALSE)
  probs$lambda_t1[1] <- NA_real_          # U01 vs U02 has no expected goals
  knockout <- list(probs = probs,
                   team_elo = stats::setNames(rep(1500, 48), teams))
  preds <- data.frame(home_team = character(0), away_team = character(0),
                      prob_H = numeric(0), prob_D = numeric(0),
                      prob_A = numeric(0), pred_home_goals = numeric(0),
                      pred_away_goals = numeric(0))
  expect_error(
    simulate_world_cup(preds, groups, knockout, n_sims = 2L, verbose = FALSE),
    "U01 vs U02"
  )
})

test_that("non-standard groups fall back to the random bracket with a warning", {
  # 12 groups of 4 (the size the thirds logic requires) but lettered M-X,
  # so the FIFA bracket's A-L slot references cannot apply.
  teams <- sprintf("S%02d", 1:48)
  groups <- data.frame(group = rep(LETTERS[13:24], times = 4), team = teams,
                       stringsAsFactors = FALSE)
  pairs <- t(utils::combn(teams, 2))
  probs <- data.frame(
    t1 = pairs[, 1], t2 = pairs[, 2],
    p_t1 = 0.4, p_draw = 0.2, p_t2 = 0.4,
    lambda_t1 = 1.2, lambda_t2 = 1.2, stringsAsFactors = FALSE
  )
  knockout <- list(probs = probs,
                   team_elo = stats::setNames(rep(1500, 48), teams))
  predictions <- data.frame(home_team = character(0), away_team = character(0),
                            prob_H = numeric(0), prob_D = numeric(0),
                            prob_A = numeric(0), pred_home_goals = numeric(0),
                            pred_away_goals = numeric(0))
  expect_warning(
    sim <- simulate_world_cup(predictions, groups, knockout,
                              n_sims = 5L, elo_k = 0, verbose = FALSE),
    "falling back"
  )
  expect_equal(sim$bracket, "random")
})

test_that("group ranking applies GD before GF", {
  # Equal points everywhere; team 2 has the better GD, team 1 the better GF.
  # GD must dominate — swapping -d and -f in the order() call would rank
  # team 1 first. No (p, d, f) cluster is fully tied, so head-to-head never
  # fires and the ordering comes purely from the points > GD > GF cascade.
  p <- c(4L, 4L, 4L, 4L)
  d <- c(1L, 2L, -1L, -2L)
  f <- c(5L, 2L, 4L, 0L)
  m_a <- c(1L, 1L, 1L, 2L, 2L, 3L)
  m_b <- c(2L, 3L, 4L, 3L, 4L, 4L)
  g0  <- integer(6)
  for (k in 1:25) {
    ord <- panna:::rank_group_h2h(p, d, f, stats::runif(4), m_a, m_b, g0, g0)
    expect_equal(ord, c(2L, 1L, 3L, 4L))
  }
})

test_that("3-way head-to-head tie cascades through h2h GD then h2h GF", {
  # Teams 1-3 fully tied on overall points/GD/GF (cluster of three).
  # Mutual results: 1 beats 2 3-0, 2 beats 3 2-0, 3 beats 1 1-0 — a cycle,
  # so h2h points tie at 3 each and ranking falls to h2h GD:
  #   team 1: +3 -1 = +2, team 2: -3 +2 = -1, team 3: -2 +1 = -1.
  # Teams 2 and 3 then tie on h2h GD and split on h2h GF (2 vs 1).
  # tbk is set adversarially (team 3 best draw) to prove the random
  # tiebreak is NOT what decides.
  p <- c(6L, 6L, 6L, 0L)
  d <- c(2L, 2L, 2L, -6L)
  f <- c(5L, 5L, 5L, 1L)
  m_a <- c(1L, 2L, 3L, 1L, 2L, 3L)
  m_b <- c(2L, 3L, 1L, 4L, 4L, 4L)
  g_a <- c(3L, 2L, 1L, 1L, 1L, 1L)
  g_b <- c(0L, 0L, 0L, 0L, 0L, 0L)
  tbk <- c(0.9, 0.8, 0.1, 0.5)
  ord <- panna:::rank_group_h2h(p, d, f, tbk, m_a, m_b, g_a, g_b)
  expect_equal(ord, c(1L, 2L, 3L, 4L))
})
