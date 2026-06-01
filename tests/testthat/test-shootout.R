library(testthat)

# Exact penalty-shootout win probability (R/shootout.R). The win-prob closed
# form was cross-validated to machine precision against an independent DP
# recursion; these tests lock the key invariants + the sudden-death boundary
# cases that an earlier prototype got wrong.

test_that("equal skill gives exactly 0.5 (no structural first-kicker edge)", {
  expect_equal(shootout_win_prob(0.75, 0.75), 0.5, tolerance = 1e-12)
  for (p in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
    expect_equal(shootout_win_prob(p, p), 0.5, tolerance = 1e-12)
  }
})

test_that("default uses the empirical conversion rate and returns 0.5", {
  expect_equal(shootout_win_prob(), 0.5, tolerance = 1e-12)
  expect_identical(PENALTY_SHOOTOUT_CONVERSION, 0.75)
})

test_that("better team is favoured, monotonically in its edge", {
  w0 <- shootout_win_prob(0.75, 0.75)
  w1 <- shootout_win_prob(0.80, 0.70)
  w2 <- shootout_win_prob(0.85, 0.65)
  expect_gt(w1, w0)
  expect_gt(w2, w1)
  # symmetry: swapping the rates reflects around 0.5
  expect_equal(shootout_win_prob(0.80, 0.70),
               1 - shootout_win_prob(0.70, 0.80), tolerance = 1e-12)
})

test_that("degenerate conversion rates behave correctly", {
  # nobody ever scores -> always level -> coin-flip sudden death
  expect_equal(shootout_win_prob(0, 0), 0.5, tolerance = 1e-12)
  # everybody always scores -> always level -> coin-flip sudden death
  expect_equal(shootout_win_prob(1, 1), 0.5, tolerance = 1e-12)
  # A always scores, B never -> A certain to win
  expect_equal(shootout_win_prob(1, 0), 1, tolerance = 1e-12)
  expect_equal(shootout_win_prob(0, 1), 0, tolerance = 1e-12)
})

test_that("win prob is bounded and well-formed from arbitrary live states", {
  for (sa in 0:3) for (sb in 0:3) {
    w <- shootout_win_prob(score_a = sa, score_b = sb,
                           kicks_a = 3L, kicks_b = 3L)
    expect_gte(w, 0); expect_lte(w, 1)
  }
})

test_that("a locked regulation lead returns certainty", {
  # A 3-0 up after 3 kicks each, only 2 kicks left for B: B max final = 2 < 3.
  expect_equal(
    shootout_win_prob(score_a = 3L, score_b = 0L, kicks_a = 3L, kicks_b = 3L),
    1, tolerance = 1e-12)
  # mirror
  expect_equal(
    shootout_win_prob(score_a = 0L, score_b = 3L, kicks_a = 3L, kicks_b = 3L),
    0, tolerance = 1e-12)
})

test_that("sudden-death DECIDING kick is exact (the prototype bug)", {
  # Both took 5 in regulation and were level; sudden death round 6: A scored
  # (6th), B about to take its 6th. State after A's make: kicks_a=6, score_a=+1
  # over B, B has its kick to come (kicks_b=5 -> about to take 6th). Model this
  # as the round boundary where A leads by 1 with B owing one kick.
  # A leads 1, B to take its equalising kick: A wins unless B scores.
  w <- shootout_win_prob(score_a = 1L, score_b = 0L,
                         kicks_a = 6L, kicks_b = 5L, n_regulation = 5L)
  # B scores (p=0.75) -> back to level -> sudden death 0.5; B misses (0.25) -> A wins.
  expect_equal(w, 0.75 * 0.5 + 0.25 * 1, tolerance = 1e-12)

  # If B has ALSO taken its kick and MISSED (kicks_a=6,kicks_b=6, A leads 1-0):
  # round complete, A leads -> A wins for certain.
  expect_equal(
    shootout_win_prob(score_a = 1L, score_b = 0L, kicks_a = 6L, kicks_b = 6L),
    1, tolerance = 1e-12)
  # level after a completed SD round -> coin flip
  expect_equal(
    shootout_win_prob(score_a = 1L, score_b = 1L, kicks_a = 6L, kicks_b = 6L),
    0.5, tolerance = 1e-12)
})

test_that("malformed sudden-death state errors loudly", {
  expect_error(
    shootout_win_prob(kicks_a = 8L, kicks_b = 5L, n_regulation = 5L),
    "Malformed sudden-death")
})
