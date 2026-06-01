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

# --- per-kick scoring (score_shootout_kicks) ---------------------------------

test_that("score_shootout_kicks adds wp + wpa and is consistent with win prob", {
  # A scores, B misses, A scores, B misses -> A leads 2-0 after 2 rounds.
  kicks <- data.frame(
    team_id = c("A","B","A","B"),
    scored  = c(1L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
  out <- score_shootout_kicks(kicks)
  expect_true(all(c("wp_first_kicker", "shootout_wpa") %in% names(out)))
  expect_equal(nrow(out), 4L)
  # WP after each kick must equal a direct win-prob call at that state.
  expect_equal(out$wp_first_kicker[1],
               shootout_win_prob(score_a=1,score_b=0,kicks_a=1,kicks_b=0), tolerance=1e-12)
  expect_equal(out$wp_first_kicker[3],
               shootout_win_prob(score_a=2,score_b=0,kicks_a=2,kicks_b=1), tolerance=1e-12)
  # All WP in [0,1].
  expect_true(all(out$wp_first_kicker >= 0 & out$wp_first_kicker <= 1))
})

test_that("scoring a kick helps the kicker, missing hurts (sign convention)", {
  # A's first kick: GOAL should be positive WPA for A; MISS should be negative.
  goal <- score_shootout_kicks(data.frame(team_id="A", scored=1L))
  miss <- score_shootout_kicks(data.frame(team_id="A", scored=0L))
  expect_gt(goal$shootout_wpa[1], 0)
  expect_lt(miss$shootout_wpa[1], 0)
  # A missing its kick is B's gain -> if next row is B, B's WPA sign mirrors.
  seq <- score_shootout_kicks(data.frame(team_id=c("A","B"), scored=c(0L,1L)))
  expect_lt(seq$shootout_wpa[1], 0)   # A missed -> bad for A
  expect_gt(seq$shootout_wpa[2], 0)   # B scored -> good for B
})

test_that("score_shootout_kicks handles empty input", {
  out <- score_shootout_kicks(data.frame(team_id=character(0), scored=integer(0)))
  expect_equal(nrow(out), 0L)
})

test_that("score_shootout_kicks errors without required columns", {
  expect_error(score_shootout_kicks(data.frame(foo=1)), "team_id")
})

test_that("keeper-saved miss splits WPA between taker and keeper", {
  # A's first kick is SAVED (type_id 15). Default keeper_save_share = 0.5.
  saved <- score_shootout_kicks(
    data.frame(team_id = "A", scored = 0L, type_id = 15L))
  # taker keeps half the (negative) WPA; keeper gets the positive other half.
  expect_lt(saved$shootout_wpa[1], 0)
  expect_gt(saved$keeper_wpa[1], 0)
  expect_equal(saved$shootout_wpa[1], -saved$keeper_wpa[1], tolerance = 1e-12)

  # An OFF-TARGET miss (skied = 13) is all on the taker, no keeper credit.
  skied <- score_shootout_kicks(
    data.frame(team_id = "A", scored = 0L, type_id = 13L))
  expect_lt(skied$shootout_wpa[1], 0)
  expect_equal(skied$keeper_wpa[1], 0, tolerance = 1e-12)

  # keeper_save_share = 0 reproduces all-taker blame (old behaviour).
  none <- score_shootout_kicks(
    data.frame(team_id = "A", scored = 0L, type_id = 15L), keeper_save_share = 0)
  expect_equal(none$keeper_wpa[1], 0, tolerance = 1e-12)

  # total WPA is conserved: taker + keeper share == the full kick swing.
  full <- score_shootout_kicks(
    data.frame(team_id = "A", scored = 0L, type_id = 13L))$shootout_wpa[1]
  expect_equal(saved$shootout_wpa[1] - saved$keeper_wpa[1], full, tolerance = 1e-12)
})

test_that("no type_id column -> all misses all-taker, keeper_wpa all zero", {
  out <- score_shootout_kicks(data.frame(team_id = c("A","B"), scored = c(0L,0L)))
  expect_true("keeper_wpa" %in% names(out))
  expect_equal(out$keeper_wpa, c(0, 0), tolerance = 1e-12)
})

# --- per-player aggregation (aggregate_shootout_wpa) -------------------------

test_that("aggregate_shootout_wpa resolves keeper saves to the GK via lineups", {
  # One match: team A takes 2 kicks (1 scored, 1 SAVED by B's keeper);
  # team B takes 1 kick (scored). 5-kick regulation but tiny for the test.
  kicks <- data.frame(
    match_id  = rep("m1", 3),
    team_id   = c("A", "B", "A"),
    player_id = c("takerA1", "takerB1", "takerA2"),
    player_name = c("Taker A1", "Taker B1", "Taker A2"),
    scored    = c(1L, 1L, 0L),
    type_id   = c(16L, 16L, 15L),   # A2's kick is SAVED
    minute    = c(120L, 120L, 121L), second = c(0L, 10L, 0L),
    stringsAsFactors = FALSE
  )
  lineups <- data.frame(
    match_id = rep("m1", 4),
    team_id  = c("A","A","B","B"),
    player_id = c("takerA1","gkA","takerB1","gkB"),
    player_name = c("Taker A1","Keeper A","Taker B1","Keeper B"),
    position = c("Striker","Goalkeeper","Striker","Goalkeeper"),
    minutes_played = c(120, 120, 120, 120),
    stringsAsFactors = FALSE
  )
  agg <- aggregate_shootout_wpa(kicks, lineups)
  # B's keeper (gkB) saved A2's kick -> should hold positive keeper_wpa.
  gkb <- agg[player_id == "gkB"]
  expect_equal(nrow(gkb), 1L)
  expect_gt(gkb$keeper_wpa, 0)
  expect_equal(gkb$kicks_taken, 0L)
  # The saved taker (takerA2) carries negative taker WPA, no keeper credit.
  a2 <- agg[player_id == "takerA2"]
  expect_lt(a2$taker_wpa, 0)
  expect_equal(a2$keeper_wpa, 0, tolerance = 1e-12)
})

test_that("aggregate_shootout_wpa drops blank player_id kicks", {
  kicks <- data.frame(
    match_id = c("m1","m1"), team_id = c("A","B"),
    player_id = c("", "realB"), player_name = c("", "Real B"),
    scored = c(1L, 1L), type_id = c(16L, 16L),
    minute = c(120L,120L), second = c(0L,5L), stringsAsFactors = FALSE)
  expect_warning(agg <- aggregate_shootout_wpa(kicks), "missing player_id")
  expect_false("" %in% agg$player_id)
})

test_that("aggregate_shootout_wpa without lineups warns + omits keeper attribution", {
  kicks <- data.frame(
    match_id = "m1", team_id = c("A","B","A"),
    player_id = c("a1","b1","a2"), player_name = c("A1","B1","A2"),
    scored = c(1L,1L,0L), type_id = c(16L,16L,15L),
    minute = c(120L,120L,121L), second = c(0L,5L,0L), stringsAsFactors = FALSE)
  expect_warning(agg <- aggregate_shootout_wpa(kicks, lineups = NULL),
                 "could not be|No lineups")
  expect_equal(sum(agg$keeper_wpa), 0, tolerance = 1e-12)  # unattributed, omitted
})

test_that("aggregate_shootout_wpa handles empty input", {
  agg <- aggregate_shootout_wpa(
    data.frame(match_id=character(0), team_id=character(0),
               player_id=character(0), scored=integer(0)))
  expect_equal(nrow(agg), 0L)
})
