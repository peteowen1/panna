# Tests for the EPV net goals ledger.
#
# Every expected number here is computed BY HAND in the fixture comments, never
# derived from the function under test. Torp's net points suite passed 48
# assertions against a ledger with the away-team sign flipped, because the one
# test of the raw allocation compared it to a ledger built by the same mutated
# function. See torpverse/docs/plans/EPV-NET-POINTS.md section 6.

# ---------------------------------------------------------------------------
# Fixture: one match, two teams, seven raw events.
#
# HOME = "H", AWAY = "A". Final score 1-0, so the goal difference is +1.
#
#  ev  type  team  player  note
#   1    1     H     h1    pass
#   2    1     H     h2    pass
#   3   43     H     h2    Deleted Event  -> marker, invisible to adjacency
#   4    5     A     a1    Ball Out       -> dropped by SPADL, NAMES a1
#   5    1     H     h3    pass (throw-in taker)
#   6   16     H     h3    goal
#   7   27     H     h1    Start          -> marker
#
# SPADL keeps events 1, 2, 5, 6 (types 1 and 16 are gameplay).
# SPADL's own neighbour of event 2 is event 5 (team H) -- it steps over the
# Ball Out entirely. The true next visible event after 2 is event 4 (team A),
# which is a possession change and names a1.
# ---------------------------------------------------------------------------

ng_fixture_events <- function() {
  data.frame(
    match_id  = rep("m1", 7),
    event_id  = 1:7,
    type_id   = c(1L, 1L, 43L, 5L, 1L, 16L, 27L),
    team_id   = c("H", "H", "H", "A", "H", "H", "H"),
    player_id = c("h1", "h2", "h2", "a1", "h3", "h3", "h1"),
    period_id = rep(1L, 7),
    minute    = c(0L, 0L, 0L, 0L, 1L, 1L, 1L),
    second    = c(10L, 20L, 21L, 22L, 5L, 30L, 31L),
    stringsAsFactors = FALSE
  )
}

# Four SPADL actions with hand-chosen epv_delta.
#
#   action 1  h1  H  +0.10
#   action 2  h2  H  -0.05
#   action 3  h3  H  +0.20
#   action 4  h3  H  +0.75   (the goal)
#
# All four belong to the HOME team, so in the home-margin frame every value is
# unchanged: the ledger total is 0.10 - 0.05 + 0.20 + 0.75 = +1.00, which is the
# goal difference exactly.
ng_fixture_spadl <- function() {
  data.frame(
    match_id          = rep("m1", 4),
    action_id         = 1:4,
    original_event_id = c(1L, 2L, 5L, 6L),
    team_id           = rep("H", 4),
    player_id         = c("h1", "h2", "h3", "h3"),
    action_type       = c("pass", "pass", "pass", "shot"),
    result            = c("success", "success", "success", "success"),
    epv_delta         = c(0.10, -0.05, 0.20, 0.75),
    stringsAsFactors  = FALSE
  )
}

ng_fixture_fixtures <- function() {
  data.frame(
    match_id     = "m1",
    home_team_id = "H",
    away_team_id = "A",
    home_score   = 1,
    away_score   = 0,
    stringsAsFactors = FALSE
  )
}

# The same match with the away team acting, to exercise the sign flip. Two away
# actions worth +0.30 and +0.40 in the AWAY team's own frame become -0.30 and
# -0.40 in the home-margin frame.
ng_fixture_spadl_away <- function() {
  d <- ng_fixture_spadl()
  d$team_id <- c("H", "A", "A", "H")
  d$player_id <- c("h1", "a1", "a2", "h3")
  d$epv_delta <- c(0.10, 0.30, 0.40, 0.75)
  d
}


# =============================================================================
test_that("adjacency skips markers but keeps named-player drops visible", {
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)

  expect_equal(nrow(adj), 7L)

  # Event 3 (Deleted) and 7 (Start) are markers; event 4 (Ball Out) is not.
  expect_true(adj[event_id == 3L]$is_marker)
  expect_true(adj[event_id == 7L]$is_marker)
  expect_false(adj[event_id == 4L]$is_marker)
  expect_true(adj[event_id == 4L]$is_attrib)

  # The whole point: event 2's true successor is the Ball Out (event 4, team A),
  # NOT the throw-in taker SPADL would step to (event 5, team H).
  expect_equal(adj[event_id == 2L]$next_type_id, 5L)
  expect_equal(adj[event_id == 2L]$next_team_id, "A")
  expect_equal(adj[event_id == 2L]$next_player_id, "a1")
  expect_true(adj[event_id == 2L]$true_possession_change)

  # And it is reported as a gap, so the ledger can pay a1 rather than lose him.
  expect_equal(adj[event_id == 2L]$gap_type_id, 5L)
  expect_equal(adj[event_id == 2L]$gap_player_id, "a1")

  # A row whose successor is an ordinary action has no gap.
  expect_true(is.na(adj[event_id == 1L]$gap_type_id))
})

test_that("adjacency never looks across a period boundary", {
  ev <- ng_fixture_events()
  ev$period_id <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L)
  adj <- ng_build_adjacency(ev, verbose = FALSE)

  # Event 4 is the last visible event of period 1, so it has no successor.
  expect_true(is.na(adj[event_id == 4L]$next_team_id))
  expect_false(adj[event_id == 4L]$true_possession_change)
})

test_that("adjacency aborts rather than guessing when a column is missing", {
  ev <- ng_fixture_events()
  ev$second <- NULL
  expect_error(ng_build_adjacency(ev, verbose = FALSE), "second")
})


# =============================================================================
test_that("the ledger total equals the goal difference, computed by hand", {
  pay <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = FALSE,
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  expect_equal(nrow(pay), 4L)
  # Hard-coded, NOT recomputed from the fixture: 0.10 - 0.05 + 0.20 + 0.75.
  expect_equal(sum(pay$value_home), 1.00, tolerance = 1e-12)

  cons <- ng_check_conservation(pay, ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(nrow(cons), 1L)
  expect_equal(cons$gd, 1)
  expect_equal(cons$err, 0, tolerance = 1e-12)
})

test_that("away actions are negated into the home frame", {
  pay <- ng_build_ledger(ng_fixture_spadl_away(), adj = NULL, allocate = FALSE,
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  # Home frame: +0.10 - 0.30 - 0.40 + 0.75 = +0.15, by hand.
  expect_equal(sum(pay$value_home), 0.15, tolerance = 1e-12)

  # a1 earned +0.30 for his own team; his own-frame number must stay positive
  # even though he cost the home side 0.30.
  a1 <- pay[player_id == "a1"]
  expect_equal(a1$value_home, -0.30, tolerance = 1e-12)
  expect_equal(a1$value_own, 0.30, tolerance = 1e-12)
  expect_false(a1$is_home)
})

test_that("flipping the away sign is CAUGHT, not absorbed", {
  # This is the mutation torp's suite survived. With the away side inverted the
  # home-frame total moves from +0.15 to +1.55, and the hard-coded expectation
  # above is the only thing that notices.
  d <- ng_fixture_spadl_away()
  d$epv_delta[d$team_id == "A"] <- -d$epv_delta[d$team_id == "A"]
  pay <- ng_build_ledger(d, adj = NULL, allocate = FALSE,
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(sum(pay$value_home), 1.55, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(sum(pay$value_home), 0.15)))
})

test_that("actions with no fixture row are dropped loudly, not silently", {
  d <- ng_fixture_spadl()
  d$match_id[4] <- "m_unknown"
  expect_warning(
    pay <- ng_build_ledger(d, adj = NULL, allocate = FALSE,
                           fixtures = ng_fixture_fixtures(), verbose = FALSE),
    "no fixture row"
  )
  expect_equal(nrow(pay), 3L)
})

test_that("supplying adj requires the link back to the raw feed", {
  d <- ng_fixture_spadl()
  d$original_event_id <- NULL
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)
  expect_error(
    ng_build_ledger(d, adj = adj, allocate = FALSE,
                    fixtures = ng_fixture_fixtures(), verbose = FALSE),
    "original_event_id"
  )
})

test_that("adjacency changes who is next without changing the match total", {
  # The identity is about how much, the adjacency is about who. Step 3's rules
  # only ever subdivide a row, so this invariant must hold forever.
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)
  with_adj <- ng_build_ledger(ng_fixture_spadl(), adj = adj, allocate = FALSE,
                              fixtures = ng_fixture_fixtures(), verbose = FALSE)
  without  <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = FALSE,
                              fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(sum(with_adj$value_home), sum(without$value_home),
               tolerance = 1e-12)
  expect_equal(sum(with_adj$value_home), 1.00, tolerance = 1e-12)
})


# =============================================================================
# Allocation (step 3). Every expectation below is arithmetic done by hand in the
# comment above it, on the fixture's four hand-chosen deltas.
# =============================================================================

test_that("allocation splits a row and never changes its total", {
  pay <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = TRUE,
                         convention = "margin",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  # Still +1.00: 0.10 - 0.05 + 0.20 + 0.75, unchanged by any rule.
  expect_equal(sum(pay$value_home), 1.00, tolerance = 1e-12)

  # The goal is action 4, worth 0.75. off_pool = 0.10, so the shooter takes
  # 0.9 x 0.75 = 0.675 and the attacking pool 0.075.
  sh <- pay[role == "shooter"]
  expect_equal(nrow(sh), 1L)
  expect_equal(sh$value_home, 0.675, tolerance = 1e-12)

  # Every action is retained here, so the pool takes exactly 10% of all of it:
  # 0.10 x 1.00 = 0.100.
  expect_equal(sum(pay[role == "pool_off"]$value_home), 0.100, tolerance = 1e-12)
})

test_that("a successful pass is never a turnover, even when possession changes", {
  # Regression test. Man City's third goal against Brentford: Ederson's
  # 0.13-xPass ball released Haaland to score, the next visible event was a
  # Brentford challenge, and the centre-half was charged -0.178 as its "ball
  # winner". Possession alone is not a turnover -- the action must have failed.
  d <- ng_fixture_spadl()
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)
  pay <- ng_build_ledger(d, adj = adj, allocate = TRUE, convention = "margin",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  # Event 2's true successor is a1's Ball Out, so adjacency DOES flag a
  # possession change on action 2 -- but the pass succeeded, so nobody on the
  # away side is paid and no away player appears at all.
  expect_true(adj[event_id == 2L]$true_possession_change)
  expect_equal(nrow(pay[role == "ball_winner"]), 0L)
  expect_false(any(pay$team_id == "A", na.rm = TRUE))
})

test_that("a failed pass pays the player the dropped row names", {
  # Action 2 fails. Its value is -0.05 in the home frame. exec_blame = 0.30 so
  # h2 keeps -0.015; the remaining -0.035 is the defence's, of which
  # named_share = 0.70 goes to a1 (-0.0245) and 0.30 to team A's pool (-0.0105).
  # a1 is named ONLY by the Ball Out row that SPADL drops, so this also proves
  # the adjacency table is doing its job.
  d <- ng_fixture_spadl()
  d$result[2] <- "fail"
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)
  pay <- ng_build_ledger(d, adj = adj, allocate = TRUE, convention = "margin",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  expect_equal(sum(pay$value_home), 1.00, tolerance = 1e-12)

  actor2 <- pay[action_id == 2L & role == "actor"]
  expect_equal(actor2$value_home, -0.015, tolerance = 1e-12)

  bw <- pay[role == "ball_winner"]
  expect_equal(nrow(bw), 1L)
  expect_equal(bw$player_id, "a1")
  expect_equal(bw$team_id, "A")
  expect_equal(bw$value_home, -0.0245, tolerance = 1e-12)
  # In his own frame a1 winning the ball is a GOOD thing, so positive.
  expect_equal(bw$value_own, 0.0245, tolerance = 1e-12)

  pd <- pay[role == "pool_def"]
  expect_equal(sum(pd$value_home), -0.0105, tolerance = 1e-12)
})

test_that("a stop with no shot in front of it charges nobody", {
  # The orphan case, 0.9% of stops. Crediting the stop needs the shot; charging
  # the rebound does not. Applying only the second is how Ederson took -0.194
  # for a save with nothing on the other side.
  d <- ng_fixture_spadl()
  d$action_type[3] <- "keeper_save"     # action 3, worth +0.20, no shot before
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "margin",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  expect_equal(sum(pay$value_home), 1.00, tolerance = 1e-12)
  expect_equal(nrow(pay[role == "stopper_rebound"]), 0L)
  # The whole 0.20 went to that team's pool instead.
  expect_equal(sum(pay[action_id == 3L]$value_home), 0.20, tolerance = 1e-12)
  expect_true(all(pay[action_id == 3L]$role == "pool_def"))
})

test_that("allocation aborts when result is missing rather than assuming success", {
  d <- ng_fixture_spadl()
  d$result <- NULL
  expect_error(
    ng_build_ledger(d, adj = NULL, allocate = TRUE,
                    fixtures = ng_fixture_fixtures(), verbose = FALSE),
    "result"
  )
})

test_that("shares are validated", {
  expect_error(ng_shares(exec_blame = 1.5), "exec_blame")
  expect_error(ng_shares(named_share = NA), "named_share")
  expect_equal(ng_shares()$exec_blame, 0.30)
})


# =============================================================================
# The team convention (double entry). Each team's players sum to that team's
# OWN goal difference, and the match sums to zero.
# =============================================================================

test_that("each team sums to its own goal difference, by hand", {
  # All four fixture actions belong to H and are worth +1.00 between them, and
  # the match finished 1-0 to H. So:
  #   H's players =  1.00  (its own goal difference, +1)
  #   A's players = -1.00  (its own goal difference, -1)
  #   the match   =  0.00
  pay <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = TRUE,
                         convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  expect_equal(sum(pay[team_id == "H"]$value_own), 1.00, tolerance = 1e-12)
  expect_equal(sum(pay[team_id == "A"]$value_own), -1.00, tolerance = 1e-12)
  expect_equal(sum(pay$value_own), 0, tolerance = 1e-12)

  # Which is NOT the margin convention's identity: there the match sums to the
  # goal difference and the team totals float.
  marg <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = TRUE,
                          convention = "margin",
                          fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(sum(marg$value_home), 1.00, tolerance = 1e-12)
})

test_that("every action is booked once to each side", {
  pay <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = TRUE,
                         convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  per_action <- pay[, .(net = sum(value_own)), by = action_id]
  expect_true(all(abs(per_action$net) < 1e-12))

  # The offensive half of action 4 is the goal's whole 0.75: shooter takes
  # 0.9 x 0.75 = 0.675 and the attacking pool 0.075.
  off4 <- pay[action_id == 4L & entry == "offence"]
  expect_equal(sum(off4$value_own), 0.75, tolerance = 1e-12)
  expect_equal(off4[role == "shooter"]$value_own, 0.675, tolerance = 1e-12)
  # And the defending side is charged the whole -0.75.
  expect_equal(sum(pay[action_id == 4L & entry == "defence"]$value_own),
               -0.75, tolerance = 1e-12)
})

test_that("a half that is the wrong size is caught, not just an uncancelled one", {
  # Two equal and opposite halves of the WRONG size cancel perfectly, so the
  # cancellation check alone would pass. This is the 1,106-goal bug found on
  # the first run: the goal branch booked the shooter's 90% and forgot the
  # attacking pool's 10%, and only the size check saw it.
  d <- data.table::as.data.table(ng_fixture_spadl())
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  off <- pay[entry == "offence", .(o = sum(value_own)), by = action_id]
  expect_equal(off[order(action_id)]$o, c(0.10, -0.05, 0.20, 0.75),
               tolerance = 1e-12)

  broken <- data.table::copy(pay)
  broken <- broken[!(action_id == 4L & role == "pool_off")]
  expect_error(
    panna:::.ng_assert_double_entry(
      merge(d, ng_fixture_fixtures()[, c("match_id")], by = "match_id"), broken),
    "does not cancel"
  )
})

test_that("ng_check_team_totals reports both sides of every match", {
  pay <- ng_build_ledger(ng_fixture_spadl(), adj = NULL, allocate = TRUE,
                         convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  tt <- ng_check_team_totals(pay, ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(nrow(tt), 2L)
  expect_equal(sort(tt$own_gd), c(-1, 1))
  expect_true(all(abs(tt$err) < 1e-12))
})


test_that("dacts_share routes only the defensive pool's credit half", {
  # A flat spread and a defensive-acts spread must both preserve the team
  # totals -- routing changes who is paid, never how much. That the identity
  # survives is the point; conservation alone cannot tell the two apart, which
  # is why the position tables exist.
  d <- ng_fixture_spadl()
  fxt <- ng_fixture_fixtures()
  # Eleven a side, because ng_spread_pools() asserts a squad is 10-11 players.
  # That guard exists because a pool divided among the wrong, smaller group
  # still conserves perfectly -- so the fixture is made realistic rather than
  # the guard made lenient.
  lineup <- data.frame(
    match_id = rep("m1", 22),
    player_id = c(paste0("h", 1:11), paste0("a", 1:11)),
    team_id = rep(c("H", "A"), each = 11),
    is_starter = rep(TRUE, 22),
    minutes_played = rep(90, 22),
    sub_on_minute = rep(0, 22),
    sub_off_minute = rep(0, 22),
    stringsAsFactors = FALSE
  )
  # Every fixture action belongs to H, so it is A that holds the defensive
  # entries -- and only A's defensive acts can tilt A's defensive pool. Give
  # a1 two and a2 one so the weighting has something to separate.
  acts <- transform(d, time_seconds = c(10, 20, 65, 90), period_id = 1L,
                    action_type = c("pass", "pass", "pass", "shot"))
  acts <- rbind(acts, data.frame(
    match_id = rep("m1", 3), action_id = 101:103, period_id = 1L,
    original_event_id = 101:103, team_id = rep("A", 3),
    player_id = c("a1", "a1", "a2"),
    action_type = rep("tackle", 3), result = rep("success", 3),
    # Non-zero, because the default measure weights an act by its own value:
    # a1 is worth 0.05 between his two, a2 0.01.
    epv_delta = c(0.02, 0.03, 0.01), time_seconds = c(15, 30, 70)))

  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = fxt, verbose = FALSE)
  flat  <- ng_spread_pools(pay, acts, lineup, dacts_share = 0, verbose = FALSE)
  tilt  <- ng_spread_pools(pay, acts, lineup, dacts_share = 1, verbose = FALSE)

  for (p in list(flat, tilt)) {
    expect_equal(sum(p[team_id == "H"]$value_own), 1.00, tolerance = 1e-10)
    expect_equal(sum(p[team_id == "A"]$value_own), -1.00, tolerance = 1e-10)
  }
  # ...and the two spreads are genuinely different allocations.
  # a1 made two of A's three defensive acts, so a defensive-acts spread must
  # pay him more of A's defensive credit than an equal eleventh.
  fa <- flat[player_id == "a1", sum(value_own)]
  ta <- tilt[player_id == "a1", sum(value_own)]
  expect_false(isTRUE(all.equal(fa, ta)))
  expect_gt(ta, fa)
  # And a player with no defensive act must lose share, not gain it.
  expect_lt(tilt[player_id == "a11", sum(value_own)],
            flat[player_id == "a11", sum(value_own)])
})

test_that("dacts_share is validated", {
  d <- ng_fixture_spadl()
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_error(ng_spread_pools(pay, d, data.frame(), dacts_share = 2),
               "dacts_share")
  expect_error(ng_spread_pools(pay, d, data.frame(), dacts_share = NA),
               "dacts_share")
})


test_that("under the team convention a successful pass pays no named defender", {
  # The team-convention twin of the margin-convention regression above. Here the
  # away side ALWAYS appears, because every action is booked to both sides -- so
  # the assertion is about the ROLE, not about presence: a completed pass leaves
  # nobody named on the defending side, only its pool.
  d <- ng_fixture_spadl()
  adj <- ng_build_adjacency(ng_fixture_events(), verbose = FALSE)
  pay <- ng_build_ledger(d, adj = adj, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  expect_true(adj[event_id == 2L]$true_possession_change)
  expect_equal(nrow(pay[role == "defender"]), 0L)
  expect_true(all(pay[team_id == "A"]$role == "pool_def"))
  expect_equal(sum(pay[team_id == "A"]$value_own), -1.00, tolerance = 1e-12)
})

test_that("the shipped defaults are the ones Pete chose", {
  # convention = "team" and dacts_share = 0.5, decided 2026-09-21. A default
  # that drifts silently is how a published number changes without a decision.
  expect_equal(formals(ng_build_ledger)$convention[[2]], "team")
  expect_equal(formals(ng_spread_pools)$dacts_share, 0.5)
  expect_equal(formals(ng_spread_pools)$dacts_measure[[2]], "act_value")
})


test_that("a first-half stoppage minute is not bucketed with the second half", {
  # `time_seconds` is cumulative, so minute bins 45-56 exist in BOTH halves:
  # first-half stoppage overlaps the early second half. On ENG 2024-2025 that is
  # 15.66% of all actions, and with half-time substitutions the two periods have
  # different elevens -- so a bucket keyed on the minute alone pays a first-half
  # stoppage pool to players who only came on at the interval.
  d <- ng_fixture_spadl()
  fxt <- ng_fixture_fixtures()
  # One action at minute 47 of the FIRST half.
  acts <- transform(d, time_seconds = c(10, 20, 65, 47 * 60),
                    period_id = c(1L, 1L, 1L, 1L))

  # h11 starts; h1 comes on at half time and h11 goes off.
  lineup <- data.frame(
    match_id = rep("m1", 23),
    player_id = c(paste0("h", 1:11), paste0("a", 1:11), "h12"),
    team_id = c(rep("H", 11), rep("A", 11), "H"),
    is_starter = c(FALSE, rep(TRUE, 10), rep(TRUE, 11), TRUE),
    minutes_played = c(45, rep(90, 10), rep(90, 11), 45),
    sub_on_minute = c(45, rep(0, 10), rep(0, 11), 0),
    sub_off_minute = c(0, rep(0, 10), rep(0, 11), 45),
    stringsAsFactors = FALSE
  )

  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = fxt, verbose = FALSE)
  out <- ng_spread_pools(pay, acts, lineup, dacts_share = 0, verbose = FALSE)

  # The minute-47 action happened in the FIRST half, so h12 (off at 45) must be
  # paid for it and h1 (on at 45) must not. Keyed on the minute alone, it would
  # be the other way round.
  a4 <- unique(out[play_type == "pool" & team_id == "H"]$player_id)
  expect_true("h12" %in% a4)

  # And the identity is unaffected either way.
  expect_equal(sum(out[team_id == "H"]$value_own), 1.00, tolerance = 1e-10)
  expect_equal(sum(out[team_id == "A"]$value_own), -1.00, tolerance = 1e-10)
})

test_that("ng_spread_pools refuses actions with no period_id", {
  d <- ng_fixture_spadl()
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_error(ng_spread_pools(pay, d, data.frame(), verbose = FALSE),
               "period_id")
})

test_that("a keeper_save follows the rebound rule under BOTH conventions", {
  # Regression: `reb_named` was implemented in the margin convention only, so
  # under the team convention -- the default -- a stop fell through to the
  # generic branch and the keeper kept 90% of the rebound as an ordinary on-ball
  # action. Neither row-level assertion could see it: paying the wrong recipient
  # the right amount conserves perfectly.
  d <- ng_fixture_spadl()
  d$action_type[3] <- "keeper_save"          # action 3, +0.20
  d$action_type[2] <- "shot"                 # so action 3 HAS a shot before it
  d$result[2] <- "fail"
  d$team_id[2] <- "A"                        # the shot is the opponent's
  d$player_id[2] <- "a1"

  for (conv in c("team", "margin")) {
    pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = conv,
                           fixtures = ng_fixture_fixtures(), verbose = FALSE)
    reb <- pay[action_id == 3L & role == "stopper_rebound"]
    expect_equal(nrow(reb), 1L, info = conv)
    # reb_named = 0.40 of the row's +0.20, in the keeper's own frame.
    expect_equal(reb$value_own, 0.08, tolerance = 1e-12, info = conv)
    expect_false(any(pay[action_id == 3L]$role == "actor"), info = conv)
  }
})
