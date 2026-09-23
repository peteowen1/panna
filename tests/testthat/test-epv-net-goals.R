# Tests for the EPV net goals ledger.
#
# Every expected number here is computed BY HAND in the fixture comments, never
# derived from the function under test. Torp's net points suite passed 48
# assertions against a ledger with the away-team sign flipped, because the one
# test of the raw allocation compared it to a ledger built by the same mutated
# function. See torpverse/docs/plans/EPV-NET-POINTS.md section 6.

# Most fixtures below hold a handful of shots, too few to fit the shot
# aftermath line (it needs 50 non-goal shots), so they test the rules with shots
# priced at their xG. The aftermath has its own tests at the end of the file,
# run with a given line.
ng_build_ledger <- function(..., shot_aftermath = FALSE) {
  panna::ng_build_ledger(..., shot_aftermath = shot_aftermath)
}

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
  expect_equal(formals(panna::ng_build_ledger)$convention[[2]], "team")
  # shot_aftermath = TRUE, decided with Pete 2026-09-23
  expect_true(formals(panna::ng_build_ledger)$shot_aftermath)
  # one share for every step of a shot, gain or loss (Pete, 2026-09-23)
  expect_equal(ng_shares()$shot_keep, 0.90)
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


# =============================================================================
# Per-game aggregation, and the receiver-must-be-a-teammate rule.
# =============================================================================

ng_fixture_lineup <- function() {
  data.frame(
    match_id = rep("m1", 22),
    player_id = c(paste0("h", 1:11), paste0("a", 1:11)),
    team_id = rep(c("H", "A"), each = 11),
    player_name = c(paste0("Home ", 1:11), paste0("Away ", 1:11)),
    match_date = rep("2026-01-01", 22),
    is_starter = rep(TRUE, 22),
    minutes_played = rep(90, 22),
    sub_on_minute = rep(0, 22),
    sub_off_minute = rep(0, 22),
    competition = rep("ENG", 22),
    season = rep("2025-2026", 22),
    stringsAsFactors = FALSE
  )
}

test_that("a successful pass to an OPPONENT pays no receiver share", {
  # Regression. SPADL names a receiver on 26.4% of actions who is on the other
  # side -- 11,905 of them on passes it calls successful, carrying 201.7 goals
  # of absolute value. Paying those the teammate split credited an opponent and
  # booked it under HIS team, i.e. on the wrong side of the double entry, which
  # left 55% of player-matches holding payments under two team ids.
  d <- ng_fixture_spadl()
  d$receiver_player_id <- c("h2", "a1", NA, NA)     # action 2 "reaches" an opponent
  d$receiver_team_id   <- c("H", "A", NA, NA)
  d$xpass              <- c(0.9, 0.9, NA, NA)

  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)

  # Action 1's receiver is a teammate and is paid; action 2's is not.
  expect_equal(nrow(pay[action_id == 1L & role == "receiver"]), 1L)
  expect_equal(pay[action_id == 1L & role == "receiver"]$player_id, "h2")
  expect_equal(nrow(pay[action_id == 2L & role == "receiver"]), 0L)
  # a1 must not appear on the HOME side of the ledger at all.
  expect_equal(nrow(pay[player_id == "a1" & team_id == "H"]), 0L)
  # The identity is untouched either way.
  expect_equal(sum(pay[team_id == "H"]$value_own), 1.00, tolerance = 1e-12)
})

test_that("ng_player_game refuses unspread pools rather than dropping them", {
  # A pool has no player, so aggregating before the spread would shrink every
  # player-game total while the team totals stayed exactly right.
  d <- ng_fixture_spadl()
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_error(ng_player_game(pay, ng_fixture_lineup(), verbose = FALSE),
               "unspread team pools")
})

test_that("per-game halves and roles both sum to net goals", {
  d <- ng_fixture_spadl()
  acts <- transform(d, time_seconds = c(10, 20, 65, 90), period_id = 1L)
  pay <- ng_build_ledger(d, adj = NULL, allocate = TRUE, convention = "team",
                         fixtures = ng_fixture_fixtures(), verbose = FALSE)
  sp <- ng_spread_pools(pay, acts, ng_fixture_lineup(), verbose = FALSE)
  pg <- ng_player_game(sp, ng_fixture_lineup(), verbose = FALSE)

  expect_true(all(c("player_id", "player_name", "match_date", "minutes_played",
                    "epv_offensive", "epv_defensive") %in% names(pg)))
  expect_lt(max(abs(pg$epv_offensive + pg$epv_defensive - pg$net_goals)), 1e-10)
  rc <- grep("^ng_", names(pg), value = TRUE)
  expect_lt(max(abs(rowSums(pg[, ..rc]) - pg$net_goals)), 1e-10)

  # Team totals survive the aggregation: H to +1, A to -1, match to zero.
  tt <- pg[, .(v = sum(net_goals)), by = team_id]
  expect_equal(tt[team_id == "H"]$v, 1.00, tolerance = 1e-10)
  expect_equal(tt[team_id == "A"]$v, -1.00, tolerance = 1e-10)
  expect_lt(abs(sum(pg$net_goals)), 1e-10)
})


# =============================================================================
# The rating-layer adjustment (step 7): centring belongs downstream of the
# ledger, because subtracting a positional mean breaks conservation by design.
# =============================================================================

ng_fixture_pg <- function() {
  # Two positions, two players each, deliberately offset so centring has work
  # to do: strikers average +1, defenders -1, and within each pair the spread
  # is the same so a correct centring cannot reorder anyone.
  data.table::data.table(
    player_id = c("s1", "s2", "d1", "d2"),
    player_name = c("S One", "S Two", "D One", "D Two"),
    match_id = rep("m1", 4),
    season = rep("2025-2026", 4),
    minutes_played = rep(90, 4),
    net_goals = c(1.5, 0.5, -0.5, -1.5),
    epv_offensive = c(1.0, 0.4, -0.2, -0.8),
    epv_defensive = c(0.5, 0.1, -0.3, -0.7)
  )
}
ng_fixture_pos <- function() {
  data.frame(player_id = c("s1", "s2", "d1", "d2"),
             position = c("Striker", "Striker", "Defender", "Defender"),
             stringsAsFactors = FALSE)
}

test_that("centring removes the position mean and nothing else", {
  out <- ng_adjust_for_rating(ng_fixture_pg(), ng_fixture_pos(), verbose = FALSE)

  # Strikers averaged +0.7 offence, defenders -0.5; both go to zero.
  m <- out[, .(o = mean(epv_offensive), d = mean(epv_defensive)), by = position]
  expect_true(all(abs(m$o) < 1e-12))
  expect_true(all(abs(m$d) < 1e-12))

  # Hand-computed: s1's offence 1.0 against a striker mean of 0.7 leaves +0.3.
  expect_equal(out[player_id == "s1"]$epv_offensive, 0.3, tolerance = 1e-12)
  expect_equal(out[player_id == "d2"]$epv_defensive, -0.2, tolerance = 1e-12)
})

test_that("centring does not reorder players inside a position", {
  # The whole safety argument. A level shift is fine; a transform that changes
  # the ranking inside a position is a different metric, not an adjustment.
  pg <- ng_fixture_pg()
  out <- ng_adjust_for_rating(pg, ng_fixture_pos(), verbose = FALSE)
  for (p in c("Striker", "Defender")) {
    ids <- out[position == p][order(-net_goals)]$player_id
    raw <- out[position == p][order(-net_goals_raw)]$player_id
    expect_equal(ids, raw, info = p)
  }
  # And the spread within a position is untouched.
  expect_equal(sd(out[position == "Striker"]$net_goals),
               sd(out[position == "Striker"]$net_goals_raw), tolerance = 1e-12)
})

test_that("the ledger's own totals survive as _raw", {
  pg <- ng_fixture_pg()
  out <- ng_adjust_for_rating(pg, ng_fixture_pos(), verbose = FALSE)
  expect_equal(sum(out$net_goals_raw), sum(pg$net_goals), tolerance = 1e-12)
  expect_equal(out[order(player_id)]$epv_offensive_raw,
               pg[order(player_id)]$epv_offensive, tolerance = 1e-12)
  # The centred total is ~0 by construction, which is exactly why this cannot
  # live inside the conserving layer.
  expect_lt(abs(sum(out$net_goals)), 1e-12)
})

test_that("a player with no position is grouped, not dropped", {
  pos <- ng_fixture_pos()[1:3, ]          # d2 has no position
  expect_warning(
    out <- ng_adjust_for_rating(ng_fixture_pg(), pos, verbose = TRUE),
    "no position")
  expect_equal(nrow(out), 4L)
  expect_true("d2" %in% out$player_id)
})

test_that("the EPR column contract is preserved", {
  out <- ng_adjust_for_rating(ng_fixture_pg(), ng_fixture_pos(), verbose = FALSE)
  expect_true(all(c("player_id", "player_name", "minutes_played",
                    "epv_offensive", "epv_defensive") %in% names(out)))
})


# ---- ng_reconcile_margin() -------------------------------------------------
# The step that makes the published column exactly conserving. Every number
# below is hand-computed in the comments so a failure says which rule broke.

test_that("ng_reconcile_margin lands each team on its own goal difference", {
  # Match m1, home H beat away A 3-1, so H's own gd is +2 and A's is -2.
  # The ledger gave H 1.5 and A -1.5, so H is 0.5 short and A is 0.5 long
  # (short = want - got = -2 - -1.5 = -0.5).
  ng <- data.table::data.table(
    match_id       = rep("m1", 6),
    player_id      = c("h1", "h2", "h3", "a1", "a2", "a3"),
    team_id        = rep(c("H", "A"), each = 3),
    minutes_played = c(90, 60, 30, 90, 90, 0),
    net_goals      = c(1.0, 0.3, 0.2, -1.0, -0.4, -0.1),
    ng_offensive   = c(0.8, 0.2, 0.1, -0.6, -0.3, -0.1),
    ng_defensive   = c(0.2, 0.1, 0.1, -0.4, -0.1,  0.0))
  fx <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
                   home_score = 3, away_score = 1)

  out <- ng_reconcile_margin(ng, fx, verbose = FALSE)
  data.table::setkey(out, player_id)

  # H: short = +0.5 over 180 minutes -> 90/180, 60/180, 30/180 of it.
  expect_equal(out["h1"]$ng_recon, 0.25)
  expect_equal(out["h2"]$ng_recon, 0.5 * 60 / 180)
  expect_equal(out["h3"]$ng_recon, 0.5 * 30 / 180)
  # A: short = -0.5 over 180 minutes, and a3 played none, so he gets nothing.
  expect_equal(out["a1"]$ng_recon, -0.25)
  expect_equal(out["a2"]$ng_recon, -0.25)
  expect_equal(out["a3"]$ng_recon, 0)

  tot <- out[, .(s = sum(net_goals)), by = team_id]
  expect_equal(tot[team_id == "H"]$s, 2)
  expect_equal(tot[team_id == "A"]$s, -2)
  # The two halves still add to the whole, with the residual inside defence.
  expect_equal(out$ng_offensive + out$ng_defensive, out$net_goals)
  expect_equal(out["h1"]$ng_offensive, 0.8)
  expect_equal(out["h1"]$ng_defensive, 0.2 + 0.25)
})

test_that("ng_reconcile_margin splits equally when a team has no minutes", {
  # Every minute missing: the fallback is an equal split, because dropping the
  # value would defeat the point of the step.
  ng <- data.table::data.table(
    match_id = rep("m1", 4), player_id = c("h1", "h2", "a1", "a2"),
    team_id = rep(c("H", "A"), each = 2),
    minutes_played = c(NA_real_, NA_real_, 90, 90),
    net_goals = c(0.5, 0.5, -0.5, -0.5),
    ng_offensive = c(0.5, 0.5, -0.5, -0.5), ng_defensive = c(0, 0, 0, 0))
  fx <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
                   home_score = 2, away_score = 0)
  out <- ng_reconcile_margin(ng, fx, verbose = FALSE)
  data.table::setkey(out, player_id)
  # H short = 2 - 1 = 1, split 0.5 / 0.5.
  expect_equal(out["h1"]$ng_recon, 0.5)
  expect_equal(out["h2"]$ng_recon, 0.5)
  # A short = -2 - -1 = -1, split by real minutes, also even here.
  expect_equal(out["a1"]$ng_recon, -0.5)
  expect_equal(sum(out[team_id == "H"]$net_goals), 2)
  expect_equal(sum(out[team_id == "A"]$net_goals), -2)
})

test_that("ng_reconcile_margin leaves a team-less row alone and says so", {
  ng <- data.table::data.table(
    match_id = rep("m1", 5), player_id = c("h1", "h2", "a1", "a2", "x"),
    team_id = c("H", "H", "A", "A", NA_character_),
    minutes_played = c(90, 90, 90, 90, 5),
    net_goals = c(0.5, 0.5, -0.5, -0.4, -0.1),
    ng_offensive = c(0.5, 0.5, -0.5, -0.4, -0.1),
    ng_defensive = c(0, 0, 0, 0, 0))
  fx <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
                   home_score = 1, away_score = 0)
  expect_warning(out <- ng_reconcile_margin(ng, fx, verbose = FALSE),
                 "no .*team_id")
  data.table::setkey(out, player_id)
  expect_equal(out["x"]$ng_recon, 0)
  expect_equal(out["x"]$net_goals, -0.1)
  expect_equal(sum(out[!is.na(team_id) & team_id == "H"]$net_goals), 1)
  expect_equal(sum(out[!is.na(team_id) & team_id == "A"]$net_goals), -1)
})

test_that("ng_reconcile_margin skips a match with no score and keeps its value", {
  ng <- data.table::data.table(
    match_id = rep(c("m1", "m2"), each = 2),
    player_id = c("h1", "a1", "h2", "a2"),
    team_id = c("H", "A", "H", "A"), minutes_played = rep(90, 4),
    net_goals = c(0.4, -0.4, 0.6, -0.6),
    ng_offensive = c(0.4, -0.4, 0.6, -0.6), ng_defensive = rep(0, 4))
  fx <- data.frame(match_id = c("m1", "m2"), home_team_id = "H",
                   away_team_id = "A", home_score = c(1, NA), away_score = c(0, NA))
  out <- ng_reconcile_margin(ng, fx, verbose = FALSE)
  data.table::setkey(out, player_id)
  expect_equal(out["h2"]$ng_recon, 0)
  expect_equal(out["h2"]$net_goals, 0.6)      # unplayed match untouched
  expect_equal(out["h1"]$net_goals, 1)
})

test_that("ng_reconcile_margin accepts the epv_* spelling of the halves", {
  ng <- data.table::data.table(
    match_id = rep("m1", 2), player_id = c("h1", "a1"),
    team_id = c("H", "A"), minutes_played = c(90, 90),
    net_goals = c(0.4, -0.4),
    epv_offensive = c(0.4, -0.4), epv_defensive = c(0, 0))
  fx <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
                   home_score = 1, away_score = 0)
  out <- ng_reconcile_margin(ng, fx, verbose = FALSE)
  expect_equal(out[player_id == "h1"]$epv_defensive, 0.6)
  expect_equal(out[player_id == "h1"]$net_goals, 1)
})

test_that("ng_reconcile_margin works with a non-character match_id", {
  # The regression this exists for: `%chin%` errors outright when its table is
  # not character, and `match_id` arrives as an integer from some loaders here.
  # Every other test in this file uses character ids, so none of them would
  # have caught it -- and the call site publishes an additive column, so an
  # abort would have cost a whole league's game logs, not just this one.
  ng <- data.table::data.table(
    match_id = rep(101L, 4), player_id = c("h1", "h2", "a1", "a2"),
    team_id = rep(c("H", "A"), each = 2), minutes_played = rep(90, 4),
    net_goals = c(0.4, 0.4, -0.4, -0.4),
    ng_offensive = c(0.4, 0.4, -0.4, -0.4), ng_defensive = rep(0, 4))
  fx <- data.frame(match_id = 101L, home_team_id = "H", away_team_id = "A",
                   home_score = 3, away_score = 1)
  out <- ng_reconcile_margin(ng, fx, verbose = FALSE)
  # H short = 2 - 0.8 = 1.2 over two equal shares; A short = -2 - -0.8 = -1.2.
  expect_equal(sum(out[team_id == "H"]$net_goals), 2)
  expect_equal(sum(out[team_id == "A"]$net_goals), -2)
  expect_equal(out[player_id == "h1"]$ng_recon, 0.6)
})

test_that("ng_reconcile_margin aborts rather than guessing when a half is missing", {
  ng <- data.table::data.table(
    match_id = "m1", player_id = c("h1", "a1"), team_id = c("H", "A"),
    minutes_played = c(90, 90), net_goals = c(0.4, -0.4))
  fx <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
                   home_score = 1, away_score = 0)
  expect_error(ng_reconcile_margin(ng, fx, verbose = FALSE), "epv_defensive")
})


# =============================================================================
# The xGOT shot split and the shot chain (Pete, 2026-09-23), on his worked
# example: a shot worth xG 0.03, struck on target at xGOT 0.20, saved, and the
# attack regathers at 0.04.
# =============================================================================
ng_fixture_shot_save <- function(result = "fail", xgot = 0.20) {
  data.frame(
    match_id = "m1", action_id = 1:3, original_event_id = 1:3,
    team_id = c("H", "A", "H"), player_id = c("h1", "k1", "h2"),
    action_type = c("shot", "keeper_save", "pass"),
    result = c(result, "success", "success"),
    epv = c(0.03, -0.03, 0.04),
    # shot: outcome - xG; save (keeper's frame): the model's -0.03 -> -0.04;
    # pass: whatever, it is not under test
    epv_delta = c(if (result == "success") 0.97 else -0.03, -0.01, 0.01),
    xgot = c(xgot, NA, NA),
    stringsAsFactors = FALSE)
}

test_that("a saved on-target shot pays the strike to the shooter and the save to the keeper", {
  pay <- ng_build_ledger(ng_fixture_shot_save(), fixtures = ng_fixture_fixtures(), verbose = FALSE)
  sh <- ng_shares()
  s <- pay[action_id == 1L]
  # strike +0.17 and finish -0.20: the shooter keeps shot_keep (0.9) of EACH,
  # gain or loss alike, so +0.153 and -0.180, and his team the other 10%.
  expect_equal(s[role == "shot_strike", value_own], 0.153, tolerance = 1e-12)
  expect_equal(s[role == "shot_finish", value_own], -0.180, tolerance = 1e-12)
  expect_equal(s[entry == "offence" & role == "pool_off", sum(value_own)], 0.1 * -0.03, tolerance = 1e-12)
  # the keeper (named as the stopper) takes named_share of the save, +0.20
  expect_equal(s[role == "defender" & player_id == "k1", value_own], 0.20 * sh$named_share,
               tolerance = 1e-12)
  # each side still books exactly the row's value, -0.03 / +0.03
  expect_equal(s[entry == "offence", sum(value_own)], -0.03, tolerance = 1e-12)
  expect_equal(s[entry == "defence", sum(value_own)], 0.03, tolerance = 1e-12)
})

test_that("the row after a shot starts from 0, so the rebound is booked in full", {
  on <- ng_build_ledger(ng_fixture_shot_save(), fixtures = ng_fixture_fixtures(), verbose = FALSE)
  off <- ng_build_ledger(ng_fixture_shot_save(), fixtures = ng_fixture_fixtures(),
                         shot_chain = FALSE, verbose = FALSE)
  # keeper's side of the save row: -0.03 + -0.01 = -0.04 (attack regathered at 0.04)
  expect_equal(on[action_id == 2L & entry == "offence", sum(value_own)], -0.04, tolerance = 1e-12)
  expect_equal(off[action_id == 2L & entry == "offence", sum(value_own)], -0.01, tolerance = 1e-12)
  # a shot's own row is never restarted
  expect_equal(on[action_id == 1L & entry == "offence", sum(value_own)], -0.03, tolerance = 1e-12)
})

test_that("a goal blames the side's keeper from the lineup, and off target nobody is named", {
  d <- ng_fixture_shot_save(result = "success")[1, ]
  lu <- data.frame(match_id = "m1", team_id = "A", player_id = "gkA",
                   position = "Goalkeeper", sub_off_minute = NA_real_)
  pay <- ng_build_ledger(d, fixtures = ng_fixture_fixtures(), lineups = lu, verbose = FALSE)
  sh <- ng_shares()
  # finish = 0.97 - 0.17 = 0.80 (xGOT 0.20 -> 1): keeper blamed named_share of it
  expect_equal(pay[player_id == "gkA", value_own], -0.80 * sh$named_share, tolerance = 1e-12)
  expect_equal(pay[entry == "defence", sum(value_own)], -0.97, tolerance = 1e-12)

  off <- ng_fixture_shot_save(xgot = 0)[1, ]
  p2 <- ng_build_ledger(off, fixtures = ng_fixture_fixtures(), lineups = lu, verbose = FALSE)
  expect_false(any(p2$player_id %in% "gkA"))                 # no keeper step off target
  expect_equal(p2[entry == "offence", sum(value_own)], -0.03, tolerance = 1e-12)
})

test_that("a deflected goal (xGOT 0, not on target) still pays its finish", {
  d <- ng_fixture_shot_save(result = "success", xgot = 0)[1, ]
  pay <- ng_build_ledger(d, fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(pay[entry == "offence", sum(value_own)], 0.97, tolerance = 1e-12)
  expect_equal(pay[entry == "defence", sum(value_own)], -0.97, tolerance = 1e-12)
})

test_that("a missed shot that rebounds to a second shot books its whole value", {
  # the first shot misses (xGOT 0) but its value runs to the rebound's xG 0.81,
  # which is what calculate_action_epv() gives a shot followed by a shot
  d <- data.frame(match_id = "m1", action_id = 1:2, original_event_id = 1:2,
                  team_id = "H", player_id = c("h1", "h2"), action_type = "shot",
                  result = c("fail", "success"), epv = c(0.05, 0.81),
                  epv_delta = c(0.76, 0.19), xgot = c(0, 0.94), stringsAsFactors = FALSE)
  pay <- ng_build_ledger(d, fixtures = ng_fixture_fixtures(), verbose = FALSE)
  expect_equal(pay[action_id == 1L & entry == "offence", sum(value_own)], 0.76, tolerance = 1e-12)
  expect_equal(pay[action_id == 1L & entry == "defence", sum(value_own)], -0.76, tolerance = 1e-12)
})

test_that("keepers share pools only for play in their own third; team totals hold", {
  d <- ng_fixture_spadl()
  fxt <- ng_fixture_fixtures()
  lineup <- data.frame(
    match_id = "m1", player_id = c(paste0("h", 1:11), paste0("a", 1:11)),
    team_id = rep(c("H", "A"), each = 11), is_starter = TRUE, minutes_played = 90,
    sub_on_minute = 0, sub_off_minute = 0,
    position = rep(c("Goalkeeper", rep("Defender", 10)), 2), stringsAsFactors = FALSE)
  # every fixture action is H's, taken at x = 50 (midfield): outside both
  # keepers' own thirds, so neither keeper shares any pool from them
  acts <- transform(d, time_seconds = c(10, 20, 65, 90), period_id = 1L, start_x = 50)
  pay <- ng_build_ledger(d, fixtures = fxt, verbose = FALSE)
  flat <- ng_spread_pools(pay, acts, lineup, dacts_share = 0, keeper_outside_weight = 1, verbose = FALSE)
  zone <- ng_spread_pools(pay, acts, lineup, dacts_share = 0, verbose = FALSE)   # the default
  kpool <- function(p, who) p[player_id == who & grepl("_spread$", role), sum(abs(value_own))]
  expect_gt(kpool(flat, "a1"), 0)
  expect_equal(kpool(zone, "a1"), 0)
  expect_equal(kpool(zone, "h1"), 0)
  # deep in A's own third (H attacking at x = 90), A's keeper shares again
  acts2 <- transform(acts, start_x = 90)
  deep <- ng_spread_pools(pay, acts2, lineup, dacts_share = 0, verbose = FALSE)
  expect_gt(kpool(deep, "a1"), 0)
  for (p in list(flat, zone, deep)) {
    expect_equal(sum(p[team_id == "A"]$value_own), -1.00, tolerance = 1e-10)
    expect_equal(sum(p[team_id == "H"]$value_own), 1.00, tolerance = 1e-10)
  }
})

test_that("the keeper named on a goal is the one who played, and the incoming one after a swap", {
  d <- ng_fixture_shot_save(result = "success")[1, ]
  d$time_seconds <- 80 * 60
  lu <- data.frame(match_id = "m1", team_id = "A",
                   player_id = c("bench", "gk1", "gk2"),
                   position = c("Goalkeeper", "Goalkeeper", "Substitute"),
                   minutes_played = c(0, 60, 30), sub_on_minute = c(0, 0, 60),
                   sub_off_minute = c(0, 60, 0), stringsAsFactors = FALSE)
  pay <- ng_build_ledger(d, fixtures = ng_fixture_fixtures(), lineups = lu, verbose = FALSE)
  # the goal is in the 80th minute: gk1 went off at 60 and gk2 came on then
  expect_true("gk2" %in% pay$player_id)
  expect_false(any(pay$player_id %in% c("bench", "gk1")))
  d$time_seconds <- 30 * 60
  pay <- ng_build_ledger(d, fixtures = ng_fixture_fixtures(), lineups = lu, verbose = FALSE)
  expect_true("gk1" %in% pay$player_id)
})

test_that("the shot chain never carries a shot into the next match", {
  d <- rbind(ng_fixture_shot_save()[1, ], ng_fixture_shot_save()[3, ])
  d$match_id <- c("m1", "m2"); d$action_id <- c(1L, 1L)
  fx <- rbind(ng_fixture_fixtures(), transform(ng_fixture_fixtures(), match_id = "m2"))
  pay <- ng_build_ledger(d, fixtures = fx, verbose = FALSE)
  # m2's first row keeps its own value (0.01), not epv + delta (0.05)
  expect_equal(pay[match_id == "m2" & entry == "offence", sum(value_own)], 0.01, tolerance = 1e-12)
})


# ---------------------------------------------------------------------------
# Shot aftermath (2026-09-23). Fixture: H passes, shoots (xG 0.10, xGOT 0.30),
# A's keeper saves and A clears. Given line A = 0.04 flat (slope 0), so the shot
# is worth V0 = 0.10 + 0.90 * 0.04 = 0.136. By hand:
#   pass:  0.05 -> 0.10 targeted xG (+0.05); now -> 0.136, so +0.086
#   shot (miss): ends at the save row's start, flipped: -(-0.05) = +0.05, so
#          0.05 - 0.136 = -0.086; goal part 0 - 0.10 = -0.10; aftermath +0.014
#   save:  its own change only, -0.05 -> -0.06 = -0.01 (keeper's frame)
ng_fixture_aftermath <- function(result = "fail") {
  data.frame(
    match_id = "m1", action_id = 1:4, original_event_id = 1:4, period_id = 1L,
    team_id = c("H", "H", "A", "A"), player_id = c("h1", "h2", "k1", "a2"),
    action_type = c("pass", "shot", "keeper_save", "clearance"),
    result = c("success", result, "success", "success"),
    epv = c(0.05, 0.10, -0.05, -0.06),
    epv_delta = c(0.05, if (result == "success") 0.90 else -0.10, -0.01, 0.01),
    xgot = c(NA, 0.30, NA, NA),
    stringsAsFactors = FALSE)
}
aft_line <- list(intercept = 0.04, slope = 0)

test_that("shot aftermath: the shot is worth more than its xG and ends where the next row starts", {
  pay <- panna::ng_build_ledger(ng_fixture_aftermath(), fixtures = ng_fixture_fixtures(),
                                shot_aftermath = aft_line, verbose = FALSE)
  sh <- ng_shares()
  off <- function(i) pay[action_id == i & entry == "offence", sum(value_own)]
  def <- function(i) pay[action_id == i & entry == "defence", sum(value_own)]
  expect_equal(off(1L), 0.086, tolerance = 1e-12)
  expect_equal(off(2L), -0.086, tolerance = 1e-12)
  expect_equal(off(3L), -0.01, tolerance = 1e-12)   # the keeper's own change only
  for (i in 1:4) expect_equal(def(i), -off(i), tolerance = 1e-12)
  # aftermath +0.014 is a gain: the shooter keeps (1 - off_pool), the defence pool pays it
  s <- pay[action_id == 2L]
  expect_equal(s[role == "shot_aftermath", value_own], 0.014 * 0.9, tolerance = 1e-12)
  expect_true(any(abs(s[entry == "defence" & role == "pool_def"]$value_own + 0.014) < 1e-12))
  # strike +0.20 and finish -0.30, the shooter keeps 0.9 of each: +0.18, -0.27
  expect_equal(s[role == "shot_strike", value_own], 0.18, tolerance = 1e-12)
  expect_equal(s[role == "shot_finish", value_own], -0.27, tolerance = 1e-12)
  expect_null(attr(pay, "shot_aftermath_fit")$n_fit)
})

test_that("shot aftermath: a goal still ends at 1 and gives back the aftermath it did not need", {
  pay <- panna::ng_build_ledger(ng_fixture_aftermath("success"), fixtures = ng_fixture_fixtures(),
                                shot_aftermath = aft_line, verbose = FALSE)
  sh <- ng_shares()
  # 1 - 0.136 = 0.864; goal part 1 - 0.10 = 0.90; aftermath -0.036, a loss
  expect_equal(pay[action_id == 2L & entry == "offence", sum(value_own)], 0.864, tolerance = 1e-12)
  expect_equal(pay[action_id == 2L & role == "shot_aftermath", value_own], -0.036 * 0.9,
               tolerance = 1e-12)
  # the row after a goal still restarts from 0: -0.05 + -0.01
  expect_equal(pay[action_id == 3L & entry == "offence", sum(value_own)], -0.06, tolerance = 1e-12)
})

test_that("shot aftermath: a shot that ends the period ends at 0", {
  d <- ng_fixture_aftermath()
  d$period_id <- c(1L, 1L, 2L, 2L)
  pay <- panna::ng_build_ledger(d, fixtures = ng_fixture_fixtures(),
                                shot_aftermath = aft_line, verbose = FALSE)
  expect_equal(pay[action_id == 2L & entry == "offence", sum(value_own)], -0.136, tolerance = 1e-12)
})

test_that("shot aftermath: the line is fitted on non-goal shots, and too few shots fall back loudly", {
  set.seed(1)
  n <- 60L
  xg <- round(runif(n, 0.02, 0.4), 3)
  nxt <- round(0.03 + 0.05 * xg + rnorm(n, 0, 0.005), 4)   # next state, attackers' frame
  d <- data.frame(
    match_id = "m1", action_id = seq_len(2L * n), original_event_id = seq_len(2L * n), period_id = 1L,
    team_id = rep(c("H", "A"), n), player_id = rep(c("h1", "k1"), n),
    action_type = rep(c("shot", "keeper_save"), n), result = rep(c("fail", "success"), n),
    epv = as.vector(rbind(xg, -nxt)), epv_delta = as.vector(rbind(-xg, 0)),
    stringsAsFactors = FALSE)
  pay <- panna::ng_build_ledger(d, fixtures = ng_fixture_fixtures(), verbose = FALSE)
  fit <- attr(pay, "shot_aftermath_fit")
  ref <- unname(coef(lm(nxt ~ xg)))
  expect_equal(c(fit$intercept, fit$slope), ref, tolerance = 1e-10)
  expect_equal(fit$n_fit, n)

  # Too few shots: the default line, said at the time, and flagged on the fit.
  few <- d[1:20, ]
  expect_message(p2 <- panna::ng_build_ledger(few, fixtures = ng_fixture_fixtures(), verbose = FALSE),
                 "too few to fit")
  f2 <- attr(p2, "shot_aftermath_fit")
  expect_true(f2$fallback)
  expect_equal(c(f2$intercept, f2$slope), c(0.0349, 0.0364))
})

test_that("shot aftermath: an own goal keeps its sign even without the is_own_goal flag", {
  # H scores into its own net: epv 0.02 before, booked -1 - 0.02 = -1.02.
  d <- ng_fixture_aftermath()[1:2, ]
  d$result[2] <- "success"; d$epv_delta[2] <- -1.02
  pay <- panna::ng_build_ledger(d, fixtures = ng_fixture_fixtures(),
                                shot_aftermath = aft_line, verbose = FALSE)
  expect_equal(pay[action_id == 2L & entry == "offence", sum(value_own)], -1.02, tolerance = 1e-12)
  expect_equal(pay[action_id == 1L & entry == "offence", sum(value_own)], 0.05, tolerance = 1e-12)
})

test_that("shot aftermath: a shot that ends a match does not read the next match", {
  # m1 ends on H's missed shot; m2 opens with A's pass. The shot must end at 0,
  # not at m2's first value: 0 - 0.136 = -0.136.
  d <- ng_fixture_aftermath()[1:2, ]
  d2 <- ng_fixture_aftermath()[3:4, ]
  d2$match_id <- "m2"; d2$action_id <- 1:2
  fx <- rbind(ng_fixture_fixtures(), transform(ng_fixture_fixtures(), match_id = "m2"))
  pay <- panna::ng_build_ledger(rbind(d, d2), fixtures = fx,
                                shot_aftermath = aft_line, verbose = FALSE)
  expect_equal(pay[match_id == "m1" & action_id == 2L & entry == "offence", sum(value_own)],
               -0.136, tolerance = 1e-12)
})
