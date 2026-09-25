# .ng_breakdown(): a published player-match's categories add up exactly to its
# net_goals, and the gate refuses anything that does not.

ng_bd_fixture <- function() {
  # Match m1, team A: p1 and p2 published, p3 (unused sub) not. Team B: q1.
  pay <- data.table::data.table(
    match_id  = "m1",
    player_id = c("p1", "p1", "p2", "p2", "p3", "q1", "q1"),
    team_id   = c("A", "A", "A", "A", "A", "B", "B"),
    play_type = c("pass", "shot", "tackle", "pass", "pass", "shot", "clearance"),
    role      = c("actor", "shot_strike", "actor", "receiver", "actor", "defender", "actor"),
    value_own = c(0.10, 0.30, 0.05, -0.02, 0.04, -0.20, 0.07))
  pre <- pay[, .(net_goals = sum(value_own)), by = .(match_id, player_id, team_id)]
  pre[, minutes_played := c(90, 30, 0, 90)]
  # Fold p3's 0.04 to p1/p2 by minutes (90:30), then anchor.
  pub <- data.table::data.table(match_id = "m1", player_id = c("p1", "p2", "q1"),
                                team_id = c("A", "A", "B"))
  pub[, net_goals := c(0.40 + 0.03, 0.03 + 0.01, -0.13)]
  pub[, ng_recon := c(0.2, 0.1, -0.05)]
  pub[, net_goals := net_goals + ng_recon]
  list(pay = pay, pre = pre, pub = pub)
}

test_that("parts add up to published net_goals, with fold and anchor as their own rows", {
  f <- ng_bd_fixture()
  out <- .ng_breakdown(f$pay, f$pre, f$pub)
  tot <- out[, .(v = sum(value)), by = .(match_id, player_id)]
  m <- merge(tot, f$pub, by = c("match_id", "player_id"))
  expect_equal(m$v, m$net_goals, tolerance = 1e-12)
  expect_false("p3" %in% out$player_id)
  expect_equal(out[player_id == "p1" & category == "Share of unlisted team-mates", value], 0.03)
  expect_equal(out[player_id == "p1" & category == "Anchor to the real goal difference", value], 0.2)
  expect_equal(out[player_id == "p1" & category == "Shooting: placement (xG to xGOT)", value], 0.30)
  expect_equal(out[player_id == "q1" & category == "Stopping shots", value], -0.20)
})

test_that("a fold that does not match the unlisted players' value is refused", {
  f <- ng_bd_fixture()
  f$pub[player_id == "p1", net_goals := net_goals + 0.01]   # extra value from nowhere
  expect_error(.ng_breakdown(f$pay, f$pre, f$pub), "unlisted team-mates", class = "panna_ng_breakdown_mismatch")
})

test_that("a payment table that disagrees with ng_player_game is refused", {
  f <- ng_bd_fixture()
  f$pay[1, value_own := 0.5]     # pre_fold was built from the old value
  expect_error(.ng_breakdown(f$pay, f$pre, f$pub), "do not add up", class = "panna_ng_breakdown_mismatch")
})

test_that("a team with no published rows does not trip the fold check", {
  f <- ng_bd_fixture()
  extra <- data.table::data.table(match_id = "m1", player_id = "z1", team_id = "C",
                                  net_goals = 0.5, minutes_played = 90)
  f$pre <- rbind(f$pre, extra)
  expect_silent(out <- .ng_breakdown(f$pay, f$pre, f$pub))
  expect_false("z1" %in% out$player_id)
})

test_that("play types map role first", {
  expect_equal(.ng_play_type(c("shot", "shot", "pass", "pass", "keeper_save"),
                             c("shot_finish", "defender", "receiver", "actor", "actor")),
               c("Shooting: against the keeper", "Stopping shots", "Receiving a pass",
                 "Passing", "Keeper: handling"))
})
