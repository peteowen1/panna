library(testthat)

# Extra-time / penalty-shootout WPA fixes (2026-05-31). The earlier (separate)
# POV regression is covered elsewhere; these tests cover the ET/pens follow-up:
#   - is_shootout_period() period classification
#   - convert_opta_to_spadl() drops shootout events
#   - create_wp_features() match-aware time denominator + is_extra_time feature

test_that("is_shootout_period flags period_id >= 5 only", {
  expect_equal(
    is_shootout_period(c(1L, 2L, 3L, 4L, 5L, 16L, NA_integer_)),
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE)
  )
  # Regulation and extra-time periods are never shootout.
  expect_false(any(is_shootout_period(OPTA_REGULATION_PERIODS)))
  expect_false(any(is_shootout_period(OPTA_EXTRA_TIME_PERIODS)))
})

test_that("convert_opta_to_spadl drops penalty-shootout events", {
  # Two regulation passes + one shootout 'goal' (period 5, minute 120).
  events <- data.frame(
    match_id = rep("m1", 3),
    type_id = c(1L, 1L, 16L),        # pass, pass, goal (shootout)
    period_id = c(1L, 2L, 5L),
    team_id = c("A", "B", "A"),
    player_id = c("p1", "p2", "p3"),
    player_name = c("P1", "P2", "P3"),
    minute = c(10L, 60L, 120L),
    second = c(0L, 0L, 0L),
    x = c(50, 40, 94),
    y = c(50, 50, 50),
    outcome = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  spadl <- suppressWarnings(convert_opta_to_spadl(events))

  # The shootout event (period 5) must not survive into SPADL.
  expect_false(any(spadl$period_id >= 5L))
  expect_false("p3" %in% spadl$player_id)
})

test_that("create_wp_features uses a match-aware time denominator + is_extra_time", {
  ht <- data.frame(match_id = c("reg", "et"),
                   home_team_id = c("A", "A"), stringsAsFactors = FALSE)

  # Regulation-only match: a 90:00 action should read time_remaining ~ 0.
  reg <- data.frame(
    match_id = "reg", team_id = c("A", "A"),
    player_id = c("p1", "p2"), player_name = c("P1", "P2"),
    action_type = c("pass", "pass"), result = c("success", "success"),
    period_id = c(1L, 2L),
    time_seconds = c(100, 5400),      # last action at exactly 90:00
    x = c(50, 60), y = c(50, 40), stringsAsFactors = FALSE
  )
  fr <- create_wp_features(reg, home_teams = ht)
  expect_true("is_extra_time" %in% names(fr))
  expect_equal(fr$is_extra_time, c(0L, 0L))
  expect_equal(fr$time_remaining[fr$time_seconds == 5400], 0)
  expect_true(all(fr$time_remaining >= 0 & fr$time_remaining <= 1))

  # Extra-time match: a 100:00 action must NOT clamp to 0 the way a fixed-5400
  # denominator would; it should be (7200 - 6000)/7200.
  et <- data.frame(
    match_id = "et", team_id = c("A", "A", "B"),
    player_id = c("p1", "p2", "p3"), player_name = c("P1", "P2", "P3"),
    action_type = c("pass", "pass", "pass"),
    result = c("success", "success", "success"),
    period_id = c(2L, 3L, 4L),        # reg 2H, ET1, ET2 -> match reached ET
    time_seconds = c(3000, 6000, 7000),
    x = c(50, 60, 40), y = c(50, 40, 30), stringsAsFactors = FALSE
  )
  fe <- create_wp_features(et, home_teams = ht)
  expect_equal(fe$is_extra_time, c(0L, 1L, 1L))   # periods 3,4 are ET
  tr_100min <- fe$time_remaining[fe$time_seconds == 6000]
  expect_gt(tr_100min, 0)                          # not clamped to 0
  expect_equal(tr_100min, (7200 - 6000) / 7200, tolerance = 1e-9)
  expect_true(all(fe$time_remaining >= 0 & fe$time_remaining <= 1))
})
