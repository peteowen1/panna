# Tests for splint creation functions

test_that("create_splint_boundaries creates valid boundaries", {
  # Mock events - need is_home column for goal tracking
  events <- data.frame(
    minute = c(25, 45, 67, 80),
    is_goal = c(TRUE, FALSE, FALSE, TRUE),
    is_sub = c(FALSE, TRUE, TRUE, FALSE),
    is_home = c(TRUE, FALSE, TRUE, FALSE)
  )

  boundaries <- create_splint_boundaries(events, include_goals = TRUE)

  expect_true(all(boundaries$start_minute >= 0))
  # Match end is now dynamic (91 minimum for 1 minute stoppage)
  expect_true(all(boundaries$end_minute <= 91))
  expect_true(all(boundaries$duration > 0))
  expect_equal(boundaries$start_minute[1], 0)
  # Last boundary ends at match_end (91 by default)
  expect_equal(boundaries$end_minute[nrow(boundaries)], 91)
})

test_that("create_splint_boundaries handles empty events", {
  events <- data.frame(
    minute = numeric(0),
    is_goal = logical(0),
    is_sub = logical(0),
    is_home = logical(0)
  )

  # With include_halftime = TRUE (default), empty events creates 2 splints:
  # 0-46 (first half + default 1 min stoppage), 46-91 (second half + default stoppage)
  boundaries <- create_splint_boundaries(events)

  expect_equal(nrow(boundaries), 2)
  expect_equal(boundaries$start_minute[1], 0)
  expect_equal(boundaries$end_minute[1], 46)
  expect_equal(boundaries$start_minute[2], 46)
  expect_equal(boundaries$end_minute[2], 91)
})

test_that("create_splint_boundaries respects include_goals parameter", {
  events <- data.frame(
    minute = c(30, 60),
    is_goal = c(TRUE, TRUE),
    is_sub = c(FALSE, FALSE),
    is_home = c(TRUE, FALSE)
  )

  with_goals <- create_splint_boundaries(events, include_goals = TRUE)
  without_goals <- create_splint_boundaries(events, include_goals = FALSE)

  expect_gt(nrow(with_goals), nrow(without_goals))
})

test_that("create_splint_boundaries handles added time correctly", {
  # Events with stoppage time: 3 mins first half, 11 mins second half
  events <- data.frame(
    minute = c(30, 45, 45, 60, 90, 90),
    added_time = c(NA, NA, 3L, NA, 5L, 11L),  # Goals at 45', 45+3', 60', 90+5', 90+11'
    is_goal = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE),
    is_sub = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    is_home = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
  )

  boundaries <- create_splint_boundaries(events, include_goals = TRUE)

  # With continuous time: first half ends at 48, second half offset by 3
  # Match end should be 90 + 11 + 3 (offset) + buffer = 104.5
  match_end <- boundaries$end_minute[nrow(boundaries)]
  expect_gt(match_end, 104)

  # Check that first-half stoppage goal (45+3 = 48) creates boundary at 48
  expect_true(48 %in% boundaries$start_minute | 48 %in% boundaries$end_minute)

  # Second half event at minute 60 should be offset: 60 + 3 = 63
  expect_true(63 %in% boundaries$start_minute | 63 %in% boundaries$end_minute)

  # Check boundaries are properly ordered
  expect_true(all(diff(boundaries$start_minute) > 0))
})

test_that("calculate_effective_minute uses continuous time with offset", {
  # Without first-half stoppage (default behavior)
  expect_equal(calculate_effective_minute(45, 3), 48)
  expect_equal(calculate_effective_minute(45, 0), 45)
  expect_equal(calculate_effective_minute(90, 11), 101)
  expect_equal(calculate_effective_minute(46, 0), 46)  # No offset when no stoppage

  # With 3 mins first-half stoppage - second half events get offset
  expect_equal(calculate_effective_minute(46, 0, first_half_stoppage = 3), 49)  # 46 + 3
  expect_equal(calculate_effective_minute(60, 0, first_half_stoppage = 3), 63)  # 60 + 3
  expect_equal(calculate_effective_minute(90, 0, first_half_stoppage = 3), 93)  # 90 + 3
  expect_equal(calculate_effective_minute(90, 11, first_half_stoppage = 3), 104) # 90 + 11 + 3

  # First half events NOT offset
  expect_equal(calculate_effective_minute(30, NA, first_half_stoppage = 3), 30)
  expect_equal(calculate_effective_minute(45, 3, first_half_stoppage = 3), 48)  # 45 + 3, no extra offset

  # Test vectorized with offset
  result <- calculate_effective_minute(c(30, 45, 46, 90), c(NA, 3L, NA, 11L), first_half_stoppage = 3)
  expect_equal(result, c(30, 48, 49, 104))
})
