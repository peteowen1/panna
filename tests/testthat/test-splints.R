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
  expect_true(all(boundaries$end_minute <= 90))
  expect_true(all(boundaries$duration > 0))
  expect_equal(boundaries$start_minute[1], 0)
  expect_equal(boundaries$end_minute[nrow(boundaries)], 90)
})

test_that("create_splint_boundaries handles empty events", {
  events <- data.frame(
    minute = numeric(0),
    is_goal = logical(0),
    is_sub = logical(0),
    is_home = logical(0)
  )

  # With include_halftime = TRUE (default), empty events creates 2 splints (0-45, 45-90)
  boundaries <- create_splint_boundaries(events)

  expect_equal(nrow(boundaries), 2)
  expect_equal(boundaries$start_minute[1], 0)
  expect_equal(boundaries$end_minute[1], 45)
  expect_equal(boundaries$start_minute[2], 45)
  expect_equal(boundaries$end_minute[2], 90)
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
