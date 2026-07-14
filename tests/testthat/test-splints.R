# Tests for splint creation functions

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
