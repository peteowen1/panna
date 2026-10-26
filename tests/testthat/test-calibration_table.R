# Tests for calibration_table()

test_that("calibration_table reports empty bins with n = 0 and NA means", {
  # Draw probability never exceeds 0.4, so the top Draw bins must be empty
  set.seed(1)
  n <- 200
  p_draw <- runif(n, 0.1, 0.4)
  p_home <- (1 - p_draw) * 0.6
  p_away <- (1 - p_draw) * 0.4
  probs <- cbind(p_home, p_draw, p_away)
  y <- sample(0:2, n, replace = TRUE)

  cal <- calibration_table(y, probs, n_bins = 5L)
  draw <- cal[cal$outcome == "Draw", ]

  expect_equal(nrow(draw), 5L)
  empty <- draw[draw$bin_lower >= 0.4, ]
  expect_gt(nrow(empty), 0L)
  expect_true(all(empty$n == 0L))
  expect_true(all(is.na(empty$pred_mean)))
  expect_true(all(is.na(empty$actual_mean)))
  expect_false(any(is.na(draw$bin)))
})

test_that("calibration_table returns all bins for every outcome and keeps column names", {
  probs <- matrix(c(0.7, 0.2, 0.1,
                    0.2, 0.5, 0.3,
                    0.1, 0.1, 0.8), ncol = 3, byrow = TRUE)
  cal <- calibration_table(c(0L, 1L, 2L), probs, n_bins = 4L)

  expect_equal(nrow(cal), 12L)
  expect_true(all(c("bin", "pred_mean", "actual_mean", "n", "outcome") %in% names(cal)))
  expect_equal(as.vector(table(cal$outcome)), c(4L, 4L, 4L))
  expect_setequal(unique(cal$outcome), c("Home", "Draw", "Away"))
})

test_that("calibration_table bin counts add up to the number of rows per outcome", {
  set.seed(2)
  n <- 137
  raw <- matrix(runif(n * 3), ncol = 3)
  probs <- raw / rowSums(raw)
  y <- sample(0:2, n, replace = TRUE)

  cal <- calibration_table(y, probs, n_bins = 5L)
  totals <- tapply(cal$n, cal$outcome, sum)
  expect_equal(as.vector(totals), rep(n, 3L))
})

test_that("calibration_table puts probabilities of exactly 0 and 1 in the end bins", {
  probs <- matrix(c(1, 0, 0,
                    0, 1, 0), ncol = 3, byrow = TRUE)
  cal <- calibration_table(c(0L, 1L), probs, n_bins = 5L)
  home <- cal[cal$outcome == "Home", ]
  expect_equal(home$n, c(1L, 0L, 0L, 0L, 1L))
})

test_that("calibration_table on perfectly calibrated data gives pred_mean ~ actual_mean", {
  set.seed(3)
  n <- 60000
  raw <- matrix(runif(n * 3), ncol = 3)
  probs <- raw / rowSums(raw)
  # Draw outcomes from the stated probabilities
  u <- runif(n)
  y <- as.integer(u > probs[, 1]) + as.integer(u > probs[, 1] + probs[, 2])

  cal <- calibration_table(y, probs, n_bins = 5L)
  filled <- cal[cal$n > 200, ]
  expect_gt(nrow(filled), 0L)
  expect_lt(max(abs(filled$pred_mean - filled$actual_mean)), 0.03)
})

test_that("calibration_table counts NA probabilities in a separate NA-bin row", {
  probs <- matrix(c(0.7, 0.2, 0.1,
                    NA,  0.5, 0.3,
                    NA,  0.1, 0.8,
                    0.2, 0.3, 0.5), ncol = 3, byrow = TRUE)
  y <- c(0L, 1L, 2L, 0L)
  cal <- calibration_table(y, probs, n_bins = 5L)

  home <- cal[cal$outcome == "Home", ]
  expect_equal(nrow(home), 6L)
  expect_equal(home$n[is.na(home$bin)], 2L)
  expect_equal(sum(home$n), length(y))
  # Outcomes without NAs keep exactly n_bins rows
  expect_equal(nrow(cal[cal$outcome == "Draw", ]), 5L)
  expect_equal(sum(cal$n[cal$outcome == "Away"]), length(y))
})

test_that("calibration_table stops on probabilities outside [0, 1]", {
  probs <- matrix(c(0.7, 0.2, 0.1,
                    1.2, 0.5, 0.3), ncol = 3, byrow = TRUE)
  expect_error(calibration_table(c(0L, 1L), probs, n_bins = 5L), "Home.*\\[0, 1\\]")
  probs2 <- matrix(c(0.7, 0.2, 0.1,
                     0.2, -0.1, 0.3), ncol = 3, byrow = TRUE)
  expect_error(calibration_table(c(0L, 1L), probs2, n_bins = 5L), "Draw.*\\[0, 1\\]")
})
