# Guards for the sequence-feature lookback (R/epv_features.R)

test_that("n_prev = 0 means no sequence features, not leaked ones", {
  # The bug this pins: the loops used `1:n_prev`, and 1:0 is c(1, 0). Asking
  # for zero sequence features therefore built a `_prev0` column set where
  # shift(x, 0) is the identity -- the CURRENT action's result_success, dx, dy
  # and action type entering the model as "previous action" features. Target
  # leakage that nothing would have surfaced. With seq_len(), 0 correctly
  # produces no lag columns at all.
  expect_silent(panna:::.check_n_prev(0))
  cols <- panna:::get_epv_feature_cols(include_sequence = TRUE, n_prev = 0)
  expect_false(any(grepl("_prev[0-9]+$", cols)))
  # _prev0 specifically is the leakage signature.
  expect_false(any(grepl("_prev0$", cols)))
})

test_that(".check_n_prev rejects the other malformed inputs", {
  expect_error(panna:::.check_n_prev(-1), "non-negative whole number")
  expect_error(panna:::.check_n_prev(2.5), "non-negative whole number")
  expect_error(panna:::.check_n_prev(NA_real_), "non-negative whole number")
  expect_error(panna:::.check_n_prev(c(1, 2)), "non-negative whole number")
  expect_error(panna:::.check_n_prev("3"), "non-negative whole number")
  expect_silent(panna:::.check_n_prev(3L))
  expect_silent(panna:::.check_n_prev(1))
})

test_that("feature builder and column list share one n_prev", {
  # They used to hardcode 3 independently, so changing one silently gave the
  # model a feature set the builder never produced.
  expect_identical(formals(panna:::create_epv_features)$n_prev,
                   quote(EPV_N_PREV))
  expect_identical(formals(panna:::get_epv_feature_cols)$n_prev,
                   quote(EPV_N_PREV))
  expect_identical(panna:::EPV_N_PREV, 3L)
})

test_that("get_epv_feature_cols emits exactly n_prev lag suffixes", {
  cols <- panna:::get_epv_feature_cols(include_sequence = TRUE, n_prev = 2)
  expect_true(all(c("dx_prev1", "dx_prev2") %in% cols))
  expect_false(any(grepl("_prev0$", cols)))
  expect_false(any(grepl("_prev3$", cols)))

  none <- panna:::get_epv_feature_cols(include_sequence = FALSE)
  expect_false(any(grepl("_prev[0-9]+$", none)))
})
