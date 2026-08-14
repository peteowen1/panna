# Guards added after the 2026-08-14 review of the RAPM fitting core. Each one
# covers an assumption the code already relied on but never checked, where the
# failure mode was silent-and-wrong rather than an error.
#
# Self-contained synthetic fixtures, deliberately not shared with
# test-rapm-model.R / test-rapm-tripwires.R, so this file has no dependency on
# cross-file execution order.

.guard_rapm_data <- function(cov_last = TRUE, n_players = 4, n_rows = 60) {
  set.seed(42)
  pids <- paste0("g_player_", seq_len(n_players))
  pcols <- c(paste0(pids, "_off"), paste0(pids, "_def"))
  covs <- c("is_home", "avg_min")

  X <- matrix(stats::rnorm(n_rows * (length(pcols) + length(covs))), nrow = n_rows)
  colnames(X) <- c(pcols, covs)
  if (!cov_last) {
    # Move one covariate to the front -- the exact shape the positional
    # penalty factor cannot detect on its own.
    X <- X[, c("is_home", pcols, "avg_min"), drop = FALSE]
  }

  list(
    X = X,
    y = stats::rnorm(n_rows),
    weights = rep(1, n_rows),
    player_ids = pids,
    n_players = n_players,
    covariate_names = covs,
    player_mapping = NULL,
    mode = "od"
  )
}


test_that("fit_rapm accepts a design whose covariates are the last columns", {
  expect_no_error(
    fit_rapm(.guard_rapm_data(cov_last = TRUE), parallel = FALSE, nfolds = 3)
  )
})


test_that("fit_rapm aborts when covariates are not the last columns", {
  # The penalty factor is built positionally as c(rep(1, n - n_cov),
  # rep(0, n_cov)). Before this guard a reordered design silently left player
  # columns unpenalized and penalized a covariate instead -- distorting every
  # rating with no error anywhere.
  expect_error(
    fit_rapm(.guard_rapm_data(cov_last = FALSE), parallel = FALSE, nfolds = 3),
    "must occupy the last"
  )
})


test_that("fit_rapm and fit_rapm_with_prior resolve covariates identically", {
  # fit_rapm() previously read only $covariate_names while
  # fit_rapm_with_prior() fell back to $covariate_cols, so the same
  # test-shaped rapm_data had covariates penalized in one and not the other.
  by_names <- .guard_rapm_data()
  by_cols <- .guard_rapm_data()
  by_cols$covariate_cols <- by_cols$covariate_names
  by_cols$covariate_names <- NULL

  m_names <- fit_rapm(by_names, parallel = FALSE, nfolds = 3)
  m_cols <- fit_rapm(by_cols, parallel = FALSE, nfolds = 3)

  expect_equal(m_names$panna_metadata$n_covariates, 2L)
  expect_equal(m_cols$panna_metadata$n_covariates, 2L)
  expect_identical(
    m_names$panna_metadata$covariate_names,
    m_cols$panna_metadata$covariate_names
  )
})


test_that("fit_rapm_with_prior aborts on a non-finite prior value", {
  # y_adjusted <- y - X %*% prior_vec runs AFTER the NA-row filter, so a
  # single NA in the prior propagates to every row and surfaced only as an
  # opaque glmnet error about NA/NaN/Inf.
  d <- .guard_rapm_data()
  good <- stats::setNames(rep(0.1, length(d$player_ids)), d$player_ids)
  bad <- good
  bad[2] <- NA_real_

  expect_no_error(fit_rapm_with_prior(d, good, good, nfolds = 3))
  expect_error(
    fit_rapm_with_prior(d, bad, good, nfolds = 3),
    "non-finite value"
  )
})


test_that("fit_rapm_with_prior names the offending players in the abort", {
  d <- .guard_rapm_data()
  good <- stats::setNames(rep(0.1, length(d$player_ids)), d$player_ids)
  bad <- good
  bad[["g_player_2"]] <- Inf

  expect_error(
    fit_rapm_with_prior(d, bad, good, nfolds = 3),
    "g_player_2"
  )
})


test_that("the player_mapping join aborts on a duplicated player_id", {
  # mapping[ratings, on = "player_id"] silently multiplies rating rows when the
  # mapping has a duplicate -- every caller trusts the row count afterwards.
  ratings <- data.frame(player_id = c("p1", "p2"), rapm = c(0.3, -0.1))
  dup <- data.frame(
    player_id = c("p1", "p1", "p2"),
    player_name = c("One", "One (dup)", "Two")
  )

  expect_error(.join_player_mapping(ratings, dup), "duplicated player_id")
  expect_equal(nrow(.join_player_mapping(ratings, dup[-2, ])), 2L)
  expect_identical(.join_player_mapping(ratings, NULL), ratings)
})


test_that("n_player_cols counts the synthetic replacement column", {
  # player_ids includes "replacement"; n_players does not, so the old
  # n_players * 2 metadata was short by one column per side.
  d <- .guard_rapm_data()
  d$player_ids <- c(d$player_ids, "replacement")
  d$X <- cbind(d$X, replacement_off = 0, replacement_def = 0)
  # Keep covariates last -- the positional penalty factor asserts it.
  cn <- colnames(d$X)
  d$X <- d$X[, c(setdiff(cn, d$covariate_names), d$covariate_names), drop = FALSE]

  m <- fit_rapm(d, parallel = FALSE, nfolds = 3)
  expect_equal(m$panna_metadata$n_player_cols, 10L)
  expect_equal(m$panna_metadata$n_player_cols, length(d$player_ids) * 2L)
})


test_that(".subset_rapm_data_expanding rejects a net-mode design", {
  # The column prune is written against _off/_def pairs. Given a net design,
  # match(paste0(pids, "_off"), cn) is all-NA, pkeep becomes all-NA, and
  # new_pids <- pids[pkeep] returned a same-length vector of NAs: silent
  # garbage rather than an error.
  d <- .guard_rapm_data()
  d$X_full <- d$X
  d$mode <- "net"
  ssm <- data.frame(splint_id = 1L, season_end_year = 2024L)

  expect_error(
    .subset_rapm_data_expanding(d, ssm, cutoff_year = 2025L),
    'supports .*"od".* designs only'
  )
})
