# Tests for D4 (prior-match abort) + D5 (multi-target write-time tripwires) +
# D6 (run_multi_target gate pattern) — FABLE-PRIOR-FIX-PLAN.md Step 1
# ("guards first"). All synthetic/mocked; no cached pipeline data required.

# Minimal synthetic RAPM design -- deliberately NOT shared with
# test-rapm-model.R's create_test_rapm_data() so this file has no dependency
# on cross-file test execution order.
.tripwire_test_rapm_data <- function(n_splints = 20, n_players = 8) {
  set.seed(1)
  player_ids <- paste0("tw_player_", seq_len(n_players))
  n_rows <- n_splints * 2
  n_player_cols <- n_players * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_player_cols)

  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 4)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 4)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
  }

  covariates <- c("is_home")
  X_covariates <- matrix(rep(c(1, 0), n_splints), nrow = n_rows)
  X <- cbind(X_players, X_covariates)
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), covariates)

  y <- rnorm(n_rows, mean = 1.2, sd = 0.4)
  weights <- runif(n_rows, 5, 30)
  minutes <- sample(500:3000, n_players, replace = TRUE)

  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("TW Player", seq_len(n_players)),
    total_minutes = minutes,
    stringsAsFactors = FALSE
  )

  list(
    X = X, y = y, weights = weights,
    player_ids = player_ids, player_mapping = player_mapping,
    covariate_cols = covariates, covariate_names = covariates,
    n_players = n_players
  )
}


# =============================================================================
# D5: .check_degenerate_multi_target() tripwires
# =============================================================================

test_that(".check_degenerate_multi_target passes on healthy synthetic O/D ratings", {
  set.seed(2)
  ratings <- data.frame(
    player_id = paste0("p", 1:20),
    offense = rnorm(20, 0, 0.3),
    defense = rnorm(20, 0, 0.3)
  )
  expect_true(isTRUE(.check_degenerate_multi_target(ratings, "epv")))
})

test_that(".check_degenerate_multi_target aborts on all-shrunk coefficients", {
  # Mirrors the measured EPV all-shrunk signature (sd ~ 6e-6): whole-match
  # proration means the target can't vary within a lineup.
  ratings <- data.frame(
    player_id = paste0("p", 1:20),
    offense = 1e-6 + rnorm(20, 0, 1e-7),
    defense = -1e-6 + rnorm(20, 0, 1e-7)
  )
  expect_error(.check_degenerate_multi_target(ratings, "epv"), "all-shrunk")
})

test_that(".check_degenerate_multi_target aborts on mirrored offense/defense", {
  # Mirrors the measured WPA mirror signature (cor ~ -0.949): a near-zero-sum
  # target makes the O/D split mechanically unidentified.
  set.seed(3)
  off <- rnorm(30, 0, 0.4)
  ratings <- data.frame(
    player_id = paste0("p", 1:30),
    offense = off,
    defense = -off + rnorm(30, 0, 0.01)
  )
  expect_error(.check_degenerate_multi_target(ratings, "wpa"), "mirrored")
})

test_that(".check_degenerate_multi_target aborts on malformed input", {
  expect_error(.check_degenerate_multi_target(data.frame(x = 1), "epv"))
  expect_error(.check_degenerate_multi_target(list(offense = 1, defense = 1), "epv"))
})


# =============================================================================
# D4: fit_rapm_with_prior() prior-match abort
# =============================================================================

test_that("fit_rapm_with_prior aborts when offense_prior is supplied but unnamed (0 matched)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .tripwire_test_rapm_data()

  # Mirrors the 06_xrapm.R multi-target L3 bug: an unnamed all-zero vector --
  # match(player_ids, names(offense_prior)) can never succeed against it.
  bad_offense_prior <- rep(0, rapm_data$n_players)
  good_defense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.2), rapm_data$player_ids
  )

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = bad_offense_prior,
                        defense_prior = good_defense_prior, alpha = 0, nfolds = 3),
    "offense_prior.*supplied but matched 0"
  )
})

test_that("fit_rapm_with_prior aborts when defense_prior is supplied but unnamed (0 matched)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .tripwire_test_rapm_data()

  good_offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.2), rapm_data$player_ids
  )
  bad_defense_prior <- rep(0, rapm_data$n_players)

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = good_offense_prior,
                        defense_prior = bad_defense_prior, alpha = 0, nfolds = 3),
    "defense_prior.*supplied but matched 0"
  )
})

test_that("fit_rapm_with_prior does NOT abort on an explicit NULL prior (no-prior request)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .tripwire_test_rapm_data()

  expect_no_error(
    model <- fit_rapm_with_prior(rapm_data, offense_prior = NULL,
                                 defense_prior = NULL, alpha = 0, nfolds = 3)
  )
  expect_true(inherits(model, "cv.glmnet"))
  expect_equal(model$panna_metadata$type, "xrapm")
})

test_that("fit_rapm_with_prior works with a properly named prior (build_prior_vector pattern)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .tripwire_test_rapm_data()

  spm_data <- data.frame(
    player_id = rapm_data$player_ids,
    player_name = rapm_data$player_mapping$player_name,
    offense_spm = rnorm(rapm_data$n_players, 0, 0.2),
    stringsAsFactors = FALSE
  )
  offense_prior <- build_prior_vector(
    spm_data = spm_data, spm_col = "offense_spm",
    player_mapping = rapm_data$player_mapping
  )
  defense_prior <- stats::setNames(rep(0, rapm_data$n_players), rapm_data$player_ids)

  expect_true(sum(offense_prior != 0) > 0)

  expect_no_error(
    model <- fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                                 defense_prior = defense_prior, alpha = 0, nfolds = 3)
  )
  ratings <- extract_xrapm_ratings(model)
  expect_equal(nrow(ratings), rapm_data$n_players)
  expect_true(all(is.finite(ratings$xrapm)))
})


# =============================================================================
# D6: run_multi_target gate pattern (exists(..., inherits = FALSE), default FALSE)
# =============================================================================

test_that("run_multi_target gate pattern defaults to FALSE and ignores enclosing-scope objects", {
  # Mirrors the exact gate expression used in 04_rapm.R/05_spm.R/06_xrapm.R/
  # 07_seasonal_ratings.R. inherits = FALSE means a stray same-named object
  # in an ENCLOSING scope (the dplyr::sample_n-style collision documented in
  # r-datatable-gotchas.md) must not leak the gate on -- only an explicit
  # LOCAL assignment does.
  run_multi_target <- TRUE  # simulate a stray object in an enclosing scope

  resolved_default <- local({
    if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE
  })
  expect_false(resolved_default)

  resolved_true <- local({
    run_multi_target <- TRUE
    if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE
  })
  expect_true(resolved_true)

  resolved_false <- local({
    run_multi_target <- FALSE
    if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE
  })
  expect_false(resolved_false)
})
