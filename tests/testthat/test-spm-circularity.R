# Tests for the SPM box-score-value circularity guard
# (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.4).
#
# The loop being tested: fit_rapm_with_prior() (R/rapm_model.R:389) shrinks
# player coefficients toward an SPM prior, so any target built from it (xRAPM,
# career panna) partially embeds box-stat information back into itself. If a
# box-score-value model were ever trained against such a target, an SPM prior
# corrupted by box stats would leak into the "ground truth" it's evaluated
# against. This file proves the loop is broken for the prior-free target
# (fit_rapm() output, no prior involved) and, as a demonstrative foil, shows
# the same corruption DOES leak through the prior-contaminated posterior
# (fit_rapm_with_prior() output) -- i.e. the discipline "target prior-free
# RAPM, never xRAPM/panna" is load-bearing, not cosmetic.
#
# Self-contained fixture -- deliberately NOT shared with test-rapm-model.R's
# create_test_rapm_data() or test-spm-asof.R's make_expanding_pooled_fixture()
# (test-rapm-tripwires.R convention: no dependency on cross-file execution
# order).
.circularity_test_rapm_data <- function(n_players = 200, n_splints = 500, seed = 2026) {
  set.seed(seed)
  player_ids <- paste0("cp_", seq_len(n_players))
  n_rows <- n_splints * 2
  splint_ids <- paste0("splint_", seq_len(n_splints))

  i_idx <- integer(0); j_idx <- integer(0)
  for (s in seq_len(n_splints)) {
    home <- sample.int(n_players, 5)
    away <- sample(setdiff(seq_len(n_players), home), 5)
    row_home <- (s - 1) * 2 + 1
    row_away <- row_home + 1
    i_idx <- c(i_idx, rep(row_home, 10), rep(row_away, 10))
    j_idx <- c(j_idx, home, n_players + away, away, n_players + home)
  }
  X_players <- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = 1,
                                    dims = c(n_rows, n_players * 2))
  col_names <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"))
  colnames(X_players) <- col_names

  is_home <- rep(c(1, 0), n_splints)
  X_full <- cbind(X_players, Matrix::Matrix(is_home, ncol = 1, sparse = TRUE))
  colnames(X_full) <- c(col_names, "is_home")

  list(
    X_full = X_full,
    y = stats::rnorm(n_rows, 1.5, 0.5),
    weights = stats::runif(n_rows, 5, 30),
    row_data = data.frame(splint_id = rep(splint_ids, each = 2)),
    player_ids = player_ids,
    player_mapping = data.frame(
      player_id = player_ids, player_name = paste("Player", player_ids),
      total_minutes = sample(500:3000, n_players, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    covariate_names = "is_home",
    n_players = n_players,
    target_type = "xg"
  )
}


# ===========================================================================
# Dynamic prior-injection test (the proof)
# ===========================================================================

test_that("box-feature regression on the prior-free target is unaffected by prior contamination, but the contaminated posterior leaks it", {
  skip_if_not_installed("glmnet")

  rapm_data <- .circularity_test_rapm_data()
  player_ids <- rapm_data$player_ids
  lambda_fixed <- 0.3

  # A random cohort with a distinguishing synthetic box-score feature.
  cohort_ids <- player_ids[1:40]
  box_features <- data.frame(
    player_id = player_ids,
    cohort_dummy = as.numeric(player_ids %in% cohort_ids)
  )

  regress_cohort_beta <- function(ratings, value_col) {
    reg_data <- merge(box_features, ratings[, c("player_id", value_col)], by = "player_id")
    unname(stats::coef(stats::lm(stats::reformulate("cohort_dummy", response = value_col),
                                 data = reg_data))["cohort_dummy"])
  }

  # --- a. Fit prior-free target A, regress box features -> beta_A ---
  fit_A_before <- fit_rapm(rapm_data, alpha = 0, fixed_lambda = lambda_fixed, parallel = FALSE)
  ratings_A_before <- extract_rapm_ratings(fit_A_before)
  beta_A_before <- regress_cohort_beta(ratings_A_before, "offense")

  # --- b. Corrupt the SPM prior for the cohort (+delta on offense_prior) ---
  offense_prior_base <- stats::setNames(ratings_A_before$offense * 0.6, ratings_A_before$player_id)
  defense_prior_base <- stats::setNames(ratings_A_before$defense * 0.6, ratings_A_before$player_id)
  delta <- 2
  offense_prior_corrupt <- offense_prior_base
  offense_prior_corrupt[cohort_ids] <- offense_prior_corrupt[cohort_ids] + delta

  xrapm_clean <- fit_rapm_with_prior(rapm_data, offense_prior_base, defense_prior_base,
                                     alpha = 0, fixed_lambda = lambda_fixed)
  xrapm_corrupt <- fit_rapm_with_prior(rapm_data, offense_prior_corrupt, defense_prior_base,
                                       alpha = 0, fixed_lambda = lambda_fixed)

  # --- c. Assert: prior-free target regression is bit-identical whether or
  # not the contaminated prior fit ran ---
  fit_A_after <- fit_rapm(rapm_data, alpha = 0, fixed_lambda = lambda_fixed, parallel = FALSE)
  ratings_A_after <- extract_rapm_ratings(fit_A_after)
  beta_A_after <- regress_cohort_beta(ratings_A_after, "offense")

  expect_identical(ratings_A_before$offense, ratings_A_after$offense)
  expect_identical(ratings_A_before$defense, ratings_A_after$defense)
  expect_identical(beta_A_before, beta_A_after)

  # --- Foil: the contaminated POSTERIOR (xRAPM) moves in the delta direction
  # on the cohort's distinguishing feature -- the loop, made visible ---
  ratings_clean <- extract_xrapm_ratings(xrapm_clean)
  ratings_corrupt <- extract_xrapm_ratings(xrapm_corrupt)
  beta_clean <- regress_cohort_beta(ratings_clean, "offense")
  beta_corrupt <- regress_cohort_beta(ratings_corrupt, "offense")

  expect_gt(beta_corrupt, beta_clean)
})


# ===========================================================================
# Static provenance guard: assert_prior_free_target()
# ===========================================================================

test_that("assert_prior_free_target passes a properly stamped 04b vintage element", {
  target <- list(
    ratings = data.frame(player_id = "p1", rapm = 0.1, offense = 0.05, defense = -0.05),
    lambda_min = 0.3,
    n_obs = 5000,
    window = c(2019, 2024),
    target_provenance = "prior_free_rapm_window"
  )
  expect_true(isTRUE(assert_prior_free_target(target)))
})


test_that("assert_prior_free_target passes the top-level 04b list (attr-stamped, as 04b writes it)", {
  vintage <- list(
    ratings = data.frame(player_id = "p1", rapm = 0.1, offense = 0.05, defense = -0.05),
    lambda_min = 0.3,
    n_obs = 5000,
    window = c(2019, 2024),
    target_provenance = "prior_free_rapm_window"
  )
  top_level <- list("2024" = vintage)
  attr(top_level, "target_provenance") <- "prior_free_rapm_window"
  expect_true(isTRUE(assert_prior_free_target(top_level)))
})


test_that("assert_prior_free_target passes a legacy fit_rapm() model (type rapm, no used_prior)", {
  target <- list(panna_metadata = list(type = "rapm", mode = "od"))
  expect_true(isTRUE(assert_prior_free_target(target)))
})


test_that("assert_prior_free_target aborts on a hand-built ratings object stamped type xrapm", {
  target <- list(panna_metadata = list(type = "xrapm", mode = "od"))
  expect_error(assert_prior_free_target(target), class = "rlang_error")
})


test_that("assert_prior_free_target aborts on type xrapm_net (net-mode xRAPM)", {
  target <- list(panna_metadata = list(type = "xrapm_net", mode = "net"))
  expect_error(assert_prior_free_target(target), class = "rlang_error")
})


test_that("assert_prior_free_target aborts on an unstamped object", {
  target <- list(ratings = data.frame(player_id = "p1", rapm = 0.1))
  expect_error(assert_prior_free_target(target), class = "rlang_error")
})


test_that("assert_prior_free_target aborts on type rapm with used_prior set (contradictory metadata)", {
  target <- list(panna_metadata = list(type = "rapm", used_prior = TRUE))
  expect_error(assert_prior_free_target(target), class = "rlang_error")
})


test_that("assert_prior_free_target rejects a real fit_rapm_with_prior() model and accepts a real fit_rapm() model", {
  skip_if_not_installed("glmnet")

  rapm_data <- .circularity_test_rapm_data(n_players = 20, n_splints = 60, seed = 11)
  prior <- stats::setNames(rep(0, 20), rapm_data$player_ids)

  model_rapm <- fit_rapm(rapm_data, alpha = 0, fixed_lambda = 0.3, parallel = FALSE)
  model_xrapm <- fit_rapm_with_prior(rapm_data, prior, prior, alpha = 0, fixed_lambda = 0.3)

  expect_true(isTRUE(assert_prior_free_target(model_rapm)))
  expect_error(assert_prior_free_target(model_xrapm), class = "rlang_error")
})
