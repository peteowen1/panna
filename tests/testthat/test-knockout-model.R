# Tests for the full-model knockout matchup lookup (R/knockout_model.R)

# A tiny goals + outcome model pair whose predictions differ sharply between
# "feature is NA" and "feature is 0". That gap is what makes the train/serve
# NA-handling observable: if .ko_predict() ever re-introduces an
# `X[is.na(X)] <- 0`, these tests fail.
ko_fixture <- function() {
  skip_if_not_installed("xgboost")
  set.seed(7)
  n <- 900L
  # f1: a third 0, a third 5, a third NA. Label tracks the branch, so the
  # learned default direction for NA is far from the value learned for 0.
  f1 <- c(rep(0, n / 3), rep(5, n / 3), rep(NA_real_, n / 3))
  f2 <- stats::runif(n)
  y  <- c(rep(1, n / 3), rep(5, n / 3), rep(20, n / 3))
  X  <- cbind(f1 = f1, f2 = f2)

  fit_reg <- function(target) {
    xgboost::xgb.train(
      params = list(objective = "reg:squarederror", max_depth = 3L, eta = 0.3),
      data = xgboost::xgb.DMatrix(data = X, label = target),
      nrounds = 30L, verbose = 0L
    )
  }
  goals_models <- list(home = list(model = fit_reg(y)),
                       away = list(model = fit_reg(y / 2)))

  aug <- c("f1", "f2", "pred_home_goals", "pred_away_goals",
           "pred_goal_diff", "pred_total_goals")
  hg <- stats::predict(goals_models$home$model, xgboost::xgb.DMatrix(X))
  ag <- stats::predict(goals_models$away$model, xgboost::xgb.DMatrix(X))
  Xa <- cbind(X, pred_home_goals = hg, pred_away_goals = ag,
              pred_goal_diff = hg - ag, pred_total_goals = hg + ag)[, aug]
  cls <- as.integer(y > 10)                       # 0/1, plus a sprinkle of 2
  cls[seq(1, n, 37)] <- 2L
  outcome_result <- list(
    model = list(model = xgboost::xgb.train(
      params = list(objective = "multi:softprob", num_class = 3L,
                    max_depth = 3L, eta = 0.3),
      data = xgboost::xgb.DMatrix(data = Xa, label = cls),
      nrounds = 30L, verbose = 0L)),
    augmented_features = aug
  )
  list(goals_models = goals_models, outcome_result = outcome_result, aug = aug)
}

test_that(".ko_predict zero-imputes NA, matching how the models are trained", {
  # The production models are fitted on zero-imputed matrices (05_fit_goals_
  # model.R:68, 06_fit_outcome_model.R:55,95), so XGBoost has no learned
  # default direction for missing values. Every serving path must impute the
  # same way. This test pins that: NA and 0 must predict IDENTICALLY. It fails
  # if someone "fixes" .ko_predict to pass raw NAs through -- which reads like
  # an improvement and is actually a train/serve break.
  fx <- ko_fixture()
  X_na   <- matrix(c(NA_real_, 0.5), nrow = 1, dimnames = list(NULL, c("f1", "f2")))
  X_zero <- matrix(c(0,        0.5), nrow = 1, dimnames = list(NULL, c("f1", "f2")))

  got_na   <- panna:::.ko_predict(X_na,   fx$goals_models, fx$outcome_result, fx$aug)
  got_zero <- panna:::.ko_predict(X_zero, fx$goals_models, fx$outcome_result, fx$aug)

  expect_equal(got_na$hg, got_zero$hg)
  expect_equal(got_na$pH, got_zero$pH)
  expect_equal(got_na$pD, got_zero$pD)

  # And the fixture is genuinely capable of telling them apart -- the model
  # learned a distinct branch for NA, so this equality is evidence of the
  # imputation, not of an insensitive fixture.
  raw <- stats::predict(fx$goals_models$home$model,
                        xgboost::xgb.DMatrix(data = X_na))
  expect_gt(abs(raw - got_zero$hg), 5)
})

test_that(".ko_predict aborts when the two models disagree on features", {
  # Previously this gap was zero-filled in silence, so a goals/outcome model
  # vintage mismatch degraded predictions instead of failing.
  fx <- ko_fixture()
  X <- matrix(c(0, 0.5), nrow = 1, dimnames = list(NULL, c("f1", "f2")))
  expect_error(
    panna:::.ko_predict(X, fx$goals_models, fx$outcome_result,
                        c(fx$aug, "a_feature_that_was_never_built")),
    "disagree on the feature set"
  )
})

test_that("softprob_matrix handles both xgboost return shapes", {
  # Row-major flat vector (xgboost < 2.0): two observations, three classes.
  flat <- c(0.7, 0.2, 0.1,
            0.1, 0.3, 0.6)
  expect_equal(panna:::softprob_matrix(flat, 2L),
               matrix(flat, ncol = 3, byrow = TRUE))
  # The bug this replaced: byrow = FALSE reads class 1 of obs 2 as class 2 of
  # obs 1, and rows can STILL sum to 1, so a sum check would not catch it.
  expect_false(identical(matrix(flat, ncol = 3, byrow = FALSE),
                         panna:::softprob_matrix(flat, 2L)))
  # Matrix form (xgboost >= 2.0) passes through untouched.
  m <- matrix(flat, ncol = 3, byrow = TRUE)
  expect_identical(panna:::softprob_matrix(m, 2L), m)
  # Wrong size is a hard error, not a silent reshape.
  expect_error(panna:::softprob_matrix(flat, 5L), "Expected 5 rows")
})

test_that(".ko_predict returns rows of normalised outcome probabilities", {
  # Guards the softprob reshape: a wrong byrow= would mix classes from
  # different matchups into one row, which shows up as rows not summing to 1
  # once the matchups differ from one another.
  fx <- ko_fixture()
  X <- cbind(f1 = c(0, 5, NA, 5, 0), f2 = c(0.1, 0.9, 0.5, 0.2, 0.7))
  got <- panna:::.ko_predict(X, fx$goals_models, fx$outcome_result, fx$aug)

  expect_length(got$pH, nrow(X))
  expect_equal(got$pH + got$pD + got$pA, rep(1, nrow(X)), tolerance = 1e-6)
  expect_true(all(got$pH >= 0 & got$pH <= 1))
  expect_length(got$hg, nrow(X))
})
