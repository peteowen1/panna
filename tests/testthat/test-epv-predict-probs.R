test_that("predict_epv_probs() matches the old reshape = TRUE output without the deprecation warning", {
  skip_if_not_installed("xgboost")
  set.seed(42)
  n <- 300
  X <- cbind(a = stats::runif(n), b = stats::runif(n))
  # Classes depend on the features, so each column of the probability matrix
  # differs and a column swap or row-major/column-major mixup would show.
  y <- ifelse(X[, "a"] > 0.66, 0L, ifelse(X[, "b"] > 0.5, 1L, 2L))
  bst <- xgboost::xgb.train(
    params = list(objective = "multi:softprob", num_class = 3, max_depth = 2,
                  nthread = 1),
    data = xgboost::xgb.DMatrix(X, label = y),
    nrounds = 5, verbose = 0
  )
  model <- list(model = bst, method = "goal",
                panna_metadata = list(feature_cols = c("a", "b")))
  features <- as.data.frame(X[1:20, ])

  expect_no_warning(out <- predict_epv_probs(model, features))
  expect_named(out, c("p_team_scores", "p_opponent_scores", "p_nobody_scores"))
  expect_equal(nrow(out), 20L)
  expect_equal(rowSums(out), rep(1, 20), tolerance = 1e-6)

  # The pre-change call. Skipped once xgboost turns the deprecation into an
  # error, at which point there is nothing left to compare against.
  old <- tryCatch(
    suppressWarnings(stats::predict(bst, X[1:20, ], reshape = TRUE)),
    error = function(e) NULL
  )
  skip_if(is.null(old), "xgboost no longer accepts reshape = TRUE")
  expect_equal(unname(as.matrix(out)), unname(old), tolerance = 1e-7)
})
