# Train + predict for the two-stage minutes-projection model.
#
# Stage 1: P(plays | in_lineup) — XGBoost binary classifier
# Stage 2: E[minutes | plays]   — XGBoost regression
# Combined: E[mins] = P(plays) * E[mins | plays]

#' Fit the two-stage minutes model
#'
#' @param train Data.frame/data.table with features + `minutes_played` target.
#' @param feature_cols Character vector of feature column names.
#' @param train_idx Logical vector identifying training rows (the rest are held out).
#' @param nrounds Max XGBoost rounds. Default 600 for stage1, 800 for stage2.
#' @param early_stopping Rounds without val improvement before stopping. Default 30.
#' @param verbose Logical.
#' @return List with `play_clf`, `mins_reg`, `feature_cols`, `eval`.
#' @export
fit_minutes_model <- function(train, feature_cols, train_idx,
                                nrounds_clf = 600L, nrounds_reg = 800L,
                                early_stopping = 30L,
                                verbose = TRUE) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost package is required")
  }
  train <- as.data.frame(train)

  X <- as.matrix(train[, feature_cols, drop = FALSE])
  X[is.na(X)] <- 0
  y_min   <- train$minutes_played
  y_plays <- as.integer(y_min > 0L)

  ## Split
  X_tr  <- X[train_idx, , drop = FALSE];   X_te  <- X[!train_idx, , drop = FALSE]
  ymin_tr <- y_min[train_idx];             ymin_te <- y_min[!train_idx]
  yp_tr  <- y_plays[train_idx];            yp_te  <- y_plays[!train_idx]

  ## --- STAGE 1: P(plays) classifier --------------------------------------
  if (verbose) cli::cli_alert_info("Stage 1: P(plays) classifier — {nrow(X_tr)} train, {nrow(X_te)} val")
  d_tr <- xgboost::xgb.DMatrix(data = X_tr, label = yp_tr)
  d_te <- xgboost::xgb.DMatrix(data = X_te, label = yp_te)
  clf <- xgboost::xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 6, eta = 0.05, subsample = 0.8,
      colsample_bytree = 0.8, min_child_weight = 5
    ),
    data = d_tr,
    nrounds = nrounds_clf,
    evals = list(train = d_tr, val = d_te),
    early_stopping_rounds = early_stopping,
    verbose = 0L
  )
  if (verbose) message(sprintf("  clf best iter: %d (val logloss %.4f)",
                                clf$best_iteration, clf$best_score))

  ## --- STAGE 2: minutes regression conditional on plays -----------------
  if (verbose) cli::cli_alert_info("Stage 2: E[mins | plays] regression")
  tr_played <- which(yp_tr == 1L)
  te_played <- which(yp_te == 1L)
  d_tr2 <- xgboost::xgb.DMatrix(data = X_tr[tr_played, , drop = FALSE],
                                  label = ymin_tr[tr_played])
  d_te2 <- xgboost::xgb.DMatrix(data = X_te[te_played, , drop = FALSE],
                                  label = ymin_te[te_played])
  reg <- xgboost::xgb.train(
    params = list(
      objective = "reg:squarederror",
      eval_metric = "mae",
      max_depth = 6, eta = 0.05, subsample = 0.8,
      colsample_bytree = 0.8, min_child_weight = 10
    ),
    data = d_tr2,
    nrounds = nrounds_reg,
    evals = list(train = d_tr2, val = d_te2),
    early_stopping_rounds = early_stopping,
    verbose = 0L
  )
  if (verbose) message(sprintf("  reg best iter: %d (val MAE %.2f)",
                                reg$best_iteration, reg$best_score))

  ## --- Evaluate combined predictions on held-out set --------------------
  p_play_te <- stats::predict(clf, xgboost::xgb.DMatrix(data = X_te))
  e_mins_played_te <- stats::predict(reg, xgboost::xgb.DMatrix(data = X_te))
  e_mins_te <- p_play_te * e_mins_played_te

  mae  <- mean(abs(ymin_te - e_mins_te))
  rmse <- sqrt(mean((ymin_te - e_mins_te)^2))

  ## Baseline for comparison: "predict mean" and "predict by role"
  baseline_mean <- rep(mean(ymin_tr), length(ymin_te))
  mae_mean  <- mean(abs(ymin_te - baseline_mean))
  rmse_mean <- sqrt(mean((ymin_te - baseline_mean)^2))

  if (verbose) {
    cli::cli_alert_success("Test MAE:  {round(mae,  2)} mins  (baseline=mean: {round(mae_mean,  2)})")
    cli::cli_alert_success("Test RMSE: {round(rmse, 2)} mins  (baseline=mean: {round(rmse_mean, 2)})")
  }

  list(
    play_clf   = clf,
    mins_reg   = reg,
    feature_cols = feature_cols,
    eval = list(mae = mae, rmse = rmse,
                mae_baseline = mae_mean, rmse_baseline = rmse_mean,
                clf_best_iter = clf$best_iteration,
                reg_best_iter = reg$best_iteration),
    test_pred = data.frame(
      actual    = ymin_te,
      predicted = round(e_mins_te, 1),
      p_play    = round(p_play_te, 3),
      e_mins_played = round(e_mins_played_te, 1)
    )
  )
}

#' Predict minutes for new rows (must have same feature columns as training)
#'
#' @param model Output of `fit_minutes_model()`.
#' @param newdata Data.frame with feature columns.
#' @return Numeric vector of expected minutes per row.
#' @export
predict_minutes <- function(model, newdata) {
  X <- as.matrix(as.data.frame(newdata)[, model$feature_cols, drop = FALSE])
  X[is.na(X)] <- 0
  d <- xgboost::xgb.DMatrix(data = X)
  p_play <- stats::predict(model$play_clf, d)
  e_mins_played <- stats::predict(model$mins_reg, d)
  p_play * e_mins_played
}
