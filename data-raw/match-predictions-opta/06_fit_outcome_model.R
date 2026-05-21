# 06_fit_outcome_model.R
# Fit XGBoost multinomial models for match outcome (H/D/A)
#
# Trains a POOLED outcome model (all competitions) and an INTERNATIONAL
# outcome model, each on top of that segment's goals models from step 05.
# Out-of-fold goal predictions avoid leakage; training rows are mirrored
# (orientation-symmetric) before fitting.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "06_outcome_model.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 06_outcome_model.rds")
  outcome_result <- readRDS(output_path)
  for (seg in outcome_result$segments) {
    message(sprintf("  [%s] outcome model: %d rounds", seg,
                    outcome_result[[seg]]$model$best_nrounds))
  }
  return(invisible(NULL))
}

# 4. Load Data and Goals Models ----

message("\n=== Fitting Outcome Models (pooled + international) ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models  <- readRDS(file.path(cache_dir, "05_goals_model.rds"))

feature_cols <- goals_models$feature_cols
augmented_features <- c(feature_cols, "pred_home_goals", "pred_away_goals",
                        "pred_goal_diff", "pred_total_goals")

# Goal-model params for the out-of-fold refit (match fit_goals_xgb defaults).
oof_goal_params <- list(objective = "count:poisson",
                        eval_metric = "poisson-nloglik",
                        max_depth = 5L, eta = 0.05, subsample = 0.8,
                        colsample_bytree = 0.8, min_child_weight = 10)

# 5. Segment-fitting helper ----

fit_outcome_segment <- function(seg_name, train_df, val_df, goals_seg) {
  message(sprintf("\n--- Segment: %s ---", seg_name))

  # Out-of-fold goal predictions on the ORIGINAL (un-mirrored) training rows,
  # so a match and its mirror later share the same OOF preds (no leakage).
  X_tr <- as.matrix(train_df[, feature_cols, drop = FALSE]); X_tr[is.na(X_tr)] <- 0
  set.seed(42)
  folds <- sample(rep(seq_len(5L), length.out = nrow(train_df)))
  oof_h <- numeric(nrow(train_df))
  oof_a <- numeric(nrow(train_df))
  for (f in seq_len(5L)) {
    iv <- folds == f; it <- !iv
    dva <- xgboost::xgb.DMatrix(X_tr[iv, , drop = FALSE])
    mh <- xgboost::xgb.train(oof_goal_params,
                             xgboost::xgb.DMatrix(X_tr[it, , drop = FALSE],
                                                  label = train_df$home_goals[it]),
                             nrounds = goals_seg$home$best_nrounds, verbose = 0L)
    oof_h[iv] <- stats::predict(mh, dva)
    ma <- xgboost::xgb.train(oof_goal_params,
                             xgboost::xgb.DMatrix(X_tr[it, , drop = FALSE],
                                                  label = train_df$away_goals[it]),
                             nrounds = goals_seg$away$best_nrounds, verbose = 0L)
    oof_a[iv] <- stats::predict(ma, dva)
  }
  train_df$pred_home_goals  <- oof_h
  train_df$pred_away_goals  <- oof_a
  train_df$pred_goal_diff   <- oof_h - oof_a
  train_df$pred_total_goals <- oof_h + oof_a

  # Mirror AFTER attaching OOF preds (orientation-symmetric training).
  n_orig <- nrow(train_df)
  train_df <- rbind(train_df, mirror_match_rows(train_df))
  message(sprintf("  Train: %d (%d original + %d mirrored) | Val: %d",
                  nrow(train_df), n_orig, n_orig, nrow(val_df)))

  # Val goal predictions from the full segment goals models (out-of-sample).
  X_va <- as.matrix(val_df[, feature_cols, drop = FALSE]); X_va[is.na(X_va)] <- 0
  dva_full <- xgboost::xgb.DMatrix(X_va)
  val_df$pred_home_goals  <- stats::predict(goals_seg$home$model, dva_full)
  val_df$pred_away_goals  <- stats::predict(goals_seg$away$model, dva_full)
  val_df$pred_goal_diff   <- val_df$pred_home_goals - val_df$pred_away_goals
  val_df$pred_total_goals <- val_df$pred_home_goals + val_df$pred_away_goals

  X_train <- as.matrix(train_df[, augmented_features, drop = FALSE])
  X_val   <- as.matrix(val_df[, augmented_features, drop = FALSE])
  X_train[is.na(X_train)] <- 0
  X_val[is.na(X_val)]     <- 0

  outcome_model <- fit_outcome_xgb(X = X_train, y = train_df$outcome_label,
                                   nfolds = 5L, nrounds = 500L,
                                   early_stopping = 30L, verbose = 0L)

  vp_raw <- stats::predict(outcome_model$model, xgboost::xgb.DMatrix(X_val))
  vp <- if (is.matrix(vp_raw)) vp_raw else matrix(vp_raw, ncol = 3, byrow = TRUE)
  ll  <- compute_multiclass_logloss(val_df$outcome_label, vp)
  acc <- mean(apply(vp, 1, which.max) - 1L == val_df$outcome_label)
  message(sprintf("  Outcome model: %d rounds | val logloss=%.4f, accuracy=%.1f%%",
                  outcome_model$best_nrounds, ll, 100 * acc))

  list(model = outcome_model,
       val_metrics = list(logloss = ll, accuracy = acc, n_val = nrow(val_df)))
}

# 6. Fit each segment ----

played <- match_dataset[match_dataset$match_status == "Played" &
                          !is.na(match_dataset$outcome_label), ]
train_all <- as.data.frame(played[played$split == "train", ])
val_all   <- as.data.frame(played[played$split == "val", ])
is_intl_train <- match_is_international(train_all$league)
is_intl_val   <- match_is_international(val_all$league)

outcome_result <- list(
  pooled = fit_outcome_segment("pooled", train_all, val_all,
                               goals_models$pooled),
  international = fit_outcome_segment("international",
                                     train_all[is_intl_train, ],
                                     val_all[is_intl_val, ],
                                     goals_models$international),
  augmented_features = augmented_features,
  segments = c("pooled", "international")
)

# 7. Save ----

saveRDS(outcome_result, output_path)

# 8. Summary ----

message("\n========================================")
message("Outcome models complete (pooled + international)!")
message("========================================")
for (seg in outcome_result$segments) {
  vm <- outcome_result[[seg]]$val_metrics
  message(sprintf("  [%-13s] val logloss=%.4f, accuracy=%.1f%% (n_val=%d)",
                  seg, vm$logloss, 100 * vm$accuracy, vm$n_val))
}
message(sprintf("\nSaved to: %s", output_path))
