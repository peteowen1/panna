# 06_fit_outcome_model.R
# Fit XGBoost multinomial model for match outcome (H/D/A)
#
# Uses all features plus predicted goals from Step 5 as inputs.
# Predicts P(Home Win), P(Draw), P(Away Win).

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
  message(sprintf("  Outcome model: %d rounds", outcome_result$model$best_nrounds))
  return(invisible(NULL))
}

# 4. Load Data and Goals Models ----

message("\n=== Fitting Outcome Model ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models <- readRDS(file.path(cache_dir, "05_goals_model.rds"))

feature_cols <- goals_models$feature_cols

# 5. Generate Predicted Goals ----

played <- match_dataset[match_dataset$match_status == "Played" &
                        !is.na(match_dataset$outcome_label), ]

train_data <- played[played$split == "train", ]
val_data <- played[played$split == "val", ]

# Out-of-fold predictions for training data to avoid leakage.
# In-sample goal predictions are too accurate, causing the outcome model
# to be overconfident. OOF predictions add realistic noise.
message("  Generating out-of-fold goal predictions for training data...")

nfolds_oof <- 5L
X_train_raw <- as.matrix(train_data[, feature_cols, drop = FALSE])
X_train_raw[is.na(X_train_raw)] <- 0

set.seed(42)
fold_ids <- sample(rep(seq_len(nfolds_oof), length.out = nrow(train_data)))

oof_home <- numeric(nrow(train_data))
oof_away <- numeric(nrow(train_data))

home_params <- list(objective = "count:poisson", eval_metric = "poisson-nloglik",
                    max_depth = 4L, eta = 0.05, subsample = 0.8,
                    colsample_bytree = 0.8, min_child_weight = 5)
away_params <- home_params

for (fold in seq_len(nfolds_oof)) {
  idx_val <- fold_ids == fold
  idx_trn <- !idx_val

  d_trn <- xgboost::xgb.DMatrix(data = X_train_raw[idx_trn, , drop = FALSE],
                                  label = train_data$home_goals[idx_trn])
  d_val_fold <- xgboost::xgb.DMatrix(data = X_train_raw[idx_val, , drop = FALSE])

  m_home <- xgboost::xgb.train(params = home_params, data = d_trn,
                                 nrounds = goals_models$home$best_nrounds, verbose = 0L)
  oof_home[idx_val] <- stats::predict(m_home, d_val_fold)

  d_trn_a <- xgboost::xgb.DMatrix(data = X_train_raw[idx_trn, , drop = FALSE],
                                    label = train_data$away_goals[idx_trn])
  m_away <- xgboost::xgb.train(params = away_params, data = d_trn_a,
                                 nrounds = goals_models$away$best_nrounds, verbose = 0L)
  oof_away[idx_val] <- stats::predict(m_away, d_val_fold)

  message(sprintf("    Fold %d: %d train, %d val", fold, sum(idx_trn), sum(idx_val)))
}

train_data$pred_home_goals <- oof_home
train_data$pred_away_goals <- oof_away
train_data$pred_goal_diff <- oof_home - oof_away
train_data$pred_total_goals <- oof_home + oof_away

# Val/test: use the full goals model (out-of-sample by design)
message("  Generating goal predictions for val data...")
X_val_raw <- as.matrix(val_data[, feature_cols, drop = FALSE])
X_val_raw[is.na(X_val_raw)] <- 0
d_val <- xgboost::xgb.DMatrix(data = X_val_raw)
val_data$pred_home_goals <- stats::predict(goals_models$home$model, d_val)
val_data$pred_away_goals <- stats::predict(goals_models$away$model, d_val)
val_data$pred_goal_diff <- val_data$pred_home_goals - val_data$pred_away_goals
val_data$pred_total_goals <- val_data$pred_home_goals + val_data$pred_away_goals

augmented_features <- c(feature_cols, "pred_home_goals", "pred_away_goals",
                        "pred_goal_diff", "pred_total_goals")

# 6. Prepare Matrices ----

X_train <- as.matrix(train_data[, augmented_features, drop = FALSE])
X_val <- as.matrix(val_data[, augmented_features, drop = FALSE])
X_train[is.na(X_train)] <- 0
X_val[is.na(X_val)] <- 0

y_train <- train_data$outcome_label
y_val <- val_data$outcome_label

message(sprintf("  Train: %d matches", nrow(train_data)))
message(sprintf("  Val: %d matches", nrow(val_data)))

# 7. Fit Outcome Model ----

message("\n--- Outcome Model (multi:softprob) ---")

outcome_model <- fit_outcome_xgb(
  X = X_train,
  y = y_train,
  nfolds = 5L,
  nrounds = 500L,
  early_stopping = 30L,
  verbose = 1L
)

# 8. Evaluate on Validation ----

message("\n--- Validation Metrics ---")

val_probs_raw <- stats::predict(outcome_model$model,
                                 xgboost::xgb.DMatrix(data = X_val))
val_probs <- matrix(val_probs_raw, ncol = 3, byrow = FALSE)

# Multi-class log loss
val_logloss <- compute_multiclass_logloss(y_val, val_probs)
message(sprintf("  Validation log loss: %.4f", val_logloss))

# Accuracy
val_pred_class <- apply(val_probs, 1, which.max) - 1L
val_accuracy <- mean(val_pred_class == y_val)
message(sprintf("  Validation accuracy: %.1f%%", 100 * val_accuracy))

# Brier score
val_brier <- mean(rowSums((val_probs - model.matrix(~ factor(y_val, levels = 0:2) - 1))^2))
message(sprintf("  Validation Brier score: %.4f", val_brier))

# Baseline: naive class proportions
train_dist <- table(y_train) / length(y_train)
naive_probs <- matrix(rep(as.numeric(train_dist), each = length(y_val)),
                      ncol = 3)
naive_logloss <- compute_multiclass_logloss(y_val, naive_probs)
naive_accuracy <- mean(which.max(train_dist) - 1L == y_val)
message(sprintf("  Baseline log loss: %.4f", naive_logloss))
message(sprintf("  Baseline accuracy: %.1f%%", 100 * naive_accuracy))

# Calibration
cal_table <- calibration_table(y_val, val_probs)

# 9. Save ----

outcome_result <- list(
  model = outcome_model,
  augmented_features = augmented_features,
  val_metrics = list(
    logloss = val_logloss,
    accuracy = val_accuracy,
    brier = val_brier,
    baseline_logloss = naive_logloss,
    baseline_accuracy = naive_accuracy,
    calibration = cal_table
  )
)

saveRDS(outcome_result, output_path)

# 10. Summary ----

message("\n========================================")
message("Outcome model complete!")
message("========================================")
message(sprintf("Model: %d rounds", outcome_model$best_nrounds))
message(sprintf("Val log loss: %.4f (baseline %.4f)", val_logloss, naive_logloss))
message(sprintf("Val accuracy: %.1f%% (baseline %.1f%%)",
                100 * val_accuracy, 100 * naive_accuracy))
message(sprintf("\nTop 5 features:"))
if (nrow(outcome_model$importance) > 0) {
  top5 <- head(outcome_model$importance, 5)
  for (i in seq_len(nrow(top5))) {
    message(sprintf("  %s: %.4f", top5$Feature[i], top5$Gain[i]))
  }
}
message(sprintf("\nSaved to: %s", output_path))
