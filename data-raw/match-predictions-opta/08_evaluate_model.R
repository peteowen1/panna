# 08_evaluate_model.R
# Evaluate match prediction models on held-out test set
#
# Computes goals metrics (MAE, RMSE), outcome metrics (log-loss, accuracy,
# Brier score), calibration tables, and feature importance analysis.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "08_evaluation.rds")

# 3. Load Models and Data ----

message("\n=== Model Evaluation ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models <- readRDS(file.path(cache_dir, "05_goals_model.rds"))
outcome_result <- readRDS(file.path(cache_dir, "06_outcome_model.rds"))

feature_cols <- goals_models$feature_cols
augmented_features <- outcome_result$augmented_features

# 4. Prepare Test Data ----

test_data <- match_dataset[match_dataset$split == "test" &
                           !is.na(match_dataset$outcome_label), ]

if (nrow(test_data) == 0) {
  message("No test data available.")
  saveRDS(list(), output_path)
  return(invisible(NULL))
}

message(sprintf("  Test set: %d matches", nrow(test_data)))

X_test <- as.matrix(test_data[, feature_cols, drop = FALSE])
X_test[is.na(X_test)] <- 0

# 5. Goals Model Evaluation ----

message("\n--- Goals Model Evaluation ---\n")

d_test <- xgboost::xgb.DMatrix(data = X_test)
pred_home <- stats::predict(goals_models$home$model, d_test)
pred_away <- stats::predict(goals_models$away$model, d_test)

# MAE and RMSE
home_mae <- mean(abs(test_data$home_goals - pred_home))
home_rmse <- sqrt(mean((test_data$home_goals - pred_home)^2))
away_mae <- mean(abs(test_data$away_goals - pred_away))
away_rmse <- sqrt(mean((test_data$away_goals - pred_away)^2))

message(sprintf("  Home goals - MAE: %.3f, RMSE: %.3f", home_mae, home_rmse))
message(sprintf("  Away goals - MAE: %.3f, RMSE: %.3f", away_mae, away_rmse))

# Baseline: training set average
train_data <- match_dataset[match_dataset$split == "train" &
                            !is.na(match_dataset$home_goals), ]
avg_home <- mean(train_data$home_goals)
avg_away <- mean(train_data$away_goals)
base_home_mae <- mean(abs(test_data$home_goals - avg_home))
base_away_mae <- mean(abs(test_data$away_goals - avg_away))

message(sprintf("  Baseline Home MAE: %.3f, Away MAE: %.3f", base_home_mae, base_away_mae))
message(sprintf("  Improvement: Home %.1f%%, Away %.1f%%",
                100 * (1 - home_mae / base_home_mae),
                100 * (1 - away_mae / base_away_mae)))

# 6. Outcome Model Evaluation ----

message("\n--- Outcome Model Evaluation ---\n")

# Generate predicted goals for test set
test_data$pred_home_goals <- pred_home
test_data$pred_away_goals <- pred_away
test_data$pred_goal_diff <- pred_home - pred_away
test_data$pred_total_goals <- pred_home + pred_away

X_test_aug <- as.matrix(test_data[, augmented_features, drop = FALSE])
X_test_aug[is.na(X_test_aug)] <- 0

d_test_aug <- xgboost::xgb.DMatrix(data = X_test_aug)
test_probs_raw <- stats::predict(outcome_result$model$model, d_test_aug)
test_probs <- matrix(test_probs_raw, ncol = 3, byrow = TRUE)

y_test <- test_data$outcome_label

# Multi-class log loss
test_logloss <- compute_multiclass_logloss(y_test, test_probs)
message(sprintf("  Test log loss: %.4f", test_logloss))

# Accuracy
test_pred_class <- apply(test_probs, 1, which.max) - 1L
test_accuracy <- mean(test_pred_class == y_test)
message(sprintf("  Test accuracy: %.1f%%", 100 * test_accuracy))

# Brier score
y_onehot <- matrix(0, nrow = length(y_test), ncol = 3)
for (i in seq_along(y_test)) y_onehot[i, y_test[i] + 1L] <- 1
test_brier <- mean(rowSums((test_probs - y_onehot)^2))
message(sprintf("  Test Brier score: %.4f", test_brier))

# Baselines
train_dist <- table(train_data$outcome_label[!is.na(train_data$outcome_label)]) /
  sum(!is.na(train_data$outcome_label))
naive_probs <- matrix(rep(as.numeric(train_dist), each = length(y_test)), ncol = 3)
naive_logloss <- compute_multiclass_logloss(y_test, naive_probs)
naive_accuracy <- mean(which.max(train_dist) - 1L == y_test)

message(sprintf("  Baseline log loss: %.4f (improvement: %.1f%%)",
                naive_logloss, 100 * (1 - test_logloss / naive_logloss)))
message(sprintf("  Baseline accuracy: %.1f%%", 100 * naive_accuracy))

# Elo-only baseline
elo_home_prob <- 1 / (1 + 10^(-test_data$elo_diff / 400))
elo_probs <- cbind(elo_home_prob * 0.72, rep(0.26, length(elo_home_prob)),
                   (1 - elo_home_prob) * 0.72)
elo_probs <- elo_probs / rowSums(elo_probs)  # Normalize
elo_logloss <- compute_multiclass_logloss(y_test, elo_probs)
message(sprintf("  Elo-only log loss: %.4f", elo_logloss))

# 7. Calibration ----

message("\n--- Calibration ---\n")

cal_table <- calibration_table(y_test, test_probs, n_bins = 5L)
cal_df <- as.data.frame(cal_table)

for (out in c("Home", "Draw", "Away")) {
  cal_out <- cal_df[cal_df$outcome == out, ]
  message(sprintf("  %s calibration:", out))
  for (j in seq_len(nrow(cal_out))) {
    message(sprintf("    Bin %s: pred=%.3f, actual=%.3f, n=%d",
                    cal_out$bin[j], cal_out$pred_mean[j],
                    cal_out$actual_mean[j], cal_out$n[j]))
  }
}

# 8. Feature Importance ----

message("\n--- Top Features ---\n")

message("Goals model (home):")
home_imp <- head(goals_models$home$importance, 10)
for (i in seq_len(nrow(home_imp))) {
  message(sprintf("  %2d. %-35s %.4f", i, home_imp$Feature[i], home_imp$Gain[i]))
}

message("\nOutcome model:")
out_imp <- head(outcome_result$model$importance, 10)
for (i in seq_len(nrow(out_imp))) {
  message(sprintf("  %2d. %-35s %.4f", i, out_imp$Feature[i], out_imp$Gain[i]))
}

# 9. Per-League Breakdown ----

message("\n--- Per-League Test Accuracy ---\n")

for (lg in sort(unique(test_data$league))) {
  lg_idx <- test_data$league == lg
  lg_n <- sum(lg_idx)
  if (lg_n < 10) next
  lg_acc <- mean(test_pred_class[lg_idx] == y_test[lg_idx])
  lg_ll <- compute_multiclass_logloss(y_test[lg_idx], test_probs[lg_idx, , drop = FALSE])
  message(sprintf("  %-20s n=%4d  acc=%.1f%%  logloss=%.4f", lg, lg_n, 100 * lg_acc, lg_ll))
}

# 10. Save ----

evaluation <- list(
  goals = list(
    home_mae = home_mae, home_rmse = home_rmse,
    away_mae = away_mae, away_rmse = away_rmse,
    baseline_home_mae = base_home_mae, baseline_away_mae = base_away_mae
  ),
  outcome = list(
    test_logloss = test_logloss, test_accuracy = test_accuracy,
    test_brier = test_brier,
    naive_logloss = naive_logloss, naive_accuracy = naive_accuracy,
    elo_logloss = elo_logloss,
    calibration = cal_table
  ),
  n_test = nrow(test_data),
  test_probs = test_probs,
  test_labels = y_test
)

saveRDS(evaluation, output_path)

# 11. Summary ----

message("\n========================================")
message("Evaluation complete!")
message("========================================")
message(sprintf("Test set: %d matches", nrow(test_data)))
message(sprintf("Goals: Home MAE=%.3f (baseline %.3f), Away MAE=%.3f (baseline %.3f)",
                home_mae, base_home_mae, away_mae, base_away_mae))
message(sprintf("Outcome: LogLoss=%.4f (naive %.4f, Elo %.4f)",
                test_logloss, naive_logloss, elo_logloss))
message(sprintf("Accuracy: %.1f%% (baseline %.1f%%)",
                100 * test_accuracy, 100 * naive_accuracy))
message(sprintf("\nSaved to: %s", output_path))
