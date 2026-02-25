# 07_predict_fixtures.R
# Predict upcoming match outcomes using fitted models
#
# Loads upcoming fixtures, applies all feature engineering, and runs
# both goals and outcome models to produce predictions.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "07_predictions.rds")

# 3. Load Models and Data ----

message("\n=== Predicting Fixtures ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models <- readRDS(file.path(cache_dir, "05_goals_model.rds"))
outcome_result <- readRDS(file.path(cache_dir, "06_outcome_model.rds"))

feature_cols <- goals_models$feature_cols
augmented_features <- outcome_result$augmented_features

# 4. Filter to Fixtures ----

fixtures <- match_dataset[match_dataset$split == "fixture", ]
message(sprintf("  %d upcoming fixtures to predict", nrow(fixtures)))

if (nrow(fixtures) == 0) {
  message("  No upcoming fixtures found - skipping predictions.")
  saveRDS(data.frame(), output_path)
} else {

  # 5. Prepare Feature Matrix ----

  X_fix <- as.matrix(fixtures[, feature_cols, drop = FALSE])
  X_fix[is.na(X_fix)] <- 0

  # 6. Predict Goals ----

  message("  Predicting goals...")

  d_fix <- xgboost::xgb.DMatrix(data = X_fix)
  pred_home_goals <- stats::predict(goals_models$home$model, d_fix)
  pred_away_goals <- stats::predict(goals_models$away$model, d_fix)

  # 7. Predict Outcomes ----

  message("  Predicting outcomes...")

  # Augment with goal predictions
  goal_features <- cbind(
    pred_home_goals = pred_home_goals,
    pred_away_goals = pred_away_goals,
    pred_goal_diff = pred_home_goals - pred_away_goals,
    pred_total_goals = pred_home_goals + pred_away_goals
  )
  X_outcome <- cbind(X_fix, goal_features)

  # Ensure column alignment with training
  missing_cols <- setdiff(augmented_features, colnames(X_outcome))
  if (length(missing_cols) > 0) {
    filler <- matrix(0, nrow = nrow(X_outcome), ncol = length(missing_cols))
    colnames(filler) <- missing_cols
    X_outcome <- cbind(X_outcome, filler)
  }
  X_outcome <- X_outcome[, augmented_features, drop = FALSE]

  d_outcome <- xgboost::xgb.DMatrix(data = X_outcome)
  probs_raw <- stats::predict(outcome_result$model$model, d_outcome)
  probs <- matrix(probs_raw, ncol = 3, byrow = FALSE)

  # 8. Build Prediction Table ----

  predictions <- data.frame(
    match_id = fixtures$match_id,
    match_date = fixtures$match_date,
    league = fixtures$league,
    season = fixtures$season,
    home_team = fixtures$home_team,
    away_team = fixtures$away_team,
    pred_home_goals = round(pred_home_goals, 2),
    pred_away_goals = round(pred_away_goals, 2),
    prob_H = round(probs[, 1], 3),
    prob_D = round(probs[, 2], 3),
    prob_A = round(probs[, 3], 3),
    predicted_result = c("H", "D", "A")[apply(probs, 1, which.max)],
    stringsAsFactors = FALSE
  )

  predictions <- predictions[order(predictions$match_date, predictions$league), ]

  # 9. Save ----

  saveRDS(predictions, output_path)

  # Also save CSV for easy viewing
  csv_path <- file.path(cache_dir, "predictions.csv")
  write.csv(predictions, csv_path, row.names = FALSE)

  # Save parquet for GitHub release upload
  parquet_path <- file.path(cache_dir, "predictions.parquet")
  arrow::write_parquet(predictions, parquet_path)

  # 10. Summary ----

  message("\n========================================")
  message("Fixture predictions complete!")
  message("========================================")
  message(sprintf("Predictions: %d fixtures", nrow(predictions)))
  message(sprintf("Leagues: %s", paste(unique(predictions$league), collapse = ", ")))

  # Show predictions grouped by league
  for (lg in unique(predictions$league)) {
    lg_preds <- predictions[predictions$league == lg, ]
    message(sprintf("\n%s (%d matches):", lg, nrow(lg_preds)))
    for (i in seq_len(min(nrow(lg_preds), 10))) {
      p <- lg_preds[i, ]
      message(sprintf("  %s: %s vs %s  |  %.0f%% H / %.0f%% D / %.0f%% A  |  %.1f-%.1f",
                      substr(p$match_date, 1, 10),
                      p$home_team, p$away_team,
                      100 * p$prob_H, 100 * p$prob_D, 100 * p$prob_A,
                      p$pred_home_goals, p$pred_away_goals))
    }
    if (nrow(lg_preds) > 10) {
      message(sprintf("  ... and %d more", nrow(lg_preds) - 10))
    }
  }

  message(sprintf("\nSaved to: %s", output_path))
  message(sprintf("CSV: %s", csv_path))
  message(sprintf("Parquet: %s", parquet_path))
}
