# 05_fit_goals_model.R
# Fit XGBoost Poisson models for home and away goal prediction
#
# Two separate models predict expected goals for each side.
# Uses time-based cross-validation with early stopping.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "05_goals_model.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 05_goals_model.rds")
  goals_models <- readRDS(output_path)
  message(sprintf("  Home goals model: %d rounds", goals_models$home$best_nrounds))
  message(sprintf("  Away goals model: %d rounds", goals_models$away$best_nrounds))
  return(invisible(NULL))
}

# 4. Load Data ----

message("\n=== Fitting Goals Models ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))

# 5. Identify Feature Columns ----

# Exclude non-feature columns
exclude_cols <- c("match_id", "match_date", "match_status", "league", "season",
                  "home_team", "away_team", "home_team_id", "away_team_id",
                  "home_goals", "away_goals", "home_xg", "away_xg",
                  "result", "split", "outcome_label", "competition",
                  "home_npxg", "away_npxg")

all_cols <- names(match_dataset)
feature_cols <- setdiff(all_cols[sapply(match_dataset, is.numeric)], exclude_cols)

message(sprintf("  Feature columns: %d", length(feature_cols)))

# 6. Prepare Training Data ----

train_data <- match_dataset[match_dataset$split == "train", ]
val_data <- match_dataset[match_dataset$split == "val", ]

# Remove rows with NA goals
train_data <- train_data[!is.na(train_data$home_goals) & !is.na(train_data$away_goals), ]
val_data <- val_data[!is.na(val_data$home_goals) & !is.na(val_data$away_goals), ]

message(sprintf("  Train: %d matches", nrow(train_data)))
message(sprintf("  Val: %d matches", nrow(val_data)))

X_train <- as.matrix(train_data[, feature_cols, drop = FALSE])
X_val <- as.matrix(val_data[, feature_cols, drop = FALSE])

# Replace remaining NAs with 0
X_train[is.na(X_train)] <- 0
X_val[is.na(X_val)] <- 0

# 7. Fit Home Goals Model ----

message("\n--- Home Goals Model ---")

home_model <- fit_goals_xgb(
  X = X_train,
  y = train_data$home_goals,
  nfolds = 5L,
  nrounds = 500L,
  early_stopping = 30L,
  verbose = 1L
)

# Evaluate on validation set
home_pred_val <- stats::predict(home_model$model,
                                 xgboost::xgb.DMatrix(data = X_val))
home_rmse <- sqrt(mean((val_data$home_goals - home_pred_val)^2))
message(sprintf("  Val RMSE: %.3f", home_rmse))

# 8. Fit Away Goals Model ----

message("\n--- Away Goals Model ---")

away_model <- fit_goals_xgb(
  X = X_train,
  y = train_data$away_goals,
  nfolds = 5L,
  nrounds = 500L,
  early_stopping = 30L,
  verbose = 1L
)

# Evaluate on validation set
away_pred_val <- stats::predict(away_model$model,
                                 xgboost::xgb.DMatrix(data = X_val))
away_rmse <- sqrt(mean((val_data$away_goals - away_pred_val)^2))
message(sprintf("  Val RMSE: %.3f", away_rmse))

# 9. Baseline Comparison ----

message("\n--- Baseline (League Average) ---")
avg_home <- mean(train_data$home_goals)
avg_away <- mean(train_data$away_goals)
base_home_rmse <- sqrt(mean((val_data$home_goals - avg_home)^2))
base_away_rmse <- sqrt(mean((val_data$away_goals - avg_away)^2))
message(sprintf("  Avg home goals: %.2f, Avg away goals: %.2f", avg_home, avg_away))
message(sprintf("  Baseline Home RMSE: %.3f, Away RMSE: %.3f", base_home_rmse, base_away_rmse))

# 10. Save ----

goals_models <- list(
  home = home_model,
  away = away_model,
  feature_cols = feature_cols,
  val_metrics = list(
    home_rmse = home_rmse,
    away_rmse = away_rmse,
    baseline_home_rmse = base_home_rmse,
    baseline_away_rmse = base_away_rmse
  )
)

saveRDS(goals_models, output_path)

# 11. Summary ----

message("\n========================================")
message("Goals models complete!")
message("========================================")
message(sprintf("Home model: %d rounds, Val RMSE=%.3f (baseline %.3f)",
                home_model$best_nrounds, home_rmse, base_home_rmse))
message(sprintf("Away model: %d rounds, Val RMSE=%.3f (baseline %.3f)",
                away_model$best_nrounds, away_rmse, base_away_rmse))
message(sprintf("\nTop 5 features (home goals):"))
if (nrow(home_model$importance) > 0) {
  top5 <- head(home_model$importance, 5)
  for (i in seq_len(nrow(top5))) {
    message(sprintf("  %s: %.4f", top5$Feature[i], top5$Gain[i]))
  }
}
message(sprintf("\nSaved to: %s", output_path))
