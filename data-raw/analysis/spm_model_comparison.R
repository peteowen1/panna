# spm_model_comparison.R
# Compare Elastic Net vs XGBoost for SPM (Statistical Plus-Minus) model
#
# Both models predict RAPM from box score stats.
# This script compares their cross-validation performance for:
#   - Overall RAPM
#   - Offensive RAPM
#   - Defensive RAPM

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# 2. Load Data ----

cat("Loading data...\n")
rapm_data <- readRDS(file.path(cache_dir, "04_rapm.rds"))
rapm_ratings <- rapm_data$ratings
cat(sprintf("RAPM ratings: %d players\n", nrow(rapm_ratings)))

# Load processed stats for feature aggregation
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

# Aggregate player stats to per-90 rates
cat("\nAggregating player stats...\n")
player_features <- aggregate_player_stats(
  stats_summary = processed_data$stats_summary,
  stats_passing = processed_data$stats_passing,
  stats_defense = processed_data$stats_defense,
  stats_possession = processed_data$stats_possession,
  stats_misc = processed_data$stats_misc,
  stats_passing_types = processed_data$stats_passing_types,
  stats_keeper = processed_data$stats_keeper,
  min_minutes = 450
)
cat(sprintf("Player features: %d players, %d features\n",
            nrow(player_features), ncol(player_features)))

# Join RAPM with features for SPM training (by player_name since player_id formats differ)
spm_data <- player_features %>%
  inner_join(
    rapm_ratings %>% select(player_name, rapm, rapm_off = offense, rapm_def = defense),
    by = "player_name"
  )
cat(sprintf("Matched for SPM training: %d players\n\n", nrow(spm_data)))

# Helper function to fit and evaluate models for a given target
fit_and_compare <- function(data, target_col, target_name) {
  # Temporarily rename target column to rapm for fit_spm_model compatibility
  data_temp <- data
  data_temp$rapm <- data_temp[[target_col]]

  cat("\n")
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
  cat(sprintf("%s MODELS\n", toupper(target_name)))
  cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")

  # Elastic Net
  cat("\n--- Elastic Net ---\n")
  model_glmnet <- fit_spm_model(
    data_temp,
    alpha = 0.5,
    nfolds = 10,
    weight_by_minutes = TRUE,
    weight_transform = "sqrt"
  )
  cv_rmse_glmnet <- sqrt(model_glmnet$cvm[model_glmnet$lambda == model_glmnet$lambda.min])

  # XGBoost
  cat("\n--- XGBoost ---\n")
  model_xgb <- fit_spm_xgb(
    data_temp,
    nfolds = 10,
    max_depth = 4,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8,
    nrounds = 500,
    early_stopping_rounds = 20,
    weight_by_minutes = TRUE,
    weight_transform = "sqrt",
    verbose = 0
  )

  # Calculate weighted R² for elastic net
  X <- as.matrix(data_temp[, model_glmnet$panna_metadata$predictor_cols, drop = FALSE])
  y <- data_temp$rapm
  complete_idx <- complete.cases(X, y)
  X <- X[complete_idx, ]
  y <- y[complete_idx]
  weights <- sqrt(data_temp$total_minutes[complete_idx])
  y_pred <- as.vector(predict(model_glmnet, newx = X, s = model_glmnet$lambda.min))

  w <- weights / sum(weights)
  y_mean_w <- sum(w * y)
  ss_res <- sum(weights * (y - y_pred)^2)
  ss_tot <- sum(weights * (y - y_mean_w)^2)
  r2_weighted_glmnet <- 1 - ss_res / ss_tot

  list(
    target = target_name,
    glmnet = list(model = model_glmnet, cv_rmse = cv_rmse_glmnet, r2_weighted = r2_weighted_glmnet),
    xgb = list(model = model_xgb, cv_rmse = model_xgb$best_cv_rmse, r2_weighted = model_xgb$r_squared)
  )
}

# 3. Fit All Models ----

# Fit models for overall RAPM
results_overall <- fit_and_compare(spm_data, "rapm", "Overall RAPM")

# Top features for overall
cat("\nTop 10 features - Overall (Elastic Net):\n")
print(get_spm_feature_importance(results_overall$glmnet$model, n = 10))
cat("\nTop 10 features - Overall (XGBoost):\n")
print(head(results_overall$xgb$model$importance, 10))

# Fit models for offensive RAPM
results_offense <- fit_and_compare(spm_data, "rapm_off", "Offensive RAPM")

cat("\nTop 10 features - Offense (Elastic Net):\n")
print(get_spm_feature_importance(results_offense$glmnet$model, n = 10))
cat("\nTop 10 features - Offense (XGBoost):\n")
print(head(results_offense$xgb$model$importance, 10))

# Fit models for defensive RAPM
results_defense <- fit_and_compare(spm_data, "rapm_def", "Defensive RAPM")

cat("\nTop 10 features - Defense (Elastic Net):\n")
print(get_spm_feature_importance(results_defense$glmnet$model, n = 10))
cat("\nTop 10 features - Defense (XGBoost):\n")
print(head(results_defense$xgb$model$importance, 10))

# 4. Compare Models ----

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("MODEL COMPARISON SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

comparison <- data.frame(
  Target = c("Overall", "Overall", "Offense", "Offense", "Defense", "Defense"),
  Model = rep(c("Elastic Net", "XGBoost"), 3),
  CV_RMSE = c(
    results_overall$glmnet$cv_rmse, results_overall$xgb$cv_rmse,
    results_offense$glmnet$cv_rmse, results_offense$xgb$cv_rmse,
    results_defense$glmnet$cv_rmse, results_defense$xgb$cv_rmse
  ),
  R2_Weighted = c(
    results_overall$glmnet$r2_weighted, results_overall$xgb$r2_weighted,
    results_offense$glmnet$r2_weighted, results_offense$xgb$r2_weighted,
    results_defense$glmnet$r2_weighted, results_defense$xgb$r2_weighted
  )
)

print(comparison)

# Summary by target
cat("\n--- RMSE Comparison by Target ---\n")
for (target in c("Overall", "Offense", "Defense")) {
  sub <- comparison[comparison$Target == target, ]
  glmnet_rmse <- sub$CV_RMSE[sub$Model == "Elastic Net"]
  xgb_rmse <- sub$CV_RMSE[sub$Model == "XGBoost"]
  diff <- glmnet_rmse - xgb_rmse
  winner <- if (diff > 0) "XGBoost" else "Elastic Net"
  cat(sprintf("%s: glmnet=%.4f, xgb=%.4f, diff=%.4f (%s wins)\n",
              target, glmnet_rmse, xgb_rmse, abs(diff), winner))
}

# Keep references for backward compatibility
spm_glmnet <- results_overall$glmnet$model
spm_xgb <- results_overall$xgb$model

# 5. Top Players by Each Model ----

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("TOP PLAYERS BY EACH MODEL\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n\n")

# Elastic net predictions
spm_pred_glmnet <- calculate_spm_ratings(player_features, spm_glmnet)

# XGBoost predictions
spm_pred_xgb <- calculate_spm_ratings_xgb(player_features, spm_xgb)

cat("Top 15 by Elastic Net SPM:\n")
print(head(spm_pred_glmnet %>% select(player_name, total_minutes, spm), 15))

cat("\nTop 15 by XGBoost SPM:\n")
print(head(spm_pred_xgb %>% select(player_name, total_minutes, spm), 15))

# Compare correlation between the two SPM estimates
both_spm <- spm_pred_glmnet %>%
  select(player_id, spm_glmnet = spm) %>%
  inner_join(
    spm_pred_xgb %>% select(player_id, spm_xgb = spm),
    by = "player_id"
  )

cat(sprintf("\nCorrelation between Elastic Net and XGBoost SPM: %.3f\n",
            cor(both_spm$spm_glmnet, both_spm$spm_xgb)))

cat("\nDone!\n")
