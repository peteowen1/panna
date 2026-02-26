# 05_spm.R
# Fit SPM (Statistical Plus-Minus) model
#
# SPM predicts Panna ratings from box score statistics.
# This creates a prior that helps separate players who always appear together.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))

rapm_ratings <- rapm_results$ratings
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

# Check what stats tables are available
cat("\nAvailable stats tables:\n")
cat("  - summary:", if (!is.null(processed_data$stats_summary)) nrow(processed_data$stats_summary) else 0, "rows\n")
cat("  - passing:", if (!is.null(processed_data$stats_passing)) nrow(processed_data$stats_passing) else 0, "rows\n")
cat("  - defense:", if (!is.null(processed_data$stats_defense)) nrow(processed_data$stats_defense) else 0, "rows\n")
cat("  - possession:", if (!is.null(processed_data$stats_possession)) nrow(processed_data$stats_possession) else 0, "rows\n")
cat("  - misc:", if (!is.null(processed_data$stats_misc)) nrow(processed_data$stats_misc) else 0, "rows\n")
cat("  - passing_types:", if (!is.null(processed_data$stats_passing_types)) nrow(processed_data$stats_passing_types) else 0, "rows\n")
cat("  - keeper:", if (!is.null(processed_data$stats_keeper)) nrow(processed_data$stats_keeper) else 0, "rows\n")

# 3. Aggregate Player Statistics ----

cat("\n=== Aggregating Player Statistics ===\n")

player_stats <- aggregate_player_stats(
  stats_summary = processed_data$stats_summary,
  stats_passing = processed_data$stats_passing,
  stats_defense = processed_data$stats_defense,
  stats_possession = processed_data$stats_possession,
  stats_misc = processed_data$stats_misc,
  stats_passing_types = processed_data$stats_passing_types,
  stats_keeper = processed_data$stats_keeper,
  min_minutes = 450  # ~5 full matches minimum
)

cat("Players with sufficient minutes:", nrow(player_stats), "\n")
cat("Features per player:", ncol(player_stats), "\n")

# Show minutes distribution
cat("\nMinutes distribution:\n")
print(summary(player_stats$total_minutes))

# Join with RAPM ratings for SPM training
spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings %>%
      select(player_name, rapm, offense, defense),
    by = "player_name"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")

# 4. Fit Elastic Net and XGBoost Models ----

cat("\n=== Fitting Elastic Net SPM (weighted, sqrt) ===\n")
spm_glmnet <- fit_spm_model(
  spm_train_data,
  predictor_cols = NULL,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)

cat("\n=== Fitting XGBoost SPM ===\n")
spm_xgb <- fit_spm_xgb(
  spm_train_data,
  nfolds = 10,
  max_depth = 4,
  eta = 0.02,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

# 5. Model Comparison ----

cat("\n=== Model Comparison ===\n")

# Calculate CV RMSE for Elastic Net
cv_rmse_glmnet <- sqrt(spm_glmnet$cvm[spm_glmnet$lambda == spm_glmnet$lambda.min])
cv_rmse_xgb <- spm_xgb$best_cv_rmse

cat(sprintf("\nCross-Validation RMSE:\n"))
cat(sprintf("  Elastic Net: %.4f\n", cv_rmse_glmnet))
cat(sprintf("  XGBoost:     %.4f\n", cv_rmse_xgb))
cat(sprintf("  Winner:      %s\n", if (cv_rmse_glmnet < cv_rmse_xgb) "Elastic Net" else "XGBoost"))

# Get predictions from each model
spm_ratings_glmnet <- calculate_spm_ratings(player_stats, spm_glmnet)
spm_ratings_xgb <- calculate_spm_ratings_xgb(player_stats, spm_xgb)

# 6. Create 50/50 Blend ----

cat("\n=== Creating 50/50 Blend ===\n")

spm_ratings_blend <- calculate_spm_blend(player_stats, spm_glmnet, spm_xgb, weight_glmnet = 0.5)

cat("Blended SPM ratings:", nrow(spm_ratings_blend), "players\n")

# Evaluate correlation with RAPM
blend_eval <- spm_ratings_blend %>%
  inner_join(rapm_ratings %>% select(player_name, rapm), by = "player_name")

cat("\nCorrelation with RAPM:\n")
cat(sprintf("  Elastic Net: %.3f\n", cor(blend_eval$spm_glmnet, blend_eval$rapm)))
cat(sprintf("  XGBoost:     %.3f\n", cor(blend_eval$spm_xgb, blend_eval$rapm)))
cat(sprintf("  50/50 Blend: %.3f\n", cor(blend_eval$spm, blend_eval$rapm)))

# Correlation between the two models
cat(sprintf("\nCorrelation between Elastic Net and XGBoost: %.3f\n",
            cor(blend_eval$spm_glmnet, blend_eval$spm_xgb)))

# 7. Validation ----

cat("\n=== Validation ===\n")
val_blend <- validate_spm_prediction(
  spm_ratings_blend %>% select(-spm_glmnet, -spm_xgb),
  rapm_ratings
)

# Use blend as the primary SPM
spm_model <- spm_glmnet  # Keep glmnet as "primary" for feature importance
spm_ratings <- spm_ratings_blend
validation <- val_blend

cat("\n=== SPM Feature Importance (Top 20) ===\n")
importance <- get_spm_feature_importance(spm_model, n = 20)
print(importance)

# 8. Top/Bottom Players by SPM ----

cat("\nTop 20 by SPM:\n")
top_spm <- spm_ratings %>%
  inner_join(rapm_ratings %>% select(player_name, rapm), by = "player_name") %>%
  arrange(desc(spm)) %>%
  head(20) %>%
  select(player_name, spm, rapm, total_minutes)
print(top_spm)

cat("\nBottom 20 by SPM:\n")
bottom_spm <- spm_ratings %>%
  inner_join(rapm_ratings %>% select(player_name, rapm), by = "player_name") %>%
  arrange(spm) %>%
  head(20) %>%
  select(player_name, spm, rapm, total_minutes)
print(bottom_spm)

# 9. Separate Offense/Defense SPM (with blending) ----

cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense training data
offense_train <- spm_train_data %>%
  mutate(rapm = offense)

offense_cols <- c(
  # Goals and expected
  "goals_p90", "assists_p90", "xg_p90", "npxg_p90", "xa_p90", "npxg_plus_xa_p90",
  # Shots
  "shots_p90", "shots_on_target_p90",
  # Chance creation
  "sca_p90", "gca_p90", "key_passes_p90", "through_balls_p90",
  # Progression
  "progressive_carries_p90", "progressive_passes_p90", "prg_passes_received_p90",
  # Box involvement
  "touches_att_pen_p90", "touches_att_3rd_p90", "carries_into_box_p90",
  "carries_final_3rd_p90", "passes_into_box_p90", "final_third_passes_p90",
  # Crossing and take-ons
  "crosses_p90", "crosses_into_box_p90", "take_ons_succ_p90", "take_ons_att_p90",
  # Drawing fouls
  "fouls_drawn_p90", "penalties_won_p90"
)

# Offense Elastic Net
cat("\n--- Offense Elastic Net ---\n")
offense_spm_glmnet <- fit_spm_model(
  offense_train,
  predictor_cols = offense_cols,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE
)

# Offense XGBoost
cat("\n--- Offense XGBoost ---\n")
offense_spm_xgb <- fit_spm_xgb(
  offense_train,
  predictor_cols = offense_cols,
  nfolds = 10,
  max_depth = 4,
  eta = 0.02,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

cat("\nOffense SPM feature importance (Elastic Net):\n")
print(get_spm_feature_importance(offense_spm_glmnet, n = 10))

# Defense training data
defense_train <- spm_train_data %>%
  mutate(rapm = defense)

defense_cols <- c(
  # Tackles by zone
  "tackles_p90", "tackles_won_p90", "tackles_def_3rd_p90",
  "tackles_mid_3rd_p90", "tackles_att_3rd_p90",
  # Blocks and interceptions
  "interceptions_p90", "blocks_p90", "blocks_shots_p90", "blocks_pass_p90",
  # Clearances and recoveries
  "clearances_p90", "recoveries_p90",
  # Aerials
  "aerials_won_p90", "aerials_total_p90",
  # Defensive involvement
  "touches_def_3rd_p90", "touches_mid_3rd_p90",
  # Negative actions
  "fouls_committed_p90", "errors_p90", "penalties_conceded_p90"
)

# Defense Elastic Net
cat("\n--- Defense Elastic Net ---\n")
defense_spm_glmnet <- fit_spm_model(
  defense_train,
  predictor_cols = defense_cols,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE
)

# Defense XGBoost
cat("\n--- Defense XGBoost ---\n")
defense_spm_xgb <- fit_spm_xgb(
  defense_train,
  predictor_cols = defense_cols,
  nfolds = 10,
  max_depth = 4,
  eta = 0.02,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

cat("\nDefense SPM feature importance (Elastic Net):\n")
print(get_spm_feature_importance(defense_spm_glmnet, n = 10))

# 10. Generate Blended O/D SPM Predictions ----

cat("\n=== Generating Blended O/D SPM Predictions ===\n")

# Offense blend
offense_glmnet_pred <- calculate_spm_ratings(player_stats, offense_spm_glmnet)
offense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, offense_spm_xgb)

offense_spm_ratings <- offense_glmnet_pred %>%
  rename(offense_spm_glmnet = spm) %>%
  inner_join(
    offense_xgb_pred %>% select(player_id, offense_spm_xgb = spm),
    by = "player_id"
  ) %>%
  mutate(offense_spm = 0.5 * offense_spm_glmnet + 0.5 * offense_spm_xgb)

# Defense blend
defense_glmnet_pred <- calculate_spm_ratings(player_stats, defense_spm_glmnet)
defense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, defense_spm_xgb)

defense_spm_ratings <- defense_glmnet_pred %>%
  rename(defense_spm_glmnet = spm) %>%
  inner_join(
    defense_xgb_pred %>% select(player_id, defense_spm_xgb = spm),
    by = "player_id"
  ) %>%
  mutate(defense_spm = 0.5 * defense_spm_glmnet + 0.5 * defense_spm_xgb)

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# 11. Combined SPM Ratings ----

cat("\n=== Combined SPM Ratings ===\n")

combined_spm <- spm_ratings %>%
  select(player_id, player_name, total_minutes, spm) %>%
  left_join(
    offense_spm_ratings %>% select(player_id, offense_spm),
    by = "player_id"
  ) %>%
  left_join(
    defense_spm_ratings %>% select(player_id, defense_spm),
    by = "player_id"
  ) %>%
  left_join(
    rapm_ratings %>% select(player_name, rapm, offense, defense),
    by = "player_name"
  ) %>%
  arrange(desc(spm))

cat("\nTop 25 Players by SPM (with O/D breakdown):\n")
top_combined <- combined_spm %>%
  head(25) %>%
  select(player_name, total_minutes, spm, offense_spm, defense_spm, rapm)
print(top_combined, digits = 3)

cat("\nTop 25 by Offense SPM:\n")
top_offense <- combined_spm %>%
  arrange(desc(offense_spm)) %>%
  head(25) %>%
  select(player_name, total_minutes, offense_spm, spm, rapm)
print(top_offense, digits = 3)

cat("\nTop 25 by Defense SPM (lower = better at preventing goals):\n")
top_defense <- combined_spm %>%
  arrange(defense_spm) %>%
  head(25) %>%
  select(player_name, total_minutes, defense_spm, spm, rapm)
print(top_defense, digits = 3)

# 12. Save Results ----

cat("\n=== Saving Results ===\n")

spm_results <- list(
  # Overall models
  spm_glmnet = spm_glmnet,
  spm_xgb = spm_xgb,
  # O/D models
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb,
  # Ratings (all are 50/50 blends)
  spm_ratings = spm_ratings,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  combined_ratings = combined_spm,
  # Supporting data
  player_stats = player_stats,
  importance = importance,
  validation = validation,
  # Model comparison
  model_comparison = list(
    cv_rmse_glmnet = cv_rmse_glmnet,
    cv_rmse_xgb = cv_rmse_xgb,
    blend_weight = 0.5
  )
)

saveRDS(spm_results, file.path(cache_dir, "05_spm.rds"))
cat("Saved to cache/05_spm.rds\n")

cat("\n=== COMPLETE ===\n")
