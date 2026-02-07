# 05_spm.R
# Fit SPM model using Opta features
#
# Uses aggregate_opta_stats() (80+ features) instead of aggregate_player_stats().
# Optionally enriches features with xMetrics (xG/xA/xPass per-90).
# Fits Elastic Net + XGBoost, creates 50/50 blend, fits O/D models.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))

rapm_ratings <- rapm_results$ratings
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

# 3. Aggregate Opta Player Statistics ----

cat("\n=== Aggregating Opta Player Statistics ===\n")

opta_stats <- processed_data$opta_stats
if (is.null(opta_stats) || nrow(opta_stats) == 0) {
  stop("No Opta stats available. Check step 01 output.")
}

cat("Opta stats rows:", nrow(opta_stats), "\n")

player_stats <- aggregate_opta_stats(
  opta_stats,
  min_minutes = 450
)

cat("Players with sufficient minutes:", nrow(player_stats), "\n")
cat("Features per player:", ncol(player_stats), "\n")

# 4. Enrich with xMetrics Features ----

if (use_xmetrics_features && !is.null(processed_data$opta_xmetrics)) {
  cat("\n=== Enriching with xMetrics Features ===\n")

  xmetrics <- processed_data$opta_xmetrics
  cat("xMetrics rows:", nrow(xmetrics), "\n")

  # Aggregate xMetrics to player level (may span multiple seasons)
  xmetrics_agg <- xmetrics %>%
    mutate(player_id = clean_player_name(player_name)) %>%
    group_by(player_id) %>%
    summarise(
      xg_total = sum(xg, na.rm = TRUE),
      npxg_total = sum(npxg, na.rm = TRUE),
      xa_total = sum(xa, na.rm = TRUE),
      xmetrics_minutes = sum(minutes, na.rm = TRUE),
      xpass_overperformance_total = sum(xpass_overperformance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(xmetrics_minutes > 0) %>%
    mutate(
      xg_per90 = xg_total / xmetrics_minutes * 90,
      npxg_per90 = npxg_total / xmetrics_minutes * 90,
      xa_per90_xmetrics = xa_total / xmetrics_minutes * 90,
      xpass_overperformance_per90_xmetrics = xpass_overperformance_total / xmetrics_minutes * 90
    )

  # Join to player_stats
  before_cols <- ncol(player_stats)
  player_stats <- player_stats %>%
    left_join(
      xmetrics_agg %>% select(player_id, xg_per90, npxg_per90,
                               xa_per90_xmetrics, xpass_overperformance_per90_xmetrics),
      by = "player_id"
    )

  # Fill NAs with 0 for players without xMetrics
  xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics", "xpass_overperformance_per90_xmetrics")
  for (col in xm_cols) {
    player_stats[[col]][is.na(player_stats[[col]])] <- 0
  }

  cat(sprintf("  Added %d xMetrics features\n", ncol(player_stats) - before_cols))
  cat(sprintf("  Players with xMetrics: %d / %d\n",
              sum(player_stats$xg_per90 > 0), nrow(player_stats)))
}

# 5. Join with RAPM for Training ----

cat("\n=== Preparing SPM Training Data ===\n")

spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings %>%
      select(player_name, rapm, offense, defense),
    by = "player_name"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")

# 6. Fit Elastic Net and XGBoost Models ----

cat("\n=== Fitting Opta Elastic Net SPM ===\n")
spm_glmnet <- fit_spm_opta(
  spm_train_data,
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

# 7. Model Comparison ----

cat("\n=== Model Comparison ===\n")

cv_rmse_glmnet <- sqrt(spm_glmnet$cvm[spm_glmnet$lambda == spm_glmnet$lambda.min])
cv_rmse_xgb <- spm_xgb$best_cv_rmse

cat(sprintf("\nCross-Validation RMSE:\n"))
cat(sprintf("  Elastic Net: %.4f\n", cv_rmse_glmnet))
cat(sprintf("  XGBoost:     %.4f\n", cv_rmse_xgb))

# Get predictions from each model
spm_ratings_glmnet <- calculate_spm_ratings(player_stats, spm_glmnet)
spm_ratings_xgb <- calculate_spm_ratings_xgb(player_stats, spm_xgb)

# 8. Create 50/50 Blend ----

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

# 9. Validation ----

cat("\n=== Validation ===\n")
val_blend <- validate_spm_prediction(
  spm_ratings_blend %>% select(-spm_glmnet, -spm_xgb),
  rapm_ratings
)

spm_model <- spm_glmnet
spm_ratings <- spm_ratings_blend
validation <- val_blend

cat("\n=== SPM Feature Importance (Top 20) ===\n")
importance <- get_spm_feature_importance(spm_model, n = 20)
print(importance)

# 10. Separate Offense/Defense SPM ----

cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense training data
offense_train <- spm_train_data %>%
  mutate(rapm = offense)

offense_cols <- c(
  # Goals and shooting
  "goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
  "big_chance_scored_p90", "big_chance_created_p90",
  # Assists and creativity
  "assists_p90", "key_passes_p90", "through_balls_p90",
  "total_att_assist_p90",
  # Possession and progression
  "touches_opp_box_p90", "pen_area_entries_p90", "final_third_entries_p90",
  "final_third_passes_p90",
  # Crossing and set pieces
  "crosses_p90", "forward_pass_p90",
  # Fouls drawn
  "was_fouled_p90",
  # Efficiency
  "shot_accuracy", "goals_per_shot", "big_chance_conversion"
)

# Add xMetrics offense features if available
if ("xg_per90" %in% names(spm_train_data)) {
  offense_cols <- c(offense_cols, "xg_per90", "npxg_per90", "xa_per90_xmetrics")
}

# Filter to available columns
offense_cols <- intersect(offense_cols, names(spm_train_data))

cat("\n--- Offense Elastic Net ---\n")
offense_spm_glmnet <- fit_spm_model(
  offense_train,
  predictor_cols = offense_cols,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE
)

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

# Defense training data
defense_train <- spm_train_data %>%
  mutate(rapm = defense)

defense_cols <- c(
  # Tackles
  "tackles_p90", "tackles_won_p90",
  # Interceptions and blocks
  "interceptions_p90", "interceptions_won_p90",
  "clearances_p90", "clearances_effective_p90",
  "blocks_p90", "blocked_passes_p90",
  # Aerials
  "aerial_won_p90", "aerial_lost_p90",
  # Ball recovery
  "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
  # Negative actions
  "fouls_p90",
  # Efficiency
  "tackle_success", "aerial_success"
)

defense_cols <- intersect(defense_cols, names(spm_train_data))

cat("\n--- Defense Elastic Net ---\n")
defense_spm_glmnet <- fit_spm_model(
  defense_train,
  predictor_cols = defense_cols,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE
)

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

# 11. Generate Blended O/D SPM Predictions ----

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

# 12. Combined SPM Ratings ----

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
print(
  combined_spm %>%
    head(25) %>%
    select(player_name, total_minutes, spm, offense_spm, defense_spm, rapm),
  digits = 3
)

# 13. Save Results ----

cat("\n=== Saving Results ===\n")

spm_results <- list(
  spm_glmnet = spm_glmnet,
  spm_xgb = spm_xgb,
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb,
  spm_ratings = spm_ratings,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  combined_ratings = combined_spm,
  player_stats = player_stats,
  importance = importance,
  validation = validation,
  model_comparison = list(
    cv_rmse_glmnet = cv_rmse_glmnet,
    cv_rmse_xgb = cv_rmse_xgb,
    blend_weight = 0.5
  )
)

saveRDS(spm_results, file.path(cache_dir, "05_spm.rds"))
cat("Saved to cache-opta/05_spm.rds\n")

cat("\n=== COMPLETE ===\n")
