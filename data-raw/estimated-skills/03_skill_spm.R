# 03_skill_spm.R
# Fit SPM model using skill-based features (estimated skills)
#
# Reuses fit_spm_opta() and the existing SPM infrastructure, but feeds
# it decay-weighted skill features instead of raw season averages.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 3. Load Data ----

cat("\n=== Loading Data ===\n")

skill_features <- readRDS(file.path(cache_dir, "02_skill_features.rds"))
rapm_results <- readRDS(file.path(opta_cache_dir, "04_rapm.rds"))

rapm_ratings <- rapm_results$ratings
cat("Players with skill features:", nrow(skill_features), "\n")
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

# 4. Prepare SPM Training Data ----

cat("\n=== Preparing SPM Training Data ===\n")

# Skill features may span multiple seasons per player; aggregate to single row
# (the features are already decay-weighted at end-of-season, so for all-time SPM
# we use the most recent season's estimates per player)
player_stats <- skill_features %>%
  group_by(player_id) %>%
  slice_max(season_end_year, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("Unique players for SPM:", nrow(player_stats), "\n")

# Ensure required columns exist
if (!"player_name" %in% names(player_stats)) {
  player_stats$player_name <- player_stats$player_id
}

# Ensure mins_per_90 exists (needed by fit_spm_model weight logic)
if (!"mins_per_90" %in% names(player_stats)) {
  player_stats$mins_per_90 <- player_stats$total_minutes / 90
}

# Join with RAPM for training
spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings %>% select(player_name, rapm, offense, defense),
    by = "player_name"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")

if (nrow(spm_train_data) < 100) {
  stop("Too few players for SPM training. Check RAPM/skill feature overlap.")
}

# 5. Fit Elastic Net SPM ----

cat("\n=== Fitting Skill-Based Elastic Net SPM ===\n")

spm_glmnet <- fit_spm_opta(
  spm_train_data,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)

# 6. Fit XGBoost SPM ----

cat("\n=== Fitting Skill-Based XGBoost SPM ===\n")

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

cat(sprintf("Cross-Validation RMSE:\n"))
cat(sprintf("  Elastic Net: %.4f\n", cv_rmse_glmnet))
cat(sprintf("  XGBoost:     %.4f\n", cv_rmse_xgb))

# Get predictions
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

# Compare with raw-stat SPM if available
opta_spm_path <- file.path(opta_cache_dir, "05_spm.rds")
if (file.exists(opta_spm_path)) {
  opta_spm <- readRDS(opta_spm_path)
  raw_eval <- opta_spm$spm_ratings %>%
    inner_join(rapm_ratings %>% select(player_name, rapm), by = "player_name")
  raw_corr <- cor(raw_eval$spm, raw_eval$rapm)
  skill_corr <- cor(blend_eval$spm, blend_eval$rapm)
  cat(sprintf("\n*** Skill-based vs Raw-stat SPM ***\n"))
  cat(sprintf("  Raw-stat SPM  r(RAPM): %.3f\n", raw_corr))
  cat(sprintf("  Skill SPM     r(RAPM): %.3f\n", skill_corr))
  cat(sprintf("  Improvement:           %+.3f\n", skill_corr - raw_corr))
}

# 9. Validate ----

cat("\n=== Validation ===\n")
val_blend <- validate_spm_prediction(
  spm_ratings_blend %>% select(-spm_glmnet, -spm_xgb),
  rapm_ratings
)

cat("\n=== SPM Feature Importance (Top 20) ===\n")
importance <- get_spm_feature_importance(spm_glmnet, n = 20)
print(importance)

# 10. Fit O/D Models ----

cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense
offense_train <- spm_train_data %>% mutate(rapm = offense)

offense_cols <- c(
  "goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
  "big_chance_scored_p90", "big_chance_created_p90",
  "att_openplay_p90", "att_headed_p90", "att_one_on_one_p90",
  "assists_p90", "key_passes_p90", "through_balls_p90", "total_att_assist_p90",
  "touches_opp_box_p90", "pen_area_entries_p90", "final_third_entries_p90",
  "final_third_passes_p90", "fwd_zone_pass_p90", "open_play_pass_p90",
  "att_fastbreak_p90", "shot_fastbreak_p90",
  "crosses_p90", "crosses_open_play_p90", "forward_pass_p90",
  "was_fouled_p90", "penalty_won_p90",
  "shot_accuracy", "goals_per_shot", "big_chance_conversion",
  "fwd_zone_pass_accuracy", "open_play_pass_accuracy", "crosses_open_play_accuracy",
  "att_ibox_goal_p90", "att_obox_goal_p90", "ibox_goal_rate", "penalty_conversion",
  "chipped_pass_p90", "chipped_pass_accuracy",
  "att_rf_total_p90", "att_lf_total_p90"
)

if ("xg_per90" %in% names(spm_train_data)) {
  offense_cols <- c(offense_cols, "xg_per90", "npxg_per90", "xa_per90_xmetrics")
}

offense_cols <- intersect(offense_cols, names(spm_train_data))

cat("\n--- Offense Elastic Net ---\n")
offense_spm_glmnet <- fit_spm_model(offense_train, predictor_cols = offense_cols,
                                     alpha = 0.5, nfolds = 10, weight_by_minutes = TRUE)

cat("\n--- Offense XGBoost ---\n")
offense_spm_xgb <- fit_spm_xgb(offense_train, predictor_cols = offense_cols,
                                 nfolds = 10, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

# Defense
defense_train <- spm_train_data %>% mutate(rapm = defense)

defense_cols <- c(
  "tackles_p90", "tackles_won_p90",
  "interceptions_p90", "interceptions_won_p90",
  "clearances_p90", "clearances_effective_p90",
  "blocks_p90", "blocked_passes_p90",
  "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
  "aerial_won_p90", "aerial_lost_p90",
  "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
  "fouls_p90", "penalty_conceded_p90",
  "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
  "tackle_success", "aerial_success",
  "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch",
  "fifty_fifty_p90", "fifty_fifty_won_p90", "fifty_fifty_success",
  "back_zone_pass_p90", "back_zone_pass_accuracy",
  "long_pass_own_to_opp_p90", "long_pass_own_to_opp_accuracy"
)

defense_cols <- intersect(defense_cols, names(spm_train_data))

cat("\n--- Defense Elastic Net ---\n")
defense_spm_glmnet <- fit_spm_model(defense_train, predictor_cols = defense_cols,
                                     alpha = 0.5, nfolds = 10, weight_by_minutes = TRUE)

cat("\n--- Defense XGBoost ---\n")
defense_spm_xgb <- fit_spm_xgb(defense_train, predictor_cols = defense_cols,
                                 nfolds = 10, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

# O/D Predictions
offense_glmnet_pred <- calculate_spm_ratings(player_stats, offense_spm_glmnet)
offense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, offense_spm_xgb)
offense_spm_ratings <- offense_glmnet_pred %>%
  rename(off_glmnet = spm) %>%
  inner_join(offense_xgb_pred %>% select(player_id, off_xgb = spm), by = "player_id") %>%
  mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

defense_glmnet_pred <- calculate_spm_ratings(player_stats, defense_spm_glmnet)
defense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, defense_spm_xgb)
defense_spm_ratings <- defense_glmnet_pred %>%
  rename(def_glmnet = spm) %>%
  inner_join(defense_xgb_pred %>% select(player_id, def_xgb = spm), by = "player_id") %>%
  mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# 11. Save ----

cat("\n=== Saving Results ===\n")

spm_results <- list(
  spm_glmnet = spm_glmnet,
  spm_xgb = spm_xgb,
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb,
  spm_ratings = spm_ratings_blend,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  player_stats = player_stats,
  importance = importance,
  validation = val_blend,
  model_comparison = list(
    cv_rmse_glmnet = cv_rmse_glmnet,
    cv_rmse_xgb = cv_rmse_xgb,
    blend_weight = 0.5
  )
)

saveRDS(spm_results, file.path(cache_dir, "03_skill_spm.rds"))
cat("Saved to cache-skills/03_skill_spm.rds\n")

cat("\n=== COMPLETE ===\n")
