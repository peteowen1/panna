# 05_spm.R
# Fit SPM (Statistical Plus-Minus) model
#
# SPM predicts Panna ratings from box score statistics.
# This creates a prior that helps separate players who always appear together.

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load data
cat("\n=== Loading Data ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))

panna_ratings <- rapm_results$ratings
cat("Players with Panna ratings:", nrow(panna_ratings), "\n")

# Aggregate player box score stats
cat("\n=== Aggregating Player Statistics ===\n")

player_stats <- aggregate_player_stats(
  stats_summary = processed_data$stats_summary,
  stats_passing = processed_data$stats_passing,
  stats_defense = processed_data$stats_defense,
  stats_possession = processed_data$stats_possession,
  min_minutes = 450  # ~5 full matches minimum
)

cat("Players with sufficient minutes:", nrow(player_stats), "\n")

# Join with Panna ratings for SPM training
# Rename panna to rapm for compatibility with fit_spm_model
spm_train_data <- player_stats %>%
  inner_join(
    panna_ratings %>%
      select(player_name, rapm = panna, offense, defense),
    by = "player_name"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")

# Fit SPM model (predicts overall Panna)
cat("\n=== Fitting SPM Model (Overall) ===\n")

spm_model <- fit_spm_model(
  spm_train_data,
  predictor_cols = NULL,  # Auto-detect _p90 columns
  alpha = 0.5,            # Elastic net
  nfolds = 10
)

# Feature importance
cat("\n=== SPM Feature Importance ===\n")
importance <- get_spm_feature_importance(spm_model, n = 15)
print(importance)

# Generate SPM predictions for all players
cat("\n=== Generating SPM Predictions ===\n")

spm_ratings <- calculate_spm_ratings(player_stats, spm_model)
cat("SPM predictions generated for:", nrow(spm_ratings), "players\n")

# Top/bottom by SPM
cat("\nTop 15 by SPM:\n")
print(head(spm_ratings %>% select(player_name, spm, total_minutes), 15))

cat("\nBottom 15 by SPM:\n")
print(tail(spm_ratings %>% select(player_name, spm, total_minutes), 15))

# Validate SPM vs actual Panna
cat("\n=== SPM Validation ===\n")

validation <- validate_spm_prediction(spm_ratings, panna_ratings %>% rename(rapm = panna))
cat("R-squared:", round(validation$r_squared, 3), "\n")
cat("Correlation:", round(validation$correlation, 3), "\n")
cat("RMSE:", round(validation$rmse, 4), "\n")

# Fit separate O/D SPM models (optional - for analysis)
cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense SPM
offense_train <- spm_train_data %>%
  mutate(rapm = offense)

offense_spm <- fit_spm_model(
  offense_train,
  predictor_cols = c("goals_p90", "assists_p90", "xg_p90", "npxg_p90", "xa_p90",
                     "shots_p90", "shots_on_target_p90", "sca_p90", "gca_p90",
                     "progressive_carries_p90", "progressive_passes_p90"),
  alpha = 0.5,
  nfolds = 10
)

cat("\nOffense SPM feature importance:\n")
print(get_spm_feature_importance(offense_spm, n = 10))

# Defense SPM
defense_train <- spm_train_data %>%
  mutate(rapm = defense)

defense_spm <- fit_spm_model(
  defense_train,
  predictor_cols = c("tackles_p90", "interceptions_p90", "blocks_p90",
                     "tackles_won_p90", "clearances_p90", "touches_p90"),
  alpha = 0.5,
  nfolds = 10
)

cat("\nDefense SPM feature importance:\n")
print(get_spm_feature_importance(defense_spm, n = 10))

# Generate O/D SPM predictions for xRAPM priors
cat("\n=== Generating O/D SPM Predictions ===\n")

offense_spm_ratings <- calculate_spm_ratings(player_stats, offense_spm)
names(offense_spm_ratings)[names(offense_spm_ratings) == "spm"] <- "offense_spm"

defense_spm_ratings <- calculate_spm_ratings(player_stats, defense_spm)
names(defense_spm_ratings)[names(defense_spm_ratings) == "spm"] <- "defense_spm"

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# Save results
cat("\n=== Saving Results ===\n")

spm_results <- list(
  spm_model = spm_model,
  offense_spm = offense_spm,
  defense_spm = defense_spm,
  spm_ratings = spm_ratings,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  player_stats = player_stats,
  importance = importance,
  validation = validation
)

saveRDS(spm_results, file.path(cache_dir, "05_spm.rds"))
cat("Saved to cache/05_spm.rds\n")

cat("\n=== COMPLETE ===\n")
