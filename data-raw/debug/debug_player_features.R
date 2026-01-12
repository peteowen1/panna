# debug_player_features.R
# Explore aggregated player features used in SPM models
# Run from panna/ root directory, then View(player_view) to explore

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load results
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
xrapm_results <- readRDS(file.path(cache_dir, "06_xrapm.rds"))

# Get player stats
player_stats <- spm_results$player_stats

# Get ratings
rapm_ratings <- rapm_results$ratings %>%
  select(player_name, rapm)

spm_ratings <- spm_results$spm_ratings %>%
  select(player_name, spm)

xrapm_ratings <- xrapm_results$ratings %>%
  select(player_name, xrapm)

# Get top 20 features from offense XGBoost importance
importance <- xgboost::xgb.importance(model = spm_results$offense_spm_xgb$model)
top_20_features <- head(importance$Feature, 20)

cat("=== Top 20 Features (Offense XGB) ===\n")
print(head(importance, 20))

# Select player info + top 20 features
feature_cols <- intersect(top_20_features, names(player_stats))

player_view <- player_stats %>%
  filter(total_minutes >= 1000) %>%
  select(player_name, total_minutes, all_of(feature_cols)) %>%
  left_join(rapm_ratings, by = "player_name") %>%
  left_join(spm_ratings, by = "player_name") %>%
  left_join(xrapm_ratings, by = "player_name") %>%
  select(player_name, total_minutes, rapm, spm, xrapm, everything()) %>%
  mutate(across(where(is.numeric), ~round(.x, 3))) %>%
  arrange(desc(total_minutes))

View(player_view)

cat("\n=== Player View Dataset ===\n")
cat("Rows:", nrow(player_view), "\n")
cat("Columns:", ncol(player_view), "\n")
cat("\nNow run: View(player_view)\n")
