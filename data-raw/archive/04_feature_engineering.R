# 04_feature_engineering.R
# Build rate statistics and player features for SPM model
#
# Converts counting stats to per-100-sequences rates
# and applies Bayesian padding for low-minute players.

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load processed data
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

# Create player feature matrix
features <- create_player_feature_matrix(
  processed_data,
  min_minutes = 180  # Minimum ~2 full matches
)

# Calculate finishing modifiers
finishing <- calculate_finishing_modifier(
  processed_data$shooting,
  min_shots = 20
)

# Calculate touch zones if possession data available
touch_zones <- calculate_touch_zones(processed_data$stats_possession)

# Summary
cat("\n=== Feature Engineering Summary ===\n")
cat(paste("Players with features:", nrow(features), "\n"))
cat(paste("Feature columns:", sum(grepl("_p100", names(features))), "\n"))
cat(paste("Players with finishing data:", nrow(finishing), "\n"))

# Distribution of key features
cat("\n=== Key Feature Distributions ===\n")
if ("npx_g_p100" %in% names(features)) {
  cat("npxG per 100 sequences:\n")
  print(summary(features$npx_g_p100))
}

if ("tkl_p100" %in% names(features)) {
  cat("\nTackles per 100 sequences:\n")
  print(summary(features$tkl_p100))
}

# Combine all features
all_features <- list(
  player_features = features,
  finishing = finishing,
  touch_zones = touch_zones
)

# Save features
saveRDS(all_features, file.path(cache_dir, "04_features.rds"))

message("Feature engineering complete!")
