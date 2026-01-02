# 05_multi_season_rapm.R
# Fit multi-season RAPM model
#
# RAPM (Regularized Adjusted Plus-Minus) provides a stable
# baseline estimate of player impact across multiple seasons.

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load splint data
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

# Prepare RAPM data
rapm_data <- prepare_rapm_data(
  splint_data,
  min_minutes = 180,     # ~2 full matches minimum
  separate_od = FALSE,   # Start with combined RAPM
  apply_weights = TRUE   # Weight by splint duration
)

# Summary of RAPM data
cat("\n=== RAPM Data Summary ===\n")
cat(paste("Splints:", rapm_data$summary$n_splints, "\n"))
cat(paste("Players:", rapm_data$summary$n_players, "\n"))
cat(paste("Total minutes:", round(rapm_data$summary$total_minutes), "\n"))
cat(paste("Response range:", paste(round(rapm_data$summary$response_range, 2), collapse = " to "), "\n"))

# Fit multi-season RAPM
rapm_model <- fit_multi_season_rapm(
  rapm_data,
  alpha = 0,         # Ridge regression
  nfolds = 10,
  use_weights = TRUE
)

# Extract ratings
rapm_ratings <- extract_rapm_coefficients(rapm_model, lambda = "min")

# Display top/bottom players
cat("\n=== Top 10 RAPM Ratings ===\n")
print(head(rapm_ratings, 10))

cat("\n=== Bottom 10 RAPM Ratings ===\n")
print(tail(rapm_ratings, 10))

# Summary statistics
cat("\n=== RAPM Distribution ===\n")
print(summary(rapm_ratings$rapm))

# Save results
rapm_results <- list(
  model = rapm_model,
  ratings = rapm_ratings,
  rapm_data = rapm_data
)

saveRDS(rapm_results, file.path(cache_dir, "05_rapm.rds"))

message("Multi-season RAPM complete!")
