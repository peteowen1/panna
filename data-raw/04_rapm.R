# 04_rapm.R
# Fit base RAPM model
#
# Structure:
# - 2 rows per stint (one per team's attacking perspective)
# - Target: xgf90 (xG FOR per 90)
# - Covariates: gd, gf, ga, avg_min, is_home
# - Player columns: playerX_off, playerX_def
# - Final rating: rapm = offense - defense

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# 2. Load Splint Data ----

cat("\n=== Loading Splint Data ===\n")
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

cat("Raw splints:", nrow(splint_data$splints), "\n")
cat("Raw players:", nrow(splint_data$players), "\n")

# Filter out league-seasons with bad xG data (>20% zero xG splints)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = 20, verbose = TRUE)
splint_data <- filter_result$splint_data

cat("\nAfter filtering:\n")
cat("  Splints:", nrow(splint_data$splints), "\n")
cat("  Players:", nrow(splint_data$players), "\n")

# 3. Create RAPM Design Matrix ----

cat("\n=== Creating RAPM Design Matrix ===\n")

rapm_data <- prepare_rapm_data(
  splint_data,
  min_minutes = 200,
  include_covariates = TRUE
)

cat("\nDesign matrix summary:\n")
cat("  Rows:", rapm_data$n_rows, "\n")
cat("  Players:", rapm_data$n_players, "\n")
cat("  Player columns:", rapm_data$n_players * 2, "(off + def)\n")
cat("  Covariates:", length(rapm_data$covariate_names), "\n")

# Show covariate details
if (length(rapm_data$covariate_names) > 0) {
  cat("\nCovariates included:\n")
  for (cov in rapm_data$covariate_names) {
    cat("  -", cov, "\n")
  }
}

# Show league/season info if present
if (!is.null(rapm_data$leagues)) {
  cat("\nLeagues:", paste(rapm_data$leagues, collapse = ", "), "\n")
}
if (!is.null(rapm_data$seasons)) {
  cat("Seasons:", paste(rapm_data$seasons, collapse = ", "), "\n")
}

# 4. Fit RAPM Model ----

cat("\n=== Fitting RAPM Model ===\n")

model <- fit_rapm(
  rapm_data,
  alpha = 0,           # Ridge regression
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE
)

# 5. Covariate Effects ----

cat("\n=== Covariate Effects ===\n")
cov_effects <- get_covariate_effects(model)
for (name in names(cov_effects)) {
  cat(sprintf("  %s: %.4f\n", name, cov_effects[name]))
}

# 6. Extract Player Ratings ----

cat("\n=== Player Ratings ===\n")
ratings <- extract_rapm_ratings(model)

cat("\nTop 20 players:\n")
print(
  ratings %>%
    head(20) %>%
    select(player_name, rapm, offense, defense, total_minutes)
)

# 7. Save Results ----

cat("\n=== Saving Results ===\n")

rapm_results <- list(
  rapm_data = rapm_data,
  model = model,
  ratings = ratings,
  covariate_effects = cov_effects
)

saveRDS(rapm_results, file.path(cache_dir, "04_rapm.rds"))
cat("Saved to cache/04_rapm.rds\n")

message("\nRAPM complete! Run 05_spm.R next.")
