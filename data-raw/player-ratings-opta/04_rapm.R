# 04_rapm.R
# Fit base RAPM model on Opta splints
#
# Near-identical to FBref version. Uses SPADL-derived xG as target.
# Structure: 2 rows per splint (one per attacking perspective),
# target = xgf90, player columns = offense/defense.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# 2. Load Splint Data ----

cat("\n=== Loading Splint Data ===\n")
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

cat("Raw splints:", nrow(splint_data$splints), "\n")
cat("Raw players:", nrow(splint_data$players), "\n")

# Filter out league-seasons with bad xG data
# Opta uses SPADL-derived xG so ~25% zero-xG splints is normal (short splints without shots)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = 30, verbose = TRUE)
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

if (length(rapm_data$covariate_names) > 0) {
  cat("\nCovariates included:\n")
  for (cov in rapm_data$covariate_names) {
    cat("  -", cov, "\n")
  }
}

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
cat("Saved to cache-opta/04_rapm.rds\n")

message("\nRAPM complete!")
