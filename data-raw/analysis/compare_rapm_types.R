# compare_rapm_types.R
# Compare xG-based RAPM vs Goals-based RAPM
#
# Two types of RAPM:
# - xG-RAPM: Uses non-penalty xG as target (more stable, better process measure)
# - Goals-RAPM: Uses actual goals as target (can use all matches, more data)
#
# Configuration (set before sourcing):
#   leagues = "ENG"  # Use league-specific cache (NULL = main cache)

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")
leagues <- if (exists("leagues")) leagues else NULL

# 2. Load Splint Data ----

cat("\n", strrep("=", 60), "\n")
cat("COMPARING xG-RAPM vs GOALS-RAPM\n")
cat(strrep("=", 60), "\n\n")

cat("=== Loading Splint Data ===\n")

# Determine which cache file to use
if (!is.null(leagues)) {
  splint_file <- file.path(cache_dir, paste0("03_splints_", paste(leagues, collapse = "_"), ".rds"))
  cat("Using league-specific cache:", splint_file, "\n")
} else {
  splint_file <- file.path(cache_dir, "03_splints.rds")
}
splint_data <- readRDS(splint_file)

cat("Raw splints:", nrow(splint_data$splints), "\n")
cat("Raw players:", nrow(splint_data$players), "\n")

# Check for required columns
cat("\nChecking for goals_home/goals_away columns:\n")
if ("goals_home" %in% names(splint_data$splints)) {
  cat("  goals_home: present\n")
  cat("    Range:", range(splint_data$splints$goals_home, na.rm = TRUE), "\n")
  cat("    Total goals:", sum(splint_data$splints$goals_home, na.rm = TRUE), "\n")
} else {
  cat("  goals_home: MISSING - need to recreate splints\n")
}
if ("goals_away" %in% names(splint_data$splints)) {
  cat("  goals_away: present\n")
  cat("    Range:", range(splint_data$splints$goals_away, na.rm = TRUE), "\n")
  cat("    Total goals:", sum(splint_data$splints$goals_away, na.rm = TRUE), "\n")
} else {
  cat("  goals_away: MISSING - need to recreate splints\n")
}

# Check xG data quality (for info only, don't filter)
# Many splints legitimately have zero xG (no shots in that time window)
zero_xg_both <- sum(splint_data$splints$npxg_home == 0 & splint_data$splints$npxg_away == 0, na.rm = TRUE)
cat("\nxG quality check:\n")
cat("  Splints with zero xG (both teams):", zero_xg_both, "of", nrow(splint_data$splints),
    sprintf("(%.1f%%)\n", 100 * zero_xg_both / nrow(splint_data$splints)))
cat("  (This is normal - many short splints have no shots)\n")

# Use all data for both RAPM types
splint_data_xg <- splint_data

# 3. Fit xG-RAPM ----

cat("\n", strrep("=", 60), "\n")
cat("xG-RAPM (non-penalty xG target)\n")
cat(strrep("=", 60), "\n\n")

rapm_data_xg <- prepare_rapm_data(
  splint_data_xg,
  min_minutes = 200,
  target_type = "xg",
  include_covariates = TRUE
)

cat("\nDesign matrix:\n")
cat("  Rows:", rapm_data_xg$n_rows, "\n")
cat("  Players:", rapm_data_xg$n_players, "\n")
cat("  Target type:", rapm_data_xg$target_type, "\n")

model_xg <- fit_rapm(
  rapm_data_xg,
  alpha = 0,
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE
)

ratings_xg <- extract_rapm_ratings(model_xg)
cat("\nTop 10 xG-RAPM:\n")
print(
  ratings_xg %>%
    head(10) %>%
    select(player_name, rapm, offense, defense, total_minutes)
)

# 4. Fit Goals-RAPM ----

cat("\n", strrep("=", 60), "\n")
cat("GOALS-RAPM (actual goals target)\n")
cat(strrep("=", 60), "\n\n")

# For goals-RAPM, we can potentially use ALL data (no xG filter needed)
rapm_data_goals <- prepare_rapm_data(
  splint_data,  # Use full data, not xG-filtered
  min_minutes = 200,
  target_type = "goals",
  include_covariates = TRUE
)

cat("\nDesign matrix:\n")
cat("  Rows:", rapm_data_goals$n_rows, "\n")
cat("  Players:", rapm_data_goals$n_players, "\n")
cat("  Target type:", rapm_data_goals$target_type, "\n")

model_goals <- fit_rapm(
  rapm_data_goals,
  alpha = 0,
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE
)

ratings_goals <- extract_rapm_ratings(model_goals)
cat("\nTop 10 Goals-RAPM:\n")
print(
  ratings_goals %>%
    head(10) %>%
    select(player_name, rapm, offense, defense, total_minutes)
)

# 5. Compare Ratings ----

cat("\n", strrep("=", 60), "\n")
cat("COMPARISON\n")
cat(strrep("=", 60), "\n\n")

# Join ratings
comparison <- ratings_xg %>%
  select(player_id, player_name, total_minutes, rapm_xg = rapm) %>%
  inner_join(
    ratings_goals %>% select(player_id, rapm_goals = rapm),
    by = "player_id"
  ) %>%
  mutate(diff = rapm_xg - rapm_goals) %>%
  arrange(desc(abs(diff)))

cat("Players in both models:", nrow(comparison), "\n\n")

# Correlation
corr <- cor(comparison$rapm_xg, comparison$rapm_goals, use = "complete.obs")
cat("Correlation between xG-RAPM and Goals-RAPM:", round(corr, 3), "\n\n")

cat("Players with BIGGEST POSITIVE difference (xG > Goals):\n")
cat("(Better process than results - unlucky finishers?)\n")
print(
  comparison %>%
    filter(diff > 0) %>%
    head(10) %>%
    select(player_name, total_minutes, rapm_xg, rapm_goals, diff)
)

cat("\nPlayers with BIGGEST NEGATIVE difference (Goals > xG):\n")
cat("(Better results than process - clinical finishers?)\n")
print(
  comparison %>%
    filter(diff < 0) %>%
    head(10) %>%
    select(player_name, total_minutes, rapm_xg, rapm_goals, diff)
)

# 6. Save Results ----

cat("\n=== Saving Results ===\n")

rapm_comparison <- list(
  xg_model = model_xg,
  goals_model = model_goals,
  ratings_xg = ratings_xg,
  ratings_goals = ratings_goals,
  comparison = comparison,
  correlation = corr
)

saveRDS(rapm_comparison, file.path(cache_dir, "rapm_comparison.rds"))
cat("Saved to cache/rapm_comparison.rds\n")

cat("\n", strrep("=", 60), "\n")
cat("COMPLETE\n")
cat(strrep("=", 60), "\n")
