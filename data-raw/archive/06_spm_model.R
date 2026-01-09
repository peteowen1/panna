# 06_spm_model.R
# Build SPM (Statistical Plus-Minus) model and re-fit RAPM with prior
#
# This script:
# 1. Aggregates player box score statistics to per-90 rates
# 2. Fits SPM model (predicts RAPM from box scores)
# 3. Creates SPM prior for all players
# 4. Re-fits RAPM shrinking toward SPM prior (instead of zero)
# 5. Compares results with and without prior

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load data ----
cat("\n=== Loading Data ===\n")
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))

rapm_data <- rapm_results$rapm_data
rapm_ratings_no_prior <- rapm_results$ratings
rapm_model_no_prior <- rapm_results$model

cat(paste("Players in RAPM:", nrow(rapm_ratings_no_prior), "\n"))

# 1. Aggregate Player Stats ----
cat("\n=== 1. Aggregating Player Statistics ===\n")

player_stats <- aggregate_player_stats(
  stats_summary = processed_data$stats_summary,
  stats_passing = processed_data$stats_passing,
  stats_defense = processed_data$stats_defense,
  stats_possession = processed_data$stats_possession,
  min_minutes = 450  # ~5 full matches minimum
)

cat(paste("Players with sufficient minutes:", nrow(player_stats), "\n"))

# Show available predictors
p90_cols <- names(player_stats)[grepl("_p90$", names(player_stats))]
cat(paste("Per-90 predictors available:", length(p90_cols), "\n"))

# 2. Fit SPM Model ----
cat("\n=== 2. Fitting SPM Model ===\n")

# Join player stats with RAPM ratings for training
spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings_no_prior %>% select(player_name, rapm),
    by = "player_name"
  )

cat(paste("Players for SPM training:", nrow(spm_train_data), "\n"))

# Fit SPM model
spm_model <- fit_spm_model(
  spm_train_data,
  predictor_cols = NULL,  # Use all _p90 columns
  alpha = 0.5,            # Elastic net
  nfolds = 10
)

# Feature importance
cat("\n=== SPM Feature Importance ===\n")
importance <- get_spm_feature_importance(spm_model, n = 15)
print(importance)

# 3. Generate SPM Predictions ----
cat("\n=== 3. Generating SPM Predictions ===\n")

spm_ratings <- calculate_spm_ratings(player_stats, spm_model)

cat(paste("SPM predictions generated for:", nrow(spm_ratings), "players\n"))

# Show top/bottom SPM
cat("\nTop 10 by SPM (box score prediction):\n")
print(head(spm_ratings %>% select(player_name, spm, total_minutes), 10))

cat("\nBottom 10 by SPM:\n")
print(tail(spm_ratings %>% select(player_name, spm, total_minutes), 10))

# 4. Validate SPM vs RAPM ----
cat("\n=== 4. SPM Validation ===\n")

validation <- validate_spm_prediction(spm_ratings, rapm_ratings_no_prior)

cat(paste("R-squared:", round(validation$r_squared, 3), "\n"))
cat(paste("RMSE:", round(validation$rmse, 4), "\n"))
cat(paste("Correlation:", round(validation$correlation, 3), "\n"))

# 5. Create SPM Prior ----
cat("\n=== 5. Creating SPM Prior ===\n")

spm_prior <- create_spm_prior(
  spm_ratings,
  rapm_data$player_mapping,
  default_prior = 0  # Players without SPM get prior of 0
)

cat(paste("Prior created for:", sum(spm_prior != 0), "players with non-zero values\n"))
cat(paste("Prior range:", round(min(spm_prior), 3), "to", round(max(spm_prior), 3), "\n"))

# 6. Re-fit RAPM with Prior ----
cat("\n=== 6. Fitting RAPM with SPM Prior ===\n")

rapm_model_with_prior <- fit_multi_season_rapm(
  rapm_data,
  prior = spm_prior,
  alpha = 0,     # Ridge
  nfolds = 10
)

rapm_ratings_with_prior <- extract_rapm_coefficients(rapm_model_with_prior)

# 7. Compare Results ----
cat("\n=== 7. Comparison: RAPM With vs Without Prior ===\n")

comparison <- rapm_ratings_no_prior %>%
  select(player_name, rapm_no_prior = rapm) %>%
  inner_join(
    rapm_ratings_with_prior %>%
      select(player_name, rapm_with_prior = rapm, spm_prior, deviation_from_prior),
    by = "player_name"
  ) %>%
  mutate(
    rating_change = rapm_with_prior - rapm_no_prior,
    abs_change = abs(rating_change)
  ) %>%
  arrange(desc(rapm_with_prior))

# Top players with prior
cat("\nTop 20 players (RAPM with SPM prior):\n")
print(
  comparison %>%
    head(20) %>%
    select(player_name, rapm_with_prior, spm_prior, deviation_from_prior, rapm_no_prior)
)

# Biggest improvements
cat("\nBiggest rating improvements (prior helped):\n")
print(
  comparison %>%
    arrange(desc(rating_change)) %>%
    head(10) %>%
    select(player_name, rapm_no_prior, spm_prior, rapm_with_prior, rating_change)
)

# Biggest drops
cat("\nBiggest rating drops (prior hurt):\n")
print(
  comparison %>%
    arrange(rating_change) %>%
    head(10) %>%
    select(player_name, rapm_no_prior, spm_prior, rapm_with_prior, rating_change)
)

# Correlation between old and new
cat("\nCorrelation between RAPM (no prior) and RAPM (with prior):\n")
cat(sprintf("  r = %.3f\n", cor(comparison$rapm_no_prior, comparison$rapm_with_prior)))

# 8. Team-Level Validation ----
cat("\n=== 8. Team-Level Validation ===\n")

# Get team assignments
if (!is.null(processed_data$lineups)) {
  player_teams <- processed_data$lineups %>%
    group_by(player_name, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_name) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_name, primary_team = team)

  # Team ratings with prior
  team_ratings_with_prior <- comparison %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      mean_rapm = mean(rapm_with_prior),
      sum_rapm = sum(rapm_with_prior),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_rapm))

  cat("\nTeam average RAPM (with prior):\n")
  print(team_ratings_with_prior, n = 25)

  # Compare with actual npxGD
  if (!is.null(processed_data$results)) {
    results <- processed_data$results

    home_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = home_team) %>%
      summarise(total_npxgd = sum(home_xg - away_xg, na.rm = TRUE), .groups = "drop")

    away_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = away_team) %>%
      summarise(total_npxgd = sum(away_xg - home_xg, na.rm = TRUE), .groups = "drop")

    team_npxgd <- home_npxgd %>%
      full_join(away_npxgd, by = "team", suffix = c("_home", "_away")) %>%
      mutate(total_npxgd = coalesce(total_npxgd_home, 0) + coalesce(total_npxgd_away, 0)) %>%
      select(team, total_npxgd)

    team_comparison <- team_npxgd %>%
      inner_join(
        team_ratings_with_prior %>% select(primary_team, sum_rapm),
        by = c("team" = "primary_team")
      )

    cat("\nCorrelation: Team npxGD vs Sum of Player RAPM (with prior):\n")
    cat(sprintf("  r = %.3f (was %.3f without prior)\n",
                cor(team_comparison$total_npxgd, team_comparison$sum_rapm),
                cor(team_comparison$total_npxgd,
                    team_ratings_with_prior$sum_rapm[match(team_comparison$team, team_ratings_with_prior$primary_team)])))
  }
}

# 9. Save Results ----
cat("\n=== 9. Saving Results ===\n")

spm_results <- list(
  # SPM model
  spm_model = spm_model,
  spm_ratings = spm_ratings,
  spm_prior = spm_prior,
  importance = importance,
  validation = validation,

  # Aggregated player stats
  player_stats = player_stats,

  # RAPM with prior
  rapm_model_with_prior = rapm_model_with_prior,
  rapm_ratings_with_prior = rapm_ratings_with_prior,

  # Comparison
  comparison = comparison
)

saveRDS(spm_results, file.path(cache_dir, "06_spm.rds"))

cat("SPM model and RAPM with prior saved to cache/06_spm.rds\n")
cat("\n=== COMPLETE ===\n")
