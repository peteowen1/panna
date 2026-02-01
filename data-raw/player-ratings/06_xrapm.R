# 06_xrapm.R
# Fit xRAPM (RAPM with SPM prior)
#
# xRAPM uses SPM predictions as a Bayesian prior for RAPM fitting.
# This helps separate players who always appear together (teammate confounding)
# by leveraging box score statistics.
#
# The SPM priors are 50/50 blends of Elastic Net and XGBoost predictions
# (see 05_spm.R for model comparison).

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Lambda for xRAPM: "min" (default), "1se", or numeric value
# Higher values = more shrinkage toward SPM prior
xrapm_lambda <- "min"

cat(sprintf("Using lambda = %s for xRAPM\n", xrapm_lambda))

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Players with RAPM:", nrow(rapm_results$ratings), "\n")
cat("Players with SPM:", nrow(spm_results$spm_ratings), "\n")

# 3. Create SPM Priors (from blended models) ----

cat("\n=== Creating SPM Priors ===\n")
cat("Using 50/50 Elastic Net + XGBoost blend\n")

# Get player mapping from RAPM data
player_mapping <- rapm_results$rapm_data$player_mapping

# Build priors using helper function (replaces manual for-loops)
offense_prior <- build_prior_vector(
  spm_data = spm_results$offense_spm_ratings,
  spm_col = "offense_spm",
  player_mapping = player_mapping
)

defense_prior <- build_prior_vector(
  spm_data = spm_results$defense_spm_ratings,
  spm_col = "defense_spm",
  player_mapping = player_mapping
)

cat("Offense priors set:", sum(offense_prior != 0), "\n")
cat("Defense priors set:", sum(defense_prior != 0), "\n")

# Summary of prior values
cat("\nOffense prior summary:\n")
print(summary(offense_prior[offense_prior != 0]))
cat("\nDefense prior summary:\n")
print(summary(defense_prior[defense_prior != 0]))

# 4. Fit xRAPM Model ----

cat("\n=== Fitting xRAPM Model ===\n")

# Use the same RAPM data (design matrix)
rapm_data <- rapm_results$rapm_data

# Fit RAPM with SPM priors
xrapm_model <- fit_rapm_with_prior(
  rapm_data,
  offense_prior = offense_prior,
  defense_prior = defense_prior,
  alpha = 0,           # Ridge regression
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE
)

# 5. Extract xRAPM Ratings ----

cat("\n=== xRAPM Ratings ===\n")

xrapm_ratings <- extract_xrapm_ratings(xrapm_model, lambda = xrapm_lambda)

cat("\nTop 25 by xRAPM:\n")
print(
  xrapm_ratings %>%
    head(25) %>%
    select(player_name, xrapm, offense, defense, off_deviation, def_deviation, total_minutes)
)

cat("\nBottom 25 by xRAPM:\n")
print(
  xrapm_ratings %>%
    tail(25) %>%
    select(player_name, xrapm, offense, defense, off_deviation, def_deviation, total_minutes)
)

# 6. Compare xRAPM vs Base RAPM ----

cat("\n=== xRAPM vs Base RAPM ===\n")

base_ratings <- rapm_results$ratings %>%
  select(player_name, base_rapm = rapm, base_off = offense, base_def = defense)

comparison <- xrapm_ratings %>%
  select(player_name, xrapm, xrapm_off = offense, xrapm_def = defense,
         off_deviation, def_deviation, off_prior, def_prior, total_minutes) %>%
  inner_join(base_ratings, by = "player_name") %>%
  mutate(
    rating_diff = xrapm - base_rapm,
    off_diff = xrapm_off - base_off,
    def_diff = xrapm_def - base_def
  )

cat("\nCorrelation: xRAPM vs Base RAPM:", round(cor(comparison$xrapm, comparison$base_rapm), 3), "\n")
cat("Correlation: xRAPM Offense vs Base Offense:", round(cor(comparison$xrapm_off, comparison$base_off), 3), "\n")
cat("Correlation: xRAPM Defense vs Base Defense:", round(cor(comparison$xrapm_def, comparison$base_def), 3), "\n")

# Players with largest changes
cat("\nPlayers most improved by xRAPM (moved up from prior):\n")
print(
  comparison %>%
    arrange(desc(rating_diff)) %>%
    head(15) %>%
    select(player_name, xrapm, base_rapm, rating_diff, off_deviation, def_deviation)
)

cat("\nPlayers most penalized by xRAPM (moved down from prior):\n")
print(
  comparison %>%
    arrange(rating_diff) %>%
    head(15) %>%
    select(player_name, xrapm, base_rapm, rating_diff, off_deviation, def_deviation)
)

# 7. Team-Level Validation ----

cat("\n=== Team-Level Validation ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

if (!is.null(processed_data$lineups)) {
  # Get primary team for each player
  player_teams <- processed_data$lineups %>%
    group_by(player_name, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_name) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_name, primary_team = team)

  # Team-level xRAPM
  team_xrapm <- xrapm_ratings %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      sum_xrapm = sum(xrapm),
      mean_xrapm = mean(xrapm),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_xrapm))

  # Team-level base RAPM for comparison
  team_rapm <- rapm_results$ratings %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      sum_rapm = sum(rapm),
      mean_rapm = mean(rapm),
      .groups = "drop"
    )

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
      inner_join(team_xrapm %>% select(primary_team, sum_xrapm), by = c("team" = "primary_team")) %>%
      inner_join(team_rapm %>% select(primary_team, sum_rapm), by = c("team" = "primary_team"))

    cat("\nTeam correlations:\n")
    cat("  npxGD vs Sum xRAPM:", round(cor(team_comparison$total_npxgd, team_comparison$sum_xrapm), 3), "\n")
    cat("  npxGD vs Sum RAPM:", round(cor(team_comparison$total_npxgd, team_comparison$sum_rapm), 3), "\n")

    cat("\nTeam rankings:\n")
    print(
      team_comparison %>%
        mutate(
          npxgd_rank = rank(-total_npxgd),
          xrapm_rank = rank(-sum_xrapm),
          rapm_rank = rank(-sum_rapm)
        ) %>%
        arrange(npxgd_rank) %>%
        select(team, total_npxgd, sum_xrapm, sum_rapm, npxgd_rank, xrapm_rank, rapm_rank)
    )
  }
}

# 8. Save Results ----

cat("\n=== Saving Results ===\n")

xrapm_results <- list(
  model = xrapm_model,
  ratings = xrapm_ratings,
  comparison = comparison,
  offense_prior = offense_prior,
  defense_prior = defense_prior,
  team_xrapm = if (exists("team_xrapm")) team_xrapm else NULL,
  team_comparison = if (exists("team_comparison")) team_comparison else NULL
)

saveRDS(xrapm_results, file.path(cache_dir, "06_xrapm.rds"))
cat("Saved to cache/06_xrapm.rds\n")

cat("\n=== COMPLETE ===\n")
