# 06_xrapm.R
# Fit xRAPM (RAPM with SPM prior)
#
# xRAPM uses SPM predictions as a Bayesian prior for RAPM fitting.
# This helps separate players who always appear together (teammate confounding)
# by leveraging box score statistics.

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# =============================================================================
# 1. Load Data
# =============================================================================
cat("\n=== 1. Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Players with RAPM:", nrow(rapm_results$ratings), "\n")
cat("Players with SPM:", nrow(spm_results$spm_ratings), "\n")

# =============================================================================
# 2. Create SPM Priors
# =============================================================================
cat("\n=== 2. Creating SPM Priors ===\n")

# Get player mapping from RAPM data
player_mapping <- rapm_results$rapm_data$player_mapping

# Create offense prior (keyed by player_id)
offense_spm <- spm_results$offense_spm_ratings

# Match player names to player IDs
name_to_id <- setNames(
  player_mapping$player_id,
  player_mapping$player_name
)

# Build offense prior vector
offense_prior <- rep(0, length(unique(player_mapping$player_id)))
names(offense_prior) <- unique(player_mapping$player_id)

for (i in seq_len(nrow(offense_spm))) {
  pname <- offense_spm$player_name[i]
  if (pname %in% names(name_to_id)) {
    pid <- name_to_id[pname]
    offense_prior[pid] <- offense_spm$offense_spm[i]
  }
}

# Build defense prior vector
defense_spm <- spm_results$defense_spm_ratings
defense_prior <- rep(0, length(unique(player_mapping$player_id)))
names(defense_prior) <- unique(player_mapping$player_id)

for (i in seq_len(nrow(defense_spm))) {
  pname <- defense_spm$player_name[i]
  if (pname %in% names(name_to_id)) {
    pid <- name_to_id[pname]
    defense_prior[pid] <- defense_spm$defense_spm[i]
  }
}

cat("Offense priors set:", sum(offense_prior != 0), "\n")
cat("Defense priors set:", sum(defense_prior != 0), "\n")

# Summary of prior values
cat("\nOffense prior summary:\n")
print(summary(offense_prior[offense_prior != 0]))
cat("\nDefense prior summary:\n")
print(summary(defense_prior[defense_prior != 0]))

# =============================================================================
# 3. Fit xRAPM Model
# =============================================================================
cat("\n=== 3. Fitting xRAPM Model ===\n")

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

# =============================================================================
# 4. Extract xRAPM Ratings
# =============================================================================
cat("\n=== 4. xRAPM Ratings ===\n")

xrapm_ratings <- extract_xrapm_ratings(xrapm_model)

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

# =============================================================================
# 5. Compare xRAPM vs Base RAPM
# =============================================================================
cat("\n=== 5. xRAPM vs Base RAPM ===\n")

base_ratings <- rapm_results$ratings %>%
  select(player_name, base_panna = panna, base_off = offense, base_def = defense)

comparison <- xrapm_ratings %>%
  select(player_name, xrapm, xrapm_off = offense, xrapm_def = defense,
         off_deviation, def_deviation, off_prior, def_prior, total_minutes) %>%
  inner_join(base_ratings, by = "player_name") %>%
  mutate(
    rating_diff = xrapm - base_panna,
    off_diff = xrapm_off - base_off,
    def_diff = xrapm_def - base_def
  )

cat("\nCorrelation: xRAPM vs Base RAPM:", round(cor(comparison$xrapm, comparison$base_panna), 3), "\n")
cat("Correlation: xRAPM Offense vs Base Offense:", round(cor(comparison$xrapm_off, comparison$base_off), 3), "\n")
cat("Correlation: xRAPM Defense vs Base Defense:", round(cor(comparison$xrapm_def, comparison$base_def), 3), "\n")

# Players with largest changes
cat("\nPlayers most improved by xRAPM (moved up from prior):\n")
print(
  comparison %>%
    arrange(desc(rating_diff)) %>%
    head(15) %>%
    select(player_name, xrapm, base_panna, rating_diff, off_deviation, def_deviation)
)

cat("\nPlayers most penalized by xRAPM (moved down from prior):\n")
print(
  comparison %>%
    arrange(rating_diff) %>%
    head(15) %>%
    select(player_name, xrapm, base_panna, rating_diff, off_deviation, def_deviation)
)

# =============================================================================
# 6. Team-Level Validation
# =============================================================================
cat("\n=== 6. Team-Level Validation ===\n")

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
      sum_panna = sum(panna),
      mean_panna = mean(panna),
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
      inner_join(team_rapm %>% select(primary_team, sum_panna), by = c("team" = "primary_team"))

    cat("\nTeam correlations:\n")
    cat("  npxGD vs Sum xRAPM:", round(cor(team_comparison$total_npxgd, team_comparison$sum_xrapm), 3), "\n")
    cat("  npxGD vs Sum RAPM:", round(cor(team_comparison$total_npxgd, team_comparison$sum_panna), 3), "\n")

    cat("\nTeam rankings:\n")
    print(
      team_comparison %>%
        mutate(
          npxgd_rank = rank(-total_npxgd),
          xrapm_rank = rank(-sum_xrapm),
          rapm_rank = rank(-sum_panna)
        ) %>%
        arrange(npxgd_rank) %>%
        select(team, total_npxgd, sum_xrapm, sum_panna, npxgd_rank, xrapm_rank, rapm_rank)
    )
  }
}

# =============================================================================
# 7. Save Results
# =============================================================================
cat("\n=== 7. Saving Results ===\n")

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
