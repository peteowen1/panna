# 08_rapm_v2.R
# New RAPM pipeline with restructured design matrix
#
# Structure:
# - 2 rows per stint (one per team's attacking perspective)
# - Target: xgf90 (xG FOR per 90)
# - Covariates: gd, gf, ga, avg_min, is_home
# - Player columns: playerX_off, playerX_def
# - Final rating: panna = offense - defense

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# =============================================================================
# 1. Re-create splints with game state tracking
# =============================================================================
cat("\n=== 1. Creating Splints with Game State ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

splint_data <- create_all_splints(
  processed_data,
  include_goals = TRUE,
  verbose = TRUE
)

# Validate game state columns exist
cat("\nSplint columns:", paste(names(splint_data$splints), collapse = ", "), "\n")
has_game_state <- all(c("gf_home", "ga_home", "avg_min") %in% names(splint_data$splints))
cat("Game state columns present:", has_game_state, "\n")

if (!has_game_state) {
  stop("Game state columns missing from splint data!")
}

# Save updated splint data
saveRDS(splint_data, file.path(cache_dir, "03_splints.rds"))

# =============================================================================
# 2. Create new design matrix
# =============================================================================
cat("\n=== 2. Creating RAPM Design Matrix (v2) ===\n")

rapm_data <- prepare_rapm_data_v2(
  splint_data,
  min_minutes = 90,
  include_covariates = TRUE
)

cat("\nDesign matrix summary:\n")
cat("  Rows:", rapm_data$n_rows, "\n")
cat("  Players:", rapm_data$n_players, "\n")
cat("  Player columns:", rapm_data$n_players * 2, "(off + def)\n")
cat("  Covariates:", length(rapm_data$covariate_names), "\n")
cat("  Total columns:", ncol(rapm_data$X_full), "\n")

cat("\nResponse (xgf90) summary:\n")
print(summary(rapm_data$y))

cat("\nSample of row data:\n")
print(head(rapm_data$row_data, 10))

# =============================================================================
# 3. Fit RAPM model
# =============================================================================
cat("\n=== 3. Fitting RAPM v2 Model ===\n")

model <- fit_rapm_v2(
  rapm_data,
  alpha = 0,           # Ridge regression
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE  # Don't penalize gd, gf, ga, avg_min, is_home
)

# =============================================================================
# 4. Extract covariate effects
# =============================================================================
cat("\n=== 4. Covariate Effects ===\n")

cov_effects <- get_covariate_effects(model)
cat("\nGame state effects on xGF/90:\n")
for (name in names(cov_effects)) {
  cat(sprintf("  %s: %.4f\n", name, cov_effects[name]))
}

# Interpretation:
# gd > 0: teams create more xG when ahead (pressing less?)
# gd < 0: teams create less xG when ahead (sitting back)
# avg_min > 0: more xG later in games
# is_home > 0: home advantage in xG creation

# =============================================================================
# 5. Extract player ratings
# =============================================================================
cat("\n=== 5. Player Ratings (Panna) ===\n")

ratings <- extract_panna_ratings(model)

cat("\nTop 25 players by Panna rating:\n")
print(
  ratings %>%
    head(25) %>%
    select(player_name, panna, offense, defense, total_minutes)
)

cat("\nBottom 25 players:\n")
print(
  ratings %>%
    tail(25) %>%
    select(player_name, panna, offense, defense, total_minutes)
)

# =============================================================================
# 6. Team-level validation
# =============================================================================
cat("\n=== 6. Team-Level Validation ===\n")

# Get primary team for each player
if (!is.null(processed_data$lineups)) {
  player_teams <- processed_data$lineups %>%
    group_by(player_name, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_name) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_name, primary_team = team)

  # Team-level ratings
  team_ratings <- ratings %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      mean_panna = mean(panna),
      sum_panna = sum(panna),
      mean_offense = mean(offense),
      mean_defense = mean(defense),
      top_player = player_name[which.max(panna)],
      .groups = "drop"
    ) %>%
    arrange(desc(mean_panna))

  cat("\nTeam average Panna:\n")
  print(team_ratings, n = 25)

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
        team_ratings %>% select(primary_team, sum_panna, mean_panna),
        by = c("team" = "primary_team")
      )

    cat("\nCorrelation: Team npxGD vs Sum of Player Panna:\n")
    cat(sprintf("  r = %.3f\n", cor(team_comparison$total_npxgd, team_comparison$sum_panna)))

    cat("\nTeam ranking comparison:\n")
    team_comparison <- team_comparison %>%
      mutate(
        npxgd_rank = rank(-total_npxgd),
        panna_rank = rank(-sum_panna),
        rank_diff = npxgd_rank - panna_rank
      ) %>%
      arrange(npxgd_rank)

    print(
      team_comparison %>%
        select(team, total_npxgd, sum_panna, npxgd_rank, panna_rank, rank_diff)
    )
  }
}

# =============================================================================
# 7. Save results
# =============================================================================
cat("\n=== 7. Saving Results ===\n")

results_v2 <- list(
  rapm_data = rapm_data,
  model = model,
  ratings = ratings,
  covariate_effects = cov_effects,
  team_ratings = if (exists("team_ratings")) team_ratings else NULL,
  team_comparison = if (exists("team_comparison")) team_comparison else NULL
)

saveRDS(results_v2, file.path(cache_dir, "08_rapm_v2.rds"))

cat("Results saved to cache/08_rapm_v2.rds\n")
cat("\n=== COMPLETE ===\n")
