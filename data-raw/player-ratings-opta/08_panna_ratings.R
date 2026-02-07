# 08_panna_ratings.R
# Final Opta Panna ratings combining all components
#
# Near-identical to FBref version. Combines xRAPM + RAPM + SPM
# into the final ratings table with team-level validation.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# 2. Load All Results ----

cat("\n=== Loading Results ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))
xrapm_results <- readRDS(file.path(cache_dir, "06_xrapm.rds"))

cat("Base RAPM players:", nrow(rapm_results$ratings), "\n")
cat("SPM players:", nrow(spm_results$spm_ratings), "\n")
cat("xRAPM players:", nrow(xrapm_results$ratings), "\n")

# 3. Create Unified Ratings Table ----

cat("\n=== Creating Unified Ratings ===\n")

panna_ratings <- xrapm_results$ratings %>%
  select(
    player_id,
    player_name,
    total_minutes,
    panna = xrapm,
    offense = offense,
    defense = defense,
    off_deviation,
    def_deviation,
    off_prior,
    def_prior
  )

# Add base RAPM for comparison (deduplicate by player_name to avoid many-to-many)
base_rapm <- rapm_results$ratings %>%
  group_by(player_name) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_name, base_rapm = rapm, base_offense = offense, base_defense = defense)

panna_ratings <- panna_ratings %>%
  left_join(base_rapm, by = "player_name")

# Add overall SPM prediction (deduplicate similarly)
spm_overall <- spm_results$spm_ratings %>%
  group_by(player_name) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_name, spm_overall = spm)

panna_ratings <- panna_ratings %>%
  left_join(spm_overall, by = "player_name")

# Calculate percentiles and ranks
panna_ratings <- panna_ratings %>%
  mutate(
    panna_percentile = 100 * rank(panna) / n(),
    panna_rank = rank(-panna)
  ) %>%
  arrange(desc(panna))

cat("Final ratings for", nrow(panna_ratings), "players\n")

# 4. Summary Statistics ----

cat("\n=== Rating Distributions ===\n")

cat("\nPanna rating distribution:\n")
print(summary(panna_ratings$panna))

cat("\nOffense rating distribution:\n")
print(summary(panna_ratings$offense))

cat("\nDefense rating distribution:\n")
print(summary(panna_ratings$defense))

cat("\nCorrelations:\n")
cat("  Panna vs Base RAPM:", round(cor(panna_ratings$panna, panna_ratings$base_rapm, use = "complete"), 3), "\n")
cat("  Panna vs SPM:", round(cor(panna_ratings$panna, panna_ratings$spm_overall, use = "complete"), 3), "\n")
cat("  Offense vs Defense:", round(cor(panna_ratings$offense, panna_ratings$defense, use = "complete"), 3), "\n")

# 5. Player Rankings ----

cat("\n=== Player Rankings ===\n")

cat("\nTop 25 by Panna:\n")
print(
  panna_ratings %>%
    head(25) %>%
    select(panna_rank, player_name, panna, offense, defense, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("rank|minutes"), ~round(.x, 3)))
)

cat("\nBottom 25 by Panna:\n")
print(
  panna_ratings %>%
    tail(25) %>%
    select(panna_rank, player_name, panna, offense, defense, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("rank|minutes"), ~round(.x, 3)))
)

cat("\nTop 15 Offensive Players:\n")
print(
  panna_ratings %>%
    arrange(desc(offense)) %>%
    head(15) %>%
    select(player_name, offense, defense, panna, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("minutes"), ~round(.x, 3)))
)

cat("\nTop 15 Defensive Players (lowest = best):\n")
print(
  panna_ratings %>%
    arrange(defense) %>%
    head(15) %>%
    select(player_name, defense, offense, panna, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("minutes"), ~round(.x, 3)))
)

# 6. Team-Level Validation ----

cat("\n=== Team-Level Validation ===\n")

if (!is.null(processed_data$lineups)) {
  player_teams <- processed_data$lineups %>%
    mutate(team = if ("team_name" %in% names(.)) team_name else team) %>%
    group_by(player_name, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_name) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_name, primary_team = team)

  team_panna <- panna_ratings %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      total_minutes = sum(total_minutes),
      sum_panna = sum(panna),
      mean_panna = mean(panna),
      sum_offense = sum(offense),
      sum_defense = sum(defense),
      top_player = player_name[which.max(panna)],
      top_panna = max(panna),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_panna))

  cat("\nTeam Panna ratings (sum of player ratings):\n")
  print(team_panna %>% select(primary_team, n_players, sum_panna, mean_panna, top_player))

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

    team_validation <- team_npxgd %>%
      inner_join(
        team_panna %>% select(primary_team, sum_panna, sum_offense, sum_defense),
        by = c("team" = "primary_team")
      ) %>%
      mutate(
        npxgd_rank = rank(-total_npxgd),
        panna_rank = rank(-sum_panna),
        rank_diff = npxgd_rank - panna_rank
      ) %>%
      arrange(npxgd_rank)

    cat("\n--- Team Validation Metrics ---\n")
    cat("npxGD vs Sum Panna:   r =", round(cor(team_validation$total_npxgd, team_validation$sum_panna), 3), "\n")
    cat("npxGD vs Sum Offense: r =", round(cor(team_validation$total_npxgd, team_validation$sum_offense), 3), "\n")
    cat("npxGD vs Sum Defense: r =", round(cor(team_validation$total_npxgd, -team_validation$sum_defense), 3), "\n")

    cat("\nTeam ranking comparison:\n")
    print(
      team_validation %>%
        select(team, total_npxgd, sum_panna, npxgd_rank, panna_rank, rank_diff) %>%
        mutate(across(c(total_npxgd, sum_panna), ~round(.x, 2)))
    )
  }
}

# 7. Save Final Results ----

cat("\n=== Saving Final Results ===\n")

final_results <- list(
  panna_ratings = panna_ratings,
  team_panna = if (exists("team_panna")) team_panna else NULL,
  team_validation = if (exists("team_validation")) team_validation else NULL,
  covariate_effects = rapm_results$covariate_effects,
  metadata = list(
    source = "opta",
    n_players = nrow(panna_ratings),
    total_minutes = sum(panna_ratings$total_minutes),
    created = Sys.time()
  )
)

saveRDS(final_results, file.path(cache_dir, "08_panna.rds"))

panna_export <- panna_ratings %>%
  select(
    panna_rank,
    player_name,
    panna,
    offense,
    defense,
    spm_overall,
    total_minutes,
    panna_percentile
  ) %>%
  mutate(across(where(is.numeric) & !matches("rank|minutes|percentile"), ~round(.x, 4)))

write.csv(panna_export, file.path(cache_dir, "panna_ratings.csv"), row.names = FALSE)

cat("Saved:\n")
cat("  -", file.path(cache_dir, "08_panna.rds"), "\n")
cat("  -", file.path(cache_dir, "panna_ratings.csv"), "\n")

cat("\n=== COMPLETE ===\n")
cat("\nOpta Pipeline Summary:\n")
cat("  01_load_opta_data.R    -> Raw Opta data + SPADL xG scoring\n")
cat("  02_data_processing.R   -> Processed data (Opta adapters)\n")
cat("  03_splint_creation.R   -> Splints for RAPM\n")
cat("  04_rapm.R              -> Base RAPM (SPADL-derived xG target)\n")
cat("  05_spm.R               -> Opta SPM (80+ features + xMetrics)\n")
cat("  06_xrapm.R             -> xRAPM (RAPM with Opta SPM prior)\n")
cat("  07_seasonal_ratings.R  -> Per-season ratings\n")
cat("  08_panna_ratings.R     -> Final Opta Panna ratings\n")
