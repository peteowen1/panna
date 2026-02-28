# 05_skill_panna_ratings.R
# Final skill-based Panna ratings
#
# Combines skill-based xRAPM ratings with team-level validation.
# Parallel to 08_panna_ratings.R in the Opta pipeline.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")

# 3. Load Results ----

cat("\n=== Loading Results ===\n")

processed_data <- readRDS(file.path(opta_cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(opta_cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "03_skill_spm.rds"))
xrapm_results <- readRDS(file.path(cache_dir, "04_skill_xrapm.rds"))

cat("Base RAPM players:", nrow(rapm_results$ratings), "\n")
cat("Skill SPM players:", nrow(spm_results$spm_ratings), "\n")
cat("Skill xRAPM players:", nrow(xrapm_results$ratings), "\n")

# 4. Create Unified Ratings ----

cat("\n=== Creating Unified Skill-Based Ratings ===\n")

panna_ratings <- xrapm_results$ratings %>%
  select(
    player_id, player_name, total_minutes,
    panna = xrapm, offense = offense, defense = defense,
    off_deviation, def_deviation, off_prior, def_prior
  )

# Add base RAPM
base_rapm <- rapm_results$ratings %>%
  group_by(player_id) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, base_rapm = rapm, base_offense = offense, base_defense = defense)

panna_ratings <- panna_ratings %>%
  left_join(base_rapm, by = "player_id")

# Add skill SPM
spm_overall <- spm_results$spm_ratings %>%
  group_by(player_id) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_id, spm_overall = spm)

panna_ratings <- panna_ratings %>%
  left_join(spm_overall, by = "player_id")

# Ranks and percentiles
panna_ratings <- panna_ratings %>%
  mutate(
    panna_percentile = 100 * rank(panna) / n(),
    panna_rank = rank(-panna)
  ) %>%
  arrange(desc(panna))

cat("Final skill-based ratings for", nrow(panna_ratings), "players\n")

# 5. Summary ----

cat("\n=== Rating Distributions ===\n")
cat("\nPanna:\n"); print(summary(panna_ratings$panna))
cat("\nOffense:\n"); print(summary(panna_ratings$offense))
cat("\nDefense:\n"); print(summary(panna_ratings$defense))

cat("\nCorrelations:\n")
cat("  Panna vs Base RAPM:", round(cor(panna_ratings$panna, panna_ratings$base_rapm, use = "complete"), 3), "\n")
cat("  Panna vs Skill SPM:", round(cor(panna_ratings$panna, panna_ratings$spm_overall, use = "complete"), 3), "\n")

# 6. Player Rankings ----

cat("\n=== Top 25 by Skill-Based Panna ===\n")
print(
  panna_ratings %>%
    head(25) %>%
    select(panna_rank, player_name, panna, offense, defense, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("rank|minutes"), ~round(.x, 3)))
)

# 7. Compare with Raw Panna ----

cat("\n=== Comparison with Raw-Stat Panna ===\n")

opta_panna_path <- file.path(opta_cache_dir, "08_panna.rds")
if (file.exists(opta_panna_path)) {
  raw_panna <- readRDS(opta_panna_path)

  comp <- panna_ratings %>%
    select(player_id, player_name, skill_panna = panna) %>%
    inner_join(
      raw_panna$panna_ratings %>% select(player_id, raw_panna = panna),
      by = "player_id"
    )

  cat(sprintf("Skill vs Raw Panna: r = %.3f\n", cor(comp$skill_panna, comp$raw_panna)))

  # Players most changed
  comp <- comp %>% mutate(diff = skill_panna - raw_panna)
  cat("\nMost improved by skill estimation:\n")
  print(head(comp %>% arrange(desc(diff)) %>% select(player_name, skill_panna, raw_panna, diff), 10))
  cat("\nMost penalized by skill estimation:\n")
  print(head(comp %>% arrange(diff) %>% select(player_name, skill_panna, raw_panna, diff), 10))
}

# 8. Team Validation ----

cat("\n=== Team-Level Validation ===\n")

if (!is.null(processed_data$lineups)) {
  player_teams <- processed_data$lineups %>%
    mutate(team = if ("team_name" %in% names(.)) team_name else team) %>%
    group_by(player_id, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_id) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_id, primary_team = team)

  team_panna <- panna_ratings %>%
    left_join(player_teams, by = "player_id") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(), sum_panna = sum(panna),
      mean_panna = mean(panna), .groups = "drop"
    ) %>%
    arrange(desc(sum_panna))

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
      inner_join(team_panna %>% select(primary_team, sum_panna), by = c("team" = "primary_team"))

    cat("npxGD vs Sum Skill Panna: r =", round(cor(team_validation$total_npxgd, team_validation$sum_panna), 3), "\n")
  }
}

# 9. Save ----

cat("\n=== Saving Final Results ===\n")

final_results <- list(
  panna_ratings = panna_ratings,
  team_panna = if (exists("team_panna")) team_panna else NULL,
  team_validation = if (exists("team_validation")) team_validation else NULL,
  metadata = list(
    source = "opta_skills",
    n_players = nrow(panna_ratings),
    total_minutes = sum(panna_ratings$total_minutes),
    created = Sys.time()
  )
)

saveRDS(final_results, file.path(cache_dir, "05_skill_panna.rds"))

panna_export <- panna_ratings %>%
  select(panna_rank, player_name, panna, offense, defense,
         spm_overall, total_minutes, panna_percentile) %>%
  mutate(across(where(is.numeric) & !matches("rank|minutes|percentile"), ~round(.x, 4)))

write.csv(panna_export, file.path(cache_dir, "skill_panna_ratings.csv"), row.names = FALSE)

cat("Saved:\n")
cat("  -", file.path(cache_dir, "05_skill_panna.rds"), "\n")
cat("  -", file.path(cache_dir, "skill_panna_ratings.csv"), "\n")

cat("\n=== COMPLETE ===\n")
