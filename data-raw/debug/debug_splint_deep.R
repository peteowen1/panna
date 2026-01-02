# debug_splint_deep.R
# Deep dive into splint data quality

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load data
cat("\n=== Loading Data ===\n")
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

splints <- splint_data$splints
players <- splint_data$players
match_info <- splint_data$match_info

cat("Total splints:", nrow(splints), "\n")
cat("Total player-splint rows:", nrow(players), "\n")
cat("Total matches:", nrow(match_info), "\n")

# =============================================================================
# 1. SPLINT DURATION ANALYSIS
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 1. SPLINT DURATION ANALYSIS\n")
cat("###############################################################\n")

cat("\nDuration summary (minutes):\n")
print(summary(splints$duration))

cat("\nSplints by duration bucket:\n")
splints_bucketed <- splints %>%
  mutate(duration_bucket = cut(duration, breaks = c(0, 5, 10, 20, 30, 45, 90, Inf),
                                labels = c("0-5", "5-10", "10-20", "20-30", "30-45", "45-90", "90+")))
print(table(splints_bucketed$duration_bucket))

cat("\nTop 20 longest splints:\n")
print(
  splints %>%
    arrange(desc(duration)) %>%
    head(20) %>%
    select(match_id, splint_num, duration, npxg_home, npxg_away, npxgd, npxgd_per_90)
)

# =============================================================================
# 2. NPXGD DISTRIBUTION
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 2. NPXGD DISTRIBUTION\n")
cat("###############################################################\n")

cat("\nnpxgd_per_90 summary:\n")
print(summary(splints$npxgd_per_90))

cat("\nQuantiles:\n")
print(quantile(splints$npxgd_per_90, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm = TRUE))

cat("\nStandard deviation:", sd(splints$npxgd_per_90, na.rm = TRUE), "\n")

# Splints with extreme npxgd
cat("\nMost positive npxgd_per_90 (top 10):\n")
print(
  splints %>%
    arrange(desc(npxgd_per_90)) %>%
    head(10) %>%
    select(match_id, splint_num, duration, npxg_home, npxg_away, npxgd, npxgd_per_90)
)

cat("\nMost negative npxgd_per_90 (bottom 10):\n")
print(
  splints %>%
    arrange(npxgd_per_90) %>%
    head(10) %>%
    select(match_id, splint_num, duration, npxg_home, npxg_away, npxgd, npxgd_per_90)
)

# Zero xG splints
zero_both <- splints %>% filter(npxg_home == 0 & npxg_away == 0)
cat("\nSplints with zero xG (both teams):", nrow(zero_both), "/", nrow(splints),
    sprintf("(%.1f%%)\n", 100 * nrow(zero_both) / nrow(splints)))

# =============================================================================
# 3. PLAYER ASSIGNMENT ANALYSIS - CRITICAL!
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 3. PLAYER ASSIGNMENT ANALYSIS - CRITICAL!\n")
cat("###############################################################\n")

# How many players per splint?
players_per_splint <- players %>%
  group_by(splint_id) %>%
  summarise(
    n_players = n(),
    n_home = sum(is_home),
    n_away = sum(!is_home),
    .groups = "drop"
  )

cat("\nPlayers per splint summary:\n")
print(summary(players_per_splint$n_players))

cat("\nHome players per splint:\n")
print(summary(players_per_splint$n_home))

cat("\nAway players per splint:\n")
print(summary(players_per_splint$n_away))

# Distribution of player counts
cat("\nDistribution of player counts per splint:\n")
print(table(players_per_splint$n_players))

# Splints that should have 22 players (11 vs 11)
correct_22 <- sum(players_per_splint$n_players == 22)
cat("\nSplints with exactly 22 players:", correct_22, "/", nrow(splints),
    sprintf("(%.1f%%)\n", 100 * correct_22 / nrow(splints)))

# Splints with wrong player count
wrong_count <- players_per_splint %>%
  filter(n_players != 22) %>%
  arrange(desc(n_players))

if (nrow(wrong_count) > 0) {
  cat("\nSplints with != 22 players (sample):\n")
  print(head(wrong_count, 20))
}

# =============================================================================
# 4. PLAYER PARTICIPATION
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 4. PLAYER PARTICIPATION\n")
cat("###############################################################\n")

# Players with most splints
player_splint_counts <- players %>%
  group_by(player_id, player_name) %>%
  summarise(
    n_splints = n_distinct(splint_id),
    n_matches = n_distinct(match_id),
    .groups = "drop"
  ) %>%
  arrange(desc(n_splints))

cat("\nTop 20 players by number of splints:\n")
print(head(player_splint_counts, 20))

cat("\nPlayers per match average splints:\n")
player_splint_counts <- player_splint_counts %>%
  mutate(splints_per_match = n_splints / n_matches)
cat("Average:", mean(player_splint_counts$splints_per_match), "\n")

# =============================================================================
# 5. MATCH-LEVEL SANITY CHECK
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 5. MATCH-LEVEL SANITY CHECK\n")
cat("###############################################################\n")

match_splint_summary <- splints %>%
  group_by(match_id) %>%
  summarise(
    n_splints = n(),
    total_duration = sum(duration),
    total_npxg_home = sum(npxg_home, na.rm = TRUE),
    total_npxg_away = sum(npxg_away, na.rm = TRUE),
    total_npxgd = sum(npxgd, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nSplints per match:\n")
print(summary(match_splint_summary$n_splints))

cat("\nTotal duration per match (should be ~90):\n")
print(summary(match_splint_summary$total_duration))

# Matches with wrong total duration
wrong_duration <- match_splint_summary %>%
  filter(abs(total_duration - 90) > 5) %>%
  arrange(desc(abs(total_duration - 90)))

if (nrow(wrong_duration) > 0) {
  cat("\nMatches with duration != 90 (sample):\n")
  print(head(wrong_duration, 10))
}

# Compare match-level npxGD with processed results
cat("\n=== Validating match xG totals ===\n")
match_comparison <- match_splint_summary %>%
  left_join(
    match_info %>% select(match_id, home_team, away_team),
    by = "match_id"
  ) %>%
  left_join(
    processed_data$results %>% select(match_id, home_xg, away_xg),
    by = "match_id"
  ) %>%
  mutate(
    xg_diff_home = total_npxg_home - home_xg,
    xg_diff_away = total_npxg_away - away_xg
  )

cat("\nDifference between splint total and match xG (home):\n")
print(summary(match_comparison$xg_diff_home))

cat("\nDifference between splint total and match xG (away):\n")
print(summary(match_comparison$xg_diff_away))

# =============================================================================
# 6. SINGLE MATCH DEEP DIVE
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 6. SINGLE MATCH DEEP DIVE\n")
cat("###############################################################\n")

# Pick a match with typical number of splints
sample_match <- match_splint_summary %>%
  filter(n_splints >= 5, n_splints <= 10) %>%
  slice(1) %>%
  pull(match_id)

cat("Sample match:", sample_match, "\n\n")

match_splints <- splints %>% filter(match_id == sample_match)
match_players <- players %>% filter(match_id == sample_match)

cat("Match splints:\n")
print(match_splints %>% select(splint_num, start_minute, end_minute, duration, npxg_home, npxg_away, npxgd))

cat("\nPlayers per splint in this match:\n")
match_player_counts <- match_players %>%
  group_by(splint_num) %>%
  summarise(
    n_home = sum(is_home),
    n_away = sum(!is_home),
    home_players = paste(sort(player_name[is_home]), collapse = ", "),
    away_players = paste(sort(player_name[!is_home]), collapse = ", "),
    .groups = "drop"
  )
print(match_player_counts %>% select(splint_num, n_home, n_away))

# Check if players change between splints
cat("\nDo players change between splints?\n")
for (i in 2:nrow(match_player_counts)) {
  prev_home <- strsplit(match_player_counts$home_players[i-1], ", ")[[1]]
  curr_home <- strsplit(match_player_counts$home_players[i], ", ")[[1]]

  added <- setdiff(curr_home, prev_home)
  removed <- setdiff(prev_home, curr_home)

  if (length(added) > 0 || length(removed) > 0) {
    cat(sprintf("Splint %d -> %d: ", i-1, i))
    if (length(removed) > 0) cat("OUT:", paste(removed, collapse=", "), " ")
    if (length(added) > 0) cat("IN:", paste(added, collapse=", "))
    cat("\n")
  } else {
    cat(sprintf("Splint %d -> %d: NO CHANGES (this is the bug!)\n", i-1, i))
  }
}

# =============================================================================
# 7. THE ACTUAL BUG - Compare lineup minutes to splint assignments
# =============================================================================
cat("\n")
cat("###############################################################\n")
cat("# 7. BUG INVESTIGATION - Lineup vs Splint Assignment\n")
cat("###############################################################\n")

# Get lineup data for this match
match_lineups <- processed_data$lineups %>% filter(match_id == sample_match)

cat("\nActual lineup data for this match:\n")
print(
  match_lineups %>%
    arrange(is_home, desc(is_starter), desc(minutes)) %>%
    select(team, is_home, player_name, is_starter, minutes)
)

cat("\nStarters should be in ALL splints, subs should only be in LATER splints\n")
cat("But current implementation puts ALL players in ALL splints!\n")

cat("\n=== DEBUG COMPLETE ===\n")
