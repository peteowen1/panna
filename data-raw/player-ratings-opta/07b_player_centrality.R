# 07b_player_centrality.R
# Calculate network centrality scores for rated players
#
# DELIBERATELY OPT-IN / MANUAL: not run by any workflow. Its cache
# (cache-opta/07b_centrality.rds) is optionally consumed by predictions step
# 02 (02_player_ratings_to_team.R) if present -- centrality features are 0 in
# cloud runs by design (decision 2026-07-14).
#
# Runs after step 07 (seasonal ratings) to compute opponent quality
# adjustment scores. Centrality can be used as:
# - A multiplier on RAPM/SPM ratings (quality-adjusted rating)
# - A standalone feature in match prediction models
# - A filter to identify inflated ratings from weak competition

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Check Cache ----

cache_dir <- file.path("data-raw", "cache-opta")
output_path <- file.path(cache_dir, "07b_centrality.rds")

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 07b_centrality.rds")
  centrality_results <- readRDS(output_path)
  message(sprintf("  %d players with centrality scores", nrow(centrality_results)))
  return(invisible(NULL))
}

# 3. Load Lineup Data ----

message("\n=== Calculating Player Centrality ===\n")

# Build player-match data from lineups
# We need: player_id, team, opponent, match_id
lineups_path <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (!file.exists(lineups_path)) {
  message("  No raw data cache found - skipping centrality")
  return(invisible(NULL))
}

raw_data <- readRDS(lineups_path)

# Extract player-match records from lineups
if (is.null(raw_data$lineups) || nrow(raw_data$lineups) == 0) {
  message("  No lineup data available - skipping centrality")
  return(invisible(NULL))
}

lineups <- raw_data$lineups

# Build player_matches data frame
# Each row: player appeared in a match with their team vs opponent
player_matches <- lineups %>%
  select(
    player_id = any_of(c("player_id", "player_href")),
    team = any_of(c("team")),
    match_id = any_of(c("match_id", "match_url")),
    minutes = any_of(c("minutes", "min"))
  )

if (nrow(player_matches) == 0 || !"player_id" %in% names(player_matches)) {
  message("  Could not build player_matches - skipping centrality")
  return(invisible(NULL))
}

# Add opponent (other team in same match)
match_teams <- player_matches %>%
  distinct(match_id, team) %>%
  group_by(match_id) %>%
  mutate(opponent = rev(team)) %>%
  ungroup()

player_matches <- player_matches %>%
  left_join(match_teams %>% select(match_id, team, opponent), by = c("match_id", "team"))

# Filter out any NA opponents
player_matches <- player_matches %>%
  filter(!is.na(opponent), !is.na(player_id))

message(sprintf("  Built player network: %d player-match records",
                nrow(player_matches)))

# 4. Calculate Centrality ----

centrality_results <- tryCatch(
  calculate_player_centrality(
    player_matches,
    min_matches = 5L,
    damping = 0.85
  ),
  error = function(e) {
    message(sprintf("  Centrality calculation failed: %s", e$message))
    NULL
  }
)

if (is.null(centrality_results)) {
  return(invisible(NULL))
}

# 5. Save Results ----

saveRDS(centrality_results, output_path)
message(sprintf("  Saved %d centrality scores to %s",
                nrow(centrality_results), basename(output_path)))
message(sprintf("  Centrality range: [%.3f, %.3f]",
                min(centrality_results$centrality),
                max(centrality_results$centrality)))
