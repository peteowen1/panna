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

# NOTE (review R060/R068/R076): there is deliberately NO cache-hit early exit here.
# A top-level return() inside a source()d script does not stop the script, so the
# old branch only logged and then rebuilt anyway. Making it work would serve a stale
# centrality table: the key is file existence only, so it ignores new lineups, and predictions step 02 consumes it.

# One-shot block so the early exits below can `break` out of it. A top-level
# return() inside a source()d script does NOT stop the script (review R068); break
# does, and unlike a function wrapper it keeps <<- and variable scope unchanged.
# (Body deliberately not re-indented, to keep the diff reviewable.)
repeat {
# 3. Load Lineup Data ----

message("\n=== Calculating Player Centrality ===\n")

# Build player-match data from lineups
# We need: player_id, team, opponent, match_id
lineups_path <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (!file.exists(lineups_path)) {
  message("  No raw data cache found - skipping centrality")
  break
}

raw_data <- readRDS(lineups_path)

# Extract player-match records from lineups
if (is.null(raw_data$lineups) || nrow(raw_data$lineups) == 0) {
  message("  No lineup data available - skipping centrality")
  break
}

lineups <- raw_data$lineups

# Build player_matches data frame
# Each row: player appeared in a match with their team vs opponent
# Column names are those of load_opta_lineups() (team_name, minutes_played,
# team_position); the old team/minutes/player_href names do not exist there, so
# the select() silently produced a table with no team column.
needed <- c("player_id", "team_name", "match_id", "team_position")
if (!all(needed %in% names(lineups))) {
  message(sprintf("  Lineups missing columns (%s) - skipping centrality",
                  paste(setdiff(needed, names(lineups)), collapse = ", ")))
  break
}
player_matches <- lineups %>%
  select(
    player_id,
    team = team_name,
    match_id,
    team_position,
    minutes = any_of("minutes_played")
  )

# Add opponent (the other side of the same match, by home/away)
side_teams <- function(side) {
  player_matches %>%
    filter(tolower(team_position) == side) %>%
    distinct(match_id, .keep_all = FALSE, team) %>%
    group_by(match_id) %>% slice(1L) %>% ungroup()
}
home_teams <- side_teams("home") %>% rename(home = team)
away_teams <- side_teams("away") %>% rename(away = team)

player_matches <- player_matches %>%
  left_join(home_teams, by = "match_id") %>%
  left_join(away_teams, by = "match_id") %>%
  mutate(opponent = ifelse(tolower(team_position) == "home", away, home)) %>%
  select(-team_position, -home, -away)

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
  break
}

# 5. Save Results ----

saveRDS(centrality_results, output_path)
message(sprintf("  Saved %d centrality scores to %s",
                nrow(centrality_results), basename(output_path)))
message(sprintf("  Centrality range: [%.3f, %.3f]",
                min(centrality_results$centrality),
                max(centrality_results$centrality)))

break  # end of one-shot block
}
