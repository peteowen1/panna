# 01_load_pannadata.R
# Load data from pannadata repository (replaces worldfootballR scraping)
#
# This script loads cached FBref data from pannadata and formats it
# for the panna pipeline.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

# Point to pannadata location (relative to panna directory)
pannadata_dir("../pannadata/data")

load_summary()

# LEAGUE SELECTION
# Big 5 European Leagues
big5 <- c("ENG", "ESP", "GER", "ITA", "FRA")

# European Club Competitions
european_clubs <- c("UCL", "UEL")

# Domestic Cups
domestic_cups <- c("FA_CUP", "EFL_CUP", "COPA_DEL_REY", "DFB_POKAL", "COPPA_ITALIA", "COUPE_DE_FRANCE")

# International Competitions
international <- c("EURO", "WC", "NATIONS_LEAGUE", "COPA_AMERICA", "GOLD_CUP", "AFCON", "ASIAN_CUP")

# Load ALL available competitions
leagues <- c(big5, european_clubs, domestic_cups, international)

# SEASONS (NULL = all available)
seasons <- NULL  # Load all seasons

# 3. Helper Function ----

#' Load and filter stats from pannadata cache
#'
#' @param stat_type Type of stats to load (e.g., "metadata", "summary", "passing")
#' @param leagues Vector of league codes to filter (NULL = all)
#' @param seasons Vector of seasons to filter (NULL = all)
#' @param label Display label for messages (defaults to stat_type)
#' @return Filtered data frame or NULL if no data found
load_and_filter_stats <- function(stat_type, leagues = NULL, seasons = NULL, label = NULL) {
  if (is.null(label)) label <- stat_type

  message(sprintf("Loading %s...", label))

  data <- tryCatch(
    aggregate_cached_matches(stat_type, league = NULL, season = NULL),
    error = function(e) {
      warning(sprintf("Failed to load %s: %s", stat_type, e$message))
      NULL
    }
  )

  if (is.null(data) || nrow(data) == 0) {
    message(sprintf("  No %s data found", label))
    return(NULL)
  }

  # Apply filters

if (!is.null(leagues) && length(leagues) > 0) {
    data <- data %>% filter(league %in% leagues)
  }
  if (!is.null(seasons) && length(seasons) > 0) {
    data <- data %>% filter(season %in% seasons)
  }

  message(sprintf("  Found %d %s records", nrow(data), label))
  data
}

# 4. Load Data from Pannadata ----

message("\n=== Loading Data from pannadata ===\n")

# Load all stat types using helper
metadata        <- load_and_filter_stats("metadata", leagues, seasons, "metadata")
summary_stats   <- load_and_filter_stats("summary", leagues, seasons, "player summary stats")
passing_stats   <- load_and_filter_stats("passing", leagues, seasons, "passing stats")
defense_stats   <- load_and_filter_stats("defense", leagues, seasons, "defense stats")
possession_stats <- load_and_filter_stats("possession", leagues, seasons, "possession stats")
misc_stats      <- load_and_filter_stats("misc", leagues, seasons, "misc stats")
passing_types_stats <- load_and_filter_stats("passing_types", leagues, seasons, "passing types stats")
keeper_stats    <- load_and_filter_stats("keeper", leagues, seasons, "keeper stats")
shots           <- load_and_filter_stats("shots", leagues, seasons, "shots")
events          <- load_and_filter_stats("events", leagues, seasons, "events")

# 5. Format Data for Pipeline ----

message("\n=== Formatting for pipeline ===\n")

# Create results table from metadata + aggregated xG from summary
message("Creating results table with xG...")

# Convert xG columns to numeric (they come as character from parsing)
summary_stats <- summary_stats %>%
  mutate(
    x_g = as.numeric(x_g),
    npx_g = as.numeric(npx_g),
    x_ag = as.numeric(x_ag)
  )

# Aggregate player xG to match level - separate home and away
home_xg <- summary_stats %>%
  filter(is_home == TRUE) %>%
  group_by(match_url) %>%
  summarise(
    home_xg = sum(x_g, na.rm = TRUE),
    home_npxg = sum(npx_g, na.rm = TRUE),
    .groups = "drop"
  )

away_xg <- summary_stats %>%
  filter(is_home == FALSE) %>%
  group_by(match_url) %>%
  summarise(
    away_xg = sum(x_g, na.rm = TRUE),
    away_npxg = sum(npx_g, na.rm = TRUE),
    .groups = "drop"
  )

match_xg <- home_xg %>%
  left_join(away_xg, by = "match_url")

# Join xG to metadata
results <- metadata %>%
  left_join(match_xg, by = "match_url") %>%
  # Rename columns to match expected format
  rename(
    home = home_team,
    away = away_team,
    date = match_date,
    home_goals = home_score,
    away_goals = away_score
  ) %>%
  mutate(
    # Add season_end_year for compatibility
    season_end_year = as.numeric(substr(season, 6, 9))
  )

message(sprintf("  Results: %d matches with xG data", sum(!is.na(results$home_xg))))

# Create lineups table from summary stats
message("Creating lineups table...")

lineups <- summary_stats %>%
  # Detect substitutes BEFORE trimming (leading non-breaking spaces = sub)
  # FBref encodes subs with leading U+00A0 (non-breaking space) in player names
  mutate(
    is_starter = !startsWith(player, "\u00A0"),
    player_name = trimws(gsub("\u00A0", " ", player))  # Convert NBSP to space, then trim
  ) %>%
  rename(
    shirt_number = number,
    position = pos,
    minutes = min,
    goals = gls,
    assists = ast,
    xg = x_g,
    npxg = npx_g,
    xag = x_ag
  ) %>%
  # Add match_id by joining with results
  left_join(
    results %>% select(match_url, home, away, date, league, season),
    by = c("match_url", "league", "season")
  ) %>%
  mutate(
    # Ensure minutes is numeric for calculations
    minutes = as.numeric(minutes)
  ) %>%
  # Calculate on/off minutes using CHAINS
  # Each starter begins a new chain; subs continue the chain
  group_by(match_url, team) %>%
  mutate(
    chain_id = cumsum(is_starter)
  ) %>%
  group_by(match_url, team, chain_id) %>%
  mutate(
    # Within each chain: cumulative minutes = off_minute
    off_minute = cumsum(minutes),
    # on_minute = previous player's off_minute (0 for starters)
    on_minute = lag(off_minute, default = 0)
  ) %>%
  ungroup() %>%
  select(-chain_id)

message(sprintf("  Lineups: %d player-match records", nrow(lineups)))

# Format shots if available
if (!is.null(shots) && nrow(shots) > 0) {
  message("Formatting shots data...")
  shooting <- shots
  message(sprintf("  Shots: %d records", nrow(shooting)))
} else {
  shooting <- NULL
}

# 6. Create Raw Data List ----

raw_data <- list(
  results = results,
  lineups = lineups,
  shooting = shooting,
  events = events,
  stats_summary = summary_stats,
  stats_passing = passing_stats,
  stats_defense = defense_stats,
  stats_possession = possession_stats,
  stats_misc = misc_stats,
  stats_passing_types = passing_types_stats,
  stats_keeper = keeper_stats
)

# 7. Save ----

cache_dir <- file.path("data-raw", "cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

saveRDS(raw_data, file.path(cache_dir, "01_raw_data.rds"))

# 8. Summary ----

message("\n========================================")
message("Data loading complete!")
message("========================================")
message(sprintf("Match results: %d", nrow(raw_data$results)))
message(sprintf("Lineups: %d", nrow(raw_data$lineups)))
message(sprintf("Shots: %s", if(!is.null(raw_data$shooting)) nrow(raw_data$shooting) else "N/A"))

message("\nStats tables:")
message(sprintf("  summary: %s", if(!is.null(raw_data$stats_summary)) nrow(raw_data$stats_summary) else "N/A"))
message(sprintf("  passing: %s", if(!is.null(raw_data$stats_passing)) nrow(raw_data$stats_passing) else "N/A"))
message(sprintf("  defense: %s", if(!is.null(raw_data$stats_defense)) nrow(raw_data$stats_defense) else "N/A"))
message(sprintf("  possession: %s", if(!is.null(raw_data$stats_possession)) nrow(raw_data$stats_possession) else "N/A"))
message(sprintf("  misc: %s", if(!is.null(raw_data$stats_misc)) nrow(raw_data$stats_misc) else "N/A"))
message(sprintf("  passing_types: %s", if(!is.null(raw_data$stats_passing_types)) nrow(raw_data$stats_passing_types) else "N/A"))
message(sprintf("  keeper: %s", if(!is.null(raw_data$stats_keeper)) nrow(raw_data$stats_keeper) else "N/A"))

# League breakdown
message("\nLeague breakdown:")
league_counts <- table(raw_data$results$league)
for (league in names(league_counts)) {
  message(sprintf("  %s: %d matches", league, league_counts[league]))
}

# Season breakdown
message("\nSeason breakdown:")
season_counts <- table(raw_data$results$season)
for (season in names(sort(unique(raw_data$results$season)))) {
  message(sprintf("  %s: %d matches", season, season_counts[season]))
}

message("\nData saved to: data-raw/cache/01_raw_data.rds")
message("Next step: source('data-raw/02_data_processing.R')")
