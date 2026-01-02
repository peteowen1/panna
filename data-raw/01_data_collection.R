# 01_data_collection.R
# Collect football data from FBref via worldfootballR
#
# This script collects all required data for panna ratings:
# - Match results with xG
# - Match lineups and substitutions
# - Match events (goals, subs with timing)
# - Shot-level data with xG
# - Advanced player stats
#
# Supports single league (EPL) or multi-league (Big 5) collection.
# DATA IS CACHED INCREMENTALLY - re-running this script will only
# scrape NEW matches that aren't already in the cache.

library(worldfootballR)
library(dplyr)
devtools::load_all()

# =============================================================================
# CONFIGURATION - Adjust these settings as needed
# =============================================================================

# LEAGUE SELECTION
# - "ENG" for Premier League only
# - "big5" for all Big 5 European leagues (ENG, ESP, GER, ITA, FRA)
# - c("ENG", "ESP") for specific leagues
leagues <- "big5"

# SEASONS
# worldfootballR cache has recent seasons (2018+) - instant loading
# Historical seasons may require scraping (respect rate limits)
seasons <- c(
  "2018-2019",
  "2019-2020",
  "2020-2021",
  "2021-2022",
  "2022-2023",
  "2023-2024",
  "2024-2025"
)

# Delay between requests (be respectful to FBref servers)
request_delay <- 4

# =============================================================================
# DATA COLLECTION
# =============================================================================

if (leagues == "ENG" || (length(leagues) == 1 && leagues == "ENG")) {
  # Single league (EPL only)
  message("\n=== Collecting Premier League Data ===\n")
  raw_data <- scrape_pl_comprehensive(
    seasons = seasons,
    delay = request_delay,
    verbose = TRUE
  )
} else {
  # Multi-league collection
  message("\n=== Collecting Multi-League Data ===\n")
  message("Leagues: ", ifelse(leagues == "big5", "Big 5 (ENG, ESP, GER, ITA, FRA)",
                               paste(leagues, collapse = ", ")))

  raw_data <- scrape_multi_league(
    leagues = leagues,
    seasons = seasons,
    delay = request_delay,
    verbose = TRUE
  )
}

# =============================================================================
# SAVE DATA
# =============================================================================

cache_dir <- file.path("data-raw", "cache")
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE)
}

saveRDS(raw_data, file.path(cache_dir, "01_raw_data.rds"))

# =============================================================================
# SUMMARY
# =============================================================================

message("\n========================================")
message("Data collection complete!")
message("========================================")
message(paste("Match results:", nrow(raw_data$results)))
message(paste("Lineups:", if(!is.null(raw_data$lineups)) nrow(raw_data$lineups) else 0))
message(paste("Events:", if(!is.null(raw_data$events)) nrow(raw_data$events) else 0))
message(paste("Shots:", if(!is.null(raw_data$shooting)) nrow(raw_data$shooting) else 0))

# Show league breakdown if multi-league
if ("league" %in% names(raw_data$results)) {
  message("\nLeague breakdown:")
  league_counts <- table(raw_data$results$league)
  for (league in names(league_counts)) {
    message(paste(" ", league, ":", league_counts[league], "matches"))
  }
}

# Show season breakdown
if ("season_end_year" %in% names(raw_data$results)) {
  message("\nSeason breakdown:")
  season_counts <- table(raw_data$results$season_end_year)
  for (season in names(season_counts)) {
    message(paste("  ", season, ":", season_counts[season], "matches"))
  }
}

message("\nData cached to: data-raw/cache/01_raw_data.rds")
message("Re-run this script anytime to add more seasons/leagues.")
