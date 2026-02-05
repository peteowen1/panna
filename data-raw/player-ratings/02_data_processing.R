# 02_data_processing.R
# Process and clean collected data
#
# This script:
# - Loads raw data from cache
# - Cleans and standardizes all data
# - Creates match and player IDs
# - Merges related data sources
#
# Supports incremental processing - skips if raw data hasn't changed.
# Use force = TRUE to force reprocessing.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache")
force <- exists("force") && isTRUE(force)  # Set force=TRUE before sourcing to force reprocess

raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
processed_data_path <- file.path(cache_dir, "02_processed_data.rds")

# Check if we can skip processing
raw_mtime <- file.mtime(raw_data_path)

# Check if ALL components are cached and up to date
components <- c("results", "lineups", "events", "shooting",
                "stats_summary", "stats_passing", "stats_defense", "stats_possession",
                "stats_misc", "stats_passing_types", "stats_keeper")

all_cached <- !force && all(sapply(components, function(comp) {
  cache_file <- file.path(cache_dir, paste0("02_", comp, ".rds"))
  file.exists(cache_file) && file.mtime(cache_file) > raw_mtime
}))

# 3. Process Data ----

if (all_cached && file.exists(processed_data_path) && file.mtime(processed_data_path) > raw_mtime) {
  message("=== All components cached - loading from cache ===")
  message("  Use force=TRUE before sourcing to force reprocessing")
  processed_data <- readRDS(processed_data_path)
} else {
  message("=== Processing data (with per-component caching) ===")
  raw_data <- readRDS(raw_data_path)
  # Per-component caching: only processes components that need updating
  processed_data <- process_all_data(raw_data, cache_dir = cache_dir, raw_data_mtime = raw_mtime)
  saveRDS(processed_data, processed_data_path)
}

# 4. Summary Statistics ----

cat("\n=== Processed Data Summary ===\n")
cat(paste("Matches:", nrow(processed_data$results), "\n"))
cat(paste("Unique teams:", length(unique(c(
  processed_data$results$home_team,
  processed_data$results$away_team
))), "\n"))
cat(paste("Lineup records:", if (!is.null(processed_data$lineups)) nrow(processed_data$lineups) else 0, "\n"))
cat(paste("Events:", if (!is.null(processed_data$events)) nrow(processed_data$events) else "0 (not available for ENG)", "\n"))
cat(paste("Shots:", if (!is.null(processed_data$shooting)) nrow(processed_data$shooting) else 0, "\n"))

cat("\n=== Stats Tables ===\n")
cat(paste("  summary:", if (!is.null(processed_data$stats_summary)) nrow(processed_data$stats_summary) else 0, "\n"))
cat(paste("  passing:", if (!is.null(processed_data$stats_passing)) nrow(processed_data$stats_passing) else 0, "\n"))
cat(paste("  defense:", if (!is.null(processed_data$stats_defense)) nrow(processed_data$stats_defense) else 0, "\n"))
cat(paste("  possession:", if (!is.null(processed_data$stats_possession)) nrow(processed_data$stats_possession) else 0, "\n"))
cat(paste("  misc:", if (!is.null(processed_data$stats_misc)) nrow(processed_data$stats_misc) else 0, "\n"))
cat(paste("  passing_types:", if (!is.null(processed_data$stats_passing_types)) nrow(processed_data$stats_passing_types) else 0, "\n"))
cat(paste("  keeper:", if (!is.null(processed_data$stats_keeper)) nrow(processed_data$stats_keeper) else 0, "\n"))

# League breakdown if multi-league
if ("league" %in% names(processed_data$results)) {
  cat("\n=== League Breakdown ===\n")
  league_counts <- table(processed_data$results$league)
  for (league in names(league_counts)) {
    cat(sprintf("  %s: %d matches\n", league, league_counts[league]))
  }
}

# Season breakdown
if ("season_end_year" %in% names(processed_data$results)) {
  cat("\n=== Season Breakdown ===\n")
  season_counts <- table(processed_data$results$season_end_year)
  for (season in names(season_counts)) {
    cat(sprintf("  %s: %d matches\n", season, season_counts[season]))
  }
}

# 5. Data Quality Check ----

cat("\n=== Data Quality Check ===\n")
validation <- validate_data_completeness(
  processed_data$results,
  required_cols = c("match_id", "season", "home_team", "away_team", "home_xg", "away_xg")
)
cat(paste("Rows:", validation$n_rows, "\n"))
if (length(validation$missing_cols) > 0) {
  cat(paste("Missing columns:", paste(validation$missing_cols, collapse = ", "), "\n"))
}

message("Data processing complete!")
