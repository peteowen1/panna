# 02_data_processing.R
# Process and clean collected data
#
# This script:
# - Loads raw data from cache
# - Cleans and standardizes all data
# - Creates match and player IDs
# - Merges related data sources

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load raw data
raw_data <- readRDS(file.path(cache_dir, "01_raw_data.rds"))

# Process all data
processed_data <- process_all_data(raw_data)

# Summary statistics
cat("\n=== Processed Data Summary ===\n")
cat(paste("Matches:", nrow(processed_data$results), "\n"))
cat(paste("Unique teams:", length(unique(c(
  processed_data$results$home_team,
  processed_data$results$away_team
))), "\n"))
cat(paste("Lineup records:", if (!is.null(processed_data$lineups)) nrow(processed_data$lineups) else 0, "\n"))
cat(paste("Events:", if (!is.null(processed_data$events)) nrow(processed_data$events) else "0 (not available for ENG)", "\n"))
cat(paste("Shots:", if (!is.null(processed_data$shooting)) nrow(processed_data$shooting) else 0, "\n"))

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

# Check data quality
cat("\n=== Data Quality Check ===\n")
validation <- validate_data_completeness(
  processed_data$results,
  required_cols = c("match_id", "season", "home_team", "away_team", "home_xg", "away_xg")
)
cat(paste("Rows:", validation$n_rows, "\n"))
if (length(validation$missing_cols) > 0) {
  cat(paste("Missing columns:", paste(validation$missing_cols, collapse = ", "), "\n"))
}

# Save processed data
saveRDS(processed_data, file.path(cache_dir, "02_processed_data.rds"))

message("Data processing complete!")
