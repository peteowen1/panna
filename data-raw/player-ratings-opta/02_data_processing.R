# 02_data_processing.R
# Process Opta raw data into processed_data structure for splints
#
# Uses create_opta_processed_data() adapter to convert Opta formats
# to the standard processed_data list expected by create_all_splints().

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")

raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
processed_data_path <- file.path(cache_dir, "02_processed_data.rds")

# Check cache
if (file.exists(processed_data_path)) {
  raw_mtime <- file.mtime(raw_data_path)
  proc_mtime <- file.mtime(processed_data_path)
  if (proc_mtime > raw_mtime) {
    message("=== Processed data cache is up to date ===")
    processed_data <- readRDS(processed_data_path)
  }
}

# 3. Process Data ----

if (!exists("processed_data")) {
  message("=== Processing Opta data ===")
  raw_opta_data <- readRDS(raw_data_path)

  # Use the Opta adapter to create processed_data structure
  processed_data <- create_opta_processed_data(
    opta_lineups = raw_opta_data$lineups,
    opta_events = raw_opta_data$events,
    opta_shot_events = NULL,  # We use SPADL-derived shots instead
    opta_stats = raw_opta_data$stats
  )

  # Override shooting with SPADL-derived shots (which have model xG)
  if (!is.null(raw_opta_data$shooting)) {
    processed_data$shooting <- raw_opta_data$shooting
  }

  # Enrich results with xG from Step 01 and metadata
  if (!is.null(raw_opta_data$results)) {
    # Use results from Step 01 which already has xG
    processed_data$results <- raw_opta_data$results
  }

  # Add season_end_year to results if not present
  if (!is.null(processed_data$results) && !"season_end_year" %in% names(processed_data$results)) {
    processed_data$results <- processed_data$results %>%
      mutate(season_end_year = as.numeric(substr(season, 6, 9)))
  }

  # Store raw stats for SPM (Step 05)
  processed_data$opta_stats <- raw_opta_data$stats
  processed_data$opta_xmetrics <- raw_opta_data$xmetrics

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
cat(paste("Events:", if (!is.null(processed_data$events)) nrow(processed_data$events) else 0, "\n"))
cat(paste("Shots:", if (!is.null(processed_data$shooting)) nrow(processed_data$shooting) else 0, "\n"))

# League breakdown
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

# Check xG coverage
xg_coverage <- sum(!is.na(processed_data$results$home_xg)) / nrow(processed_data$results) * 100
cat(sprintf("xG coverage: %.1f%% (%d/%d matches)\n",
            xg_coverage,
            sum(!is.na(processed_data$results$home_xg)),
            nrow(processed_data$results)))

message("Data processing complete!")
