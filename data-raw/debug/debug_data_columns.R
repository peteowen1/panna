# debug_data_columns.R
# Debug script to trace column names through the data pipeline

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load raw and processed data
raw_data <- readRDS(file.path(cache_dir, "01_raw_data.rds"))
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

# Function to compare columns
compare_columns <- function(raw_df, processed_df, stat_type, expected_cols) {
  cat(sprintf("\n%s\n", paste(rep("=", 60), collapse = "")))
  cat(sprintf("=== %s ===\n", toupper(stat_type)))
  cat(sprintf("%s\n", paste(rep("=", 60), collapse = "")))

  if (is.null(raw_df)) {
    cat("RAW DATA: NULL\n")
  } else {
    cat(sprintf("\nRAW DATA: %d rows, %d columns\n", nrow(raw_df), ncol(raw_df)))
    cat("Raw column names:\n")
    print(names(raw_df))
  }

  cat(sprintf("\nEXPECTED COLUMNS (from get_stat_columns):\n"))
  print(expected_cols)

  if (!is.null(raw_df)) {
    # Check which expected columns exist in raw
    raw_names_lower <- tolower(names(raw_df))
    expected_lower <- tolower(expected_cols)

    found <- expected_cols[expected_lower %in% raw_names_lower]
    missing <- expected_cols[!expected_lower %in% raw_names_lower]

    cat(sprintf("\nFOUND in raw data (%d/%d):\n", length(found), length(expected_cols)))
    if (length(found) > 0) print(found)

    cat(sprintf("\nMISSING from raw data (%d):\n", length(missing)))
    if (length(missing) > 0) print(missing)

    # Show columns in raw that we're NOT capturing
    base_cols <- c("match_url", "matchurl", "player", "team", "home_away", "homeaway")
    extra_cols <- names(raw_df)[!tolower(names(raw_df)) %in% c(expected_lower, tolower(base_cols))]
    if (length(extra_cols) > 0) {
      cat(sprintf("\nEXTRA columns in raw (not being captured) (%d):\n", length(extra_cols)))
      print(extra_cols)
    }
  }

  if (is.null(processed_df)) {
    cat("\nPROCESSED DATA: NULL\n")
  } else {
    cat(sprintf("\nPROCESSED DATA: %d rows, %d columns\n", nrow(processed_df), ncol(processed_df)))
    cat("Processed column names:\n")
    print(names(processed_df))
  }
}

# Compare each stat type
cat("\n\n")
cat("##############################################################\n")
cat("# DATA PIPELINE COLUMN DEBUGGING\n")
cat("##############################################################\n")

# 1. Summary Stats
compare_columns(
  raw_data$stats_summary,
  processed_data$stats_summary,
  "SUMMARY STATS",
  get_stat_columns("summary")
)

# 2. Passing Stats
compare_columns(
  raw_data$stats_passing,
  processed_data$stats_passing,
  "PASSING STATS",
  get_stat_columns("passing")
)

# 3. Defense Stats
compare_columns(
  raw_data$stats_defense,
  processed_data$stats_defense,
  "DEFENSE STATS",
  get_stat_columns("defense")
)

# 4. Possession Stats
compare_columns(
  raw_data$stats_possession,
  processed_data$stats_possession,
  "POSSESSION STATS",
  get_stat_columns("possession")
)

# 5. Shooting Data
cat(sprintf("\n%s\n", paste(rep("=", 60), collapse = "")))
cat("=== SHOOTING DATA ===\n")
cat(sprintf("%s\n", paste(rep("=", 60), collapse = "")))

if (is.null(raw_data$shooting)) {
  cat("RAW SHOOTING: NULL\n")
} else {
  cat(sprintf("\nRAW SHOOTING: %d rows, %d columns\n", nrow(raw_data$shooting), ncol(raw_data$shooting)))
  cat("Raw column names:\n")
  print(names(raw_data$shooting))
}

if (is.null(processed_data$shooting)) {
  cat("\nPROCESSED SHOOTING: NULL\n")
} else {
  cat(sprintf("\nPROCESSED SHOOTING: %d rows, %d columns\n", nrow(processed_data$shooting), ncol(processed_data$shooting)))
  cat("Processed column names:\n")
  print(names(processed_data$shooting))
}

# Summary of issues
cat("\n\n")
cat("##############################################################\n")
cat("# SUMMARY OF COLUMN ISSUES\n")
cat("##############################################################\n")

issues <- list()

# Check each processed table for sparse columns
tables <- list(
  summary = processed_data$stats_summary,
  passing = processed_data$stats_passing,
  defense = processed_data$stats_defense,
  possession = processed_data$stats_possession,
  shooting = processed_data$shooting
)

for (name in names(tables)) {
  df <- tables[[name]]
  if (!is.null(df)) {
    base_cols <- c("match_id", "team", "is_home", "player_name")
    stat_cols <- setdiff(names(df), base_cols)
    cat(sprintf("\n%s: %d stat columns\n", toupper(name), length(stat_cols)))
    if (length(stat_cols) == 0) {
      cat("  WARNING: No stat columns found!\n")
    }
  } else {
    cat(sprintf("\n%s: NULL\n", toupper(name)))
  }
}

cat("\n\nDone debugging.\n")
