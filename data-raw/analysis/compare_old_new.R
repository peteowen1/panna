# Compare old cached data vs new scraped data
devtools::load_all()

# Set to real pannadata
pannadata_dir(file.path(dirname(getwd()), "pannadata", "data"))

# First test match ID
test_id <- "cc5b4244"

cat("=== COMPARING OLD vs NEW DATA ===\n\n")

# Load old metadata
old_meta <- load_match_table("ENG", "2024-2025", test_id, "metadata")

if (is.null(old_meta)) {
  cat("No old metadata found for", test_id, "\n")
} else {
  cat("--- OLD METADATA COLUMNS ---\n")
  cat(paste(names(old_meta), collapse = "\n"), "\n\n")

  cat("--- OLD METADATA VALUES ---\n")
  for (col in names(old_meta)) {
    val <- old_meta[[col]]
    if (is.na(val) || val == "") {
      cat(sprintf("  %s: [EMPTY/NA]\n", col))
    } else {
      cat(sprintf("  %s: %s\n", col, val))
    }
  }
}

# Check events table
cat("\n--- EVENTS TABLE ---\n")
old_events <- load_match_table("ENG", "2024-2025", test_id, "events")
if (is.null(old_events)) {
  cat("Events table: DOES NOT EXIST in cache\n")
} else {
  cat("Events table exists with", nrow(old_events), "rows\n")
  cat("Columns:", paste(names(old_events), collapse = ", "), "\n")
}

# New columns that would be added
cat("\n--- NEW COLUMNS (not in old data) ---\n")
new_cols <- c("home_manager", "away_manager", "home_captain", "away_captain",
              "home_captain_id", "away_captain_id", "venue", "attendance",
              "kickoff_time", "kickoff_epoch", "referee", "ar1", "ar2",
              "fourth_official", "var_official", "home_formation", "away_formation")

if (!is.null(old_meta)) {
  missing_cols <- setdiff(new_cols, names(old_meta))
  if (length(missing_cols) > 0) {
    cat(paste(missing_cols, collapse = "\n"), "\n")
  } else {
    cat("All new columns already present\n")
  }
}

# Check a few more matches for events
cat("\n--- EVENTS STATUS FOR TEST MATCHES ---\n")
test_ids <- c("cc5b4244", "34557647", "71618ace", "4efc72e4", "c0e3342a")
for (id in test_ids) {
  events <- load_match_table("ENG", "2024-2025", id, "events")
  meta <- load_match_table("ENG", "2024-2025", id, "metadata")
  teams <- if (!is.null(meta)) paste(meta$home_team, "vs", meta$away_team) else id
  status <- if (is.null(events)) "NO EVENTS" else paste(nrow(events), "events")
  cat(sprintf("  %s: %s\n", teams, status))
}
