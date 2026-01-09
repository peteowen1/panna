# Test scrape: 10 matches to verify new metadata and events work
# Run from panna directory

devtools::load_all()

# Set pannadata directory
pannadata_dir(file.path(dirname(getwd()), "pannadata", "data"))

# Get fixtures for current season
fixtures <- scrape_fixtures("ENG", "2024-2025", completed_only = TRUE)

cat("Found", nrow(fixtures), "completed matches\n")

# Take first 10 matches
test_urls <- head(fixtures$match_url, 10)

cat("\nTest URLs:\n")
for (url in test_urls) {
 cat(" ", url, "\n")
}

# Scrape with force (no cache) to test fresh scraping
cat("\n--- Scraping 10 matches (force fresh) ---\n")

# Temporarily use a test cache directory so we don't overwrite real data
test_cache_dir <- file.path(tempdir(), "panna_test_cache")
pannadata_dir(test_cache_dir)

result <- scrape_fbref_matches(
  test_urls,
  league = "ENG",
  season = "2024-2025",
  delay = 4,
  use_cache = TRUE,
  verbose = TRUE
)

# Check metadata
cat("\n=== METADATA ===\n")
if (!is.null(result$metadata)) {
  meta <- result$metadata
  cat("Rows:", nrow(meta), "\n")
  cat("Columns:", paste(names(meta), collapse = ", "), "\n\n")

  # Show extended metadata for first few matches
  cat("Sample extended metadata:\n")
  for (i in 1:min(3, nrow(meta))) {
    cat("\nMatch", i, ":", meta$home_team[i], "vs", meta$away_team[i], "\n")
    cat("  Manager (H):", meta$home_manager[i], "\n")
    cat("  Manager (A):", meta$away_manager[i], "\n")
    cat("  Captain (H):", meta$home_captain[i], "\n")
    cat("  Captain (A):", meta$away_captain[i], "\n")
    cat("  Formation (H):", meta$home_formation[i], "\n")
    cat("  Formation (A):", meta$away_formation[i], "\n")
    cat("  Venue:", meta$venue[i], "\n")
    cat("  Attendance:", meta$attendance[i], "\n")
    cat("  Referee:", meta$referee[i], "\n")
  }
} else {
  cat("No metadata returned!\n")
}

# Check events
cat("\n=== EVENTS ===\n")
if (!is.null(result$events)) {
  events <- result$events
  cat("Total events:", nrow(events), "\n")
  cat("Columns:", paste(names(events), collapse = ", "), "\n\n")

  # Event type breakdown
  cat("Event types:\n")
  print(table(events$event_type))

  # Show events with added time
  cat("\nEvents with added time (stoppage time):\n")
  added_time_events <- events[!is.na(events$added_time), ]
  if (nrow(added_time_events) > 0) {
    for (i in 1:min(10, nrow(added_time_events))) {
      e <- added_time_events[i, ]
      cat(sprintf("  %s' +%s: %s - %s (%s)\n",
                  e$minute, e$added_time, e$event_type, e$player,
                  if(e$is_home) "Home" else "Away"))
    }
  } else {
    cat("  (none found)\n")
  }

  # Show sample of all events from first match
  cat("\nFirst match timeline:\n")
  first_match_id <- events$fbref_id[1]
  first_match_events <- events[events$fbref_id == first_match_id, ]
  for (i in 1:nrow(first_match_events)) {
    e <- first_match_events[i, ]
    time_str <- if (!is.na(e$added_time)) {
      sprintf("%d+%d'", e$minute, e$added_time)
    } else {
      sprintf("%d'", e$minute)
    }
    secondary <- if (!is.na(e$secondary_player)) {
      sprintf(" (%s: %s)",
              if(e$event_type == "sub_on") "for" else "assist",
              e$secondary_player)
    } else ""
    cat(sprintf("  %s %s: %s%s [%s]\n",
                time_str, e$event_type, e$player, secondary,
                if(e$is_home) "Home" else "Away"))
  }
} else {
  cat("No events returned!\n")
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Tables returned:\n")
for (name in names(result)) {
  if (!is.null(result[[name]])) {
    cat(sprintf("  %s: %d rows\n", name, nrow(result[[name]])))
  }
}

# Cleanup test cache
cat("\nTest complete. Temp cache at:", test_cache_dir, "\n")
