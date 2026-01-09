# Test script for new scraping functionality
# Tests: extended metadata, events timeline, caching
#
# Run this before doing a full rescrape to verify everything works

devtools::load_all()

# Reset session for clean state
reset_fbref_session()

# Use a temp directory for testing (won't affect real cache)
test_dir <- file.path(tempdir(), "panna_scrape_test")
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
pannadata_dir(test_dir)

cat("Test cache directory:", test_dir, "\n\n")

# Pick completed matches with full stats available
test_urls <- c(
  "https://fbref.com/en/matches/74125d47/Arsenal-Manchester-United-September-3-2023-Premier-League",
  "https://fbref.com/en/matches/59b0e4f0/Barcelona-Real-Madrid-October-28-2023-La-Liga"
)

cat("=== Testing scrape on", length(test_urls), "matches ===\n\n")

# Scrape with all table types including events
data <- scrape_fbref_matches(
  test_urls,
  league = "TEST",
  season = "2024-2025",
  delay = 5,
  use_cache = TRUE,
  verbose = TRUE
)

# Check metadata
cat("\n=== METADATA (Extended Fields) ===\n")
if (!is.null(data$metadata)) {
  meta <- data$metadata
  cat("Matches scraped:", nrow(meta), "\n\n")

  for (i in seq_len(nrow(meta))) {
    cat("Match:", meta$home_team[i], "vs", meta$away_team[i], "\n")
    cat("  Score:", meta$home_score[i], "-", meta$away_score[i], "\n")
    cat("  Date:", meta$match_date[i], "\n")
    cat("  Venue:", meta$venue[i], "\n")
    cat("  Attendance:", format(meta$attendance[i], big.mark = ","), "\n")
    cat("  Home Manager:", meta$home_manager[i], "\n")
    cat("  Away Manager:", meta$away_manager[i], "\n")
    cat("  Home Captain:", meta$home_captain[i], "(", meta$home_captain_id[i], ")\n")
    cat("  Away Captain:", meta$away_captain[i], "(", meta$away_captain_id[i], ")\n")
    cat("  Formations:", meta$home_formation[i], "vs", meta$away_formation[i], "\n")
    cat("  Referee:", meta$referee[i], "\n")
    cat("  Kickoff:", meta$kickoff_time[i], "\n")
    cat("\n")
  }
} else {
  cat("ERROR: No metadata returned!\n")
}

# Check events
cat("\n=== EVENTS TIMELINE ===\n")
if (!is.null(data$events)) {
  events <- data$events
  cat("Total events:", nrow(events), "\n\n")

  # Show events by match
  for (match_id in unique(events$fbref_id)) {
    match_events <- events[events$fbref_id == match_id, ]
    meta_row <- data$metadata[data$metadata$fbref_id == match_id, ]

    cat("Match:", meta_row$home_team, "vs", meta_row$away_team, "\n")
    cat(sprintf("  %-6s %-15s %-5s %-25s %s\n",
                "Min", "Type", "Home?", "Player", "Secondary"))
    cat("  ", strrep("-", 70), "\n", sep = "")

    for (j in seq_len(nrow(match_events))) {
      e <- match_events[j, ]
      min_str <- if (!is.na(e$added_time)) {
        sprintf("%d+%d'", e$minute, e$added_time)
      } else {
        sprintf("%d'", e$minute)
      }
      secondary <- if (!is.na(e$secondary_player)) e$secondary_player else ""

      cat(sprintf("  %-6s %-15s %-5s %-25s %s\n",
                  min_str, e$event_type, ifelse(e$is_home, "H", "A"),
                  substr(e$player, 1, 25), secondary))
    }
    cat("\n")
  }
} else {
  cat("WARNING: No events returned (page may not have events section)\n")
}

# Check caching
cat("\n=== CACHE VERIFICATION ===\n")
cached_ids <- get_cached_match_ids("TEST", "2024-2025")
cat("Cached match IDs:", length(cached_ids), "\n")
cat("IDs:", paste(cached_ids, collapse = ", "), "\n")

# List what was cached
cat("\nCached table types:\n")
for (tt in c("metadata", "summary", "events")) {
  cached <- list_cached_matches(tt, "TEST", "2024-2025")
  cat(sprintf("  %s: %d files\n", tt, nrow(cached)))
}

# Test loading from cache (second run should be instant)
cat("\n=== RE-LOADING FROM CACHE (should be instant) ===\n")
start_time <- Sys.time()
data2 <- scrape_fbref_matches(
  test_urls,
  league = "TEST",
  season = "2024-2025",
  delay = 5,
  use_cache = TRUE,
  verbose = TRUE
)
elapsed <- Sys.time() - start_time
cat("Cache load time:", round(elapsed, 2), "seconds\n")

# Cleanup
cat("\n=== CLEANUP ===\n")
unlink(test_dir, recursive = TRUE)
cat("Test directory removed\n")

cat("\n=== TEST COMPLETE ===\n")
cat("If you saw metadata and events above, the new scraping is working!\n")
