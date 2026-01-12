# Test full scraping pipeline with new events + continuous time
setwd("C:/Users/peteo/OneDrive/Documents/pannaverse/panna")
devtools::load_all()

# Test with a few real matches (using valid IDs from cache)
test_urls <- c(
  # Arsenal vs Man Utd (2023-09-03)
  "https://fbref.com/en/matches/74125d47/Arsenal-Manchester-United-September-3-2023-Premier-League",
  # El Clasico (2023-10-28)
  "https://fbref.com/en/matches/59b0e4f0/Barcelona-Real-Madrid-October-28-2023-La-Liga",
  # Arsenal vs Crystal Palace (from cache - valid ID)
  "https://fbref.com/en/matches/006fb5b5/Arsenal-Crystal-Palace-January-20-2024-Premier-League"
)

for (url in test_urls) {
  cat("\n", strrep("=", 70), "\n")
  cat("Testing:", url, "\n")
  cat(strrep("=", 70), "\n")

  # Fetch and parse
  page <- fetch_match_page(url)
  result <- parse_match_page(page, url)

  # Show metadata
  cat("\n--- METADATA ---\n")
  meta <- result$metadata
  cat("Match:", meta$home_team, "vs", meta$away_team, "\n")
  cat("Score:", meta$home_score, "-", meta$away_score, "\n")
  cat("Date:", meta$match_date, "\n")
  cat("Venue:", meta$venue, "\n")
  cat("Attendance:", meta$attendance, "\n")
  cat("Home Manager:", meta$home_manager, "\n")
  cat("Away Manager:", meta$away_manager, "\n")
  cat("Home Formation:", meta$home_formation, "\n")
  cat("Away Formation:", meta$away_formation, "\n")
  cat("Referee:", meta$referee, "\n")

  # Show events
  cat("\n--- EVENTS ---\n")
  events <- result$events
  if (!is.null(events) && nrow(events) > 0) {
    cat("Total events:", nrow(events), "\n\n")

    # Show each event
    for (i in seq_len(nrow(events))) {
      e <- events[i, ]
      time_str <- if (!is.na(e$added_time) && e$added_time > 0) {
        paste0(e$minute, "+", e$added_time, "'")
      } else {
        paste0(e$minute, "'")
      }
      team <- if (e$is_home) meta$home_team else meta$away_team
      cat(sprintf("%6s  %-12s  %-20s  %s\n",
                  time_str, e$event_type, e$player, team))
    }

    # Calculate effective minutes
    cat("\n--- EFFECTIVE MINUTES (Continuous Time) ---\n")
    first_half_stoppage <- get_first_half_stoppage(events)
    cat("First-half stoppage:", first_half_stoppage, "mins\n")

    events$effective_minute <- calculate_effective_minute(
      events$minute, events$added_time, first_half_stoppage
    )

    # Show key time conversions
    cat("\nTime conversions:\n")
    unique_times <- unique(events[, c("minute", "added_time", "effective_minute")])
    unique_times <- unique_times[order(unique_times$effective_minute), ]
    for (i in seq_len(nrow(unique_times))) {
      t <- unique_times[i, ]
      raw <- if (!is.na(t$added_time) && t$added_time > 0) {
        paste0(t$minute, "+", t$added_time)
      } else {
        as.character(t$minute)
      }
      cat(sprintf("  %s' -> effective %g\n", raw, t$effective_minute))
    }

    # Create splint boundaries
    cat("\n--- SPLINT BOUNDARIES ---\n")

    # Need to add is_goal, is_sub, is_red_card columns for splint creation
    events$is_goal <- events$event_type %in% c("goal", "penalty_goal", "own_goal")
    events$is_sub <- events$event_type == "sub_in"
    events$is_red_card <- events$event_type %in% c("red_card", "second_yellow")

    boundaries <- create_splint_boundaries(events, include_goals = TRUE)
    cat("Number of splints:", nrow(boundaries), "\n\n")

    # Show boundaries
    cat(sprintf("%-6s  %-10s  %-10s  %-8s\n", "Splint", "Start", "End", "Duration"))
    for (i in seq_len(nrow(boundaries))) {
      b <- boundaries[i, ]
      cat(sprintf("%-6d  %-10.1f  %-10.1f  %-8.1f\n",
                  b$splint_num, b$start_minute, b$end_minute, b$duration))
    }
  } else {
    cat("No events found!\n")
  }

  cat("\n")
}

cat("\n", strrep("=", 70), "\n")
cat("All tests complete!\n")
