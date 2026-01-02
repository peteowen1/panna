# debug_lineup_events.R
# Check what data we have for player timing

cache_dir <- file.path("data-raw", "cache")

processed <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

cat("=== LINEUP DATA ===\n")
cat("Columns:", paste(names(processed$lineups), collapse = ", "), "\n")
cat("Rows:", nrow(processed$lineups), "\n\n")
cat("Sample (first 10 rows):\n")
print(head(processed$lineups, 10))

cat("\n=== EVENT DATA ===\n")
cat("Columns:", paste(names(processed$events), collapse = ", "), "\n")
cat("Rows:", nrow(processed$events), "\n\n")

cat("Event type breakdown:\n")
print(table(processed$events$event_type))

cat("\nSample substitutions:\n")
subs <- processed$events[processed$events$event_type == "substitution", ]
print(head(subs, 10))

# Check a single match
cat("\n=== SINGLE MATCH EXAMPLE ===\n")
sample_match <- processed$lineups$match_id[1]
cat("Match ID:", sample_match, "\n\n")

match_lineups <- processed$lineups[processed$lineups$match_id == sample_match, ]
cat("Players in this match:", nrow(match_lineups), "\n")
cat("Starters:", sum(match_lineups$is_starter), "\n")
cat("Subs used:", sum(!match_lineups$is_starter & match_lineups$minutes > 0), "\n\n")
print(match_lineups[order(-match_lineups$is_starter, -match_lineups$minutes), ])

match_events <- processed$events[processed$events$match_id == sample_match, ]
cat("\nEvents in this match:\n")
print(match_events)
