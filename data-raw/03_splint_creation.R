# 03_splint_creation.R
# Create splints (time segments between subs/goals) for RAPM analysis
#
# A "splint" is a time segment where the lineup remains constant.
# This is the unit of analysis for RAPM models.
#
# Supports incremental processing - skips if processed data hasn't changed.
# Use force = TRUE to force reprocessing.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache")
force <- exists("force") && isTRUE(force)  # Set force=TRUE before sourcing to force reprocess

processed_data_path <- file.path(cache_dir, "02_processed_data.rds")
splint_data_path <- file.path(cache_dir, "03_splints.rds")

# 3. Create Splints ----

if (!force && file.exists(splint_data_path)) {
  processed_mtime <- file.mtime(processed_data_path)
  splint_mtime <- file.mtime(splint_data_path)

  if (splint_mtime > processed_mtime) {
    message("=== Skipping splint creation (cache is up to date) ===")
    message(sprintf("  Processed data: %s", processed_mtime))
    message(sprintf("  Splint data:    %s", splint_mtime))
    message("  Use force=TRUE before sourcing to force reprocessing")
    splint_data <- readRDS(splint_data_path)
  } else {
    message("=== Processed data is newer - recreating splints ===")
    processed_data <- readRDS(processed_data_path)
    splint_data <- create_all_splints(
      processed_data,
      include_goals = TRUE,
      verbose = TRUE
    )
    saveRDS(splint_data, splint_data_path)
  }
} else {
  message("=== Creating splints ===")
  processed_data <- readRDS(processed_data_path)
  splint_data <- create_all_splints(
    processed_data,
    include_goals = TRUE,
    verbose = TRUE
  )
  saveRDS(splint_data, splint_data_path)
}

# 4. Summary Statistics ----

cat("\n=== Splint Summary ===\n")
cat(paste("Total splints:", nrow(splint_data$splints), "\n"))
cat(paste("Average splints per match:",
          round(nrow(splint_data$splints) / length(unique(splint_data$splints$match_id)), 1), "\n"))
cat(paste("Player-splint records:", nrow(splint_data$players), "\n"))

# League breakdown if multi-league
if ("league" %in% names(splint_data$splints)) {
  cat("\n=== League Breakdown ===\n")
  league_splints <- table(splint_data$splints$league)
  for (league in names(league_splints)) {
    cat(sprintf("  %s: %d splints\n", league, league_splints[league]))
  }
}

# Season breakdown
if ("season_end_year" %in% names(splint_data$splints)) {
  cat("\n=== Season Breakdown ===\n")
  season_splints <- table(splint_data$splints$season_end_year)
  for (season in names(season_splints)) {
    cat(sprintf("  %s: %d splints\n", season, season_splints[season]))
  }
}

# 5. Splint Duration Distribution ----

cat("\n=== Splint Duration Distribution ===\n")
duration_summary <- summary(splint_data$splints$duration)
print(duration_summary)

# 6. Player Assignment Validation ----

cat("\n=== Player Assignment Validation ===\n")
players_per_splint <- splint_data$players %>%
  group_by(splint_id) %>%
  summarise(
    n_players = n(),
    n_home = sum(is_home),
    n_away = sum(!is_home),
    .groups = "drop"
  )

cat("Players per splint:\n")
print(summary(players_per_splint$n_players))

# Should be ~22 (11 vs 11) for most splints
correct_22 <- sum(players_per_splint$n_players == 22)
cat(sprintf("\nSplints with exactly 22 players: %d / %d (%.1f%%)\n",
            correct_22, nrow(splint_data$splints),
            100 * correct_22 / nrow(splint_data$splints)))

# 7. Player Turnover Check ----

cat("\n=== Player Turnover Check (sample match) ===\n")
sample_match <- unique(splint_data$splints$match_id)[1]
match_splints <- splint_data$splints %>% filter(match_id == sample_match)
match_players <- splint_data$players %>% filter(match_id == sample_match)

for (i in 2:min(nrow(match_splints), 5)) {
  splint_prev <- match_splints$splint_id[i-1]
  splint_curr <- match_splints$splint_id[i]

  prev_players <- match_players$player_name[match_players$splint_id == splint_prev]
  curr_players <- match_players$player_name[match_players$splint_id == splint_curr]

  added <- setdiff(curr_players, prev_players)
  removed <- setdiff(prev_players, curr_players)

  if (length(added) > 0 || length(removed) > 0) {
    cat(sprintf("Splint %d -> %d: OUT=%d, IN=%d\n", i-1, i, length(removed), length(added)))
  } else {
    cat(sprintf("Splint %d -> %d: No player changes\n", i-1, i))
  }
}

# 8. Data Quality Checks ----

n_zero_duration <- sum(splint_data$splints$duration <= 0, na.rm = TRUE)
if (n_zero_duration > 0) {
  warning(paste(n_zero_duration, "splints have zero or negative duration"))
}

# 9. npxGD Distribution ----

cat("\n=== npxGD Distribution ===\n")
cat("npxgd_per_90 summary:\n")
print(summary(splint_data$splints$npxgd_per_90))

message("\nSplint creation complete! Run 04_rapm.R next.")
