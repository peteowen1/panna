# 03_splint_creation.R
# Create splints (time segments between subs/goals) for Opta RAPM analysis
#
# Near-identical to FBref version. Calls create_all_splints() on
# Opta processed data.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")

processed_data_path <- file.path(cache_dir, "02_processed_data.rds")
splint_data_path <- file.path(cache_dir, "03_splints.rds")

# 3. Create Splints ----

if (file.exists(splint_data_path)) {
  processed_mtime <- file.mtime(processed_data_path)
  splint_mtime <- file.mtime(splint_data_path)

  if (splint_mtime > processed_mtime) {
    message("=== Skipping splint creation (cache is up to date) ===")
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

# League breakdown
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

correct_22 <- sum(players_per_splint$n_players == 22)
cat(sprintf("\nSplints with exactly 22 players: %d / %d (%.1f%%)\n",
            correct_22, nrow(splint_data$splints),
            100 * correct_22 / nrow(splint_data$splints)))

# 7. npxGD Distribution ----

cat("\n=== npxGD Distribution ===\n")
cat("npxgd_per_90 summary:\n")
print(summary(splint_data$splints$npxgd_per_90))

message("\nSplint creation complete!")
