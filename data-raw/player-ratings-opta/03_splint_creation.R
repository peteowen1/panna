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
    # Free memory
    rm(processed_data); gc(verbose = FALSE)
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
  # Free memory
  rm(processed_data); gc(verbose = FALSE)
}

# Validate splint output
validate_step_output(splint_data$splints, step_name = "03_splints: splints",
                     min_rows = 1000, warn_below = 100000)
validate_step_output(splint_data$players, step_name = "03_splints: players",
                     min_rows = 1000, warn_below = 200000)

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

# 8. Add Value Metrics to Splints (optional) ----
# If per-game EPV/WPA/PSV caches exist, add them to splints for multi-target RAPM

use_value_metrics <- if (exists("use_value_metrics")) use_value_metrics else TRUE

if (use_value_metrics) {
  epv_cache_dir <- "data-raw/cache/epv/players"

  # Load per-game EPV (if available)
  epv_files <- list.files(epv_cache_dir, pattern = "^player_game_epv_", full.names = TRUE)
  player_game_epv <- if (length(epv_files) > 0) {
    cat("\n=== Adding per-game EPV to splints ===\n")
    dt <- data.table::rbindlist(lapply(epv_files, readRDS), fill = TRUE)
    if (nrow(dt) == 0) { cat("  Warning: EPV files loaded but 0 rows\n"); NULL } else dt
  } else NULL

  # Load per-game WPA (if available)
  wpa_files <- list.files(epv_cache_dir, pattern = "^player_game_wpa_", full.names = TRUE)
  player_game_wpa <- if (length(wpa_files) > 0) {
    cat("=== Adding per-game WPA to splints ===\n")
    dt <- data.table::rbindlist(lapply(wpa_files, readRDS), fill = TRUE)
    if (nrow(dt) == 0) { cat("  Warning: WPA files loaded but 0 rows\n"); NULL } else dt
  } else NULL

  # Load per-game PSV (if available from skills pipeline)
  psv_cache <- file.path("data-raw", "cache-skills", "player_game_psv.rds")
  player_game_psv <- if (file.exists(psv_cache)) {
    cat("=== Adding per-game PSV to splints ===\n")
    readRDS(psv_cache)
  } else NULL

  if (!is.null(player_game_epv) || !is.null(player_game_wpa) || !is.null(player_game_psv)) {
    splint_data <- add_value_metrics_to_splints(
      splint_data,
      player_game_epv = player_game_epv,
      player_game_wpa = player_game_wpa,
      player_game_psv = player_game_psv
    )
    # Re-save with value metrics
    saveRDS(splint_data, splint_data_path)

    added <- c()
    if ("epv_home" %in% names(splint_data$splints)) added <- c(added, "EPV")
    if ("wpa_home" %in% names(splint_data$splints)) added <- c(added, "WPA")
    if ("psv_home" %in% names(splint_data$splints)) added <- c(added, "PSV")
    cat(sprintf("Value metrics added to splints: %s\n", paste(added, collapse = ", ")))
  } else {
    cat("\nNo per-game value metric caches found — skipping value metrics on splints.\n")
    cat("Run EPV/WPA pipeline steps first to enable multi-target RAPM.\n")
  }
}

message("\nSplint creation complete!")
