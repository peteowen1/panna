# 01_compute_match_stats.R
# Load all Opta data and compute match-level stats with per-90 rates
#
# This is the first step of the estimated skills pipeline. It loads raw
# Opta player_stats for all leagues/seasons and computes per-90 rates
# and derived features at the match level (one row per player-match).

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

leagues <- if (exists("leagues")) leagues else c(
  "ENG", "ESP", "GER", "ITA", "FRA",
  "NED", "POR", "TUR", "ENG2", "SCO",
  "UCL", "UEL", "UECL",
  "WC", "EURO"
)

seasons <- if (exists("seasons")) seasons else NULL
min_season <- if (exists("min_season")) min_season else "2013-2014"
min_match_minutes <- if (exists("min_match_minutes")) min_match_minutes else 10
apply_context_adjustments <- if (exists("apply_context_adjustments")) apply_context_adjustments else FALSE

cache_path <- file.path(cache_dir, "01_match_stats.rds")
config_path <- file.path(cache_dir, "01_config.rds")

# 3. Check Cache ----

current_config <- list(leagues = leagues, seasons = seasons,
                       min_season = min_season,
                       min_match_minutes = min_match_minutes)

if (file.exists(cache_path) && file.exists(config_path)) {
  cached_config <- readRDS(config_path)
  if (identical(cached_config, current_config)) {
    message("Cache is up to date - skipping match stats computation")
    match_stats <- readRDS(cache_path)
    message(sprintf("  Loaded from cache: %d player-match rows", nrow(match_stats)))
    return(invisible(NULL))
  }
  message("Configuration changed - recomputing match stats")
}

# 4. Load and Process by League-Season ----

message("\n=== Loading Opta Data and Computing Match-Level Stats ===\n")

all_stats <- list()

for (league in leagues) {
  available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))

  if (length(available_seasons) == 0) {
    message(sprintf("  No seasons found for %s, skipping", league))
    next
  }

  if (!is.null(seasons)) available_seasons <- intersect(available_seasons, seasons)
  if (!is.null(min_season)) available_seasons <- available_seasons[available_seasons >= min_season]

  message(sprintf("\n--- %s: %d seasons ---", league, length(available_seasons)))

  for (season in available_seasons) {
    label <- paste(league, season)

    tryCatch({
      stats <- load_opta_stats(league, season = season)

      if (is.null(stats) || nrow(stats) == 0) {
        message(sprintf("  Skipping %s: no data", label))
        next
      }

      stats$league <- league
      stats$season <- season

      # Compute match-level features
      match_level <- compute_match_level_opta_stats(stats, min_minutes = min_match_minutes)

      if (!is.null(match_level) && nrow(match_level) > 0) {
        all_stats[[label]] <- match_level
        message(sprintf("  %s: %d player-matches", label, nrow(match_level)))
      }

    }, error = function(e) {
      message(sprintf("  ERROR in %s: %s", label, e$message))
    })
  }
}

# 5. Combine ----

message("\n=== Combining All Match Stats ===\n")

match_stats <- data.table::rbindlist(all_stats, fill = TRUE, use.names = TRUE)

message(sprintf("Total player-match rows: %d", nrow(match_stats)))
message(sprintf("Unique players: %d", data.table::uniqueN(match_stats$player_id)))
message(sprintf("Unique matches: %d", data.table::uniqueN(match_stats$match_id)))
message(sprintf("Features: %d columns", ncol(match_stats)))

# Date range
if ("match_date" %in% names(match_stats)) {
  dates <- as.Date(match_stats$match_date)
  message(sprintf("Date range: %s to %s", min(dates, na.rm = TRUE), max(dates, na.rm = TRUE)))
}

# League breakdown
league_counts <- match_stats[, .N, by = competition]
message("\nLeague breakdown:")
for (i in seq_len(nrow(league_counts))) {
  message(sprintf("  %s: %d rows", league_counts$competition[i], league_counts$N[i]))
}

# 6. Apply Context Adjustments (optional) ----

if (isTRUE(apply_context_adjustments)) {
  message("\n=== Applying Context Adjustments ===\n")

  # Try to load Elo ratings for opponent adjustment
  elo_ratings <- NULL
  prediction_cache <- file.path("data-raw", "cache-predictions-opta")
  rolling_path <- file.path(prediction_cache, "03_rolling_features.rds")
  if (file.exists(rolling_path)) {
    tryCatch({
      rolling <- readRDS(rolling_path)
      # Extract latest Elo per team from rolling features if available
      if ("home_elo" %in% names(rolling)) {
        home_elos <- stats::setNames(rolling$home_elo, rolling$home_team)
        away_elos <- stats::setNames(rolling$away_elo, rolling$away_team)
        elo_ratings <- c(home_elos, away_elos)
        elo_ratings <- tapply(elo_ratings, names(elo_ratings), mean)
        message(sprintf("  Loaded Elo ratings for %d teams", length(elo_ratings)))
      }
    }, error = function(e) {
      message(sprintf("  Could not load Elo ratings: %s", e$message))
    })
  }

  match_stats <- adjust_match_stats_for_context(
    match_stats,
    elo_ratings = elo_ratings,
    adjust_opponent = !is.null(elo_ratings),
    adjust_venue = TRUE,
    adjust_league = TRUE
  )
}

# 7. Save ----

saveRDS(match_stats, cache_path)
saveRDS(current_config, config_path)
message(sprintf("\nSaved to: %s", cache_path))
