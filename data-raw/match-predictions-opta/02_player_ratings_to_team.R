# 02_player_ratings_to_team.R
# Aggregate player ratings (xRAPM/SPM/RAPM) to team-level features
#
# For each match, gets the starting XI from lineups, joins to seasonal
# ratings by player_name + season_end_year, and computes team-level
# summary statistics (sum, mean, max, positional group averages, etc.)

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "02_team_ratings.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 02_team_ratings.rds")
  team_ratings <- readRDS(output_path)
  message(sprintf("  %d matches with team ratings", nrow(team_ratings)))
  return(invisible(NULL))
}

# 4. Load Required Data ----

message("\n=== Aggregating Player Ratings to Team Level ===\n")

# Load fixture results from step 01
fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
played <- fixture_results[fixture_results$match_status == "Played", ]

# Load seasonal ratings from RAPM pipeline
ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")
if (!file.exists(ratings_path)) {
  stop("Seasonal ratings not found. Run the Opta RAPM pipeline first:\n  ",
       "source('data-raw/player-ratings-opta/run_pipeline_opta.R')")
}
seasonal_data <- readRDS(ratings_path)

# Extract and combine rating tables
# The RDS contains separate tables: seasonal_xrapm, seasonal_spm, seasonal_rapm
if (is.data.frame(seasonal_data)) {
  ratings <- seasonal_data
} else if ("seasonal_xrapm" %in% names(seasonal_data)) {
  # Standard structure from Opta RAPM pipeline
  xrapm <- seasonal_data$seasonal_xrapm[, c("player_name", "season_end_year",
                                             "xrapm", "offense", "defense",
                                             "total_minutes")]
  names(xrapm)[names(xrapm) == "xrapm"] <- "panna"

  spm <- seasonal_data$seasonal_spm[, c("player_name", "season_end_year", "spm")]
  spm <- spm[!duplicated(spm[, c("player_name", "season_end_year")]), ]

  ratings <- merge(xrapm, spm,
                   by = c("player_name", "season_end_year"),
                   all.x = TRUE)
  ratings$spm[is.na(ratings$spm)] <- 0
} else if ("combined" %in% names(seasonal_data)) {
  ratings <- seasonal_data$combined
} else {
  ratings <- seasonal_data[[1]]
}

message(sprintf("  Ratings: %d player-seasons", nrow(ratings)))

# Validate required columns
required_rating_cols <- c("player_name", "season_end_year", "panna", "offense", "defense", "spm")
missing <- setdiff(required_rating_cols, names(ratings))
if (length(missing) > 0) {
  stop("Missing required columns in ratings: ", paste(missing, collapse = ", "))
}

# 5. Load Lineups ----

# Try RAPM cache first
rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (file.exists(rapm_cache)) {
  message("  Loading lineups from RAPM cache...")
  raw_data <- readRDS(rapm_cache)
  lineups <- raw_data$lineups
} else {
  message("  Loading lineups from Opta data...")
  leagues <- unique(played$league)
  all_lineups <- list()
  for (league in leagues) {
    available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
    for (season in available_seasons) {
      tryCatch({
        lu <- load_opta_lineups(league, season = season)
        if (!is.null(lu) && nrow(lu) > 0) {
          lu$league <- league
          lu$season <- season
          all_lineups[[paste(league, season)]] <- lu
        }
      }, error = function(e) NULL)
    }
  }
  lineups <- bind_rows(all_lineups)
}

message(sprintf("  Lineups: %d rows", nrow(lineups)))

# 6. Aggregate Ratings to Team Level ----

# Get unique season_end_years
sey_values <- unique(played$season_end_year)
message(sprintf("  Processing %d season-years...", length(sey_values)))

all_team_ratings <- list()

for (sey in sey_values) {
  matches_sey <- played[played$season_end_year == sey, ]
  match_ids <- unique(matches_sey$match_id)

  # Filter lineups to these matches
  lu_sey <- lineups[lineups$match_id %in% match_ids, ]
  if (nrow(lu_sey) == 0) next

  tryCatch({
    tr <- aggregate_lineup_ratings(lu_sey, ratings, season_end_year = sey)
    all_team_ratings[[as.character(sey)]] <- tr
    message(sprintf("    SEY %d: %d matches", sey, nrow(tr)))
  }, error = function(e) {
    message(sprintf("    SEY %d ERROR: %s", sey, e$message))
  })
}

team_ratings <- bind_rows(all_team_ratings)

# 7. Handle Future Fixtures ----

# For upcoming fixtures, use most recent season ratings
upcoming <- fixture_results[fixture_results$match_status != "Played", ]
if (nrow(upcoming) > 0) {
  message(sprintf("\n  Processing %d upcoming fixtures...", nrow(upcoming)))
  latest_sey <- max(sey_values, na.rm = TRUE)

  # Get most recent lineup per team (last played match)
  latest_lineups <- lineups %>%
    filter(is_starter) %>%
    group_by(team_name) %>%
    filter(match_date == max(match_date)) %>%
    ungroup()

  # For each upcoming match, construct synthetic lineup rows
  upcoming_lineups <- list()
  for (i in seq_len(nrow(upcoming))) {
    m <- upcoming[i, ]
    ht <- m$home_team
    at <- m$away_team

    home_lu <- latest_lineups %>%
      filter(team_name == ht) %>%
      mutate(match_id = m$match_id, team_position = "home")

    away_lu <- latest_lineups %>%
      filter(team_name == at) %>%
      mutate(match_id = m$match_id, team_position = "away")

    if (nrow(home_lu) > 0 && nrow(away_lu) > 0) {
      upcoming_lineups[[i]] <- bind_rows(home_lu, away_lu)
    }
  }

  if (length(upcoming_lineups) > 0) {
    upcoming_lu <- bind_rows(upcoming_lineups)
    tryCatch({
      upcoming_tr <- aggregate_lineup_ratings(upcoming_lu, ratings, season_end_year = latest_sey)
      team_ratings <- bind_rows(team_ratings, upcoming_tr)
      message(sprintf("  Added %d fixture team ratings", nrow(upcoming_tr)))
    }, error = function(e) {
      message(sprintf("  Fixture ratings error: %s", e$message))
    })
  }
}

message(sprintf("\nTotal: %d matches with team ratings", nrow(team_ratings)))

# 8. Save ----

saveRDS(team_ratings, output_path)
message(sprintf("Saved to: %s", output_path))
