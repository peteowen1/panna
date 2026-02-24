# 01_build_fixture_results.R
# Build historical results and future fixtures from Opta data
#
# Loads lineups and events to construct match results with goals and xG.
# Also loads fixture data for upcoming matches. Reuses cached RAPM pipeline
# data when available.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

leagues <- if (exists("leagues")) leagues else c(
  "ENG", "ESP", "GER", "ITA", "FRA",
  "NED", "POR", "TUR", "ENG2", "SCO",
  "UCL", "UEL", "UECL",
  "WC", "EURO"
)
seasons <- if (exists("seasons")) seasons else NULL
min_season <- if (exists("min_season")) min_season else "2013-2014"

# Tournament leagues (neutral venue flag)
TOURNAMENT_LEAGUES <- c("WC", "EURO")

extract_season_end_year <- function(season) {
  if (grepl("^\\d{4}-\\d{4}$", season)) return(as.numeric(substr(season, 6, 9)))
  year <- as.numeric(sub("^(\\d{4}).*", "\\1", season))
  if (!is.na(year)) return(year)
  NA_real_
}

output_path <- file.path(cache_dir, "01_fixture_results.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 01_fixture_results.rds")
  fixture_results <- readRDS(output_path)
  message(sprintf("  %d historical + %d fixtures",
                  sum(fixture_results$match_status == "Played"),
                  sum(fixture_results$match_status != "Played")))
  return(invisible(NULL))
}

# 4. Try Loading from RAPM Cache ----

rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
use_rapm_cache <- file.exists(rapm_cache)

if (use_rapm_cache) {
  message("Loading results from RAPM pipeline cache...")
  raw_data <- readRDS(rapm_cache)
  results <- raw_data$results
  # Filter to requested leagues
  results <- results[results$league %in% leagues, ]
  message(sprintf("  %d matches from RAPM cache (filtered to %s)",
                  nrow(results), paste(leagues, collapse = ", ")))
} else {
  message("No RAPM cache found - loading from Opta data directly...")
  results <- NULL
}

# 5. Load Results from Opta (if no cache) ----

if (is.null(results)) {
  message("\n=== Loading Opta Data ===\n")

  all_results <- list()

  for (league in leagues) {
    opta_league <- to_opta_league(league)
    available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
    if (length(available_seasons) == 0) next

    if (!is.null(seasons)) available_seasons <- intersect(available_seasons, seasons)
    if (!is.null(min_season)) available_seasons <- available_seasons[available_seasons >= min_season]

    for (season in available_seasons) {
      label <- paste(league, season)
      tryCatch({
        lineups <- load_opta_lineups(league, season = season, source = "local")
        events <- load_opta_events(league, season = season, source = "local")

        if (is.null(lineups) || nrow(lineups) == 0) next

        # Build match info from lineups
        match_info <- lineups %>%
          filter(is_starter) %>%
          group_by(match_id) %>%
          summarise(
            home_team = first(team_name[tolower(team_position) == "home"]),
            away_team = first(team_name[tolower(team_position) == "away"]),
            match_date = first(match_date),
            home_team_id = first(team_id[tolower(team_position) == "home"]),
            away_team_id = first(team_id[tolower(team_position) == "away"]),
            .groups = "drop"
          )

        # Derive goals from events
        goal_counts <- events %>%
          filter(event_type == "goal") %>%
          count(match_id, team_id, name = "goals")

        home_goals <- match_info %>%
          select(match_id, home_team_id) %>%
          left_join(goal_counts, by = c("match_id", "home_team_id" = "team_id")) %>%
          mutate(home_goals = coalesce(goals, 0L)) %>%
          select(match_id, home_goals)

        away_goals <- match_info %>%
          select(match_id, away_team_id) %>%
          left_join(goal_counts, by = c("match_id", "away_team_id" = "team_id")) %>%
          mutate(away_goals = coalesce(goals, 0L)) %>%
          select(match_id, away_goals)

        match_results <- match_info %>%
          left_join(home_goals, by = "match_id") %>%
          left_join(away_goals, by = "match_id") %>%
          mutate(league = !!league, season = !!season)

        all_results[[label]] <- match_results
      }, error = function(e) {
        message(sprintf("  ERROR %s: %s", label, e$message))
      })
    }
  }

  results <- bind_rows(all_results)
  message(sprintf("  Loaded %d matches from Opta", nrow(results)))
}

# 6. Ensure Required Columns ----

if (!"season_end_year" %in% names(results)) {
  results$season_end_year <- sapply(results$season, extract_season_end_year)
}
if (!"home_xg" %in% names(results)) results$home_xg <- NA_real_
if (!"away_xg" %in% names(results)) results$away_xg <- NA_real_

# Compute result label
results$result <- ifelse(results$home_goals > results$away_goals, "H",
                  ifelse(results$home_goals == results$away_goals, "D", "A"))
results$match_status <- "Played"

# Tournament/neutral venue flag
results$is_neutral_venue <- as.integer(results$league %in% TOURNAMENT_LEAGUES)

# 7. Load Fixtures (Upcoming Matches) ----

message("\n=== Loading Fixtures ===\n")

all_fixtures <- list()
for (league in leagues) {
  available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
  if (length(available_seasons) == 0) next

  # Only load current/recent season for fixtures
  current_season <- available_seasons[1]

  tryCatch({
    fixtures <- load_opta_fixtures(league, season = current_season, status = "Fixture", source = "local")
    if (!is.null(fixtures) && nrow(fixtures) > 0) {
      fixtures$league <- league
      fixtures$season_end_year <- extract_season_end_year(current_season)
      fixtures$home_goals <- NA_integer_
      fixtures$away_goals <- NA_integer_
      fixtures$home_xg <- NA_real_
      fixtures$away_xg <- NA_real_
      fixtures$result <- NA_character_
      fixtures$is_neutral_venue <- as.integer(league %in% TOURNAMENT_LEAGUES)
      all_fixtures[[league]] <- fixtures
      message(sprintf("  %s %s: %d upcoming fixtures", league, current_season, nrow(fixtures)))
    }
  }, error = function(e) {
    message(sprintf("  No fixtures for %s: %s", league, e$message))
  })
}

fixtures_df <- if (length(all_fixtures) > 0) bind_rows(all_fixtures) else NULL

# 8. Combine ----

# Ensure consistent columns
keep_cols <- c("match_id", "match_date", "match_status", "league", "season",
               "season_end_year", "home_team", "away_team", "home_team_id",
               "away_team_id", "home_goals", "away_goals", "home_xg", "away_xg",
               "result", "is_neutral_venue")

# Add missing columns
for (col in keep_cols) {
  if (!col %in% names(results)) results[[col]] <- NA
}
results_clean <- results[, intersect(keep_cols, names(results))]

if (!is.null(fixtures_df)) {
  for (col in keep_cols) {
    if (!col %in% names(fixtures_df)) fixtures_df[[col]] <- NA
  }
  fixtures_clean <- fixtures_df[, intersect(keep_cols, names(fixtures_df))]
  fixture_results <- bind_rows(results_clean, fixtures_clean)
} else {
  fixture_results <- results_clean
}

fixture_results <- fixture_results[order(fixture_results$match_date), ]

# 9. Save ----

saveRDS(fixture_results, output_path)

# 10. Summary ----

message("\n========================================")
message("Fixture results complete!")
message("========================================")
message(sprintf("Historical: %d matches", sum(fixture_results$match_status == "Played")))
message(sprintf("Upcoming: %d fixtures", sum(fixture_results$match_status != "Played", na.rm = TRUE)))
message(sprintf("Leagues: %d", length(unique(fixture_results$league))))
message(sprintf("Date range: %s to %s", min(fixture_results$match_date, na.rm = TRUE),
                max(fixture_results$match_date, na.rm = TRUE)))
message(sprintf("\nSaved to: %s", output_path))
