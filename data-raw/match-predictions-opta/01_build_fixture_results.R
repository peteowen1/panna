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

# extract_season_end_year() is defined in R/utils.R

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

        # Drop matches with lineups but no events — Opta publishes lineups
        # ahead of events for recent fixtures, so the match looks "played"
        # but we can't compute a score. Without this guard, coalesce(goals, 0L)
        # below silently codes them as 0-0 draws, corrupting standings.
        matches_with_events <- unique(events$match_id)
        matches_without_events <- setdiff(match_info$match_id, matches_with_events)
        if (length(matches_without_events) > 0) {
          message(sprintf("  WARNING: %s %s: dropping %d matches with no events (scraper gap)",
                          league, season, length(matches_without_events)))
          match_info <- match_info %>% filter(!match_id %in% matches_without_events)
        }

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

  # Normalize fixture team names to the variant Opta uses in its lineup feed.
  # Opta's fixtures endpoint sometimes serves full legal names ("AFC Ajax",
  # "BV Borussia 09 Dortmund", "Çaykur Rize Spor Kulübü") while lineups use a
  # different spelling ("Ajax", "Borussia Dortmund", "Rizespor"). That split
  # caused sim scripts to treat them as different teams and inflate team counts.
  # Use team_id (stable across both feeds) to rewrite fixture names to match.
  team_variants <- results_clean %>%
    filter(!is.na(home_team_id), !is.na(home_team)) %>%
    select(team_id = home_team_id, team_name = home_team) %>%
    bind_rows(
      results_clean %>%
        filter(!is.na(away_team_id), !is.na(away_team)) %>%
        select(team_id = away_team_id, team_name = away_team)
    ) %>%
    count(team_id, team_name, name = "n")

  # Deterministic tie-break: prefer the most-frequent name; on ties, prefer the
  # shortest (lineup variants tend to be shorter than fixture-feed legal names)
  # then alphabetically. Without this, the canonical name for a mid-season
  # renamed team could flip between pipeline runs based on row ordering, and
  # downstream name-keyed consumers (blog standings, PSR rollups) would see
  # phantom "team renamed" events.
  team_name_map <- team_variants %>%
    group_by(team_id) %>%
    arrange(desc(n), nchar(team_name), team_name, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(team_id, team_name)

  # Report team_ids with multiple name variants — these are the genuine
  # split-identity cases and the debug info someone will want next time
  # the standings view shows a split team.
  collisions <- team_variants %>% count(team_id, name = "variants") %>% filter(variants > 1)
  if (nrow(collisions) > 0) {
    message(sprintf("  %d team_ids have multiple name variants in lineups (canonical = most frequent, tie-break = shortest)",
                    nrow(collisions)))
  }

  n_home_renamed <- 0L
  n_away_renamed <- 0L
  unresolved_home <- 0L
  unresolved_away <- 0L
  if (nrow(team_name_map) > 0) {
    home_lookup <- setNames(team_name_map$team_name, team_name_map$team_id)
    new_home <- home_lookup[as.character(fixtures_clean$home_team_id)]
    n_home_renamed <- sum(!is.na(new_home) & (is.na(fixtures_clean$home_team) | new_home != fixtures_clean$home_team))
    unresolved_home <- sum(!is.na(fixtures_clean$home_team_id) & is.na(new_home))
    fixtures_clean$home_team <- ifelse(is.na(new_home), fixtures_clean$home_team, unname(new_home))

    new_away <- home_lookup[as.character(fixtures_clean$away_team_id)]
    n_away_renamed <- sum(!is.na(new_away) & (is.na(fixtures_clean$away_team) | new_away != fixtures_clean$away_team))
    unresolved_away <- sum(!is.na(fixtures_clean$away_team_id) & is.na(new_away))
    fixtures_clean$away_team <- ifelse(is.na(new_away), fixtures_clean$away_team, unname(new_away))
  }

  # Always-on diagnostic: distinguishes "rename block ran correctly" from
  # "rename block silently skipped due to upstream issue" (empty lineup map,
  # zero fixture rows, etc.). Even the zero cases should be visible in logs.
  message(sprintf("  Fixture team-name normalization: %d lineup variants, %d fixtures, %d renamed",
                  nrow(team_name_map), nrow(fixtures_clean),
                  n_home_renamed + n_away_renamed))

  # Unresolved team_ids (fixture has a team_id absent from the lineup map) are
  # the silent split-identity risk: a newly-promoted team with zero played
  # matches yet keeps its Opta-fixtures name, and the moment they play their
  # first match the same team_id will start producing the lineup variant —
  # reintroducing the exact bug this block is meant to prevent. Warn loudly.
  if (unresolved_home + unresolved_away > 0) {
    message(sprintf("  WARNING: %d fixture team_ids have no lineup match (home: %d, away: %d) — names kept as-is, may cause split-identity after first played match",
                    unresolved_home + unresolved_away, unresolved_home, unresolved_away))
  }

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
