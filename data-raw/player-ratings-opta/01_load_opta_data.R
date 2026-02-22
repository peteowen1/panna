# 01_load_opta_data.R
# Load Opta data and score shots with xG model
#
# Loads Opta lineups, events, stats, and xMetrics. Loads SPADL cache,
# scores shots with the pre-trained xG model, detects penalties from
# raw events, and computes match-level xG for splint creation.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Inherit from parent pipeline if available
leagues <- if (exists("leagues")) leagues else c("ENG", "ESP", "GER", "ITA", "FRA")
seasons <- if (exists("seasons")) seasons else NULL
min_season <- if (exists("min_season")) min_season else NULL
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# PENALTY_XG is exported from panna::constants.R (loaded via devtools::load_all())
SPADL_CACHE_DIR <- "data-raw/cache/epv/spadl"

# Helper: extract season end year from both standard and tournament formats
# "2024-2025" -> 2025, "2018 Russia" -> 2018, "2024 Germany" -> 2024
extract_season_end_year <- function(season) {
  # Standard "YYYY-YYYY" format
  if (grepl("^\\d{4}-\\d{4}$", season)) return(as.numeric(substr(season, 6, 9)))
  # Tournament "YYYY Country" format (year IS the end year)
  year <- as.numeric(sub("^(\\d{4}).*", "\\1", season))
  if (!is.na(year)) return(year)
  NA_real_
}

raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
config_path <- file.path(cache_dir, "01_config.rds")

# 3. Check Cache ----

current_config <- list(leagues = leagues, seasons = seasons,
                       min_season = min_season,
                       use_xmetrics_features = use_xmetrics_features)

if (file.exists(raw_data_path) && file.exists(config_path)) {
  cached_config <- readRDS(config_path)
  if (identical(cached_config, current_config)) {
    message("Cache is up to date - skipping data load")
    raw_opta_data <- readRDS(raw_data_path)
    message(sprintf("  Loaded from cache: %d matches, %d lineups",
                    nrow(raw_opta_data$results), nrow(raw_opta_data$lineups)))
    return(invisible(NULL))
  }
  message("Configuration changed - reloading data")
}

# 4. Load xG Model ----

message("\n=== Loading xG Model ===\n")
xg_model <- load_xg_model()

# 5. Load Data by League-Season ----

message("\n=== Loading Opta Data ===\n")

all_lineups <- list()
all_events <- list()
all_stats <- list()
all_xmetrics <- list()
all_shots_from_spadl <- list()
all_match_xg <- list()

for (league in leagues) {
  opta_league <- to_opta_league(league)
  available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))

  if (length(available_seasons) == 0) {
    message(sprintf("  No seasons found for %s, skipping", league))
    next
  }

  if (!is.null(seasons)) {
    available_seasons <- intersect(available_seasons, seasons)
  }

  # Filter by min_season (works for both "2024-2025" and "2018 Russia" via string comparison)
  if (!is.null(min_season)) {
    available_seasons <- available_seasons[available_seasons >= min_season]
  }

  message(sprintf("\n--- %s (%s): %d seasons ---", league, opta_league, length(available_seasons)))

  for (season in available_seasons) {
    label <- paste(league, season)
    message(sprintf("  Loading %s...", label))

    tryCatch({
      # Load standard Opta data
      lineups <- load_opta_lineups(league, season = season)
      events <- load_opta_events(league, season = season)
      stats <- load_opta_stats(league, season = season)

      if (is.null(lineups) || nrow(lineups) == 0) {
        message(sprintf("    Skipping %s: no lineup data", label))
        next
      }

      lineups$league <- league
      lineups$season <- season
      events$league <- league
      events$season <- season
      stats$league <- league
      stats$season <- season

      all_lineups[[label]] <- lineups
      all_events[[label]] <- events
      all_stats[[label]] <- stats

      # Load xMetrics if enabled
      if (use_xmetrics_features) {
        xmetrics <- tryCatch(
          load_opta_xmetrics(league, season = season),
          error = function(e) NULL
        )
        if (!is.null(xmetrics) && nrow(xmetrics) > 0) {
          xmetrics$league <- league
          xmetrics$season <- season
          all_xmetrics[[label]] <- xmetrics
        }
      }

      # Load SPADL cache and score shots
      spadl_cache <- file.path(SPADL_CACHE_DIR,
                               sprintf("spadl_%s_%s.rds", league, season))

      if (file.exists(spadl_cache)) {
        spadl <- readRDS(spadl_cache)

        # Load raw match events for penalty detection
        raw_events <- tryCatch(
          load_opta_match_events(league, season = season),
          error = function(e) NULL
        )

        # Detect penalties from raw events (qualifier 9)
        # Uses (match_id, player_id, minute, second) composite key to avoid
        # collisions when a player has multiple shots in the same minute
        spadl$is_penalty <- 0L
        if (!is.null(raw_events) && nrow(raw_events) > 0) {
          # Match qualifier "9" as a standalone value in the JSON array
          # Anchored regex avoids false positives from "19", "90", "109", etc.
          has_pen_qualifier <- grepl(
            '(^|[,\\[])\\s*"9"\\s*(,|\\]|$)', raw_events$qualifier_json
          )
          pen_mask <- raw_events$type_id %in% c(13L, 14L, 15L, 16L) & has_pen_qualifier
          if (sum(pen_mask) > 0) {
            raw_second <- if ("second" %in% names(raw_events)) {
              raw_events$second[pen_mask]
            } else {
              message(sprintf("    Note: raw events missing 'second' column for %s %s. Penalty matching may be incomplete.", league, season))
              rep(0L, sum(pen_mask))
            }
            pen_keys <- paste(
              raw_events$match_id[pen_mask],
              raw_events$player_id[pen_mask],
              raw_events$minute[pen_mask],
              raw_second,
              sep = "_"
            )
            spadl_shot_idx <- which(spadl$action_type == "shot")
            if (length(spadl_shot_idx) > 0) {
              spadl_keys <- paste(
                spadl$match_id[spadl_shot_idx],
                spadl$player_id[spadl_shot_idx],
                floor(spadl$time_seconds[spadl_shot_idx] / 60),
                spadl$time_seconds[spadl_shot_idx] %% 60,
                sep = "_"
              )
              spadl$is_penalty[spadl_shot_idx[spadl_keys %in% pen_keys]] <- 1L
            }
          }
        }

        # Score shots with xG model
        spadl <- add_xg_to_spadl(spadl, xg_model)

        # Override penalty xG
        penalty_idx <- spadl$action_type == "shot" & spadl$is_penalty == 1L
        if (sum(penalty_idx) > 0) {
          spadl$xg[penalty_idx] <- PENALTY_XG
        }

        # Extract shots for splint pipeline
        shots_df <- extract_shots_from_spadl(spadl, lineups)
        shots_df$league <- league
        shots_df$season <- season
        all_shots_from_spadl[[label]] <- shots_df

        # Compute match-level xG
        # Non-penalty xG per team per match
        np_shots <- shots_df[!shots_df$is_penalty, ]

        if (nrow(np_shots) > 0) {
          # Build team-to-home lookup from lineups
          home_teams <- lineups %>%
            filter(tolower(team_position) == "home") %>%
            distinct(match_id, team_name) %>%
            rename(home_team = team_name)

          match_team_xg <- np_shots %>%
            group_by(match_id, team) %>%
            summarise(team_npxg = sum(xg, na.rm = TRUE), .groups = "drop")

          # Also total xG (with penalties)
          match_team_xg_total <- shots_df %>%
            group_by(match_id, team) %>%
            summarise(team_xg = sum(xg, na.rm = TRUE), .groups = "drop")

          match_team_xg <- match_team_xg %>%
            left_join(match_team_xg_total, by = c("match_id", "team")) %>%
            left_join(home_teams, by = "match_id") %>%
            mutate(is_home = team == home_team)

          home_xg <- match_team_xg %>%
            filter(is_home) %>%
            select(match_id, home_xg = team_xg, home_npxg = team_npxg)

          away_xg <- match_team_xg %>%
            filter(!is_home) %>%
            select(match_id, away_xg = team_xg, away_npxg = team_npxg)

          match_xg <- home_xg %>%
            full_join(away_xg, by = "match_id") %>%
            mutate(
              home_xg = coalesce(home_xg, 0),
              away_xg = coalesce(away_xg, 0),
              home_npxg = coalesce(home_npxg, 0),
              away_npxg = coalesce(away_npxg, 0)
            )

          all_match_xg[[label]] <- match_xg
        }

        message(sprintf("    SPADL: %d shots scored, %d penalties",
                        nrow(shots_df), sum(shots_df$is_penalty)))
      } else {
        message(sprintf("    No SPADL cache for %s", label))
      }

    }, error = function(e) {
      message(sprintf("    ERROR in %s: %s", label, e$message))
    })
  }
}

# 6. Combine All Data ----

message("\n=== Combining Data ===\n")

combined_lineups <- bind_rows(all_lineups)
combined_events <- bind_rows(all_events)
combined_stats <- bind_rows(all_stats)
combined_xmetrics <- if (length(all_xmetrics) > 0) bind_rows(all_xmetrics) else NULL
combined_shots <- if (length(all_shots_from_spadl) > 0) bind_rows(all_shots_from_spadl) else NULL
combined_match_xg <- if (length(all_match_xg) > 0) bind_rows(all_match_xg) else NULL

# Data scale validation: catch partial/empty loads early
n_leagues_loaded <- length(unique(combined_lineups$league))
n_matches_loaded <- length(unique(combined_lineups$match_id))
if (n_leagues_loaded < length(leagues)) {
  warning(sprintf("Expected %d leagues but only loaded %d: %s",
                  length(leagues), n_leagues_loaded,
                  paste(unique(combined_lineups$league), collapse = ", ")),
          call. = FALSE)
}
if (n_matches_loaded < 100 && length(leagues) >= 5) {
  warning(sprintf("Only %d matches loaded for %d leagues. Expected thousands. Check data availability.",
                  n_matches_loaded, n_leagues_loaded),
          call. = FALSE)
}

message(sprintf("  Lineups: %d rows (%d leagues, %d matches)",
                nrow(combined_lineups), n_leagues_loaded, n_matches_loaded))
message(sprintf("  Events: %d rows", nrow(combined_events)))
message(sprintf("  Stats: %d rows", nrow(combined_stats)))
message(sprintf("  xMetrics: %s rows",
                if (!is.null(combined_xmetrics)) nrow(combined_xmetrics) else "N/A"))
message(sprintf("  Shots (SPADL): %s rows",
                if (!is.null(combined_shots)) nrow(combined_shots) else "N/A"))
message(sprintf("  Match xG: %s matches",
                if (!is.null(combined_match_xg)) nrow(combined_match_xg) else "N/A"))

# 7. Create Results Table ----

message("\n=== Creating Results Table ===\n")

# Build match info from lineups (home/away teams)
match_info <- combined_lineups %>%
  filter(is_starter) %>%
  group_by(match_id, league, season) %>%
  summarise(
    home_team = first(team_name[tolower(team_position) == "home"]),
    away_team = first(team_name[tolower(team_position) == "away"]),
    match_date = first(match_date),
    home_team_id = first(team_id[tolower(team_position) == "home"]),
    away_team_id = first(team_id[tolower(team_position) == "away"]),
    .groups = "drop"
  )

# Derive match scores from goal events
goal_counts <- combined_events %>%
  filter(event_type == "goal") %>%
  group_by(match_id, team_id) %>%
  summarise(goals = n(), .groups = "drop")

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

results <- match_info %>%
  left_join(home_goals, by = "match_id") %>%
  left_join(away_goals, by = "match_id") %>%
  select(-home_team_id, -away_team_id) %>%
  mutate(
    season_end_year = sapply(season, extract_season_end_year)
  )

# Join match-level xG
if (!is.null(combined_match_xg)) {
  results <- results %>%
    left_join(combined_match_xg, by = "match_id")
}

# Fill missing xG with 0
if (!"home_xg" %in% names(results)) results$home_xg <- NA_real_
if (!"away_xg" %in% names(results)) results$away_xg <- NA_real_
if (!"home_npxg" %in% names(results)) results$home_npxg <- NA_real_
if (!"away_npxg" %in% names(results)) results$away_npxg <- NA_real_

message(sprintf("  Results: %d matches, %d with xG",
                nrow(results), sum(!is.na(results$home_xg))))

# 8. Save ----

raw_opta_data <- list(
  results = results,
  lineups = combined_lineups,
  events = combined_events,
  stats = combined_stats,
  xmetrics = combined_xmetrics,
  shooting = combined_shots,
  match_xg = combined_match_xg
)

saveRDS(raw_opta_data, raw_data_path)
saveRDS(current_config, config_path)

# 9. Summary ----

message("\n========================================")
message("Opta data loading complete!")
message("========================================")
message(sprintf("Matches: %d", nrow(results)))
message(sprintf("Lineups: %d", nrow(combined_lineups)))
message(sprintf("Stats: %d", nrow(combined_stats)))
message(sprintf("Shots (SPADL xG): %s",
                if (!is.null(combined_shots)) nrow(combined_shots) else "N/A"))

message("\nLeague breakdown:")
league_counts <- table(results$league)
for (l in names(league_counts)) {
  message(sprintf("  %s: %d matches", l, league_counts[l]))
}

message("\nSeason breakdown:")
season_counts <- table(results$season)
for (s in names(sort(unique(results$season)))) {
  message(sprintf("  %s: %d matches", s, season_counts[s]))
}

message(sprintf("\nSaved to: %s", raw_data_path))
