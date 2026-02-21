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

# Use skill-based ratings if available (from estimated skills pipeline)
if (!exists("use_skill_ratings")) use_skill_ratings <- TRUE

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

# Load seasonal ratings: prefer skill-based, fall back to raw-stat
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")

if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  message("  Using SKILL-BASED seasonal ratings (cache-skills/06_seasonal_ratings.rds)")
  seasonal_data <- readRDS(skill_ratings_path)
} else if (file.exists(raw_ratings_path)) {
  if (isTRUE(use_skill_ratings)) {
    warning("Skill ratings not found at ", skill_ratings_path,
            ". Falling back to raw-stat ratings. Run the skills pipeline first.",
            call. = FALSE)
  } else {
    message("  Using raw-stat seasonal ratings (use_skill_ratings = FALSE)")
  }
  seasonal_data <- readRDS(raw_ratings_path)
} else {
  stop("No seasonal ratings found. Run the Opta RAPM pipeline first:\n  ",
       "source('data-raw/player-ratings-opta/run_pipeline_opta.R')")
}

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
      }, error = function(e) {
        message(sprintf("  Warning: failed to load lineups for %s %s: %s", league, season, e$message))
        NULL
      })
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

# For upcoming fixtures, compute date-specific skill ratings if available
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

  # Try date-specific skill ratings for fixtures
  fixture_ratings <- NULL
  skill_cache_dir <- file.path("data-raw", "cache-skills")
  match_stats_path <- file.path(skill_cache_dir, "01_match_stats.rds")
  decay_params_path <- file.path(skill_cache_dir, "02b_decay_params.rds")
  skill_spm_path <- file.path(skill_cache_dir, "03_skill_spm.rds")

  if (isTRUE(use_skill_ratings) && file.exists(match_stats_path) &&
      file.exists(skill_spm_path)) {
    tryCatch({
      message("  Computing date-specific skill estimates for fixtures...")
      match_stats <- readRDS(match_stats_path)
      decay_params <- if (file.exists(decay_params_path)) readRDS(decay_params_path) else NULL
      skill_spm <- readRDS(skill_spm_path)

      # Compute skills at the earliest fixture date (close enough for all)
      fixture_date <- min(as.Date(upcoming$match_date), na.rm = TRUE)
      live_skills <- estimate_player_skills_at_date(
        match_stats = match_stats,
        decay_params = decay_params,
        date = fixture_date
      )

      if (!is.null(live_skills) && nrow(live_skills) > 0) {
        # estimate_player_skills() now outputs "primary_position" directly

        # Add position dummies (SPM models use these as predictors)
        if ("primary_position" %in% names(live_skills)) {
          pos <- live_skills$primary_position
          live_skills$is_gk <- as.integer(grepl("GK|Goalkeeper", pos, ignore.case = TRUE))
          live_skills$is_df <- as.integer(grepl("DEF|Defender", pos, ignore.case = TRUE))
          live_skills$is_mf <- as.integer(grepl("MID|Midfielder", pos, ignore.case = TRUE))
          live_skills$is_fw <- as.integer(grepl("FWD|FW|Forward|Striker", pos, ignore.case = TRUE))
        }

        # Add total_minutes and n_matches (SPM models may use these)
        if (!"total_minutes" %in% names(live_skills)) {
          live_skills$total_minutes <- live_skills$weighted_90s * 90
        }
        if (!"n_matches" %in% names(live_skills)) {
          live_skills$n_matches <- 0L
        }

        # Add mins_per_90 for SPM model compatibility
        if (!"mins_per_90" %in% names(live_skills)) {
          live_skills$mins_per_90 <- live_skills$weighted_90s
        }

        # Generate SPM predictions from live skills
        off_blend <- calculate_spm_blend(
          live_skills, skill_spm$offense_spm_glmnet, skill_spm$offense_spm_xgb
        )
        def_blend <- calculate_spm_blend(
          live_skills, skill_spm$defense_spm_glmnet, skill_spm$defense_spm_xgb
        )

        # Build fixture-specific ratings table
        fixture_ratings <- off_blend %>%
          select(player_name, total_minutes, offense_spm = spm) %>%
          inner_join(
            def_blend %>% select(player_name, defense_spm = spm),
            by = "player_name"
          ) %>%
          mutate(
            panna = offense_spm - defense_spm,
            offense = offense_spm,
            defense = defense_spm,
            spm = panna,
            season_end_year = latest_sey
          )

        message(sprintf("  Live skill ratings for %d players at %s",
                        nrow(fixture_ratings), fixture_date))
      }
    }, error = function(e) {
      message(sprintf("  Date-specific skills failed: %s (using seasonal fallback)", e$message))
    })
  }

  if (length(upcoming_lineups) > 0) {
    upcoming_lu <- bind_rows(upcoming_lineups)
    # Use fixture-specific ratings if available, else seasonal fallback
    fixture_rat <- if (!is.null(fixture_ratings)) fixture_ratings else ratings
    tryCatch({
      upcoming_tr <- aggregate_lineup_ratings(upcoming_lu, fixture_rat,
                                               season_end_year = latest_sey)
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
