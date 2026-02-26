# 02b_team_skill_features.R
# Compute team-level skill features for the match dataset
#
# For each played match, computes skill estimates for the starting XI at
# the match date, then aggregates to team-level attacking/defensive skill
# features. For fixtures, uses the most recent skill estimates.
#
# These features supplement the panna/Elo/rolling features with granular
# skill information (e.g., team average shooting skill, tackling skill).

# 1. Setup ----

# 2. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
skill_cache_dir <- file.path("data-raw", "cache-skills")
output_path <- file.path(cache_dir, "02b_team_skill_features.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 02b_team_skill_features.rds")
  team_skill_features <- readRDS(output_path)
  message(sprintf("  %d matches with team skill features", nrow(team_skill_features)))
  return(invisible(NULL))
}

# 4. Check Prerequisites ----

match_stats_path <- file.path(skill_cache_dir, "01_match_stats.rds")
decay_params_path <- file.path(skill_cache_dir, "02b_decay_params.rds")

if (!file.exists(match_stats_path)) {
  warning("Skill match stats not found - model will train WITHOUT skill features. ",
          "Run the estimated skills pipeline first.", call. = FALSE)
  team_skill_features <- NULL
  saveRDS(team_skill_features, output_path)
  return(invisible(NULL))
}

message("\n=== Computing Team-Level Skill Features ===\n")

# 5. Load Data ----

fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
match_stats <- readRDS(match_stats_path)
decay_params <- if (file.exists(decay_params_path)) readRDS(decay_params_path) else NULL

# Load lineups
rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (file.exists(rapm_cache)) {
  raw_data <- readRDS(rapm_cache)
  lineups <- raw_data$lineups
} else {
  warning("RAPM cache not found for lineups - model will train WITHOUT skill features.",
          call. = FALSE)
  team_skill_features <- NULL
  saveRDS(team_skill_features, output_path)
  return(invisible(NULL))
}

played <- fixture_results[fixture_results$match_status == "Played", ]
upcoming <- fixture_results[fixture_results$match_status != "Played", ]

message(sprintf("  Played matches: %d", nrow(played)))
message(sprintf("  Match stats rows: %d", nrow(match_stats)))

# 6. Compute Seasonal Skill Estimates ----

# Instead of computing per-match-date (very expensive), compute per season end
# and use those for all matches in that season
sey_values <- sort(unique(played$season_end_year))
all_skill_features <- list()
n_failed <- 0L

for (sey in sey_values) {
  # Use season START (previous July) to avoid data leakage: skills estimated
  # before the season began, so no future information enters the training set
  target_date <- as.Date(paste0(sey - 1, "-07-01"))

  tryCatch({
    skills <- estimate_player_skills(
      match_stats = match_stats,
      decay_params = decay_params,
      target_date = target_date
    )

    if (is.null(skills) || nrow(skills) == 0) next

    # Get lineups for this season's matches
    matches_sey <- played[played$season_end_year == sey, ]
    match_ids <- unique(matches_sey$match_id)
    lu_sey <- lineups[lineups$match_id %in% match_ids, ]

    if (nrow(lu_sey) == 0) next

    # Aggregate skills to team level
    tr <- aggregate_lineup_skills(lu_sey, skills)
    if (!is.null(tr) && nrow(tr) > 0) {
      all_skill_features[[as.character(sey)]] <- tr
      message(sprintf("    SEY %d: %d matches with skill features", sey, nrow(tr)))
    }
  }, error = function(e) {
    n_failed <<- n_failed + 1L
    message(sprintf("    SEY %d ERROR: %s", sey, e$message))
  })
}

if (n_failed > 0) {
  warning(sprintf("%d/%d season-end-years failed skill estimation.",
                  n_failed, length(sey_values)), call. = FALSE)
}

# 7. Handle Fixtures ----

if (nrow(upcoming) > 0) {
  tryCatch({
    # Use current date skills for fixtures
    live_skills <- estimate_player_skills(
      match_stats = match_stats,
      decay_params = decay_params,
      target_date = Sys.Date()
    )

    if (!is.null(live_skills) && nrow(live_skills) > 0) {
      latest_lineups <- lineups %>%
        filter(is_starter) %>%
        group_by(team_name) %>%
        filter(match_date == max(match_date)) %>%
        ungroup()

      # Build fixture lineups
      fixture_lu_list <- list()
      for (i in seq_len(nrow(upcoming))) {
        m <- upcoming[i, ]
        home_lu <- latest_lineups %>%
          filter(team_name == m$home_team) %>%
          mutate(match_id = m$match_id, team_position = "home")
        away_lu <- latest_lineups %>%
          filter(team_name == m$away_team) %>%
          mutate(match_id = m$match_id, team_position = "away")
        if (nrow(home_lu) > 0 && nrow(away_lu) > 0) {
          fixture_lu_list[[i]] <- bind_rows(home_lu, away_lu)
        }
      }

      if (length(fixture_lu_list) > 0) {
        fixture_lu <- bind_rows(fixture_lu_list)
        fixture_tr <- aggregate_lineup_skills(fixture_lu, live_skills)
        if (!is.null(fixture_tr) && nrow(fixture_tr) > 0) {
          all_skill_features[["fixtures"]] <- fixture_tr
          message(sprintf("  Added %d fixture skill features", nrow(fixture_tr)))
        }
      }
    }
  }, error = function(e) {
    warning(sprintf("Fixture skills failed: %s (using seasonal only)", e$message),
            call. = FALSE)
  })
}

# 8. Combine and Save ----

if (length(all_skill_features) > 0) {
  team_skill_features <- bind_rows(all_skill_features)
  message(sprintf("\nTotal: %d matches with team skill features (%d columns)",
                  nrow(team_skill_features), ncol(team_skill_features)))
} else {
  warning("No team skill features computed - model will train WITHOUT skill features.",
          call. = FALSE)
  team_skill_features <- NULL
}

saveRDS(team_skill_features, output_path)
message(sprintf("Saved to: %s", output_path))
