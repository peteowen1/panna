# 06_calculate_wpa.R
# Score all SPADL actions with WP model and compute per-game WPA
#
# Run from panna directory: Rscript data-raw/epv/06_calculate_wpa.R
#
# Requires:
#   - Trained WP model (from step 05)
#   - SPADL data with EPV credits
#   - Match results (for final outcome labels)
#
# Outputs:
#   - data-raw/cache/epv/players/player_game_wpa_{league}_{season}.rds

library(cli)
devtools::load_all()

# ============================================================================
# Internal helper (defined before use)
# ============================================================================

.build_match_results_from_events <- function(events, lineups) {
  dt_events <- data.table::as.data.table(events)
  dt_lineups <- data.table::as.data.table(lineups)

  if ("team_position" %in% names(dt_lineups)) {
    match_teams <- dt_lineups[, .(
      home_team_id = team_id[tolower(team_position) == "home"][1],
      away_team_id = team_id[tolower(team_position) == "away"][1]
    ), by = match_id]
  } else if ("is_home" %in% names(dt_lineups)) {
    match_teams <- dt_lineups[, .(
      home_team_id = team_id[is_home == 1L][1],
      away_team_id = team_id[is_home == 0L][1]
    ), by = match_id]
  } else {
    cli::cli_abort("Lineups must have team_position or is_home column")
  }

  goals <- dt_events[type_id == 16L]
  if (nrow(goals) == 0 && "type_name" %in% names(dt_events)) {
    goals <- dt_events[grepl("[Gg]oal", type_name) & !grepl("[Oo]wn", type_name)]
  }

  goal_counts <- goals[, .N, by = .(match_id, team_id)]
  match_teams[goal_counts, home_goals := i.N, on = .(match_id, home_team_id = team_id)]
  match_teams[goal_counts, away_goals := i.N, on = .(match_id, away_team_id = team_id)]
  match_teams[is.na(home_goals), home_goals := 0L]
  match_teams[is.na(away_goals), away_goals := 0L]

  as.data.frame(match_teams)
}

# 1. Configuration ----

LEAGUES <- if (exists("leagues")) leagues else c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- if (exists("seasons")) seasons else c("2023-2024")

CACHE_DIR <- "data-raw/cache/epv"
OUTPUT_DIR <- "data-raw/cache/epv/players"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate WPA (Win Probability Added)")

# 2. Load WP model ----

cli_h2("Step 1: Load WP Model")

wp_model <- load_wp_model()
cli_alert_success("WP model loaded ({length(wp_model$feature_names)} features)")

# 3. Process each league/season ----

cli_h2("Step 2: Calculate WPA per Player per Game")

all_wpa <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Processing {league} {season}...")

    tryCatch({
      # Load data
      events <- load_opta_match_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      # Convert to SPADL
      spadl <- convert_opta_to_spadl(events)
      spadl_chains <- create_possession_chains(spadl)

      # Build match results
      match_results <- .build_match_results_from_events(events, lineups)

      # Create WP features
      wp_features <- create_wp_features(spadl_chains, match_results)

      # Add WP and WPA
      spadl_wpa <- add_wp_vars(wp_features, wp_model)

      # Assign credit between actor and receiver
      spadl_wpa <- assign_wpa_credit(spadl_wpa)

      # Write per-action WPA for chain enrichment by pannadata's
      # build_chains_ci.R. Mirrors the per-season action_equity_*.parquet
      # pattern from 10c_export_equity.R, but sharded per (league, season)
      # to bound memory if/when this runs across all 15 leagues × 12 seasons.
      # Schema matches the followup_panna_chains_per_event_wp memory note:
      # match_id, event_id, wp, wpa, wpa_actor, wpa_receiver.
      action_wpa_file <- file.path(opta_data_dir(),
                                    sprintf("action_wpa_%s_%s.parquet",
                                            league, season))
      action_wpa_cols <- intersect(
        c("match_id", "original_event_id",
          "wp", "wpa", "wpa_actor", "wpa_receiver"),
        names(spadl_wpa))
      action_wpa_dt <- spadl_wpa[, ..action_wpa_cols]
      if ("original_event_id" %in% names(action_wpa_dt)) {
        data.table::setnames(action_wpa_dt, "original_event_id", "event_id")
      }
      arrow::write_parquet(action_wpa_dt, action_wpa_file)
      cli_alert_info("    action_wpa written: {action_wpa_file} ({nrow(action_wpa_dt)} rows)")
      rm(action_wpa_dt)

      # Aggregate per player per game
      player_game_wpa <- aggregate_player_game_wpa(spadl_wpa, lineups)
      player_game_wpa$league <- league
      player_game_wpa$season <- season

      # Save
      output_file <- file.path(OUTPUT_DIR,
                                sprintf("player_game_wpa_%s_%s.rds", league, season))
      saveRDS(player_game_wpa, output_file)

      all_wpa[[paste(league, season)]] <- player_game_wpa
      n_matches <- length(unique(player_game_wpa$match_id))
      cli_alert_success("  {nrow(player_game_wpa)} player-games from {n_matches} matches")

    }, error = function(e) {
      cli_alert_warning("  Skipping {league} {season}: {e$message}")
    })
  }
}

# 4. Summary ----

cli_h1("Complete!")

if (length(all_wpa) > 0) {
  combined <- data.table::rbindlist(all_wpa, fill = TRUE)
  cli_alert_success("Total: {nrow(combined)} player-games")

  cat("\nTop 20 Players by Total WPA:\n")
  top_wpa <- head(combined[order(-combined$wpa_total), ], 20)
  print(top_wpa[, c("player_name", "league", "match_id",
                     "wpa_total", "wpa_as_actor", "wpa_as_receiver", "max_wpa")])
}
