# 05_train_wp_model.R
# Train win probability model from historical SPADL + match results
#
# Run from panna directory: Rscript data-raw/epv/05_train_wp_model.R
#
# Requires:
#   - SPADL data with EPV credits (from step 02)
#   - Match results (from cache-opta/ or Opta lineups)
#
# Outputs:
#   - pannadata/data/opta/models/wp_model.rds

library(cli)
devtools::load_all()

# 1. Configuration ----

# Use same leagues/seasons as EPV pipeline
LEAGUES <- if (exists("leagues")) leagues else c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- if (exists("seasons")) seasons else c("2018-2019", "2019-2020", "2020-2021",
                                                   "2021-2022", "2022-2023", "2023-2024")

CACHE_DIR <- "data-raw/cache/epv"
MODEL_DIR <- file.path(opta_data_dir(), "models")
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Train Win Probability Model")

# 2. Load and prepare training data ----

cli_h2("Step 1: Load SPADL data with EPV")

all_wp_features <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Loading {league} {season}...")

    tryCatch({
      # Load SPADL with EPV (cached from step 02)
      events <- load_opta_match_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      # Convert to SPADL
      spadl <- convert_opta_to_spadl(events)
      spadl_chains <- create_possession_chains(spadl)

      # Build match results from lineups/events
      match_results <- .build_match_results_from_events(events, lineups)

      # Create WP features
      wp_feat <- create_wp_features(spadl_chains, match_results)
      wp_feat$league <- league
      wp_feat$season <- season

      all_wp_features[[paste(league, season)]] <- wp_feat
      n_matches <- length(unique(wp_feat$match_id))
      cli_alert_success("  {nrow(wp_feat)} actions from {n_matches} matches")

    }, error = function(e) {
      cli_alert_warning("  Skipping {league} {season}: {e$message}")
    })
  }
}

wp_data <- data.table::rbindlist(all_wp_features, fill = TRUE)
cli_alert_success("Total training data: {nrow(wp_data)} actions from {length(unique(wp_data$match_id))} matches")

# 3. Train model ----

cli_h2("Step 2: Train XGBoost WP Model")

wp_model <- train_wp_model(wp_data, nrounds = 300L, max_depth = 4L, eta = 0.03)

# 4. Save model ----

cli_h2("Step 3: Save Model")

save_wp_model(wp_model, MODEL_DIR)

cli_h1("Complete!")


# ============================================================================
# Internal helper
# ============================================================================

#' Build match results from Opta events/lineups
#' @keywords internal
.build_match_results_from_events <- function(events, lineups) {
  dt_events <- data.table::as.data.table(events)
  dt_lineups <- data.table::as.data.table(lineups)

  # Get home/away team per match from lineups
  match_teams <- dt_lineups[, .(
    home_team_id = team_id[is_home == 1L][1],
    away_team_id = team_id[is_home == 0L][1]
  ), by = match_id]

  # Count goals per team per match from events
  goals <- dt_events[type_id == 16L | (grepl("[Gg]oal", type_name) & !grepl("[Oo]wn", type_name))]
  if (nrow(goals) == 0) {
    # Fallback: use SPADL shot successes
    goals <- dt_events[type_name == "Goal"]
  }

  goal_counts <- goals[, .N, by = .(match_id, team_id)]

  # Pivot to home/away goals
  match_teams[goal_counts, home_goals := i.N,
              on = .(match_id, home_team_id = team_id)]
  match_teams[goal_counts, away_goals := i.N,
              on = .(match_id, away_team_id = team_id)]
  match_teams[is.na(home_goals), home_goals := 0L]
  match_teams[is.na(away_goals), away_goals := 0L]

  as.data.frame(match_teams)
}
