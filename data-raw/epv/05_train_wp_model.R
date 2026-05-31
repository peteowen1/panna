# 05_train_wp_model.R
# Train win probability model from historical SPADL + match results
#
# Run from panna directory: Rscript data-raw/epv/05_train_wp_model.R
#
# Requires:
#   - Opta match events and lineups (local data)
#
# Outputs:
#   - pannadata/data/opta/models/wp_model.rds

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

  # Exclude penalty-shootout goals (period_id >= 5): a match decided on pens is
  # a draw in open play, so shootout conversions must not produce a win/loss
  # label for what was actually a drawn match.
  reg_events <- if ("period_id" %in% names(dt_events)) {
    dt_events[!is_shootout_period(period_id)]
  } else {
    dt_events
  }
  goals <- reg_events[type_id == 16L]
  if (nrow(goals) == 0 && "type_name" %in% names(reg_events)) {
    goals <- reg_events[grepl("[Gg]oal", type_name) & !grepl("[Oo]wn", type_name)]
  }

  goal_counts <- goals[, .N, by = .(match_id, team_id)]
  match_teams[goal_counts, home_goals := i.N, on = .(match_id, home_team_id = team_id)]
  match_teams[goal_counts, away_goals := i.N, on = .(match_id, away_team_id = team_id)]
  match_teams[is.na(home_goals), home_goals := 0L]
  match_teams[is.na(away_goals), away_goals := 0L]

  as.data.frame(match_teams)
}

# ============================================================================
# 1. Configuration
# ============================================================================

# Default training scope MUST include the cup + continental competitions —
# they are the ONLY source of extra-time game states (domestic leagues never
# go to ET). Training on Big-5 leagues alone yields ZERO extra-time rows, so
# the is_extra_time feature + 120-min time denominator would have nothing to
# learn from and ET matches would still be mis-scored. The cups below carry
# ~470k ET actions across these seasons (DFB-Pokal/Copa del Rey/FA Cup are the
# richest). Domestic Big-5 still supply the bulk of regulation signal.
LEAGUES <- if (exists("leagues")) leagues else c(
  # Domestic (regulation signal)
  "ENG", "ESP", "GER", "ITA", "FRA",
  # Continental + domestic cups (extra-time + shootout signal)
  "UCL", "UEL", "UECL", "FA_Cup", "League_Cup", "Copa_del_Rey",
  "Coppa_Italia", "DFB_Pokal", "Coupe_de_France", "KNVB_Beker"
)
SEASONS <- if (exists("seasons")) seasons else c("2020-2021", "2021-2022",
                                                   "2022-2023", "2023-2024")

MODEL_DIR <- file.path(opta_data_dir(), "models")
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Train Win Probability Model")

# ============================================================================
# 2. Load and prepare training data
# ============================================================================

cli_h2("Step 1: Load SPADL + match results")

all_wp_features <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Loading {league} {season}...")

    tryCatch({
      events <- load_opta_match_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      spadl <- convert_opta_to_spadl(events)
      spadl_chains <- create_possession_chains(spadl)

      match_results <- .build_match_results_from_events(events, lineups)
      rm(events); gc(verbose = FALSE)

      wp_feat <- create_wp_features(spadl_chains, match_results)
      rm(spadl, spadl_chains, match_results); gc(verbose = FALSE)

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
rm(all_wp_features); gc(verbose = FALSE)
cli_alert_success("Total: {nrow(wp_data)} actions from {length(unique(wp_data$match_id))} matches")

# ============================================================================
# 3. Train model
# ============================================================================

cli_h2("Step 2: Train XGBoost WP Model")

# Match AFL v3 training harness: eta=0.1, max_depth=4, 5-fold match-grouped CV,
# early_stopping_rounds=20. nrounds=500 is the cap — early stopping typically
# halts well before that (AFL v3 stopped at 172).
wp_model <- train_wp_model(wp_data, nrounds = 500L, max_depth = 4L, eta = 0.1)
rm(wp_data); gc(verbose = FALSE)

# ============================================================================
# 4. Save model
# ============================================================================

cli_h2("Step 3: Save Model")

save_wp_model(wp_model, MODEL_DIR)

cli_h1("Complete!")
