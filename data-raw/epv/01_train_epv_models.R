# =============================================================================
# EPV Model Training Pipeline
# =============================================================================
# Trains xG, xPass, and EPV models on Opta event data
#
# Run from panna directory: Rscript data-raw/epv/01_train_epv_models.R
#
# Outputs:
#   - data-raw/cache/epv/xg_model.rds
#   - data-raw/cache/epv/xpass_model.rds
#   - data-raw/cache/epv/epv_model.rds
# =============================================================================

library(cli)
devtools::load_all()

# =============================================================================
# Configuration
# =============================================================================

# Leagues and seasons to train on
LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- c("2022-2023", "2023-2024")

# EPV method: "goal" (multinomial) or "xg" (regression)
EPV_METHOD <- "goal"

# XGBoost parameters
XGB_PARAMS <- list(
  nrounds = 1000,
  early_stopping_rounds = 50,
  verbose = 0
)

# Output directory
CACHE_DIR <- "data-raw/cache/epv"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("EPV Model Training Pipeline")

# =============================================================================
# 1. Load and Prepare Data
# =============================================================================
cli_h2("Step 1: Load Opta Match Events")

all_events <- list()
all_shots <- list()
all_lineups <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Loading {league} {season}...")

    tryCatch({
      events <- load_opta_match_events(league, season = season)
      shots <- load_opta_shot_events(league, season = season)
      lineups <- load_opta_lineups(league, season = season)

      all_events[[paste(league, season)]] <- events
      all_shots[[paste(league, season)]] <- shots
      all_lineups[[paste(league, season)]] <- lineups

      cli_alert_success("  {nrow(events)} events, {nrow(shots)} shots")
    }, error = function(e) {
      cli_alert_warning("  Skipping: {e$message}")
    })
  }
}

# Combine all data
events <- do.call(rbind, all_events)
shots <- do.call(rbind, all_shots)
lineups <- do.call(rbind, all_lineups)

cli_alert_success("Total: {format(nrow(events), big.mark=',')} events from {length(unique(events$match_id))} matches")

# =============================================================================
# 2. Convert to SPADL
# =============================================================================
cli_h2("Step 2: Convert to SPADL Format")

spadl <- convert_opta_to_spadl(events)
cli_alert_success("SPADL: {format(nrow(spadl), big.mark=',')} actions")

# =============================================================================
# 3. Create Possession Chains
# =============================================================================
cli_h2("Step 3: Create Possession Chains")

spadl_chains <- create_possession_chains(spadl)
chain_outcomes <- classify_chain_outcomes(spadl_chains)
chain_outcomes <- add_next_chain_outcome(chain_outcomes)
spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)

cli_alert_success("Chains: {nrow(chain_outcomes)} possession sequences")

# =============================================================================
# 4. Create Labels
# =============================================================================
cli_h2("Step 4: Create Labels ({EPV_METHOD} method)")

spadl_labeled <- create_next_goal_labels(spadl_labeled)

if (EPV_METHOD == "xg") {
  spadl_labeled <- create_next_xg_labels(spadl_labeled)
}

# =============================================================================
# 5. Train xG Model
# =============================================================================
cli_h2("Step 5: Train xG Model")

shot_features <- prepare_shots_for_xg(shots)
xg_model <- fit_xg_model(shot_features,
                          nrounds = XGB_PARAMS$nrounds,
                          early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                          verbose = XGB_PARAMS$verbose)

cli_alert_success("xG Model: best iter={xg_model$best_nrounds}, logloss={round(xg_model$best_logloss, 4)}")
saveRDS(xg_model, file.path(CACHE_DIR, "xg_model.rds"))

# =============================================================================
# 6. Train xPass Model
# =============================================================================
cli_h2("Step 6: Train xPass Model")

pass_features <- prepare_passes_for_xpass(spadl)
xpass_model <- fit_xpass_model(pass_features,
                                nrounds = XGB_PARAMS$nrounds,
                                early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                                verbose = XGB_PARAMS$verbose)

cli_alert_success("xPass Model: best iter={xpass_model$best_nrounds}, logloss={round(xpass_model$best_logloss, 4)}")
saveRDS(xpass_model, file.path(CACHE_DIR, "xpass_model.rds"))

# =============================================================================
# 7. Train EPV Model
# =============================================================================
cli_h2("Step 7: Train EPV Model")

epv_features <- create_epv_features(spadl_labeled, n_prev = 3)
epv_model <- fit_epv_model(epv_features, spadl_labeled,
                            method = EPV_METHOD,
                            nrounds = XGB_PARAMS$nrounds,
                            early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                            verbose = XGB_PARAMS$verbose)

metric_name <- if (EPV_METHOD == "goal") "mlogloss" else "rmse"
cli_alert_success("EPV Model: best iter={epv_model$best_nrounds}, {metric_name}={round(epv_model$best_metric, 4)}")
saveRDS(epv_model, file.path(CACHE_DIR, "epv_model.rds"))

# =============================================================================
# 8. Summary
# =============================================================================
cli_h1("Training Complete!")

cli_alert_info("Models saved to {CACHE_DIR}/")
cli_alert_info("  - xg_model.rds")
cli_alert_info("  - xpass_model.rds")
cli_alert_info("  - epv_model.rds")

cat("\nFeature Importance (EPV model, top 10):\n")
print(head(epv_model$importance, 10))
