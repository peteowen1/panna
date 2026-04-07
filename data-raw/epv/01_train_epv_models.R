# 01_train_epv_models.R
# Train xG, xPass, and EPV models on Opta event data
#
# Run from panna directory: Rscript data-raw/epv/01_train_epv_models.R
#
# Outputs:
#   - data-raw/cache/epv/xg_model.rds
#   - data-raw/cache/epv/xpass_model.rds
#   - data-raw/cache/epv/epv_model.rds

library(cli)
devtools::load_all()

# 1. Configuration ----

# Train on all 15 leagues (uses OPTA_LEAGUES from opta_loaders.R)
if (!exists("LEAGUES")) LEAGUES <- names(OPTA_LEAGUES)
if (!exists("SEASONS")) SEASONS <- c("2020-2021", "2021-2022", "2022-2023", "2023-2024")

# Tournament seasons use "YYYY Country" format (not "YYYY-YYYY")
TOURNAMENT_SEASONS <- list(
  WC   = c("2014 Brazil", "2018 Russia"),
  EURO = c("2016 France", "2024 Germany")
)

# EPV method: "goal" (multinomial) or "xg" (regression on signed next-shot xG)
EPV_METHOD <- "xg"

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
cli_alert_info("Leagues: {paste(LEAGUES, collapse = ', ')}")
cli_alert_info("Seasons: {paste(SEASONS, collapse = ', ')}")

# 2. Load Data & Convert to SPADL (per-league to preserve league tag) ----

cli_h2("Step 1: Load Opta Events and Convert to SPADL")

all_spadl <- list()
all_shots <- list()
all_lineups <- list()
total_events <- 0L

# Build league → season list (domestic use SEASONS, tournaments use TOURNAMENT_SEASONS)
league_seasons <- list()
for (league in LEAGUES) {
  if (league %in% names(TOURNAMENT_SEASONS)) {
    league_seasons[[league]] <- TOURNAMENT_SEASONS[[league]]
  } else {
    league_seasons[[league]] <- SEASONS
  }
}

for (league in LEAGUES) {
  for (season in league_seasons[[league]]) {
    key <- paste(league, season)

    tryCatch({
      events <- load_opta_match_events(league, season = season, source = "local")
      shots <- load_opta_shot_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      # Convert to SPADL and tag with league
      spadl <- convert_opta_to_spadl(events)
      spadl$league <- league

      all_spadl[[key]] <- spadl
      all_shots[[key]] <- shots
      all_lineups[[key]] <- lineups
      total_events <- total_events + nrow(events)

      cli_alert_success("  {key}: {nrow(events)} events -> {nrow(spadl)} SPADL actions")
    }, error = function(e) {
      cli_alert_warning("  Skipping {key}: {e$message}")
    })
  }
}

# Combine
spadl <- data.table::rbindlist(all_spadl, fill = TRUE)
shots <- do.call(rbind, all_shots)
lineups <- do.call(rbind, all_lineups)

n_leagues_loaded <- length(unique(spadl$league))
cli_alert_success("Total: {format(total_events, big.mark=',')} events -> {format(nrow(spadl), big.mark=',')} SPADL actions from {n_leagues_loaded} leagues")

# 3. Create Possession Chains ----

cli_h2("Step 2: Create Possession Chains")

spadl_chains <- create_possession_chains(spadl)
chain_outcomes <- classify_chain_outcomes(spadl_chains)
chain_outcomes <- add_next_chain_outcome(chain_outcomes)
spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)

cli_alert_success("Chains: {nrow(chain_outcomes)} possession sequences")

# 4. Create Labels ----

cli_h2("Step 3: Create Labels ({EPV_METHOD} method)")

spadl_labeled <- create_next_goal_labels(spadl_labeled)

if (EPV_METHOD == "xg") {
  spadl_labeled <- create_next_xg_labels(spadl_labeled)
}

# 5. Train xG Model ----

cli_h2("Step 4: Train xG Model")

shot_features <- prepare_shots_for_xg(shots)
xg_model <- fit_xg_model(shot_features,
                          nrounds = XGB_PARAMS$nrounds,
                          early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                          verbose = XGB_PARAMS$verbose)

cli_alert_success("xG Model: best iter={xg_model$best_nrounds}, logloss={round(xg_model$best_logloss, 4)}")
saveRDS(xg_model, file.path(CACHE_DIR, "xg_model.rds"))

# 6. Train xPass Model ----

cli_h2("Step 5: Train xPass Model")

pass_features <- prepare_passes_for_xpass(spadl)

# Subsample if too large for CV (15M+ rows OOMs on xgb.cv)
MAX_XPASS_ROWS <- 2000000L
if (nrow(pass_features) > MAX_XPASS_ROWS) {
  set.seed(42)
  pass_features <- pass_features[sample(nrow(pass_features), MAX_XPASS_ROWS), ]
  cli_alert_info("Subsampled xPass training data to {format(MAX_XPASS_ROWS, big.mark=',')} rows")
}

xpass_model <- fit_xpass_model(pass_features,
                                nrounds = XGB_PARAMS$nrounds,
                                early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                                verbose = XGB_PARAMS$verbose)

cli_alert_success("xPass Model: best iter={xpass_model$best_nrounds}, logloss={round(xpass_model$best_logloss, 4)}")
saveRDS(xpass_model, file.path(CACHE_DIR, "xpass_model.rds"))

# 7. Train EPV Model (simple features with league) ----

cli_h2("Step 6: Train EPV Model (simple features)")

epv_features <- create_epv_features_simple(spadl_labeled)

# Subsample if too large for CV (21M+ rows can OOM on xgb.cv)
MAX_EPV_ROWS <- 5000000L
if (nrow(epv_features) > MAX_EPV_ROWS) {
  set.seed(42)
  sample_idx <- sample(nrow(epv_features), MAX_EPV_ROWS)
  epv_features <- epv_features[sample_idx, ]
  spadl_labeled <- spadl_labeled[sample_idx, ]
  cli_alert_info("Subsampled EPV training data to {format(MAX_EPV_ROWS, big.mark=',')} rows")
}

epv_model <- fit_epv_model(epv_features, spadl_labeled,
                            method = EPV_METHOD,
                            nrounds = XGB_PARAMS$nrounds,
                            early_stopping_rounds = XGB_PARAMS$early_stopping_rounds,
                            verbose = XGB_PARAMS$verbose)

# Tag model metadata for simple feature mode
epv_model$panna_metadata$feature_mode <- "simple"

metric_name <- if (EPV_METHOD == "goal") "mlogloss" else "rmse"
cli_alert_success("EPV Model: best iter={epv_model$best_nrounds}, {metric_name}={round(epv_model$best_metric, 4)}")
cli_alert_info("  Trained on {n_leagues_loaded} leagues, {format(epv_model$panna_metadata$n_actions, big.mark=',')} actions")
# Save with method suffix (epv_model_goal.rds or epv_model_xg.rds)
epv_method_file <- paste0("epv_model_", EPV_METHOD, ".rds")
saveRDS(epv_model, file.path(CACHE_DIR, epv_method_file))
saveRDS(epv_model, file.path(CACHE_DIR, "epv_model.rds"))  # also save as default
cli_alert_success("Saved: {epv_method_file} + epv_model.rds (default)")

# 8. Save to Pannadata ----

cli_h2("Step 7: Save Models to Pannadata")

# Save models to pannadata/opta/models/ for package use
pannadata_models <- file.path(opta_data_dir(), "models")
if (!dir.exists(pannadata_models)) dir.create(pannadata_models, recursive = TRUE)

saveRDS(xg_model, file.path(pannadata_models, "xg_model.rds"))
saveRDS(xpass_model, file.path(pannadata_models, "xpass_model.rds"))
saveRDS(epv_model, file.path(pannadata_models, epv_method_file))
saveRDS(epv_model, file.path(pannadata_models, "epv_model.rds"))

cli_alert_success("Models saved to {pannadata_models}/")
cli_alert_info("  - xg_model.rds")
cli_alert_info("  - xpass_model.rds")
cli_alert_info("  - {epv_method_file} + epv_model.rds (default)")

# 9. Summary ----

cli_h1("Training Complete!")

cli_alert_info("Models also cached to {CACHE_DIR}/")

cat("\nFeature Importance (EPV model, top 15):\n")
print(head(epv_model$importance, 15))

cat("\nLeague distribution in training data:\n")
print(table(spadl$league))
