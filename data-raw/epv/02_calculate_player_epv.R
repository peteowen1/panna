# 02_calculate_player_epv.R
# Calculate player-level EPV metrics using trained models
#
# Run from panna directory: Rscript data-raw/epv/02_calculate_player_epv.R
#
# Requires:
#   - data-raw/cache/epv/xg_model.rds
#   - data-raw/cache/epv/xpass_model.rds
#   - data-raw/cache/epv/epv_model.rds
#
# Outputs:
#   - data-raw/cache/epv/player_epv_{league}_{season}.rds

library(cli)
devtools::load_all()

# 1. Configuration ----

# Leagues and seasons to process
LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- c("2023-2024")

# Minimum minutes for player output
MIN_MINUTES <- 450

# Input/output directories
MODEL_DIR <- "data-raw/cache/epv"
OUTPUT_DIR <- "data-raw/cache/epv/players"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate Player EPV Values")

# 2. Load Models ----

cli_h2("Step 1: Load Trained Models")

xg_model <- readRDS(file.path(MODEL_DIR, "xg_model.rds"))
xpass_model <- readRDS(file.path(MODEL_DIR, "xpass_model.rds"))
epv_model <- readRDS(file.path(MODEL_DIR, "epv_model.rds"))

cli_alert_success("Models loaded (method: {epv_model$method})")

# 3. Calculate Player EPV ----

cli_h2("Step 2: Calculate Player EPV")

all_player_epv <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Processing {league} {season}...")

    tryCatch({
      # Load data
      events <- load_opta_match_events(league, season = season)
      lineups <- load_opta_lineups(league, season = season)

      # Convert to SPADL
      spadl <- convert_opta_to_spadl(events)

      # Create chains and labels
      spadl_chains <- create_possession_chains(spadl)
      chain_outcomes <- classify_chain_outcomes(spadl_chains)
      chain_outcomes <- add_next_chain_outcome(chain_outcomes)
      spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
      spadl_labeled <- create_next_goal_labels(spadl_labeled)

      if (epv_model$method == "xg") {
        spadl_labeled <- create_next_xg_labels(spadl_labeled)
      }

      # Create features and calculate EPV
      epv_features <- create_epv_features(spadl_labeled, n_prev = 3)
      spadl_epv <- calculate_action_epv(spadl_labeled, epv_features, epv_model)

      # Assign credit
      spadl_credit <- assign_epv_credit(spadl_epv, xpass_model)

      # Aggregate to player level
      player_epv <- aggregate_player_epv(spadl_credit, lineups, min_minutes = MIN_MINUTES)
      player_epv$league <- league
      player_epv$season <- season

      # Save
      output_file <- file.path(OUTPUT_DIR, sprintf("player_epv_%s_%s.rds", league, season))
      saveRDS(player_epv, output_file)

      all_player_epv[[paste(league, season)]] <- player_epv
      cli_alert_success("  {nrow(player_epv)} players saved")

    }, error = function(e) {
      cli_alert_warning("  Skipping: {e$message}")
    })
  }
}

# 4. Combine Results ----

cli_h2("Step 3: Combine Results")

combined <- do.call(rbind, all_player_epv)
saveRDS(combined, file.path(OUTPUT_DIR, "player_epv_all.rds"))

cli_alert_success("Combined: {nrow(combined)} player-seasons")

# 5. Summary ----

cli_h1("Complete!")

cat("\nTop 20 Players by EPV per 90:\n")
top_p90 <- head(combined[order(-combined$epv_total_p90), ], 20)
print(top_p90[, c("player_name", "league", "season", "total_minutes",
                   "epv_total_p90", "epv_passing", "epv_shooting", "epv_defending")])
