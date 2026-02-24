# 03_calculate_player_xmetrics.R
# Calculate player-level xG, xA, and xPass metrics from Opta event data
#
# Applies pre-trained xG and xPass models to all Opta match events,
# derives xA from shot-pass linkage, and saves player-level aggregates.
#
# Run from panna directory: Rscript data-raw/epv/03_calculate_player_xmetrics.R
#
# Requires:
#   - pannadata/opta/models/xg_model.rds
#   - pannadata/opta/models/xpass_model.rds
#   - Opta match_events and lineups parquet files
#
# Outputs:
#   - pannadata/data/opta/xmetrics/{league}/{season}.parquet (player aggregates)

library(cli)
library(arrow)
devtools::load_all()

# 1. Configuration ----

LEAGUES <- c(
  # Big 5
  "ENG", "ESP", "GER", "ITA", "FRA",
  # Extended domestic
  "NED", "POR", "TUR", "ENG2", "SCO",
  # European comps
  "UCL", "UEL", "UECL",
  # International
  "WC", "EURO"
)

# Only process seasons from 2013-2014 onwards (2014+ data)
START_SEASON <- "2013-2014"

# PENALTY_XG is exported from panna::constants.R (loaded via devtools::load_all())

# Minimum minutes for output (0 = keep all)
MIN_MINUTES <- 0

# Cache directory for intermediate SPADL files
CACHE_DIR <- "data-raw/cache/epv/spadl"
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate Player xG/xA/xPass Metrics")

# 2. Load Models ----

cli_h2("Step 1: Load Trained Models")

xg_model <- load_xg_model()
xpass_model <- load_xpass_model()

cli_alert_success("Models loaded")

# 3. Discover Available Seasons ----

cli_h2("Step 2: Discover Available Seasons")

league_seasons <- list()
for (league in LEAGUES) {
  opta_league <- to_opta_league(league)
  seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
  if (length(seasons) > 0) {
    # Filter to START_SEASON onwards (works for both "2024-2025" and "2018 Russia" formats)
    if (exists("START_SEASON") && !is.null(START_SEASON)) {
      seasons <- seasons[seasons >= START_SEASON]
    }
    if (length(seasons) > 0) {
      league_seasons[[league]] <- seasons
      cli_alert_info("{league} ({opta_league}): {length(seasons)} seasons ({min(seasons)} to {max(seasons)})")
    }
  }
}

# 4. Process Each League-Season ----

cli_h2("Step 3: Process League-Seasons")

all_results <- list()
errors <- list()

for (league in names(league_seasons)) {
  opta_league <- to_opta_league(league)

  for (season in league_seasons[[league]]) {
    label <- paste(league, season)
    cli_alert_info("Processing {label}...")

    tryCatch({
      # 4a. Load match events
      events <- load_opta_match_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      if (nrow(events) < 100) {
        cli_alert_warning("  Skipping {label}: too few events ({nrow(events)})")
        next
      }

      # 4b. Convert to SPADL (cache to avoid re-conversion)
      spadl_cache <- file.path(CACHE_DIR, sprintf("spadl_%s_%s.rds", league, season))
      if (file.exists(spadl_cache)) {
        spadl <- readRDS(spadl_cache)
        cli_alert_info("  Loaded cached SPADL ({format(nrow(spadl), big.mark=',')} actions)")
      } else {
        spadl <- convert_opta_to_spadl(events)
        saveRDS(spadl, spadl_cache)
        cli_alert_info("  Converted to SPADL ({format(nrow(spadl), big.mark=',')} actions)")
      }

      # 4c. Detect penalties from raw events using match on player/minute
      pen_keys <- paste(
        events$match_id[events$type_id %in% c(13L, 14L, 15L, 16L) & grepl('"9"', events$qualifier_json)],
        events$player_id[events$type_id %in% c(13L, 14L, 15L, 16L) & grepl('"9"', events$qualifier_json)],
        events$minute[events$type_id %in% c(13L, 14L, 15L, 16L) & grepl('"9"', events$qualifier_json)],
        sep = "_"
      )
      spadl$is_penalty <- 0L
      spadl_shot_idx <- which(spadl$action_type == "shot")
      if (length(pen_keys) > 0 && length(spadl_shot_idx) > 0) {
        spadl_keys <- paste(
          spadl$match_id[spadl_shot_idx],
          spadl$player_id[spadl_shot_idx],
          floor(spadl$time_seconds[spadl_shot_idx] / 60),
          sep = "_"
        )
        spadl$is_penalty[spadl_shot_idx[spadl_keys %in% pen_keys]] <- 1L
      }

      # 4d. Add xG to shots
      spadl <- add_xg_to_spadl(spadl, xg_model)

      # Override penalty xG with fixed value
      penalty_idx <- spadl$action_type == "shot" & spadl$is_penalty == 1L
      if (sum(penalty_idx) > 0) {
        spadl$xg[penalty_idx] <- PENALTY_XG
        cli_alert_info("  Set {sum(penalty_idx)} penalties to xG={PENALTY_XG}")
      }

      # 4e. Add xPass to passes
      spadl <- add_xpass_to_spadl(spadl, xpass_model)

      # 4f. Derive xA
      spadl <- derive_xa(spadl)

      # 4g. Aggregate to player level
      player_metrics <- aggregate_player_xmetrics(spadl, lineups, min_minutes = MIN_MINUTES)
      player_metrics$league <- league
      player_metrics$season <- season

      # 4h. Save as parquet
      output_dir <- file.path(opta_data_dir(), "xmetrics", opta_league)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      output_file <- file.path(output_dir, paste0(season, ".parquet"))
      arrow::write_parquet(player_metrics, output_file)

      all_results[[label]] <- player_metrics
      cli_alert_success("  {label}: {nrow(player_metrics)} players saved")

    }, error = function(e) {
      errors[[label]] <<- e$message
      cli_alert_warning("  Skipping {label}: {e$message}")
    })
  }
}

# 5. Summary ----

cli_h2("Step 4: Summary")

if (length(all_results) > 0) {
  combined <- do.call(rbind, all_results)

  cli_alert_success("Processed {length(all_results)} league-seasons, {nrow(combined)} total player-seasons")

  # Show top xG for current season
  current <- combined[combined$season == max(combined$season) & combined$minutes >= 450, ]
  if (nrow(current) > 0) {
    cat("\nTop 15 Players by xG (latest season, 450+ mins):\n")
    top_xg <- head(current[order(-current$xg), ], 15)
    print(top_xg[, c("player_name", "team_name", "league", "minutes",
                      "goals", "xg", "goals_minus_xg", "xa")])

    cat("\nTop 15 Players by xA (latest season, 450+ mins):\n")
    top_xa <- head(current[order(-current$xa), ], 15)
    print(top_xa[, c("player_name", "team_name", "league", "minutes",
                      "assists", "xa", "key_passes")])

    cat("\nTop 15 xPass Overperformers (latest season, 450+ mins):\n")
    top_xpass <- head(current[order(-current$xpass_overperformance), ], 15)
    print(top_xpass[, c("player_name", "team_name", "league", "minutes",
                         "passes_completed", "sum_xpass", "xpass_overperformance")])
  }
}

if (length(errors) > 0) {
  cat("\nErrors:\n")
  for (nm in names(errors)) {
    cat(sprintf("  %s: %s\n", nm, errors[[nm]]))
  }
}

cli_h1("Complete!")
