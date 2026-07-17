# 06_calculate_wpa.R
# Score all SPADL actions with WP model and compute per-game WPA
#
# DELIBERATELY OPT-IN / MANUAL: feeds the multi-target RAPM cache path in
# 03_splint_creation.R (player_game_wpa), which never fires in CI -- the
# per-game value caches this step produces don't exist there (decision
# 2026-07-14).
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

# .build_match_results_from_events() (own-goal-aware match result/label
# builder, H2-OG-WP) now lives in R/wp_model.R as a shared internal helper --
# see roxygen there. Previously duplicated inline in this script and in
# 05_train_wp_model.R.

# 1. Configuration ----

LEAGUES <- if (exists("leagues", inherits = FALSE)) leagues else c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- if (exists("seasons", inherits = FALSE)) seasons else c("2023-2024")

CACHE_DIR <- "data-raw/cache/epv"
OUTPUT_DIR <- "data-raw/cache/epv/players"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate WPA (Win Probability Added)")

# 2. Load WP model ----

cli_h2("Step 1: Load WP Model")

wp_model <- load_wp_model()
cli_alert_success("WP model loaded ({length(wp_model$feature_names)} features)")

# EPV model for the WP `epv` feature — same resolution as 05_train_wp_model.R
# (train/serve parity): local cache first, pannamodels fallback inside
# load_epv_model(). NULL (with a warning) degrades to margin_poss-only
# features, which the post-overhaul WP model should never score with.
epv_model <- if (exists("epv_model_override", inherits = FALSE)) {
  cli_alert_info("Using injected EPV model override")
  epv_model_override
} else tryCatch(load_epv_model(path = "data-raw/cache/epv"), error = function(e) {
  cli_alert_warning("Could not load EPV model ({e$message}); WP features will lack the epv feature.")
  NULL
})

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

      # Train/serve parity with 05_train_wp_model.R (the retrained WP model's
      # feature recipe): attach per-action EPV so create_wp_features surfaces
      # the standalone `epv` feature (and a non-degenerate xmargin), and
      # re-derive red cards so red_card_diff is live. Without these the
      # post-overhaul model scores against degraded fallback features.
      if (!is.null(epv_model)) {
        spadl_chains <- calculate_action_epv(spadl_chains, features = NULL,
                                             epv_model, league = league)
      }
      spadl_chains <- add_red_card_to_chains(spadl_chains, events)

      # Build match results
      match_results <- .build_match_results_from_events(events, lineups)

      # Create WP features
      wp_features <- create_wp_features(spadl_chains, match_results)

      # Add WP and WPA
      spadl_wpa <- add_wp_vars(wp_features, wp_model)

      # Assign credit between actor and receiver
      spadl_wpa <- assign_wpa_credit(spadl_wpa)

      # Persist a slim per-action, home-perspective net WP-delta stream for
      # the multi-target RAPM per-splint attribution path
      # (FABLE-PRIOR-FIX-PLAN.md D2/Step 2).
      #
      # F2 (review): `wpa` (add_wp_vars(), R/wp_model.R:725-734) is the
      # acting-team-POV delta AFTER per-match mean-centering (`wpa := wpa -
      # mean(wpa), by = match_id` -- removes WP-model calibration bias).
      # Folding that CENTERED value into home-POV via a per-row sign flip
      # gives the match's centering constant `m` OPPOSITE signs on home vs
      # away rows: summing over a match, home-acting rows contribute `-m *
      # n_home` and away-acting rows contribute `+m * n_away`, a net bias of
      # `m * (n_away - n_home)` that does not cancel unless the match has an
      # exactly equal number of home- and away-acting rows. Per-splint sums
      # of the persisted stream would then carry a bias proportional to
      # home/away action-count imbalance -- not the exact zero-sum quantity
      # D2's single net WPA-RAPM column requires.
      #
      # Fix: recompute the UNCENTERED per-action delta directly from `wp`
      # (the raw, uncentered per-row win probability -- untouched by
      # centering) and `team_id`, reproducing add_wp_vars()'s exact
      # pre-centering formula (R/wp_model.R:710-731), including its
      # end-of-match fallback (last action: wp_next -> wp_label, or wp
      # itself if wp_label is absent; team_id_next -> team_id, so the
      # same-team branch applies). add_wp_vars() itself drops `wp_next` /
      # `team_id_next` before returning, so they are rebuilt here from `wp`
      # -- row order is unchanged since assign_wpa_credit() does not reorder,
      # so the by-match_id lead-shift reproduces the same next-action pairing
      # add_wp_vars() used. The sign flip on the UNCENTERED delta is then
      # exact: there is only one P(home wins) value at each instant, so a
      # change in it is mechanically the equal-and-opposite change in
      # P(away wins) -- independent of any centering constant.
      wpa_raw_dt <- data.table::copy(spadl_wpa)
      wpa_raw_dt[, wp_next_raw := data.table::shift(wp, type = "lead"), by = match_id]
      wpa_raw_dt[, team_id_next_raw := data.table::shift(team_id, type = "lead"), by = match_id]
      if ("wp_label" %in% names(wpa_raw_dt)) {
        wpa_raw_dt[is.na(wp_next_raw), wp_next_raw := wp_label]
      } else {
        wpa_raw_dt[is.na(wp_next_raw), wp_next_raw := wp]
      }
      wpa_raw_dt[is.na(team_id_next_raw), team_id_next_raw := team_id]
      wpa_raw_dt[, wpa_raw := data.table::fifelse(
        team_id_next_raw == team_id,
        wp_next_raw - wp,
        (1 - wp_next_raw) - wp
      )]

      wpa_home_dt <- wpa_raw_dt[, .(
        match_id, period_id, time_seconds,
        wp_delta_home = data.table::fifelse(is_home == 1L, wpa_raw, -wpa_raw)
      )]
      # C1: filter rows with NA in the delta or its inputs before writing --
      # mirrors the EPV stream's explicit !is.na() inclusion filters.
      wpa_home_dt <- wpa_home_dt[!is.na(wp_delta_home)]
      rm(wpa_raw_dt)

      wpa_home_file <- file.path(OUTPUT_DIR,
                                  sprintf("match_action_wpa_%s_%s.parquet", league, season))
      arrow::write_parquet(wpa_home_dt, wpa_home_file)
      cli_alert_info("  {nrow(wpa_home_dt)} home-perspective WPA rows -> {wpa_home_file}")
      rm(wpa_home_dt)

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
