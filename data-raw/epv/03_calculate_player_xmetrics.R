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

# Config override pattern: debug/test scripts can set LEAGUES before sourcing
# (e.g. to score only newly-added leagues against existing SPADL caches).
# Canonical rating/display set (PANNA_RATING_LEAGUES) + bridge comps for
# cross-league offset connectivity. Shared with skills/PSR, RAPM and 10b.
if (!exists("LEAGUES", inherits = FALSE)) {
  LEAGUES <- c(PANNA_RATING_LEAGUES, PANNA_BRIDGE_LEAGUES)
}

# Only process seasons from 2013-2014 onwards (2014+ data)
START_SEASON <- "2013-2014"

# PENALTY_XG is exported from panna::constants.R (loaded via devtools::load_all())

# Minimum minutes for output (0 = keep all)
MIN_MINUTES <- 0

# SPADL_CACHE_DIR is from panna::constants.R (loaded via devtools::load_all())
CACHE_DIR <- SPADL_CACHE_DIR
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate Player xG/xA/xPass Metrics")

# 2. Load Models ----

cli_h2("Step 1: Load Trained Models")

xg_model <- load_xg_model()
xpass_model <- load_xpass_model()
xgot_model <- load_xgot_model()   # NULL until goalmouth-enabled model ships
# xDuel: expected duel/aerial/tackle win prob → "duels won above expected" features
# (replace the *_success ratios in PSR/PSV). NULL until 01b_train_duel_model.R run.
duel_model <- tryCatch(load_duel_model(), error = function(e) {
  msg <- conditionMessage(e)
  # The xDuel WOE features REPLACED the *_success ratios in PSR/PSV, so a missing
  # model silently retrains the displayed ratings without them. Distinguish two
  # cases: a fresh bootstrap (01b not yet run) is legitimate; a production run
  # losing the model (pannamodels publish lag) is not.
  if (nzchar(Sys.getenv("PANNA_DUEL_REQUIRED"))) {
    cli::cli_abort(c(
      "xDuel model required (PANNA_DUEL_REQUIRED set) but not loadable: {msg}",
      "i" = "Publish duel_model.rds to pannamodels, or unset PANNA_DUEL_REQUIRED to bootstrap without duel features."
    ))
  }
  # Bootstrap/dev: loud, never silent — surface as a CI warning too.
  cli::cli_alert_danger("xDuel model UNAVAILABLE — PSR/PSV will train WITHOUT duel-above-expected features: {msg}")
  cat("::warning::xDuel model unavailable — the 5 duel WOE features will be ABSENT from this xMetrics run\n")
  NULL
})

cli_alert_success("Models loaded{if (is.null(xgot_model)) ' (xGOT unavailable — skipping post-shot xG)' else ''}{if (is.null(duel_model)) ' (xDuel unavailable)' else ''}")

# 3. Discover Available Seasons ----

cli_h2("Step 2: Discover Available Seasons")

league_seasons <- list()
for (league in LEAGUES) {
  opta_league <- to_opta_league(league)
  seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
  if (length(seasons) > 0) {
    # Filter to START_SEASON onwards by END YEAR — never a lexical string
    # compare: calendar/tournament labels ("2026", "2018 Russia") sort wrong
    # against "2013-2014" and silently drop valid seasons (panna/CLAUDE.md
    # "Season subsetting"; 01_train uses the same extract_season_end_year path).
    if (exists("START_SEASON") && !is.null(START_SEASON)) {
      floor_yr <- extract_season_end_year(START_SEASON)
      ey <- vapply(seasons, extract_season_end_year, numeric(1))
      seasons <- seasons[!is.na(ey) & ey >= floor_yr]
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

      # 4c. Penalty detection (H2-PEN). SPADL already carries a correctly
      # parsed is_penalty (qualifier 9, anchored regex `[{,]"9":` in
      # parse_opta_qualifiers()/spadl_conversion.R) -- use it rather than
      # re-deriving and overwriting it. The unanchored `'"9"'` match used here
      # previously false-positived on qualifier VALUES (e.g. `"55":"9"`), not
      # just qualifier keys.
      if ("is_penalty" %in% names(spadl)) {
        spadl$is_penalty <- as.integer(spadl$is_penalty)
      } else {
        # Fallback for stale cached SPADL RDS files predating the is_penalty
        # column: re-derive from raw events using the same anchored pattern.
        cli_alert_warning("  spadl$is_penalty missing (stale cache?) - deriving from raw events")
        pen_evt_idx <- events$type_id %in% c(13L, 14L, 15L, 16L) &
          grepl('[{,]"9":', events$qualifier_json)
        pen_keys <- paste(
          events$match_id[pen_evt_idx],
          events$player_id[pen_evt_idx],
          events$minute[pen_evt_idx],
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
      }

      # 4c-bis. Shot events, loaded ONCE and used by both xG and xGOT. This load
      # used to sit inside the xGOT block below; xG needs it too, because SPADL
      # cannot supply body part or situation (see add_xg_to_spadl()).
      shot_ev <- tryCatch(
        load_opta_shot_events(league, season = season, source = "local"),
        error = function(e) {
          cli_alert_warning("  shot_events failed to load for {label}: {e$message}")
          NULL
        }
      )

      # 4d. Add xG to shots. shot_lookup carries body_part + situation: without
      # it every header scores as a foot shot (+6.3% on total xG) and every set
      # piece as open play (-4.7%).
      xg_lk_cols <- intersect(c("match_id", "event_id", "body_part", "situation"),
                              names(shot_ev))
      xg_lookup <- if (!is.null(shot_ev) &&
                       all(c("match_id", "event_id") %in% xg_lk_cols)) {
        as.data.frame(shot_ev)[, xg_lk_cols]
      } else {
        NULL
      }
      spadl <- add_xg_to_spadl(spadl, xg_model, season = season,
                               shot_lookup = xg_lookup)

      # Override penalty xG with fixed value
      penalty_idx <- spadl$action_type == "shot" & spadl$is_penalty == 1L
      if (sum(penalty_idx) > 0) {
        spadl$xg[penalty_idx] <- PENALTY_XG
        cli_alert_info("  Set {sum(penalty_idx)} penalties to xG={PENALTY_XG}")
      }

      # 4d-bis. Add xGOT (post-shot xG) to on-target shots. Goal-mouth coords
      # come from shot_events (backfilled, locale-safe); joined onto SPADL by
      # original_event_id inside add_xgot_to_spadl. Skips cleanly if the model
      # or the goalmouth columns aren't available yet.
      if (!is.null(xgot_model)) {
        req_cols <- c("match_id", "event_id", "type_id", "goalmouth_y", "goalmouth_z")
        if (!is.null(shot_ev) && all(req_cols %in% names(shot_ev))) {
          # Pass `situation` too (when present) — add_xgot_to_spadl needs it to
          # match training features and avoid set-piece train/serve skew.
          lk_cols <- c(req_cols, intersect("situation", names(shot_ev)))
          spadl <- add_xgot_to_spadl(spadl, xgot_model,
                                     as.data.frame(shot_ev)[, lk_cols])
        } else if (!is.null(shot_ev)) {
          cli_alert_warning("  Skipping xGOT for {label}: shot_events lack goalmouth coords (run backfill + re-upload)")
        }
      }

      # 4e. Add xPass to passes
      spadl <- add_xpass_to_spadl(spadl, xpass_model)

      # 4f. Derive xA
      spadl <- derive_xa(spadl)

      # 4g. Add possession chain data
      spadl <- create_possession_chains(spadl)
      chain_outcomes <- classify_chain_outcomes(spadl)
      spadl <- label_actions_with_outcomes(spadl, chain_outcomes)

      # 4i. Aggregate to player level
      player_metrics <- aggregate_player_xmetrics(spadl, lineups, min_minutes = MIN_MINUTES)
      player_metrics$league <- league
      player_metrics$season <- season

      # 4j. Save season-level as parquet (existing consumers: ratings/blog/compare)
      output_dir <- file.path(opta_data_dir(), "xmetrics", opta_league)
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
      output_file <- file.path(output_dir, paste0(season, ".parquet"))
      arrow::write_parquet(player_metrics, output_file)

      # 4j-ii. Per-match xmetrics (by_match) — one row per player-match — for the
      # skills-model xG join (puts xg/npxg + finishing over-performance into
      # match_stats). Separate artifact; leaves the season-level product above
      # untouched.
      player_metrics_bymatch <- aggregate_player_xmetrics(
        spadl, lineups, min_minutes = MIN_MINUTES, by_match = TRUE)
      player_metrics_bymatch$league <- league
      player_metrics_bymatch$season <- season

      # 4j-iii. Duels/aerials/tackles won above expected (xDuel). Computed from
      # RAW events (both contest participants survive, unlike post-merge SPADL),
      # joined per (player_id, match_id), divided by that row's minutes. These
      # per-90 above-expected counts replace the *_success ratios in PSR/PSV.
      if (!is.null(duel_model)) {
        # Five contests (R/duel_model.R): aerial_win, aerial_poss, takeon (attacker),
        # tackle_poss, containment (defender). Each -> a per-90 above-expected count.
        .duel_woe_cols <- c("aerial_woe", "aerial_poss_woe", "takeon_woe",
                            "tackle_poss_woe", "containment_woe")
        .attach_duel_woe <- function(pm, by_match) {
          woe <- tryCatch(compute_duel_woe(events, duel_model, by_match = by_match),
                          error = function(e) {
                            # A real compute error (the model loaded, this league threw) is
                            # concerning — loud + CI-visible. A benign empty league (no error,
                            # nrow 0) falls through silently below, which is correct.
                            cli_alert_danger("  duel WOE FAILED for {label} — features zero-filled: {e$message}")
                            cat(sprintf("::warning::duel WOE compute failed for %s\n", label))
                            NULL
                          })
          if (is.null(woe) || nrow(woe) == 0) {
            for (p90 in paste0(.duel_woe_cols, "_per90")) pm[[p90]] <- 0
            return(pm)
          }
          key <- intersect(c("player_id", "team_id", "match_id"), names(woe))
          have <- intersect(.duel_woe_cols, names(woe))
          dt <- data.table::as.data.table(pm)
          dt <- woe[, c(key, have), with = FALSE][dt, on = key]
          for (wc in .duel_woe_cols) {
            p90 <- paste0(wc, "_per90")
            base <- if (wc %in% names(dt)) dt[[wc]] else rep(NA_real_, nrow(dt))
            val <- data.table::fifelse(!is.na(base) & dt$minutes > 0,
                                       round(base / dt$minutes * 90, 3), 0)
            val[is.na(val)] <- 0
            data.table::set(dt, j = p90, value = val)
            if (wc %in% names(dt)) data.table::set(dt, which(is.na(dt[[wc]])), wc, 0)
          }
          as.data.frame(dt)
        }
        player_metrics_bymatch <- .attach_duel_woe(player_metrics_bymatch, by_match = TRUE)
        player_metrics      <- .attach_duel_woe(player_metrics,      by_match = FALSE)
        arrow::write_parquet(player_metrics, output_file)  # rewrite season-level with WOE
      }

      bymatch_dir <- file.path(opta_data_dir(), "xmetrics_bymatch", opta_league)
      dir.create(bymatch_dir, recursive = TRUE, showWarnings = FALSE)
      arrow::write_parquet(player_metrics_bymatch,
                           file.path(bymatch_dir, paste0(season, ".parquet")))

      all_results[[label]] <- player_metrics
      cli_alert_success("  {label}: {nrow(player_metrics)} players ({nrow(player_metrics_bymatch)} player-matches) saved")

    }, error = function(e) {
      errors[[label]] <<- e$message
      cli_alert_warning("  Skipping {label}: {e$message}")
    })
  }
}

# 5. Summary ----

cli_h2("Step 4: Summary")

if (length(all_results) > 0) {
  # rbindlist(fill=TRUE): leagues differ in columns now (gsaa / placement_added
  # are only present where keepers/xGOT exist), so do.call(rbind) errors on
  # mismatched ncol. fill=TRUE unions columns; as.data.frame keeps the
  # downstream data.frame-style indexing below working.
  combined <- as.data.frame(data.table::rbindlist(all_results, fill = TRUE))

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
