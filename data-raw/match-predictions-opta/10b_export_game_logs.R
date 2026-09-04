# 10b_export_game_logs.R
# Export per-match player value metrics (EPV + WPA + PSV) for the blog
#
# Produces game_logs_<season>.parquet — one parquet per season — uploaded to
# the blog-latest release on peteowen1/pannadata. A mirror copy named
# game_logs.parquet (= most recent season) is also produced/uploaded so the
# blog workflow's download-by-name step keeps working unchanged.
#
# Default: current season only (weekly predictions pipeline). For historical
# backfill, set `game_log_seasons <- c("2015-2016", ..., "2025-2026")` before
# sourcing — see 10b_backfill_game_logs.R.
#
# Pipeline per season:
#   EPV : SPADL → EPV model → credit assignment → aggregate_player_game_epv()
#   WPA : SPADL → WP model  → credit assignment → aggregate_player_game_wpa()
#   PSV : match stats → compute_player_psv()
#   Merged via build_player_game_ratings() → piero_value (50/50 EPV + PSV)

# 1. Configuration ----

# When sourced standalone (outside run_predictions_opta.R) pipeline_utils.R
# isn't loaded yet — source it here so resolve_blog_leagues() is available
# regardless of entry point (direct Rscript, 10b_backfill_game_logs.R, or the
# full pipeline).
if (!exists("resolve_blog_leagues", mode = "function")) {
  source(file.path("data-raw", "pipeline_utils.R"))
}

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

# panna#126: GHA has no local xmetrics_bymatch/ tree, so the two
# enrich_match_stats_with_xmetrics() calls below must read the consolidated
# opta_xmetrics_bymatch.parquet from opta-latest instead — else game logs are
# scored xG-blind (finishing over-performance / gsaa silently absent). Env var
# (predictions-pipeline.yml sets XMETRICS_SOURCE=remote), not an R flag —
# local pipeline runs default to the pipeline-generated local files.
xm_source <- if (identical(Sys.getenv("XMETRICS_SOURCE"), "remote")) "remote" else "local"

# Leagues to include in the per-match blog export. Three categories:
#   (1) domestic       — iterate with the export season ("YYYY-YYYY")
#   (2) continental    — UCL / UEL / UECL use "YYYY-YYYY" too
#   (3) intl_tournament — WC / EURO use "YYYY Country"; map a summer
#                         tournament to the domestic season ending that year.
# Groups come from resolve_blog_leagues() (pipeline_utils.R), backed by the
# shared canonical constant (constants.R: PANNA_LEAGUE_GROUPS), so step 03 /
# skills / RAPM / 10b can't drift. Grouping drives season-label resolution:
# domestic = "YYYY-YYYY"; calendar = "YYYY"; intl = "YYYY Country".
.blog_league_groups <- resolve_blog_leagues()
domestic_leagues    <- .blog_league_groups$domestic_leagues
calendar_leagues    <- .blog_league_groups$calendar_leagues    # calendar-year season labels
continental_cups    <- .blog_league_groups$continental_cups
intl_tournaments    <- .blog_league_groups$intl_tournaments
# Leagues whose season label is resolved by year prefix rather than passed through
season_label_leagues <- .blog_league_groups$season_label_leagues
# Override guard: backfill runs can process a league subset. CAUTION — the
# per-season output parquet contains ONLY the processed leagues, so a subset
# run must set upload_game_logs <- FALSE and merge into the existing
# game_logs_<season>.parquet files instead of clobbering them.
## sign convention aside, these config-flag guards use envir=globalenv()
## (not bare inherits=FALSE): the pipeline driver sources this via
## source(local=TRUE), so a driver-set global is invisible to a plain
## inherits=FALSE lookup -- same bug class as the upload_psr incident
## (2026-09-04) and the career_panna silent-skip (2026-07-17).
if (!exists("blog_leagues", envir = globalenv(), inherits = FALSE)) {
  blog_leagues <- .blog_league_groups$blog_leagues
}

# Seasons to export. Vector (new) or scalar `game_log_season` (back-compat).
if (!exists("game_log_seasons", envir = globalenv(), inherits = FALSE)) {
  if (exists("game_log_season", envir = globalenv(), inherits = FALSE)) {
    game_log_seasons <- game_log_season
  } else {
    game_log_seasons <- "2025-2026"
  }
}
game_log_seasons <- as.character(game_log_seasons)

# The "current" season (most recent in the vector) is mirrored to
# game_logs.parquet so the blog workflow's name-pinned download still works.
current_season_alias <- sort(game_log_seasons, decreasing = TRUE)[1]

# Within-position normalization (per-role skill means) for the displayed PSV —
# values a player vs their role (BPM-style). Set position_normalize <- FALSE to
# disable. The match-stats path supplies `position`, mapped to the broad bucket
# by .player_role; the RAPM psvf90 target is untouched.
.psv_position_means <- if (exists("position_normalize") && !isTRUE(position_normalize)) {
  NULL
} else load_position_role_means()

# Reliability-lambda shrinkage is RETIRED from the display path (2026-07-20
# audit, #158: lambda estimates skill, but per-game PSV is a production
# metric — see LIVE-PSV-UNBLOCK plan doc). Default OFF; set
# psv_reliability_pricing <- TRUE only for skill-side experiments.
.psv_reliability <- if (exists("psv_reliability_pricing") && isTRUE(psv_reliability_pricing)) {
  load_psv_match_reliability()
} else NULL

# Minutes-weighted round centring (LIVE-PSV-UNBLOCK 2026-07-20, task 2): the
# default plain row-mean centering in calculate_psv() doesn't zero-sum once
# scale_to_minutes multiplies by minutes/90 (a round with lopsided cameo
# minutes drifts off 0). "minutes" weights the round mean by minutes/90 so
# the SUMMED scaled psv is exactly 0 within (season, round) — see
# calculate_psv(center_weights=)'s docs for the algebra. Set
# psv_center_weights <- "none" before sourcing to fall back to the legacy
# plain-mean centering.
# envir=globalenv() (not bare inherits=FALSE): the pipeline driver sources this
# via source(local=TRUE), so a driver-set global is invisible to a plain
# inherits=FALSE lookup — the silent-skip bug from career_panna, 2026-07-17.
.psv_center_weights <- if (exists("psv_center_weights", envir = globalenv(),
                                  inherits = FALSE) &&
                           identical(get("psv_center_weights", envir = globalenv()),
                                     "none")) {
  "none"
} else "minutes"

# Upload toggle — set FALSE during local dev to skip the GH release push.
if (!exists("upload_game_logs")) upload_game_logs <- TRUE

# Build toggle — set FALSE to skip the per-season processing loop (e.g. when
# parquets were already built in parallel workers and this invocation only
# needs to do the alias + upload step in a single main-process pass).
if (!exists("build_game_logs", envir = globalenv(), inherits = FALSE)) build_game_logs <- TRUE

# Subset-league backfill: MERGE the processed leagues into each existing
# game_logs_<season>.parquet instead of clobbering it. Set TRUE when running a
# league SUBSET (e.g. adding AUS/BEL/BRA/CAFCL) so the other leagues' rows for
# that season are preserved. Idempotent (drops + re-appends the rebuilt leagues).
if (!exists("merge_subset_leagues", envir = globalenv(), inherits = FALSE)) merge_subset_leagues <- FALSE

# Alias toggle — mirror the most-recent processed season to game_logs.parquet
# (the blog chain builder's name-pinned download). Default TRUE for weekly
# runs, but set FALSE when back-filling a NON-current historical subset so
# the alias keeps pointing at the real current season.
if (!exists("mirror_alias", envir = globalenv(), inherits = FALSE)) mirror_alias <- TRUE

message(sprintf("\n=== Building Game Logs: %d season(s) ===", length(game_log_seasons)))
message(sprintf("  Seasons: %s", paste(game_log_seasons, collapse = ", ")))
message(sprintf("  Alias (game_logs.parquet) → %s", current_season_alias))

# 2. Load shared resources (once across all seasons) ----

# Model overrides (set by a driver to score with candidate models, e.g. for the
# worker gate-fixture regen). EPV override is required when the package's
# EPV_SIMPLE_FEATURE_COLS contract has changed (14-feature clean model).
epv_model   <- if (exists("epv_model_override")) epv_model_override else load_epv_model()
xpass_model <- load_xpass_model()
wp_model    <- if (exists("wp_model_override")) wp_model_override else load_wp_model()

match_stats_path <- file.path("data-raw", "cache-skills", "01_match_stats.rds")
has_match_stats  <- file.exists(match_stats_path)
if (has_match_stats) {
  all_match_stats <- readRDS(match_stats_path)
  message(sprintf("  Loaded match stats: %d player-games", nrow(all_match_stats)))
} else {
  message("  Note: No match stats cache — PSV will be unavailable")
}

# Seasonal SPM (used for spm_overall enrichment, filtered per-season in loop)
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path   <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")
if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  seasonal_results <- readRDS(skill_ratings_path)
} else if (file.exists(raw_ratings_path)) {
  seasonal_results <- readRDS(raw_ratings_path)
} else {
  seasonal_results <- NULL
}
if (!is.null(seasonal_results) && !is.null(seasonal_results$seasonal_spm)) {
  all_spm_dt <- data.table::as.data.table(seasonal_results$seasonal_spm)
  message(sprintf("  Seasonal SPM: %d player-seasons across %d years",
                  nrow(all_spm_dt), length(unique(all_spm_dt$season_end_year))))
} else {
  all_spm_dt <- NULL
  message("  Note: No seasonal SPM — spm_overall column will be NA")
}

# 3. Helpers ----

.build_match_results <- function(events, lineups) {
  dt_lineups <- data.table::as.data.table(lineups)
  match_teams <- dt_lineups[, .(
    home_team_id = team_id[tolower(team_position) == "home"][1],
    away_team_id = team_id[tolower(team_position) == "away"][1]
  ), by = match_id]
  dt_events <- data.table::as.data.table(events)
  # Exclude penalty-shootout goals (period_id >= 5) — a pens match is a draw in
  # open play, so shootout conversions must not inflate the match score.
  if ("period_id" %in% names(dt_events)) {
    dt_events <- dt_events[!is_shootout_period(period_id)]
  }
  goals <- dt_events[type_id == 16L]
  goal_counts <- goals[, .N, by = .(match_id, team_id)]
  match_teams[goal_counts, home_goals := i.N, on = .(match_id, home_team_id = team_id)]
  match_teams[goal_counts, away_goals := i.N, on = .(match_id, away_team_id = team_id)]
  match_teams[is.na(home_goals), home_goals := 0L]
  match_teams[is.na(away_goals), away_goals := 0L]
  as.data.frame(match_teams)
}

# season_str = "2025-2026"; returns integer season_end_year (2026) or NA
.season_end_year <- function(season_str) {
  m <- regmatches(season_str, regexpr("\\d{4}$", season_str))
  if (length(m) == 0) NA_integer_ else as.integer(m)
}

# League-season resolution lives in panna::resolve_league_season() so 10b
# and 10c_export_equity can share it. intl_tournaments list above controls
# which leagues go through the tournament-year remapping.

# Typed "skip this league" signal. Using a condition class (caught by
# `tryCatch(..., panna_skip_league = handler)`) instead of a magic message
# string — class dispatch is robust against message drift and clearly
# distinguishes intentional skips from real errors in the outer handler.
skip_league_cond <- function(reason) {
  structure(
    class = c("panna_skip_league", "error", "condition"),
    list(message = sprintf("skip_league: %s", reason),
         reason  = reason)
  )
}

# Minimum columns build_player_game_ratings() must emit before a league's
# frame can be added to the season output. Catches drift in the builder's
# schema before we ship a malformed parquet to the blog.
.required_game_log_cols <- c(
  "player_id", "team_id", "match_id",
  "minutes_played", "piero_value", "piero_value_p90"
)

validate_game_log_schema <- function(dt, league, season) {
  missing <- setdiff(.required_game_log_cols, names(dt))
  if (length(missing) > 0L) {
    stop(sprintf(
      "[%s %s] game_ratings missing required columns: %s",
      league, season, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(dt)
}

# Pull fresh per-league events (+ consolidated fixtures) from opta-latest into
# the LOCAL data dir. The pre-flight coverage guard below runs against
# source="local", but on a dev box that local copy can lag the daily cloud
# scrape by days — tripping the abort even though opta-latest (and the
# pipeline's own remote event loads on line ~210) are complete. Refreshing the
# short leagues closes that gap so the re-check sees current data; a genuine
# abort is then reserved for the case that matters — the CLOUD itself is short.
.refresh_local_events <- function(leagues, repo = "peteowen1/pannadata",
                                   tag = "opta-latest") {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    warning("piggyback not installed — cannot auto-refresh local events; ",
            "falling through to the guard with existing local files.",
            call. = FALSE)
    return(invisible(character(0)))
  }
  events_dir <- file.path(opta_data_dir(), "events_consolidated")
  dir.create(events_dir, showWarnings = FALSE, recursive = TRUE)

  refreshed <- character(0)
  for (lg in unique(leagues)) {
    file_name <- sprintf("events_%s.parquet", to_opta_league(lg))
    ok <- tryCatch({
      piggyback::pb_download(file = file_name, repo = repo, tag = tag,
                             dest = events_dir, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      warning(sprintf("Auto-refresh: failed to pull %s — %s",
                      file_name, e$message), call. = FALSE)
      FALSE
    })
    if (ok) {
      message(sprintf("    Refreshed %s", file_name))
      refreshed <- c(refreshed, lg)
    }
  }

  # Refresh the consolidated singles the coverage guard's denominator reads:
  #   opta_fixtures.parquet      — played-match context
  #   opta_player_stats.parquet  — the "should have events" universe
  #   event_less_match_ids.parquet — matches Opta has no event feed for
  # so the local hard re-check compares fresh events against a fresh, coherent
  # denominator (a stale player_stats would mis-size the expected set).
  for (single in c("opta_fixtures.parquet", "opta_player_stats.parquet",
                   "event_less_match_ids.parquet")) {
    tryCatch({
      piggyback::pb_download(file = single, repo = repo, tag = tag,
                             dest = opta_data_dir(), overwrite = TRUE)
      message(sprintf("    Refreshed %s", single))
    }, error = function(e) {
      # The registry won't exist until the first rebuild has run — a miss
      # there is benign (coverage falls back to the stricter denominator).
      warning(sprintf("Auto-refresh: failed to pull %s — %s",
                      single, e$message), call. = FALSE)
    })
  }

  invisible(refreshed)
}

# Process a single season: returns path to written parquet, or NULL on failure.
.process_season <- function(season) {
  message(sprintf("\n########## SEASON %s ##########", season))
  all_game_logs <- list()

  # --- Pre-flight: events_consolidated coverage check (panna#NN) -----------
  # The EPV pipeline reads from events_consolidated/events_<comp>.parquet.
  # If pannadata's scraper produced a short file (observed 2026-05-29 for
  # Championship: 265 / 557 matches), step 10b would silently produce
  # game_logs that miss matches and the blog Value tab gets stuck. Refuse
  # to continue when ANY blog league is missing more than abort_threshold
  # matches vs its played fixtures. warn_threshold is the noisy-but-OK band.
  ls_pairs <- lapply(blog_leagues, function(lg) {
    lg_season <- resolve_league_season(lg, season,
                                         tournament_leagues = season_label_leagues)
    if (is.null(lg_season)) return(NULL)
    list(league = lg, season = lg_season)
  })
  ls_pairs <- Filter(Negate(is.null), ls_pairs)
  if (length(ls_pairs) > 0L) {
    # Read from globalenv explicitly: this guard runs INSIDE .process_season, so
    # `inherits = FALSE` against the local frame can never see a driver's global
    # setting (the config is staged in globalenv by the backfill/driver scripts).
    abort_thresh <- if (exists("events_coverage_abort_threshold",
                               envir = globalenv(), inherits = FALSE)) {
      get("events_coverage_abort_threshold", envir = globalenv())
    } else {
      20L  # default: tolerate 20 missing per league, abort beyond
    }

    # Auto-refresh stale local events before the hard guard. Default ON: the
    # cloud (opta-latest) is the source of truth and this step's own event
    # loads are remote, so a lagging local copy shouldn't block the run. Set
    # `auto_refresh_stale_events <- FALSE` before sourcing to keep the legacy
    # "abort on stale local" behaviour (e.g. a deliberately offline run).
    auto_refresh <- if (exists("auto_refresh_stale_events",
                               envir = globalenv(), inherits = FALSE)) {
      isTRUE(get("auto_refresh_stale_events", envir = globalenv()))
    } else {
      TRUE
    }

    if (isTRUE(auto_refresh)) {
      # Warn-only probe (abort_threshold = Inf never aborts): returns the
      # per-league reports so we can see which LOCAL files are short. A
      # "source_missing" league (0 local events, lazy-loaded remotely
      # downstream) is intentionally NOT refreshed — only real partial gaps.
      probe <- assert_events_coverage(ls_pairs, warn_threshold = 5L,
                                       abort_threshold = Inf, source = "local")
      short_leagues <- unique(vapply(
        Filter(function(r) identical(r$status, "partial_gap"), probe$reports),
        function(r) r$league, character(1)
      ))
      if (length(short_leagues) > 0L) {
        message(sprintf(
          "\n  Auto-refresh: %d league(s) have stale/short local events — pulling fresh from opta-latest:\n    %s",
          length(short_leagues), paste(short_leagues, collapse = ", ")
        ))
        .refresh_local_events(short_leagues)
      }
    }

    # Hard guard. After an auto-refresh this aborts ONLY if the cloud itself is
    # genuinely short (the case worth stopping for); on a normal dev box the
    # refresh closes the gap and this passes.
    assert_events_coverage(ls_pairs,
                            warn_threshold = 5L,
                            abort_threshold = abort_thresh,
                            source = "local")
  }

  for (league in blog_leagues) {
    tryCatch({
      league_season <- resolve_league_season(league, season,
                                               tournament_leagues = season_label_leagues)
      if (is.null(league_season)) {
        message(sprintf("\n  Skipping %s %s — no tournament this year", league, season))
        stop(skip_league_cond("no tournament this year"))
      }
      label <- if (identical(league_season, season)) league else
               sprintf("%s (%s)", league, league_season)
      message(sprintf("\n  Processing %s %s...", label, season))

      events  <- load_opta_match_events(league, season = league_season)
      lineups <- load_opta_lineups(league, season = league_season)
      # Optional: restrict to specific match_ids (worker gate-fixture regen — only
      # the 2 reference matches need rebuilding, not the whole league-season).
      if (exists("target_match_ids")) {
        events  <- events[events$match_id %in% target_match_ids, ]
        lineups <- lineups[lineups$match_id %in% target_match_ids, ]
        if (nrow(events) == 0) stop(skip_league_cond("no target matches in this league-season"))
      }

      if (is.null(events) || nrow(events) < 100) {
        message(sprintf("    Skipping %s — insufficient data", league))
        # Signal a skip via a typed condition caught by the outer handler.
        # `return()` here would exit .process_season, aborting remaining leagues.
        stop(skip_league_cond("insufficient data"))
      }

      n_matches <- length(unique(events$match_id))
      message(sprintf("    %d matches, %d events", n_matches, nrow(events)))

      # --- SPADL conversion (shared by EPV and WPA). Cached on disk per
      # league-season since it's the single biggest cost in this pipeline
      # and deterministic given raw events. Use `league_season` in the key
      # so tournament years (WC 2014 vs 2018) get separate cache entries.
      spadl          <- get_or_build_spadl(events, league, league_season)
      spadl_chains   <- create_possession_chains(spadl)
      chain_outcomes <- classify_chain_outcomes(spadl_chains)
      chain_outcomes <- add_next_chain_outcome(chain_outcomes)
      spadl_labeled  <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
      spadl_labeled  <- create_next_goal_labels(spadl_labeled)

      # --- EPV path ---
      # league_season, not season: a calendar-year league's label ("2026")
      # differs from the tournament-year one and the xG season term reads the
      # END YEAR off whichever label it is handed. shot_lookup supplies
      # body_part + situation, which SPADL cannot (its bodypart says "foot" for
      # every shot) - without it the xG behind these game logs is ~6% skewed.
      spadl_epv        <- calculate_action_epv(spadl_labeled, features = NULL, epv_model,
                                               league = league, season = league_season,
                                               shot_lookup = .epv_shot_lookup(league, league_season))
      spadl_credit     <- assign_epv_credit(spadl_epv, xpass_model)
      player_game_epv  <- aggregate_player_game_epv(spadl_credit, lineups)

      # EPV adjustments (position centering + opponent)
      tryCatch({
        dt_lu <- data.table::as.data.table(lineups)
        if ("match_date" %in% names(dt_lu)) {
          match_dates     <- dt_lu[, .(match_date = match_date[1]), by = match_id]
          player_game_epv <- merge(player_game_epv, match_dates, by = "match_id", all.x = TRUE)
        }
        if ("position" %in% names(player_game_epv)) {
          player_game_epv <- adjust_epv_for_position(
            player_game_epv,
            credit_cols = c("epv_total", "epv_offensive", "epv_defensive")
          )
        }
        if (all(c("match_date", "team_id", "minutes_played") %in% names(player_game_epv))) {
          player_game_epv <- adjust_epv_for_opponents(
            player_game_epv, credit_col = "epv_total"
          )
        }
        # Fold opponent adjustment into epv_total_adj so "adj" means
        # position + opponent everywhere downstream. Offensive/defensive stay
        # position-only — opp adj is computed at team-match level and not split
        # across attack/defense. Expose the opp component separately as opp_adj.
        player_game_epv <- data.table::as.data.table(player_game_epv)
        if (all(c("epv_total_adj", "player_opp_adj") %in% names(player_game_epv))) {
          player_game_epv[, epv_total_adj := epv_total_adj + player_opp_adj]
          data.table::setnames(player_game_epv, "player_opp_adj", "opp_adj")
        }
      }, error = function(e) {
        warning(sprintf("EPV adjustments skipped for %s %s: %s",
                        league, season, e$message), call. = FALSE)
      })

      message(sprintf("    EPV: %d player-games", nrow(player_game_epv)))

      # --- WPA path ---
      player_game_wpa <- tryCatch({
        match_results <- .build_match_results(events, lineups)

        # #92: surface the standalone live-threat `epv` feature. spadl_chains
        # carry no EPV, but spadl_epv (computed above) does -- join it back on
        # the within-match action key so create_wp_features sees a real `epv`
        # column (and a non-degenerate xmargin) instead of the margin_poss-only
        # fallback.
        wp_chains <- spadl_chains
        if (all(c("match_id", "action_id", "epv") %in% names(spadl_epv))) {
          wp_chains <- data.table::as.data.table(data.table::copy(spadl_chains))
          epv_lookup <- data.table::as.data.table(spadl_epv)[
            , .(match_id, action_id, epv)]
          wp_chains[epv_lookup, epv := i.epv, on = c("match_id", "action_id")]
        }

        # #93: re-derive red cards from raw events and flag them onto the chains
        # so red_card_diff stops being a dead constant 0.
        wp_chains     <- add_red_card_to_chains(wp_chains, events)

        wp_feat       <- create_wp_features(wp_chains, match_results)
        spadl_wpa     <- add_wp_vars(wp_feat, wp_model)
        spadl_wpa     <- assign_wpa_credit(spadl_wpa)
        pgw           <- aggregate_player_game_wpa(spadl_wpa, lineups)
        message(sprintf("    WPA: %d player-games", nrow(pgw)))
        pgw
      }, error = function(e) {
        warning(sprintf("WPA failed for %s %s: %s", league, season, e$message), call. = FALSE)
        NULL
      })

      # --- PSV path ---
      player_game_psv <- NULL
      league_match_ids <- unique(events$match_id)
      if (has_match_stats) {
        tryCatch({
          league_stats <- all_match_stats[all_match_stats$match_id %in% league_match_ids, ]
          if (nrow(league_stats) > 0) {
            # Enrich with per-match xMetrics BEFORE scoring — the blend model was
            # trained WITH over-performance/gsaa features (step 7), so serving on
            # box-score-only stats is a train/serve skew (finishing under-credited).
            # fail_if_missing_frac left at the library default (Inf, warn-only) —
            # this whole block is per-league tryCatch'd anyway, but a hard stop()
            # here would still convert "PSV present but box-score-only" into
            # "PSV entirely NA for this league" on any local/remote gap.
            league_stats <- enrich_match_stats_with_xmetrics(league_stats, verbose = FALSE,
                                                             source = xm_source)
            player_game_psv <- compute_player_psv(league_stats, min_adjust = FALSE,
                                                  center = TRUE, scale_to_minutes = TRUE,
                                                  exclude_efficiency = FALSE, target = "blend",
                                                  position_means = .psv_position_means,
                                                  reliability = .psv_reliability,
                                                  center_weights = .psv_center_weights)
            message(sprintf("    PSV: %d player-games", nrow(player_game_psv)))
          }
        }, error = function(e) {
          warning(sprintf("PSV failed for %s %s: %s",
                          league, season, e$message), call. = FALSE)
        })
      }

      # pannadata#74: the skills cache (cache-skills/01_match_stats.rds) is built
      # by a SEPARATE skills-pipeline run and can lag the predictions pipeline —
      # for World Cup it has no current WC box-score rows, so the path above
      # yields 0 PSV/OSV/DSV for every WC player-game. When the cache gives no
      # PSV for ANY of this league's matches, compute PSV inline from the league's
      # own box scores (load_opta_stats → compute_match_level_opta_stats →
      # compute_player_psv), exactly the transform the skills pipeline applies in
      # 01_compute_match_stats.R. center = TRUE centers over the INPUT population
      # (here: this tournament's players, i.e. "vs the average WC player") — the
      # documented per-population semantics (panna/CLAUDE.md), preserved here.
      cache_covered_ids <- if (!is.null(player_game_psv) &&
                               "match_id" %in% names(player_game_psv)) {
        unique(player_game_psv$match_id)
      } else {
        character(0)
      }
      if (length(setdiff(league_match_ids, cache_covered_ids)) > 0L) {
        tryCatch({
          box_stats <- load_opta_stats(league, season = league_season)
          if (!is.null(box_stats) && nrow(box_stats) > 0) {
            box_dt <- data.table::as.data.table(box_stats)
            box_dt <- box_dt[match_id %in% league_match_ids]
            # Only fill matches the cache path missed — never clobber cache rows.
            if (length(cache_covered_ids) > 0L) {
              box_dt <- box_dt[!match_id %in% cache_covered_ids]
            }
            if (nrow(box_dt) > 0L) {
              box_dt[, league := league]
              box_dt[, season := league_season]
              match_level <- compute_match_level_opta_stats(box_dt, min_minutes = 10)
              if (!is.null(match_level) && nrow(match_level) > 0L) {
                # Enrich BEFORE scoring (train/serve parity — see note above).
                # fail_if_missing_frac left at the library default — see the
                # matching note on the cache-path enrich call above.
                match_level <- enrich_match_stats_with_xmetrics(match_level, verbose = FALSE,
                                                                source = xm_source)
                inline_psv <- compute_player_psv(match_level, min_adjust = FALSE,
                                                 center = TRUE, scale_to_minutes = TRUE,
                                                 exclude_efficiency = FALSE, target = "blend",
                                                 position_means = .psv_position_means,
                                                 reliability = .psv_reliability,
                                                 center_weights = .psv_center_weights)
                player_game_psv <- data.table::rbindlist(
                  list(player_game_psv, inline_psv), fill = TRUE, use.names = TRUE
                )
                message(sprintf("    PSV (inline box scores, #74): +%d player-games",
                                nrow(inline_psv)))
              }
            }
          } else {
            # Surface, don't fabricate: leave PSV NA for the uncovered matches.
            warning(sprintf(
              "PSV gap for %s %s (#74): %d match(es) absent from skills cache AND no box scores from load_opta_stats — PSV left NA",
              league, season,
              length(setdiff(league_match_ids, cache_covered_ids))
            ), call. = FALSE)
          }
        }, error = function(e) {
          warning(sprintf("Inline PSV (#74) failed for %s %s: %s",
                          league, season, e$message), call. = FALSE)
        })
      }

      # --- Merge ---
      game_ratings <- build_player_game_ratings(
        player_game_epv = player_game_epv,
        player_game_wpa = player_game_wpa,
        player_game_psv = player_game_psv
      )

      # --- Display: finishing luck (goals - xGOT) + placement skill ---
      # "Unlucky striker" signal: a player who placed shots well (high xGOT) but
      # didn't score reads negative. Pulled from the per-match xMetrics; display
      # only (not a value-blend input). NA-safe left join by (player_id, match_id).
      tryCatch({
        xg_disp <- data.table::as.data.table(
          load_opta_xmetrics(league, season = league_season,
                             source = xm_source, by_match = TRUE))
        # GSAA + duel WOE ride the same per-match xMetrics table as the trio —
        # display-only columns for the blog's Defending/Duels tabs (requested
        # 2026-07-18); intersect() keeps this NA-safe when a column is absent.
        disp_cols <- intersect(c("goals_minus_xgot", "placement_added", "xgot",
                                 "gsaa", "gsaa_per90", "xgot_faced", "goals_conceded",
                                 "aerial_woe_per90", "aerial_poss_woe_per90",
                                 "takeon_woe_per90", "tackle_poss_woe_per90",
                                 "containment_woe_per90"),
                               names(xg_disp))
        if (length(disp_cols) > 0 && all(c("player_id", "match_id") %in% names(xg_disp))) {
          xg_disp <- unique(xg_disp[, c("player_id", "match_id", disp_cols), with = FALSE],
                            by = c("player_id", "match_id"))
          game_ratings[, match_id := as.character(match_id)]
          xg_disp[, match_id := as.character(match_id)]
          game_ratings <- merge(game_ratings, xg_disp,
                                by = c("player_id", "match_id"), all.x = TRUE)
        }
      }, error = function(e) {
        warning(sprintf("xGOT display cols join failed for %s %s: %s",
                        league, season, e$message), call. = FALSE)
      })

      # match_date from lineups
      dt_lineups <- data.table::as.data.table(lineups)
      if ("match_date" %in% names(dt_lineups)) {
        match_dates  <- dt_lineups[, .(match_date = match_date[1]), by = match_id]
        game_ratings <- merge(game_ratings, match_dates, by = "match_id", all.x = TRUE)
      }

      game_ratings[, league := league]
      game_ratings[, season := season]

      validate_game_log_schema(game_ratings, league, season)

      all_game_logs[[league]] <- game_ratings
      message(sprintf("    Final: %d player-games", nrow(game_ratings)))

      # Free memory between leagues
      rm(events, lineups, spadl, spadl_chains, chain_outcomes, spadl_labeled,
         spadl_epv, spadl_credit, player_game_epv,
         player_game_wpa, player_game_psv, game_ratings)
      gc(verbose = FALSE)

    },
    panna_skip_league = function(e) {
      # Intentional skip — already messaged at the source.
      invisible(NULL)
    },
    error = function(e) {
      is_data_error <- inherits(e, "panna_data_not_found") ||
        grepl("^No data found for|^No .+ data available", e$message)
      if (is_data_error) {
        message(sprintf("    Skipping %s %s — data not available", league, season))
      } else {
        warning(sprintf("ERROR processing %s %s: %s",
                        league, season, e$message), call. = FALSE)
      }
    })
  }

  # --- Combine season output ---
  if (length(all_game_logs) == 0) {
    warning(sprintf("No game logs produced for season %s — skipping", season), call. = FALSE)
    return(NULL)
  }

  game_logs <- data.table::rbindlist(all_game_logs, fill = TRUE)

  n_leagues_ok <- length(all_game_logs)
  if (n_leagues_ok < length(blog_leagues) / 2) {
    warning(sprintf("Season %s: only %d/%d leagues produced game logs.",
                    season, n_leagues_ok, length(blog_leagues)), call. = FALSE)
  }

  message(sprintf("\n  [%s] Combined: %d player-games across %d leagues",
                  season, nrow(game_logs), n_leagues_ok))

  # Rename columns to match blog expectations
  data.table::setnames(
    game_logs,
    old = c("piero_value", "epv_offensive", "epv_defensive", "minutes_played"),
    new = c("panna", "offense", "defense", "total_minutes"),
    skip_absent = TRUE
  )

  # --- Cross-league PSV calibration (LIVE-PSV-UNBLOCK 2026-07-20, task 3) ---
  # PSV is a dot-product of box-score rates that barely vary by league (same
  # reason PSR needs it — see league-offsets.md), so a Saudi/MLS dominator can
  # outrank a Big-5 star on a cross-league sort. Reuse the SAME artifact 08b
  # applies to PSR: compute_psr_league_offsets() runs build_league_network()
  # on per-game PSV itself (the PSR analogue), so the offset is already on
  # PSV's own per-90 scale — no cross-metric rescaling needed. Saved by step
  # 06 (estimated-skills) to cache-skills/psr_league_offsets.parquet.
  # DISPLAY-SIDE ONLY: applied here, after compute_player_psv() has already
  # scored/centered every league — the RAPM psvf90 target and the per-league
  # within-round centering above never see it. `psv` here is already
  # minutes-scaled (scale_to_minutes = TRUE), so end-add
  # offset * total_minutes / 90 (mirrors how scale_to_minutes itself turns a
  # per-90 rate into a minutes-scaled one). Split evenly across osv/dsv,
  # mirroring apply_psr_league_offsets()'s /2 split (osv + dsv = psv stays
  # exact). Set psv_league_offset_pricing <- FALSE before sourcing to disable.
  # envir=globalenv(): see the psv_center_weights guard above (source(local=TRUE)).
  .psv_league_offset_pricing <- !exists("psv_league_offset_pricing",
                                        envir = globalenv(), inherits = FALSE) ||
    isTRUE(get("psv_league_offset_pricing", envir = globalenv()))
  if (isTRUE(.psv_league_offset_pricing) && "psv" %in% names(game_logs)) {
    psv_offsets_path <- file.path("data-raw", "cache-skills", "psr_league_offsets.parquet")
    psv_offsets <- if (file.exists(psv_offsets_path)) {
      arrow::read_parquet(psv_offsets_path)
    } else NULL
    if (is.null(psv_offsets)) {
      message(sprintf(
        "\n  [%s] NOTE: PSV league offsets not found (%s) — run estimated-skills step 06 first; game-logs PSV left league-relative",
        season, psv_offsets_path))
      game_logs[, psv_league_offset := 0]
    } else {
      off_dt <- data.table::as.data.table(psv_offsets)[, .(.comp = league, .off = offset)]
      game_logs[, .comp := vapply(league, function(L)
        tryCatch(to_opta_league(L), error = function(e) L), character(1))]
      game_logs <- merge(game_logs, off_dt, by = ".comp", all.x = TRUE, sort = FALSE)

      # Don't silently impute: report every league the offset artifact doesn't
      # cover (it may not span every blog league) rather than pretend offset=0
      # is a known value.
      missing_leagues <- sort(unique(game_logs[is.na(.off)]$league))
      if (length(missing_leagues) > 0L) {
        message(sprintf(
          "\n  [%s] NOTE: PSV league offsets missing for %d league(s) (offset=0): %s",
          season, length(missing_leagues), paste(missing_leagues, collapse = ", ")))
      }
      game_logs[is.na(.off), .off := 0]

      mins_scale <- as.numeric(game_logs$total_minutes) / 90
      mins_scale[is.na(mins_scale) | mins_scale < 0] <- 0
      game_logs[, psv_league_offset := .off * mins_scale]
      game_logs[, psv := psv + psv_league_offset]
      if (all(c("osv", "dsv") %in% names(game_logs))) {
        game_logs[, osv := osv + psv_league_offset / 2]
        game_logs[, dsv := dsv + psv_league_offset / 2]
      }
      game_logs[, c(".comp", ".off") := NULL]
      n_adj <- game_logs[psv_league_offset != 0, .N]
      message(sprintf("  [%s] PSV league offsets applied to %d/%d rows (%d leagues in artifact)",
                      season, n_adj, nrow(game_logs), nrow(psv_offsets)))
    }
  }

  # SPM lookup (for this season's end year)
  if (!is.null(all_spm_dt)) {
    sy <- .season_end_year(season)
    spm_lookup <- all_spm_dt[season_end_year == sy, .(player_id, spm_overall = spm)]
    spm_lookup <- spm_lookup[, .SD[1], by = player_id]
    if (nrow(spm_lookup) > 0) {
      game_logs <- merge(game_logs, spm_lookup, by = "player_id", all.x = TRUE)
      na_spm <- sum(is.na(game_logs$spm_overall))
      message(sprintf("  [%s] SPM joined: %d/%d have SPM (season_end_year=%s)",
                      season, nrow(game_logs) - na_spm, nrow(game_logs), sy))
    } else {
      message(sprintf("  [%s] No SPM rows for season_end_year=%s — skipping join",
                      season, sy))
    }
  }

  # Season-scoped panna percentile
  player_totals <- game_logs[, .(total_panna = sum(panna, na.rm = TRUE)), by = player_id]
  player_totals[, panna_percentile := round(100 * rank(total_panna, ties.method = "min") / .N, 1)]
  game_logs <- merge(game_logs, player_totals[, .(player_id, panna_percentile)],
                     by = "player_id", all.x = TRUE)

  # Column selection/order
  blog_cols <- intersect(
    c("player_id", "player_name", "match_id", "match_date", "league", "season",
      "team_id", "position", "total_minutes",
      "panna", "offense", "defense", "spm_overall", "panna_percentile",
      "epv_total", "epv_total_adj",
      "epv_offensive_adj", "epv_defensive_adj", "opp_adj",
      "epv_passing", "epv_shooting", "epv_dribbling", "epv_aerial",
      "epv_keeping", "epv_defending",
      # epv_duel_blame and epv_aerial_att complete the defensive roll-up:
      #   epv_defensive = epv_defending + epv_keeping
      #                   + (epv_aerial - epv_aerial_att) + epv_duel_blame
      # Without them `epv_defensive` cannot be reconstructed from the exported
      # components -- `epv_aerial` ships as a TOTAL while only its non-attacking
      # share is defensive. Added 2026-09-02 (panna#228), where that gap caused
      # an inversion to be attributed to the wrong term.
      "epv_duel_blame", "epv_aerial_att",
      "wpa_total", "wpa_as_actor", "wpa_as_receiver",
      "psv", "osv", "dsv", "psv_league_offset",
      "goals_minus_xgot", "placement_added", "xgot",
      "gsaa", "gsaa_per90", "xgot_faced", "goals_conceded",
      "aerial_woe_per90", "aerial_poss_woe_per90",
      "takeon_woe_per90", "tackle_poss_woe_per90", "containment_woe_per90",
      "piero_value_p90"),
    names(game_logs)
  )
  game_logs <- game_logs[, ..blog_cols]

  # Round numerics (except minutes)
  num_cols   <- names(game_logs)[vapply(game_logs, is.numeric, logical(1))]
  round_cols <- setdiff(num_cols, "total_minutes")
  for (col in round_cols) {
    data.table::set(game_logs, j = col, value = round(game_logs[[col]], 4))
  }

  data.table::setorder(game_logs, league, match_date, match_id, -panna)

  out_path <- file.path(cache_dir, sprintf("game_logs_%s.parquet", season))
  # Subset-league backfill: merge into the existing per-season file rather than
  # clobbering it (which would delete every other league's rows for the season).
  # Idempotent: drop existing rows for the leagues we just rebuilt, then append.
  if (isTRUE(merge_subset_leagues) && file.exists(out_path)) {
    existing <- data.table::as.data.table(arrow::read_parquet(out_path))
    # Release arrow's memory-mapped file handle before we overwrite the same
    # path — Windows error 1224 ("user-mapped section open") otherwise.
    gc()
    # Transition shim (2026-07-07 panna_value -> piero_value rename): a
    # pre-rename per-season parquet carries panna_value_p90; without the
    # rename the rbindlist(fill=TRUE) below would ship BOTH columns each
    # half-NA (mixed schema) instead of one complete piero_value_p90.
    if ("panna_value_p90" %in% names(existing) &&
        !"piero_value_p90" %in% names(existing)) {
      data.table::setnames(existing, "panna_value_p90", "piero_value_p90")
    }
    rebuilt <- unique(game_logs$league)
    kept <- existing[!league %in% rebuilt]
    n_kept <- nrow(kept)
    n_dropped <- nrow(existing) - n_kept
    game_logs <- data.table::rbindlist(list(kept, game_logs), fill = TRUE, use.names = TRUE)
    data.table::setorder(game_logs, league, match_date, match_id, -panna)
    rm(existing, kept); gc()
    message(sprintf("  [%s] Merge: kept %d existing rows (replaced %d for %s), total %d",
                    season, n_kept, n_dropped, paste(rebuilt, collapse = ","),
                    nrow(game_logs)))
  }
  # Write atomically via a temp file then replace, so a write failure can never
  # corrupt the existing per-season parquet (which holds every other league).
  out_tmp <- paste0(out_path, ".tmp")
  arrow::write_parquet(game_logs, out_tmp)
  if (file.exists(out_path)) file.remove(out_path)
  file.rename(out_tmp, out_path)
  message(sprintf("  [%s] Written: %s (%.1f MB, %d rows × %d cols)",
                  season, out_path,
                  file.size(out_path) / (1024 * 1024),
                  nrow(game_logs), ncol(game_logs)))

  # Free memory between seasons
  rm(game_logs, player_totals, all_game_logs)
  gc(verbose = FALSE)

  out_path
}

# 4. Process each season ----

season_paths <- list()
if (isTRUE(build_game_logs)) {
  for (s in game_log_seasons) {
    p <- tryCatch(
      .process_season(s),
      error = function(e) {
        warning(sprintf("Season %s aborted: %s", s, e$message), call. = FALSE)
        NULL
      }
    )
    if (!is.null(p)) season_paths[[s]] <- p
  }
} else {
  # Upload-only mode: reconstruct season_paths from existing files so the
  # alias + upload steps below have something to act on.
  for (s in game_log_seasons) {
    p <- file.path(cache_dir, sprintf("game_logs_%s.parquet", s))
    if (file.exists(p)) season_paths[[s]] <- p
  }
  message(sprintf("Upload-only mode: %d existing season parquet(s) found",
                  length(season_paths)))
}

if (length(season_paths) == 0) {
  stop("No seasons produced game logs. Check upstream data availability.")
}

# 5. Mirror current-season alias → game_logs.parquet (blog-workflow compat) ----

alias_src  <- file.path(cache_dir, sprintf("game_logs_%s.parquet", current_season_alias))
alias_path <- file.path(cache_dir, "game_logs.parquet")
if (isTRUE(mirror_alias) && file.exists(alias_src)) {
  file.copy(alias_src, alias_path, overwrite = TRUE)
  message(sprintf("\n  Mirrored alias: %s → game_logs.parquet",
                  basename(alias_src)))
} else if (!isTRUE(mirror_alias)) {
  message("\n  Skipping alias mirror (mirror_alias = FALSE) — keeping existing game_logs.parquet")
}

# 6. Register for step-13 publish (PA5/H-TORN: no upload here) ----

if (isTRUE(upload_game_logs)) {
  # Only include the alias file when we actually rewrote it -- otherwise a
  # partial historical re-backfill would overwrite the current-season alias
  # on the release with a stale copy. (Publish itself now happens once, for
  # every registered blog-latest file across all build steps, in
  # 13_publish_release_data.R.)
  candidates <- if (isTRUE(mirror_alias)) {
    unique(c(unlist(season_paths), alias_path))
  } else {
    unlist(season_paths)
  }
  files_to_publish <- candidates[file.exists(candidates)]

  if (exists("publish_files", envir = .GlobalEnv)) {
    publish_files$blog_latest <<- c(publish_files$blog_latest, files_to_publish)
    message(sprintf("\n  Registered %d file(s) for blog-latest publish (step 13)",
                    length(files_to_publish)))
  } else {
    message("\n  (standalone run -- not registered for step-13 publish)")
  }
} else {
  message("\n(upload_game_logs = FALSE — not registering for publish)")
}

# 7. Summary ----

message("\n========================================")
message("Game logs exported successfully!")
message("========================================")
for (s in names(season_paths)) {
  fi <- file.info(season_paths[[s]])
  message(sprintf("  %s  %s  (%.1f MB)",
                  s, season_paths[[s]], fi$size / (1024 * 1024)))
}
if (isTRUE(upload_game_logs)) {
  message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
}
