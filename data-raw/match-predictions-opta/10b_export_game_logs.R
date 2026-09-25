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
# League-seasons whose xMetrics display join was tried / failed this run. A
# systemic failure (every league, as on 2026-09-24 with a stale local table)
# blocks the upload below; an isolated one is reported loudly at the end.
.xm_join_tried <- 0L
.xm_join_failed <- character(0)

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
    # Derived from the clock, never pinned. The pinned "2025-2026" that used to
    # sit here went stale on 1 August 2026 and nothing failed: the weekly
    # pipeline rebuilt last season's game logs and republished them as the
    # current-season file every day, so the blog showed an empty Player Stats
    # page three matchweeks into the season while the file's timestamp said it
    # was built that morning. `current_season_alias` below is the max of this
    # vector, so this one line decides which season becomes game_logs.parquet.
    game_log_seasons <- current_domestic_season()
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
  data.table::setDT(all_match_stats)
  message(sprintf("  Loaded match stats: %d player-games", nrow(all_match_stats)))

  # Resolve the GK router and the PSV calibration bucket ONCE, here, on the
  # FULL cross-league/cross-season population -- then carry both as columns so
  # the per-league row-filter below hands each compute_player_psv() call the
  # same answers. Both resolvers only see the rows passed to them:
  #   - .detect_gk_rows()'s majority vote is scope-dependent (panna PR #250)
  #   - resolve_position_group()'s season- and career-modal fallback tiers are
  #     capped by the population handed in, so resolving per league-season
  #     narrows "career" to one league's own history. That is what leaves
  #     ~3.6% of rows / ~2.3% of minutes with pos_grp = NA (scored
  #     uncalibrated, factor 1), concentrated in 2013-2016 league-seasons
  #     whose `position` is blank on every row -- those players' buckets are
  #     obvious from their careers elsewhere, just not from inside that slice.
  # season_end_year is NOT in this cache (only the `season` LABEL), and
  # resolve_position_group() resolves per season ONLY when it's present --
  # without it the full-population call collapses to one career-wide bucket
  # per player, silently losing the season dimension the old per-league-season
  # slices kept for free (each slice was one season). Measured on the live
  # cache: omitting this changes 7.71% of already-resolved buckets (mostly
  # MID<->FWD/DEF churn from converted players); with it, 1.16% -- and that
  # remainder IS the intended gain (one consistent bucket for a player who
  # splits a season across leagues, instead of a different one per league).
  # Derived from the LABEL, never match_date (panna/CLAUDE.md), and mapped
  # over unique labels rather than row-by-row (vectorization gotcha).
  if (!"season_end_year" %in% names(all_match_stats)) {
    .sey_map <- vapply(unique(all_match_stats$season), extract_season_end_year,
                       numeric(1))
    all_match_stats[, season_end_year :=
                      as.integer(.sey_map[as.character(season)])]
  }
  all_match_stats[, .is_gk_full := .detect_gk_rows(all_match_stats)]
  all_match_stats[, .pos_grp_full := .psv_pos_grp(all_match_stats, .is_gk_full)]
  # Report the live figure only -- a baked-in "was X%" baseline would keep
  # printing a fixed historical number as though it were a current comparison.
  # The 2026-09-14 measurement (3.52% -> 0.77% of rows, 2.24% -> 0.44% of
  # minutes) is recorded in the commit and the comment above instead.
  message(sprintf(
    "  Resolved pos_grp once on the full population: %.2f%% of rows unresolved",
    100 * mean(is.na(all_match_stats$.pos_grp_full))))
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

# Net goals by play type for one season: ng_breakdown_<season>.parquet beside
# the game logs. Every league whose breakdown built is written; a league whose
# breakdown FAILED is left out entirely -- also dropped from the existing file on
# a subset re-run -- so its players show no chart rather than last run's numbers
# beside this run's game logs. Subset-league runs merge the way the game logs do.
.write_ng_breakdown <- function(parts, failed, season) {
  if (length(failed)) {
    message(sprintf("  [%s] net goals breakdown left OUT for %s (failed); other leagues written",
                    season, paste(failed, collapse = ", ")))
  }
  bd <- data.table::rbindlist(parts, use.names = TRUE)
  path <- file.path(cache_dir, sprintf("ng_breakdown_%s.parquet", season))
  if (isTRUE(merge_subset_leagues) && file.exists(path)) {
    existing <- data.table::as.data.table(arrow::read_parquet(path))
    gc()   # release arrow's mapped handle before overwriting (Windows 1224)
    drop <- c(names(parts), failed)
    bd <- data.table::rbindlist(list(existing[!league %in% drop], bd),
                                use.names = TRUE, fill = TRUE)
  }
  if (!nrow(bd)) return(invisible(NULL))
  tmp <- paste0(path, ".tmp")
  arrow::write_parquet(bd, tmp)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  message(sprintf("  [%s] Written: %s (%d rows, %d leagues)", season, path,
                  nrow(bd), data.table::uniqueN(bd$league)))
  c(path, .write_ng_player_breakdown(bd, season, cache_dir))
}

# Season totals per player (the file the player page reads): sorted by player in
# row groups of 5,000, so the page's filtered read touches one group instead of a
# season's 2M+ match rows. Built from the whole season file after any merge, so a
# subset re-run still totals every league.
.write_ng_player_breakdown <- function(bd, season, dir) {
  pl <- .ng_breakdown_players(bd)
  path <- file.path(dir, sprintf("ng_player_breakdown_%s.parquet", season))
  tmp <- paste0(path, ".tmp")
  arrow::write_parquet(pl, tmp, chunk_size = 5000L)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  message(sprintf("  [%s] Written: %s (%d players)", season, path, data.table::uniqueN(pl$player_id)))
  path
}

# Stage timers. Reading the code cannot say where a 10b run spends its time --
# only a timer can -- so every league reports its split and the run ends with
# the total per stage. Base R only, so it works in CI as well as locally.
.stage_secs <- numeric(0)     # run totals by stage
.stage_league <- numeric(0)   # this league's split
.stage_t0 <- NULL
.stage_start <- function() {
  .stage_league <<- numeric(0)
  .stage_t0 <<- proc.time()[["elapsed"]]
}
.stage <- function(name) {
  if (is.null(.stage_t0)) return(invisible())
  now <- proc.time()[["elapsed"]]
  .stage_league[name] <<- sum(.stage_league[name], now - .stage_t0, na.rm = TRUE)
  .stage_secs[name] <<- sum(.stage_secs[name], now - .stage_t0, na.rm = TRUE)
  .stage_t0 <<- now
}
.stage_report <- function(label) {
  tot <- sum(.stage_league)
  message(sprintf("    time %s: %.0fs (%s)", label, tot, paste(sprintf("%s %.0f%%",
    names(.stage_league), 100 * .stage_league / max(tot, 1e-9)), collapse = ", ")))
}

# Breakdown files written by THIS run -- the only ones registered for publish, so
# a season whose breakdown failed never ships last run's file beside new logs.
ng_bd_paths <- character(0)
ng_bd_missing <- character(0)   # "season (leagues)" whose breakdown was not built

# Process a single season: returns path to written parquet, or NULL on failure.
.process_season <- function(season) {
  message(sprintf("\n########## SEASON %s ##########", season))
  all_game_logs <- list()
  all_ng_breakdown <- list()   # net goals by play type, one table per league
  ng_bd_failed <- character(0)

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

      .stage_start()
      events  <- load_opta_match_events(league, season = league_season)
      lineups <- load_opta_lineups(league, season = league_season)
      .stage("load events + lineups")
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
      .stage("SPADL + chains")

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
      .stage("EPV + credit")

      ng_cols <- NULL   # reset per league: a skip must not inherit the last one's
      ng_pay <- NULL; ng_pre <- NULL
      # --- Net goals ledger (ADDITIVE; every production column above is
      # untouched). Three columns join the frame: `net_goals` and its two
      # halves. The ledger allocates each action so a team's players sum to
      # that team's own goal difference; see
      # pannaverse/docs/plans/EPV-NET-GOALS.md.
      #
      # Deliberately computed BEFORE the position/opponent adjustments below.
      # Centring breaks conservation by construction, so the published column
      # carries the raw ledger and any rating layer centres it itself via
      # ng_adjust_for_rating().
      #
      # xPass has to be added explicitly. assign_epv_credit() computes it
      # internally and does not leave it on the frame, and WITHOUT IT the
      # passer/receiver difficulty split silently degrades to actor-keeps-all
      # rather than failing -- so it is asserted rather than assumed.
      tryCatch({
        spadl_ng <- add_xpass_to_spadl(spadl_epv, xpass_model)
        n_pass_xp <- sum(spadl_ng$action_type == "pass" & !is.na(spadl_ng$xpass))
        if (n_pass_xp < 0.5 * sum(spadl_ng$action_type == "pass")) {
          stop(sprintf("xpass on only %d of %d passes", n_pass_xp,
                       sum(spadl_ng$action_type == "pass")))
        }
        # xGOT and lineups for the shot split (strike xG -> xGOT, finish
        # xGOT -> outcome, keeper named on goals and saves). The ledger's
        # keeper pool rule assumes the split ran, so publishing without it
        # would have keepers credited for saves, never blamed for goals, and
        # out of the pool blame too. Required, not optional: a league without
        # xGOT on its shots drops its net goals columns instead.
        # xgot_model_override pins the model like the EPV/WP overrides do: the
        # bare loader falls back to pannadata's LOCAL copy when pannamodels is
        # not installed, which can lag the published model (MODELS.md).
        ng_xgot_model <- if (exists("xgot_model_override")) xgot_model_override else load_xgot_model()
        ng_shots <- as.data.frame(load_opta_shot_events(league, season = league_season))
        ng_lk <- c("match_id", "event_id", "type_id", "goalmouth_y", "goalmouth_z",
                   intersect(c("situation", "is_blocked", "body_part"), names(ng_shots)))
        spadl_ng <- add_xgot_to_spadl(spadl_ng, ng_xgot_model, ng_shots[, ng_lk])
        is_shot <- spadl_ng$action_type == "shot"
        if (sum(is_shot) > 0 && mean(!is.na(spadl_ng$xgot[is_shot])) < 0.95) {
          stop(sprintf("xGOT on only %d of %d shots", sum(!is.na(spadl_ng$xgot[is_shot])),
                       sum(is_shot)))
        }
        ng_fx <- as.data.frame(load_opta_fixtures(league, season = league_season,
                                                  source = "local"))
        ng_pay <- ng_build_ledger(spadl_ng,
                                  adj = ng_build_adjacency(events, verbose = FALSE),
                                  fixtures = ng_fx, lineups = lineups, verbose = FALSE)
        # Which shot-aftermath line this league-season used, said now and with
        # the league's name: a small tournament borrows the default line, and
        # that must be findable in the log (message, not a deferred warning).
        ng_af <- attr(ng_pay, "shot_aftermath_fit")
        message(if (is.null(ng_af)) sprintf("    net goals (%s): shot aftermath OFF, shots at xG", league)
                else sprintf("    net goals (%s): shot aftermath A = %.4f + %.4f x xG, %s (%d shots)",
                             league, ng_af$intercept, ng_af$slope,
                             if (isTRUE(ng_af$fallback)) "DEFAULT line, too few shots to fit" else "fitted here",
                             ng_af$n_fit))
        ng_pay <- ng_spread_pools(ng_pay, spadl_ng, lineups, verbose = FALSE)
        ng_pg  <- data.table::as.data.table(
          ng_player_game(ng_pay, lineups, verbose = FALSE))
        data.table::setnames(ng_pg, c("epv_offensive", "epv_defensive"),
                             c("ng_offensive", "ng_defensive"))
        # Stashed, NOT merged onto player_game_epv:
        # build_player_game_ratings() assembles the published frame from a fixed
        # column set, so anything joined here is silently dropped before the
        # parquet is written. It reported "11427 of 11427 matched" and wrote a
        # file with no net-goals columns at all. The join happens after that
        # function instead.
        ng_cols <- ng_pg[, .(match_id, player_id, team_id, minutes_played,
                             net_goals, ng_offensive, ng_defensive)]
        message(sprintf("    net goals: %d player-rows computed", nrow(ng_cols)))
      }, error = function(e) {
        # Never fail the export over an additive column: the production
        # columns are complete without it, and a league that cannot build a
        # ledger should publish the rest rather than nothing.
        message(sprintf("    net goals SKIPPED for %s: %s", league,
                        conditionMessage(e)))
      })


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
      .stage("net goals ledger + EPV adjustments")

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

      .stage("WPA")
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
            # Both resolved once on the full population above and carried
            # through this row-filter -- NOT recomputed on this league-season
            # slice, which is what narrows resolve_position_group()'s career
            # fallback and leaves blank-position seasons uncalibrated.
            player_game_psv <- compute_player_psv(league_stats, min_adjust = FALSE,
                                                  center = TRUE, scale_to_minutes = TRUE,
                                                  exclude_efficiency = FALSE, target = "blend",
                                                  position_means = .psv_position_means,
                                                  reliability = .psv_reliability,
                                                  center_weights = .psv_center_weights,
                                                  is_gk = league_stats$.is_gk_full,
                                                  .pos_grp_override = league_stats$.pos_grp_full)
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
                # These rows are NOT in all_match_stats (that's why we're here),
                # so the full-population columns can't ride along -- join by
                # player_id instead, falling back to this slice's own resolution
                # for anyone with no cache history at all (e.g. a WC player from
                # an uncovered domestic league). Better than resolving purely
                # within one tournament, honest where no wider history exists.
                .inline_is_gk <- .detect_gk_rows(match_level)
                .inline_pos   <- resolve_position_group(match_level)
                if (has_match_stats) {
                  # Join on (player_id, season_end_year), NOT player_id alone:
                  # .pos_grp_full is season-varying by construction (that is
                  # the whole reason season_end_year is derived above), so a
                  # player_id-only unique() would keep whichever season loaded
                  # first and hand a converted player (CB->FWD, winger->
                  # fullback) their wrong-era bucket. .is_gk_full is likewise
                  # not constant per player -- .detect_gk_rows() ORs the
                  # majority vote with each row's own raw label, so the
                  # emergency-keeper cohort has genuinely row-varying values.
                  .ml_sey <- if ("season_end_year" %in% names(match_level)) {
                    as.integer(match_level$season_end_year)
                  } else {
                    rep(extract_season_end_year(league_season), nrow(match_level))
                  }
                  .pg_season <- unique(
                    all_match_stats[!is.na(season_end_year),
                                    .(player_id, season_end_year,
                                      .is_gk_full, .pos_grp_full)],
                    by = c("player_id", "season_end_year")
                  )
                  .idx <- .pg_season[
                    data.table::data.table(player_id = match_level$player_id,
                                            season_end_year = .ml_sey),
                    on = .(player_id, season_end_year), which = TRUE]
                  .j_gk  <- .pg_season$.is_gk_full[.idx]
                  .j_pos <- .pg_season$.pos_grp_full[.idx]
                  .inline_is_gk <- data.table::fifelse(is.na(.j_gk), .inline_is_gk, .j_gk)
                  .inline_pos   <- data.table::fifelse(is.na(.j_pos), .inline_pos, .j_pos)
                }
                # Pin LAST, to the is_gk actually being used for scoring, so the
                # calibration bucket can never disagree with the model that
                # scored the row (the 07c failure mode, panna PR #250).
                .inline_pos <- .psv_pin_gk(.inline_pos, .inline_is_gk)
                inline_psv <- compute_player_psv(match_level, min_adjust = FALSE,
                                                 center = TRUE, scale_to_minutes = TRUE,
                                                 exclude_efficiency = FALSE, target = "blend",
                                                 position_means = .psv_position_means,
                                                 reliability = .psv_reliability,
                                                 center_weights = .psv_center_weights,
                                                 is_gk = .inline_is_gk,
                                                 .pos_grp_override = .inline_pos)
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

      .stage("PSV")
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
      .xm_join_tried <<- .xm_join_tried + 1L   # inside .process_season(): <<- reaches the script-level counter
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
        # message(), not warning(): R defers warnings to the end of the run, and
        # on 2026-09-24 this one hid a whole season published without its 12
        # xGOT / GSAA / duel columns (a stale LOCAL xmetrics table; set
        # XMETRICS_SOURCE=remote). pack_publish_game_logs.R now refuses a season
        # missing columns the others have.
        message(sprintf("!! xGOT display cols join FAILED for %s %s (%s): this season will lack xgot/gsaa/duel columns",
                        league, season, e$message))
        .xm_join_failed <<- c(.xm_join_failed, paste(league, season))
      })

      # match_date from lineups
      dt_lineups <- data.table::as.data.table(lineups)
      if ("match_date" %in% names(dt_lineups)) {
        match_dates  <- dt_lineups[, .(match_date = match_date[1]), by = match_id]
        game_ratings <- merge(game_ratings, match_dates, by = "match_id", all.x = TRUE)
      }

      if (!is.null(ng_cols)) {
        # The ledger pays every player on the pitch; this frame is action-driven
        # and has no row for a substitute who never touched the ball. Joining
        # straight in would drop their value and stop a match's sides
        # cancelling. Fold it back to their teams first, so a published team's
        # rows sum to exactly what the ledger gave that team.
        ng_pre <- ng_cols   # kept for the play-type breakdown's fold check
        ng_cols <- ng_fold_unpublished(
          ng_cols, data.table::as.data.table(game_ratings)[, .(match_id, player_id)],
          verbose = FALSE)
        game_ratings <- merge(
          data.table::as.data.table(game_ratings),
          ng_cols[, .(match_id, player_id, ng_team_id = team_id,
                      net_goals, ng_offensive, ng_defensive)],
          by = c("match_id", "player_id"), all.x = TRUE)
        # 14 of ENG 2024-2025's published rows have no `team_id` at all -- late
        # substitutes the lineups frame has no club for. They are the whole
        # reason a match's two sides did not cancel, because a row belonging to
        # neither side is in no team's total. The ledger derived a team for them
        # from the payment table, so take it where the frame has none.
        game_ratings[is.na(team_id) & !is.na(ng_team_id), team_id := ng_team_id]
        game_ratings[, ng_team_id := NULL]
        # Then force each team to its own goal difference. Without this the
        # ledger is antisymmetric but anchored to nothing: the median team-match
        # lands 0.20 goals from the scoreline, because restarts, half-time and
        # the event types SPADL does not carry move the state without booking a
        # payment. Sized before it was written -- 7.6% of absolute value,
        # per-player-game correlation 0.9972, no minutes bias -- and it is the
        # same step torp runs to reach 0.000. See `ng_reconcile_margin()`.
        #
        # Wrapped, and the columns DROPPED rather than shipped raw if it fails.
        # Everything above this line is best-effort by design -- the ledger build
        # is wrapped for exactly that reason -- but this call was not, so a single
        # bad fixtures frame would have taken the whole league's game logs with it
        # rather than just the additive column. Shipping an UNRECONCILED
        # `net_goals` instead would be worse than shipping none: it looks like the
        # real column, and the only thing that would notice is the blog's units
        # gate, days later and one repo away.
        game_ratings <- tryCatch(
          ng_reconcile_margin(game_ratings, ng_fx, verbose = TRUE),
          error = function(e) {
            message(sprintf(
              "    net goals DROPPED for %s: margin reconciliation failed (%s)",
              league, conditionMessage(e)))
            data.table::as.data.table(game_ratings)[
              , c("net_goals", "ng_offensive", "ng_defensive") := NULL][]
          })
        message(sprintf("    net goals: %d of %d published rows carry it",
                        sum(!is.na(game_ratings$net_goals)), nrow(game_ratings)))
        # Net goals by play type for the player page ("where their EPV comes
        # from"). .ng_breakdown() aborts unless every published row's parts add
        # up to its net_goals. A failure costs only this file, never the game
        # logs, and is counted so the season's breakdown is not written short.
        if ("net_goals" %in% names(game_ratings)) {
          # Caught whatever the error: the breakdown is additive and must not
          # cost the league its game logs. The message says which kind it was,
          # because a code bug and a real add-up mismatch need different fixes.
          bd <- tryCatch(.ng_breakdown(ng_pay, ng_pre, game_ratings), error = function(e) {
            kind <- if (inherits(e, "panna_ng_breakdown_mismatch")) "parts do not add up" else "code error"
            message(sprintf("    net goals breakdown FAILED for %s (%s): %s", league, kind,
                            conditionMessage(e)))
            NULL
          })
          if (is.null(bd)) {
            ng_bd_failed <- c(ng_bd_failed, league)
          } else {
            bd[, `:=`(league = league, season = season)]
            all_ng_breakdown[[league]] <- bd
            message(sprintf("    net goals breakdown: %d rows, %d categories",
                            nrow(bd), data.table::uniqueN(bd$category)))
          }
        }
      }
      game_ratings[, league := league]
      game_ratings[, season := season]

      validate_game_log_schema(game_ratings, league, season)

      .stage("merge + fold/anchor + breakdown")
      .stage_report(label)
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

  # --- PSV position calibration (panna#211) ---
  # A unit of PSV means different amounts of goal difference depending on
  # position, so a cross-position sort is not comparable without it: keepers'
  # PSV overstates their goal-difference impact by roughly 1/0.529 = 1.9x, which
  # put three of them in the published top 10 (Joan García 4th, Raya 7th,
  # Butez 9th on 2025-2026).
  #
  # BEFORE the league offsets below, which is the OPPOSITE of PSR's ordering in
  # steps 06/08b -- see apply_psv_calibration()'s roxygen for why PSV flips it.
  # In short: PSR's offset is separately estimated and independent of PSR's
  # scale, so PSR can be scaled either side of it. PSV's offsets are DERIVED
  # FROM PSV (compute_psr_league_offsets -> build_league_network(value_col =
  # "psv")) by step 06, which calibrates before deriving them -- so the offsets
  # in psr_league_offsets.parquet already arrive on the CALIBRATED scale.
  # Calibrating after adding them would compute (psv + offset) * f and scale the
  # offset a second time, shrinking a keeper's league offset by 0.529.
  # Calibrate first, then add: psv * f + offset, both terms on one scale.
  #
  # Keys on `pos_grp`, carried through from compute_player_psv(). Do NOT
  # substitute the exported `position` column: that is the per-match LINEUP
  # position and reads "Substitute" on ~29% of rows, which .player_role()
  # collapses to "OTHER" -- a calibration keyed on it is a silent no-op that
  # changes nothing and reports no error (verified: bit-identical output).
  # Own tryCatch, deliberately. This block sits AFTER the per-league loop closes,
  # so it runs inside the per-SEASON handler -- an error escaping here aborts the
  # whole season and publishes game logs for no league at all, while every other
  # season still reports success. A calibration regression should be loud and
  # distinguishable but must not destroy the unrelated, previously-reliable
  # export, which is the same reasoning 06_seasonal_skill_ratings.R gives for
  # wrapping its own PSV calibration call.
  game_logs <- tryCatch({
  if ("psv" %in% names(game_logs)) {
    if (!"pos_grp" %in% names(game_logs)) {
      warning(sprintf(
        "[%s] game_logs lacks `pos_grp` — PSV position calibration SKIPPED, so ",
        season),
        "published PSV is not comparable across positions and keepers will be ",
        "over-rated (see panna#211). Check that compute_player_psv() still ",
        "returns it and that build_player_game_ratings() carries it through.",
        call. = FALSE)
    } else {
      .psv_cal <- load_psv_calibration()
      if (is.null(.psv_cal) || nrow(.psv_cal) == 0) {
        warning(sprintf("[%s] PSV calibration table unavailable; PSV left ", season),
                "position-uncalibrated (keepers over-rated, see panna#211).",
                call. = FALSE)
      } else {
        # A blank cell or a renamed level in the CSV would silently coerce that
        # position's factor to 1 (see .psv_calibration_factor / the fac[pos]
        # lookup) -- indistinguishable from "correctly uncalibrated". Assert the
        # four expected levels are actually present and usable before relying on
        # them.
        .want <- c("GK", "DEF", "MID", "FWD")
        .have <- .psv_cal[.psv_cal$axis == "position" &
                            is.finite(.psv_cal$factor) & .psv_cal$factor > 0, ]$level
        if (!all(.want %in% .have)) {
          warning(sprintf("[%s] PSV calibration table is missing a usable factor for: %s",
                          season, paste(setdiff(.want, .have), collapse = ", ")),
                  " -- those positions ship UNCALIBRATED at factor 1 while the ",
                  "others are scaled, which is worse than calibrating none of ",
                  "them (it changes the relative ordering between positions). ",
                  "Check inst/extdata/psv_calibration.csv (see panna#211).",
                  call. = FALSE)
        }

        # Report the uncalibrated share rather than letting it pass as a known
        # value. Unresolved is NA (not the string "OTHER" -- resolve_position_group()
        # and .psv_pin_gk() both emit NA, and apply_psv_calibration() treats NA as
        # factor 1). It means a player whose every appearance in this league-season
        # was a substitute, plus (now rare, since the 2026-09-13 .detect_gk_rows()
        # majority-vote fix) a substitute keeper whose scoring path disagreed with
        # its resolved position. Measured 2026-09-12 (BEFORE that fix): ~3.6% of
        # rows, ~2.3% of minutes, concentrated in 2013-2016 league-seasons whose
        # `position` is blank on every row -- not yet re-measured, but the
        # substitute-keeper share of this should now be smaller.
        n_other <- game_logs[is.na(pos_grp), .N]
        .gk_top20 <- function(d) {
          # Resolve each player to ONE bucket by minutes, not by whichever row
          # happens to come first: a player appearing in two blog leagues in the
          # same season can carry a different pos_grp in each, and this is the
          # number meant to detect a calibration regression.
          tot <- d[!is.na(psv), .(psv = sum(psv, na.rm = TRUE),
                                  mins = sum(total_minutes, na.rm = TRUE),
                                  pos_grp = pos_grp[which.max(
                                    data.table::fifelse(is.na(total_minutes), 0,
                                                        as.numeric(total_minutes)))]),
                   by = player_id]
          tot <- tot[mins >= 900]
          if (nrow(tot) < 20) return(NA_real_)
          100 * mean(tot[order(-psv)][1:20]$pos_grp == "GK", na.rm = TRUE)
        }
        .pool_gk <- function(d) {
          tot <- d[!is.na(psv), .(mins = sum(total_minutes, na.rm = TRUE),
                                  pos_grp = pos_grp[1]), by = player_id][mins >= 900]
          if (nrow(tot) == 0) return(NA_real_)
          100 * mean(tot$pos_grp == "GK", na.rm = TRUE)
        }
        .before <- .gk_top20(game_logs)
        .pool   <- .pool_gk(game_logs)
        game_logs <- data.table::as.data.table(
          apply_psv_calibration(game_logs, position_col = "pos_grp",
                                calibration = .psv_cal)
        )
        .after <- .gk_top20(game_logs)
        message(sprintf(
          "  [%s] PSV position calibration applied (%d/%d rows uncalibrated); keeper share of top 20: %.0f%% -> %.0f%% (pool %.0f%%)",
          season, n_other, nrow(game_logs), .before, .after, .pool))

        # Anchor check (stats-discipline §1), matching the one step 06 applies to
        # PSR. Without it this block reports a regression as ordinary pipeline
        # chatter: if pos_grp were to resolve to NA for most rows while the COLUMN
        # still existed, the calibration would no-op, n_other would be large, and
        # the message would read "45% -> 45%" with nothing flagged. This is the
        # published-blog path, so it needs at least the safety net the seasonal
        # path has. Warn, don't stop: 10b is expensive and killing it here would
        # discard every league's game logs for the season.
        if (!is.na(.after) && !is.na(.pool) && .after > .pool + 25) {
          warning(sprintf(
            "[%s] PSV anchor FAILED: keepers are %.0f%% of the top 20 against a %.0f%% pool ",
            season, .after, .pool),
            sprintf("share, AFTER calibration (%d of %d rows were uncalibrated). ",
                    n_other, nrow(game_logs)),
            "Published PSV is keeper-dominated -- do not publish without checking ",
            "that pos_grp resolved and that inst/extdata/psv_calibration.csv is ",
            "current (see panna#211).", call. = FALSE)
        }
        rm(.psv_cal, n_other, .gk_top20, .pool_gk, .before, .after, .pool, .want, .have)
      }
    }
  }
  game_logs
  }, error = function(e) {
    warning(sprintf("[%s] PSV position calibration FAILED: %s", season,
                    conditionMessage(e)),
            " -- proceeding with UNCALIBRATED PSV for this season. Keepers will ",
            "be over-rated by roughly 1.9x relative to outfielders and a ",
            "cross-position sort is not comparable (see panna#211). The rest of ",
            "the game-log export is unaffected.", call. = FALSE)
    game_logs
  })

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
      # pos_grp ships alongside `position` rather than replacing it (additive).
      # `position` is the per-match LINEUP position and reads "Substitute" on
      # ~29% of rows, so it cannot be used to group or filter by position;
      # pos_grp is the GK/DEF/MID/FWD bucket the PSV calibration keys on, with
      # substitute rows carrying the player's own most-played bucket. "OTHER"
      # means genuinely unknown -- treat it as missing, not as a category.
      "team_id", "position", "pos_grp", "total_minutes",
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
      # Net goals ledger (panna >= 0.3.62). `net_goals` sums, per team, to that
      # team's OWN goal difference EXACTLY (max 1.8e-15 on ENG 2024-2025), so a
      # consumer must assert it PER TEAM and never through a home-minus-away fit
      # -- the difference is 2x the margin by construction. `ng_offensive` +
      # `ng_defensive` = `net_goals`, split by which half of the double entry the
      # payment sat on rather than by action type. `ng_recon` is the third part:
      # the share of the team's gap to the scoreline this player was given, kept
      # as its own column so one player's number can be traced by hand
      # (`ng_recon` is already inside both `net_goals` and `ng_defensive`, not
      # added on top). Absent for a league whose ledger could not be built; the
      # intersect() above drops it silently in that case, which is intended.
      "net_goals", "ng_offensive", "ng_defensive", "ng_recon",
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

  # Guarded: the season's game logs are already on disk, and a failure writing
  # the additive breakdown must not get the whole season reported ABORTED.
  bd_path <- tryCatch(.write_ng_breakdown(all_ng_breakdown, ng_bd_failed, season),
                      error = function(e) {
    message(sprintf("  [%s] net goals breakdown write FAILED: %s", season, conditionMessage(e)))
    NULL
  })
  if (!is.null(bd_path)) ng_bd_paths <<- c(ng_bd_paths, bd_path)
  if (length(ng_bd_failed) || (is.null(bd_path) && length(all_ng_breakdown))) {
    ng_bd_missing <<- c(ng_bd_missing, sprintf("%s (%s)", season,
      if (length(ng_bd_failed)) paste(ng_bd_failed, collapse = ", ") else "write failed"))
  }

  # Free memory between seasons
  rm(game_logs, player_totals, all_game_logs, all_ng_breakdown)
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
        # `message()`, NOT `warning()`. R DEFERS warnings to the end of the
        # script and caps the deferred list at 50, so an aborted season used to
        # surface as one line of "There were 50 or more warnings" printed AFTER
        # "Game logs exported successfully!". On 2026-09-22 that hid seven of
        # eleven seasons aborting at the events-coverage guard: the run reported
        # success, listed four seasons where eleven were asked for, and nothing
        # said the other seven were missing. A message prints at the moment it
        # happens, which is the only time it can stop a run that is going wrong.
        message(sprintf("\n  !! SEASON %s ABORTED: %s\n", s, conditionMessage(e)))
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
    b <- file.path(cache_dir, sprintf(c("ng_breakdown_%s.parquet", "ng_player_breakdown_%s.parquet"), s))
    ng_bd_paths <- c(ng_bd_paths, b[file.exists(b)])
  }
  message(sprintf("Upload-only mode: %d existing season parquet(s) found",
                  length(season_paths)))
}

if (length(season_paths) == 0) {
  stop("No seasons produced game logs. Check upstream data availability.")
}

# Requested vs built. A run that produces SOME of what was asked for is the
# dangerous case: "No seasons produced game logs" already stops the empty run,
# and a complete run is fine, but a partial one used to print
# "Game logs exported successfully!" over a short list and nothing else. That is
# how a net_goals backfill quietly built 4 of 11 seasons on 2026-09-22.
.missing_seasons <- setdiff(game_log_seasons, names(season_paths))
if (length(.missing_seasons) > 0L) {
  message(sprintf(paste0(
    "\n########################################\n",
    "INCOMPLETE: %d of %d season(s) produced game logs.\n",
    "MISSING: %s\n",
    "########################################\n"),
    length(season_paths), length(game_log_seasons),
    paste(.missing_seasons, collapse = ", ")))
  # Never publish a partial set. Uploading is outward-facing and the consumer
  # cannot tell a short release from a complete one -- the blog would serve a
  # history with holes in it and nothing would go red. A local build keeps what
  # it made; only the publish is blocked.
  if (isTRUE(upload_game_logs)) {
    stop(sprintf(
      paste0("Refusing to upload a partial backfill: %d of %d seasons built, ",
             "missing %s. Re-run with the upstream gap fixed, or set ",
             "upload_game_logs <- FALSE to keep the local files."),
      length(season_paths), length(game_log_seasons),
      paste(.missing_seasons, collapse = ", ")))
  }
}

# xMetrics display join: report every failure, and refuse to publish when it is
# systemic (more than a fifth of the league-seasons tried) -- that is a broken
# source, not one competition without xMetrics.
if (length(.xm_join_failed)) {
  .xm_share <- length(.xm_join_failed) / max(.xm_join_tried, 1L)
  message(sprintf(paste0("
!! xMetrics display join FAILED for %d of %d league-seasons (%.0f%%): %s
",
                         "   Those rows lack xgot / gsaa / duel columns. XMETRICS_SOURCE=%s."),
                  length(.xm_join_failed), .xm_join_tried, 100 * .xm_share,
                  paste(.xm_join_failed, collapse = ", "), xm_source))
  if (isTRUE(upload_game_logs) && .xm_share > 0.2) {
    stop(sprintf(paste0("Refusing to upload: the xMetrics display join failed for %.0f%% of league-seasons, ",
                        "so the game logs would lose their xGOT / GSAA / duel columns. Fix the source ",
                        "(on a dev box: XMETRICS_SOURCE=remote) or set upload_game_logs <- FALSE."), 100 * .xm_share))
  }
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
  files_to_publish <- c(candidates[file.exists(candidates)], ng_bd_paths)

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
# The breakdown is additive, so a missing one never blocks the game logs -- but
# the release then keeps LAST run's breakdown for that season beside new game
# logs. Say so where it will be seen: at the end, and as a GitHub Actions
# annotation on the run page (plain text on a local run).
if (length(.stage_secs)) {
  message("
Time by stage across every league-season built (seconds, share of total):")
  o <- sort(.stage_secs, decreasing = TRUE)
  for (nm in names(o)) message(sprintf("  %-36s %7.0f  %4.1f%%", nm, o[[nm]], 100 * o[[nm]] / sum(o)))
}
if (length(ng_bd_missing)) {
  txt <- sprintf(paste0("Net goals breakdown NOT built for %d season(s): %s. Those leagues' ",
                        "players get no player-page EPV chart; a season whose file was not ",
                        "rewritten keeps the previous one (the blog hides a player's chart ",
                        "where it no longer adds up to the game logs)."),
                 length(ng_bd_missing), paste(ng_bd_missing, collapse = ", "))
  message("
!! ", txt)
  if (nzchar(Sys.getenv("GITHUB_ACTIONS"))) cat("::warning::", txt, "
", sep = "")
}
