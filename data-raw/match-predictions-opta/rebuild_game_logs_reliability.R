# rebuild_game_logs_reliability.R
# Wave-2 driver: full-history game-logs rebuild for LIVE-PSV-UNBLOCK D1-v2.
#
# Why this exists (pannaverse/docs/plans/LIVE-PSV-UNBLOCK-2026-07-20.md, D3):
#   (a) PSV reliability shrinkage (per-feature lambda, wired into
#       compute_player_psv()/calculate_psv() behind the `psv_reliability_pricing`
#       guard in 10b_export_game_logs.R) repriced psv/osv/dsv — this changes
#       every published season, not just the current one.
#   (b) game-logs must pick up post-#143 xGOT-based GSAA.
# D3 requires ONE rebuild covering ALL currently-shipped seasons before 07c
# regenerates psv_live_constants.csv against the republished logs (K-constant
# calibration order matters — regenerating twice wastes a cycle).
#
# This is NOT the same job as 10b_backfill_game_logs.R (which fills coverage
# gaps for NEW seasons and defaults to an 11-season 2015-2016+ list — it
# predates the 2026-06-20 22-season historical expansion). This driver always
# force-rebuilds, and derives its season list from what's actually on disk
# (see step 2) rather than a second hand-maintained season vector that would
# drift from the first.
#
# Usage (from panna/, once gate #2 — the D1-v2 empirical check in the plan —
# has passed):
#   Rscript data-raw/match-predictions-opta/rebuild_game_logs_reliability.R
# `dry_run` defaults TRUE: prints the resolved (league, season) plan and does
# NOT source 10b or write anything. Set `dry_run <- FALSE` before sourcing
# (or edit the default below) to actually run the rebuild.

# 1. Load package ----

if (!"panna" %in% (.packages())) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(quiet = TRUE)
  } else {
    stop("devtools required. Run: install.packages('devtools')")
  }
}
if (!exists("resolve_blog_leagues", mode = "function")) {
  source(file.path("data-raw", "pipeline_utils.R"))
}
suppressMessages(library(data.table))

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# 2. Config ----

# --- Model overrides (MODELS.md: "How to rebuild game-logs correctly") ---
# NEVER rely on the silent default loader for a game-logs rebuild — the
# local/pannadata fallback copies are the pre-overhaul (over-reactive)
# EPV/WP models and would ship stale values across every rebuilt season.
epv_override_path <- "data-raw/cache/epv/epv_model_xg_clean_full.rds"
wp_override_path  <- "data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds"
if (!file.exists(epv_override_path)) {
  stop("EPV override model missing: ", epv_override_path,
       " -- MODELS.md's canonical clean model is required for a game-logs rebuild.")
}
if (!file.exists(wp_override_path)) {
  stop("WP override model missing: ", wp_override_path,
       " -- MODELS.md's canonical clean model is required for a game-logs rebuild.")
}
epv_model_override <- readRDS(epv_override_path)
wp_model_override  <- readRDS(wp_override_path)
message(sprintf("EPV override: %s (mtime %s)", epv_override_path,
                format(file.info(epv_override_path)$mtime)))
message(sprintf("WP override:  %s (mtime %s)", wp_override_path,
                format(file.info(wp_override_path)$mtime)))

# --- Reliability shrinkage (D1-v2, #158 Rec 2) ---
# 10b_export_game_logs.R's own guard defaults `psv_reliability_pricing` to ON
# (it only disables when the global is explicitly set FALSE) — left unset
# here on purpose so a missing/stale artifact surfaces via 10b's
# load_psv_match_reliability() warning instead of this driver silently
# masking it. We only fail fast if someone explicitly turned it off, since
# shipping the reliability repricing is the entire point of this driver.
if (exists("psv_reliability_pricing", inherits = FALSE) &&
    !isTRUE(psv_reliability_pricing)) {
  stop("psv_reliability_pricing is explicitly disabled in this session, but ",
       "this driver exists to ship the D1-v2 reliability repricing. Unset it ",
       "or set it TRUE before sourcing.")
}
.reliability_artifact <- system.file("extdata", "psv_match_reliability.csv", package = "panna")
if (!nzchar(.reliability_artifact) || !file.exists(.reliability_artifact)) {
  stop("psv_match_reliability.csv not found in the installed/loaded panna package. ",
       "Wave 1 (07b_build_position_means.R) must have built it and the package ",
       "must be re-loaded (devtools::load_all()) before this driver can run.")
}
message(sprintf("Reliability artifact: %s (mtime %s)", .reliability_artifact,
                format(file.info(.reliability_artifact)$mtime)))

# --- Leagues: canonical resolver (constants.R PANNA_LEAGUE_GROUPS), same
# source 10b itself uses — never hand-maintain a parallel list. ---
.blog_league_groups <- resolve_blog_leagues()
blog_leagues         <- .blog_league_groups$blog_leagues
season_label_leagues <- .blog_league_groups$season_label_leagues

# --- Seasons: every season CURRENTLY SHIPPED, discovered from the existing
# game_logs_<season>.parquet files in cache_dir. The only other season list
# in this repo (10b_backfill_game_logs.R's hardcoded 2015-2016..2025-2026
# vector) predates the 2026-06-20 22-season historical expansion (see
# project_complete_gamelogs_2026-06 in memory) — reusing it here would
# silently skip 11 older seasons and leave them on stale pre-D1v2/pre-#143
# PSV/GSAA. Override with an explicit `game_log_seasons` before sourcing to
# rebuild a subset (e.g. while iterating). ---
if (!exists("game_log_seasons", inherits = FALSE)) {
  .existing_files <- list.files(cache_dir, pattern = "^game_logs_\\d{4}-\\d{4}\\.parquet$")
  game_log_seasons <- sort(sub("^game_logs_(.*)\\.parquet$", "\\1", .existing_files))
  if (length(game_log_seasons) == 0) {
    stop("No existing game_logs_<season>.parquet found in ", cache_dir,
         " to derive the season list from. Set game_log_seasons explicitly before sourcing.")
  }
}
game_log_seasons <- sort(as.character(game_log_seasons))
latest_season     <- game_log_seasons[length(game_log_seasons)]

# --- Force rebuild: this is a REPRICE of already-shipped seasons, not a
# coverage backfill, so every season in the list is rewritten regardless of
# whether its parquet already exists. ---
force_rebuild <- TRUE

# --- Upload: OFF by default for this authoring/dry-run pass. Flip to TRUE
# once gate #2 has passed and this is run for real (Wave 2 publishes via the
# registered files, see 10b step 6 / 13_publish_release_data.R). ---
if (!exists("upload_game_logs", inherits = FALSE)) upload_game_logs <- FALSE

# --- use_skill_ratings: mirror 10b_backfill_game_logs.R's TRUE (skill-adjusted
# SPM priors) for parity with the 2026-06-20 historical backfill, rather than
# 10b_export_game_logs.R's own bare default (raw xRAPM via cache-opta). ---
if (!exists("use_skill_ratings", inherits = FALSE)) use_skill_ratings <- TRUE

# --- dry_run: TRUE (default) only prints the resolved plan. ---
if (!exists("dry_run", inherits = FALSE)) dry_run <- TRUE

message(sprintf("\n=== rebuild_game_logs_reliability: %d season(s), %d league(s), dry_run=%s ===",
                length(game_log_seasons), length(blog_leagues), dry_run))
message(sprintf("  Seasons: %s", paste(game_log_seasons, collapse = ", ")))
message(sprintf("  Latest (alias target): %s", latest_season))
message(sprintf("  Upload: %s", if (isTRUE(upload_game_logs)) "yes" else "no"))

# 3. Helpers ----

# Resolve every (league, season) pair exactly the way 10b's own pre-flight
# coverage guard does (resolve_league_season() + season_label_leagues), so
# the printed plan matches what 10b will actually attempt — no separate
# hand-derived applicability logic.
.build_plan <- function(seasons, leagues) {
  rows <- vector("list", length(seasons) * length(leagues))
  i <- 0L
  for (s in seasons) {
    for (lg in leagues) {
      resolved <- tryCatch(
        resolve_league_season(lg, s, tournament_leagues = season_label_leagues),
        error = function(e) NULL
      )
      i <- i + 1L
      rows[[i]] <- data.table::data.table(
        season = s, league = lg,
        resolved_season = if (is.null(resolved)) NA_character_ else resolved,
        included = !is.null(resolved)
      )
    }
  }
  data.table::rbindlist(rows)
}

# Data-sanity header (repo convention: nrow, date range, key-column spread —
# see CLAUDE.md "Always sanity-check data after loading").
.print_data_sanity <- function(dt, label) {
  if (is.null(dt) || nrow(dt) == 0) {
    message(sprintf("    [data-sanity] %s: 0 rows", label))
    return(invisible(NULL))
  }
  date_range <- if ("match_date" %in% names(dt)) {
    sprintf("%s to %s", min(dt$match_date, na.rm = TRUE), max(dt$match_date, na.rm = TRUE))
  } else {
    "n/a (no match_date col)"
  }
  n_leagues <- if ("league" %in% names(dt)) length(unique(dt$league)) else NA_integer_
  n_players <- if ("player_id" %in% names(dt)) length(unique(dt$player_id)) else NA_integer_
  message(sprintf("    [data-sanity] %s: %d rows, %d cols, date range %s, %d leagues, %d players",
                  label, nrow(dt), ncol(dt), date_range, n_leagues, n_players))
}

# One season = one 10b invocation (10b loops leagues internally, with its own
# per-league tryCatch/skip handling). We wrap the whole season call so a
# season-level failure (e.g. upstream data outage) doesn't abort the rest of
# the backfill (fail-fast OFF, per the task).
.run_one_season <- function(season) {
  message(sprintf("\n########## %s ##########", season))
  t0 <- Sys.time()
  status  <- "ok"
  err_msg <- NA_character_

  ok <- tryCatch({
    ge <- globalenv()
    assign("game_log_seasons",  season,                                  envir = ge)
    assign("build_game_logs",   TRUE,                                    envir = ge)
    assign("upload_game_logs",  upload_game_logs,                        envir = ge)
    assign("mirror_alias",      identical(season, latest_season),        envir = ge)
    assign("use_skill_ratings", use_skill_ratings,                       envir = ge)
    assign("cache_dir",         cache_dir,                               envir = ge)
    assign("blog_leagues",      blog_leagues,                            envir = ge)
    # Reprice runs re-process seasons whose PUBLISHED game-logs already ship
    # partial coverage (POR/UEL/NED/BEL/TUR event gaps in 2010-2018 are genuine
    # opta-latest holes — see panna#158 rebuild diagnosis, 2026-07-20). 10b's
    # events-coverage abort (default 20 missing matches) would kill each such
    # season wholesale, so disarm it here: this driver never REDUCES coverage,
    # it reprices what exists. New-coverage builds must not copy this override.
    assign("events_coverage_abort_threshold", Inf,                       envir = ge)
    source("data-raw/match-predictions-opta/10b_export_game_logs.R", local = FALSE)
    TRUE
  }, error = function(e) {
    status  <<- "error"
    err_msg <<- conditionMessage(e)
    message(sprintf("  [%s] FAILED: %s", season, err_msg))
    FALSE
  })

  out_path <- file.path(cache_dir, sprintf("game_logs_%s.parquet", season))
  league_rows <- data.table::data.table(league = character(0), rows = integer(0))
  n_rows <- NA_integer_

  if (isTRUE(ok)) {
    if (file.exists(out_path)) {
      dt <- tryCatch(data.table::as.data.table(arrow::read_parquet(out_path)),
                     error = function(e) NULL)
      if (is.null(dt)) {
        status  <- "error"
        err_msg <- "output parquet unreadable"
      } else {
        n_rows <- nrow(dt)
        .print_data_sanity(dt, sprintf("season %s output", season))
        if ("league" %in% names(dt)) {
          league_rows <- dt[, .(rows = .N), by = league]
        }
      }
    } else {
      status  <- "error"
      err_msg <- "10b completed but output parquet missing"
    }
  }

  elapsed_min <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  message(sprintf("  [%s] status=%s  rows=%s  elapsed=%.1f min",
                  season, status, ifelse(is.na(n_rows), "NA", n_rows), elapsed_min))

  list(season = season, status = status, message = err_msg,
       n_rows = n_rows, elapsed_min = elapsed_min, league_rows = league_rows)
}

# 4. Dry-run plan OR real run ----

if (isTRUE(dry_run)) {
  plan <- .build_plan(game_log_seasons, blog_leagues)
  message(sprintf("\n=== DRY RUN: resolved rebuild plan (%d seasons x %d leagues = %d pairs) ===",
                  length(game_log_seasons), length(blog_leagues), nrow(plan)))
  season_summary <- plan[, .(leagues_included = sum(included),
                             leagues_skipped  = sum(!included)),
                         by = season][order(season)]
  print(season_summary)
  message(sprintf("\nTotal (league, season) pairs: %d included, %d skipped (no tournament that year)",
                  sum(plan$included), sum(!plan$included)))
  message("\nNo 10b sourcing performed (dry_run = TRUE). Set dry_run <- FALSE before")
  message("sourcing this script to run the rebuild for real.")
  rebuild_plan <- plan  # left in globalenv for inspection
} else {
  message(sprintf("\n=== LIVE RUN: rebuilding %d season(s) (force_rebuild = TRUE) ===",
                  length(game_log_seasons)))
  season_results <- lapply(game_log_seasons, .run_one_season)
  names(season_results) <- game_log_seasons

  # 5. Summary table (league, season, rows, status) ----
  summary_dt <- data.table::rbindlist(lapply(season_results, function(r) {
    if (nrow(r$league_rows) == 0) {
      data.table::data.table(season = r$season, league = NA_character_,
                             rows = NA_integer_, status = r$status,
                             message = r$message, elapsed_min = r$elapsed_min)
    } else {
      cbind(r$league_rows[, .(league, rows)], season = r$season,
            status = r$status, message = r$message, elapsed_min = r$elapsed_min)
    }
  }), fill = TRUE)
  data.table::setcolorder(summary_dt, c("league", "season", "rows", "status"))
  data.table::setorder(summary_dt, season, league)

  message("\n========================================")
  message("Rebuild summary (league, season, rows, status)")
  message("========================================")
  print(summary_dt)

  n_season_ok    <- sum(vapply(season_results, function(r) identical(r$status, "ok"), logical(1)))
  n_season_error <- length(season_results) - n_season_ok
  message(sprintf("\nSeasons: %d ok, %d failed (of %d)",
                  n_season_ok, n_season_error, length(season_results)))
  if (n_season_error > 0) {
    failed <- Filter(function(r) !identical(r$status, "ok"), season_results)
    for (r in failed) message(sprintf("  FAILED %s: %s", r$season, r$message))
  }

  summary_path <- file.path(cache_dir, "rebuild_game_logs_reliability_summary.csv")
  data.table::fwrite(summary_dt, summary_path)
  message(sprintf("\nSummary written: %s", summary_path))
}
