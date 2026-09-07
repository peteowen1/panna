# 10c_export_equity.R
# Export per-action EPV credit for the blog match-events page
#
# Produces action_equity_<season>.parquet — one parquet per season — with a
# slim lookup of (match_id, event_id, epv_credit) for every SPADL action. The
# pannadata chain builder joins this onto chain parquets as the `epv_credit`
# column for per-action visualisation.
#
# The column is `epv_credit` (renamed from `equity` 2026-06-03): it holds
# per-action player CREDIT (sum it; never diff it), distinct from the worker's
# `equity` field which is an EPV STATE. The old name collided with that state
# meaning. The file name (action_equity.parquet) is unchanged.
#
# Default: current season only (weekly predictions pipeline). For historical
# backfill, set `equity_seasons <- c("2015-2016", ..., "2025-2026")` before
# sourcing — see 10c_backfill_action_equity.R.
#
# Pipeline per season:
#   events → SPADL (cached) → chains → EPV → credit → (match_id, event_id, epv_credit)
#
# Cup competitions (UCL/UEL/UECL and WC/EURO where available) are included
# via resolve_league_season(), matching 10b_export_game_logs.R.

# 1. Configuration ----

# When sourced standalone (outside run_predictions_opta.R) pipeline_utils.R
# isn't loaded yet — source it here so resolve_blog_leagues() is available
# regardless of entry point (direct Rscript, 10c_backfill_action_equity.R, or
# the full pipeline).
if (!exists("resolve_blog_leagues", mode = "function")) {
  source(file.path("data-raw", "pipeline_utils.R"))
}

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

# Groups come from resolve_blog_leagues() (pipeline_utils.R), backed by the
# shared canonical constant (constants.R: PANNA_LEAGUE_GROUPS), so this can't
# drift from 10b_export_game_logs.R's league set (H-DRIFT, 2026-07-08 review —
# this list previously hardcoded 10 domestic leagues and no calendar leagues,
# missing MLS/ARG/BRA/SAU/AUS/CAFCL that 10b covers).
.blog_league_groups <- resolve_blog_leagues()
domestic_leagues     <- .blog_league_groups$domestic_leagues
calendar_leagues     <- .blog_league_groups$calendar_leagues    # calendar-year season labels
continental_cups     <- .blog_league_groups$continental_cups
intl_tournaments     <- .blog_league_groups$intl_tournaments
# Leagues whose season label is resolved by year prefix rather than passed through
season_label_leagues <- .blog_league_groups$season_label_leagues
if (!exists("blog_leagues", inherits = FALSE)) {
  blog_leagues <- .blog_league_groups$blog_leagues
}

# Seasons to export. Vector (new) or scalar `game_log_season` (back-compat
# with the previous single-season behavior).
if (!exists("equity_seasons", inherits = FALSE)) {
  if (exists("game_log_season", inherits = FALSE)) {
    equity_seasons <- game_log_season
  } else {
    # Derived from the clock, never pinned — the same time bomb that emptied
    # the blog's Player Stats page in August 2026 when 10b's pin went stale.
    # See current_domestic_season() in pipeline_utils.R.
    equity_seasons <- current_domestic_season()
  }
}
equity_seasons <- as.character(equity_seasons)

# The "current" season is mirrored to action_equity.parquet so the blog
# chain builder's name-pinned download keeps working unchanged.
current_season_alias <- sort(equity_seasons, decreasing = TRUE)[1]

# Upload toggle
if (!exists("upload_equity", inherits = FALSE)) upload_equity <- TRUE

# Build toggle — FALSE = skip per-season build, just do alias+upload
if (!exists("build_equity", inherits = FALSE)) build_equity <- TRUE

# Alias toggle — mirror most-recent processed season to action_equity.parquet.
# Set FALSE when back-filling a non-current historical subset to avoid
# clobbering the blog chain builder's current-season pointer.
if (!exists("mirror_alias", inherits = FALSE)) mirror_alias <- TRUE

# Subset-league backfill: MERGE the processed leagues into each existing
# action_equity_<season>.parquet instead of clobbering it. Set TRUE when
# running a league SUBSET (e.g. backfilling WC historical tournaments into an
# era file that already holds other leagues' rows for that season). Mirrors
# 10b_export_game_logs.R's merge_subset_leagues, but keyed on `match_id`
# rather than `league` — this slim equity lookup (match_id, event_id,
# epv_credit) carries no league column, and match_id is already unique per
# match across every competition, so dropping existing rows whose match_id
# belongs to the just-rebuilt league(s) and re-appending is equivalent and
# needs no schema change. Idempotent (drops + re-appends the rebuilt
# league's matches).
if (!exists("merge_subset_leagues", inherits = FALSE)) merge_subset_leagues <- FALSE

message(sprintf("\n=== Building Action Equity: %d season(s) ===",
                length(equity_seasons)))
message(sprintf("  Seasons: %s", paste(equity_seasons, collapse = ", ")))
message(sprintf("  Alias (action_equity.parquet) → %s", current_season_alias))

# 2. Load models (once across all seasons) ----

# EPV override (set by a driver to score with a candidate model, e.g. after a
# model swap when the published model may be stale in the local pannamodels
# cache). Mirrors 10b_export_game_logs.R.
epv_model   <- if (exists("epv_model_override")) epv_model_override else load_epv_model()
xpass_model <- load_xpass_model()

# Typed "skip this league" signal. Caught in the per-league tryCatch via
# `panna_skip_league =` handler; replaces a magic-string match on e$message.
skip_league_cond <- function(reason) {
  structure(
    class = c("panna_skip_league", "error", "condition"),
    list(message = sprintf("skip_league: %s", reason),
         reason  = reason)
  )
}

# Minimum columns the slim equity lookup must emit. Catches drift in the
# SPADL credit assignment before we ship a malformed parquet.
.required_equity_cols <- c("match_id", "event_id", "epv_credit")

validate_equity_schema <- function(dt, league, season) {
  missing <- setdiff(.required_equity_cols, names(dt))
  if (length(missing) > 0L) {
    stop(sprintf(
      "[%s %s] equity missing required columns: %s",
      league, season, paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(dt)
}

# 3. Per-season processing ----

.process_equity_season <- function(season) {
  message(sprintf("\n########## EQUITY %s ##########", season))
  season_equity <- list()

  for (league in blog_leagues) {
    tryCatch({
      league_season <- resolve_league_season(league, season,
                                              tournament_leagues = season_label_leagues)
      if (is.null(league_season)) {
        message(sprintf("  Skipping %s %s — no tournament this year", league, season))
        stop(skip_league_cond("no tournament this year"))
      }
      label <- if (identical(league_season, season)) league else
               sprintf("%s (%s)", league, league_season)
      message(sprintf("  Processing %s %s...", label, season))

      events <- load_opta_match_events(league, season = league_season)
      if (is.null(events) || nrow(events) < 100) {
        message(sprintf("    Skipping %s — insufficient data", league))
        stop(skip_league_cond("insufficient data"))
      }

      n_matches <- length(unique(events$match_id))

      # SPADL from shared disk cache (populated by 10b, xMetrics, etc).
      # Cache key uses `league_season` so WC 2014 and WC 2018 don't collide.
      spadl <- get_or_build_spadl(events, league, league_season)
      spadl_chains   <- create_possession_chains(spadl)
      chain_outcomes <- classify_chain_outcomes(spadl_chains)
      chain_outcomes <- add_next_chain_outcome(chain_outcomes)
      spadl_labeled  <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
      spadl_labeled  <- create_next_goal_labels(spadl_labeled)

      # EPV credit — features built internally by calculate_action_epv.
      spadl_epv    <- calculate_action_epv(spadl_labeled, features = NULL,
                                           epv_model, league = league)
      spadl_credit <- assign_epv_credit(spadl_epv, xpass_model)

      # Slim equity lookup — drop rows without an original_event_id
      # (synthetic SPADL actions like merged duels).
      dt <- data.table::as.data.table(spadl_credit)
      equity <- dt[, .(
        match_id   = match_id,
        event_id   = original_event_id,
        epv_credit = round(player_credit, 4)   # per-action credit (sum, don't diff)
      )]
      equity <- equity[!is.na(event_id) & event_id != ""]

      validate_equity_schema(equity, league, season)

      season_equity[[league]] <- equity
      message(sprintf("    %d matches, %d actions with equity",
                      n_matches, nrow(equity)))

      rm(events, spadl, spadl_chains, chain_outcomes, spadl_labeled,
         spadl_epv, spadl_credit, dt, equity)
      gc(verbose = FALSE)

    },
    panna_skip_league = function(e) {
      # Intentional skip — already messaged at the source.
      invisible(NULL)
    },
    error = function(e) {
      # Tight match on typed errors + explicit data-absence phrasings.
      # Generic "not found" is too broad — it matches unrelated column lookup
      # bugs, silently swallowing real errors as if they were missing data.
      is_data_error <- inherits(e, "panna_data_not_found") ||
        grepl("^No data found for|^No .+ data available", e$message)
      if (is_data_error) {
        message(sprintf("    Skipping %s %s — data not available", league, season))
      } else {
        # Print to stderr immediately so errors surface in real time rather
        # than being deferred as warnings until script end.
        message(sprintf("    ERROR %s %s: %s", league, season, e$message))
        warning(sprintf("ERROR processing %s %s: %s", league, season, e$message),
                call. = FALSE)
      }
    })
  }

  if (length(season_equity) == 0) {
    warning(sprintf("No equity data produced for season %s — skipping", season),
            call. = FALSE)
    return(NULL)
  }

  action_equity <- data.table::rbindlist(season_equity, fill = TRUE)
  message(sprintf("\n  [%s] Combined: %d actions across %d leagues",
                  season, nrow(action_equity), length(season_equity)))

  out_path <- file.path(cache_dir, sprintf("action_equity_%s.parquet", season))
  # Subset-league backfill: merge into the existing per-season file rather
  # than clobbering it (which would delete every other league's rows for the
  # season). Idempotent: drop existing rows whose match_id belongs to the
  # league(s) we just rebuilt, then append.
  if (isTRUE(merge_subset_leagues) && file.exists(out_path)) {
    existing <- data.table::as.data.table(arrow::read_parquet(out_path))
    # Release arrow's memory-mapped file handle before overwriting the same
    # path — Windows error 1224 ("user-mapped section open") otherwise.
    gc()
    rebuilt_match_ids <- unique(action_equity$match_id)
    kept <- existing[!match_id %in% rebuilt_match_ids]
    n_kept <- nrow(kept)
    n_dropped <- nrow(existing) - n_kept
    action_equity <- data.table::rbindlist(list(kept, action_equity), fill = TRUE, use.names = TRUE)
    rm(existing, kept); gc()
    message(sprintf("  [%s] Merge: kept %d existing rows (replaced %d matching rebuilt match_ids), total %d",
                    season, n_kept, n_dropped, nrow(action_equity)))
  }
  # Write atomically via a temp file then replace, so a write failure can
  # never corrupt the existing per-season parquet (which may hold every
  # other league once merge_subset_leagues is used).
  out_tmp <- paste0(out_path, ".tmp")
  arrow::write_parquet(action_equity, out_tmp)
  if (file.exists(out_path)) file.remove(out_path)
  file.rename(out_tmp, out_path)
  message(sprintf("  [%s] Written: %s (%.1f MB)",
                  season, out_path, file.size(out_path) / (1024 * 1024)))

  rm(action_equity, season_equity); gc(verbose = FALSE)
  out_path
}

# 4. Process each season ----

season_paths <- list()
if (isTRUE(build_equity)) {
  for (s in equity_seasons) {
    p <- tryCatch(.process_equity_season(s), error = function(e) {
      warning(sprintf("Equity season %s aborted: %s", s, e$message), call. = FALSE)
      NULL
    })
    if (!is.null(p)) season_paths[[s]] <- p
  }
} else {
  # Upload-only mode: reconstruct paths from existing files.
  for (s in equity_seasons) {
    p <- file.path(cache_dir, sprintf("action_equity_%s.parquet", s))
    if (file.exists(p)) season_paths[[s]] <- p
  }
  message(sprintf("Upload-only mode: %d existing equity parquet(s) found",
                  length(season_paths)))
}

if (length(season_paths) == 0) {
  stop("No equity data produced. Check that events are available.")
}

# 5. Mirror current-season alias → action_equity.parquet ----

alias_src  <- file.path(cache_dir, sprintf("action_equity_%s.parquet", current_season_alias))
alias_path <- file.path(cache_dir, "action_equity.parquet")
if (isTRUE(mirror_alias) && file.exists(alias_src)) {
  file.copy(alias_src, alias_path, overwrite = TRUE)
  message(sprintf("\n  Mirrored alias: %s → action_equity.parquet",
                  basename(alias_src)))
} else if (!isTRUE(mirror_alias)) {
  message("\n  Skipping alias mirror (mirror_alias = FALSE) — keeping existing action_equity.parquet")
}

# 6. Register for step-13 publish (PA5/H-TORN: no upload here) ----

if (isTRUE(upload_equity)) {
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
}

# 7. Summary ----

message("\n========================================")
message("Action equity exported successfully!")
message("========================================")
for (s in names(season_paths)) {
  fi <- file.info(season_paths[[s]])
  message(sprintf("  %s  %s  (%.1f MB)",
                  s, season_paths[[s]], fi$size / (1024 * 1024)))
}
if (isTRUE(upload_equity)) {
  message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
}
