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

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

# Groups come from the shared canonical constant (constants.R: PANNA_LEAGUE_GROUPS),
# so this can't drift from 10b_export_game_logs.R's league set (H-DRIFT,
# 2026-07-08 review — this list previously hardcoded 10 domestic leagues and
# no calendar leagues, missing MLS/ARG/BRA/SAU/AUS/CAFCL that 10b covers).
domestic_leagues  <- PANNA_LEAGUE_GROUPS$domestic
calendar_leagues  <- PANNA_LEAGUE_GROUPS$calendar    # calendar-year season labels
continental_cups  <- PANNA_LEAGUE_GROUPS$continental
intl_tournaments  <- PANNA_LEAGUE_GROUPS$intl
# Leagues whose season label is resolved by year prefix rather than passed through
season_label_leagues <- c(intl_tournaments, calendar_leagues)
if (!exists("blog_leagues", inherits = FALSE)) {
  blog_leagues    <- c(domestic_leagues, calendar_leagues,
                       continental_cups, intl_tournaments)
}

# Seasons to export. Vector (new) or scalar `game_log_season` (back-compat
# with the previous single-season behavior).
if (!exists("equity_seasons", inherits = FALSE)) {
  if (exists("game_log_season", inherits = FALSE)) {
    equity_seasons <- game_log_season
  } else {
    equity_seasons <- "2025-2026"
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
  arrow::write_parquet(action_equity, out_path)
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

# 6. Upload to GitHub Releases ----

if (isTRUE(upload_equity)) {
  message("\n=== Uploading equity to GitHub ===\n")

  gh_check <- tryCatch(
    system2("gh", "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  if (is.null(gh_check)) {
    stop("'gh' CLI is not installed or not on PATH.")
  }

  candidates <- if (isTRUE(mirror_alias)) {
    unique(c(unlist(season_paths), alias_path))
  } else {
    unlist(season_paths)
  }
  files_to_upload <- candidates[file.exists(candidates)]

  for (f in files_to_upload) {
    message(sprintf("  Uploading %s...", basename(f)))
    result <- system2("gh", c("release", "upload", tag, shQuote(f),
                               "--repo", repo, "--clobber"),
                      stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
      stop(sprintf("Failed to upload %s: %s", basename(f),
                   paste(result, collapse = "\n")))
    }
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
