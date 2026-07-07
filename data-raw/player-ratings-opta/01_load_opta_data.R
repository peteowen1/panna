# 01_load_opta_data.R
# Load Opta data and score shots with xG model
#
# Loads Opta lineups, events, stats, and xMetrics. Loads SPADL cache,
# scores shots with the pre-trained xG model, detects penalties from
# raw events, and computes match-level xG for splint creation.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Inherit from parent pipeline if available
leagues <- if (exists("leagues")) leagues else c("ENG", "ESP", "GER", "ITA", "FRA")
seasons <- if (exists("seasons")) seasons else NULL
min_season <- if (exists("min_season")) min_season else NULL
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# PENALTY_XG and SPADL_CACHE_DIR are exported from panna::constants.R (loaded via devtools::load_all())

# extract_season_end_year() is defined in R/utils.R

raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
config_path <- file.path(cache_dir, "01_config.rds")

# 3. Check Cache ----

current_config <- list(leagues = leagues, seasons = seasons,
                       min_season = min_season,
                       use_xmetrics_features = use_xmetrics_features)

if (file.exists(raw_data_path) && file.exists(config_path)) {
  cached_config <- readRDS(config_path)
  if (identical(cached_config, current_config)) {
    message("Cache is up to date - skipping data load")
    raw_opta_data <- readRDS(raw_data_path)
    message(sprintf("  Loaded from cache: %d matches, %d lineups",
                    nrow(raw_opta_data$results), nrow(raw_opta_data$lineups)))
    return(invisible(NULL))
  }
  message("Configuration changed - reloading data")
}

# 4. Load xG Model ----

message("\n=== Loading xG Model ===\n")
xg_model <- load_xg_model()

# 5. Load Data by League-Season ----

message("\n=== Loading Opta Data ===\n")

# Memory: stage the 5 large tables (lineups/events/stats/xmetrics/shots) to disk
# per league-season rather than accumulating them in RAM. The old all_* lists held
# ~8GB across 20 leagues AND coexisted with the ~8GB combined_* frames during the
# combine, peaking ~15GB and OOMing the 16GB runner once the new leagues (~+40%
# rows) landed. Staging keeps loop-peak to one league and combine-peak to one
# table at a time. The small per-match tables (period ends / timing / match xG)
# stay in RAM — they're negligible.
stage_dir <- file.path(cache_dir, "01_staging")
unlink(stage_dir, recursive = TRUE, force = TRUE)
.BIG_TABLES <- c("lineups", "events", "stats", "xmetrics", "shots")
for (.t in .BIG_TABLES) dir.create(file.path(stage_dir, .t), recursive = TRUE, showWarnings = FALSE)
.stage_label <- function(label) gsub("[^A-Za-z0-9]+", "_", label)
.stage_write <- function(df, tbl, label) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  arrow::write_parquet(df, file.path(stage_dir, tbl, paste0(.stage_label(label), ".parquet")))
}
# Combine one staged table: read its per-league-season parquets and rbindlist with
# fill=TRUE (identical to the old in-RAM combine), freeing the read list right
# after. Peak = one table (not all five) since the others sit on disk.
# panna#87 flight-recorder fix (run 28881348258 died 68 seconds into
# "Combining Data" with 9.8GB free): the old form held THREE copies of the
# table at peak — `parts` + the rbindlist result + `as.data.frame(<data.table>)`,
# which is a FULL DEEP COPY (the hidden-copy gotcha in CLAUDE.md). setDF()
# converts in place (zero copy), cutting peak to the ~2x rbindlist floor, and
# each table's staged files are deleted right after combining.
.combine_staged <- function(tbl) {
  files <- list.files(file.path(stage_dir, tbl), pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) return(NULL)
  # panna#87 round 2 (attempt-2 flight recorder: the stats combine alone jumped
  # RSS 5GB -> 15,035MB in 15s and died at exit 143; the June cache measures
  # stats at 7.5GB in RAM, so even the 2x rbindlist floor cannot fit). This is
  # a 1x combine that preserves rbindlist(fill=TRUE) semantics EXACTLY:
  #   1. Build a 0-row schema-union TEMPLATE with rbindlist itself over
  #      zero-row heads of every file (arrow pushdown makes head(0) free) —
  #      so column set, order, and type promotion are identical to the old
  #      full rbindlist by construction.
  #   2. Allocate the full all-NA result ONCE by NA-indexing the template
  #      (correct types, 1x the table).
  #   3. Assign each file's rows into place with data.table::set(), freeing
  #      each chunk as it lands. Peak = result + one chunk, not parts + result.
  # File order = row order (same as before); columns absent in a chunk stay
  # NA (= fill=TRUE); a chunk's narrower type upward-coerces into the
  # template's promoted type (same result as rbindlist's promotion).
  template <- data.table::rbindlist(
    lapply(files, function(f) {
      dplyr::collect(utils::head(arrow::open_dataset(f), 0))
    }),
    fill = TRUE, use.names = TRUE
  )
  n_per_file <- vapply(files, function(f) {
    ds <- arrow::open_dataset(f)
    as.integer(ds$num_rows)
  }, integer(1))
  n_total <- sum(n_per_file)
  out <- template[rep(NA_integer_, n_total)]
  pos <- 1L
  for (k in seq_along(files)) {
    n_k <- n_per_file[k]
    if (n_k == 0L) next
    chunk <- arrow::read_parquet(files[k])
    idx <- seq.int(pos, pos + n_k - 1L)
    for (cc in intersect(names(chunk), names(out))) {
      data.table::set(out, i = idx, j = cc, value = chunk[[cc]])
    }
    pos <- pos + n_k
    rm(chunk)
    if (k %% 20L == 0L) gc(verbose = FALSE)
  }
  data.table::setDF(out)
  unlink(file.path(stage_dir, tbl), recursive = TRUE, force = TRUE)
  gc(verbose = FALSE)
  if (exists(".log_rss", mode = "function")) {
    .log_rss(sprintf("combined %s (%s rows)", tbl, format(nrow(out), big.mark = ",")))
  }
  out
}

all_period_ends <- list()  # exact second-precision period boundaries (Opta type_id == 30)
all_player_timing <- list()  # per-(match, player) on/off times derived from chains
all_match_xg <- list()

for (league in leagues) {
  opta_league <- to_opta_league(league)
  available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))

  if (length(available_seasons) == 0) {
    message(sprintf("  No seasons found for %s, skipping", league))
    next
  }

  if (!is.null(seasons)) {
    available_seasons <- intersect(available_seasons, seasons)
  }

  # Filter by min_season using year prefix (handles both "2024-2025" and "2018 Russia")
  if (!is.null(min_season)) {
    min_year <- as.integer(substr(min_season, 1, 4))
    season_years <- as.integer(substr(available_seasons, 1, 4))
    available_seasons <- available_seasons[season_years >= min_year]
  }

  message(sprintf("\n--- %s (%s): %d seasons ---", league, opta_league, length(available_seasons)))

  # OPT #1: Load each table ONCE for the whole league (all seasons) instead of
  # once per (league, season). The old per-season loads opened a fresh DuckDB
  # connection and re-scanned the consolidated parquet ~250x total (5 tables x
  # ~250 league-seasons); loading per-league cuts that ~12x. Each frame carries
  # a `season` column (the WHERE filter the per-season path used), so we slice
  # by season in R below. Frames are freed at the end of the league iteration.
  .slice_season <- function(df, s) {
    if (is.null(df) || !"season" %in% names(df)) return(df)
    df[df$season == s, , drop = FALSE]
  }
  lg_lineups      <- tryCatch(load_opta_lineups(league, season = NULL, source = "local"), error = function(e) NULL)
  lg_events       <- tryCatch(load_opta_events(league, season = NULL, source = "local"),  error = function(e) NULL)
  lg_stats        <- tryCatch(load_opta_stats(league, season = NULL, source = "local"),   error = function(e) NULL)
  lg_xmetrics     <- if (use_xmetrics_features) tryCatch(load_opta_xmetrics(league, season = NULL), error = function(e) NULL) else NULL
  lg_match_events <- tryCatch(load_opta_match_events(league, season = NULL), error = function(e) NULL)

  for (season in available_seasons) {
    label <- paste(league, season)
    message(sprintf("  Loading %s...", label))

    tryCatch({
      # OPT #1: slice the per-league frames by season (was: per-season loads).
      lineups <- .slice_season(lg_lineups, season)
      events  <- .slice_season(lg_events, season)
      stats   <- .slice_season(lg_stats, season)

      if (is.null(lineups) || nrow(lineups) == 0) {
        message(sprintf("    Skipping %s: no lineup data", label))
        next
      }

      lineups$league <- league
      lineups$season <- season
      events$league <- league
      events$season <- season
      stats$league <- league
      stats$season <- season

      .stage_write(lineups, "lineups", label)
      .stage_write(events, "events", label)
      .stage_write(stats, "stats", label)

      # Load xMetrics if enabled (OPT #1: slice the per-league frame)
      if (use_xmetrics_features) {
        xmetrics <- .slice_season(lg_xmetrics, season)
        if (!is.null(xmetrics) && nrow(xmetrics) > 0) {
          xmetrics$league <- league
          xmetrics$season <- season
          .stage_write(xmetrics, "xmetrics", label)
        }
      }

      # Load raw match events. Used for both SPADL building (via
      # get_or_build_spadl) and penalty detection (qualifier 9).
      #
      # source = "remote" matches what 10b/10c do (the working predictions
      # workflow). match_events live as per-league `events_<League>.parquet`
      # files on the opta-latest release — not as a consolidated file — so
      # source = "local" would only work if the workflow reorganised them
      # into the hierarchical layout `load_opta_table` expects. Remote pulls
      # each per-league file once via piggyback and caches it per R session.
      raw_events <- .slice_season(lg_match_events, season)  # OPT #1: slice per-league

      if (is.null(raw_events) || nrow(raw_events) == 0) {
        message(sprintf("    No raw events for %s — skipping shot scoring", label))
        next
      }

      # Build (or load cached) SPADL. Replaces an old file.exists() check that
      # silently degraded the pipeline to 0-xG splints when caches were absent
      # (notably on fresh GHA runners that don't ship SPADL caches alongside
      # opta-latest assets). get_or_build_spadl() rebuilds from raw_events on
      # cache miss; downstream behaviour is identical when a valid cache exists.
      spadl <- tryCatch(
        get_or_build_spadl(raw_events, league, season),
        error = function(e) {
          message(sprintf("    SPADL build failed for %s: %s", label, e$message))
          NULL
        }
      )

      if (is.null(spadl) || nrow(spadl) == 0) {
        message(sprintf("    No SPADL for %s — skipping shot scoring", label))
        next
      }

      {
        # Detect penalties from raw events (qualifier 9)
        # Uses (match_id, player_id, minute, second) composite key to avoid
        # collisions when a player has multiple shots in the same minute
        spadl$is_penalty <- 0L
        if (!is.null(raw_events) && nrow(raw_events) > 0) {
          # Match qualifier 9 as a standalone value in the JSON array.
          # Handles both string ("9") and bare numeric (9) formats.
          # Anchored regex avoids false positives from "19", "90", "109", etc.
          has_pen_qualifier <- grepl(
            '(^|[,\\[])\\s*"?9"?\\s*(,|\\]|$)', raw_events$qualifier_json
          )
          pen_mask <- raw_events$type_id %in% c(13L, 14L, 15L, 16L) & has_pen_qualifier
          if (sum(pen_mask) > 0) {
            raw_second <- if ("second" %in% names(raw_events)) {
              raw_events$second[pen_mask]
            } else {
              message(sprintf("    Note: raw events missing 'second' column for %s %s. Penalty matching may be incomplete.", league, season))
              rep(0L, sum(pen_mask))
            }
            pen_keys <- paste(
              raw_events$match_id[pen_mask],
              raw_events$player_id[pen_mask],
              raw_events$minute[pen_mask],
              raw_second,
              sep = "_"
            )
            spadl_shot_idx <- which(spadl$action_type == "shot")
            if (length(spadl_shot_idx) > 0) {
              spadl_keys <- paste(
                spadl$match_id[spadl_shot_idx],
                spadl$player_id[spadl_shot_idx],
                floor(spadl$time_seconds[spadl_shot_idx] / 60),
                floor(spadl$time_seconds[spadl_shot_idx] %% 60),
                sep = "_"
              )
              spadl$is_penalty[spadl_shot_idx[spadl_keys %in% pen_keys]] <- 1L
            }
          }
        }

        # Capture exact period-end times from raw events (type_id == 30
        # marker; second-level precision). Splint creation uses these to
        # avoid the historical "+0.5 min buffer" hack that produced
        # spurious 0.5-minute stoppage-time splints.
        if (!is.null(raw_events) && nrow(raw_events) > 0) {
          all_period_ends[[label]] <- extract_period_end_times(raw_events)

          # Derive per-player on/off times entirely from chain data:
          # starters from type_id == 34 squad events, sub events from type_id
          # 18/19, red cards from type_id 17 + qualifier 33/14, match end from
          # type_id == 30. Every time has second-precision. This replaces the
          # lineup-derived on/off (which records 90 min for finishers regardless
          # of stoppage time and rounds sub minutes to integer).
          all_player_timing[[label]] <- extract_player_timing_from_events(raw_events)
        }

        # Score shots with xG model
        spadl <- add_xg_to_spadl(spadl, xg_model)

        # Override penalty xG
        penalty_idx <- spadl$action_type == "shot" & spadl$is_penalty == 1L
        if (sum(penalty_idx) > 0) {
          spadl$xg[penalty_idx] <- PENALTY_XG
        }

        # Extract shots for splint pipeline
        shots_df <- extract_shots_from_spadl(spadl, lineups)
        shots_df$league <- league
        shots_df$season <- season
        .stage_write(shots_df, "shots", label)

        # Compute match-level xG
        # Non-penalty xG per team per match
        np_shots <- shots_df[!shots_df$is_penalty, ]

        if (nrow(np_shots) > 0) {
          # Build team-to-home lookup from lineups
          home_teams <- lineups %>%
            filter(tolower(team_position) == "home") %>%
            distinct(match_id, team_name) %>%
            rename(home_team = team_name)

          match_team_xg <- np_shots %>%
            group_by(match_id, team) %>%
            summarise(team_npxg = sum(xg, na.rm = TRUE), .groups = "drop")

          # Also total xG (with penalties)
          match_team_xg_total <- shots_df %>%
            group_by(match_id, team) %>%
            summarise(team_xg = sum(xg, na.rm = TRUE), .groups = "drop")

          match_team_xg <- match_team_xg %>%
            left_join(match_team_xg_total, by = c("match_id", "team")) %>%
            left_join(home_teams, by = "match_id") %>%
            mutate(is_home = team == home_team)

          home_xg <- match_team_xg %>%
            filter(is_home) %>%
            select(match_id, home_xg = team_xg, home_npxg = team_npxg)

          away_xg <- match_team_xg %>%
            filter(!is_home) %>%
            select(match_id, away_xg = team_xg, away_npxg = team_npxg)

          match_xg <- home_xg %>%
            full_join(away_xg, by = "match_id") %>%
            mutate(
              home_xg = coalesce(home_xg, 0),
              away_xg = coalesce(away_xg, 0),
              home_npxg = coalesce(home_npxg, 0),
              away_npxg = coalesce(away_npxg, 0)
            )

          all_match_xg[[label]] <- match_xg
        }

        message(sprintf("    SPADL: %d shots scored, %d penalties",
                        nrow(shots_df), sum(shots_df$is_penalty)))
      }

    }, error = function(e) {
      # Log the failing call too: ~49 league-seasons hit "non-character argument"
      # in the 2026-06 rerun and got silently dropped (no xG -> RAPM drops them).
      # conditionCall pinpoints which function so we can fix the root cause.
      cc <- tryCatch(deparse(conditionCall(e))[1], error = function(...) "<no call>")
      message(sprintf("    ERROR in %s: %s  [call: %s]", label, conditionMessage(e), cc))
    })
  }

  # OPT #1: free this league's full-season frames before the next league.
  rm(lg_lineups, lg_events, lg_stats, lg_xmetrics, lg_match_events)
  gc(verbose = FALSE)
}

# 6. Combine All Data ----

message("\n=== Combining Data ===\n")

# Free each accumulator list IMMEDIATELY after combining it. Otherwise the
# all_* lists (~8GB across 20 leagues) stay resident alongside the combined_*
# frames (~8GB) through the results table + save, peaking ~15GB and OOMing the
# 16GB runner. The combined data is only ~8GB; the duplication was the problem.
# data.table::rbindlist instead of dplyr::bind_rows -- rbindlist combines the
# per-league-season frames with far less copying than bind_rows (the stats
# table alone is 2.4M x 288 cols), then as.data.frame() restores the exact
# data.frame output type the rest of step 01 + step 02 expect. Output is
# identical; this is purely a memory/speed win on the combine.
# Combine each big table from disk staging, one at a time (peak = one table's
# ~2x rbindlist floor on top of the already-combined frames). Order matters:
# combine the BIGGEST tables FIRST while baseline RAM is lowest — every
# combined frame stays resident, so a big table combined last pays its 2x
# transient on top of everything already combined. match_xg (small, in-RAM)
# goes first so the all_* accumulator frees before the heavy lifting.
combined_match_xg <- if (length(all_match_xg) > 0) {
  mx <- data.table::rbindlist(all_match_xg, fill = TRUE, use.names = TRUE)
  data.table::setDF(mx)
  mx
} else NULL
rm(all_match_xg); gc(verbose = FALSE)
combined_stats    <- .combine_staged("stats")     # widest (~288 cols) first
combined_events   <- .combine_staged("events")
combined_lineups  <- .combine_staged("lineups")
combined_xmetrics <- .combine_staged("xmetrics")
combined_shots    <- .combine_staged("shots")
unlink(stage_dir, recursive = TRUE, force = TRUE)  # free any remaining staging
gc(verbose = FALSE)

# Data scale validation: catch partial/empty loads early
if (is.null(combined_lineups) || nrow(combined_lineups) == 0) {
  stop("No lineup data loaded for any league-season. Check data availability and source paths.")
}

# Fail fast at step 01 if SPADL produced zero shots across the entire run.
# Otherwise the failure surfaces four steps downstream when filter_bad_xg_data
# drops every splint as 100% zero-xG, with the actual root cause buried in
# a per-season "No SPADL ..." message.
if (is.null(combined_shots) || nrow(combined_shots) == 0) {
  stop("No SPADL shots loaded for any league-season. ",
       "Splints would have zero xG and step 04 would drop everything. ",
       "Verify opta_match_events.parquet is downloaded and SPADL building succeeds.",
       call. = FALSE)
}
n_leagues_loaded <- length(unique(combined_lineups$league))
n_matches_loaded <- length(unique(combined_lineups$match_id))
if (n_leagues_loaded < length(leagues)) {
  warning(sprintf("Expected %d leagues but only loaded %d: %s",
                  length(leagues), n_leagues_loaded,
                  paste(unique(combined_lineups$league), collapse = ", ")),
          call. = FALSE)
}
if (n_matches_loaded < 100 && length(leagues) >= 5) {
  warning(sprintf("Only %d matches loaded for %d leagues. Expected thousands. Check data availability.",
                  n_matches_loaded, n_leagues_loaded),
          call. = FALSE)
}

message(sprintf("  Lineups: %d rows (%d leagues, %d matches)",
                nrow(combined_lineups), n_leagues_loaded, n_matches_loaded))
message(sprintf("  Events: %d rows", nrow(combined_events)))
message(sprintf("  Stats: %d rows", nrow(combined_stats)))
message(sprintf("  xMetrics: %s rows",
                if (!is.null(combined_xmetrics)) nrow(combined_xmetrics) else "N/A"))
message(sprintf("  Shots (SPADL): %s rows",
                if (!is.null(combined_shots)) nrow(combined_shots) else "N/A"))
message(sprintf("  Match xG: %s matches",
                if (!is.null(combined_match_xg)) nrow(combined_match_xg) else "N/A"))

# 7. Create Results Table ----

message("\n=== Creating Results Table ===\n")

# Build match info from lineups (home/away teams). OPT #5: data.table aggregation
# on the 2.4M-row lineups (was dplyr group_by/summarise). `keyby` sorts groups to
# match dplyr's sorted output; `x[cond][1]` == dplyr `first(x[cond])`.
match_info <- as.data.frame(
  data.table::as.data.table(combined_lineups)[is_starter == TRUE, .(
    home_team    = team_name[tolower(team_position) == "home"][1],
    away_team    = team_name[tolower(team_position) == "away"][1],
    match_date   = match_date[1],
    home_team_id = team_id[tolower(team_position) == "home"][1],
    away_team_id = team_id[tolower(team_position) == "away"][1]
  ), keyby = .(match_id, league, season)]
)

# Derive match scores from goal events. OPT #5: data.table aggregation on the
# ~1M-row events table (was dplyr group_by/summarise).
goal_counts <- as.data.frame(
  data.table::as.data.table(combined_events)[event_type == "goal",
    .(goals = .N), keyby = .(match_id, team_id)]
)

# Drop matches that exist in lineups but have no events at all. This is a
# scraper-gap case — Opta publishes lineups ahead of events, so the match
# looks "played" but we can't compute a score. Without this guard,
# coalesce(goals, 0L) silently records them as 0-0, injecting phantom
# draws into standings and downstream ratings.
matches_with_events <- unique(combined_events$match_id)
matches_without_events <- setdiff(match_info$match_id, matches_with_events)
if (length(matches_without_events) > 0) {
  warning(sprintf(
    "%d match_ids have lineups but no events — dropping from results (likely scraper gap). First 5: %s",
    length(matches_without_events),
    paste(head(matches_without_events, 5), collapse = ", ")
  ), call. = FALSE)
  match_info <- match_info %>% filter(!match_id %in% matches_without_events)
}

home_goals <- match_info %>%
  select(match_id, home_team_id) %>%
  left_join(goal_counts, by = c("match_id", "home_team_id" = "team_id")) %>%
  mutate(home_goals = coalesce(goals, 0L)) %>%
  select(match_id, home_goals)

away_goals <- match_info %>%
  select(match_id, away_team_id) %>%
  left_join(goal_counts, by = c("match_id", "away_team_id" = "team_id")) %>%
  mutate(away_goals = coalesce(goals, 0L)) %>%
  select(match_id, away_goals)

results <- match_info %>%
  left_join(home_goals, by = "match_id") %>%
  left_join(away_goals, by = "match_id") %>%
  select(-home_team_id, -away_team_id) %>%
  mutate(
    season_end_year = sapply(season, extract_season_end_year)
  )

# Join match-level xG
if (!is.null(combined_match_xg)) {
  results <- results %>%
    left_join(combined_match_xg, by = "match_id")
}

# Join precomputed second-level period-end times (Opta type_id == 30 markers).
# Splint creation reads first_half_end_time / match_end_time from results
# to set exact period boundaries. Matches with no markers get NA and the
# splint pipeline falls back to last-event time.
combined_period_ends <- if (length(all_period_ends) > 0) as.data.frame(data.table::rbindlist(all_period_ends, fill = TRUE, use.names = TRUE)) else NULL
rm(all_period_ends)
if (!is.null(combined_period_ends) && nrow(combined_period_ends) > 0) {
  results <- results %>%
    left_join(combined_period_ends, by = "match_id")
  message(sprintf("  Period-end markers attached: %d / %d matches (%.1f%%)",
                  sum(!is.na(results$match_end_time)),
                  nrow(results),
                  100 * sum(!is.na(results$match_end_time)) / nrow(results)))
}

# Combine chain-derived player timing across all leagues/seasons.
# This becomes the authoritative source of on_minute/off_minute for splint
# creation; lineups are kept only for player_name + metadata.
combined_player_timing <- if (length(all_player_timing) > 0) as.data.frame(data.table::rbindlist(all_player_timing, fill = TRUE, use.names = TRUE)) else NULL
rm(all_player_timing)
if (!is.null(combined_player_timing) && nrow(combined_player_timing) > 0) {
  message(sprintf("  Chain-derived player timing rows: %d (across %d matches)",
                  nrow(combined_player_timing),
                  length(unique(combined_player_timing$match_id))))
}

# Fill missing xG with 0
if (!"home_xg" %in% names(results)) results$home_xg <- NA_real_
if (!"away_xg" %in% names(results)) results$away_xg <- NA_real_
if (!"home_npxg" %in% names(results)) results$home_npxg <- NA_real_
if (!"away_npxg" %in% names(results)) results$away_npxg <- NA_real_

message(sprintf("  Results: %d matches, %d with xG",
                nrow(results), sum(!is.na(results$home_xg))))

# 8. Save ----

raw_opta_data <- list(
  results = results,
  lineups = combined_lineups,
  events = combined_events,
  stats = combined_stats,
  xmetrics = combined_xmetrics,
  shooting = combined_shots,
  match_xg = combined_match_xg,
  player_timing = combined_player_timing  # chain-derived on/off (second precision)
)

saveRDS(raw_opta_data, raw_data_path)
saveRDS(current_config, config_path)

# Validate critical outputs
validate_step_output(results, step_name = "01_load: results", min_rows = 100,
                     warn_below = 10000)
validate_step_output(combined_lineups, step_name = "01_load: lineups", min_rows = 100,
                     warn_below = 50000)

# 9. Summary ----

message("\n========================================")
message("Opta data loading complete!")
message("========================================")
message(sprintf("Matches: %d", nrow(results)))
message(sprintf("Lineups: %d", nrow(combined_lineups)))
message(sprintf("Stats: %d", nrow(combined_stats)))
message(sprintf("Shots (SPADL xG): %s",
                if (!is.null(combined_shots)) nrow(combined_shots) else "N/A"))

message("\nLeague breakdown:")
league_counts <- table(results$league)
for (l in names(league_counts)) {
  message(sprintf("  %s: %d matches", l, league_counts[l]))
}

message("\nSeason breakdown:")
season_counts <- table(results$season)
for (s in names(sort(unique(results$season)))) {
  message(sprintf("  %s: %d matches", s, season_counts[s]))
}

message(sprintf("\nSaved to: %s", raw_data_path))
