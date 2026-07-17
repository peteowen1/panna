# 03_splint_creation.R
# Create splints (time segments between subs/goals) for Opta RAPM analysis
#
# Near-identical to FBref version. Calls create_all_splints() on
# Opta processed data.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")

processed_data_path <- file.path(cache_dir, "02_processed_data.rds")
splint_data_path <- file.path(cache_dir, "03_splints.rds")

# Boundary-merge minimum: any sub/goal/red within MIN_SPLINT_DURATION minutes
# of the most recently kept boundary is dropped. Players who came on/off
# inside such a removed boundary are still credited via fractional `share`
# in the design matrix. Halves and match start/end are hard boundaries
# (always kept regardless). 5 minutes is conservative — see panna/CLAUDE.md.
if (!exists("MIN_SPLINT_DURATION", inherits = FALSE)) MIN_SPLINT_DURATION <- 5

# 3. Create Splints ----
#
# Build splints per-league with intermediate disk writes. The previous code
# called create_all_splints(chunk_by = "league") which chunks by league
# internally but accumulates all chunks in a list before rbinding — peak
# memory = sum of all leagues' splints + processed_data. With shots now
# populated by the SPADL fix in step 01, that pushed standard GHA runners
# (7GB) into OOM at iteration ~1-2 of step 03 (silently — manifests as
# "operation cancelled" in the GHA log with no preceding error).
#
# This streaming version writes each league's splints to a per-league RDS
# in a chunks dir, frees memory, then combines at the end. Peak memory
# drops to processed_data + ONE league's splints. cleanup at end.
.build_splints_streaming <- function(processed_data, chunks_dir,
                                      min_splint_duration) {
  if (!"league" %in% names(processed_data$results)) {
    # No league column — fall back to single-pass (matches old non-chunked path).
    return(create_all_splints(processed_data, include_goals = TRUE,
                              verbose = TRUE, chunk_by = "none",
                              min_splint_duration = min_splint_duration))
  }

  leagues <- unique(processed_data$results$league)
  n_matches_total <- length(unique(processed_data$results$match_id))
  message(sprintf(
    "Streaming splint creation: %d matches across %d leagues (per-league disk writes)",
    n_matches_total, length(leagues)))

  unlink(chunks_dir, recursive = TRUE, force = TRUE)
  dir.create(chunks_dir, recursive = TRUE)

  for (li in seq_along(leagues)) {
    lg <- leagues[li]
    message(sprintf("  League %d/%d: %s", li, length(leagues), lg))

    league_match_ids <- processed_data$results$match_id[
      processed_data$results$league == lg]

    chunk_data <- list(
      lineups = processed_data$lineups[
        processed_data$lineups$match_id %in% league_match_ids, , drop = FALSE],
      shooting = processed_data$shooting[
        processed_data$shooting$match_id %in% league_match_ids, , drop = FALSE],
      results = processed_data$results[
        processed_data$results$match_id %in% league_match_ids, , drop = FALSE],
      events = if (!is.null(processed_data$events))
        processed_data$events[
          processed_data$events$match_id %in% league_match_ids, , drop = FALSE]
        else NULL,
      stats_summary = if (!is.null(processed_data$stats_summary))
        processed_data$stats_summary[
          processed_data$stats_summary$match_id %in% league_match_ids, , drop = FALSE]
        else NULL
    )

    league_splints <- create_all_splints(
      chunk_data, include_goals = TRUE, verbose = FALSE,
      chunk_by = "none", min_splint_duration = min_splint_duration)

    saveRDS(league_splints, file.path(chunks_dir, sprintf("%s.rds", lg)))
    rm(chunk_data, league_splints); gc(verbose = FALSE)
  }

  # Combine per-league chunks. Read sequentially with rbindlist incremental
  # accumulation rather than holding all in memory at once.
  message("  Combining per-league chunks...")
  chunk_files <- list.files(chunks_dir, pattern = "\\.rds$",
                             full.names = TRUE)
  splints_list <- vector("list", length(chunk_files))
  players_list <- vector("list", length(chunk_files))
  match_info_list <- vector("list", length(chunk_files))
  for (i in seq_along(chunk_files)) {
    one <- readRDS(chunk_files[i])
    splints_list[[i]]    <- one$splints
    players_list[[i]]    <- one$players
    match_info_list[[i]] <- one$match_info
    rm(one)
  }
  combined <- list(
    splints    = as.data.frame(data.table::rbindlist(splints_list, fill = TRUE, use.names = TRUE)),
    players    = as.data.frame(data.table::rbindlist(players_list, fill = TRUE, use.names = TRUE)),
    match_info = as.data.frame(data.table::rbindlist(match_info_list, fill = TRUE, use.names = TRUE))
  )
  rm(splints_list, players_list, match_info_list); gc(verbose = FALSE)

  unlink(chunks_dir, recursive = TRUE, force = TRUE)
  message(sprintf("Created %d splints from %d matches",
                  nrow(combined$splints), n_matches_total))
  combined
}

# Build splints by reading the per-league processed slices written by step 02,
# ONE league resident at a time. Identical output to .build_splints_streaming()
# (same combined splints), but it never loads the full multi-GB processed_data
# -- the splint_creation OOM fix. Peak RAM = one league's slice + the (small,
# aggregated) splint chunks, instead of all ~62.8K matches' raw events at once.
.build_splints_from_league_dir <- function(leagues_dir, chunks_dir,
                                            min_splint_duration) {
  league_files <- list.files(leagues_dir, pattern = "\\.rds$", full.names = TRUE)
  message(sprintf(
    "Streaming splint creation from %d per-league files (1 league resident at a time)",
    length(league_files)))

  unlink(chunks_dir, recursive = TRUE, force = TRUE)
  dir.create(chunks_dir, recursive = TRUE)

  for (i in seq_along(league_files)) {
    chunk_data <- readRDS(league_files[i])
    lg <- if (!is.null(chunk_data$results) && "league" %in% names(chunk_data$results)) {
      chunk_data$results$league[1]
    } else tools::file_path_sans_ext(basename(league_files[i]))
    message(sprintf("  League %d/%d: %s (%d matches)", i, length(league_files), lg,
                    length(unique(chunk_data$results$match_id))))
    league_splints <- create_all_splints(
      chunk_data, include_goals = TRUE, verbose = FALSE,
      chunk_by = "none", min_splint_duration = min_splint_duration)
    saveRDS(league_splints, file.path(chunks_dir, sprintf("chunk_%03d.rds", i)))
    rm(chunk_data, league_splints); gc(verbose = FALSE)
  }

  # Combine per-league chunks (same incremental accumulation as the legacy path).
  message("  Combining per-league chunks...")
  chunk_files <- list.files(chunks_dir, pattern = "\\.rds$", full.names = TRUE)
  splints_list    <- vector("list", length(chunk_files))
  players_list    <- vector("list", length(chunk_files))
  match_info_list <- vector("list", length(chunk_files))
  for (i in seq_along(chunk_files)) {
    one <- readRDS(chunk_files[i])
    splints_list[[i]]    <- one$splints
    players_list[[i]]    <- one$players
    match_info_list[[i]] <- one$match_info
    rm(one)
  }
  combined <- list(
    splints    = as.data.frame(data.table::rbindlist(splints_list, fill = TRUE, use.names = TRUE)),
    players    = as.data.frame(data.table::rbindlist(players_list, fill = TRUE, use.names = TRUE)),
    match_info = as.data.frame(data.table::rbindlist(match_info_list, fill = TRUE, use.names = TRUE))
  )
  rm(splints_list, players_list, match_info_list); gc(verbose = FALSE)
  unlink(chunks_dir, recursive = TRUE, force = TRUE)
  message(sprintf("Created %d splints from per-league slices", nrow(combined$splints)))
  combined
}

# Build splints, preferring the per-league disk stream (step 02 slices) to keep
# memory bounded; fall back to the legacy full-load streaming only if the
# per-league slices aren't present (e.g. a resume that skipped step 02).
.create_splints <- function() {
  leagues_dir <- file.path(cache_dir, "02_processed_leagues")
  if (dir.exists(leagues_dir) &&
      length(list.files(leagues_dir, pattern = "\\.rds$")) > 0) {
    .build_splints_from_league_dir(leagues_dir, splint_chunks_dir, MIN_SPLINT_DURATION)
  } else {
    # The combined 02_processed_data.rds no longer carries events (step 02
    # drops them to keep steps 05-08 from OOMing), so the per-league slices are
    # now the ONLY events source for splint creation. If they're missing, fail
    # loudly rather than silently build degraded, event-less splints.
    stop("Per-league processed slices not found in '", leagues_dir,
         "'. Re-run step 02 (e.g. force_rebuild_from=2) to regenerate them.",
         call. = FALSE)
  }
}

splint_chunks_dir <- file.path(cache_dir, "03_splint_chunks")

if (file.exists(splint_data_path) &&
    file.mtime(splint_data_path) > file.mtime(processed_data_path)) {
  message("=== Skipping splint creation (cache is up to date) ===")
  splint_data <- readRDS(splint_data_path)
} else {
  message(if (file.exists(splint_data_path))
            "=== Processed data is newer - recreating splints ==="
          else "=== Creating splints ===")
  splint_data <- .create_splints()
  saveRDS(splint_data, splint_data_path)
}

# Validate splint output
validate_step_output(splint_data$splints, step_name = "03_splints: splints",
                     min_rows = 1000, warn_below = 100000)
validate_step_output(splint_data$players, step_name = "03_splints: players",
                     min_rows = 1000, warn_below = 200000)

# 4. Summary Statistics ----

cat("\n=== Splint Summary ===\n")
cat(paste("Total splints:", nrow(splint_data$splints), "\n"))
cat(paste("Average splints per match:",
          round(nrow(splint_data$splints) / length(unique(splint_data$splints$match_id)), 1), "\n"))
cat(paste("Player-splint records:", nrow(splint_data$players), "\n"))

# League breakdown
if ("league" %in% names(splint_data$splints)) {
  cat("\n=== League Breakdown ===\n")
  league_splints <- table(splint_data$splints$league)
  for (league in names(league_splints)) {
    cat(sprintf("  %s: %d splints\n", league, league_splints[league]))
  }
}

# Season breakdown
if ("season_end_year" %in% names(splint_data$splints)) {
  cat("\n=== Season Breakdown ===\n")
  season_splints <- table(splint_data$splints$season_end_year)
  for (season in names(season_splints)) {
    cat(sprintf("  %s: %d splints\n", season, season_splints[season]))
  }
}

# 5. Splint Duration Distribution ----

cat("\n=== Splint Duration Distribution ===\n")
duration_summary <- summary(splint_data$splints$duration)
print(duration_summary)

# 6. Player Assignment Validation ----

cat("\n=== Player Assignment Validation ===\n")
players_per_splint <- splint_data$players %>%
  group_by(splint_id) %>%
  summarise(
    n_players = n(),
    n_home = sum(is_home),
    n_away = sum(!is_home),
    .groups = "drop"
  )

cat("Players per splint:\n")
print(summary(players_per_splint$n_players))

correct_22 <- sum(players_per_splint$n_players == 22)
cat(sprintf("\nSplints with exactly 22 players: %d / %d (%.1f%%)\n",
            correct_22, nrow(splint_data$splints),
            100 * correct_22 / nrow(splint_data$splints)))

# 7. npxGD Distribution ----

cat("\n=== npxGD Distribution ===\n")
cat("npxgd_per_90 summary:\n")
print(summary(splint_data$splints$npxgd_per_90))

# 8. Add Value Metrics to Splints (optional) ----
# If per-action EPV/WPA streams (or a per-game PSV cache) exist, add them to
# splints for multi-target RAPM.

use_value_metrics <- if (exists("use_value_metrics")) use_value_metrics else TRUE

# D6 (FABLE-PRIOR-FIX-PLAN.md): same experimental gate as 04-07. The EPV/WPA
# per-action streams (Step 2) feed Step 3's true per-splint attribution
# (add_value_metrics_to_splints()) -- when the pipeline operator has opted
# into multi-target RAPM (run_multi_target <- TRUE), a missing stream parquet
# is a pipeline bug, not an optional feature: abort loudly instead of
# silently producing splints without epv_home/wpa_home (repo rule: no silent
# fallback on missing required data). When FALSE (default), EPV/WPA are
# skipped entirely -- matches the pre-Step-3 behaviour of the cloud pipeline,
# which never ran the multi-target section anyway. inherits = FALSE so a
# same-named object from an enclosing/parent scope can't silently flip this
# on (same reasoning as the other pipeline config guards).
run_multi_target <- if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE

if (use_value_metrics) {
  epv_action_dir <- "data-raw/cache/epv/players"

  player_action_epv <- NULL
  match_action_wpa <- NULL

  if (run_multi_target) {
    epv_action_files <- list.files(epv_action_dir, pattern = "^player_action_epv_.*\\.parquet$",
                                    full.names = TRUE)
    if (length(epv_action_files) == 0) {
      cli::cli_abort(c(
        "run_multi_target = TRUE but no {.file player_action_epv_*.parquet} files found in {.path {epv_action_dir}}.",
        "i" = "Run data-raw/epv/02_calculate_player_epv.R first (FABLE-PRIOR-FIX-PLAN.md Step 2)."
      ))
    }
    cat("\n=== Adding per-splint EPV to splints (Step 3 per-action attribution) ===\n")
    player_action_epv <- data.table::rbindlist(
      lapply(epv_action_files, arrow::read_parquet), fill = TRUE)
    if (nrow(player_action_epv) == 0) {
      cli::cli_abort("{.file player_action_epv_*.parquet} files found but contained 0 rows.")
    }

    wpa_action_files <- list.files(epv_action_dir, pattern = "^match_action_wpa_.*\\.parquet$",
                                    full.names = TRUE)
    if (length(wpa_action_files) == 0) {
      cli::cli_abort(c(
        "run_multi_target = TRUE but no {.file match_action_wpa_*.parquet} files found in {.path {epv_action_dir}}.",
        "i" = "Run data-raw/epv/06_calculate_wpa.R first (FABLE-PRIOR-FIX-PLAN.md Step 2)."
      ))
    }
    cat("=== Adding per-splint WPA to splints (Step 3 per-action attribution) ===\n")
    match_action_wpa <- data.table::rbindlist(
      lapply(wpa_action_files, arrow::read_parquet), fill = TRUE)
    if (nrow(match_action_wpa) == 0) {
      cli::cli_abort("{.file match_action_wpa_*.parquet} files found but contained 0 rows.")
    }
  }

  # Load per-game PSV (if available from skills pipeline). Unchanged by
  # Step 3 (D3): PSV keeps the whole-match-value x duration-proration path
  # inside add_value_metrics_to_splints() -- no per-splint box-score count
  # cache exists, and PSV has its own standalone pipeline (R/psr.R).
  psv_cache <- file.path("data-raw", "cache-skills", "player_game_psv.rds")
  player_game_psv <- if (file.exists(psv_cache)) {
    cat("=== Adding per-game PSV to splints ===\n")
    readRDS(psv_cache)
  } else NULL

  if (!is.null(player_action_epv) || !is.null(match_action_wpa) || !is.null(player_game_psv)) {
    splint_data <- add_value_metrics_to_splints(
      splint_data,
      player_action_epv = player_action_epv,
      match_action_wpa = match_action_wpa,
      player_game_psv = player_game_psv
    )
    # Re-save with value metrics
    saveRDS(splint_data, splint_data_path)

    added <- c()
    if ("epv_home" %in% names(splint_data$splints)) added <- c(added, "EPV")
    if ("wpa_home" %in% names(splint_data$splints)) added <- c(added, "WPA")
    if ("psv_home" %in% names(splint_data$splints)) added <- c(added, "PSV")
    cat(sprintf("Value metrics added to splints: %s\n", paste(added, collapse = ", ")))
  } else {
    cat("\nNo value metric sources found — skipping value metrics on splints.\n")
    cat("Set run_multi_target <- TRUE and run the EPV/WPA pipeline steps first to enable multi-target RAPM.\n")
  }
}

message("\nSplint creation complete!")
