# 02_data_processing.R
# Process Opta raw data into processed_data structure for splints
#
# Uses create_opta_processed_data() adapter to convert Opta formats
# to the standard processed_data list expected by create_all_splints().

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-opta")

raw_data_path <- file.path(cache_dir, "01_raw_data.rds")
processed_data_path <- file.path(cache_dir, "02_processed_data.rds")

# Check cache
if (file.exists(processed_data_path)) {
  raw_mtime <- file.mtime(raw_data_path)
  proc_mtime <- file.mtime(processed_data_path)
  if (proc_mtime > raw_mtime) {
    message("=== Processed data cache is up to date ===")
    processed_data <- readRDS(processed_data_path)
  }
}

# 3. Process Data ----

if (!exists("processed_data")) {
  message("=== Processing Opta data ===")
  raw_opta_data <- readRDS(raw_data_path)

  # Use the Opta adapter to create processed_data structure.
  # Pass chain-derived player_timing (from step 01) so on/off times come from
  # event chains rather than lineups (second precision; correct stoppage-time
  # accounting for finishers).
  processed_data <- create_opta_processed_data(
    opta_lineups = raw_opta_data$lineups,
    opta_events = raw_opta_data$events,
    opta_shot_events = NULL,  # We use SPADL-derived shots instead
    opta_stats = raw_opta_data$stats,
    player_timing = raw_opta_data$player_timing
  )

  # Override shooting with SPADL-derived shots (which have model xG)
  if (!is.null(raw_opta_data$shooting)) {
    processed_data$shooting <- raw_opta_data$shooting
  }

  # Enrich results with xG from Step 01 and metadata
  if (!is.null(raw_opta_data$results)) {
    # Use results from Step 01 which already has xG
    processed_data$results <- raw_opta_data$results
  }

  # Add season_end_year to results if not present
  if (!is.null(processed_data$results) && !"season_end_year" %in% names(processed_data$results)) {
    processed_data$results <- processed_data$results %>%
      mutate(season_end_year = vapply(season, extract_season_end_year, numeric(1)))
  }

  # panna#87: opta_stats/opta_xmetrics saved to their OWN file, never attached
  # to processed_data. opta_stats alone measured ~7.5GB in RAM at June scale
  # (dense ~3.5M x 287 numeric) -- readRDS() must deserialize an object's
  # ENTIRE graph before returning ANY of it, so bundling stats/xmetrics inside
  # the same list as lineups/shooting/results/stats_summary made the file's
  # readRDS() peak (not just post-load usage) require the sum of ALL of them
  # at once. This was true regardless of how quickly a consumer narrowed
  # AFTER loading -- confirmed live: step 05 (SPM) survived its own
  # processed_data load with only ~110MB free out of 16GB (2026-07-08, run
  # 28920296396), and step 07 (which loads the identical file) OOM'd inside
  # readRDS() itself, before any narrowing code could even run (run
  # 28921032951). Steps 05/07 need ONLY opta_stats/opta_xmetrics; steps
  # 03/06/08 and the skills pipeline need ONLY lineups/shooting/results/
  # stats_summary (confirmed: grep shows no production script reads
  # processed_data$opta_stats or $opta_xmetrics) -- so this is a real split,
  # not a workaround.
  if (!exists("save_cache_with_meta", mode = "function")) {
    source(file.path("data-raw", "pipeline_utils.R"))
  }
  opta_stats_path <- file.path(cache_dir, "02_opta_stats.rds")
  save_cache_with_meta(
    list(opta_stats = raw_opta_data$stats, opta_xmetrics = raw_opta_data$xmetrics),
    opta_stats_path, pipeline = "player-ratings-opta"
  )

  # Combined file WITHOUT the multi-GB events blob (and, as of panna#87,
  # without opta_stats/opta_xmetrics -- see above). Consumers: RAPM steps
  # 03/06/08, the skills pipeline. NONE read $events (verified) -- loading
  # events for nothing OOM'd step 05 (SPM) at 15.9GB just to grab two small
  # tables, back when stats/xmetrics were still bundled in here too. Events
  # live only in the per-league slices below (step 03's input). Detach for
  # the save, reattach for the slice write.
  .events_keep <- processed_data$events
  processed_data$events <- NULL
  # Growth tripwire (panna#128/#133): this cache is in the same incident class
  # as cache-skills/01_match_stats.rds — grown by full-sync/league expansion,
  # looped over by opta steps 03/06/08 + skills pipeline.
  save_cache_with_meta(processed_data, processed_data_path,
                       pipeline = "player-ratings-opta")
  processed_data$events <- .events_keep
  rm(.events_keep); gc(verbose = FALSE)
}

# 3b. Per-league processed slices (splint-creation memory fix) ----
# Step 03 builds splints league-by-league. Writing the slices HERE -- where
# processed_data is already resident -- lets step 03 read ONE league at a time
# from disk instead of loading the full multi-GB, events-heavy blob and copying
# it per league, which OOM'd the 16GB runner in splint_creation. The combined
# 02_processed_data.rds above is still written (steps 05-08 read it). Rewritten
# only when missing or older than the processed_data cache.
leagues_dir <- file.path(cache_dir, "02_processed_leagues")
.done_marker <- file.path(leagues_dir, "_done.txt")
.slices_stale <- !file.exists(.done_marker) ||
  file.mtime(.done_marker) < file.mtime(processed_data_path)
if ("league" %in% names(processed_data$results) && isTRUE(.slices_stale)) {
  message("=== Writing per-league processed slices (streaming splint input) ===")
  unlink(leagues_dir, recursive = TRUE, force = TRUE)
  dir.create(leagues_dir, recursive = TRUE)
  .lgs <- unique(processed_data$results$league)
  for (.lg in .lgs) {
    .mids <- processed_data$results$match_id[processed_data$results$league == .lg]
    .slice <- list(
      lineups       = processed_data$lineups[processed_data$lineups$match_id %in% .mids, , drop = FALSE],
      shooting      = processed_data$shooting[processed_data$shooting$match_id %in% .mids, , drop = FALSE],
      results       = processed_data$results[processed_data$results$match_id %in% .mids, , drop = FALSE],
      events        = if (!is.null(processed_data$events))
                        processed_data$events[processed_data$events$match_id %in% .mids, , drop = FALSE] else NULL,
      stats_summary = if (!is.null(processed_data$stats_summary))
                        processed_data$stats_summary[processed_data$stats_summary$match_id %in% .mids, , drop = FALSE] else NULL
    )
    saveRDS(.slice, file.path(leagues_dir,
                              paste0(gsub("[^A-Za-z0-9_-]", "_", .lg), ".rds")))
    rm(.slice); gc(verbose = FALSE)
  }
  writeLines(as.character(.lgs), .done_marker)
  message(sprintf("  Wrote %d per-league slices to %s", length(.lgs), leagues_dir))
}

# 4. Summary Statistics ----

cat("\n=== Processed Data Summary ===\n")
cat(paste("Matches:", nrow(processed_data$results), "\n"))
cat(paste("Unique teams:", length(unique(c(
  processed_data$results$home_team,
  processed_data$results$away_team
))), "\n"))
cat(paste("Lineup records:", if (!is.null(processed_data$lineups)) nrow(processed_data$lineups) else 0, "\n"))
cat(paste("Events:", if (!is.null(processed_data$events)) nrow(processed_data$events) else 0, "\n"))
cat(paste("Shots:", if (!is.null(processed_data$shooting)) nrow(processed_data$shooting) else 0, "\n"))

# League breakdown
if ("league" %in% names(processed_data$results)) {
  cat("\n=== League Breakdown ===\n")
  league_counts <- table(processed_data$results$league)
  for (league in names(league_counts)) {
    cat(sprintf("  %s: %d matches\n", league, league_counts[league]))
  }
}

# Season breakdown
if ("season_end_year" %in% names(processed_data$results)) {
  cat("\n=== Season Breakdown ===\n")
  season_counts <- table(processed_data$results$season_end_year)
  for (season in names(season_counts)) {
    cat(sprintf("  %s: %d matches\n", season, season_counts[season]))
  }
}

# 5. Data Quality Check ----

cat("\n=== Data Quality Check ===\n")

# Check xG coverage
xg_coverage <- sum(!is.na(processed_data$results$home_xg)) / nrow(processed_data$results) * 100
cat(sprintf("xG coverage: %.1f%% (%d/%d matches)\n",
            xg_coverage,
            sum(!is.na(processed_data$results$home_xg)),
            nrow(processed_data$results)))

message("Data processing complete!")
