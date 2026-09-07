# 10d_export_shootout_wpa.R
# Export per-player penalty-shootout WPA for the blog.
#
# Shootout kicks are FILTERED OUT of open-play SPADL/chains (period_id >= 5),
# so this is a separate path straight off the raw match events: for each blog
# league we pull the shootout shot-outcome events, score every kick with
# panna::score_shootout_kicks(), and aggregate per player (taker + keeper) with
# panna::aggregate_shootout_wpa() — resolving the saving keeper via lineups.
#
# Output: shootout_wpa.parquet (one row per player, current season) uploaded to
# the pannadata blog-latest release, where build-blog-data.yml passes it to R2
# as football/shootout-wpa.parquet. Mirrors the 10b game-logs pattern.
#
# Run from panna/: Rscript data-raw/match-predictions-opta/10d_export_shootout_wpa.R

library(cli)
devtools::load_all()

# 1. Configuration ----

# resolve_blog_leagues() isn't loaded until pipeline_utils.R is sourced —
# guard so this runs standalone (Rscript) as well as via the full pipeline.
if (!exists("resolve_blog_leagues", mode = "function")) {
  source(file.path("data-raw", "pipeline_utils.R"))
}

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag  <- "blog-latest"

# Only cup/continental/international comps ever have shootouts; domestic and
# calendar-year leagues never do. Continental/intl groups come from
# resolve_blog_leagues() (pipeline_utils.R), backed by the shared canonical
# constant (constants.R: PANNA_LEAGUE_GROUPS) so this can't drift from
# 10b/10c's league set (H-DRIFT, 2026-07-08 review) -- the old hand-list
# missed CAFCL/AFCON/Copa_America, which DO have shootouts.
.blog_league_groups <- resolve_blog_leagues()
continental_cups <- .blog_league_groups$continental_cups
intl_tournaments <- .blog_league_groups$intl_tournaments
domestic_cups    <- character(0)  # add domestic cups here if/when scraped to blog
blog_leagues     <- c(continental_cups, intl_tournaments, domestic_cups)

# Derived from the clock, never pinned — same time bomb as 10b's, which
# emptied the blog's Player Stats page in August 2026. See
# current_domestic_season() in pipeline_utils.R.
#
# The guard keeps main's `inherits = FALSE` form deliberately: dev changed
# these flag guards separately, and that change is not part of this fix.
if (!exists("shootout_season", inherits = FALSE)) shootout_season <- current_domestic_season()
if (!exists("upload_shootout_wpa", inherits = FALSE)) upload_shootout_wpa <- TRUE

# Opta shootout shot-outcome type_ids: 16 goal, 15 saved, 14 post, 13 missed.
SO_OUTCOME_TYPE_IDS <- c(16L, 15L, 14L, 13L)

message(sprintf("\n=== Shootout WPA export: season %s ===", shootout_season))

# 2. Gather shootout kicks + lineups across leagues ----
all_kicks <- list()
all_lineups <- list()

for (league in blog_leagues) {
  league_season <- resolve_league_season(league, shootout_season,
                                          tournament_leagues = intl_tournaments)
  if (is.null(league_season)) next

  res <- tryCatch({
    events <- load_opta_match_events(league, season = league_season)
    if (is.null(events) || nrow(events) < 100) return(NULL)
    dt <- data.table::as.data.table(events)
    if (!"period_id" %in% names(dt)) return(NULL)
    k <- dt[period_id >= 5L & type_id %in% SO_OUTCOME_TYPE_IDS]
    if (nrow(k) == 0L) {
      message(sprintf("  %s %s: no shootouts", league, league_season))
      return(NULL)
    }
    k[, scored := as.integer(type_id == 16L)]
    k[, league := league]
    lu <- tryCatch(load_opta_lineups(league, season = league_season),
                   error = function(e) NULL)
    message(sprintf("  %s %s: %d shootout kicks across %d matches",
                    league, league_season, nrow(k), length(unique(k$match_id))))
    list(kicks = k, lineups = lu)
  }, error = function(e) {
    cli_alert_warning("  {league} {league_season}: {e$message}")
    NULL
  })

  if (!is.null(res)) {
    all_kicks[[league]] <- res$kicks
    if (!is.null(res$lineups)) all_lineups[[league]] <- data.table::as.data.table(res$lineups)
  }
}

if (length(all_kicks) == 0L) {
  message("No shootout kicks found for this season — nothing to export.")
  quit(save = "no", status = 0)
}

kicks   <- data.table::rbindlist(all_kicks, fill = TRUE)
lineups <- if (length(all_lineups)) data.table::rbindlist(all_lineups, fill = TRUE) else NULL

# 3. Score + aggregate per player ----
agg <- aggregate_shootout_wpa(kicks, lineups = lineups)
agg[, season := shootout_season]

message(sprintf("\n  %d players with shootout WPA (%d kicks, %d matches)",
                nrow(agg), nrow(kicks), length(unique(kicks$match_id))))
message("  Top 5 by shootout_wpa_total:")
top5 <- head(agg[order(-shootout_wpa_total)], 5)
for (i in seq_len(nrow(top5))) {
  message(sprintf("    %-22s total=%+.3f (taker %+.3f, keeper %+.3f)",
                  top5$player_name[i], top5$shootout_wpa_total[i],
                  top5$taker_wpa[i], top5$keeper_wpa[i]))
}

# 4. Write + upload ----
out_path <- file.path(cache_dir, "shootout_wpa.parquet")
arrow::write_parquet(agg, out_path)
# Small published table -> CSV companion too (per project convention).
csv_path <- file.path(cache_dir, "shootout_wpa.csv")
utils::write.csv(agg, csv_path, row.names = FALSE)
message(sprintf("\n  Written: %s (%d rows) + CSV", out_path, nrow(agg)))

if (isTRUE(upload_shootout_wpa)) {
  # PA5/H-TORN: no upload here -- register for the single gated publish in
  # 13_publish_release_data.R.
  if (exists("publish_files", envir = .GlobalEnv)) {
    publish_files$blog_latest <<- c(publish_files$blog_latest, out_path, csv_path)
    message(sprintf("\n  Registered shootout_wpa.parquet + .csv for blog-latest publish (step 13)"))
  } else {
    message("\n  (standalone run -- not registered for step-13 publish)")
  }
} else {
  message("\n  (upload_shootout_wpa = FALSE — not registering for publish)")
}

message("\n=== Shootout WPA export complete ===")
