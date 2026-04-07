# 10b_export_game_logs.R
# Export per-match player value metrics (EPV + WPA + PSV) for the blog
#
# Produces game_logs.parquet with one row per player per match for the
# current season across blog leagues. Uploaded to blog-latest release
# on peteowen1/pannadata for the blog to consume.
#
# EPV: SPADL → EPV model → credit assignment → aggregate_player_game_epv()
# WPA: SPADL → WP model → credit assignment → aggregate_player_game_wpa()
# PSV: match stats → compute_player_psv()
# Merged via build_player_game_ratings() → panna_value (50/50 EPV + PSV)

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

# Blog leagues (domestic only — no tournaments)
blog_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "NED", "POR", "SCO", "TUR", "ENG2")

# Current season only
if (!exists("game_log_season")) game_log_season <- "2025-2026"

output_path <- file.path(cache_dir, "game_logs.parquet")

# 2. Load models ----

message("\n=== Building Game Logs ===\n")

epv_model <- load_epv_model()
xpass_model <- load_xpass_model()
wp_model <- load_wp_model()

# Load match stats for PSV (from skills pipeline cache)
match_stats_path <- file.path("data-raw", "cache-skills", "01_match_stats.rds")
has_match_stats <- file.exists(match_stats_path)
if (has_match_stats) {
  all_match_stats <- readRDS(match_stats_path)
  message(sprintf("  Loaded match stats: %d player-games", nrow(all_match_stats)))
} else {
  message("  Note: No match stats cache — PSV will be unavailable")
}

# Load seasonal SPM for spm_overall column
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")

if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  seasonal_results <- readRDS(skill_ratings_path)
} else if (file.exists(raw_ratings_path)) {
  seasonal_results <- readRDS(raw_ratings_path)
} else {
  seasonal_results <- NULL
}

# Extract SPM lookup (player_id → spm_overall for current season)
spm_lookup <- NULL
if (!is.null(seasonal_results) && !is.null(seasonal_results$seasonal_spm)) {
  spm_dt <- data.table::as.data.table(seasonal_results$seasonal_spm)
  latest_year <- max(spm_dt$season_end_year, na.rm = TRUE)
  spm_lookup <- spm_dt[season_end_year == latest_year, .(player_id, spm_overall = spm)]
  spm_lookup <- spm_lookup[, .SD[1], by = player_id]
  message(sprintf("  SPM lookup: %d players (season %d)", nrow(spm_lookup), latest_year))
}

# 3. Helper: build match results from events + lineups ----

.build_match_results <- function(events, lineups) {
  dt_lineups <- data.table::as.data.table(lineups)
  match_teams <- dt_lineups[, .(
    home_team_id = team_id[tolower(team_position) == "home"][1],
    away_team_id = team_id[tolower(team_position) == "away"][1]
  ), by = match_id]
  dt_events <- data.table::as.data.table(events)
  goals <- dt_events[type_id == 16L]
  goal_counts <- goals[, .N, by = .(match_id, team_id)]
  match_teams[goal_counts, home_goals := i.N, on = .(match_id, home_team_id = team_id)]
  match_teams[goal_counts, away_goals := i.N, on = .(match_id, away_team_id = team_id)]
  match_teams[is.na(home_goals), home_goals := 0L]
  match_teams[is.na(away_goals), away_goals := 0L]
  as.data.frame(match_teams)
}

# 4. Process each league ----

all_game_logs <- list()

for (league in blog_leagues) {
  tryCatch({
    message(sprintf("\n  Processing %s %s...", league, game_log_season))

    events <- load_opta_match_events(league, season = game_log_season)
    lineups <- load_opta_lineups(league, season = game_log_season)

    if (is.null(events) || nrow(events) < 100) {
      message(sprintf("    Skipping %s — insufficient data", league))
      next
    }

    n_matches <- length(unique(events$match_id))
    message(sprintf("    %d matches, %d events", n_matches, nrow(events)))

    # --- SPADL conversion (shared by EPV and WPA) ---
    spadl <- convert_opta_to_spadl(events)
    spadl_chains <- create_possession_chains(spadl)
    chain_outcomes <- classify_chain_outcomes(spadl_chains)
    chain_outcomes <- add_next_chain_outcome(chain_outcomes)
    spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
    spadl_labeled <- create_next_goal_labels(spadl_labeled)

    # --- EPV path ---
    # Features created internally by calculate_action_epv when feature_mode = "simple"
    spadl_epv <- calculate_action_epv(spadl_labeled, features = NULL, epv_model,
                                      league = league)
    spadl_credit <- assign_epv_credit(spadl_epv, xpass_model)
    player_game_epv <- aggregate_player_game_epv(spadl_credit, lineups)

    # --- EPV adjustments (position centering + opponent) ---
    tryCatch({
      # Add match_date for opponent adjustment
      dt_lu <- data.table::as.data.table(lineups)
      if ("match_date" %in% names(dt_lu)) {
        match_dates <- dt_lu[, .(match_date = match_date[1]), by = match_id]
        player_game_epv <- merge(player_game_epv, match_dates, by = "match_id", all.x = TRUE)
      }

      # Position centering
      if ("position" %in% names(player_game_epv)) {
        player_game_epv <- adjust_epv_for_position(
          player_game_epv,
          credit_cols = c("epv_total", "epv_offensive", "epv_defensive")
        )
      }

      # Opponent adjustment
      if (all(c("match_date", "team_id", "minutes_played") %in% names(player_game_epv))) {
        player_game_epv <- adjust_epv_for_opponents(
          player_game_epv, credit_col = "epv_total"
        )
      }
    }, error = function(e) {
      warning(sprintf("EPV adjustments skipped for %s: %s", league, e$message), call. = FALSE)
    })

    message(sprintf("    EPV: %d player-games", nrow(player_game_epv)))

    # --- WPA path ---
    has_wpa <- FALSE
    player_game_wpa <- tryCatch({
      match_results <- .build_match_results(events, lineups)
      wp_feat <- create_wp_features(spadl_chains, match_results)
      spadl_wpa <- add_wp_vars(wp_feat, wp_model)
      spadl_wpa <- assign_wpa_credit(spadl_wpa)
      pgw <- aggregate_player_game_wpa(spadl_wpa, lineups)
      message(sprintf("    WPA: %d player-games", nrow(pgw)))
      has_wpa <<- TRUE
      pgw
    }, error = function(e) {
      warning(sprintf("WPA failed for %s: %s", league, e$message), call. = FALSE)
      NULL
    })

    # --- PSV path ---
    has_psv <- FALSE
    player_game_psv <- NULL
    if (has_match_stats) {
      tryCatch({
        league_match_ids <- unique(events$match_id)
        league_stats <- all_match_stats[all_match_stats$match_id %in% league_match_ids, ]
        if (nrow(league_stats) > 0) {
          player_game_psv <- compute_player_psv(league_stats, min_adjust = FALSE, center = TRUE)
          message(sprintf("    PSV: %d player-games", nrow(player_game_psv)))
          has_psv <- TRUE
        }
      }, error = function(e) {
        warning(sprintf("PSV failed for %s: %s", league, e$message), call. = FALSE)
      })
    }

    # --- Merge via build_player_game_ratings ---
    game_ratings <- build_player_game_ratings(
      player_game_epv = player_game_epv,
      player_game_wpa = player_game_wpa,
      player_game_psv = player_game_psv
    )

    # --- Add match_date from lineups ---
    dt_lineups <- data.table::as.data.table(lineups)
    if ("match_date" %in% names(dt_lineups)) {
      match_dates <- dt_lineups[, .(match_date = match_date[1]), by = match_id]
      game_ratings <- merge(game_ratings, match_dates, by = "match_id", all.x = TRUE)
    }

    # --- Add league/season ---
    game_ratings[, league := league]
    game_ratings[, season := game_log_season]

    all_game_logs[[league]] <- game_ratings
    message(sprintf("    Final: %d player-games", nrow(game_ratings)))

    # Free memory
    rm(events, lineups, spadl, spadl_chains, chain_outcomes, spadl_labeled,
       epv_features, spadl_epv, spadl_credit, player_game_epv,
       player_game_wpa, player_game_psv, game_ratings)
    gc(verbose = FALSE)

  }, error = function(e) {
    # Only treat data-loading errors as "skip" — all others are real errors
    is_data_error <- inherits(e, "panna_data_not_found") ||
      grepl("^No data found for|^No .+ data available", e$message)
    if (is_data_error) {
      message(sprintf("    Skipping %s — data not available", league))
    } else {
      warning(sprintf("ERROR processing %s: %s", league, e$message), call. = FALSE)
    }
  })
}

# 5. Combine and reshape for blog ----

if (length(all_game_logs) == 0) {
  stop("No game logs produced. Check that events/lineups are available for the current season.")
}

game_logs <- data.table::rbindlist(all_game_logs, fill = TRUE)

n_leagues_ok <- length(all_game_logs)
if (n_leagues_ok < length(blog_leagues) / 2) {
  warning(sprintf("Only %d/%d leagues produced game logs. Data may be incomplete.",
                  n_leagues_ok, length(blog_leagues)), call. = FALSE)
}

message(sprintf("\n  Combined: %d player-games across %d leagues",
                nrow(game_logs), n_leagues_ok))

# Rename columns to match blog expectations
# panna_value → panna, epv_offensive → offense, epv_defensive → defense
# minutes_played → total_minutes
data.table::setnames(game_logs, old = c("panna_value", "epv_offensive", "epv_defensive", "minutes_played"),
                      new = c("panna", "offense", "defense", "total_minutes"),
                      skip_absent = TRUE)

# Join SPM lookup
if (!is.null(spm_lookup)) {
  game_logs <- merge(game_logs, spm_lookup, by = "player_id", all.x = TRUE)
  na_spm <- sum(is.na(game_logs$spm_overall))
  message(sprintf("  SPM joined: %d/%d have SPM", nrow(game_logs) - na_spm, nrow(game_logs)))
}

# Compute panna_percentile (across all players in the season)
player_totals <- game_logs[, .(total_panna = sum(panna, na.rm = TRUE)), by = player_id]
player_totals[, panna_percentile := round(100 * rank(total_panna, ties.method = "min") / .N, 1)]
game_logs <- merge(game_logs, player_totals[, .(player_id, panna_percentile)],
                    by = "player_id", all.x = TRUE)

# Select and order columns for blog
blog_cols <- intersect(
  c("player_id", "player_name", "match_id", "match_date", "league", "season",
    "team_id", "position", "total_minutes",
    "panna", "offense", "defense", "spm_overall", "panna_percentile",
    "epv_total", "epv_passing", "epv_shooting", "epv_dribbling", "epv_defending",
    "wpa_total", "wpa_as_actor", "wpa_as_receiver",
    "psv", "osv", "dsv",
    "panna_value_p90"),
  names(game_logs)
)
game_logs <- game_logs[, ..blog_cols]

# Round numeric columns
num_cols <- names(game_logs)[vapply(game_logs, is.numeric, logical(1))]
round_cols <- setdiff(num_cols, "total_minutes")
for (col in round_cols) {
  data.table::set(game_logs, j = col, value = round(game_logs[[col]], 4))
}

data.table::setorder(game_logs, league, match_date, match_id, -panna)

message(sprintf("  Final game logs: %d rows, %d columns", nrow(game_logs), ncol(game_logs)))
message(sprintf("  Leagues: %s", paste(unique(game_logs$league), collapse = ", ")))

arrow::write_parquet(game_logs, output_path)
message(sprintf("  Written: %s (%.1f MB)", output_path,
                file.size(output_path) / (1024 * 1024)))

# 6. Upload to GitHub Releases ----

message("\n=== Uploading game logs to GitHub ===\n")

gh_check <- tryCatch(
  system2("gh", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(gh_check)) {
  stop("'gh' CLI is not installed or not on PATH.")
}

message(sprintf("  Uploading to %s/%s...", repo, tag))
result <- system2(
  "gh", c("release", "upload", tag, shQuote(output_path),
          "--repo", repo, "--clobber"),
  stdout = TRUE, stderr = TRUE
)
if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
  stop(sprintf("Failed to upload game_logs.parquet: %s", paste(result, collapse = "\n")))
}

# 7. Summary ----

message("\n========================================")
message("Game logs exported successfully!")
message("========================================")
message(sprintf("  %d player-games across %d leagues", nrow(game_logs), length(unique(game_logs$league))))
message(sprintf("  Season: %s", game_log_season))
message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
