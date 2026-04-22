# 10_export_blog_data.R
# Export player ratings and match predictions for the blog
#
# Produces two parquet files and uploads them to the blog-latest
# release on peteowen1/pannadata for the blog to consume directly.

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
if (!exists("use_skill_ratings")) use_skill_ratings <- TRUE
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

ratings_output <- file.path(cache_dir, "panna_ratings.parquet")
predictions_output <- file.path(cache_dir, "match_predictions.parquet")

# 3. Build Ratings Parquet ----

message("\n=== Building Blog Ratings ===\n")

# Load seasonal ratings: prefer skill-based (same logic as step 02)
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")

if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  ratings_path <- skill_ratings_path
  message("  USING: SKILL-BASED ratings (cache-skills/06_seasonal_ratings.rds)")
} else if (file.exists(raw_ratings_path)) {
  ratings_path <- raw_ratings_path
  if (isTRUE(use_skill_ratings)) {
    warning("Skill ratings not found. Falling back to raw-stat ratings.", call. = FALSE, immediate. = TRUE)
    message("  USING: RAW-STAT ratings (FALLBACK - skill ratings not found)")
  } else {
    message("  USING: RAW-STAT ratings (use_skill_ratings = FALSE)")
  }
} else {
  stop("No ratings cache found. Run the Opta RAPM pipeline first.")
}

seasonal_results <- load_cache_with_meta(ratings_path, max_age_hours = 336)
if (!is.list(seasonal_results) ||
    is.null(seasonal_results$seasonal_xrapm) ||
    is.null(seasonal_results$seasonal_spm)) {
  stop(sprintf("Ratings cache at '%s' has unexpected structure. Rebuild the ratings pipeline.", ratings_path))
}
latest_season <- max(seasonal_results$seasonal_xrapm$season_end_year)

message(sprintf("  Latest season end year: %d", latest_season))

# Determine dedup/join key based on available columns
dedup_key <- if ("player_id" %in% names(seasonal_results$seasonal_xrapm)) "player_id" else "player_name"

# Filter xRAPM to latest season, drop the synthetic replacement-pool row,
# and deduplicate to one row per player.
# Why drop "replacement": rapm_matrix.R deliberately creates a synthetic
# player_id == "replacement" representing all <200-min players pooled. It's
# a model artifact (picks up uncontrolled game-state variance), not a
# coherent player rating, so it should never appear on the blog leaderboard.
seasonal_xrapm <- seasonal_results$seasonal_xrapm %>%
  filter(season_end_year == latest_season,
         !player_id %in% c("replacement"),
         !player_name %in% c("Replacement Level")) %>%
  group_by(.data[[dedup_key]]) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Filter SPM to latest season, drop replacement-pool row, deduplicate.
seasonal_spm <- seasonal_results$seasonal_spm %>%
  filter(season_end_year == latest_season,
         !player_id %in% c("replacement"),
         !player_name %in% c("Replacement Level")) %>%
  group_by(.data[[dedup_key]]) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(all_of(dedup_key), spm_overall = spm)

message(sprintf("  xRAPM players: %d (joined by %s)", nrow(seasonal_xrapm), dedup_key))
message(sprintf("  SPM players: %d", nrow(seasonal_spm)))

if (nrow(seasonal_xrapm) == 0) {
  stop(sprintf("No xRAPM data for season_end_year = %d. Check the ratings cache.", latest_season))
}

# Join and compute ranks/percentiles.
# Sign convention for the published file: POSITIVE = good for both offense
# and defense (matches torpverse, NBA RAPM, and consumer intuition).
# Internally the model treats `defense` as "additive contribution to opponent
# xG" where negative = good defender. We flip the sign here so the blog shows
# `defense` as "defensive value added (xG suppression per 90)" — positive = good.
# `panna` is unchanged because panna = offense - defense_internal = offense + defense_published.
panna_ratings <- seasonal_xrapm %>%
  left_join(seasonal_spm, by = dedup_key) %>%
  mutate(
    panna_rank = as.integer(rank(-xrapm, ties.method = "min")),
    panna_percentile = round(100 * rank(xrapm, ties.method = "min") / n(), 1)
  ) %>%
  select(
    panna_rank,
    any_of("player_id"),
    player_name,
    panna = xrapm,
    offense,
    defense,
    spm_overall,
    total_minutes,
    panna_percentile
  ) %>%
  mutate(defense = -defense) %>%   # flip: positive = good defender
  mutate(across(c(panna, offense, defense, spm_overall), ~round(.x, 4))) %>%
  arrange(panna_rank)

na_spm <- sum(is.na(panna_ratings$spm_overall))
if (na_spm > 0) message(sprintf("  Note: %d players have no SPM rating (NA)", na_spm))
message(sprintf("  Final ratings: %d players", nrow(panna_ratings)))
message(sprintf("  Top player: %s (%.3f)", panna_ratings$player_name[1], panna_ratings$panna[1]))

arrow::write_parquet(panna_ratings, ratings_output)
message(sprintf("  Written: %s", ratings_output))

# 4. Build Predictions Parquet ----

message("\n=== Building Blog Predictions ===\n")

predictions_input <- file.path(cache_dir, "predictions.parquet")
if (!file.exists(predictions_input)) {
  stop("Missing cache-predictions-opta/predictions.parquet. Run the prediction pipeline (07_predict_fixtures) first.")
}

predictions <- arrow::read_parquet(predictions_input)

# Required columns the blog cannot function without.
# `status` ("played"/"fixture") is a required contract for sim consumers:
# predictions.parquet covers every historical match plus upcoming fixtures
# (the blog's Results view shows past predictions alongside projections), so
# sims must filter to status == "fixture" to avoid re-simulating completed
# matches. Hard-fail here rather than fall through to date-based guessing.
required_cols <- c("match_id", "match_date", "league", "home_team", "away_team",
                   "prob_H", "prob_D", "prob_A", "status")
missing_required <- setdiff(required_cols, names(predictions))
if (length(missing_required) > 0) {
  stop(sprintf("Missing required prediction columns: %s\nRebuild predictions with the current pipeline.",
               paste(missing_required, collapse = ", ")))
}

# Optional columns — include if available.
optional_cols <- c("season", "pred_home_goals", "pred_away_goals", "predicted_result")
missing_optional <- setdiff(optional_cols, names(predictions))
if (length(missing_optional) > 0) {
  message(sprintf("  Note: Optional columns missing (excluded): %s",
                  paste(missing_optional, collapse = ", ")))
}
pred_cols <- intersect(c(required_cols, optional_cols), names(predictions))

match_predictions <- predictions %>% select(all_of(pred_cols))

if (nrow(match_predictions) == 0) {
  stop("No predictions to export. Run the prediction pipeline (step 07) first.")
}

message(sprintf("  %d predictions across %d leagues",
                nrow(match_predictions), length(unique(match_predictions$league))))
message(sprintf("  Date range: %s to %s",
                min(match_predictions$match_date), max(match_predictions$match_date)))

arrow::write_parquet(match_predictions, predictions_output)
message(sprintf("  Written: %s", predictions_output))

# 4b. Build Season Standings Parquet ----

message("\n=== Building Season Standings ===\n")

standings_output <- file.path(cache_dir, "season_standings.parquet")

# Load fixture results from step 01 cache
fixture_results_path <- file.path(cache_dir, "01_fixture_results.rds")
if (!file.exists(fixture_results_path)) {
  warning("01_fixture_results.rds not found — skipping standings export.", call. = FALSE)
  standings_ok <- FALSE
} else {
  fixture_results <- readRDS(fixture_results_path)

  # Leagues included in standings (domestic + UEFA cups for simulation scripts)
  standings_leagues <- c("ENG", "ENG2", "ESP", "FRA", "GER", "ITA", "NED", "POR", "SCO", "TUR",
                         "UCL", "UEL", "UECL")

  played <- fixture_results %>%
    filter(match_status == "Played", league %in% standings_leagues,
           !is.na(home_goals), !is.na(away_goals))

  # Current season only (same as predictions)
  if ("season" %in% names(played)) {
    latest <- max(played$season, na.rm = TRUE)
    played <- played %>% filter(season == latest)
  } else if ("season_end_year" %in% names(played)) {
    latest <- max(played$season_end_year, na.rm = TRUE)
    played <- played %>% filter(season_end_year == latest)
  }

  message(sprintf("  Played matches (current season): %d", nrow(played)))

  # Compute standings from home and away perspectives
  home_stats <- played %>%
    group_by(league, team = home_team) %>%
    summarise(gp = n(),
              pts = sum(ifelse(home_goals > away_goals, 3L,
                               ifelse(home_goals == away_goals, 1L, 0L))),
              gf = sum(home_goals), ga = sum(away_goals), .groups = "drop")

  away_stats <- played %>%
    group_by(league, team = away_team) %>%
    summarise(gp = n(),
              pts = sum(ifelse(away_goals > home_goals, 3L,
                               ifelse(away_goals == home_goals, 1L, 0L))),
              gf = sum(away_goals), ga = sum(home_goals), .groups = "drop")

  season_standings <- bind_rows(home_stats, away_stats) %>%
    group_by(league, team) %>%
    summarise(games_played = sum(gp), points = sum(pts),
              gf = sum(gf), ga = sum(ga), .groups = "drop") %>%
    mutate(gd = gf - ga) %>%
    select(league, team, games_played, points, gd) %>%
    arrange(league, desc(points), desc(gd))

  message(sprintf("  Teams with standings: %d across %d leagues",
                  nrow(season_standings), length(unique(season_standings$league))))

  # Self-consistency invariants on the aggregation. Pure arithmetic on the
  # same inputs step 10 already filtered; a violation means step 10's grouping
  # or summation has a bug. Provider drift (stale Opta, FD.org disagreement)
  # cannot trip these — only panna's own code can.
  #
  # - sum(games_played) == 2 * n_matches per league (each match adds to two gp)
  # - sum(gd) == 0 per league (zero-sum across the league)
  # - sum(points) == 2*n_draws + 3*n_decisives per league (2 pts distributed on
  #   a draw, 3 on a decisive). Assumes the standard 3-1-0 with no modeled
  #   administrative point deductions — currently true across all our leagues.
  per_league_stats <- played %>%
    group_by(league) %>%
    summarise(n_matches = n(),
              n_draw = sum(home_goals == away_goals),
              n_decisive = sum(home_goals != away_goals),
              .groups = "drop")

  totals <- season_standings %>%
    group_by(league) %>%
    summarise(sum_gp = sum(games_played), sum_pts = sum(points),
              sum_gd = sum(gd), n_teams = n(), .groups = "drop") %>%
    left_join(per_league_stats, by = "league") %>%
    mutate(
      expected_gp = 2L * n_matches,
      expected_pts = 2L * n_draw + 3L * n_decisive,
      gp_ok = sum_gp == expected_gp,
      pts_ok = sum_pts == expected_pts,
      gd_ok = sum_gd == 0
    )

  failed <- totals %>% filter(!gp_ok | !pts_ok | !gd_ok)
  if (nrow(failed) > 0) {
    message("  STANDINGS INVARIANT VIOLATION — aggregation bug in step 10:")
    for (i in seq_len(nrow(failed))) {
      r <- failed[i, ]
      message(sprintf(
        "    %s: sum_gp=%d (expected %d%s), sum_pts=%d (expected %d%s), sum_gd=%d (expected 0%s)",
        r$league, r$sum_gp, r$expected_gp, if (r$gp_ok) "" else " [FAIL]",
        r$sum_pts, r$expected_pts, if (r$pts_ok) "" else " [FAIL]",
        r$sum_gd, if (r$gd_ok) "" else " [FAIL]"))
    }
    # Hard-fail rather than warn: if the invariants trip, panna's aggregation
    # is broken and publishing the file would ship known-wrong standings to
    # the blog. Safer to block the upload and surface the bug.
    stop(sprintf("Standings invariants failed for %d league(s). Fix aggregation logic before republishing.",
                 nrow(failed)), call. = FALSE)
  } else if (nrow(totals) > 0) {
    message(sprintf("  Invariants OK across all %d leagues (gp, pts, gd sums consistent)",
                    nrow(totals)))
  }

  arrow::write_parquet(season_standings, standings_output)
  message(sprintf("  Written: %s", standings_output))
  standings_ok <- TRUE
}

# 5. Upload to GitHub Releases ----

message("\n=== Uploading to GitHub ===\n")

# Check gh CLI is available
gh_check <- tryCatch(
  system2("gh", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(gh_check)) {
  stop("'gh' CLI is not installed or not on PATH. Install from https://cli.github.com/")
}

# Ensure release exists
message(sprintf("  Checking release '%s' on %s...", tag, repo))

release_check <- system2(
  "gh", c("release", "view", tag, "--repo", repo),
  stdout = TRUE, stderr = TRUE
)
release_status <- attr(release_check, "status")

if (!is.null(release_status) && release_status != 0) {
  stderr_text <- paste(release_check, collapse = "\n")

  # Match "not found" specifically; treat everything else as an unexpected error
  if (grepl("release not found|not found", stderr_text, ignore.case = TRUE)) {
    message("  Release not found. Creating...")
    create_result <- system2(
      "gh", c("release", "create", tag,
              "--repo", repo,
              "--title", shQuote("Blog Data (Latest)"),
              "--notes", shQuote("Player ratings and match predictions for the blog.")),
      stdout = TRUE, stderr = TRUE
    )
    create_status <- attr(create_result, "status")
    if (!is.null(create_status) && create_status != 0) {
      stop(sprintf("Failed to create release '%s': %s",
                   tag, paste(create_result, collapse = "\n")))
    }
  } else {
    stop(sprintf("Failed to check release '%s': %s\nCheck network, auth (gh auth login), and repo name.",
                 tag, stderr_text))
  }
}

# Upload files
upload_files <- c(ratings_output, predictions_output)
if (exists("standings_ok") && isTRUE(standings_ok)) {
  upload_files <- c(upload_files, standings_output)
}
for (fpath in upload_files) {
  fname <- basename(fpath)
  size_mb <- round(file.size(fpath) / (1024 * 1024), 2)
  message(sprintf("  Uploading %s (%.2f MB)...", fname, size_mb))
  result <- system2(
    "gh", c("release", "upload", tag, shQuote(fpath),
            "--repo", repo, "--clobber"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(sprintf("Failed to upload %s: %s", fname, paste(result, collapse = "\n")))
  }
}

# 6. Summary ----

message("\n========================================")
message("Blog data exported successfully!")
message("========================================")
message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
message(sprintf("  Ratings: %d players (season %d)", nrow(panna_ratings), latest_season))
message(sprintf("  Predictions: %d matches", nrow(match_predictions)))
if (exists("standings_ok") && isTRUE(standings_ok)) {
  message(sprintf("  Standings: %d teams", nrow(season_standings)))
}
message("\nBlog URLs:")
message(sprintf("  https://github.com/%s/releases/download/%s/panna_ratings.parquet", repo, tag))
message(sprintf("  https://github.com/%s/releases/download/%s/match_predictions.parquet", repo, tag))
if (exists("standings_ok") && isTRUE(standings_ok)) {
  message(sprintf("  https://github.com/%s/releases/download/%s/season_standings.parquet", repo, tag))
}
