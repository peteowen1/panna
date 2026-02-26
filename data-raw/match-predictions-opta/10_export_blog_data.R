# 10_export_blog_data.R
# Export player ratings and match predictions for the blog
#
# Produces two parquet files and uploads them to the blog-latest
# release on peteowen1/pannadata for the blog to consume directly.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
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
    message("  USING: RAW-STAT ratings (FALLBACK - skill ratings not found)")
  } else {
    message("  USING: RAW-STAT ratings (use_skill_ratings = FALSE)")
  }
} else {
  stop("No ratings cache found. Run the Opta RAPM pipeline first.")
}

seasonal_results <- readRDS(ratings_path)
latest_season <- max(seasonal_results$seasonal_xrapm$season_end_year)

message(sprintf("  Latest season end year: %d", latest_season))

# Filter xRAPM to latest season, deduplicate by player_name
seasonal_xrapm <- seasonal_results$seasonal_xrapm %>%
  filter(season_end_year == latest_season) %>%
  group_by(player_name) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Filter SPM to latest season, deduplicate by player_name
seasonal_spm <- seasonal_results$seasonal_spm %>%
  filter(season_end_year == latest_season) %>%
  group_by(player_name) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(player_name, spm_overall = spm)

message(sprintf("  xRAPM players: %d", nrow(seasonal_xrapm)))
message(sprintf("  SPM players: %d", nrow(seasonal_spm)))

if (nrow(seasonal_xrapm) == 0) {
  stop(sprintf("No xRAPM data for season_end_year = %d. Check the ratings cache.", latest_season))
}

# Join and compute ranks/percentiles
panna_ratings <- seasonal_xrapm %>%
  left_join(seasonal_spm, by = "player_name") %>%
  mutate(
    panna_rank = as.integer(rank(-xrapm, ties.method = "min")),
    panna_percentile = round(100 * rank(xrapm, ties.method = "min") / n(), 1)
  ) %>%
  select(
    panna_rank,
    player_name,
    panna = xrapm,
    offense,
    defense,
    spm_overall,
    total_minutes,
    panna_percentile
  ) %>%
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

# Required columns the blog cannot function without
required_cols <- c("match_id", "match_date", "league", "home_team", "away_team",
                   "prob_H", "prob_D", "prob_A")
missing_required <- setdiff(required_cols, names(predictions))
if (length(missing_required) > 0) {
  stop(sprintf("Missing required prediction columns: %s\nRebuild predictions with the current pipeline.",
               paste(missing_required, collapse = ", ")))
}

# Optional columns — include if available
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

# Upload both files
for (fpath in c(ratings_output, predictions_output)) {
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
message(sprintf("  Files: panna_ratings.parquet, match_predictions.parquet"))
message(sprintf("  Ratings: %d players (season %d)", nrow(panna_ratings), latest_season))
message(sprintf("  Predictions: %d matches", nrow(match_predictions)))
message("\nBlog URLs:")
message(sprintf("  https://github.com/%s/releases/download/%s/panna_ratings.parquet", repo, tag))
message(sprintf("  https://github.com/%s/releases/download/%s/match_predictions.parquet", repo, tag))
