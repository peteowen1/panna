# 10_export_blog_data.R
# Export player ratings and match predictions for the blog
#
# Produces two parquet files and uploads them to the blog-latest
# release on peteowen1/pannadata for the blog to consume directly.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
ratings_cache_dir <- file.path("data-raw", "cache-opta")
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

ratings_output <- file.path(cache_dir, "panna_ratings.parquet")
predictions_output <- file.path(cache_dir, "match_predictions.parquet")

# 3. Build Ratings Parquet ----

message("\n=== Building Blog Ratings ===\n")

ratings_path <- file.path(ratings_cache_dir, "07_seasonal_ratings.rds")
if (!file.exists(ratings_path)) {
  stop("Missing cache-opta/07_seasonal_ratings.rds. Run the Opta RAPM pipeline first.")
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

# Join and compute ranks/percentiles
panna_ratings <- seasonal_xrapm %>%
  left_join(seasonal_spm, by = "player_name") %>%
  mutate(
    panna_rank = as.integer(rank(-xrapm)),
    panna_percentile = round(100 * rank(xrapm) / n(), 1)
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

message(sprintf("  Final ratings: %d players", nrow(panna_ratings)))
message(sprintf("  Top player: %s (%.3f)", panna_ratings$player_name[1], panna_ratings$panna[1]))

arrow::write_parquet(panna_ratings, ratings_output)
message(sprintf("  Written: %s", ratings_output))

# 4. Build Predictions Parquet ----

message("\n=== Building Blog Predictions ===\n")

predictions_input <- file.path(cache_dir, "predictions.parquet")
if (!file.exists(predictions_input)) {
  stop("Missing cache-predictions-opta/predictions.parquet. Run step 07 first.")
}

predictions <- arrow::read_parquet(predictions_input)

# Select the blog-facing columns
pred_cols <- c("match_id", "match_date", "league", "season",
               "home_team", "away_team",
               "pred_home_goals", "pred_away_goals",
               "prob_H", "prob_D", "prob_A", "predicted_result")

missing_cols <- setdiff(pred_cols, names(predictions))
if (length(missing_cols) > 0) {
  warning(sprintf("Missing prediction columns (will be excluded): %s",
                  paste(missing_cols, collapse = ", ")))
  pred_cols <- intersect(pred_cols, names(predictions))
}

match_predictions <- predictions %>% select(all_of(pred_cols))

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
  error = function(e) NULL,
  warning = function(w) NULL
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
  if (grepl("auth|login|credential|forbidden|401|403", stderr_text, ignore.case = TRUE)) {
    stop(sprintf("gh authentication error: %s\nRun 'gh auth login' first.", stderr_text))
  }

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
