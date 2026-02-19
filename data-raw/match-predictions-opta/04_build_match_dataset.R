# 04_build_match_dataset.R
# Combine all features into final match dataset for model training
#
# Joins fixture_results + team_ratings + rolling_features.
# Adds structural features (league dummies, month, etc.).
# Creates time-based train/validation/test splits.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "04_match_dataset.rds")

# Number of holdout seasons from the end
N_VAL_SEASONS <- 1L
N_TEST_SEASONS <- 1L

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 04_match_dataset.rds")
  match_dataset <- readRDS(output_path)
  message(sprintf("  Train: %d, Val: %d, Test: %d, Fixture: %d",
                  sum(match_dataset$split == "train"),
                  sum(match_dataset$split == "val"),
                  sum(match_dataset$split == "test"),
                  sum(match_dataset$split == "fixture")))
  return(invisible(NULL))
}

# 4. Load All Components ----

message("\n=== Building Match Dataset ===\n")

fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
team_ratings <- readRDS(file.path(cache_dir, "02_team_ratings.rds"))
rolling_features <- readRDS(file.path(cache_dir, "03_rolling_features.rds"))

message(sprintf("  Fixture results: %d rows", nrow(fixture_results)))
message(sprintf("  Team ratings: %d rows", nrow(team_ratings)))
message(sprintf("  Rolling features: %d rows", nrow(rolling_features)))

# 5. Join All Features ----

dataset <- fixture_results %>%
  left_join(team_ratings, by = "match_id") %>%
  left_join(rolling_features, by = "match_id")

message(sprintf("  After joins: %d rows, %d columns", nrow(dataset), ncol(dataset)))

# 6. Add Structural Features ----

message("  Adding structural features...")

# Month of match
dataset$match_month <- as.integer(format(as.Date(dataset$match_date), "%m"))

# League dummies (only if multiple leagues)
if (length(unique(dataset$league)) >= 2) {
  league_dummies <- model.matrix(~ league - 1, data = dataset)
  colnames(league_dummies) <- gsub("league", "league_", colnames(league_dummies))
  dataset <- cbind(dataset, league_dummies)
} else {
  # Single league - add a constant column
  dataset[[paste0("league_", unique(dataset$league))]] <- 1L
}

# 7. Fill Early-Season NAs ----

# For rolling features, fill NAs with column means within each league
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
rolling_cols <- grep("_last_\\d+$|days_since_last", numeric_cols, value = TRUE)

if (length(rolling_cols) > 0) {
  for (col in rolling_cols) {
    na_idx <- is.na(dataset[[col]])
    if (any(na_idx)) {
      # Fill with league-specific mean, or global mean
      for (lg in unique(dataset$league)) {
        lg_idx <- dataset$league == lg & na_idx
        lg_mean <- mean(dataset[[col]][dataset$league == lg & !na_idx], na.rm = TRUE)
        if (!is.na(lg_mean)) {
          dataset[[col]][lg_idx] <- lg_mean
        }
      }
      # Remaining NAs get global mean
      still_na <- is.na(dataset[[col]])
      if (any(still_na)) {
        dataset[[col]][still_na] <- mean(dataset[[col]], na.rm = TRUE)
      }
    }
  }
}

# Fill remaining numeric NAs with 0
for (col in numeric_cols) {
  dataset[[col]][is.na(dataset[[col]])] <- 0
}

# 8. Create Time-Based Split ----

message("  Creating train/val/test splits...")

played <- dataset[dataset$match_status == "Played", ]
fixtures <- dataset[dataset$match_status != "Played", ]

# Sort by season_end_year to split temporally
sey_values <- sort(unique(played$season_end_year))
n_sey <- length(sey_values)

if (n_sey >= 3) {
  test_sey <- sey_values[n_sey]
  val_sey <- sey_values[n_sey - 1]
  train_sey <- sey_values[seq_len(n_sey - 2)]
} else if (n_sey == 2) {
  test_sey <- sey_values[2]
  val_sey <- sey_values[1]
  train_sey <- sey_values[1]
} else {
  test_sey <- sey_values[1]
  val_sey <- sey_values[1]
  train_sey <- sey_values[1]
}

played$split <- "train"
played$split[played$season_end_year == val_sey] <- "val"
played$split[played$season_end_year == test_sey] <- "test"

fixtures$split <- "fixture"

match_dataset <- rbind(played, fixtures)
match_dataset <- match_dataset[order(match_dataset$match_date), ]

# 9. Encode Outcome Label ----

# 0 = Home Win, 1 = Draw, 2 = Away Win
match_dataset$outcome_label <- NA_integer_
match_dataset$outcome_label[match_dataset$result == "H"] <- 0L
match_dataset$outcome_label[match_dataset$result == "D"] <- 1L
match_dataset$outcome_label[match_dataset$result == "A"] <- 2L

# 10. Save ----

saveRDS(match_dataset, output_path)

# 11. Summary ----

message("\n========================================")
message("Match dataset complete!")
message("========================================")
message(sprintf("Total rows: %d", nrow(match_dataset)))
message(sprintf("Total features: %d", ncol(match_dataset)))
message(sprintf("Train: %d (SEY <= %d)", sum(match_dataset$split == "train"), max(train_sey)))
message(sprintf("Val:   %d (SEY = %d)", sum(match_dataset$split == "val"), val_sey))
message(sprintf("Test:  %d (SEY = %d)", sum(match_dataset$split == "test"), test_sey))
message(sprintf("Fixture: %d", sum(match_dataset$split == "fixture")))

# Result distribution
if (sum(!is.na(match_dataset$result)) > 0) {
  result_dist <- table(match_dataset$result[match_dataset$split == "train"])
  message(sprintf("\nTrain result distribution:"))
  message(sprintf("  H: %d (%.1f%%)", result_dist["H"],
                  100 * result_dist["H"] / sum(result_dist)))
  message(sprintf("  D: %d (%.1f%%)", result_dist["D"],
                  100 * result_dist["D"] / sum(result_dist)))
  message(sprintf("  A: %d (%.1f%%)", result_dist["A"],
                  100 * result_dist["A"] / sum(result_dist)))
}

message(sprintf("\nSaved to: %s", output_path))
