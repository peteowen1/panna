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

# Load team skill features (optional, from 02b)
skill_features_path <- file.path(cache_dir, "02b_team_skill_features.rds")
team_skill_features <- if (file.exists(skill_features_path)) readRDS(skill_features_path) else NULL

message(sprintf("  Fixture results: %d rows", nrow(fixture_results)))
message(sprintf("  Team ratings: %d rows", nrow(team_ratings)))
message(sprintf("  Rolling features: %d rows", nrow(rolling_features)))
if (!is.null(team_skill_features)) {
  message(sprintf("  Team skill features: %d rows", nrow(team_skill_features)))
}

# 5. Join All Features ----

dataset <- fixture_results %>%
  left_join(team_ratings, by = "match_id") %>%
  left_join(rolling_features, by = "match_id")

# Merge team skill features if available
if (!is.null(team_skill_features) && nrow(team_skill_features) > 0) {
  dataset <- dataset %>%
    left_join(team_skill_features, by = "match_id")
  message(sprintf("  Merged team skill features (%d new columns)",
                  ncol(team_skill_features) - 1))
}

# has_sk_data indicator — explicit 0/1 flag for whether this match has
# both-sides skill estimates available. XGBoost's NA-split-direction is
# good but can be subtly miscalibrated when "this match has no skill data"
# is conflated with "skill data exists but a particular feature is zero".
# The indicator gives the model the explicit "this row's skill features
# are meaningful vs not" signal alongside the NAs. ~13.5% of historical
# matches (pre-2014, before the skill cache exists) have it as 0.
if ("home_sk_att_goals" %in% names(dataset) &&
    "away_sk_att_goals" %in% names(dataset)) {
  dataset$has_sk_data <- as.integer(
    !is.na(dataset$home_sk_att_goals) & !is.na(dataset$away_sk_att_goals))
  message(sprintf("  has_sk_data: %d / %d rows (%.1f%%) have skill data on both sides",
                  sum(dataset$has_sk_data), nrow(dataset),
                  100 * mean(dataset$has_sk_data)))
} else {
  dataset$has_sk_data <- 0L
}

message(sprintf("  After joins: %d rows, %d columns", nrow(dataset), ncol(dataset)))

# 6. Add Weather Features (optional) ----

if ("venue" %in% names(dataset) && any(!is.na(dataset$venue))) {
  message("  Adding weather features...")
  dataset <- tryCatch(
    add_weather_features(dataset, venue_col = "venue", date_col = "match_date"),
    error = function(e) {
      message(sprintf("  Weather features skipped: %s", e$message))
      dataset
    }
  )
} else {
  message("  No venue column — weather features skipped (add venue data for weather-adjusted predictions)")
}

# 7. Add Structural Features ----

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

# Flag early-season matches (those with rolling feature NAs) BEFORE imputation
# so the model can distinguish "new team with no form" from "average team"
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
rolling_cols <- grep("_last_\\d+$|days_since_last", numeric_cols, value = TRUE)

# Identify train rows. Split assignment hasn't happened yet at this point in
# the script — derive an equivalent here (held-out seasons get split labels
# in section 8 below). Computing imputation statistics from TRAIN ONLY
# prevents leakage of val/test/fixture distributions into the rolling-feature
# NA-fill that the model then sees in training.
imp_played    <- dataset$match_status == "Played"
imp_sey_vals  <- sort(unique(dataset$season_end_year[imp_played]))
imp_n_sey     <- length(imp_sey_vals)
imp_train_sey <- if (imp_n_sey >= 3L) {
  imp_sey_vals[seq_len(imp_n_sey - 2L)]
} else if (imp_n_sey == 2L) {
  imp_sey_vals[1L]
} else {
  imp_sey_vals
}
imp_train_idx <- imp_played & dataset$season_end_year %in% imp_train_sey
message(sprintf("  Imputation stats computed from %d train rows (split-leakage-free)",
                sum(imp_train_idx)))

if (length(rolling_cols) > 0) {
  # is_early_season MUST be computed BEFORE the imputation block below —
  # otherwise every row has non-NA rolling features and the flag is
  # always 0. Computed here, the flag captures the pre-imputed state so
  # the model learns to discount imputed rolling values for these rows.
  # Code-review item 22.
  ## (force data.frame indexing — rolling_features merge may return data.table)
  dataset$is_early_season <- as.integer(
    rowSums(is.na(as.data.frame(dataset)[, rolling_cols, drop = FALSE])) > 0
  )
  early_count <- sum(dataset$is_early_season)
  if (early_count > 0) {
    message(sprintf("  Flagged %d early-season matches (%.1f%%) with NA rolling features",
                    early_count, 100 * early_count / nrow(dataset)))
  }

  # Fill NAs with TRAIN-only league-specific means for rolling features.
  # Previously the per-league mean was computed across the full dataset
  # (train + val + test + fixture), leaking held-out distribution into the
  # fill values the model saw during training. Effect on published metrics
  # was small (imputed values are constant per league, not row-specific),
  # but the leak was real. Fall back to train-only global mean if a league
  # has no train rows, then to 0.
  for (col in rolling_cols) {
    na_idx <- is.na(dataset[[col]])
    if (any(na_idx)) {
      col_train <- dataset[[col]][imp_train_idx]
      for (lg in unique(dataset$league)) {
        lg_train_mask <- imp_train_idx & dataset$league == lg & !is.na(dataset[[col]])
        lg_mean <- if (any(lg_train_mask)) mean(dataset[[col]][lg_train_mask], na.rm = TRUE) else NA_real_
        if (!is.na(lg_mean)) {
          dataset[[col]][na_idx & dataset$league == lg] <- lg_mean
        }
      }
      # Remaining NAs get train-only global mean
      still_na <- is.na(dataset[[col]])
      if (any(still_na)) {
        global_mean <- mean(col_train, na.rm = TRUE)
        if (!is.na(global_mean)) {
          dataset[[col]][still_na] <- global_mean
        }
      }
    }
  }
} else {
  dataset$is_early_season <- 0L
}

# Fill remaining numeric NAs with 0 — but VERY narrowly. The previous
# "fill everything with 0" pattern silently hid real bugs (today's
# Elo-NA-cascade ended at home_elo=0 in the published team_strength;
# the EPR/PSR fixture join failure ended at sum_epr=0). Per feedback
# 2026-05-28: do NOT substitute fake values for missing data. Let NAs
# propagate so they're visible to the model (XGBoost handles NA natively
# as a separate split direction) and to downstream consumers.
#
# Columns we DO 0-fill: the structural / engineered features where 0 is
# a semantically valid default — e.g., a league dummy is 0 for matches
# not in that league. Anything that represents a TEAM STRENGTH
# measurement (panna, EPR, PSR, Elo, rolling form) must NOT be filled —
# NA there means "we don't know", and we want that visible.
skip_zero_fill <- c(
  # Actual match outcomes — fixtures legitimately have these NA
  "home_goals", "away_goals", "home_xg", "away_xg",
  # Elo features — NA means the team wasn't in the played history
  grep("^(home|away)_elo$|^elo_diff$", numeric_cols, value = TRUE),
  # Team-aggregate ratings — NA means the join failed (visible signal)
  grep("^(home|away)_(sum|avg|max|min|gk|stdev)_", numeric_cols, value = TRUE),
  grep("_diff$", numeric_cols, value = TRUE),
  grep("^(home|away)_sk_", numeric_cols, value = TRUE),
  # Rolling form features — already imputed earlier with TRAIN-only means
  # in the rolling-cols block above; this protects against NA leakage if
  # a new rolling feature is added in future without that imputation.
  grep("_last_\\d+$|days_since_last", numeric_cols, value = TRUE)
)
skip_zero_fill <- unique(skip_zero_fill)
fill_cols <- setdiff(numeric_cols, skip_zero_fill)
message(sprintf("  NA-filling %d structural numeric columns with 0; preserving NAs in %d value columns (Elo, ratings, rolling)",
                length(fill_cols), length(skip_zero_fill)))
for (col in fill_cols) {
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

  played$split <- "train"
  played$split[played$season_end_year == val_sey] <- "val"
  played$split[played$season_end_year == test_sey] <- "test"

} else if (n_sey == 2) {
  # 2 seasons: use first for train, second split temporally into val/test
  warning("Only 2 seasons available. Using temporal split within the latest season ",
          "for val/test to avoid data leakage.", call. = FALSE)
  train_sey <- sey_values[1]
  test_sey <- sey_values[2]
  val_sey <- sey_values[2]

  played$split <- "train"
  # Split the latest season temporally: first half = val, second half = test
  latest <- played[played$season_end_year == sey_values[2], ]
  latest <- latest[order(latest$match_date), ]
  midpoint <- ceiling(nrow(latest) / 2)
  val_ids <- latest$match_id[seq_len(midpoint)]
  test_ids <- latest$match_id[seq(midpoint + 1, nrow(latest))]
  played$split[played$match_id %in% val_ids] <- "val"
  played$split[played$match_id %in% test_ids] <- "test"

} else {
  # 1 season: temporal thirds (train/val/test)
  warning("Only 1 season available. Using temporal thirds within the season. ",
          "Model evaluation may be unreliable with this little data.", call. = FALSE)
  train_sey <- sey_values[1]
  val_sey <- sey_values[1]
  test_sey <- sey_values[1]

  played <- played[order(played$match_date), ]
  n_played <- nrow(played)
  train_end <- floor(n_played * 0.6)
  val_end <- floor(n_played * 0.8)
  played$split <- "test"
  played$split[seq_len(train_end)] <- "train"
  played$split[seq(train_end + 1, val_end)] <- "val"
}

fixtures$split <- "fixture"

match_dataset <- rbind(played, fixtures)
match_dataset <- match_dataset[order(match_dataset$match_date), ]

# 9. Encode Outcome Label ----

# 0 = Home Win, 1 = Draw, 2 = Away Win
match_dataset$outcome_label <- NA_integer_
match_dataset$outcome_label[match_dataset$result == "H"] <- 0L
match_dataset$outcome_label[match_dataset$result == "D"] <- 1L
match_dataset$outcome_label[match_dataset$result == "A"] <- 2L

# 9b. Signed venue feature ----
# home_field carries the home-advantage signal for the orientation-symmetric
# models: +1 = home_team is the real host, 0 = neutral, -1 = home_team is the
# visitor. Original rows are always +1 (or 0 for neutral games); the mirrored
# copy added in steps 05/06 has it negated by mirror_match_rows(). Once the
# models train on both orientations, ALL home advantage flows through this
# single feature instead of being smeared across the home_* columns.
match_dataset$home_field <- ifelse(
  !is.na(match_dataset$is_neutral_venue) & match_dataset$is_neutral_venue == 1,
  0L, 1L)

# 9c. World Cup 2026 host advantage ----
# The WC2026 feed flags every group game as is_neutral_venue == 1, but the
# three hosts (USA / Canada / Mexico) play their group games in their own
# country and genuinely have home advantage. Re-flag those games: host as
# home_team -> home_field +1, host as away_team -> -1, and mark them
# non-neutral. The hosts are in different groups so no host-vs-host group
# game exists. (Home-advantage magnitude is the model's learned home_field
# effect, calibrated from club football — a reasonable proxy for host edge.)
#
# Key by team_id (not name) because Opta has already served at least one
# variant for these teams ("USA" vs "United States" — see the fixture-name
# normalisation block in 01_build_fixture_results.R). Assert all three host
# IDs resolve in the WC2026 fixture set before applying the flag — silent
# host-name drift would otherwise zero the host advantage and quietly tank
# the US/Canada/Mexico projections.
is_wc26 <- match_dataset$league == WC2026_LEAGUE &
  match_dataset$season == WC2026_SEASON_LABEL
if (any(is_wc26)) {
  wc26_ids_seen <- unique(c(
    match_dataset$home_team_id[is_wc26],
    match_dataset$away_team_id[is_wc26]
  ))
  hosts_missing <- setdiff(WC2026_HOST_TEAM_IDS, wc26_ids_seen)
  if (length(hosts_missing) > 0) {
    stop(sprintf(
      "WC2026 host team_id(s) missing from fixture set: %s. Has Opta renamed a host? Refusing to publish without host-advantage flags.",
      paste(names(WC2026_HOST_TEAM_IDS)[match(hosts_missing, WC2026_HOST_TEAM_IDS)],
            collapse = ", ")
    ), call. = FALSE)
  }
}
host_is_home <- is_wc26 & match_dataset$home_team_id %in% WC2026_HOST_TEAM_IDS
host_is_away <- is_wc26 & match_dataset$away_team_id %in% WC2026_HOST_TEAM_IDS
match_dataset$is_neutral_venue[host_is_home | host_is_away] <- 0L
match_dataset$home_field[host_is_home] <- 1L
match_dataset$home_field[host_is_away] <- -1L
message(sprintf("  WC2026 host advantage flagged: %d games (%d host-home, %d host-away)",
                sum(host_is_home | host_is_away),
                sum(host_is_home), sum(host_is_away)))

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
