# 07_seasonal_ratings.R
# Fit RAPM, SPM, and xRAPM per season
#
# This script generates season-by-season ratings:
# 1. Seasonal SPM: Apply overall SPM models to season-aggregated box score stats
# 2. Seasonal RAPM: Fit base RAPM per season (no prior)
# 3. Seasonal xRAPM: Fit RAPM with season-specific SPM as prior
# 4. Output unified seasonal ratings tables for all three

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Lambda for seasonal ratings: "min" (default), "1se", or numeric value
# Higher values = more shrinkage toward prior (for xRAPM)
seasonal_lambda <- "min"

cat(sprintf("Using lambda = %s for seasonal ratings\n", seasonal_lambda))

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Player-splint records:", nrow(splint_data$players), "\n")

# Filter bad xG data (same as 04_rapm.R)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = 20, verbose = TRUE)
splint_data <- filter_result$splint_data

# Show available seasons
seasons <- sort(unique(splint_data$splints$season_end_year))
cat("\nAvailable seasons:", paste(seasons, collapse = ", "), "\n")

# 3. Load SPM Models ----

cat("\n=== Loading SPM Models ===\n")
cat("Using 50/50 Elastic Net + XGBoost blend\n")

# Get the fitted models (not the predictions - we'll predict per season)
offense_spm_glmnet <- spm_results$offense_spm_glmnet
offense_spm_xgb <- spm_results$offense_spm_xgb
defense_spm_glmnet <- spm_results$defense_spm_glmnet
defense_spm_xgb <- spm_results$defense_spm_xgb

# Validate models exist before expensive processing
required_models <- list(
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb
)

missing_models <- names(required_models)[sapply(required_models, is.null)]
if (length(missing_models) > 0) {
  stop(sprintf(
    "Missing required SPM models: %s\nRe-run 05_spm.R to generate O/D models with blending.",
    paste(missing_models, collapse = ", ")
  ))
}

cat("Offense models loaded: glmnet + xgb\n")
cat("Defense models loaded: glmnet + xgb\n")

# 4. Define Season Processing Function ----

fit_season_ratings <- function(splint_data, processed_data, season,
                               offense_spm_glmnet, offense_spm_xgb,
                               defense_spm_glmnet, defense_spm_xgb,
                               min_minutes_spm = 200, min_minutes_rapm = 200) {
  cat(sprintf("\n--- Season %d ---\n", season))

  # Filter splints to this season
  season_splints <- splint_data$splints[splint_data$splints$season_end_year == season, ]
  season_splint_ids <- season_splints$splint_id
  season_players <- splint_data$players[splint_data$players$splint_id %in% season_splint_ids, ]

  season_splint_data <- list(
    splints = season_splints,
    players = season_players,
    match_info = splint_data$match_info
  )

  cat(sprintf("  Splints: %d, Player-splint records: %d\n",
              nrow(season_splints), nrow(season_players)))

  # Check if we have enough data
  if (nrow(season_splints) < 100) {
    warning(sprintf("Season %d has only %d splints, skipping", season, nrow(season_splints)))
    return(NULL)
  }

  # --- Aggregate season-specific stats for SPM ---

  # Stats tables don't have a season column - extract from match_id
  # match_id format: "2017-2018_20170915_Bournemouth_Brighton"
  # Convert season_end_year (e.g., 2018) to season string (e.g., "2017-2018")
  season_str <- paste0(season - 1, "-", season)

  filter_to_season <- function(df, season_str) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df[extract_season_from_match_id(df$match_id) == season_str, ]
  }

  season_stats <- list(
    stats_summary = filter_to_season(processed_data$stats_summary, season_str),
    stats_passing = filter_to_season(processed_data$stats_passing, season_str),
    stats_defense = filter_to_season(processed_data$stats_defense, season_str),
    stats_possession = filter_to_season(processed_data$stats_possession, season_str),
    stats_misc = filter_to_season(processed_data$stats_misc, season_str),
    stats_passing_types = filter_to_season(processed_data$stats_passing_types, season_str),
    stats_keeper = filter_to_season(processed_data$stats_keeper, season_str)
  )

  cat(sprintf("  Season string: %s\n", season_str))

  cat(sprintf("  Season stats rows: %d\n",
              if (!is.null(season_stats$stats_summary)) nrow(season_stats$stats_summary) else 0))

  # Aggregate player stats for this season
  season_player_stats <- aggregate_player_stats(
    stats_summary = season_stats$stats_summary,
    stats_passing = season_stats$stats_passing,
    stats_defense = season_stats$stats_defense,
    stats_possession = season_stats$stats_possession,
    stats_misc = season_stats$stats_misc,
    stats_passing_types = season_stats$stats_passing_types,
    stats_keeper = season_stats$stats_keeper,
    min_minutes = min_minutes_spm
  )

  cat(sprintf("  Players with aggregated stats: %d\n", nrow(season_player_stats)))

  # --- Calculate season-specific SPM predictions ---

  # Offense SPM (blended)
  off_glmnet <- calculate_spm_ratings(season_player_stats, offense_spm_glmnet)
  off_xgb <- calculate_spm_ratings_xgb(season_player_stats, offense_spm_xgb)

  offense_spm_season <- off_glmnet %>%
    rename(off_glmnet = spm) %>%
    inner_join(off_xgb %>% select(player_id, off_xgb = spm), by = "player_id") %>%
    mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

  # Defense SPM (blended)
  def_glmnet <- calculate_spm_ratings(season_player_stats, defense_spm_glmnet)
  def_xgb <- calculate_spm_ratings_xgb(season_player_stats, defense_spm_xgb)

  defense_spm_season <- def_glmnet %>%
    rename(def_glmnet = spm) %>%
    inner_join(def_xgb %>% select(player_id, def_xgb = spm), by = "player_id") %>%
    mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

  cat(sprintf("  Season SPM predictions: %d offense, %d defense\n",
              nrow(offense_spm_season), nrow(defense_spm_season)))

  # --- Create seasonal SPM ratings table ---
  seasonal_spm <- offense_spm_season %>%
    select(player_id, player_name, total_minutes, offense_spm) %>%
    inner_join(
      defense_spm_season %>% select(player_id, defense_spm),
      by = "player_id"
    ) %>%
    mutate(
      spm = offense_spm - defense_spm,
      season_end_year = season
    ) %>%
    arrange(desc(spm))

  cat(sprintf("  Seasonal SPM ratings: %d players\n", nrow(seasonal_spm)))

  # Store season SPM data for prior building (used after RAPM data is prepared)
  # We'll use build_prior_vector() with the data frames rather than lookup vectors

  # --- Prepare RAPM data for this season ---

  rapm_data <- prepare_rapm_data(
    season_splint_data,
    min_minutes = min_minutes_rapm,
    include_covariates = TRUE
  )

  cat(sprintf("  Players meeting RAPM min_minutes (%d): %d\n",
              min_minutes_rapm, rapm_data$n_players))

  # Check if we have enough players
  if (rapm_data$n_players < 50) {
    warning(sprintf("Season %d has only %d players, skipping", season, rapm_data$n_players))
    return(NULL)
  }

  # Use fewer folds if not enough observations
  n_folds <- min(10, floor(nrow(rapm_data$X) / 20))
  n_folds <- max(n_folds, 3)  # At least 3 folds

  # --- Fit base RAPM (no prior) ---

  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds, use_weights = TRUE)
  seasonal_rapm <- extract_rapm_ratings(rapm_model, lambda = seasonal_lambda)
  seasonal_rapm$season_end_year <- season

  cat(sprintf("  Seasonal RAPM ratings: %d players\n", nrow(seasonal_rapm)))

  # --- Build prior vectors for xRAPM ---

  player_mapping <- rapm_data$player_mapping

  # Build priors using helper function (replaces manual for-loops)
  offense_prior <- build_prior_vector(
    spm_data = offense_spm_season,
    spm_col = "offense_spm",
    player_mapping = player_mapping
  )

  defense_prior <- build_prior_vector(
    spm_data = defense_spm_season,
    spm_col = "defense_spm",
    player_mapping = player_mapping
  )

  cat(sprintf("  Matched season SPM priors: %d offense, %d defense\n",
              sum(offense_prior != 0), sum(defense_prior != 0)))

  # --- Fit xRAPM with season-specific SPM prior ---

  xrapm_model <- fit_rapm_with_prior(
    rapm_data,
    offense_prior = offense_prior,
    defense_prior = defense_prior,
    alpha = 0,           # Ridge regression
    nfolds = n_folds,
    use_weights = TRUE,
    penalize_covariates = FALSE
  )

  # Extract xRAPM ratings
  seasonal_xrapm <- extract_xrapm_ratings(xrapm_model, lambda = seasonal_lambda)
  seasonal_xrapm$season_end_year <- season

  cat(sprintf("  Seasonal xRAPM ratings: %d players\n", nrow(seasonal_xrapm)))

  # Return all three rating types
  list(
    spm = seasonal_spm,
    rapm = seasonal_rapm,
    xrapm = seasonal_xrapm
  )
}

# 5. Process All Seasons ----

cat("\n=== Processing All Seasons ===\n")
cat(sprintf("Processing %d seasons: %s\n",
            length(seasons), paste(seasons, collapse = ", ")))

# Process each season
seasonal_ratings_list <- lapply(seasons, function(season) {
  tryCatch({
    fit_season_ratings(
      splint_data = splint_data,
      processed_data = processed_data,
      season = season,
      offense_spm_glmnet = offense_spm_glmnet,
      offense_spm_xgb = offense_spm_xgb,
      defense_spm_glmnet = defense_spm_glmnet,
      defense_spm_xgb = defense_spm_xgb,
      min_minutes_spm = 200,
      min_minutes_rapm = 200
    )
  }, error = function(e) {
    warning(sprintf("Failed to process season %d: %s", season, e$message))
    NULL
  })
})

# Remove NULL entries (failed seasons)
seasonal_ratings_list <- Filter(Negate(is.null), seasonal_ratings_list)

# Combine each rating type into single data frames
seasonal_spm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "spm"))
seasonal_rapm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "rapm"))
seasonal_xrapm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "xrapm"))

cat(sprintf("\n=== Combined Results ===\n"))
cat(sprintf("Seasons processed: %d\n", length(seasonal_ratings_list)))
cat(sprintf("Seasonal SPM:  %d player-seasons, %d unique players\n",
            nrow(seasonal_spm), n_distinct(seasonal_spm$player_name)))
cat(sprintf("Seasonal RAPM: %d player-seasons, %d unique players\n",
            nrow(seasonal_rapm), n_distinct(seasonal_rapm$player_name)))
cat(sprintf("Seasonal xRAPM: %d player-seasons, %d unique players\n",
            nrow(seasonal_xrapm), n_distinct(seasonal_xrapm$player_name)))

# 6. Summary Statistics ----

cat("\n=== Top Players by Season (xRAPM) ===\n")

for (s in sort(unique(seasonal_xrapm$season_end_year))) {
  cat(sprintf("\nTop 10 xRAPM - Season %d:\n", s))
  print(
    seasonal_xrapm %>%
      filter(season_end_year == s) %>%
      arrange(desc(xrapm)) %>%
      head(10) %>%
      select(player_name, xrapm, offense, defense, off_prior, def_prior, total_minutes)
  )
}

cat("\n=== Top Players by Season (SPM) ===\n")

for (s in sort(unique(seasonal_spm$season_end_year))) {
  cat(sprintf("\nTop 10 SPM - Season %d:\n", s))
  print(
    seasonal_spm %>%
      filter(season_end_year == s) %>%
      arrange(desc(spm)) %>%
      head(10) %>%
      select(player_name, spm, offense_spm, defense_spm, total_minutes)
  )
}

cat("\n=== Top Players by Season (RAPM) ===\n")

for (s in sort(unique(seasonal_rapm$season_end_year))) {
  cat(sprintf("\nTop 10 RAPM - Season %d:\n", s))
  print(
    seasonal_rapm %>%
      filter(season_end_year == s) %>%
      arrange(desc(rapm)) %>%
      head(10) %>%
      select(player_name, rapm, offense, defense, total_minutes)
  )
}

# Player consistency across seasons (xRAPM)
cat("\n=== Player Consistency Across Seasons (xRAPM) ===\n")

player_season_counts <- seasonal_xrapm %>%
  group_by(player_name) %>%
  summarise(
    n_seasons = n(),
    seasons = paste(season_end_year, collapse = ", "),
    mean_xrapm = mean(xrapm),
    sd_xrapm = sd(xrapm),
    total_minutes = sum(total_minutes),
    .groups = "drop"
  ) %>%
  arrange(desc(n_seasons), desc(mean_xrapm))

cat("\nPlayers with most seasons:\n")
print(head(player_season_counts, 20))

cat("\nMost consistent multi-season players (low SD):\n")
print(
  player_season_counts %>%
    filter(n_seasons >= 3) %>%
    arrange(sd_xrapm) %>%
    head(15)
)

# 7. Compare with Overall xRAPM ----

cat("\n=== Comparison with Overall xRAPM ===\n")

overall_xrapm_file <- file.path(cache_dir, "06_xrapm.rds")
if (file.exists(overall_xrapm_file)) {
  overall_results <- readRDS(overall_xrapm_file)
  overall_ratings <- overall_results$ratings

  # Compare mean seasonal to overall
  seasonal_means <- seasonal_xrapm %>%
    group_by(player_name) %>%
    summarise(
      mean_seasonal_xrapm = mean(xrapm),
      n_seasons = n(),
      .groups = "drop"
    )

  comparison <- seasonal_means %>%
    inner_join(
      overall_ratings %>% select(player_name, overall_xrapm = xrapm),
      by = "player_name"
    ) %>%
    mutate(diff = mean_seasonal_xrapm - overall_xrapm)

  cat(sprintf("Players in both: %d\n", nrow(comparison)))
  cat(sprintf("Correlation (mean seasonal vs overall): %.3f\n",
              cor(comparison$mean_seasonal_xrapm, comparison$overall_xrapm)))

  cat("\nLargest differences (seasonal mean vs overall):\n")
  print(
    comparison %>%
      arrange(desc(abs(diff))) %>%
      head(15) %>%
      select(player_name, mean_seasonal_xrapm, overall_xrapm, diff, n_seasons)
  )
}

# 8. Save Results ----

cat("\n=== Saving Results ===\n")

seasonal_results <- list(
  seasonal_spm = seasonal_spm,
  seasonal_rapm = seasonal_rapm,
  seasonal_xrapm = seasonal_xrapm,
  player_season_counts = player_season_counts,
  seasons = seasons,
  metadata = list(
    min_minutes_spm = 200,
    min_minutes_rapm = 200,
    lambda = seasonal_lambda,
    n_seasons = length(seasonal_ratings_list),
    spm_player_seasons = nrow(seasonal_spm),
    rapm_player_seasons = nrow(seasonal_rapm),
    xrapm_player_seasons = nrow(seasonal_xrapm),
    created = Sys.time()
  )
)

saveRDS(seasonal_results, file.path(cache_dir, "07_seasonal_ratings.rds"))
cat("Saved to cache/07_seasonal_ratings.rds\n")

# Export CSVs for easy viewing
write.csv(
  seasonal_spm %>%
    select(season_end_year, player_name, spm, offense_spm, defense_spm, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(spm)),
  file.path(cache_dir, "seasonal_spm.csv"),
  row.names = FALSE
)
cat("Saved to cache/seasonal_spm.csv\n")

write.csv(
  seasonal_rapm %>%
    select(season_end_year, player_name, rapm, offense, defense, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(rapm)),
  file.path(cache_dir, "seasonal_rapm.csv"),
  row.names = FALSE
)
cat("Saved to cache/seasonal_rapm.csv\n")

write.csv(
  seasonal_xrapm %>%
    select(season_end_year, player_name, xrapm, offense, defense,
           off_deviation, def_deviation, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(xrapm)),
  file.path(cache_dir, "seasonal_xrapm.csv"),
  row.names = FALSE
)
cat("Saved to cache/seasonal_xrapm.csv\n")

cat("\n=== COMPLETE ===\n")
