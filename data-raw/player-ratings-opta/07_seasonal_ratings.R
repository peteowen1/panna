# 07_seasonal_ratings.R
# Fit RAPM, SPM, and xRAPM per season for Opta data
#
# Uses aggregate_opta_stats() for season-level SPM aggregation.
# Season filtering uses Opta's season column from the data.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# Helper: extract season end year from both standard and tournament formats
# "2024-2025" -> 2025, "2018 Russia" -> 2018
extract_season_end_year <- function(season) {
  if (grepl("^\\d{4}-\\d{4}$", season)) return(as.numeric(substr(season, 6, 9)))
  year <- as.numeric(sub("^(\\d{4}).*", "\\1", season))
  if (!is.na(year)) return(year)
  NA_real_
}

seasonal_lambda <- "min"
cat(sprintf("Using lambda = %s for seasonal ratings\n", seasonal_lambda))

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Player-splint records:", nrow(splint_data$players), "\n")

# Filter bad xG data (higher threshold for SPADL-derived xG)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = 30, verbose = TRUE)
splint_data <- filter_result$splint_data

seasons <- sort(unique(splint_data$splints$season_end_year))
cat("\nAvailable seasons:", paste(seasons, collapse = ", "), "\n")

# 3. Load SPM Models ----

cat("\n=== Loading SPM Models ===\n")
cat("Using 50/50 Elastic Net + XGBoost blend\n")

offense_spm_glmnet <- spm_results$offense_spm_glmnet
offense_spm_xgb <- spm_results$offense_spm_xgb
defense_spm_glmnet <- spm_results$defense_spm_glmnet
defense_spm_xgb <- spm_results$defense_spm_xgb

required_models <- list(
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb
)

missing_models <- names(required_models)[sapply(required_models, is.null)]
if (length(missing_models) > 0) {
  stop(sprintf(
    "Missing required SPM models: %s\nRe-run 05_spm.R to generate O/D models.",
    paste(missing_models, collapse = ", ")
  ))
}

# 4. Define Season Processing Function ----

fit_season_ratings_opta <- function(splint_data, opta_stats, season,
                                     offense_spm_glmnet, offense_spm_xgb,
                                     defense_spm_glmnet, defense_spm_xgb,
                                     opta_xmetrics = NULL,
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

  if (nrow(season_splints) < 100) {
    warning(sprintf("Season %d has only %d splints, skipping", season, nrow(season_splints)))
    return(NULL)
  }

  # Filter Opta stats to this season
  # Handle both standard ("2024-2025") and tournament ("2018 Russia") season formats
  season_str <- paste0(season - 1, "-", season)
  season_opta_stats <- opta_stats[opta_stats$season == season_str, ]

  # If no match with standard format, try matching by extracted end year
  if (nrow(season_opta_stats) == 0) {
    end_years <- sapply(unique(opta_stats$season), extract_season_end_year)
    matching_seasons <- names(end_years)[end_years == season]
    if (length(matching_seasons) > 0) {
      season_opta_stats <- opta_stats[opta_stats$season %in% matching_seasons, ]
    }
  }

  cat(sprintf("  Season Opta stats rows: %d\n", nrow(season_opta_stats)))

  if (is.null(season_opta_stats) || nrow(season_opta_stats) == 0) {
    warning(sprintf("Season %d has no Opta stats, skipping", season))
    return(NULL)
  }

  # Aggregate season-specific Opta stats
  season_player_stats <- aggregate_opta_stats(
    season_opta_stats,
    min_minutes = min_minutes_spm
  )

  cat(sprintf("  Players with aggregated stats: %d\n", nrow(season_player_stats)))

  # Enrich with xMetrics if available (SPM models may require these features)
  if (!is.null(opta_xmetrics) && nrow(opta_xmetrics) > 0) {
    season_xm <- opta_xmetrics[opta_xmetrics$season == season_str, ]
    # Fallback: match by extracted end year for tournament formats
    if (nrow(season_xm) == 0) {
      xm_end_years <- sapply(unique(opta_xmetrics$season), extract_season_end_year)
      xm_matching <- names(xm_end_years)[xm_end_years == season]
      if (length(xm_matching) > 0) {
        season_xm <- opta_xmetrics[opta_xmetrics$season %in% xm_matching, ]
      }
    }
    if (nrow(season_xm) > 0) {
      xm_agg <- season_xm %>%
        group_by(player_id) %>%
        summarise(
          xg_total = sum(xg, na.rm = TRUE),
          npxg_total = sum(npxg, na.rm = TRUE),
          xa_total = sum(xa, na.rm = TRUE),
          xmetrics_minutes = sum(minutes, na.rm = TRUE),
          xpass_overperformance_total = sum(xpass_overperformance, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(xmetrics_minutes > 0) %>%
        mutate(
          xg_per90 = xg_total / xmetrics_minutes * 90,
          npxg_per90 = npxg_total / xmetrics_minutes * 90,
          xa_per90_xmetrics = xa_total / xmetrics_minutes * 90,
          xpass_overperformance_per90_xmetrics = xpass_overperformance_total / xmetrics_minutes * 90
        )

      season_player_stats <- season_player_stats %>%
        left_join(
          xm_agg %>% select(player_id, xg_per90, npxg_per90,
                             xa_per90_xmetrics, xpass_overperformance_per90_xmetrics),
          by = "player_id"
        )

      xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics", "xpass_overperformance_per90_xmetrics")
      for (col in xm_cols) {
        season_player_stats[[col]][is.na(season_player_stats[[col]])] <- 0
      }
    }
  }

  # Ensure xMetrics columns exist (even if no xMetrics data) for SPM model compatibility
  xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics", "xpass_overperformance_per90_xmetrics")
  for (col in xm_cols) {
    if (!col %in% names(season_player_stats)) {
      season_player_stats[[col]] <- 0
    }
  }

  # Calculate season-specific SPM predictions (blended)
  off_glmnet <- calculate_spm_ratings(season_player_stats, offense_spm_glmnet)
  off_xgb <- calculate_spm_ratings_xgb(season_player_stats, offense_spm_xgb)

  offense_spm_season <- off_glmnet %>%
    rename(off_glmnet = spm) %>%
    inner_join(off_xgb %>% select(player_id, off_xgb = spm), by = "player_id") %>%
    mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

  def_glmnet <- calculate_spm_ratings(season_player_stats, defense_spm_glmnet)
  def_xgb <- calculate_spm_ratings_xgb(season_player_stats, defense_spm_xgb)

  defense_spm_season <- def_glmnet %>%
    rename(def_glmnet = spm) %>%
    inner_join(def_xgb %>% select(player_id, def_xgb = spm), by = "player_id") %>%
    mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

  cat(sprintf("  Season SPM predictions: %d offense, %d defense\n",
              nrow(offense_spm_season), nrow(defense_spm_season)))

  # Create seasonal SPM ratings table
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

  # Prepare RAPM data for this season
  rapm_data <- prepare_rapm_data(
    season_splint_data,
    min_minutes = min_minutes_rapm,
    include_covariates = TRUE
  )

  cat(sprintf("  Players meeting RAPM min_minutes (%d): %d\n",
              min_minutes_rapm, rapm_data$n_players))

  if (rapm_data$n_players < 50) {
    warning(sprintf("Season %d has only %d players, skipping", season, rapm_data$n_players))
    return(NULL)
  }

  n_folds <- min(10, floor(nrow(rapm_data$X) / 20))
  n_folds <- max(n_folds, 3)

  # Fit base RAPM
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds, use_weights = TRUE)
  seasonal_rapm <- extract_rapm_ratings(rapm_model, lambda = seasonal_lambda)
  seasonal_rapm$season_end_year <- season

  cat(sprintf("  Seasonal RAPM ratings: %d players\n", nrow(seasonal_rapm)))

  # Build prior vectors for xRAPM
  player_mapping <- rapm_data$player_mapping

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

  # Fit xRAPM with season-specific SPM prior
  xrapm_model <- fit_rapm_with_prior(
    rapm_data,
    offense_prior = offense_prior,
    defense_prior = defense_prior,
    alpha = 0,
    nfolds = n_folds,
    use_weights = TRUE,
    penalize_covariates = FALSE
  )

  seasonal_xrapm <- extract_xrapm_ratings(xrapm_model, lambda = seasonal_lambda)
  seasonal_xrapm$season_end_year <- season

  cat(sprintf("  Seasonal xRAPM ratings: %d players\n", nrow(seasonal_xrapm)))

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

opta_stats <- processed_data$opta_stats
opta_xmetrics <- processed_data$opta_xmetrics

seasonal_ratings_list <- lapply(seasons, function(season) {
  tryCatch({
    fit_season_ratings_opta(
      splint_data = splint_data,
      opta_stats = opta_stats,
      season = season,
      offense_spm_glmnet = offense_spm_glmnet,
      offense_spm_xgb = offense_spm_xgb,
      defense_spm_glmnet = defense_spm_glmnet,
      defense_spm_xgb = defense_spm_xgb,
      opta_xmetrics = opta_xmetrics,
      min_minutes_spm = 200,
      min_minutes_rapm = 200
    )
  }, error = function(e) {
    warning(sprintf("Failed to process season %d: %s", season, e$message))
    NULL
  })
})

seasonal_ratings_list <- Filter(Negate(is.null), seasonal_ratings_list)

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

# Player consistency
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

# 7. Save Results ----

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
cat("Saved to cache-opta/07_seasonal_ratings.rds\n")

# Export CSVs
write.csv(
  seasonal_spm %>%
    select(season_end_year, player_id, player_name, spm, offense_spm, defense_spm, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(spm)),
  file.path(cache_dir, "seasonal_spm.csv"),
  row.names = FALSE
)

write.csv(
  seasonal_rapm %>%
    select(season_end_year, player_name, rapm, offense, defense, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(rapm)),
  file.path(cache_dir, "seasonal_rapm.csv"),
  row.names = FALSE
)

write.csv(
  seasonal_xrapm %>%
    select(season_end_year, player_name, xrapm, offense, defense,
           off_deviation, def_deviation, total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(xrapm)),
  file.path(cache_dir, "seasonal_xrapm.csv"),
  row.names = FALSE
)

cat("\n=== COMPLETE ===\n")
