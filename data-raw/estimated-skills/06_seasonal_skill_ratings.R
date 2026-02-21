# 06_seasonal_skill_ratings.R
# Fit seasonal RAPM with skill-based SPM prior → seasonal skill-based xRAPM
#
# For each season, computes skill-based SPM predictions using the pre-trained
# skill SPM models (from step 03), then uses those as Bayesian priors for
# seasonal RAPM fitting. Produces output matching cache-opta/07_seasonal_ratings.rds
# so match predictions can swap in skill-based ratings.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
seasonal_lambda <- "min"

cat(sprintf("Using lambda = %s for seasonal ratings\n", seasonal_lambda))

# Helper: extract season end year from both standard and tournament formats
extract_season_end_year <- function(season) {
  if (grepl("^\\d{4}-\\d{4}$", season)) return(as.numeric(substr(season, 6, 9)))
  year <- as.numeric(sub("^(\\d{4}).*", "\\1", season))
  if (!is.na(year)) return(year)
  NA_real_
}

# 3. Load Data ----

cat("\n=== Loading Data ===\n")

# Splints for seasonal RAPM fitting
splint_data <- readRDS(file.path(opta_cache_dir, "03_splints.rds"))

# Skill features (per player-season from step 02)
skill_features <- readRDS(file.path(cache_dir, "02_skill_features.rds"))

# Skill SPM models (from step 03)
spm_results <- readRDS(file.path(cache_dir, "03_skill_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Skill feature rows:", nrow(skill_features), "\n")

# Filter bad xG data (same threshold as Opta pipeline)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = 30, verbose = TRUE)
splint_data <- filter_result$splint_data

seasons <- sort(unique(splint_data$splints$season_end_year))
cat("\nAvailable seasons:", paste(seasons, collapse = ", "), "\n")

# 4. Load SPM Models ----

cat("\n=== Loading Skill SPM Models ===\n")
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
    "Missing required skill SPM models: %s\nRe-run 03_skill_spm.R to generate O/D models.",
    paste(missing_models, collapse = ", ")
  ))
}

# 5. Define Season Processing Function ----

fit_season_skill_ratings <- function(splint_data, skill_features, season,
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

  if (nrow(season_splints) < 100) {
    warning(sprintf("Season %d has only %d splints, skipping", season, nrow(season_splints)))
    return(NULL)
  }

  # Get skill features for this season
  season_skills <- skill_features[skill_features$season_end_year == season, ]

  cat(sprintf("  Players with skill features: %d\n", nrow(season_skills)))

  if (nrow(season_skills) == 0) {
    warning(sprintf("Season %d has no skill features, skipping", season))
    return(NULL)
  }

  # Ensure required columns for SPM prediction
  if (!"mins_per_90" %in% names(season_skills)) {
    season_skills$mins_per_90 <- season_skills$total_minutes / 90
  }

  # Ensure player_name and total_minutes exist for SPM model compatibility
  if (!"player_name" %in% names(season_skills)) {
    season_skills$player_name <- season_skills$player_id
  }
  if (!"total_minutes" %in% names(season_skills)) {
    season_skills$total_minutes <- 0
  }

  # Calculate season-specific SPM predictions using skill SPM models (blended)
  off_glmnet <- calculate_spm_ratings(season_skills, offense_spm_glmnet)
  off_xgb <- calculate_spm_ratings_xgb(season_skills, offense_spm_xgb)

  offense_spm_season <- off_glmnet %>%
    rename(off_glmnet = spm) %>%
    inner_join(off_xgb %>% select(player_id, off_xgb = spm), by = "player_id") %>%
    mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

  def_glmnet <- calculate_spm_ratings(season_skills, defense_spm_glmnet)
  def_xgb <- calculate_spm_ratings_xgb(season_skills, defense_spm_xgb)

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

  cat(sprintf("  Matched skill SPM priors: %d offense, %d defense\n",
              sum(offense_prior != 0), sum(defense_prior != 0)))

  # Fit xRAPM with skill-based SPM prior
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

# 6. Process All Seasons ----

cat("\n=== Processing All Seasons ===\n")
cat(sprintf("Processing %d seasons: %s\n",
            length(seasons), paste(seasons, collapse = ", ")))

seasonal_ratings_list <- lapply(seasons, function(season) {
  tryCatch({
    fit_season_skill_ratings(
      splint_data = splint_data,
      skill_features = skill_features,
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

# 7. Summary Statistics ----

cat("\n=== Top Players by Season (Skill xRAPM) ===\n")

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

# 8. Compare with Raw Seasonal Ratings ----

cat("\n=== Comparison with Raw-Stat Seasonal Ratings ===\n")

raw_seasonal_path <- file.path(opta_cache_dir, "07_seasonal_ratings.rds")
if (file.exists(raw_seasonal_path)) {
  raw_seasonal <- readRDS(raw_seasonal_path)
  raw_xrapm <- raw_seasonal$seasonal_xrapm

  comp <- seasonal_xrapm %>%
    select(player_name, season_end_year, skill_xrapm = xrapm) %>%
    inner_join(
      raw_xrapm %>% select(player_name, season_end_year, raw_xrapm = xrapm),
      by = c("player_name", "season_end_year")
    )

  if (nrow(comp) > 0) {
    cat(sprintf("Overlapping player-seasons: %d\n", nrow(comp)))
    cat(sprintf("Skill vs Raw seasonal xRAPM: r = %.3f\n",
                cor(comp$skill_xrapm, comp$raw_xrapm)))

    # Per-season correlations
    for (s in sort(unique(comp$season_end_year))) {
      sc <- comp[comp$season_end_year == s, ]
      if (nrow(sc) > 10) {
        cat(sprintf("  Season %d: r = %.3f (n=%d)\n", s, cor(sc$skill_xrapm, sc$raw_xrapm), nrow(sc)))
      }
    }
  }
}

# 9. Player Consistency ----

cat("\n=== Player Consistency Across Seasons (Skill xRAPM) ===\n")

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

# 10. Save Results ----

cat("\n=== Saving Results ===\n")

seasonal_results <- list(
  seasonal_spm = seasonal_spm,
  seasonal_rapm = seasonal_rapm,
  seasonal_xrapm = seasonal_xrapm,
  player_season_counts = player_season_counts,
  seasons = seasons,
  metadata = list(
    source = "skill_based",
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

saveRDS(seasonal_results, file.path(cache_dir, "06_seasonal_ratings.rds"))
cat("Saved to cache-skills/06_seasonal_ratings.rds\n")

# Export CSVs
write.csv(
  seasonal_xrapm %>%
    select(season_end_year, player_name, xrapm, offense, defense,
           any_of(c("off_deviation", "def_deviation")), total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(xrapm)),
  file.path(cache_dir, "seasonal_skill_xrapm.csv"),
  row.names = FALSE
)

cat("\n=== COMPLETE ===\n")
