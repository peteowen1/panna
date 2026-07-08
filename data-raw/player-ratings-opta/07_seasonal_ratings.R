# 07_seasonal_ratings.R
# Fit RAPM, SPM, and xRAPM per season for Opta data
#
# Uses aggregate_opta_stats() for season-level SPM aggregation.
# Season filtering uses Opta's season column from the data.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# extract_season_end_year() is defined in R/utils.R

seasonal_lambda <- "min"
cat(sprintf("Using lambda = %s for seasonal ratings\n", seasonal_lambda))

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

# panna#87: opta_stats/opta_xmetrics live in their OWN file (02_opta_stats.rds)
# as of the step-02 split. Narrowing AFTER loading the monolithic
# 02_processed_data.rds was insufficient — readRDS() must deserialize the
# WHOLE object graph (stats + xmetrics + lineups + shooting + results +
# stats_summary) before returning anything, so the peak happened inside the
# readRDS() call itself. Confirmed live: this step OOM'd during "=== Loading
# Data ===" (run 28921032951) one step after 05_spm's identical load left
# only ~110MB of 16GB free (run 28920296396). Loading the narrow file
# directly removes the peak instead of shrinking what's kept after it.
opta_stats_bundle <- readRDS(file.path(cache_dir, "02_opta_stats.rds"))
opta_stats <- opta_stats_bundle$opta_stats
opta_xmetrics <- opta_stats_bundle$opta_xmetrics
rm(opta_stats_bundle); gc(verbose = FALSE)

spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Player-splint records:", nrow(splint_data$players), "\n")

# Filter bad xG data (higher threshold for SPADL-derived xG)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = TRUE)
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

# Free memory
rm(spm_results); gc(verbose = FALSE)

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
  # Select by extracted end year, not exact "YYYY-YYYY" match: calendar-year
  # leagues (MLS/Argentina/Brazil, label "2026") and tournaments ("2026
  # Canada-Mexico-USA") share an end year with the European "2025-2026"
  # season. The old exact-match-then-fallback logic only reached the
  # fallback when the standard format matched NOTHING — so calendar-league
  # stats were silently excluded from every season's SPM build (the blog's
  # 25% missing-SPM gap, found 2026-06-12).
  end_years <- sapply(unique(opta_stats$season), extract_season_end_year)
  matching_seasons <- names(end_years)[end_years == season]
  if (length(matching_seasons) > 1) {
    cat(sprintf("  Note: %d seasons match end year %d: %s\n",
                length(matching_seasons), season, paste(matching_seasons, collapse = ", ")))
  }
  season_opta_stats <- opta_stats[opta_stats$season %in% matching_seasons, ]

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

  # Enrich with xMetrics if available (SPM models may require these features).
  # panna#87: uses the SAME .aggregate_xmetrics_for_spm() as 05_spm.R (shared
  # in R/spm_opta.R) — this block used to be an independent, near-identical
  # copy that never got the WOE/finishing columns added to the other, which
  # is exactly why every one of 14 seasons failed with "undefined columns
  # selected" the moment the fitted SPM model's predictor_cols grew to
  # include them.
  season_xm <- NULL
  if (!is.null(opta_xmetrics) && nrow(opta_xmetrics) > 0) {
    # End-year matching for the same reason as the stats subset above —
    # calendar-year league labels never equal the "YYYY-YYYY" season_str.
    xm_end_years <- sapply(unique(opta_xmetrics$season), extract_season_end_year)
    xm_matching <- names(xm_end_years)[xm_end_years == season]
    season_xm <- opta_xmetrics[opta_xmetrics$season %in% xm_matching, ]
    if (nrow(season_xm) > 0) {
      xm_agg <- .aggregate_xmetrics_for_spm(season_xm)
      season_player_stats <- season_player_stats %>%
        left_join(xm_agg, by = "player_id")
      for (col in intersect(names(xm_agg), names(season_player_stats))) {
        if (col == "player_id") next
        season_player_stats[[col]][is.na(season_player_stats[[col]])] <- 0
      }
    }
  }

  # Enrich with chain features if available in xMetrics
  if (!is.null(opta_xmetrics) && nrow(opta_xmetrics) > 0) {
    chain_cols_avail <- c("chains_involved", "chain_actions", "successful_chains",
                          "chain_goals", "chain_starts", "chain_xg")
    if (any(chain_cols_avail %in% names(season_xm %||% opta_xmetrics))) {
      xm_source <- if (exists("season_xm") && nrow(season_xm) > 0) season_xm else NULL
      if (!is.null(xm_source) && any(chain_cols_avail %in% names(xm_source))) {
        chain_agg <- xm_source %>%
          filter(minutes > 0) %>%
          group_by(player_id) %>%
          summarise(
            chains_total = sum(chains_involved, na.rm = TRUE),
            chain_actions_total = sum(chain_actions, na.rm = TRUE),
            successful_chains_total = sum(successful_chains, na.rm = TRUE),
            chain_goals_total = sum(chain_goals, na.rm = TRUE),
            chain_starts_total = sum(chain_starts, na.rm = TRUE),
            chain_xg_total = sum(chain_xg, na.rm = TRUE),
            chain_minutes = sum(minutes, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          filter(chain_minutes > 0) %>%
          mutate(
            chains_p90 = chains_total / chain_minutes * 90,
            chain_shot_pct = ifelse(chains_total > 0, successful_chains_total / chains_total, 0),
            chain_goal_pct = ifelse(chains_total > 0, chain_goals_total / chains_total, 0),
            chain_starts_p90 = chain_starts_total / chain_minutes * 90,
            avg_actions_per_chain = ifelse(chains_total > 0, chain_actions_total / chains_total, 0),
            chain_xg_p90 = chain_xg_total / chain_minutes * 90
          )

        chain_join_cols <- c("player_id", "chains_p90", "chain_shot_pct", "chain_goal_pct",
                             "chain_starts_p90", "avg_actions_per_chain", "chain_xg_p90")
        chain_join_cols <- intersect(chain_join_cols, names(chain_agg))
        season_player_stats <- season_player_stats %>%
          left_join(chain_agg %>% select(all_of(chain_join_cols)), by = "player_id")

        for (col in setdiff(chain_join_cols, "player_id")) {
          season_player_stats[[col]][is.na(season_player_stats[[col]])] <- 0
        }
      }
    }
  }

  # Ensure xMetrics and chain columns exist (even if no data) for SPM model
  # compatibility. panna#87: uses the CANONICAL full column list
  # (.spm_xmetrics_per90_cols(), shared with 05_spm.R) rather than a
  # hand-maintained subset — a thin season whose xmetrics coverage misses
  # some above-expected columns still gets every column the fitted model's
  # predictor_cols can reference, defaulted to 0 (population mean).
  chain_feat_cols <- c("chains_p90", "chain_shot_pct", "chain_goal_pct",
                       "chain_starts_p90", "avg_actions_per_chain", "chain_xg_p90")
  for (col in c(.spm_xmetrics_per90_cols(), chain_feat_cols)) {
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

  # Fit base RAPM. parallel = FALSE for the same reason as step 04 — 2-core
  # parallel CV doubles peak memory by forking the design matrix per worker.
  # Step 07 fits 12+ seasons sequentially (one per loop iter); without
  # parallel = FALSE, accumulated peak across iters trips the 7 GB ceiling.
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds,
                          use_weights = TRUE, parallel = FALSE)
  seasonal_rapm <- extract_rapm_ratings(rapm_model, lambda = seasonal_lambda)
  seasonal_rapm$season_end_year <- season

  # Free rapm_model before xRAPM fits another cv.glmnet on the same X.
  # Ridge (alpha=0) glmnet stores a dense coef path (~ncol(X) × ~100
  # lambdas × 8B per fold-aggregated entry), which is hundreds of MB per
  # fit; holding two simultaneously OOM-kills the 7 GB ubuntu runner on
  # large seasons (2024+).
  rm(rapm_model); inv <- gc(verbose = FALSE)
  cat(sprintf("  [mem] after rm(rapm_model): %.0f MB used\n",
              sum(inv[, 2])))  # col 2 = current used Mb (Ncells + Vcells)

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

  # Probe peak memory after xRAPM fit so the next run's log lets us
  # confirm (or rule out) OOM-from-double-fit as the cause of the
  # 05-10/05-13/05-14 runner-shutdown failures.
  inv <- gc(verbose = FALSE)
  cat(sprintf("  [mem] after xRAPM fit (season %d): %.0f MB used\n",
              season, sum(inv[, 2])))

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

# opta_stats/opta_xmetrics already extracted at load time (panna#87) —
# processed_data itself was freed right after.

# panna#87: extract-and-SHRINK loop. Confirmed live in two stages:
# (1) run 28925567800 (post "undefined columns" fix): seasons succeeded
#     individually and grew progressively slower, then killed (exit 143)
#     partway through — splint_data$players (~1.2GB+ at June scale, bigger
#     now) and opta_stats (~7.5GB+) stayed fully resident for ALL 14
#     iterations, previously freed only after the loop ended.
# (2) A "pre-split everything into bundles, then rm() the originals" fix
#     died EVEN FASTER (before any season even started) — building a
#     complete second copy (the bundles) while the originals were still
#     resident briefly needs ~2x peak, which alone exceeds 16GB at this
#     scale. Building all bundles first doesn't work; must shrink the
#     ORIGINAL tables as each season is extracted, so the extraction
#     transient is bounded by ONE season's slice, never the full dataset
#     twice over.
#
# fit_season_ratings_opta's own filtering logic (season-end-year matching,
# calendar-league handling) is UNTOUCHED — pre-filtering to a single
# season, then having the function re-filter that already-single-season
# subset, is a provable no-op (verified: season 2013's SPM/RAPM/xRAPM
# output is bit-for-bit identical old-style vs this style,
# data-raw/debug/repro_bundle_equivalence.R). Seasons are DISJOINT
# partitions of each source table by construction (a splint/stats/xmetrics
# row matches exactly one season_end_year), so removing "this season's
# rows" from the remaining pool each iteration can't drop or duplicate
# anything.
cat("\n=== Processing All Seasons (shrinking-source loop) ===\n")
remaining_splints <- splint_data$splints
remaining_players <- splint_data$players
remaining_stats <- opta_stats
remaining_xm <- opta_xmetrics
match_info_shared <- splint_data$match_info
stats_end_years <- sapply(unique(opta_stats$season), extract_season_end_year)
xm_end_years <- if (!is.null(opta_xmetrics) && nrow(opta_xmetrics) > 0) {
  sapply(unique(opta_xmetrics$season), extract_season_end_year)
} else NULL
rm(splint_data, opta_stats, opta_xmetrics); gc(verbose = FALSE)

seasonal_ratings_list <- vector("list", length(seasons))
names(seasonal_ratings_list) <- as.character(seasons)
for (season in seasons) {
  key <- as.character(season)

  is_this_season <- remaining_splints$season_end_year == season
  s_splints <- remaining_splints[is_this_season, ]
  remaining_splints <- remaining_splints[!is_this_season, ]

  is_this_player <- remaining_players$splint_id %in% s_splints$splint_id
  s_players <- remaining_players[is_this_player, ]
  remaining_players <- remaining_players[!is_this_player, ]

  s_stats_seasons <- names(stats_end_years)[stats_end_years == season]
  is_this_stats <- remaining_stats$season %in% s_stats_seasons
  s_stats <- remaining_stats[is_this_stats, ]
  remaining_stats <- remaining_stats[!is_this_stats, ]

  s_xm <- NULL
  if (!is.null(xm_end_years)) {
    xm_matching <- names(xm_end_years)[xm_end_years == season]
    is_this_xm <- remaining_xm$season %in% xm_matching
    s_xm <- remaining_xm[is_this_xm, ]
    remaining_xm <- remaining_xm[!is_this_xm, ]
  }

  bundle_splint_data <- list(splints = s_splints, players = s_players,
                             match_info = match_info_shared)

  seasonal_ratings_list[[key]] <- tryCatch({
    fit_season_ratings_opta(
      splint_data = bundle_splint_data,
      opta_stats = s_stats,
      season = season,
      offense_spm_glmnet = offense_spm_glmnet,
      offense_spm_xgb = offense_spm_xgb,
      defense_spm_glmnet = defense_spm_glmnet,
      defense_spm_xgb = defense_spm_xgb,
      opta_xmetrics = s_xm,
      min_minutes_spm = 200,
      min_minutes_rapm = 200
    )
  }, error = function(e) {
    # panna#87: R buffers warning() into a terse "There were N warnings"
    # summary by default, hiding the actual message. cat() to stderr prints
    # immediately regardless of the warning options, so a run that silently
    # returns 0 processed seasons (as happened in run 28921623204 — all 14
    # seasons failed with no visible cause) shows its real error inline.
    cat(sprintf("\n[season %d ERROR]: %s\n", season, conditionMessage(e)))
    cat(sprintf("[season %d CALL]: %s\n", season, deparse(conditionCall(e))))
    warning(sprintf("Failed to process season %d: %s", season, e$message))
    NULL
  })

  rm(s_splints, s_players, s_stats, s_xm, bundle_splint_data)
  gc(verbose = FALSE)
}
rm(remaining_splints, remaining_players, remaining_stats, remaining_xm, match_info_shared)
gc(verbose = FALSE)

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

if (nrow(seasonal_xrapm) > 0) {
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
} else {
  player_season_counts <- data.frame()
  cat("No seasonal xRAPM data available\n")
}

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

save_cache_with_meta(seasonal_results, file.path(cache_dir, "07_seasonal_ratings.rds"),
                     pipeline = "opta-rapm")
validate_step_output(seasonal_xrapm, step_name = "07_seasonal: xrapm ratings",
                     min_rows = 100, warn_below = 5000)
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

# Multi-target seasonal ratings ----
# If multi-target xRAPM results exist, save seasonal ratings for each target

multi_xrapm_path <- file.path(cache_dir, "06_xrapm_multi.rds")
if (file.exists(multi_xrapm_path)) {
  cat("\n=== Multi-Target Seasonal Ratings ===\n")
  multi_xrapm <- readRDS(multi_xrapm_path)

  for (tgt in names(multi_xrapm)) {
    ratings_tgt <- multi_xrapm[[tgt]]$ratings
    if (!is.null(ratings_tgt) && nrow(ratings_tgt) > 0) {
      saveRDS(ratings_tgt, file.path(cache_dir, sprintf("07_seasonal_%s.rds", tgt)))
      cat(sprintf("  Saved %s seasonal ratings: %d players\n", toupper(tgt), nrow(ratings_tgt)))
    }
  }
}

cat("\n=== COMPLETE ===\n")
