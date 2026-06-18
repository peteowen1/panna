# 06_xrapm.R
# Fit xRAPM (RAPM with SPM prior) for Opta data
#
# Near-identical to FBref version. Uses Opta SPM predictions as
# Bayesian prior for RAPM fitting.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

xrapm_lambda <- "min"
cat(sprintf("Using lambda = %s for xRAPM\n", xrapm_lambda))

# panna#87 OOM mitigation (option A): env-gated CV skip. When OPTA_FIXED_LAMBDA
# is set (any non-empty value), skip cv.glmnet in fit_rapm_with_prior() and fit
# at the closed-form lambda = 16.67 * n_obs^-0.58. Default (unset) keeps CV.
use_fixed_lambda <- nzchar(Sys.getenv("OPTA_FIXED_LAMBDA"))
# Sample-size lambda formula (same as data-raw/estimated-skills/09b_career_panna_asof.R).
lambda_formula <- function(n) 16.67 * n^(-0.58)
# n_obs for a fit = count of valid (non-NA, finite) responses, matching the
# count fit_rapm_with_prior() uses internally.
.n_obs_valid <- function(rd) sum(!is.na(rd$y) & is.finite(rd$y))

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Players with RAPM:", nrow(rapm_results$ratings), "\n")
cat("Players with SPM:", nrow(spm_results$spm_ratings), "\n")

# Free memory
rm(splint_data); gc(verbose = FALSE)

# 3. Create SPM Priors (from blended models) ----

cat("\n=== Creating SPM Priors ===\n")
cat("Using 50/50 Elastic Net + XGBoost blend\n")

player_mapping <- rapm_results$rapm_data$player_mapping

offense_prior <- build_prior_vector(
  spm_data = spm_results$offense_spm_ratings,
  spm_col = "offense_spm",
  player_mapping = player_mapping
)

defense_prior <- build_prior_vector(
  spm_data = spm_results$defense_spm_ratings,
  spm_col = "defense_spm",
  player_mapping = player_mapping
)

cat("Offense priors set:", sum(offense_prior != 0), "\n")
cat("Defense priors set:", sum(defense_prior != 0), "\n")

cat("\nOffense prior summary:\n")
print(summary(offense_prior[offense_prior != 0]))
cat("\nDefense prior summary:\n")
print(summary(defense_prior[defense_prior != 0]))

# Free memory
rm(spm_results); gc(verbose = FALSE)

# 4. Fit xRAPM Model ----

cat("\n=== Fitting xRAPM Model ===\n")

rapm_data <- rapm_results$rapm_data

# panna#87: fixed-lambda mode skips CV (the 11-refit memory spike); else CV.
xrapm_fixed_lambda <- if (use_fixed_lambda) lambda_formula(.n_obs_valid(rapm_data)) else NULL
if (use_fixed_lambda) {
  cli::cli_alert_info(
    "xRAPM fixed-lambda mode (CV skipped), lambda={round(xrapm_fixed_lambda, 5)} (n_obs={.n_obs_valid(rapm_data)})")
}

xrapm_model <- fit_rapm_with_prior(
  rapm_data,
  offense_prior = offense_prior,
  defense_prior = defense_prior,
  alpha = 0,
  nfolds = 5,          # panna#87: 10 -> 5 to halve the CV memory/time spike
  use_weights = TRUE,
  penalize_covariates = FALSE,
  fixed_lambda = xrapm_fixed_lambda  # panna#87: NULL = CV (default), else closed-form
)

# Free memory
rm(rapm_data); gc(verbose = FALSE)

# 5. Extract xRAPM Ratings ----

cat("\n=== xRAPM Ratings ===\n")

xrapm_ratings <- extract_xrapm_ratings(xrapm_model, lambda = xrapm_lambda)

cat("\nTop 25 by xRAPM:\n")
print(
  xrapm_ratings %>%
    head(25) %>%
    select(player_name, xrapm, offense, defense, off_deviation, def_deviation, total_minutes)
)

# 6. Compare xRAPM vs Base RAPM ----

cat("\n=== xRAPM vs Base RAPM ===\n")

base_ratings <- rapm_results$ratings %>%
  select(player_id, base_rapm = rapm, base_off = offense, base_def = defense)

comparison <- xrapm_ratings %>%
  select(player_id, player_name, xrapm, xrapm_off = offense, xrapm_def = defense,
         off_deviation, def_deviation, off_prior, def_prior, total_minutes) %>%
  inner_join(base_ratings, by = "player_id") %>%
  mutate(
    rating_diff = xrapm - base_rapm,
    off_diff = xrapm_off - base_off,
    def_diff = xrapm_def - base_def
  )

cat("\nCorrelation: xRAPM vs Base RAPM:", round(cor(comparison$xrapm, comparison$base_rapm), 3), "\n")
cat("Correlation: xRAPM Offense vs Base Offense:", round(cor(comparison$xrapm_off, comparison$base_off), 3), "\n")
cat("Correlation: xRAPM Defense vs Base Defense:", round(cor(comparison$xrapm_def, comparison$base_def), 3), "\n")

cat("\nPlayers most improved by xRAPM:\n")
print(
  comparison %>%
    arrange(desc(rating_diff)) %>%
    head(15) %>%
    select(player_name, xrapm, base_rapm, rating_diff, off_deviation, def_deviation)
)

# Free memory
rm(rapm_results, base_ratings); gc(verbose = FALSE)

# 7. Team-Level Validation ----

cat("\n=== Team-Level Validation ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

if (!is.null(processed_data$lineups)) {
  player_teams <- processed_data$lineups %>%
    mutate(team = if ("team_name" %in% names(.)) team_name else team) %>%
    group_by(player_id, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_id) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_id, primary_team = team)

  team_xrapm <- xrapm_ratings %>%
    left_join(player_teams, by = "player_id") %>%
    filter(!is.na(primary_team)) %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      sum_xrapm = sum(xrapm),
      mean_xrapm = mean(xrapm),
      .groups = "drop"
    ) %>%
    arrange(desc(sum_xrapm))

  if (!is.null(processed_data$results)) {
    results <- processed_data$results

    home_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = home_team) %>%
      summarise(total_npxgd = sum(home_xg - away_xg, na.rm = TRUE), .groups = "drop")

    away_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = away_team) %>%
      summarise(total_npxgd = sum(away_xg - home_xg, na.rm = TRUE), .groups = "drop")

    team_npxgd <- home_npxgd %>%
      full_join(away_npxgd, by = "team", suffix = c("_home", "_away")) %>%
      mutate(total_npxgd = coalesce(total_npxgd_home, 0) + coalesce(total_npxgd_away, 0)) %>%
      select(team, total_npxgd)

    team_comparison <- team_npxgd %>%
      inner_join(team_xrapm %>% select(primary_team, sum_xrapm), by = c("team" = "primary_team"))

    cat("\nnpxGD vs Sum xRAPM:", round(cor(team_comparison$total_npxgd, team_comparison$sum_xrapm), 3), "\n")
  }
}

# 8. Save Results ----

cat("\n=== Saving Results ===\n")

xrapm_results <- list(
  model = xrapm_model,
  ratings = xrapm_ratings,
  comparison = comparison,
  offense_prior = offense_prior,
  defense_prior = defense_prior,
  team_xrapm = if (exists("team_xrapm")) team_xrapm else NULL,
  team_comparison = if (exists("team_comparison")) team_comparison else NULL
)

saveRDS(xrapm_results, file.path(cache_dir, "06_xrapm.rds"))
cat("Saved to cache-opta/06_xrapm.rds\n")

# 9. Multi-Target xRAPM (optional) ----
# Fit xRAPM for value metric targets using their RAPM + SPM results

use_multi_target <- if (exists("use_multi_target")) use_multi_target else TRUE
multi_rapm_path <- file.path(cache_dir, "04_rapm_multi.rds")
multi_spm_path <- file.path(cache_dir, "05_spm_multi.rds")

if (use_multi_target && file.exists(multi_rapm_path) && file.exists(multi_spm_path)) {
  cat("\n=== Multi-Target xRAPM ===\n")
  multi_rapm <- readRDS(multi_rapm_path)
  multi_spm <- readRDS(multi_spm_path)

  multi_xrapm_results <- list()

  for (tgt in intersect(names(multi_rapm), names(multi_spm))) {
    cat(sprintf("\n--- Fitting xRAPM for target: %s ---\n", tgt))

    tryCatch({
      rapm_data_tgt <- multi_rapm[[tgt]]$rapm_data
      spm_ratings_tgt <- multi_spm[[tgt]]$ratings

      off_prior <- spm_ratings_tgt$spm_offense %||% rep(0, nrow(spm_ratings_tgt))
      def_prior <- spm_ratings_tgt$spm_defense %||% rep(0, nrow(spm_ratings_tgt))

      # Match SPM to RAPM player ordering
      player_map <- rapm_data_tgt$player_mapping
      off_prior_aligned <- rep(0, rapm_data_tgt$n_players_total)
      def_prior_aligned <- rep(0, rapm_data_tgt$n_players_total)

      for (i in seq_len(nrow(spm_ratings_tgt))) {
        pid <- spm_ratings_tgt$player_id[i]
        if (pid %in% names(player_map)) {
          idx <- player_map[[pid]]
          off_prior_aligned[idx] <- off_prior[i]
          def_prior_aligned[idx] <- def_prior[i]
        }
      }

      # panna#87: per-target fixed lambda from that target's own n_obs.
      tgt_fixed_lambda <- if (use_fixed_lambda) lambda_formula(.n_obs_valid(rapm_data_tgt)) else NULL
      if (use_fixed_lambda) {
        cli::cli_alert_info(
          "xRAPM[{tgt}] fixed-lambda mode (CV skipped), lambda={round(tgt_fixed_lambda, 5)} (n_obs={.n_obs_valid(rapm_data_tgt)})")
      }

      xrapm_model_tgt <- fit_rapm_with_prior(
        rapm_data_tgt,
        offense_prior = off_prior_aligned,
        defense_prior = def_prior_aligned,
        fixed_lambda = tgt_fixed_lambda  # panna#87
      )

      xrapm_ratings_tgt <- extract_rapm_ratings(xrapm_model_tgt)
      rapm_col <- paste0("xrapm_", tgt)
      data.table::setnames(xrapm_ratings_tgt, "rapm", rapm_col,
                            skip_absent = TRUE)

      multi_xrapm_results[[tgt]] <- list(
        model = xrapm_model_tgt,
        ratings = xrapm_ratings_tgt
      )
      cat(sprintf("  %s xRAPM: %d players rated\n", toupper(tgt), nrow(xrapm_ratings_tgt)))

    }, error = function(e) {
      cat(sprintf("  Skipping %s xRAPM: %s\n", tgt, e$message))
    })
  }

  if (length(multi_xrapm_results) > 0) {
    saveRDS(multi_xrapm_results, file.path(cache_dir, "06_xrapm_multi.rds"))
    cat("\nSaved multi-target xRAPM to cache-opta/06_xrapm_multi.rds\n")
  }
}

cat("\n=== COMPLETE ===\n")
