# 03_skill_spm.R
# Fit SPM model using skill-based features (estimated skills)
#
# Reuses fit_spm_opta() and the existing SPM infrastructure, but feeds
# it decay-weighted skill features instead of raw season averages.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 3. Load Data ----

cat("\n=== Loading Data ===\n")

skill_features <- readRDS(file.path(cache_dir, "02_skill_features.rds"))
rapm_results <- readRDS(file.path(opta_cache_dir, "04_rapm.rds"))

rapm_ratings <- rapm_results$ratings
# Free memory
rm(rapm_results); gc(verbose = FALSE)
cat("Players with skill features:", nrow(skill_features), "\n")
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

# 4. Prepare SPM Training Data ----

cat("\n=== Preparing SPM Training Data ===\n")

# Skill features may span multiple seasons per player; aggregate to single row
# (the features are already decay-weighted at end-of-season, so for all-time SPM
# we use the most recent season's estimates per player)
player_stats <- skill_features %>%
  group_by(player_id) %>%
  slice_max(season_end_year, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("Unique players for SPM:", nrow(player_stats), "\n")

# Ensure required columns exist
if (!"player_name" %in% names(player_stats)) {
  player_stats$player_name <- player_stats$player_id
}

# Ensure mins_per_90 exists (needed by fit_spm_model weight logic)
if (!"mins_per_90" %in% names(player_stats)) {
  player_stats$mins_per_90 <- player_stats$total_minutes / 90
}

# Join with RAPM for training
spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings %>% select(player_id, rapm, offense, defense),
    by = "player_id"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")
cat(sprintf("  Match rate: %.1f%% of skill features matched RAPM ratings\n",
            100 * nrow(spm_train_data) / nrow(player_stats)))

if (nrow(spm_train_data) == 0) {
  stop(sprintf(
    "Zero players matched. player_id class: skills=%s (e.g. '%s'), RAPM=%s (e.g. '%s'). Likely a type mismatch.",
    class(player_stats$player_id), player_stats$player_id[1],
    class(rapm_ratings$player_id), rapm_ratings$player_id[1]
  ))
}
if (nrow(spm_train_data) < 100) {
  stop("Too few players for SPM training. Check RAPM/skill feature overlap.")
}

# 5. Fit Elastic Net SPM ----

cat("\n=== Fitting Skill-Based Elastic Net SPM ===\n")

spm_glmnet <- fit_spm_opta(
  spm_train_data,
  alpha = 0.5,
  nfolds = 10,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)

# 6. Fit XGBoost SPM ----

cat("\n=== Fitting Skill-Based XGBoost SPM ===\n")

spm_xgb <- fit_spm_xgb(
  spm_train_data,
  nfolds = 10,
  max_depth = 4,
  eta = 0.02,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

# 7. Model Comparison ----

cat("\n=== Model Comparison ===\n")

cv_rmse_glmnet <- sqrt(spm_glmnet$cvm[spm_glmnet$lambda == spm_glmnet$lambda.min])
cv_rmse_xgb <- spm_xgb$best_cv_rmse

cat(sprintf("Cross-Validation RMSE:\n"))
cat(sprintf("  Elastic Net: %.4f\n", cv_rmse_glmnet))
cat(sprintf("  XGBoost:     %.4f\n", cv_rmse_xgb))

# Get predictions
spm_ratings_glmnet <- calculate_spm_ratings(player_stats, spm_glmnet)
spm_ratings_xgb <- calculate_spm_ratings_xgb(player_stats, spm_xgb)

# Free memory
rm(spm_ratings_glmnet, spm_ratings_xgb); gc(verbose = FALSE)

# 8. Create 50/50 Blend ----

cat("\n=== Creating 50/50 Blend ===\n")

spm_ratings_blend <- calculate_spm_blend(player_stats, spm_glmnet, spm_xgb, weight_glmnet = SPM_BLEND_WEIGHT_GLMNET)
cat("Blended SPM ratings:", nrow(spm_ratings_blend), "players\n")

# Evaluate correlation with RAPM
blend_eval <- spm_ratings_blend %>%
  inner_join(rapm_ratings %>% select(player_id, rapm), by = "player_id")

if (nrow(blend_eval) > 0) {
  cat("\nCorrelation with RAPM:\n")
  cat(sprintf("  Elastic Net: %.3f\n", cor(blend_eval$spm_glmnet, blend_eval$rapm)))
  cat(sprintf("  XGBoost:     %.3f\n", cor(blend_eval$spm_xgb, blend_eval$rapm)))
  cat(sprintf("  50/50 Blend: %.3f\n", cor(blend_eval$spm, blend_eval$rapm)))
} else {
  cat("\n  WARNING: 0 players matched for RAPM correlation eval\n")
}

# Compare with raw-stat SPM if available
opta_spm_path <- file.path(opta_cache_dir, "05_spm.rds")
if (file.exists(opta_spm_path)) {
  opta_spm <- readRDS(opta_spm_path)
  raw_eval <- opta_spm$spm_ratings %>%
    inner_join(rapm_ratings %>% select(player_id, rapm), by = "player_id")
  if (nrow(raw_eval) > 0 && nrow(blend_eval) > 0) {
    raw_corr <- cor(raw_eval$spm, raw_eval$rapm)
    skill_corr <- cor(blend_eval$spm, blend_eval$rapm)
    cat(sprintf("\n*** Skill-based vs Raw-stat SPM ***\n"))
    cat(sprintf("  Raw-stat SPM  r(RAPM): %.3f\n", raw_corr))
    cat(sprintf("  Skill SPM     r(RAPM): %.3f\n", skill_corr))
    cat(sprintf("  Improvement:           %+.3f\n", skill_corr - raw_corr))
  }
}

# 9. Validate ----

cat("\n=== Validation ===\n")
val_blend <- validate_spm_prediction(
  spm_ratings_blend %>% select(-spm_glmnet, -spm_xgb),
  rapm_ratings
)

cat("\n=== SPM Feature Importance (Top 20) ===\n")
importance <- get_spm_feature_importance(spm_glmnet, n = 20)
print(importance)

# 10. Fit O/D Models ----

cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense
offense_train <- spm_train_data %>% mutate(rapm = offense)

# Column sets + defense sign constraints live in ONE place (R/spm_opta.R:
# .skill_spm_offense_cols() / .skill_spm_defense_cols() /
# .skill_spm_defense_constraints()) shared with the expanding-window as-of
# fit (R/spm_asof.R's fit_expanding_skill_spm(), FABLE-ASOF-EXPERIMENTS.md
# sec 4) so the two can never drift apart — hand-copied O/D feature lists
# are a recurring drift bug in this repo (see .spm_opta_predictor_cols()'s
# own history in the psr-skills.md gotcha).
offense_cols <- .skill_spm_offense_cols(spm_train_data)

cat("\n--- Offense Elastic Net ---\n")
offense_spm_glmnet <- fit_spm_model(offense_train, predictor_cols = offense_cols,
                                     alpha = 0.5, nfolds = 10, weight_by_minutes = TRUE)

cat("\n--- Offense XGBoost ---\n")
offense_spm_xgb <- fit_spm_xgb(offense_train, predictor_cols = offense_cols,
                                 nfolds = 10, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

# Defense
defense_train <- spm_train_data %>% mutate(rapm = defense)

defense_cols <- .skill_spm_defense_cols(spm_train_data)

cat("\n--- Defense Elastic Net ---\n")
# Directional sign constraints — same logic as Opta SPM step 05.
# In the negative-good defense convention, "good defense" features must have
# coef <= 0 (more = better defender). "Bad defense" features get coef >= 0.
defense_constraints <- .skill_spm_defense_constraints()
def_lower <- setNames(rep(0, length(defense_constraints$bad)),  defense_constraints$bad)
def_upper <- setNames(rep(0, length(defense_constraints$good)), defense_constraints$good)

defense_spm_glmnet <- fit_spm_model(defense_train, predictor_cols = defense_cols,
                                     alpha = 0.5, nfolds = 10, weight_by_minutes = TRUE,
                                     lower_limits = def_lower,
                                     upper_limits = def_upper)

cat("\n--- Defense XGBoost ---\n")
defense_spm_xgb <- fit_spm_xgb(defense_train, predictor_cols = defense_cols,
                                 nfolds = 10, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

# O/D Predictions
offense_glmnet_pred <- calculate_spm_ratings(player_stats, offense_spm_glmnet)
offense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, offense_spm_xgb)
offense_spm_ratings <- offense_glmnet_pred %>%
  rename(off_glmnet = spm) %>%
  inner_join(offense_xgb_pred %>% select(player_id, off_xgb = spm), by = "player_id") %>%
  mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

defense_glmnet_pred <- calculate_spm_ratings(player_stats, defense_spm_glmnet)
defense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, defense_spm_xgb)
defense_spm_ratings <- defense_glmnet_pred %>%
  rename(def_glmnet = spm) %>%
  inner_join(defense_xgb_pred %>% select(player_id, def_xgb = spm), by = "player_id") %>%
  mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# Free memory
rm(offense_glmnet_pred, offense_xgb_pred, defense_glmnet_pred, defense_xgb_pred)
rm(offense_train, defense_train, spm_train_data); gc(verbose = FALSE)

# 11. Save ----

cat("\n=== Saving Results ===\n")

spm_results <- list(
  spm_glmnet = spm_glmnet,
  spm_xgb = spm_xgb,
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb,
  spm_ratings = spm_ratings_blend,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  player_stats = player_stats,
  importance = importance,
  validation = val_blend,
  model_comparison = list(
    cv_rmse_glmnet = cv_rmse_glmnet,
    cv_rmse_xgb = cv_rmse_xgb,
    blend_weight = 0.5
  )
)

saveRDS(spm_results, file.path(cache_dir, "03_skill_spm.rds"))
cat("Saved to cache-skills/03_skill_spm.rds\n")

# 12. Expanding-Window Skill-SPM (as-of consumers) ----
#
# FABLE-ASOF-EXPERIMENTS.md sec 4: the all-history fit above is trained on
# skill features + pooled RAPM through the PRESENT -- hindsight for anything
# claiming point-in-time semantics. This section builds ONE skill-SPM weight
# set per reference year Y, trained ONLY on seasons < Y (features AND the
# pooled RAPM target it's fit against), for 09b_career_panna_asof.R to select
# by snapshot date. Sibling artifact (03_skill_spm_asof.rds) — leaves
# 03_skill_spm.rds and every consumer of it (steps 04-09, all retrospective
# seasonal/career outputs) untouched; sec 3 found those only need relabeling.
#
# Resumable + checkpointed after every year: completed years are cached and
# never refit ("freezing", sec 4 point 3) — a regular pipeline run only fits
# whatever new season just completed (~6-7 min, measured). The one-time
# historical backfill (~13 years) is ~1.5h — run standalone, not as part of
# the scheduled GHA skills pipeline.

build_skill_spm_asof <- if (exists("build_skill_spm_asof")) build_skill_spm_asof else TRUE

if (build_skill_spm_asof) {
  cat("\n=== Expanding-Window Skill-SPM (as-of consumers) ===\n")

  skill_spm_asof_path <- if (exists("skill_spm_asof_path")) skill_spm_asof_path else
    file.path(cache_dir, "03_skill_spm_asof.rds")
  asof_resume <- if (exists("asof_resume")) asof_resume else TRUE
  asof_nfolds <- if (exists("asof_nfolds")) asof_nfolds else 5
  asof_lambda_formula <- if (exists("asof_lambda_formula")) asof_lambda_formula else
    function(n) 16.67 * n^(-0.58)

  all_years <- sort(unique(skill_features$season_end_year))
  # The earliest year has no prior seasons to train on — skip it. 09b's
  # burn-in for pre-coverage dates falls back to the earliest year that DOES
  # get a model here (sec 4: "pool ... labeled retrospective, or accept
  # noisier early weights").
  reference_years <- if (exists("asof_reference_years")) asof_reference_years else all_years[-1]

  skill_spm_asof <- list()
  if (isTRUE(asof_resume) && file.exists(skill_spm_asof_path)) {
    skill_spm_asof <- readRDS(skill_spm_asof_path)
    cat(sprintf("  resume: %d cutoff-year models already in %s\n",
                length(skill_spm_asof), basename(skill_spm_asof_path)))
  }

  todo_years <- reference_years[!as.character(reference_years) %in% names(skill_spm_asof)]
  cat(sprintf("  %d cutoff years to fit: %s\n", length(todo_years),
              paste(todo_years, collapse = ", ")))

  if (length(todo_years) > 0) {
    r4 <- readRDS(file.path(opta_cache_dir, "04_rapm.rds"))
    pooled_rapm_data <- r4$rapm_data
    rm(r4); gc(verbose = FALSE)

    splints03 <- readRDS(file.path(opta_cache_dir, "03_splints.rds"))
    splint_season_map <- splints03$splints[, c("splint_id", "season_end_year")]
    rm(splints03); gc(verbose = FALSE)

    for (Y in todo_years) {
      cat(sprintf("\n--- cutoff_year %d (train on seasons < %d) ---\n", Y, Y))
      t0 <- Sys.time()

      pooled_fit <- fit_expanding_pooled_rapm(
        pooled_rapm_data, splint_season_map, cutoff_year = Y,
        lambda_formula = asof_lambda_formula, nfolds = asof_nfolds, seed = 20260710L
      )
      if (is.null(pooled_fit)) next

      skill_fit <- fit_expanding_skill_spm(
        skill_features, pooled_fit$ratings, cutoff_year = Y, nfolds = asof_nfolds
      )
      if (is.null(skill_fit)) next

      skill_spm_asof[[as.character(Y)]] <- skill_fit
      saveRDS(skill_spm_asof, skill_spm_asof_path)  # checkpoint after every year
      cat(sprintf("  cutoff_year %d done in %.1f min (n_train=%d, pooled n_obs=%d, lambda.min=%.5f)\n",
                  Y, as.numeric(difftime(Sys.time(), t0, units = "mins")),
                  skill_fit$n_train, pooled_fit$n_obs, pooled_fit$lambda_min))
      gc(verbose = FALSE)
    }
    rm(pooled_rapm_data, splint_season_map); gc(verbose = FALSE)
  }

  cat(sprintf("\nSaved %d expanding-window skill-SPM models to %s\n",
              length(skill_spm_asof), basename(skill_spm_asof_path)))
} else {
  cat("\n(skipping expanding-window skill-SPM — build_skill_spm_asof = FALSE)\n")
}

cat("\n=== COMPLETE ===\n")
