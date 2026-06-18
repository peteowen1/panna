# 05_spm.R
# Fit SPM model using Opta features
#
# Uses aggregate_opta_stats() (80+ features) instead of aggregate_player_stats().
# Optionally enriches features with xMetrics (xG/xA/xPass per-90).
# Fits Elastic Net + XGBoost, creates 50/50 blend, fits O/D models.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))
rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))

rapm_ratings <- rapm_results$ratings
# Free memory — rapm_results contains the full sparse design matrix
# (~664K x 38K), much bigger than just the ratings we need.
rm(rapm_results); gc(verbose = FALSE)
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

# Extract just the bits of processed_data we need (opta_stats and
# opta_xmetrics), then drop processed_data immediately. Holding the full
# processed_data list (~3-5 GB with lineups + events + shooting) alongside
# data.table aggregations of opta_stats (~1.3 GB) was OOM-killing step 5
# on standard 7 GB GHA runners.
opta_stats <- processed_data$opta_stats
opta_xmetrics <- processed_data$opta_xmetrics
rm(processed_data); gc(verbose = FALSE, full = TRUE)

# 3. Aggregate Opta Player Statistics ----

cat("\n=== Aggregating Opta Player Statistics ===\n")

if (is.null(opta_stats) || nrow(opta_stats) == 0) {
  stop("No Opta stats available. Check step 01 output.")
}

cat("Opta stats rows:", nrow(opta_stats), "\n")

player_stats <- aggregate_opta_stats(
  opta_stats,
  min_minutes = MIN_MINUTES_SPM
)

cat("Players with sufficient minutes:", nrow(player_stats), "\n")
cat("Features per player:", ncol(player_stats), "\n")

# 4. Enrich with xMetrics Features ----

if (use_xmetrics_features && !is.null(opta_xmetrics)) {
  cat("\n=== Enriching with xMetrics Features ===\n")

  xmetrics <- opta_xmetrics
  cat("xMetrics rows:", nrow(xmetrics), "\n")

  # Aggregate xMetrics to player level (may span multiple seasons)
  xmetrics_agg <- xmetrics %>%
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

  # Join to player_stats
  before_cols <- ncol(player_stats)
  player_stats <- player_stats %>%
    left_join(
      xmetrics_agg %>% select(player_id, xg_per90, npxg_per90,
                               xa_per90_xmetrics, xpass_overperformance_per90_xmetrics),
      by = "player_id"
    )

  # Fill NAs with 0 for players without xMetrics (no SPADL data = no modeled output)
  xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics", "xpass_overperformance_per90_xmetrics")
  n_imputed <- sum(rowSums(is.na(player_stats[, xm_cols, drop = FALSE])) > 0)
  for (col in xm_cols) {
    player_stats[[col]][is.na(player_stats[[col]])] <- 0
  }

  cat(sprintf("  Added %d xMetrics features\n", ncol(player_stats) - before_cols))
  cat(sprintf("  Players with xMetrics: %d / %d (%d imputed to 0)\n",
              nrow(player_stats) - n_imputed, nrow(player_stats), n_imputed))

  # Chain features (available if xMetrics pipeline ran with chain enrichment)
  chain_cols_avail <- c("chains_involved", "chain_actions", "successful_chains",
                        "chain_goals", "chain_starts", "chain_xg")
  if (any(chain_cols_avail %in% names(xmetrics))) {
    cat("\n=== Enriching with Chain Features ===\n")

    chain_agg <- xmetrics %>%
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

    before_cols2 <- ncol(player_stats)
    chain_join_cols <- c("player_id", "chains_p90", "chain_shot_pct", "chain_goal_pct",
                         "chain_starts_p90", "avg_actions_per_chain", "chain_xg_p90")
    chain_join_cols <- intersect(chain_join_cols, names(chain_agg))
    player_stats <- player_stats %>%
      left_join(chain_agg %>% select(all_of(chain_join_cols)), by = "player_id")

    chain_feat_cols <- setdiff(chain_join_cols, "player_id")
    for (col in chain_feat_cols) {
      player_stats[[col]][is.na(player_stats[[col]])] <- 0
    }

    cat(sprintf("  Added %d chain features\n", ncol(player_stats) - before_cols2))
    # Free memory
    rm(chain_agg); gc(verbose = FALSE)
  }

  # Free memory
  rm(xmetrics, xmetrics_agg); gc(verbose = FALSE)
}

# 5. Join with RAPM for Training ----

cat("\n=== Preparing SPM Training Data ===\n")

spm_train_data <- player_stats %>%
  inner_join(
    rapm_ratings %>%
      select(player_id, rapm, offense, defense),
    by = "player_id"
  )

cat("Players for SPM training:", nrow(spm_train_data), "\n")

# 6. Fit Elastic Net and XGBoost Models ----

cat("\n=== Fitting Opta Elastic Net SPM ===\n")
spm_glmnet <- fit_spm_opta(
  spm_train_data,
  alpha = 0.5,
  nfolds = 5,          # panna#87: 10 -> 5 to reduce CV memory/time
  weight_by_minutes = TRUE,
  weight_transform = "sqrt"
)

cat("\n=== Fitting XGBoost SPM ===\n")
spm_xgb <- fit_spm_xgb(
  spm_train_data,
  nfolds = 5,          # panna#87: 10 -> 5
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

cat(sprintf("\nCross-Validation RMSE:\n"))
cat(sprintf("  Elastic Net: %.4f\n", cv_rmse_glmnet))
cat(sprintf("  XGBoost:     %.4f\n", cv_rmse_xgb))

# Get predictions from each model
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

cat("\nCorrelation with RAPM:\n")
cat(sprintf("  Elastic Net: %.3f\n", cor(blend_eval$spm_glmnet, blend_eval$rapm)))
cat(sprintf("  XGBoost:     %.3f\n", cor(blend_eval$spm_xgb, blend_eval$rapm)))
cat(sprintf("  50/50 Blend: %.3f\n", cor(blend_eval$spm, blend_eval$rapm)))

# 9. Validation ----

cat("\n=== Validation ===\n")
val_blend <- validate_spm_prediction(
  spm_ratings_blend %>% select(-spm_glmnet, -spm_xgb),
  rapm_ratings
)

spm_model <- spm_glmnet
spm_ratings <- spm_ratings_blend
validation <- val_blend

cat("\n=== SPM Feature Importance (Top 20) ===\n")
importance <- get_spm_feature_importance(spm_model, n = 20)
print(importance)

# 10. Separate Offense/Defense SPM ----

cat("\n=== Fitting Separate Offense/Defense SPM ===\n")

# Offense training data
offense_train <- spm_train_data %>%
  mutate(rapm = offense)

offense_cols <- c(
  # Goals and shooting
  "goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
  "big_chance_scored_p90", "big_chance_created_p90",
  # Shot types
  "att_openplay_p90", "att_headed_p90", "att_one_on_one_p90",
  # Assists and creativity
  "assists_p90", "key_passes_p90", "through_balls_p90",
  "total_att_assist_p90",
  # Possession and progression
  "touches_opp_box_p90", "pen_area_entries_p90", "final_third_entries_p90",
  "final_third_passes_p90",
  # Progressive passing
  "fwd_zone_pass_p90", "open_play_pass_p90",
  # Counter-attacks
  "att_fastbreak_p90", "shot_fastbreak_p90",
  # Crossing and set pieces
  "crosses_p90", "crosses_open_play_p90", "forward_pass_p90",
  # Fouls drawn and penalties
  "was_fouled_p90", "penalty_won_p90",
  # Touch quality
  "unsuccessful_touch_p90", "overrun_p90",
  # Efficiency
  "shot_accuracy", "goals_per_shot", "big_chance_conversion",
  "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
  "crosses_open_play_accuracy",
  # Round 2: shot location and penalties
  "att_ibox_goal_p90", "att_obox_goal_p90",
  "att_ibox_target_p90", "att_obox_target_p90",
  "hit_woodwork_p90", "att_pen_goal_p90",
  "ibox_goal_rate", "penalty_conversion",
  # Round 2: passing detail
  "chipped_pass_p90", "chipped_pass_accuracy",
  # Round 2: foot preference
  "att_rf_total_p90", "att_lf_total_p90"
)

# Add xMetrics offense features if available
if ("xg_per90" %in% names(spm_train_data)) {
  offense_cols <- c(offense_cols, "xg_per90", "npxg_per90", "xa_per90_xmetrics")
}

# Add chain features to offense if available
chain_offense <- c("chains_p90", "chain_shot_pct", "chain_goal_pct",
                   "chain_starts_p90", "chain_xg_p90")
chain_offense <- intersect(chain_offense, names(spm_train_data))
if (length(chain_offense) > 0) {
  offense_cols <- c(offense_cols, chain_offense)
}

# Filter to available columns
offense_cols <- intersect(offense_cols, names(spm_train_data))

cat("\n--- Offense Elastic Net ---\n")
offense_spm_glmnet <- fit_spm_model(
  offense_train,
  predictor_cols = offense_cols,
  alpha = 0.5,
  nfolds = 5,          # panna#87: 10 -> 5
  weight_by_minutes = TRUE
)

cat("\n--- Offense XGBoost ---\n")
offense_spm_xgb <- fit_spm_xgb(
  offense_train,
  predictor_cols = offense_cols,
  nfolds = 5,          # panna#87: 10 -> 5
  max_depth = 4,
  eta = 0.02,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

# Defense training data
defense_train <- spm_train_data %>%
  mutate(rapm = defense)

defense_cols <- c(
  # Tackles
  "tackles_p90", "tackles_won_p90",
  # Interceptions and blocks
  "interceptions_p90", "interceptions_won_p90",
  "clearances_p90", "clearances_effective_p90",
  "blocks_p90", "blocked_passes_p90",
  # Last-ditch defending
  "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
  # Aerials
  "aerial_won_p90", "aerial_lost_p90",
  # Ball recovery
  "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
  # Negative actions
  "fouls_p90", "penalty_conceded_p90",
  # Errors
  "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
  # Touch quality
  "unsuccessful_touch_p90",
  # Efficiency
  "tackle_success", "aerial_success",
  # Round 2: possession control and duels
  "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch",
  "fifty_fifty_p90", "fifty_fifty_won_p90", "fifty_fifty_success",
  # Round 2: penalty conceded
  "pen_goals_conceded_p90",
  # Round 2: backward passing
  "back_zone_pass_p90", "back_zone_pass_accuracy",
  # Round 2: long pass own-to-opp
  "long_pass_own_to_opp_p90", "long_pass_own_to_opp_accuracy"
)

# Add chain features to defense if available (chain starts reflect build-up from back)
chain_defense <- c("chains_p90", "chain_starts_p90", "avg_actions_per_chain")
chain_defense <- intersect(chain_defense, names(spm_train_data))
if (length(chain_defense) > 0) {
  defense_cols <- c(defense_cols, chain_defense)
}

defense_cols <- intersect(defense_cols, names(spm_train_data))

cat("\n--- Defense Elastic Net ---\n")
# Directional sign constraints. RAPM defense column uses the model's native
# convention: negative = good defender (suppresses opponent xG). So features
# that genuinely indicate good defense should have NON-POSITIVE coefficients
# (more = lower defense = better defender), and bad-defense features should
# have NON-NEGATIVE coefficients. Without these constraints, multicollinearity
# can flip signs (e.g., elastic net learning that more tackles_won → worse
# defense, because tackles concentrate when teams are under pressure).
defense_good_features <- c(
  # Direct defensive actions — more = better
  "tackles_p90", "tackles_won_p90",
  "interceptions_p90", "interceptions_won_p90",
  "clearances_p90", "clearances_effective_p90",
  "blocks_p90", "blocked_passes_p90",
  "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
  "aerial_won_p90",
  "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
  "tackle_success", "aerial_success",
  "fifty_fifty_won_p90", "fifty_fifty_success",
  "back_zone_pass_accuracy"
)
defense_bad_features <- c(
  # Mistakes / actions that lead to opponent xG — more = worse defender
  "fouls_p90", "penalty_conceded_p90",
  "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
  "unsuccessful_touch_p90", "aerial_lost_p90",
  "pen_goals_conceded_p90",
  "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch"
)
def_lower <- setNames(rep(0,    length(defense_bad_features)),  defense_bad_features)
def_upper <- setNames(rep(0,    length(defense_good_features)), defense_good_features)

defense_spm_glmnet <- fit_spm_model(
  defense_train,
  predictor_cols = defense_cols,
  alpha = 0.5,
  nfolds = 5,          # panna#87: 10 -> 5
  weight_by_minutes = TRUE,
  lower_limits = def_lower,
  upper_limits = def_upper
)

cat("\n--- Defense XGBoost ---\n")
defense_spm_xgb <- fit_spm_xgb(
  defense_train,
  predictor_cols = defense_cols,
  nfolds = 5,          # panna#87: 10 -> 5
  max_depth = 4,
  eta = 0.02,
  nrounds = 1000,
  early_stopping_rounds = 20,
  weight_by_minutes = TRUE,
  weight_transform = "sqrt",
  verbose = 0
)

# 11. Generate Blended O/D SPM Predictions ----

cat("\n=== Generating Blended O/D SPM Predictions ===\n")

# Offense blend
offense_glmnet_pred <- calculate_spm_ratings(player_stats, offense_spm_glmnet)
offense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, offense_spm_xgb)

offense_spm_ratings <- offense_glmnet_pred %>%
  rename(offense_spm_glmnet = spm) %>%
  inner_join(
    offense_xgb_pred %>% select(player_id, offense_spm_xgb = spm),
    by = "player_id"
  ) %>%
  mutate(offense_spm = 0.5 * offense_spm_glmnet + 0.5 * offense_spm_xgb)

# Defense blend
defense_glmnet_pred <- calculate_spm_ratings(player_stats, defense_spm_glmnet)
defense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, defense_spm_xgb)

defense_spm_ratings <- defense_glmnet_pred %>%
  rename(defense_spm_glmnet = spm) %>%
  inner_join(
    defense_xgb_pred %>% select(player_id, defense_spm_xgb = spm),
    by = "player_id"
  ) %>%
  mutate(defense_spm = 0.5 * defense_spm_glmnet + 0.5 * defense_spm_xgb)

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# Free memory
rm(offense_glmnet_pred, offense_xgb_pred, defense_glmnet_pred, defense_xgb_pred)
rm(offense_train, defense_train, spm_train_data); gc(verbose = FALSE)

# 12. Combined SPM Ratings ----

cat("\n=== Combined SPM Ratings ===\n")

combined_spm <- spm_ratings %>%
  select(player_id, player_name, total_minutes, spm) %>%
  left_join(
    offense_spm_ratings %>% select(player_id, offense_spm),
    by = "player_id"
  ) %>%
  left_join(
    defense_spm_ratings %>% select(player_id, defense_spm),
    by = "player_id"
  ) %>%
  left_join(
    rapm_ratings %>% select(player_id, rapm, offense, defense),
    by = "player_id"
  ) %>%
  arrange(desc(spm))

cat("\nTop 25 Players by SPM (with O/D breakdown):\n")
print(
  combined_spm %>%
    head(25) %>%
    select(player_name, total_minutes, spm, offense_spm, defense_spm, rapm),
  digits = 3
)

# 13. Save Results ----

cat("\n=== Saving Results ===\n")

spm_results <- list(
  spm_glmnet = spm_glmnet,
  spm_xgb = spm_xgb,
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb,
  spm_ratings = spm_ratings,
  offense_spm_ratings = offense_spm_ratings,
  defense_spm_ratings = defense_spm_ratings,
  combined_ratings = combined_spm,
  player_stats = player_stats,
  importance = importance,
  validation = validation,
  model_comparison = list(
    cv_rmse_glmnet = cv_rmse_glmnet,
    cv_rmse_xgb = cv_rmse_xgb,
    blend_weight = 0.5
  )
)

saveRDS(spm_results, file.path(cache_dir, "05_spm.rds"))
cat("Saved to cache-opta/05_spm.rds\n")

# 14. Multi-Target SPM (optional) ----
# Fit SPM predicting each value metric RAPM if multi-target results exist

use_multi_target <- if (exists("use_multi_target")) use_multi_target else TRUE
multi_rapm_path <- file.path(cache_dir, "04_rapm_multi.rds")

if (use_multi_target && file.exists(multi_rapm_path)) {
  cat("\n=== Multi-Target SPM ===\n")
  multi_rapm <- readRDS(multi_rapm_path)

  multi_spm_results <- list()

  for (tgt in names(multi_rapm)) {
    cat(sprintf("\n--- Fitting SPM for target: %s ---\n", tgt))

    tryCatch({
      tgt_ratings <- multi_rapm[[tgt]]$ratings
      rapm_col <- paste0("rapm_", tgt)

      # Rename to "rapm" for fit_spm_opta compatibility
      if (rapm_col %in% names(tgt_ratings)) {
        data.table::setnames(tgt_ratings, rapm_col, "rapm")
      }

      # Prepare regression data: join player stats with RAPM ratings
      spm_data <- prepare_spm_regression_data(player_stats, tgt_ratings)

      if (nrow(spm_data) < 50) {
        cat(sprintf("  Skipping %s: only %d players with both stats and RAPM\n",
                    tgt, nrow(spm_data)))
        next
      }

      # Fit SPM
      spm_model <- fit_spm_opta(spm_data)
      spm_ratings <- calculate_spm_ratings(spm_model, player_stats)

      multi_spm_results[[tgt]] <- list(
        model = spm_model,
        ratings = spm_ratings
      )
      cat(sprintf("  %s SPM: %d players rated\n", toupper(tgt), nrow(spm_ratings)))

    }, error = function(e) {
      cat(sprintf("  Skipping %s SPM: %s\n", tgt, e$message))
    })
  }

  if (length(multi_spm_results) > 0) {
    saveRDS(multi_spm_results, file.path(cache_dir, "05_spm_multi.rds"))
    cat("\nSaved multi-target SPM to cache-opta/05_spm_multi.rds\n")
  }
}

cat("\n=== COMPLETE ===\n")
