# 05_spm.R
# Fit SPM model using Opta features
#
# Uses aggregate_opta_stats() (80+ features) instead of aggregate_player_stats().
# Optionally enriches features with xMetrics (xG/xA/xPass per-90).
# Fits Elastic Net + XGBoost, creates 50/50 blend, fits O/D models.

# 1. Setup ----

library(dplyr)
devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")
use_xmetrics_features <- if (exists("use_xmetrics_features")) use_xmetrics_features else TRUE

# 2. Load Data ----

cat("\n=== Loading Data ===\n")

# panna#87: opta_stats/opta_xmetrics live in their OWN file (02_opta_stats.rds)
# as of the step-02 split — NEVER read the monolithic 02_processed_data.rds
# here. readRDS() must deserialize an object's entire graph before returning
# any of it, so this step used to pay for lineups/shooting/results/
# stats_summary too just to reach these two fields — confirmed live: this
# exact load left only ~110MB of 16GB free (run 28920296396), one step
# after the identical load OOM'd outright (07_seasonal_ratings, run
# 28921032951). Loading the narrow file directly removes that risk instead
# of merely narrowing faster after the fact.
opta_stats_bundle <- readRDS(file.path(cache_dir, "02_opta_stats.rds"))
opta_stats <- opta_stats_bundle$opta_stats
opta_xmetrics <- opta_stats_bundle$opta_xmetrics
rm(opta_stats_bundle); gc(verbose = FALSE)

rapm_results <- readRDS(file.path(cache_dir, "04_rapm.rds"))
rapm_ratings <- rapm_results$ratings
# Free memory — rapm_results contains the full sparse design matrix
# (~664K x 38K), much bigger than just the ratings we need.
rm(rapm_results); gc(verbose = FALSE)
cat("Players with RAPM ratings:", nrow(rapm_ratings), "\n")

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

  # panna#87: aggregation logic now lives in ONE place, .aggregate_xmetrics_for_spm()
  # (R/spm_opta.R) — shared with 07_seasonal_ratings.R's per-season SPM
  # breakdown. This exact duplication (this block vs. a near-identical but
  # never-updated copy in 07) was the root cause of every one of 14 seasons
  # failing with "undefined columns selected" the first time the fitted
  # model's predictor_cols grew to include the new WOE/finishing columns.
  xmetrics_agg <- .aggregate_xmetrics_for_spm(xmetrics)
  xm_base_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics",
                    "xpass_overperformance_per90_xmetrics")
  xm_extra_present <- setdiff(names(xmetrics_agg), c("player_id", xm_base_cols))
  if (length(xm_extra_present) > 0) {
    cat(sprintf("  Above-expected columns present: %s\n",
                paste(xm_extra_present, collapse = ", ")))
  } else {
    cat("  NOTE: no above-expected columns in this opta_xmetrics vintage — refresh via epv-pipeline xmetrics_only\n")
  }

  # Guarantee the FULL canonical column set exists (0-filled) so
  # calculate_spm_ratings() never fails on a missing predictor column,
  # regardless of which columns this xmetrics vintage actually carried.
  xm_cols <- .spm_xmetrics_per90_cols()
  before_cols <- ncol(player_stats)
  player_stats <- player_stats %>%
    left_join(xmetrics_agg, by = "player_id")
  for (col in setdiff(xm_cols, names(player_stats))) {
    player_stats[[col]] <- NA_real_
  }

  # Fill NAs with 0 for players without xMetrics. For xg/npxg/xa this means
  # "no SPADL coverage = no modeled volume"; for the above-expected columns 0
  # IS the population mean (they're mean-zero by construction), so 0 = an
  # average player — a meaningful imputation, not a silent constant fallback.
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

# League control: minutes SHARES, not a dummy.
#
# aggregate_opta_stats() collapses 3.46M player-match rows to one row per
# player and no league column survives, so `league_fe = TRUE` here is a silent
# no-op. Deriving one league per player is the panna#222 trap: of 48,377
# players only 55.1% appear in a single competition, 12.0% have no competition
# holding even 60% of their minutes, and 0.3% are exact ties. (Median dominant
# share is 1.000 -- the same reassuring statistic that said #222's inputs were
# fine.) A share vector removes the decision instead of making it badly, and
# degenerates to the dummy for the 55% who played in one place.
#
# Validated as a single axis on 35,590 players, 5-fold held out: RMSE 0.03076
# -> 0.02983, -3.02%. Limiting case checked and passed: on a
# single-competition subset the shares carry no variance and the arms converge
# (-0.06%), so the gain is a league effect and not 30 unpenalized columns
# forcing the metric.
#
# DEFAULT OFF. Turning it on makes lgshare_* part of the model's predictor
# contract, and calculate_spm_ratings() ABORTS when they are absent - so every
# downstream scorer must join them too: 06_xrapm, 07_seasonal_ratings, and
# 05b_export_spm_coefficients (live per-match parity). Switch on only with
# those wired.
if (!exists("spm_league_shares")) spm_league_shares <- FALSE
if (isTRUE(spm_league_shares)) {
  # Joined onto `player_stats`, NOT spm_train_data.
  #
  # spm_train_data is player_stats inner-joined to RAPM (35,590 of 68,830
  # players), but SCORING runs on the full player_stats - so joining the shares
  # downstream leaves the scoring frame without them and
  # calculate_spm_ratings() aborts. Joining here means the training frame
  # inherits the columns through the inner_join below, and both sides carry
  # them by construction rather than by remembering to do it twice.
  #
  # `opta_stats` (player-match grain, from 02_opta_stats.rds) -- NOT
  # processed_data, which does not exist in this step's environment.
  .shares <- panna:::.spm_league_shares(opta_stats, min_n = 50)
  if (length(.shares$cols) == 0) {
    stop("spm_league_shares = TRUE but the share matrix is empty; refusing to fit unadjusted.")
  }
  player_stats <- player_stats %>% left_join(.shares$data, by = "player_id")
  for (.cc in .shares$cols) player_stats[[.cc]][is.na(player_stats[[.cc]])] <- 0
  cat(sprintf("League shares: %d columns (reference %s), %.1f%% of players non-reference\n",
              length(.shares$cols), .shares$reference,
              100 * mean(rowSums(player_stats[, .shares$cols, drop = FALSE]) > 0)))
}

# Training frame is built AFTER the shares join so it inherits those columns;
# scoring later runs on player_stats, which now carries them too.
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
  weight_transform = "sqrt",
  league_shares = spm_league_shares
)

cat("\n=== Fitting XGBoost SPM ===\n")
spm_xgb <- fit_spm_xgb(
  spm_train_data,
  # Exact feature parity with the glmnet half: fit_spm_xgb's own default grep
  # was `_p90$`-only, which kept the XGB half of the 50/50 blend xMetrics-blind
  # even after fit_spm_opta's detector was fixed (2026-07-07 review finding).
  # The league shares are appended for the SAME reason: .spm_opta_predictor_cols()
  # does not match `lgshare_*`, so adjusting only the glmnet half would leave
  # half of a 50/50 blend league-blind - the identical parity break, one feature
  # family later.
  predictor_cols = c(panna:::.spm_opta_predictor_cols(spm_train_data),
                     if (isTRUE(spm_league_shares)) grep("^lgshare_", names(spm_train_data), value = TRUE)),
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

# F4 (FABLE-PRIOR-FIX-PLAN.md review): offense/defense SPM fit+blend, shared
# by the base path (Sections 10-11 below) and the EPV multi-target branch
# (Section 14) -- previously ~65 lines duplicated verbatim between the two.
# Purely mechanical hoist: identical fit_spm_model()/fit_spm_xgb() calls,
# hyperparameters, and CALL ORDER (offense EN -> offense XGB -> defense EN ->
# defense XGB -> offense predict/blend -> defense predict/blend) as the
# pre-hoist base path, so CV fold assignment / xgb random draws are
# unaffected by the hoist.
.fit_od_spm_blend <- function(offense_train, defense_train, offense_cols, defense_cols,
                               def_lower, def_upper, player_stats) {
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

  cat("\n--- Defense Elastic Net ---\n")
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

  list(
    offense_spm_glmnet = offense_spm_glmnet,
    offense_spm_xgb = offense_spm_xgb,
    defense_spm_glmnet = defense_spm_glmnet,
    defense_spm_xgb = defense_spm_xgb,
    offense_spm_ratings = offense_spm_ratings,
    defense_spm_ratings = defense_spm_ratings
  )
}

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
  # Efficiency (conversion ratios with above-expected replacements removed
  # 2026-07-07 — see the xMetrics block below; volume-blind ratios rewarded
  # 1/1 == 10/10)
  "shot_accuracy",
  "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
  "crosses_open_play_accuracy",
  # Round 2: shot location and penalties
  "att_ibox_goal_p90", "att_obox_goal_p90",
  "att_ibox_target_p90", "att_obox_target_p90",
  "hit_woodwork_p90", "att_pen_goal_p90",
  # Round 2: passing detail
  "chipped_pass_p90", "chipped_pass_accuracy",
  # Round 2: foot preference
  "att_rf_total_p90", "att_lf_total_p90"
)

# Add xMetrics offense features if available — includes the above-expected
# replacements for the removed conversion ratios (finishing over-performance,
# placement, offensive duel WOE); intersect keeps this schema-defensive for
# older xmetrics vintages.
if ("xg_per90" %in% names(spm_train_data)) {
  offense_cols <- c(offense_cols, "xg_per90", "npxg_per90", "xa_per90_xmetrics")
}
offense_cols <- c(offense_cols, intersect(
  c("npg_minus_npxg_per90", "ibox_g_minus_xg_per90", "obox_g_minus_xg_per90",
    "placement_added_per90", "takeon_woe_per90", "aerial_woe_per90"),
  names(spm_train_data)
))

# Add chain features to offense if available
chain_offense <- c("chains_p90", "chain_shot_pct", "chain_goal_pct",
                   "chain_starts_p90", "chain_xg_p90")
chain_offense <- intersect(chain_offense, names(spm_train_data))
if (length(chain_offense) > 0) {
  offense_cols <- c(offense_cols, chain_offense)
}

# Filter to available columns
offense_cols <- intersect(offense_cols, names(spm_train_data))

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
  # Round 2: possession control and duels (tackle_success/aerial_success
  # removed 2026-07-07 — replaced by the defensive WOE counts below)
  "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch",
  "fifty_fifty_p90", "fifty_fifty_won_p90", "fifty_fifty_success",
  # Round 2: penalty conceded
  "pen_goals_conceded_p90",
  # Round 2: backward passing
  "back_zone_pass_p90", "back_zone_pass_accuracy",
  # Round 2: long pass own-to-opp
  "long_pass_own_to_opp_p90", "long_pass_own_to_opp_accuracy"
)

# Above-expected defensive replacements (schema-defensive for older vintages):
# tackle/containment/aerial WOE + keeper GSAA.
defense_cols <- c(defense_cols, intersect(
  c("tackle_poss_woe_per90", "containment_woe_per90",
    "aerial_woe_per90", "aerial_poss_woe_per90", "gsaa_per90"),
  names(spm_train_data)
))

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
  # Above-expected defensive counts (replaced tackle_success/aerial_success)
  "tackle_poss_woe_per90", "containment_woe_per90",
  "aerial_woe_per90", "aerial_poss_woe_per90", "gsaa_per90",
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

# 11. Generate Blended O/D SPM Predictions ----

cat("\n=== Generating Blended O/D SPM Predictions ===\n")

# F4: fit + blend via the shared helper (defined above Section 10).
od_fit <- .fit_od_spm_blend(offense_train, defense_train, offense_cols, defense_cols,
                             def_lower, def_upper, player_stats)
offense_spm_glmnet <- od_fit$offense_spm_glmnet
offense_spm_xgb <- od_fit$offense_spm_xgb
defense_spm_glmnet <- od_fit$defense_spm_glmnet
defense_spm_xgb <- od_fit$defense_spm_xgb
offense_spm_ratings <- od_fit$offense_spm_ratings
defense_spm_ratings <- od_fit$defense_spm_ratings
rm(od_fit)

cat("Offense SPM predictions:", nrow(offense_spm_ratings), "\n")
cat("Defense SPM predictions:", nrow(defense_spm_ratings), "\n")

# Free memory
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

# 12b. S6 panel-SPM override (Wave 4, D-W2 2026-07-22) ----
#
# Pete's D-W2 promotion: the panel SPM (S4a glmnet + player-grouped-CV XGB,
# per-target 50/50 blend, windowed prior-free target — see
# pannaverse/docs/reviews/SPM-WAVE2-BAKEOFF-2026-07-22.md) replaces the
# legacy career tables as the PRIOR source. Hybrid by design: outfield
# players get S6 values (latest-vintage panel predictions); GK + players
# absent from the panel keep the legacy values (#159 owns the GK design;
# the panel is outfield-only). Legacy tables are preserved under *_legacy
# keys; the seasonal-SPM DISPLAY path is unchanged this cut (follow-up
# with panna#168's display calibration).
#
# spm_use_panel = FALSE skips the override (legacy behavior, loud notice).
# Missing spm_panel.rds with the flag on is a HARD abort — no silent
# fallback (the panel is built by data-raw/spm-redesign/04c from the
# skills pipeline's 01_match_stats; rebuild it rather than shipping a
# silently-legacy prior).
#
# BLAST RADIUS (deliberate, D-W2 "promote it"): every 05_spm.rds table
# reader inherits the hybrid values — 06's career xRAPM AND therefore 08's
# published panna composite change, not just the seasonal loop. The Wave-4
# gate (13c) validated the per-season posterior; the career fit shares the
# mechanism. Post-regen face-validity on 08's top list is the remaining
# check. Also NB: the legacy sub-model columns (offense_spm_glmnet/xgb etc.
# inside the *_ratings tables, where present) are NOT rescored — for
# S6-covered players the blend invariant (0.5·glmnet+0.5·xgb == value) no
# longer holds against those columns; consumers must read the value
# columns, never re-derive them.

spm_use_panel <- if (exists("spm_use_panel")) spm_use_panel else TRUE

panel_s6 <- NULL
offense_spm_ratings_legacy <- offense_spm_ratings
defense_spm_ratings_legacy <- defense_spm_ratings
spm_ratings_legacy <- spm_ratings

if (isTRUE(spm_use_panel)) {
  cat("\n=== S6 panel-SPM override (Wave 4) ===\n")
  panel_path <- file.path(cache_dir, "spm_panel.rds")
  if (!file.exists(panel_path)) {
    cli::cli_abort(c(
      "spm_use_panel = TRUE but {.file {panel_path}} is missing.",
      "i" = "Build it with data-raw/spm-redesign/04c_build_spm_panel.R (needs the skills pipeline's 01_match_stats.rds), or set spm_use_panel <- FALSE to run legacy-only."
    ))
  }
  panel_age_days <- as.numeric(difftime(Sys.time(), file.mtime(panel_path), units = "days"))
  if (panel_age_days > 60) {
    warning(sprintf("spm_panel.rds is %.0f days old — the S6 prior is a slow-moving career-window trait, but consider a rebuild (04c) after the next skills-pipeline run.", panel_age_days))
  }

  panel_bundle <- readRDS(panel_path)
  s6_panel <- panel_bundle$panel
  attr(s6_panel, "target_provenance") <- panel_bundle$target_provenance

  # S4a config verbatim (05c_candidates.R): role pooling + sign constraints
  # + LINEAR minutes weights (best glmnet candidate on every vintage).
  s6_off_glmnet <- fit_spm_panel(s6_panel, target = "offense", role_pooling = TRUE,
                                 sign_constraints = TRUE, weight_transform = "linear",
                                 alpha = 0.5, deviation_penalty_mult = 5,
                                 nfolds = 5, seed = 1)
  s6_def_glmnet <- fit_spm_panel(s6_panel, target = "defense", role_pooling = TRUE,
                                 sign_constraints = TRUE, weight_transform = "linear",
                                 alpha = 0.5, deviation_penalty_mult = 5,
                                 nfolds = 5, seed = 1)
  s6_off_xgb <- fit_spm_panel_xgb(s6_panel, target = "offense", seed = 1)
  s6_def_xgb <- fit_spm_panel_xgb(s6_panel, target = "defense", seed = 1)

  s6_latest <- s6_panel[s6_panel$vintage_year == max(s6_panel$vintage_year), ]
  s6_go <- predict_spm_panel(s6_off_glmnet, s6_latest)
  s6_gd <- predict_spm_panel(s6_def_glmnet, s6_latest)
  s6_xo <- predict_spm_panel_xgb(s6_off_xgb, s6_latest)
  s6_xd <- predict_spm_panel_xgb(s6_def_xgb, s6_latest)
  stopifnot(identical(s6_go$player_id, s6_xo$player_id),
            identical(s6_gd$player_id, s6_xd$player_id))

  s6_table <- data.frame(
    player_id = s6_go$player_id,
    offense_spm_s6 = 0.5 * s6_go$pred + 0.5 * s6_xo$pred,
    defense_spm_s6 = 0.5 * s6_gd$pred + 0.5 * s6_xd$pred
  )

  # Hybrid tables: S6 where available, legacy elsewhere. Net = off − def
  # (raw internal convention; see predict_spm_panel_net()).
  offense_spm_ratings <- offense_spm_ratings %>%
    left_join(s6_table %>% select(player_id, offense_spm_s6), by = "player_id") %>%
    mutate(offense_spm = ifelse(!is.na(offense_spm_s6), offense_spm_s6, offense_spm)) %>%
    select(-offense_spm_s6)
  defense_spm_ratings <- defense_spm_ratings %>%
    left_join(s6_table %>% select(player_id, defense_spm_s6), by = "player_id") %>%
    mutate(defense_spm = ifelse(!is.na(defense_spm_s6), defense_spm_s6, defense_spm)) %>%
    select(-defense_spm_s6)
  spm_ratings <- spm_ratings %>%
    left_join(s6_table, by = "player_id") %>%
    mutate(spm = ifelse(!is.na(offense_spm_s6) & !is.na(defense_spm_s6),
                        offense_spm_s6 - defense_spm_s6, spm)) %>%
    select(-offense_spm_s6, -defense_spm_s6)

  cat(sprintf("S6 override: %d players on S6 values, %d on legacy fallback (GK + off-panel)\n",
              nrow(s6_table), nrow(offense_spm_ratings) - sum(offense_spm_ratings$player_id %in% s6_table$player_id)))

  # Rebuild the combined table from the hybrid pieces so combined_ratings
  # and the O/D tables can never disagree.
  combined_spm <- spm_ratings %>%
    select(player_id, player_name, total_minutes, spm) %>%
    left_join(offense_spm_ratings %>% select(player_id, offense_spm), by = "player_id") %>%
    left_join(defense_spm_ratings %>% select(player_id, defense_spm), by = "player_id") %>%
    left_join(rapm_ratings %>% select(player_id, rapm, offense, defense), by = "player_id") %>%
    arrange(desc(spm))

  panel_s6 <- list(
    offense_glmnet = s6_off_glmnet, defense_glmnet = s6_def_glmnet,
    offense_xgb = s6_off_xgb, defense_xgb = s6_def_xgb,
    latest_vintage = max(s6_panel$vintage_year),
    n_override = nrow(s6_table),
    panel_mtime = file.mtime(panel_path),
    config = "S4a + grouped-CV xgb, 50/50 per-target blend (D-W2)"
  )
  rm(s6_panel, panel_bundle, s6_latest, s6_go, s6_gd, s6_xo, s6_xd); invisible(gc(verbose = FALSE))
} else {
  cat("\nNOTE: spm_use_panel = FALSE — legacy SPM tables serve as the prior (S6 override skipped).\n")
}

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
  ),
  # Wave 4 (D-W2): S6 panel models + provenance; NULL when spm_use_panel
  # was FALSE. Legacy tables preserved for debuggability/rollback.
  panel_s6 = panel_s6,
  spm_ratings_legacy = spm_ratings_legacy,
  offense_spm_ratings_legacy = offense_spm_ratings_legacy,
  defense_spm_ratings_legacy = defense_spm_ratings_legacy
)

saveRDS(spm_results, file.path(cache_dir, "05_spm.rds"))
cat("Saved to cache-opta/05_spm.rds\n")

# 14. Multi-Target SPM (optional) ----
# Fit SPM predicting each value metric RAPM if multi-target results exist

# D6 (FABLE-PRIOR-FIX-PLAN.md): experimental gate, default FALSE -- see
# 04_rapm.R for rationale (inherits = FALSE for the same dplyr-collision
# reason as other pipeline config guards).
run_multi_target <- if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE
multi_rapm_path <- file.path(cache_dir, "04_rapm_multi.rds")

if (run_multi_target && file.exists(multi_rapm_path)) {
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

      if (tgt == "epv") {
        # FABLE-PRIOR-FIX-PLAN.md Step 5 (fixes dead layer L1): EPV keeps the
        # O/D split (D1: not zero-sum -- both teams accrue their own
        # threat), so its SPM prior needs an offense/defense PAIR, exactly
        # the base-path pattern above (section 10: two models + glmnet/xgb
        # blend each) -- NOT the single combined fit this loop used to
        # produce, which meant offense_spm/defense_spm never existed for
        # EPV and 06_xrapm.R's O/D prior had nothing to align against.
        if (!all(c("offense", "defense") %in% names(tgt_ratings))) {
          cli::cli_abort("Multi-target SPM for {.val {tgt}}: RAPM ratings missing {.field offense}/{.field defense} columns (expected an O/D-mode base RAPM fit).")
        }

        spm_train_tgt <- player_stats %>%
          inner_join(tgt_ratings %>% select(player_id, offense, defense), by = "player_id")

        if (nrow(spm_train_tgt) < 50) {
          cat(sprintf("  Skipping %s SPM: only %d players with both stats and RAPM\n",
                      tgt, nrow(spm_train_tgt)))
          next
        }

        offense_train_tgt <- spm_train_tgt %>% mutate(rapm = offense)
        defense_train_tgt <- spm_train_tgt %>% mutate(rapm = defense)

        # F4 (FABLE-PRIOR-FIX-PLAN.md review): same offense/defense
        # fit+blend helper the base path (Sections 10-11 above) uses -- same
        # directional sign constraints (def_lower/def_upper, keyed by
        # feature name: RAPM defense uses negative=good, so genuinely
        # defensive features must have non-positive coefficients), same
        # predictor_cols, same call order.
        od_fit_tgt <- .fit_od_spm_blend(offense_train_tgt, defense_train_tgt,
                                         offense_cols, defense_cols,
                                         def_lower, def_upper, player_stats)
        offense_glmnet_tgt <- od_fit_tgt$offense_spm_glmnet
        offense_xgb_tgt <- od_fit_tgt$offense_spm_xgb
        defense_glmnet_tgt <- od_fit_tgt$defense_spm_glmnet
        defense_xgb_tgt <- od_fit_tgt$defense_spm_xgb
        offense_spm_ratings_tgt <- od_fit_tgt$offense_spm_ratings
        defense_spm_ratings_tgt <- od_fit_tgt$defense_spm_ratings
        rm(od_fit_tgt)

        combined_spm_tgt <- offense_spm_ratings_tgt %>%
          select(player_id, player_name, total_minutes, offense_spm) %>%
          left_join(defense_spm_ratings_tgt %>% select(player_id, defense_spm), by = "player_id")

        multi_spm_results[[tgt]] <- list(
          offense_model_glmnet = offense_glmnet_tgt,
          offense_model_xgb = offense_xgb_tgt,
          defense_model_glmnet = defense_glmnet_tgt,
          defense_model_xgb = defense_xgb_tgt,
          ratings = combined_spm_tgt
        )
        cat(sprintf("  %s SPM (O/D pair): %d players rated\n", toupper(tgt), nrow(combined_spm_tgt)))

      } else if (tgt == "wpa") {
        # D2: WPA's off/def split is mechanically unidentified (near-zero-sum
        # target -- confirmed exactly zero-sum since Step 3), so a single net
        # SPM fit predicting the net base RAPM (`rapm`, from 04_rapm.R's
        # mode = "net" fit for wpa) is correct BY DESIGN, not a gap to fix.
        spm_data_tgt <- prepare_spm_regression_data(player_stats, tgt_ratings)

        if (nrow(spm_data_tgt) < 50) {
          cat(sprintf("  Skipping %s SPM: only %d players with both stats and RAPM\n",
                      tgt, nrow(spm_data_tgt)))
          next
        }

        spm_model_tgt <- fit_spm_opta(spm_data_tgt)
        spm_ratings_tgt <- calculate_spm_ratings(player_stats, spm_model_tgt)

        multi_spm_results[[tgt]] <- list(
          model = spm_model_tgt,
          ratings = spm_ratings_tgt
        )
        cat(sprintf("  %s SPM (net): %d players rated\n", toupper(tgt), nrow(spm_ratings_tgt)))

      } else {
        cli::cli_abort("Multi-target SPM: unknown target {.val {tgt}} (expected {.val epv} or {.val wpa}).")
      }

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
