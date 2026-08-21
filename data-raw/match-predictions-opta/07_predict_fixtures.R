# 07_predict_fixtures.R
# Predict upcoming match outcomes using fitted models
#
# Loads upcoming fixtures, applies all feature engineering, and runs
# both goals and outcome models to produce predictions.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "07_predictions.rds")

# 3. Load Models and Data ----

message("\n=== Predicting Fixtures ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models <- readRDS(file.path(cache_dir, "05_goals_model.rds"))
outcome_result <- readRDS(file.path(cache_dir, "06_outcome_model.rds"))

feature_cols <- goals_models$feature_cols
augmented_features <- outcome_result$augmented_features

# 4. Select matches to predict ----
# Predict ALL matches (played + fixtures) so the blog can show historical
# predictions on Results view alongside upcoming fixtures.
#
# ALL four splits, "val" included. It was omitted here until 2026-08-21, and
# because 04_build_match_dataset.R assigns "val" to a WHOLE season
# (`played$split[played$season_end_year == val_sey] <- "val"`, not a row
# sample), that silently dropped one entire season from every published
# predictions file -- a different season each year as the dataset rolls
# forward. Nothing failed; the row count stayed plausible.
#
# Predicting on val is not a leak: model quality is measured in
# 08_evaluate_model.R on its own splits. Step 07 exists to publish a prediction
# for every match, which is why "train" and "test" were already included.
PREDICT_SPLITS <- c("fixture", "test", "val", "train")
fixtures <- as.data.frame(match_dataset[match_dataset$split %in% PREDICT_SPLITS, ])
n_upcoming <- sum(match_dataset$split == "fixture")
n_played <- nrow(fixtures) - n_upcoming
message(sprintf("  %d matches to predict (%d played + %d upcoming)", nrow(fixtures), n_played, n_upcoming))

# Guard: every season with played matches must survive into the prediction set.
# The bug above was invisible precisely because a missing season leaves a row
# count that still looks reasonable -- so assert on seasons, not on rows.
if ("season_end_year" %in% names(match_dataset)) {
  seasons_in  <- sort(unique(match_dataset$season_end_year[match_dataset$split != "fixture"]))
  seasons_out <- sort(unique(fixtures$season_end_year[fixtures$split != "fixture"]))
  missing_seasons <- setdiff(seasons_in, seasons_out)
  if (length(missing_seasons) > 0) {
    stop(sprintf(
      paste0("Prediction set is missing %d played season(s) present in the match dataset: %s.
",
             "  Splits selected: %s
",
             "  Splits in dataset: %s
",
             "  A split is being excluded -- see PREDICT_SPLITS above."),
      length(missing_seasons), paste(missing_seasons, collapse = ", "),
      paste(PREDICT_SPLITS, collapse = "/"),
      paste(sort(unique(match_dataset$split)), collapse = "/")), call. = FALSE)
  }
  message(sprintf("  Season coverage OK: %d played season(s), none dropped by the split filter",
                  length(seasons_in)))
}

if (nrow(fixtures) == 0) {
  message("  No upcoming fixtures found - skipping predictions.")
  saveRDS(data.frame(), output_path)
} else {

  # 5. Prepare Feature Matrix ----

  X_fix <- as.matrix(fixtures[, feature_cols, drop = FALSE])

  # --- issue #85 detection guard (no-silent-imputation rule) ----------------
  # Step 04 deliberately PRESERVES NAs in team-strength features (Elo, team
  # rating aggregates, their diffs) so a failed strength join stays visible.
  # The zero-fill below is required for train/serve PARITY — the models were
  # trained on zero-filled matrices, so removing the fill would create a
  # train/serve skew. But a blanket fill silently turns a fixture whose
  # strength features fully failed to join into an all-zero row with a
  # confident-looking prediction and no warning.
  #
  # Guard: BEFORE filling, measure per-fixture the fraction of NA cells over
  # the STRENGTH-feature subset of feature_cols (identified the SAME WAY step
  # 04 does in its `skip_zero_fill` block — team-aggregate ratings + Elo +
  # their diffs; we exclude rolling-form cols, which step 04 already imputes,
  # and the home/away_goals/xg outcome cols, which are legitimately NA for
  # unplayed fixtures). Loudly warn on any fixture above the threshold and
  # SOFT-flag it (degraded_features) so downstream steps can grey it out —
  # publish + flag, not a hard drop. The zero-fill itself is kept intact.
  strength_feature_cols <- unique(c(
    grep("^(home|away)_elo$|^elo_diff$", feature_cols, value = TRUE),
    grep("^(home|away)_(sum|avg|max|min|gk|stdev)_", feature_cols, value = TRUE),
    grep("^(home|away)_sk_", feature_cols, value = TRUE),
    grep("_diff$", feature_cols, value = TRUE)
  ))
  DEGRADED_NA_THRESHOLD <- 0.25
  if (length(strength_feature_cols) > 0) {
    Xs <- X_fix[, strength_feature_cols, drop = FALSE]
    strength_na_frac <- rowMeans(is.na(Xs))
  } else {
    strength_na_frac <- rep(0, nrow(X_fix))
  }
  degraded_features <- strength_na_frac > DEGRADED_NA_THRESHOLD
  if (any(degraded_features)) {
    deg_idx <- which(degraded_features)
    warning(sprintf(
      "issue #85: %d fixture(s) have >%.0f%% of strength features missing (failed join). Predictions are published but flagged degraded:\n%s",
      length(deg_idx), 100 * DEGRADED_NA_THRESHOLD,
      paste(sprintf("  %s: %s vs %s (%.0f%% strength NA)",
                    fixtures$match_id[deg_idx],
                    fixtures$home_team[deg_idx], fixtures$away_team[deg_idx],
                    100 * strength_na_frac[deg_idx]),
            collapse = "\n")
    ), call. = FALSE)
  } else {
    message(sprintf("  Strength-feature join OK: 0 fixtures above the %.0f%% NA degraded threshold (issue #85 guard)",
                    100 * DEGRADED_NA_THRESHOLD))
  }
  # --- end issue #85 guard --------------------------------------------------

  # Zero-fill kept INTACT for train/serve parity (step 04 trained on this).
  X_fix[is.na(X_fix)] <- 0

  # Helper: run a segment's goals + outcome models on a feature matrix.
  predict_block <- function(X, gm_seg, om_seg) {
    X[is.na(X)] <- 0
    d <- xgboost::xgb.DMatrix(data = X)
    hg <- stats::predict(gm_seg$home$model, d)
    ag <- stats::predict(gm_seg$away$model, d)
    gf <- cbind(pred_home_goals = hg, pred_away_goals = ag,
                pred_goal_diff = hg - ag, pred_total_goals = hg + ag)
    Xo <- cbind(X, gf)
    # Structurally empty: augmented_features is feature_cols + the four goal
    # columns (06_fit_outcome_model.R:39) and Xo carries exactly those. A gap
    # means steps 05 and 06 were fitted from different feature_cols vintages.
    # Zero-filling hid that -- predictions stayed plausible while N features
    # silently read as 0. Same guard as R/knockout_model.R:.ko_predict().
    miss <- setdiff(augmented_features, colnames(Xo))
    if (length(miss) > 0) {
      stop(sprintf(
        paste0("Goals and outcome models disagree on the feature set: %d of %d ",
               "outcome-model features absent from the goals-model matrix (%s%s). ",
               "Refit steps 05 and 06 from the same match dataset."),
        length(miss), length(augmented_features),
        paste(utils::head(miss, 10), collapse = ", "),
        if (length(miss) > 10) ", ..." else ""))
    }
    Xo <- Xo[, augmented_features, drop = FALSE]
    pr <- softprob_matrix(
      stats::predict(om_seg$model$model, xgboost::xgb.DMatrix(data = Xo)),
      nrow(Xo)
    )
    list(home_goals = hg, away_goals = ag,
         pH = pr[, 1], pD = pr[, 2], pA = pr[, 3])
  }

  # 6+7. Predict Goals + Outcomes (routed + blended, orientation-symmetrized) ----
  # Routing: domestic fixtures use the pooled (all-data) model; international
  # fixtures use a blend of the pooled model and the international specialist
  # (MATCH_INTL_BLEND_WEIGHT on the specialist). Every fixture is then predicted
  # in BOTH orientations and averaged — the models are trained on mirrored data
  # so they are ~symmetric, and averaging makes the output exactly invariant to
  # which team is arbitrarily listed as home (home advantage is carried by the
  # mirror-negated `home_field`).

  fixtures_mir <- mirror_match_rows(fixtures)
  X_mir <- as.matrix(fixtures_mir[, feature_cols, drop = FALSE])
  X_mir[is.na(X_mir)] <- 0

  w_intl <- MATCH_INTL_BLEND_WEIGHT
  blend_preds <- function(p, i) list(
    home_goals = (1 - w_intl) * p$home_goals + w_intl * i$home_goals,
    away_goals = (1 - w_intl) * p$away_goals + w_intl * i$away_goals,
    pH = (1 - w_intl) * p$pH + w_intl * i$pH,
    pD = (1 - w_intl) * p$pD + w_intl * i$pD,
    pA = (1 - w_intl) * p$pA + w_intl * i$pA)
  predict_routed <- function(X, is_intl) {
    pooled <- predict_block(X, goals_models$pooled, outcome_result$pooled)
    if (!is_intl) return(pooled)
    intl <- predict_block(X, goals_models$international, outcome_result$international)
    blend_preds(pooled, intl)
  }

  is_intl <- match_is_international(fixtures$league)
  pred_home_goals <- numeric(nrow(fixtures))
  pred_away_goals <- numeric(nrow(fixtures))
  probs <- matrix(0, nrow = nrow(fixtures), ncol = 3)

  for (grp in c(FALSE, TRUE)) {
    idx <- which(is_intl == grp)
    if (length(idx) == 0) next
    message(sprintf("  Predicting %d %s fixtures (both orientations, averaged)...",
                    length(idx),
                    if (grp) "international (pooled+specialist blend)" else "domestic (pooled)"))
    orig <- predict_routed(X_fix[idx, , drop = FALSE], grp)
    mir  <- predict_routed(X_mir[idx, , drop = FALSE], grp)
    # In the mirrored orientation the original home team is the away team.
    # H/A swap under mirroring and get averaged; draw doesn't swap, so the
    # orig draw probability IS the symmetrised value (averaging it with
    # itself is a no-op). Code-review item 17.
    pred_home_goals[idx] <- (orig$home_goals + mir$away_goals) / 2
    pred_away_goals[idx] <- (orig$away_goals + mir$home_goals) / 2
    probs[idx, 1] <- (orig$pH + mir$pA) / 2
    probs[idx, 2] <- (orig$pD + mir$pD) / 2  # == orig$pD; left averaged for symmetry-of-form
    probs[idx, 3] <- (orig$pA + mir$pH) / 2
  }

  # 8. Build Prediction Table ----

  predictions <- data.frame(
    match_id = fixtures$match_id,
    match_date = fixtures$match_date,
    league = fixtures$league,
    season = fixtures$season,
    home_team = fixtures$home_team,
    away_team = fixtures$away_team,
    pred_home_goals = round(pred_home_goals, 2),
    pred_away_goals = round(pred_away_goals, 2),
    prob_H = round(probs[, 1], 3),
    prob_D = round(probs[, 2], 3),
    prob_A = round(probs[, 3], 3),
    predicted_result = c("H", "D", "A")[apply(probs, 1, which.max)],
    status = ifelse(fixtures$split == "fixture", "fixture", "played"),
    # issue #85: soft flag for fixtures whose strength features largely failed
    # to join (zero-filled for parity, but the prediction is unreliable).
    # Downstream blog steps should grey these out rather than drop them.
    degraded_features = degraded_features,
    stringsAsFactors = FALSE
  )

  predictions <- predictions[order(predictions$match_date, predictions$league), ]

  # 9. Save ----

  saveRDS(predictions, output_path)

  # Also save CSV for easy viewing
  csv_path <- file.path(cache_dir, "predictions.csv")
  write.csv(predictions, csv_path, row.names = FALSE)

  # Save parquet for GitHub release upload
  parquet_path <- file.path(cache_dir, "predictions.parquet")
  arrow::write_parquet(predictions, parquet_path)

  # 10. Summary ----

  message("\n========================================")
  message("Match predictions complete!")
  message("========================================")
  message(sprintf("Predictions: %d total (%d played, %d fixtures)",
                  nrow(predictions),
                  sum(predictions$status == "played"),
                  sum(predictions$status == "fixture")))
  message(sprintf("Leagues: %s", paste(unique(predictions$league), collapse = ", ")))

  # Show predictions grouped by league
  for (lg in unique(predictions$league)) {
    lg_preds <- predictions[predictions$league == lg, ]
    message(sprintf("\n%s (%d matches):", lg, nrow(lg_preds)))
    for (i in seq_len(min(nrow(lg_preds), 10))) {
      p <- lg_preds[i, ]
      message(sprintf("  %s: %s vs %s  |  %.0f%% H / %.0f%% D / %.0f%% A  |  %.1f-%.1f",
                      substr(p$match_date, 1, 10),
                      p$home_team, p$away_team,
                      100 * p$prob_H, 100 * p$prob_D, 100 * p$prob_A,
                      p$pred_home_goals, p$pred_away_goals))
    }
    if (nrow(lg_preds) > 10) {
      message(sprintf("  ... and %d more", nrow(lg_preds) - 10))
    }
  }

  message(sprintf("\nSaved to: %s", output_path))
  message(sprintf("CSV: %s", csv_path))
  message(sprintf("Parquet: %s", parquet_path))
}
