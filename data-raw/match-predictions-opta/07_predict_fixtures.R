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

fixtures <- as.data.frame(match_dataset[match_dataset$split %in% c("fixture", "test", "train"), ])
n_upcoming <- sum(match_dataset$split == "fixture")
n_played <- nrow(fixtures) - n_upcoming
message(sprintf("  %d matches to predict (%d played + %d upcoming)", nrow(fixtures), n_played, n_upcoming))

if (nrow(fixtures) == 0) {
  message("  No upcoming fixtures found - skipping predictions.")
  saveRDS(data.frame(), output_path)
} else {

  # 5. Prepare Feature Matrix ----

  X_fix <- as.matrix(fixtures[, feature_cols, drop = FALSE])
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
    miss <- setdiff(augmented_features, colnames(Xo))
    if (length(miss) > 0) {
      filler <- matrix(0, nrow = nrow(Xo), ncol = length(miss),
                       dimnames = list(NULL, miss))
      Xo <- cbind(Xo, filler)
    }
    Xo <- Xo[, augmented_features, drop = FALSE]
    pr <- matrix(stats::predict(om_seg$model$model,
                                 xgboost::xgb.DMatrix(data = Xo)),
                 ncol = 3, byrow = FALSE)
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
    pred_home_goals[idx] <- (orig$home_goals + mir$away_goals) / 2
    pred_away_goals[idx] <- (orig$away_goals + mir$home_goals) / 2
    probs[idx, 1] <- (orig$pH + mir$pA) / 2
    probs[idx, 2] <- (orig$pD + mir$pD) / 2
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
