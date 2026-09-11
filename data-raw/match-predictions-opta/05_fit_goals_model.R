# 05_fit_goals_model.R
# Fit XGBoost Poisson models for home and away goal prediction
#
# Trains a POOLED model (all competitions) and an INTERNATIONAL-specialist
# model (national-team competitions only). Domestic fixtures are predicted
# with the pooled model; international fixtures with a blend of the two
# (see MATCH_INTL_BLEND_WEIGHT). Each training set is mirrored
# (orientation-symmetric) before fitting.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "05_goals_model.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 05_goals_model.rds")
  goals_models <- readRDS(output_path)
  for (seg in goals_models$segments) {
    message(sprintf("  [%s] home: %d rounds, away: %d rounds", seg,
                    goals_models[[seg]]$home$best_nrounds,
                    goals_models[[seg]]$away$best_nrounds))
  }
  return(invisible(NULL))
}

# 4. Load Data ----

message("\n=== Fitting Goals Models (pooled + international) ===\n")

match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))

# 5. Identify Feature Columns ----

exclude_cols <- c("match_id", "match_date", "match_status", "league", "season",
                  "home_team", "away_team", "home_team_id", "away_team_id",
                  "home_goals", "away_goals", "home_xg", "away_xg",
                  "result", "split", "outcome_label", "competition",
                  "home_npxg", "away_npxg")

all_cols <- names(match_dataset)
feature_cols <- setdiff(all_cols[sapply(match_dataset, is.numeric)], exclude_cols)
message(sprintf("  Feature columns: %d", length(feature_cols)))

# 6. Segment-fitting helper ----
# Fits home + away goals models for one competition segment, mirroring the
# training rows for orientation symmetry.

fit_goals_segment <- function(seg_name, train_df, val_df) {
  message(sprintf("\n--- Segment: %s ---", seg_name))

  train_df <- train_df[!is.na(train_df$home_goals) & !is.na(train_df$away_goals), ]
  val_df   <- val_df[!is.na(val_df$home_goals) & !is.na(val_df$away_goals), ]

  n_orig <- nrow(train_df)
  train_df <- rbind(train_df, mirror_match_rows(train_df))
  message(sprintf("  Train: %d matches (%d original + %d mirrored) | Val: %d",
                  nrow(train_df), n_orig, n_orig, nrow(val_df)))

  X_train <- as.matrix(train_df[, feature_cols, drop = FALSE])
  X_val   <- as.matrix(val_df[, feature_cols, drop = FALSE])
  X_train[is.na(X_train)] <- 0
  X_val[is.na(X_val)]     <- 0

  # group_ids = match_id keeps a match and its mirror_match_rows() twin in the
  # same CV fold (leakage-audit finding, 2026-08-27) — without it, xgb.cv's
  # default random per-row assignment can split a match from its own mirror.
  home_model <- fit_goals_xgb(X = X_train, y = train_df$home_goals,
                              nfolds = 5L, nrounds = 500L,
                              early_stopping = 30L, verbose = 0L,
                              group_ids = train_df$match_id)
  away_model <- fit_goals_xgb(X = X_train, y = train_df$away_goals,
                              nfolds = 5L, nrounds = 500L,
                              early_stopping = 30L, verbose = 0L,
                              group_ids = train_df$match_id)

  home_pred <- stats::predict(home_model$model, xgboost::xgb.DMatrix(X_val))
  away_pred <- stats::predict(away_model$model, xgboost::xgb.DMatrix(X_val))
  home_rmse <- sqrt(mean((val_df$home_goals - home_pred)^2))
  away_rmse <- sqrt(mean((val_df$away_goals - away_pred)^2))
  base_home_rmse <- sqrt(mean((val_df$home_goals - mean(train_df$home_goals))^2))
  base_away_rmse <- sqrt(mean((val_df$away_goals - mean(train_df$away_goals))^2))
  message(sprintf("  Val RMSE — home %.3f (base %.3f), away %.3f (base %.3f)",
                  home_rmse, base_home_rmse, away_rmse, base_away_rmse))

  list(home = home_model, away = away_model,
       val_metrics = list(home_rmse = home_rmse, away_rmse = away_rmse,
                          baseline_home_rmse = base_home_rmse,
                          baseline_away_rmse = base_away_rmse))
}

# 7. Fit each segment ----

train_all <- as.data.frame(match_dataset[match_dataset$split == "train", ])
val_all   <- as.data.frame(match_dataset[match_dataset$split == "val", ])
is_intl_train <- match_is_international(train_all$league)
is_intl_val   <- match_is_international(val_all$league)

goals_models <- list(
  pooled = fit_goals_segment("pooled", train_all, val_all),
  international = fit_goals_segment("international",
                                   train_all[is_intl_train, ],
                                   val_all[is_intl_val, ]),
  feature_cols = feature_cols,
  segments = c("pooled", "international")
)

# 8. Save ----

saveRDS(goals_models, output_path)

# 9. Summary ----

message("\n========================================")
message("Goals models complete (pooled + international)!")
message("========================================")
for (seg in goals_models$segments) {
  vm <- goals_models[[seg]]$val_metrics
  message(sprintf("  [%-13s] home RMSE=%.3f (base %.3f), away RMSE=%.3f (base %.3f)",
                  seg, vm$home_rmse, vm$baseline_home_rmse,
                  vm$away_rmse, vm$baseline_away_rmse))
}
message(sprintf("\nSaved to: %s", output_path))
