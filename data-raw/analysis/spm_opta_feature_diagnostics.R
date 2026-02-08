# spm_opta_feature_diagnostics.R
# Diagnostic script for Opta SPM feature analysis
#
# Loads cached SPM results, extracts feature importance from Elastic Net
# and XGBoost models, prints baseline metrics, and checks which proposed
# new columns exist in the raw data.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# 2. Load Cached Results ----

cat("\n=== Loading Cached Results ===\n")

spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))
raw_data <- readRDS(file.path(cache_dir, "01_raw_data.rds"))

cat("SPM results loaded successfully\n")
cat("Raw data loaded successfully\n")

# 3. Baseline Metrics ----

cat("\n=== Baseline Metrics ===\n")

# CV RMSE
model_comparison <- spm_results$model_comparison
cat(sprintf("CV RMSE (Elastic Net): %.4f\n", model_comparison$cv_rmse_glmnet))
cat(sprintf("CV RMSE (XGBoost):     %.4f\n", model_comparison$cv_rmse_xgb))
cat(sprintf("Blend weight:          %.2f\n", model_comparison$blend_weight))

# Correlation with RAPM
combined <- spm_results$combined_ratings
if (!is.null(combined) && "rapm" %in% names(combined) && "spm" %in% names(combined)) {
  valid <- combined[!is.na(combined$rapm) & !is.na(combined$spm), ]
  cat(sprintf("\nCorrelation with RAPM (n=%d):\n", nrow(valid)))
  cat(sprintf("  Overall SPM:  %.4f\n", cor(valid$spm, valid$rapm)))
  if ("offense_spm" %in% names(valid) && "offense" %in% names(valid)) {
    v_off <- valid[!is.na(valid$offense_spm) & !is.na(valid$offense), ]
    cat(sprintf("  Offense SPM:  %.4f (n=%d)\n", cor(v_off$offense_spm, v_off$offense), nrow(v_off)))
  }
  if ("defense_spm" %in% names(valid) && "defense" %in% names(valid)) {
    v_def <- valid[!is.na(valid$defense_spm) & !is.na(valid$defense), ]
    cat(sprintf("  Defense SPM:  %.4f (n=%d)\n", cor(v_def$defense_spm, v_def$defense), nrow(v_def)))
  }
}

# Validation R-squared
if (!is.null(spm_results$validation)) {
  val <- spm_results$validation
  if ("r_squared" %in% names(val)) {
    cat(sprintf("\nValidation R-squared: %.4f\n", val$r_squared))
  }
  if ("rmse" %in% names(val)) {
    cat(sprintf("Validation RMSE:      %.4f\n", val$rmse))
  }
}

# 4. Elastic Net Feature Importance ----

cat("\n=== Elastic Net Coefficients (Top 40) ===\n")

# Overall model
spm_glmnet <- spm_results$spm_glmnet
importance_overall <- get_spm_feature_importance(spm_glmnet, n = 40)
cat("\n--- Overall Model (Top 40) ---\n")
print(as.data.frame(importance_overall))

# Offense model
if (!is.null(spm_results$offense_spm_glmnet)) {
  importance_off <- get_spm_feature_importance(spm_results$offense_spm_glmnet, n = 30)
  cat("\n--- Offense Model (Top 30) ---\n")
  print(as.data.frame(importance_off))
}

# Defense model
if (!is.null(spm_results$defense_spm_glmnet)) {
  importance_def <- get_spm_feature_importance(spm_results$defense_spm_glmnet, n = 30)
  cat("\n--- Defense Model (Top 30) ---\n")
  print(as.data.frame(importance_def))
}

# 5. Features Zeroed by Lasso ----

cat("\n=== Features Zeroed by Elastic Net ===\n")

coefs <- as.matrix(stats::coef(spm_glmnet, s = "lambda.min"))
all_features <- rownames(coefs)[-1]  # Exclude intercept
nonzero <- all_features[coefs[-1, 1] != 0]
zeroed <- all_features[coefs[-1, 1] == 0]

cat(sprintf("\nTotal features: %d\n", length(all_features)))
cat(sprintf("Non-zero features: %d\n", length(nonzero)))
cat(sprintf("Zeroed features: %d\n", length(zeroed)))
if (length(zeroed) > 0) {
  cat("\nZeroed features:\n")
  cat(paste(" ", zeroed, collapse = "\n"), "\n")
}

# 6. XGBoost Feature Importance (Top 30) ----

cat("\n=== XGBoost Feature Importance (Top 30) ===\n")

get_xgb_importance <- function(model, n = 30) {
  if (is.null(model) || is.null(model$handle)) return(NULL)
  imp <- xgboost::xgb.importance(model = model)
  if (is.null(imp) || nrow(imp) == 0) return(NULL)
  head(imp[order(-imp$Gain), ], n)
}

spm_xgb <- spm_results$spm_xgb
if (!is.null(spm_xgb)) {
  xgb_imp <- get_xgb_importance(spm_xgb, n = 30)
  if (!is.null(xgb_imp)) {
    cat("\n--- Overall XGBoost ---\n")
    print(as.data.frame(xgb_imp))
  } else {
    cat("Could not extract XGBoost importance (model may not have handle)\n")
  }
}

if (!is.null(spm_results$offense_spm_xgb)) {
  xgb_off <- get_xgb_importance(spm_results$offense_spm_xgb, n = 20)
  if (!is.null(xgb_off)) {
    cat("\n--- Offense XGBoost (Top 20) ---\n")
    print(as.data.frame(xgb_off))
  }
}

if (!is.null(spm_results$defense_spm_xgb)) {
  xgb_def <- get_xgb_importance(spm_results$defense_spm_xgb, n = 20)
  if (!is.null(xgb_def)) {
    cat("\n--- Defense XGBoost (Top 20) ---\n")
    print(as.data.frame(xgb_def))
  }
}

# 7. Check Proposed New Columns in Raw Data ----

cat("\n=== Checking Proposed New Columns in Raw Data ===\n")

# Get a sample of raw opta_stats to check column existence
opta_stats <- raw_data$opta_stats
if (is.null(opta_stats)) {
  opta_stats <- raw_data$stats  # fallback name
}

if (!is.null(opta_stats)) {
  cat(sprintf("Raw Opta stats: %d rows, %d columns\n", nrow(opta_stats), ncol(opta_stats)))

  proposed_cols <- c(
    "totalFwdZonePass", "accurateFwdZonePass",
    "openPlayPass", "successfulOpenPlayPass",
    "errorLeadToShot", "errorLeadToGoal",
    "attFastbreak", "shotFastbreak",
    "attOpenplay", "attSetpiece", "attHdTotal", "attHdGoal", "attOneOnOne",
    "totalCrossNocorner", "accurateCrossNocorner",
    "penaltyWon", "penaltyConceded",
    "offtargetAttAssist",
    "lastManTackle", "sixYardBlock", "clearanceOffLine",
    "totalKeeperSweeper", "accurateKeeperSweeper",
    "attemptsConcededIbox", "attemptsConcededObox",
    "gkSmother",
    "unsuccessfulTouch", "overrun",
    "totalFlickOn", "accurateFlickOn"
  )

  cat("\nColumn availability and non-zero counts:\n")
  for (col in proposed_cols) {
    if (col %in% names(opta_stats)) {
      vals <- as.numeric(opta_stats[[col]])
      nonzero_count <- sum(!is.na(vals) & vals != 0)
      total <- sum(!is.na(vals))
      cat(sprintf("  %-30s EXISTS  non-zero: %6d / %6d (%.1f%%)\n",
                  col, nonzero_count, total,
                  ifelse(total > 0, 100 * nonzero_count / total, 0)))
    } else {
      cat(sprintf("  %-30s MISSING\n", col))
    }
  }
} else {
  cat("Could not find opta_stats in raw data\n")
  cat("Available names in raw_data:", paste(names(raw_data), collapse = ", "), "\n")
}

# 8. Current Feature Count ----

cat("\n=== Current Feature Summary ===\n")

player_stats <- spm_results$player_stats
if (!is.null(player_stats)) {
  p90_cols <- names(player_stats)[grepl("_p90$", names(player_stats))]
  rate_cols <- names(player_stats)[grepl("accuracy|success|conversion|per_shot|ratio|percentage|effectiveness|differential", names(player_stats))]
  pos_cols <- names(player_stats)[grepl("^is_", names(player_stats))]

  cat(sprintf("Total columns: %d\n", ncol(player_stats)))
  cat(sprintf("Per-90 features: %d\n", length(p90_cols)))
  cat(sprintf("Derived rate features: %d\n", length(rate_cols)))
  cat(sprintf("Position dummies: %d\n", length(pos_cols)))
  cat(sprintf("Total features used by SPM: %d\n", length(p90_cols) + length(rate_cols) + length(pos_cols)))

  cat("\nAll per-90 features:\n")
  cat(paste(" ", sort(p90_cols), collapse = "\n"), "\n")
  cat("\nAll derived rate features:\n")
  cat(paste(" ", sort(rate_cols), collapse = "\n"), "\n")
}

cat("\n=== DIAGNOSTICS COMPLETE ===\n")
