# 03_epv_shap_analysis.R
# SHAP analysis of EPV model — are leagues stylistically different?
#
# Run from panna directory: Rscript data-raw/epv/03_epv_shap_analysis.R
#
# Requires:
#   - data-raw/cache/epv/epv_model.rds (trained with league_id feature)
#
# Outputs:
#   - data-raw/cache/epv/shap_importance.png
#   - data-raw/cache/epv/shap_league_dependence.png
#   - Console summary of league SHAP statistics

library(cli)
library(ggplot2)
devtools::load_all()

# 1. Configuration ----

CACHE_DIR <- "data-raw/cache/epv"
SAMPLE_SIZE <- 50000  # subsample for SHAP (full dataset is millions of rows)

# Leagues to visualise (can run on a subset for speed)
if (!exists("SHAP_LEAGUES")) SHAP_LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
if (!exists("SHAP_SEASONS")) SHAP_SEASONS <- c("2023-2024")

cli_h1("EPV SHAP Analysis")

# 2. Load model ----

epv_model <- readRDS(file.path(CACHE_DIR, "epv_model.rds"))
feature_cols <- epv_model$panna_metadata$feature_cols

if (!"league_id" %in% feature_cols) {
  cli_abort("EPV model does not include league_id feature. Retrain with 01_train_epv_models.R first.")
}

cli_alert_success("Loaded EPV model ({epv_model$method} method, {length(feature_cols)} features)")

# 3. Build SHAP dataset ----

cli_h2("Step 1: Build Feature Matrix")

all_features <- list()

for (league in SHAP_LEAGUES) {
  for (season in SHAP_SEASONS) {
    tryCatch({
      events <- load_opta_match_events(league, season = season, source = "local")
      spadl <- convert_opta_to_spadl(events)
      spadl$league <- league

      feats <- create_epv_features_simple(spadl, league = league)
      feats$league_code <- league
      all_features[[paste(league, season)]] <- feats

      cli_alert_success("  {league} {season}: {nrow(feats)} actions")
    }, error = function(e) {
      cli_alert_warning("  Skipping {league} {season}: {e$message}")
    })
  }
}

features_dt <- data.table::rbindlist(all_features, fill = TRUE)
cli_alert_success("Total: {format(nrow(features_dt), big.mark=',')} actions from {length(SHAP_LEAGUES)} leagues")

# Subsample for SHAP computation (memory-intensive)
if (nrow(features_dt) > SAMPLE_SIZE) {
  set.seed(42)
  idx <- sample(nrow(features_dt), SAMPLE_SIZE)
  features_sample <- features_dt[idx]
  cli_alert_info("Subsampled to {SAMPLE_SIZE} actions for SHAP computation")
} else {
  features_sample <- features_dt
}

# 4. Compute SHAP values ----

cli_h2("Step 2: Compute SHAP Values")

X <- as.matrix(features_sample[, ..feature_cols])
X[is.na(X)] <- 0

shap_raw <- predict(epv_model$model, X, predcontrib = TRUE)

if (epv_model$method == "goal") {
  # Multinomial: shap_raw is (n * n_class) x (n_features + 1)
  # Reshape to get class 0 (team scores) SHAP values
  n <- nrow(X)
  n_feat <- length(feature_cols) + 1  # +1 for BIAS
  n_class <- 3

  # Rows cycle: row1-class0, row1-class1, row1-class2, row2-class0, ...
  class0_idx <- seq(1, n * n_class, by = n_class)
  shap_class0 <- shap_raw[class0_idx, ]

  shap_values <- shap_class0[, 1:length(feature_cols)]
  colnames(shap_values) <- feature_cols
  shap_bias <- shap_class0[, length(feature_cols) + 1]
} else {
  # Regression: shap_raw is n x (n_features + 1)
  shap_values <- shap_raw[, 1:length(feature_cols)]
  colnames(shap_values) <- feature_cols
  shap_bias <- shap_raw[, length(feature_cols) + 1]
}

cli_alert_success("SHAP values computed: {nrow(shap_values)} x {ncol(shap_values)}")

# 5. Feature Importance (mean |SHAP|) ----

cli_h2("Step 3: Feature Importance")

mean_abs_shap <- colMeans(abs(shap_values))
importance_df <- data.frame(
  feature = names(mean_abs_shap),
  mean_abs_shap = as.numeric(mean_abs_shap),
  stringsAsFactors = FALSE
)
importance_df <- importance_df[order(-importance_df$mean_abs_shap), ]

cat("\nSHAP Feature Importance (mean |SHAP|):\n")
print(importance_df, row.names = FALSE)

league_rank <- which(importance_df$feature == "league_id")
league_pct <- round(importance_df$mean_abs_shap[league_rank] /
                    sum(importance_df$mean_abs_shap) * 100, 1)
cli_alert_info("league_id ranks #{league_rank} of {nrow(importance_df)} features ({league_pct}% of total importance)")

# Plot
p_importance <- ggplot(importance_df, aes(x = reorder(feature, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(aes(fill = feature == "league_id"), show.legend = FALSE) +
  scale_fill_manual(values = c("grey40", "steelblue")) +
  coord_flip() +
  labs(title = "EPV Model — SHAP Feature Importance",
       subtitle = paste0("league_id ranks #", league_rank, " (", league_pct, "% of total)"),
       x = NULL, y = "Mean |SHAP value|") +
  theme_minimal(base_size = 14)

ggsave(file.path(CACHE_DIR, "shap_importance.png"), p_importance, width = 8, height = 6, dpi = 150)
cli_alert_success("Saved: {CACHE_DIR}/shap_importance.png")

# 6. League Dependence Plots ----

cli_h2("Step 4: League Dependence Analysis")

# SHAP of start_x colored by league — shows spatial value differences
dep_df <- data.frame(
  start_x = features_sample$start_x,
  shap_start_x = shap_values[, "start_x"],
  shap_league = shap_values[, "league_id"],
  league = features_sample$league_code,
  stringsAsFactors = FALSE
)

# start_x dependence by league
p_dep <- ggplot(dep_df, aes(x = start_x, y = shap_start_x, color = league)) +
  geom_point(alpha = 0.05, size = 0.5) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2) +
  labs(title = "EPV SHAP Dependence: Ball Position (start_x) by League",
       subtitle = "Higher = action at this position is more likely to lead to a goal",
       x = "Ball Position (0 = own goal, 100 = opponent goal)",
       y = "SHAP value for start_x",
       color = "League") +
  theme_minimal(base_size = 14)

ggsave(file.path(CACHE_DIR, "shap_league_dependence.png"), p_dep, width = 10, height = 6, dpi = 150)
cli_alert_success("Saved: {CACHE_DIR}/shap_league_dependence.png")

# League SHAP summary — mean SHAP by league
league_shap_summary <- aggregate(shap_league ~ league, data = dep_df, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), median = median(x))
})
league_shap_summary <- do.call(data.frame, league_shap_summary)
names(league_shap_summary) <- c("league", "mean_shap", "sd_shap", "median_shap")
league_shap_summary <- league_shap_summary[order(-league_shap_summary$mean_shap), ]

cat("\nLeague SHAP Summary (league_id feature):\n")
cat("Positive = model thinks this league has higher scoring probability\n")
cat("Negative = lower scoring probability\n\n")
print(league_shap_summary, row.names = FALSE)

cli_h1("SHAP Analysis Complete!")
