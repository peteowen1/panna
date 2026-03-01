# 02b_optimize_params.R
# Joint 2D optimization of Bayesian skill parameters
#
# For each stat, jointly optimizes (L-BFGS-B):
#   - Prior strength (how much to regress toward the mean)
#   - Lambda (exponential decay rate for time-weighting)
#
# Prior center = minutes-weighted population mean (with position multipliers).
# Vectorized objective function with cumsum trick: O(n) per player.

# 1. Setup ----

library(data.table)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Override these before sourcing for custom runs
if (!exists("sample_n")) sample_n <- 500
if (!exists("n_cores")) n_cores <- max(1L, parallel::detectCores() %/% 2L)
if (!exists("optim_leagues")) optim_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")

output_path <- file.path(cache_dir, "02b_decay_params.rds")

# 3. Load Data ----

cat("\n=== Loading Match Stats ===\n")

match_stats <- readRDS(file.path(cache_dir, "01_match_stats.rds"))
cat("Total match-level rows:", nrow(match_stats), "\n")

# Filter to optimization leagues
if ("competition" %in% names(match_stats)) {
  match_stats <- match_stats[competition %in% optim_leagues]
} else if ("league" %in% names(match_stats)) {
  match_stats <- match_stats[league %in% optim_leagues]
}

cat("After filtering:", nrow(match_stats), "rows\n")
cat("Leagues:", paste(optim_leagues, collapse = ", "), "\n")
cat("Players:", uniqueN(match_stats$player_id), "\n")

# 4. Run Optimization ----

cat("\n=== Joint 2D Optimization (L-BFGS-B) ===\n")
cat(sprintf("Config: sample_n=%d, n_cores=%d\n", sample_n, n_cores))
cat("Parameters: prior strength + lambda (jointly)\n")
cat("Prior center: minutes-weighted mean (with position multipliers)\n\n")

decay_params <- get_default_decay_params()

start_time <- Sys.time()

decay_params <- optimize_all_priors(
  match_stats = match_stats,
  decay_params = decay_params,
  optimize_lambda = TRUE,
  sample_n = sample_n,
  n_cores = n_cores,
  verbose = TRUE
)

optim_duration <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nOptimization time: %.1f minutes\n", as.numeric(optim_duration)))

# 5. Save Parameters ----

saveRDS(decay_params, output_path)
cat(sprintf("Saved to: %s\n", output_path))

# 6. Backtest ----

cat("\n=== Backtest ===\n")

bt_start <- Sys.time()
bt <- backtest_skill_predictions(
  match_stats = match_stats,
  decay_params = decay_params,
  sample_n = sample_n,
  seed = 42
)
bt_duration <- difftime(Sys.time(), bt_start, units = "secs")
cat(sprintf("Backtest time: %.1f seconds\n\n", as.numeric(bt_duration)))

acc <- bt$accuracy
cat(sprintf("%-30s %8s %8s %8s %8s %8s %8s %8s\n",
            "Stat", "Metric", "Bayes", "Avg", "Last", "vsAvg", "vsLast", "N"))
cat(paste(rep("-", 100), collapse = ""), "\n")
for (i in 1:nrow(acc)) {
  r <- acc[i]
  cat(sprintf("%-30s %8s %8.4f %8.4f %8.4f %7.1f%% %7.1f%% %7d\n",
              r$stat, r$metric, r$value, r$avg_value, r$last_value,
              r$pct_vs_avg, r$pct_vs_last, r$n_predictions))
}

cat(sprintf("\nMean improvement vs career avg: %+.1f%%\n", mean(acc$pct_vs_avg)))
cat(sprintf("Mean improvement vs last game:  %+.1f%%\n", mean(acc$pct_vs_last)))
cat(sprintf("Stats where Bayes beats avg:    %d / %d\n", sum(acc$pct_vs_avg > 0), nrow(acc)))
cat(sprintf("Stats where Bayes beats last:   %d / %d\n", sum(acc$pct_vs_last > 0), nrow(acc)))

# 7. Summary ----

total_duration <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nTotal time: %.1f minutes\n", as.numeric(total_duration)))
cat("\n=== COMPLETE ===\n")
