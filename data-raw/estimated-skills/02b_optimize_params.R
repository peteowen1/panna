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
# Use inherits = FALSE to avoid colliding with dplyr::sample_n and similar exports
if (!exists("sample_n", inherits = FALSE) || is.function(sample_n)) sample_n <- 500
if (!exists("n_cores", inherits = FALSE)) n_cores <- 1L
if (!exists("optim_leagues", inherits = FALSE)) optim_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")

output_path <- file.path(cache_dir, "02b_decay_params.rds")

# 3. Load Data ----

cat("\n=== Loading Match Stats ===\n")

match_stats <- readRDS(file.path(cache_dir, "01_match_stats.rds"))
cat("Total match-level rows:", nrow(match_stats), "\n")

# Filter to optimization leagues (use short code column)
if ("league" %in% names(match_stats)) {
  match_stats <- match_stats[league %in% optim_leagues]
} else if ("competition" %in% names(match_stats)) {
  match_stats <- match_stats[competition %in% optim_leagues]
}

cat("After filtering:", nrow(match_stats), "rows\n")
cat("Leagues:", paste(optim_leagues, collapse = ", "), "\n")
cat("Players:", uniqueN(match_stats$player_id), "\n")
if (nrow(match_stats) == 0) {
  stop("No rows after league filter. Check that league codes match optim_leagues.")
}


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

# Position multipliers for EVERY skill stat (review R015). The optimiser above
# runs on box-score stats only and cached multipliers for 52 of 88 skill
# stats; the other 36 (keeper stats, xG, GSAA, duel WOE) had no positional
# prior centre. Join xMetrics here, AFTER optimisation so the optimised set
# is unchanged, and compute multipliers on this full Big-5 table for every
# stat the estimators detect. Readers never fill gaps from their own slice.
xm_source <- if (identical(Sys.getenv("XMETRICS_SOURCE"), "remote")) "remote" else "local"
# copy(): the enrich adds columns by reference, and the backtest below must
# keep running on the box-score table it always used.
ms_x <- enrich_match_stats_with_xmetrics(data.table::copy(match_stats), fail_if_missing_frac = 0.6,
                                         source = xm_source)
mult_cols <- union(.detect_skill_stat_cols(ms_x),
                   intersect(union(.get_psr_skill_cols(), .get_gk_skill_cols()), names(ms_x)))
pm_all <- compute_position_multipliers(ms_x, mult_cols)
pm_all[names(decay_params$position_multipliers)] <- decay_params$position_multipliers
decay_params$position_multipliers <- pm_all
rm(ms_x); gc(verbose = FALSE)
cat(sprintf("Position multipliers cached for %d stats\n", length(pm_all)))
miss <- setdiff(union(.get_psr_skill_cols(), .get_gk_skill_cols()), names(pm_all))
if (length(miss) > 0) warning(sprintf("No position multiplier for %d registered skill stats: %s",
                                      length(miss), paste(miss, collapse = ", ")), call. = FALSE)

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
