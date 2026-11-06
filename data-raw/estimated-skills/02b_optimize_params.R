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

# Overridable so a test run can read and write a scratch folder, not the cache.
if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-skills")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# Override these before sourcing for custom runs
# Use inherits = FALSE to avoid colliding with dplyr::sample_n and similar exports
if (!exists("sample_n", inherits = FALSE) || is.function(sample_n)) sample_n <- 500
if (!exists("n_cores", inherits = FALSE)) n_cores <- 1L
# Seed for the player sample; rerun with another seed to measure sampling noise.
if (!exists("optim_seed", inherits = FALSE)) optim_seed <- 42L
# A prior that lands on a search bound was not fitted (panna#432: 18 of the 68
# priors in the 2026-09-10 file sat at the old start value or the bound). The
# run stops unless this is TRUE, so a capped value is never saved unseen.
if (!exists("allow_bound_priors", inherits = FALSE)) allow_bound_priors <- FALSE
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

# Join xMetrics BEFORE optimising, so the xG-type stats in get_stat_tiers()
# get fitted priors too (panna#432: the box-only table skipped them, and the
# published file's xG priors came from a separate run). copy(): the enrich adds
# columns by reference, and the backtest below keeps the box-score table.
xm_source <- if (identical(Sys.getenv("XMETRICS_SOURCE"), "remote")) "remote" else "local"
ms_x <- enrich_match_stats_with_xmetrics(data.table::copy(match_stats), fail_if_missing_frac = 0.6,
                                         source = xm_source)
tiers <- get_stat_tiers()
want_stats <- unlist(tiers[c("tier1", "tier2", "efficiency", "xmetrics")], use.names = FALSE)
missing_stats <- setdiff(want_stats, names(ms_x))
if (length(missing_stats) > 0) {
  stop(sprintf("%d stats in get_stat_tiers() are not in the enriched table, so would get no prior: %s",
               length(missing_stats), paste(missing_stats, collapse = ", ")))
}
cat(sprintf("Enriched with xMetrics (%s): %d cols; %d stats to optimise\n",
            xm_source, ncol(ms_x), length(want_stats)))
# The enrich fills unmatched rows with 0, so a present column can still be
# mostly empty. Print how much of each xMetrics stat is non-zero.
for (s in tiers$xmetrics) {
  cat(sprintf("  %-38s non-zero on %5.1f%% of %d player-matches\n", s,
              100 * mean(ms_x[[s]] != 0, na.rm = TRUE), nrow(ms_x)))
}


# 4. Run Optimization ----

cat("\n=== Joint 2D Optimization (L-BFGS-B) ===\n")
cat(sprintf("Config: sample_n=%d, seed=%d, n_cores=%d\n", sample_n, optim_seed, n_cores))
cat("Parameters: prior strength + lambda (jointly)\n")
cat("Prior center: minutes-weighted mean (with position multipliers)\n\n")

decay_params <- get_default_decay_params()

start_time <- Sys.time()

decay_params <- optimize_all_priors(
  match_stats = ms_x,
  decay_params = decay_params,
  optimize_lambda = TRUE,
  sample_n = sample_n,
  seed = optim_seed,
  n_cores = n_cores,
  verbose = TRUE
)

optim_duration <- difftime(Sys.time(), start_time, units = "mins")
cat(sprintf("\nOptimization time: %.1f minutes\n", as.numeric(optim_duration)))

# Coverage: every requested stat must come back with a finite prior and lambda.
# (If every fit failed, stat_priors is empty and the checks below would see
# zero-length vectors and pass, so test that first.)
if (length(decay_params$stat_priors) == 0) stop("optimize_all_priors() returned no priors at all")
got_prior <- unname(decay_params$stat_priors[want_stats])
got_lambda <- vapply(want_stats, function(s) {
  v <- decay_params[[s]]
  if (is.numeric(v) && length(v) == 1) v else NA_real_
}, numeric(1), USE.NAMES = FALSE)
no_fit <- want_stats[!is.finite(got_prior) | !is.finite(got_lambda)]
if (length(no_fit) > 0) {
  stop(sprintf("%d of %d stats came back with no finite prior or lambda: %s",
               length(no_fit), length(want_stats), paste(no_fit, collapse = ", ")))
}
# Bounds: the ranges optimize_stat_prior() searches by default (prior 0.1 to
# 1000 for rate stats, 0.1 to 500 for efficiency; lambda 0 to 0.02).
upper <- ifelse(want_stats %in% tiers$efficiency, 500, 1000)
# A lambda still at its start value never moved: on rare stats the decay is
# not identifiable (2026-11-07 run at sample_n 500: six stats, finishing and
# placement among them, came back at exactly 0.003).
lambda_start <- vapply(want_stats, .resolve_lambda, numeric(1), decay_params = get_default_decay_params(),
                       eff_map = .classify_skill_stats(), USE.NAMES = FALSE)
at_bound <- got_prior <= 0.1 * 1.01 | got_prior >= upper * 0.99 |
  got_lambda <= 1e-6 | got_lambda >= 0.02 * 0.99 |
  abs(got_lambda - lambda_start) <= 1e-4 * lambda_start
cat(sprintf("Fitted %d stats: %d with a prior or lambda on a search bound or still at its start value\n",
            length(want_stats), sum(at_bound)))
if (any(at_bound)) {
  print(data.table(stat = want_stats[at_bound], prior = got_prior[at_bound],
                   prior_upper = upper[at_bound], lambda = got_lambda[at_bound],
                   lambda_start = lambda_start[at_bound]))
  if (!isTRUE(allow_bound_priors)) {
    stop("Priors or lambdas on a search bound or still at their start value were not fitted; set allow_bound_priors <- TRUE to save anyway")
  }
}
decay_params$fit_info <- list(sample_n = sample_n, seed = optim_seed, leagues = optim_leagues,
                              xmetrics_source = xm_source, n_stats = length(want_stats),
                              at_bound = want_stats[at_bound], created = Sys.time())

# 5. Save Parameters ----

# Position multipliers for EVERY skill stat (review R015). The optimiser
# covers the get_stat_tiers() stats; the estimators detect more (keeper stats,
# duel WOE), which would otherwise have no positional prior centre. Compute
# multipliers on the enriched Big-5 table for every stat the estimators
# detect. Readers never fill gaps from their own slice.
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
