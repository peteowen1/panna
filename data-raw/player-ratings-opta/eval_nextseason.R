# eval_nextseason.R
# FABLE-PRIOR-FIX-PLAN.md D6: next-season benchmark, materialized.
#
# Recipe source: FABLE-ASOF-EXPERIMENTS.md sec 2.3 (predictive endpoint) and
# sec 5.5 (paired bootstrap) -- this script is that recipe turned into a
# reusable, parameterized tool for the multi-target (EPV/WPA) candidates
# produced by run_multitarget_eval.R, rather than a one-off E7 script.
#
# What it measures, per target (epv, wpa) and per season S: does the
# candidate multi-target rating for S predict PRODUCTION's raw (prior-free)
# within-season seasonal RAPM for S+1 better than production's own xG
# xRAPM_S does? Reference points on the SAME pairs: xG xRAPM_S -> raw_{S+1}
# and raw_S -> raw_{S+1} (E7's own comparison points), plus a 50/50
# z-score ensemble of candidate + xG xRAPM_S (does EPV/WPA add INCREMENTAL
# signal even where it doesn't beat xG alone?).
#
# CANDIDATE SOURCE (settled, corrects an earlier design mistake in this
# script): the PRIMARY candidate source is
# candidate_seasonal_{epv,wpa}.parquet -- run_multitarget_eval.R's Step 5,
# a separate PRIOR-FREE fit_rapm() (no SPM prior) run PER SEASON on
# season-S-only splints. This script previously read the pooled
# 07_seasonal_{epv,wpa}.rds artifact (04_rapm.R's multi-target section,
# fit ONCE across the whole 2022-2026 benchmark window) as the candidate --
# that is HINDSIGHT LEAKAGE against this exact backtest: a pooled fit
# trains on season S+1's own splints before being scored against season
# S+1's raw RAPM. The pooled artifact is kept ONLY as a labeled fallback
# (loud cli_warn) for when the per-season parquet hasn't been built yet --
# NEVER treat a number from that path as a promotion signal.
#
# Usage (from the panna/ package root):
#   Rscript data-raw/player-ratings-opta/eval_nextseason.R
#
# Config overrides (exists() pattern, set before sourcing/running):
#   candidate_cache  default "data-raw/cache-opta-mteval"
#   production_cache default "data-raw/cache-opta"
#   eval_seasons     default c(2023, 2024, 2025)  (S end-years)
#   min_minutes      default 900                  (S-season minutes filter)
#   n_boot           default 2000
#
# The script is factored into pure(ish) functions + a thin main() so a
# fixture can `EVAL_NEXTSEASON_SKIP_MAIN <- TRUE; source(this file)` to get
# the functions without touching disk, and drive them against synthetic
# data.

suppressMessages({
  library(data.table)
})

# ============================================================================
# Core logic (pure functions -- no disk IO, no side effects)
# ============================================================================

#' Load candidate multi-target ratings for one target.
#'
#' Primary source: candidate_seasonal_{target}.parquet (run_multitarget_eval.R
#' Step 5 -- per-season, prior-free fit_rapm(), NOT leakage-contaminated).
#' Fallback: the pooled 07_seasonal_{target}.rds artifact (Step 3/4's
#' plumbing-validation run) -- LEAKAGE-CONTAMINATED as a benchmark input
#' (trains on every season including S+1), used only so this script degrades
#' usefully when the real per-season candidates haven't been built yet. A
#' loud warning fires whenever the fallback is used.
#'
#' @param candidate_cache Isolated eval cache dir (run_multitarget_eval.R's
#'   eval_cache).
#' @param target "epv" or "wpa".
#' @return data.table(player_id, candidate, season_end_year) or NULL if
#'   neither artifact exists (e.g. that target's D5 tripwire fired upstream).
load_candidate_ratings <- function(candidate_cache, target) {
  parquet_path <- file.path(candidate_cache, sprintf("candidate_seasonal_%s.parquet", target))
  if (file.exists(parquet_path)) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli::cli_abort("The {.pkg arrow} package is required to read {.path {parquet_path}}.")
    }
    df <- data.table::as.data.table(arrow::read_parquet(parquet_path))
    required <- c("player_id", "rating", "season_end_year")
    missing <- setdiff(required, names(df))
    if (length(missing) > 0) {
      cli::cli_abort("Candidate parquet at {.path {parquet_path}} missing {.field {missing}} (got {.field {names(df)}}).")
    }
    out <- df[, .(player_id, candidate = rating, season_end_year)]
    return(out)
  }

  pooled_path <- file.path(candidate_cache, sprintf("07_seasonal_%s.rds", target))
  if (!file.exists(pooled_path)) return(NULL)

  cli::cli_warn(c(
    "{toupper(target)}: {.file candidate_seasonal_{target}.parquet} not found -- falling back to the POOLED {.file 07_seasonal_{target}.rds} candidate.",
    "x" = "The pooled fit trains on ALL benchmark seasons including S+1 -- its correlation with raw_{{S+1}} is hindsight-leakage-inflated, not genuine predictive skill.",
    "i" = "Run run_multitarget_eval.R's Step 5 (per-season candidate stage) and re-run this script before treating any number from this fallback as a promotion signal."
  ))
  df <- data.table::as.data.table(readRDS(pooled_path))
  if (!"xrapm" %in% names(df)) {
    cli::cli_abort("Candidate ratings at {.path {pooled_path}} have no {.field xrapm} column (got {.field {names(df)}}).")
  }
  keep <- c("player_id", "xrapm")
  if ("season_end_year" %in% names(df)) keep <- c(keep, "season_end_year")
  out <- df[, ..keep]
  data.table::setnames(out, "xrapm", "candidate")
  out
}

#' Load production seasonal ratings (raw RAPM + xG xRAPM).
#'
#' @param production_cache Production cache dir (default "data-raw/cache-opta").
#' @return list(seasonal_rapm = data.table, seasonal_xrapm = data.table), both
#'   with a season_end_year column.
load_production_seasonal <- function(production_cache) {
  path <- file.path(production_cache, "07_seasonal_ratings.rds")
  if (!file.exists(path)) {
    cli::cli_abort("Production seasonal ratings not found: {.path {path}}")
  }
  s7 <- readRDS(path)
  required <- c("seasonal_rapm", "seasonal_xrapm")
  missing <- setdiff(required, names(s7))
  if (length(missing) > 0) {
    cli::cli_abort("Production {.path {path}} is missing {.field {missing}}.")
  }
  list(
    seasonal_rapm = data.table::as.data.table(s7$seasonal_rapm),
    seasonal_xrapm = data.table::as.data.table(s7$seasonal_xrapm)
  )
}

#' Build the (player_id) pairs for one season S: candidate_S, xg_xrapm_S,
#' raw_S, raw_{S+1} -- an inner join across all four, with the >= min_minutes
#' filter applied to the S-season row BEFORE joining (per FABLE-ASOF-
#' EXPERIMENTS.md sec 2.3's "≥900 S-min" filter, read from the S-season row).
#'
#' @param candidate_df data.table(player_id, candidate) for season S already
#'   (caller subsets by season_end_year first if the candidate has one).
#' @param seasonal_rapm data.table with player_id, total_minutes, rapm,
#'   season_end_year (production, prior-free raw RAPM).
#' @param seasonal_xrapm data.table with player_id, xrapm, season_end_year
#'   (production xG xRAPM).
#' @param S Integer season end-year.
#' @param min_minutes Minimum S-season total_minutes to keep a player.
#' @return data.table(player_id, season, minutes_S, candidate, xg_xrapm_S,
#'   raw_S, raw_next).
build_season_pairs <- function(candidate_df, seasonal_rapm, seasonal_xrapm, S, min_minutes) {
  stopifnot(all(c("player_id", "candidate") %in% names(candidate_df)))
  candidate_df <- data.table::as.data.table(candidate_df)
  cand_dt <- unique(candidate_df[, .(player_id, candidate)])

  s_rapm <- seasonal_rapm[season_end_year == S, .(player_id, minutes_S = total_minutes, raw_S = rapm)]
  s_rapm <- s_rapm[minutes_S >= min_minutes]
  s_xrapm <- seasonal_xrapm[season_end_year == S, .(player_id, xg_xrapm_S = xrapm)]
  next_rapm <- seasonal_rapm[season_end_year == S + 1, .(player_id, raw_next = rapm)]

  pairs <- s_rapm[cand_dt, on = "player_id", nomatch = NULL]
  pairs <- pairs[s_xrapm, on = "player_id", nomatch = NULL]
  pairs <- pairs[next_rapm, on = "player_id", nomatch = NULL]
  pairs[, season := S]
  pairs[, .(player_id, season, minutes_S, candidate, xg_xrapm_S, raw_S, raw_next)]
}

#' z-score a numeric vector (sample sd; 0-vector on zero/degenerate spread).
#'
#' @param x Numeric vector.
#' @return Numeric vector, mean 0 / sd 1 (or all-0 if sd(x) is 0/non-finite).
zscore <- function(x) {
  s <- stats::sd(x)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x)) / s
}

#' 50/50 z-score ensemble of two predictors (puts them on comparable scales
#' before averaging -- candidate and xg_xrapm_S are not in the same units).
#'
#' @param a,b Equal-length numeric vectors.
#' @return Numeric vector, 0.5 * zscore(a) + 0.5 * zscore(b).
ensemble_5050 <- function(a, b) {
  stopifnot(length(a) == length(b))
  0.5 * zscore(a) + 0.5 * zscore(b)
}

#' Pearson correlations of candidate / xg_xrapm_S / raw_S / the 50/50
#' candidate+xg_xrapm ensemble vs raw_next, on a pairs table (per-season or
#' pooled).
#'
#' @param pairs data.table from build_season_pairs() (or an rbind of several).
#' @param season_label Character label for the output row ("2023", "pooled").
#' @return data.table(season, n, cor_candidate, cor_xg_xrapm, cor_raw,
#'   cor_ensemble).
pearson_row <- function(pairs, season_label) {
  n <- nrow(pairs)
  safe_cor <- function(a, b) if (n >= 3) stats::cor(a, b) else NA_real_
  ens <- if (n >= 3) ensemble_5050(pairs$candidate, pairs$xg_xrapm_S) else NA_real_
  data.table::data.table(
    season = as.character(season_label),
    n = n,
    cor_candidate = safe_cor(pairs$candidate, pairs$raw_next),
    cor_xg_xrapm = safe_cor(pairs$xg_xrapm_S, pairs$raw_next),
    cor_raw = safe_cor(pairs$raw_S, pairs$raw_next),
    cor_ensemble = if (n >= 3) safe_cor(ens, pairs$raw_next) else NA_real_
  )
}

#' Paired bootstrap on the correlation delta cor(a, ref) - cor(b, ref),
#' resampling PAIRS with replacement (FABLE-ASOF-EXPERIMENTS.md sec 5.5).
#'
#' @param a,b,ref Equal-length numeric vectors (paired by row).
#' @param n_boot Number of bootstrap resamples.
#' @param seed Optional RNG seed for reproducibility.
#' @return list(mean_delta, ci_lo, ci_hi, p_gt0, n_boot, n_pairs).
paired_bootstrap_delta <- function(a, b, ref, n_boot = 2000, seed = NULL) {
  stopifnot(length(a) == length(b), length(a) == length(ref))
  n <- length(a)
  if (n < 3) {
    return(list(mean_delta = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
               p_gt0 = NA_real_, n_boot = n_boot, n_pairs = n))
  }
  if (!is.null(seed)) set.seed(seed)
  deltas <- vapply(seq_len(n_boot), function(i) {
    idx <- sample.int(n, n, replace = TRUE)
    stats::cor(a[idx], ref[idx]) - stats::cor(b[idx], ref[idx])
  }, numeric(1))
  list(
    mean_delta = mean(deltas),
    ci_lo = as.numeric(stats::quantile(deltas, 0.025, names = FALSE)),
    ci_hi = as.numeric(stats::quantile(deltas, 0.975, names = FALSE)),
    p_gt0 = mean(deltas > 0),
    n_boot = n_boot,
    n_pairs = n
  )
}

#' Run the full per-season + pooled + bootstrap benchmark for one target.
#'
#' Produces TWO paired bootstraps on the pooled pairs: the primary D6
#' promotion metric (candidate vs xg_xrapm) and a secondary incremental-
#' signal check (the 50/50 ensemble vs xg_xrapm alone -- does blending in
#' EPV/WPA help even where the candidate alone doesn't beat xG?). The
#' ensemble series is z-scored ONCE on the full pooled sample, then resampled
#' by index like every other series -- a standard simplification (the
#' bootstrap re-estimates the correlation, not the z-score normalization
#' constants); noted here rather than presented as more rigorous than it is.
#'
#' @param target Character label ("epv"/"wpa"), used only for messaging.
#' @param candidate_df data.table(player_id, candidate[, season_end_year]).
#' @param seasonal_rapm,seasonal_xrapm Production data.tables (see
#'   load_production_seasonal()).
#' @param eval_seasons Integer vector of S end-years to evaluate.
#' @param min_minutes Minimum S-season minutes filter.
#' @param n_boot Bootstrap resamples for the pooled deltas.
#' @param seed Bootstrap RNG seed.
#' @return list(target, per_season, pooled, pairs, bootstrap, bootstrap_ensemble).
run_target_benchmark <- function(target, candidate_df, seasonal_rapm, seasonal_xrapm,
                                  eval_seasons, min_minutes, n_boot = 2000, seed = 1) {
  candidate_df <- data.table::as.data.table(candidate_df)
  has_season_col <- "season_end_year" %in% names(candidate_df)

  per_season_rows <- vector("list", length(eval_seasons))
  pairs_list <- vector("list", length(eval_seasons))
  names(per_season_rows) <- as.character(eval_seasons)
  names(pairs_list) <- as.character(eval_seasons)

  for (S in eval_seasons) {
    cand_S <- if (has_season_col) candidate_df[season_end_year == S] else candidate_df
    pairs <- build_season_pairs(cand_S, seasonal_rapm, seasonal_xrapm, S, min_minutes)
    pairs_list[[as.character(S)]] <- pairs
    per_season_rows[[as.character(S)]] <- pearson_row(pairs, S)
  }

  pooled_pairs <- data.table::rbindlist(pairs_list)
  pooled_row <- pearson_row(pooled_pairs, "pooled")

  boot <- paired_bootstrap_delta(pooled_pairs$candidate, pooled_pairs$xg_xrapm_S,
                                 pooled_pairs$raw_next, n_boot = n_boot, seed = seed)

  ensemble_pooled <- if (nrow(pooled_pairs) >= 3) {
    ensemble_5050(pooled_pairs$candidate, pooled_pairs$xg_xrapm_S)
  } else {
    rep(NA_real_, nrow(pooled_pairs))
  }
  boot_ensemble <- paired_bootstrap_delta(ensemble_pooled, pooled_pairs$xg_xrapm_S,
                                          pooled_pairs$raw_next, n_boot = n_boot, seed = seed)

  list(
    target = target,
    per_season = data.table::rbindlist(per_season_rows),
    pooled = pooled_row,
    pairs = pooled_pairs,
    bootstrap = boot,
    bootstrap_ensemble = boot_ensemble
  )
}

# ============================================================================
# Thin main() -- disk IO + printing + CSV/parquet output. Guarded so a
# fixture can source this file for just the functions above without running
# any of it.
# ============================================================================

main <- function() {
  candidate_cache <- if (exists("candidate_cache", inherits = FALSE)) candidate_cache else file.path("data-raw", "cache-opta-mteval")
  production_cache <- if (exists("production_cache", inherits = FALSE)) production_cache else file.path("data-raw", "cache-opta")
  eval_seasons <- if (exists("eval_seasons", inherits = FALSE)) eval_seasons else c(2023, 2024, 2025)
  min_minutes <- if (exists("min_minutes", inherits = FALSE)) min_minutes else 900
  n_boot <- if (exists("n_boot", inherits = FALSE)) n_boot else 2000

  cli::cli_h1("FABLE-PRIOR-FIX-PLAN.md D6: next-season benchmark")
  cli::cli_alert_info(sprintf("Candidate cache: %s", candidate_cache))
  cli::cli_alert_info(sprintf("Production cache: %s", production_cache))
  cli::cli_alert_info(sprintf("Eval seasons (S): %s (predicting raw RAPM of S+1)",
                              paste(eval_seasons, collapse = ", ")))
  cli::cli_alert_info(sprintf("Min S-season minutes: %d | Bootstrap resamples: %d", min_minutes, n_boot))

  prod <- load_production_seasonal(production_cache)

  targets <- c("epv", "wpa")
  per_season_list <- list()
  pooled_list <- list()
  boot_list <- list()

  for (tgt in targets) {
    cand <- load_candidate_ratings(candidate_cache, tgt)
    if (is.null(cand)) {
      cli::cli_alert_warning(sprintf(
        "%s: no candidate artifact (checked candidate_seasonal_%s.parquet and 07_seasonal_%s.rds under %s) -- skipping",
        toupper(tgt), tgt, tgt, candidate_cache))
      next
    }

    res <- run_target_benchmark(tgt, cand, prod$seasonal_rapm, prod$seasonal_xrapm,
                                eval_seasons, min_minutes, n_boot = n_boot, seed = 1)

    per_season_list[[tgt]] <- data.table::copy(res$per_season)[, target := tgt]
    pooled_list[[tgt]] <- data.table::copy(res$pooled)[, target := tgt]
    boot_list[[tgt]] <- data.table::data.table(
      target = rep(tgt, 2),
      comparison = c("candidate_vs_xg_xrapm", "ensemble_vs_xg_xrapm"),
      mean_delta = c(res$bootstrap$mean_delta, res$bootstrap_ensemble$mean_delta),
      ci_lo = c(res$bootstrap$ci_lo, res$bootstrap_ensemble$ci_lo),
      ci_hi = c(res$bootstrap$ci_hi, res$bootstrap_ensemble$ci_hi),
      p_gt0 = c(res$bootstrap$p_gt0, res$bootstrap_ensemble$p_gt0),
      n_boot = c(res$bootstrap$n_boot, res$bootstrap_ensemble$n_boot),
      n_pairs = c(res$bootstrap$n_pairs, res$bootstrap_ensemble$n_pairs)
    )

    cli::cli_h2(sprintf("%s: per-season Pearson (candidate/xg_xrapm/raw/ensemble vs raw_next)", toupper(tgt)))
    print(res$per_season)
    cli::cli_h2(sprintf("%s: pooled", toupper(tgt)))
    print(res$pooled)
    cli::cli_alert_info(sprintf(
      "%s pooled paired bootstrap (candidate - xg_xrapm vs raw_next, n_boot=%d, n_pairs=%d): mean=%.4f, 95%% CI [%.4f, %.4f], P(delta>0)=%.3f",
      toupper(tgt), res$bootstrap$n_boot, res$bootstrap$n_pairs, res$bootstrap$mean_delta,
      res$bootstrap$ci_lo, res$bootstrap$ci_hi, res$bootstrap$p_gt0))
    cli::cli_alert_info(sprintf(
      "%s incremental-signal check (50/50 ensemble - xg_xrapm vs raw_next): mean=%.4f, 95%% CI [%.4f, %.4f], P(delta>0)=%.3f -> %s",
      toupper(tgt), res$bootstrap_ensemble$mean_delta, res$bootstrap_ensemble$ci_lo,
      res$bootstrap_ensemble$ci_hi, res$bootstrap_ensemble$p_gt0,
      if (!is.na(res$bootstrap_ensemble$ci_lo) && res$bootstrap_ensemble$ci_lo > 0) {
        "ensemble beats xG ALONE (positive CI) -- EPV/WPA adds incremental signal"
      } else if (!is.na(res$pooled$cor_ensemble) && res$pooled$cor_ensemble > res$pooled$cor_xg_xrapm) {
        "ensemble point estimate > xG alone but CI touches 0 -- suggestive, not conclusive"
      } else {
        "no incremental signal detected"
      }))
  }

  if (length(per_season_list) == 0) {
    cli::cli_abort("No candidate targets available in {.path {candidate_cache}} -- run run_multitarget_eval.R first.")
  }

  per_season_dt <- data.table::rbindlist(per_season_list)
  pooled_dt <- data.table::rbindlist(pooled_list)
  boot_dt <- data.table::rbindlist(boot_list)

  cli::cli_h2("Reference only: E7 (FABLE-ASOF-EXPERIMENTS.md sec 2.3), a DIFFERENT (full 13-season, all-league, >=900 S-min, n=13,200) universe")
  cli::cli_alert_info("xG xRAPM 0.400 > prior-only 0.384 > raw RAPM 0.361 -- NOT directly comparable to the 7-league/5-season numbers above; reported for context only, per D6.")

  results_path_csv <- file.path(candidate_cache, "multitarget_eval_nextseason.csv")
  results_path_parquet <- file.path(candidate_cache, "multitarget_eval_nextseason.parquet")
  combined <- data.table::rbindlist(list(
    cbind(scope = "per_season", per_season_dt),
    cbind(scope = "pooled", pooled_dt)
  ), use.names = TRUE)
  data.table::setcolorder(combined, c("target", "scope", "season", "n", "cor_candidate", "cor_xg_xrapm", "cor_raw", "cor_ensemble"))
  utils::write.csv(combined, results_path_csv, row.names = FALSE)
  cli::cli_alert_success(sprintf("Saved %s", results_path_csv))
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(combined, results_path_parquet)
    cli::cli_alert_success(sprintf("Saved %s", results_path_parquet))
  }

  boot_path_csv <- file.path(candidate_cache, "multitarget_eval_bootstrap.csv")
  boot_path_parquet <- file.path(candidate_cache, "multitarget_eval_bootstrap.parquet")
  utils::write.csv(boot_dt, boot_path_csv, row.names = FALSE)
  cli::cli_alert_success(sprintf("Saved %s", boot_path_csv))
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(boot_dt, boot_path_parquet)
    cli::cli_alert_success(sprintf("Saved %s", boot_path_parquet))
  }

  cli::cli_h1("D6 promotion criterion")
  cli::cli_alert_info("Promotion = positive pooled bootstrap CI for (candidate - xg_xrapm) -- see FABLE-PRIOR-FIX-PLAN.md D6. The ensemble comparison is a secondary, informative-only signal (incremental value even absent outright promotion). Write the promote/park decision to a short FABLE-MULTITARGET-DECISION.md memo per the plan; this script only produces the numbers.")

  invisible(list(per_season = per_season_dt, pooled = pooled_dt, bootstrap = boot_dt))
}

if (!exists("EVAL_NEXTSEASON_SKIP_MAIN", inherits = FALSE)) {
  main()
}
