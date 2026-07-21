# 06c_eval_nextwindow.R
#
# Wave 2 held-out prediction gate (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.2):
# for vintage Y, score a candidate's rating against the NEXT-window
# prior-free RAPM target for Y+1 (rapm_window_targets.rds, SAME source as
# the training target -- both windowed, both prior-free, no xRAPM/panna
# endpoint per sec 2.4.1's evaluation-hygiene rule). >=900 window-minutes in
# Y, Pearson + minutes-weighted Pearson, paired bootstrap (2,000 resamples)
# vs the S0 baseline. Mirrors data-raw/player-ratings-opta/eval_nextseason.R's
# harness shape (pure functions + thin main(), EVAL_NEXTWINDOW_SKIP_MAIN
# guard for fixtures).
#
# CRITICAL leak fix (review finding): the candidate is fit PER EVAL VINTAGE
# via run_candidate_asof(panel, Y, config, ...) (05c_candidates.R), which
# restricts training to vintage_year <= Y before fitting and asserts this
# structurally (assert_asof_panel_window()). The earlier design fit ONE
# model on the whole (pooled, all-vintages) panel and reused it to score
# every eval vintage -- that model had literally TRAINED on vintage Y+1's
# own panel row (label = the Y+1 target being scored against) before ever
# being asked to predict vintage Y, exactly the hindsight-leakage pattern
# eval_nextseason.R's header bans. See run_candidate_asof()'s docstring and
# test-spm-panel.R's leak-guard test (which fits BOTH the old pooled shape
# and the new asof shape on a fixture with a target-identical planted
# feature restricted to vintage Y+1, and shows the pooled fit learns it
# while the asof fit structurally cannot).
#
# S0 BASELINE VINTAGE CAVEAT (read before trusting a number): S0 is
# cache-opta/05_spm.rds's RETROSPECTIVE offense_spm_ratings/defense_spm_ratings
# -- an all-history-vs-all-history fit with no vintage concept at all (it
# doesn't know what year the training data was cut off at). It is used
# here as a SINGLE fixed comparator across every candidate vintage Y, which
# is generous to S0 in early vintages (its "all history" includes data far
# past Y for a career-window candidate fit through Y only) and NOT an
# apples-to-apples as-of comparison. This is the same caveat
# eval_nextseason.R documents for its own xg_xrapm_S baseline; treat S0's
# numbers here as a ceiling-ish reference point, not a strict vintage-honest
# baseline. A true vintage-honest S0 would need an expanding/windowed
# refit of the CURRENT (non-panel) SPM per Y -- out of this Wave's scope.
#
# Usage (from panna/):
#   Rscript data-raw/spm-redesign/06c_eval_nextwindow.R
#
# Config overrides (exists() pattern):
#   cache_dir      default "data-raw/cache-opta"
#   panel_path     default file.path(cache_dir, "spm_panel.rds")
#   eval_vintages  default the panel's vintage_years minus the max (no Y+1
#                  target for the last vintage)
#   min_minutes    default 900 (Y-window minutes filter)
#   n_boot         default 2000

devtools::load_all()
suppressMessages(library(data.table))

# ============================================================================
# Core logic (pure functions)
# ============================================================================

#' Load S0 (current production SPM) as a single fixed comparator
#'
#' @param cache_dir Production cache dir.
#' @return data.table(player_id, pred_net_s0) or NULL if 05_spm.rds absent.
load_s0_baseline <- function(cache_dir) {
  path <- file.path(cache_dir, "05_spm.rds")
  if (!file.exists(path)) {
    cli::cli_warn("S0 baseline not found at {.path {path}} -- bootstrap-vs-S0 comparisons will be skipped.")
    return(NULL)
  }
  s5 <- readRDS(path)
  combined <- data.table::as.data.table(s5$combined_ratings)
  combined[, .(player_id, pred_net_s0 = spm)]
}

#' Build the (player) pairs for one vintage Y: candidate_net, s0_net,
#' minutes_Y, target_next -- inner join, >= min_minutes filter on Y.
#'
#' @param candidate_pred data.table(player_id, pred_net) for vintage Y.
#' @param panel_Y The vintage-Y slice of the panel (for window_minutes).
#' @param s0_pred data.table(player_id, pred_net_s0) or NULL.
#' @param next_target data.table(player_id, rapm) -- vintage Y+1's window
#'   target ratings.
#' @param min_minutes Minimum Y-window minutes to keep a player.
#' @return data.table(player_id, minutes_Y, candidate, s0, target_next).
build_vintage_pairs <- function(candidate_pred, panel_Y, s0_pred, next_target, min_minutes) {
  mins_dt <- unique(panel_Y[, .(player_id, minutes_Y = window_minutes)])
  mins_dt <- mins_dt[minutes_Y >= min_minutes]

  pairs <- merge(mins_dt, candidate_pred[, .(player_id, candidate = pred_net)], by = "player_id")
  pairs <- merge(pairs, next_target[, .(player_id, target_next = rapm)], by = "player_id")
  if (!is.null(s0_pred)) {
    pairs <- merge(pairs, s0_pred, by = "player_id")
    data.table::setnames(pairs, "pred_net_s0", "s0")
  } else {
    pairs[, s0 := NA_real_]
  }
  pairs
}

#' Pearson + minutes-weighted Pearson of candidate/s0 vs target_next.
#'
#' @param pairs data.table from `build_vintage_pairs()` (or an rbind).
#' @param label Character label for the output row.
#' @return data.table(vintage, n, cor_candidate, cor_s0, wcor_candidate,
#'   wcor_s0).
weighted_cor <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  if (length(x) < 3) return(NA_real_)
  wx <- stats::weighted.mean(x, w); wy <- stats::weighted.mean(y, w)
  num <- sum(w * (x - wx) * (y - wy))
  den <- sqrt(sum(w * (x - wx)^2) * sum(w * (y - wy)^2))
  if (!is.finite(den) || den == 0) return(NA_real_)
  num / den
}

pearson_row <- function(pairs, label) {
  n <- nrow(pairs)
  safe_cor <- function(a, b) if (n >= 3 && stats::sd(a, na.rm = TRUE) > 0) stats::cor(a, b, use = "complete.obs") else NA_real_
  data.table::data.table(
    vintage = as.character(label), n = n,
    cor_candidate = safe_cor(pairs$candidate, pairs$target_next),
    cor_s0 = safe_cor(pairs$s0, pairs$target_next),
    wcor_candidate = weighted_cor(pairs$candidate, pairs$target_next, pairs$minutes_Y),
    wcor_s0 = weighted_cor(pairs$s0, pairs$target_next, pairs$minutes_Y)
  )
}

#' Paired bootstrap on cor(candidate, target_next) - cor(s0, target_next),
#' resampling PAIRS with replacement (mirrors eval_nextseason.R).
paired_bootstrap_delta <- function(candidate, s0, target_next, n_boot = 2000, seed = NULL) {
  n <- length(candidate)
  if (n < 3 || any(is.na(s0))) {
    return(list(mean_delta = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_, p_gt0 = NA_real_,
               n_boot = n_boot, n_pairs = n))
  }
  if (!is.null(seed)) set.seed(seed)
  deltas <- vapply(seq_len(n_boot), function(i) {
    idx <- sample.int(n, n, replace = TRUE)
    stats::cor(candidate[idx], target_next[idx]) - stats::cor(s0[idx], target_next[idx])
  }, numeric(1))
  list(mean_delta = mean(deltas), ci_lo = as.numeric(stats::quantile(deltas, 0.025, names = FALSE)),
       ci_hi = as.numeric(stats::quantile(deltas, 0.975, names = FALSE)),
       p_gt0 = mean(deltas > 0), n_boot = n_boot, n_pairs = n)
}

#' Full eval for one candidate CONFIG (not a pre-fitted model) across
#' `eval_vintages` -- fits a fresh, leak-free as-of model PER vintage.
#'
#' @param panel The full panel (all vintages) -- config is NEVER pre-fit on
#'   this directly; each vintage gets its own `run_candidate_asof()` call
#'   restricted to `vintage_year <= Y`.
#' @param config One element of `candidate_configs` (05c_candidates.R).
#' @param rapm_window_targets Loaded `rapm_window_targets.rds`.
#' @param s0_pred `load_s0_baseline()` result (or NULL).
#' @param eval_vintages Integer vector of Y's to evaluate (each needs a
#'   Y+1 target in `rapm_window_targets`).
#' @param min_minutes Y-window minutes filter.
#' @param nfolds,seed CV-fold / bootstrap RNG config, passed through to
#'   `run_candidate_asof()` and the bootstrap.
#' @param n_boot Bootstrap resamples.
#' @return list(per_vintage, pooled, pairs, bootstrap).
eval_candidate_nextwindow <- function(panel, config, rapm_window_targets, s0_pred,
                                      eval_vintages, min_minutes = 900,
                                      nfolds = 5, n_boot = 2000, seed = 1) {
  per_vintage_rows <- list()
  pairs_list <- list()

  for (Y in eval_vintages) {
    next_entry <- rapm_window_targets[[as.character(Y + 1)]]
    if (is.null(next_entry)) {
      cli::cli_warn("eval_candidate_nextwindow: no Y+1 target for vintage {Y} (need {Y + 1}) -- skipping.")
      next
    }
    panel_Y <- panel[vintage_year == Y]
    if (nrow(panel_Y) == 0) next

    # LEAK-FREE: fit ONLY on vintage_year <= Y (structurally asserted by
    # run_candidate_asof() -> assert_asof_panel_window()), never the pooled
    # whole-panel fit. See this file's header for why that distinction is
    # load-bearing, not stylistic.
    fits <- run_candidate_asof(panel, Y, config, nfolds = nfolds, seed = seed)

    candidate_pred <- predict_spm_panel_net(fits, panel_Y)
    pairs <- build_vintage_pairs(candidate_pred, panel_Y, s0_pred,
                                 data.table::as.data.table(next_entry$ratings), min_minutes)
    pairs_list[[as.character(Y)]] <- pairs
    per_vintage_rows[[as.character(Y)]] <- pearson_row(pairs, Y)
  }

  if (length(pairs_list) == 0) {
    cli::cli_abort("eval_candidate_nextwindow: no vintage produced a pairs table (check eval_vintages / target coverage).")
  }

  pooled_pairs <- data.table::rbindlist(pairs_list)
  pooled_row <- pearson_row(pooled_pairs, "pooled")
  boot <- paired_bootstrap_delta(pooled_pairs$candidate, pooled_pairs$s0, pooled_pairs$target_next,
                                 n_boot = n_boot, seed = seed)

  list(per_vintage = data.table::rbindlist(per_vintage_rows), pooled = pooled_row,
       pairs = pooled_pairs, bootstrap = boot)
}

# ============================================================================
# Thin main()
# ============================================================================

main <- function() {
  # Plain exists() (NOT inherits = FALSE) -- this guard runs INSIDE a
  # function, so an inherits = FALSE check only sees main()'s own local
  # frame, never a driver script's globals set before source()ing this file
  # (the exact "R config-flag guards" gotcha: through source(), an
  # inherits = FALSE guard inside a function can't see driver globals ->
  # silently falls back to defaults instead of erroring). The exists()
  # calls at this file's top level (outside main(), e.g.
  # EVAL_NEXTWINDOW_SKIP_MAIN below) are unaffected -- those already run in
  # the global frame directly.
  cache_dir <- if (exists("cache_dir")) cache_dir else file.path("data-raw", "cache-opta")
  panel_path <- if (exists("panel_path")) panel_path else file.path(cache_dir, "spm_panel.rds")
  min_minutes <- if (exists("min_minutes")) min_minutes else 900
  n_boot <- if (exists("n_boot")) n_boot else 2000
  candidate_ids <- if (exists("candidate_ids")) candidate_ids else c("S1")
  nfolds <- if (exists("eval_nfolds")) eval_nfolds else 5
  seed <- if (exists("eval_seed")) eval_seed else 1

  if (!file.exists(panel_path)) {
    cli::cli_abort("Expected {.file {panel_path}} -- run 04c_build_spm_panel.R first.")
  }
  panel_bundle <- readRDS(panel_path)
  panel <- panel_bundle$panel
  # Re-stamp provenance on load (attributes are NOT guaranteed to survive a
  # saveRDS/readRDS round-trip identically for all object types, so this is
  # cheap insurance, not redundant paranoia).
  attr(panel, "target_provenance") <- panel_bundle$target_provenance

  cache_dir_04b <- file.path("data-raw", "cache-opta")
  rapm_window_targets <- readRDS(file.path(cache_dir_04b, "rapm_window_targets.rds"))

  s0_pred <- load_s0_baseline(cache_dir_04b)

  panel_vintages <- sort(unique(panel$vintage_year))
  eval_vintages <- if (exists("eval_vintages")) {
    eval_vintages
  } else {
    panel_vintages[panel_vintages < max(panel_vintages)]
  }

  cli::cli_h1("SMOKE-SCALE NOTICE: numbers from this run mean nothing until the fresh 04_rapm.rds + regenerated window targets land (see task brief).")
  cli::cli_alert_info(sprintf("Panel: %d rows, vintages [%s] | eval vintages: [%s]",
                              nrow(panel), paste(panel_vintages, collapse = ","),
                              paste(eval_vintages, collapse = ",")))

  # NOT local = TRUE: eval_candidate_nextwindow() is a separate top-level
  # function (lexically scoped to this script's global frame, not main()'s
  # execution frame) that calls run_candidate_asof() internally -- sourcing
  # 05c privately into main()'s own frame would make run_candidate_asof()
  # invisible to it (R uses lexical, not dynamic, scoping: a function's free
  # variables resolve against where IT was defined, not against its
  # caller's local frame). Global sourcing also matches 05c's own header
  # ("Runnable individually: source this file").
  source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))

  results <- list()
  for (cid in candidate_ids) {
    cfg <- candidate_configs[[cid]]
    if (is.null(cfg)) cli::cli_abort("Unknown candidate id {.val {cid}}.")
    cli::cli_h1(sprintf("Candidate %s", cid))
    # eval_candidate_nextwindow() fits a fresh, leak-free as-of model PER
    # eval vintage internally (run_candidate_asof()) -- no single pre-fit
    # model is shared across vintages.
    res <- eval_candidate_nextwindow(panel, cfg, rapm_window_targets, s0_pred,
                                     eval_vintages, min_minutes = min_minutes,
                                     nfolds = nfolds, n_boot = n_boot, seed = seed)
    results[[cid]] <- res

    cli::cli_h2(sprintf("%s: per-vintage Pearson (candidate/s0 vs next-window target)", cid))
    print(res$per_vintage)
    cli::cli_h2(sprintf("%s: pooled", cid))
    print(res$pooled)
    cli::cli_alert_info(sprintf(
      "%s pooled paired bootstrap (candidate - s0 vs target_next, n_boot=%d, n_pairs=%d): mean=%.4f, 95%% CI [%.4f, %.4f], P(delta>0)=%.3f",
      cid, res$bootstrap$n_boot, res$bootstrap$n_pairs, res$bootstrap$mean_delta,
      res$bootstrap$ci_lo, res$bootstrap$ci_hi, res$bootstrap$p_gt0))
  }

  invisible(results)
}

if (!exists("EVAL_NEXTWINDOW_SKIP_MAIN", inherits = FALSE)) {
  main()
}
