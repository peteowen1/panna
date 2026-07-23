# 05c_candidates.R
#
# Wave 2 candidate configs S1-S5 (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.1),
# each a config list feeding fit_spm_panel() twice (offense, defense) via
# run_candidate() below. Runnable individually: source this file, then e.g.
#   fit_S3 <- run_candidate(panel, candidate_configs$S3, seed = 1)
#
# For the LEAK-FREE next-window eval gate (06c_eval_nextwindow.R), use
# run_candidate_asof(panel, Y, config, ...) instead of run_candidate()
# directly -- it restricts training to vintage_year <= Y before fitting,
# which run_candidate() alone does NOT do (see run_candidate_asof()'s
# docstring for why a pooled all-vintages fit leaks the eval target).
#
# S0 (current production SPM) is NOT built here -- it already exists as
# cache-opta/05_spm.rds's retrospective offense_spm_ratings/defense_spm_ratings
# (see 06c_eval_nextwindow.R's baseline loader and the vintage caveat noted
# there).
#
# Config axes (sec 3.1/3.2):
#   S1  panel + windowed target, GLOBAL coefficients only, sqrt weights
#   S2  S1 + role-group partial pooling (sec 3.1's 6-group deviation columns)
#   S3  S2 + extended sign constraints (sec 3.1's new offense list + the
#       05_spm.R-verbatim defense lists)
#   S4a S3 + LINEAR minutes weights (instead of sqrt)
#   S4b S3 + EIV target rescale (sec 2.3.2, m0 = 8000 per the Wave-1 study)
#   S5  S3 + chain features, IF present in the panel source -- see note below
#
# Run from panna/ (relative cache paths assume cwd = panna/).

devtools::load_all()

candidate_configs <- list(
  S1  = list(role_pooling = FALSE, sign_constraints = FALSE,
             weight_transform = "sqrt", eiv_rescale = FALSE,
             alpha = 0.5, deviation_penalty_mult = 5),
  S2  = list(role_pooling = TRUE, sign_constraints = FALSE,
             weight_transform = "sqrt", eiv_rescale = FALSE,
             alpha = 0.5, deviation_penalty_mult = 5),
  S3  = list(role_pooling = TRUE, sign_constraints = TRUE,
             weight_transform = "sqrt", eiv_rescale = FALSE,
             alpha = 0.5, deviation_penalty_mult = 5),
  S4a = list(role_pooling = TRUE, sign_constraints = TRUE,
             weight_transform = "linear", eiv_rescale = FALSE,
             alpha = 0.5, deviation_penalty_mult = 5),
  S4b = list(role_pooling = TRUE, sign_constraints = TRUE,
             weight_transform = "sqrt", eiv_rescale = TRUE,
             eiv_m0 = 8000, eiv_floor = 0.4,
             alpha = 0.5, deviation_penalty_mult = 5),
  S5  = list(role_pooling = TRUE, sign_constraints = TRUE,
             weight_transform = "sqrt", eiv_rescale = FALSE,
             alpha = 0.5, deviation_penalty_mult = 5,
             chain_cols = c("chains_p90", "chain_shot_pct", "chain_goal_pct",
                            "chain_starts_p90", "chain_xg_p90", "avg_actions_per_chain"))
)

#' Fit one candidate config's offense + defense models
#'
#' @param panel Output of `build_spm_panel()` / `04c_build_spm_panel.R`'s
#'   cached `spm_panel.rds$panel`.
#' @param config One element of `candidate_configs` above.
#' @param nfolds CV folds (default 5; use a smaller value at smoke scale --
#'   must be <= the number of distinct players in `panel`).
#' @param seed Grouped-fold RNG seed (recommend a fixed value so candidates
#'   are compared on the SAME fold assignment).
#' @param predictor_cols Passed through to `fit_spm_panel()` (default `NULL`
#'   = canonical `.spm_opta_predictor_cols()` selector).
#' @return List(config = config, offense = <fit_spm_panel result>,
#'   defense = <fit_spm_panel result>).
run_candidate <- function(panel, config, nfolds = 5, seed = 1, predictor_cols = NULL) {
  # S5's chain features: 01_match_stats.rds (this panel's source, per the
  # task brief) carries no chain columns -- they only exist in the
  # career-level opta_xmetrics aggregation 05_spm.R reads separately, and
  # enrich_match_stats_with_xmetrics()'s xm_map doesn't carry them either.
  # Documented degrade (sec 5.1 "if present in the panel source"): S5 is a
  # no-op identical to S3 until a match-grain chain source exists. Loud,
  # not silent.
  chain_cols <- config$chain_cols
  if (!is.null(chain_cols)) {
    present <- intersect(chain_cols, names(panel))
    if (length(present) == 0) {
      cli::cli_warn("run_candidate: S5 chain columns not present in this panel source (Wave 2 match-grain build has none) -- falling back to the S3 config (no chain features).")
    } else {
      cli::cli_inform(sprintf("run_candidate: %d chain column(s) present -- including in predictor_cols.", length(present)))
      if (is.null(predictor_cols)) predictor_cols <- .spm_opta_predictor_cols(panel)
      predictor_cols <- union(predictor_cols, present)
    }
  }

  common_args <- list(
    panel = panel, role_pooling = config$role_pooling,
    deviation_penalty_mult = config$deviation_penalty_mult %||% 5,
    alpha = config$alpha %||% 0.5, weight_transform = config$weight_transform,
    eiv_rescale = isTRUE(config$eiv_rescale),
    eiv_m0 = config$eiv_m0 %||% 8000, eiv_floor = config$eiv_floor %||% 0.4,
    sign_constraints = config$sign_constraints, predictor_cols = predictor_cols,
    nfolds = nfolds, seed = seed
  )

  cli::cli_h2("Offense")
  offense_fit <- do.call(fit_spm_panel, c(common_args, list(target = "offense")))
  cli::cli_h2("Defense")
  defense_fit <- do.call(fit_spm_panel, c(common_args, list(target = "defense")))

  list(config = config, offense = offense_fit, defense = defense_fit)
}

#' Fit one candidate config AS OF eval vintage `Y` -- the leak-free fitting
#' unit for `06c_eval_nextwindow.R`
#'
#' CRITICAL leak fix (found in review): scoring vintage `Y`'s rows against
#' the `Y+1` window-RAPM target is only a valid held-out test if the
#' candidate was fit WITHOUT ever seeing a `vintage_year > Y` panel row --
#' otherwise the fit trained directly on vintage `Y+1`'s row (whose label
#' literally IS the `Y+1` target), and vintage `Y`'s window overlaps `Y+1`'s
#' in 4 of 5 seasons, so a pooled (all-vintages) fit partially encodes the
#' "held-out" answer. This is the exact hindsight-leakage pattern
#' `eval_nextseason.R`'s header bans for its own pooled-vs-per-season
#' candidate choice, and BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.2 states the
#' rule directly: "every candidate fit for vintage Y uses features and
#' targets strictly from seasons < Y" (here: vintages <= Y, since a panel
#' row's OWN vintage_year is its window's cutoff).
#'
#' Subsets `panel` to `vintage_year <= Y` (an expanding-window fit, one per
#' eval vintage -- ~7 refits per candidate across a full 2019:2026 panel,
#' cheap at this scale per the review), asserts the subset structurally via
#' `assert_asof_panel_window()`, then delegates to `run_candidate()`.
#'
#' @param panel The FULL panel (all vintages) -- this function does its own
#'   `vintage_year <= Y` subsetting; do not pre-filter.
#' @param Y Eval vintage being scored (features/target restricted to
#'   `vintage_year <= Y`).
#' @param config One element of `candidate_configs`.
#' @param nfolds,seed,predictor_cols Passed through to `run_candidate()`.
#' @return Same shape as `run_candidate()`: `list(config, offense, defense)`.
run_candidate_asof <- function(panel, Y, config, nfolds = 5, seed = 1, predictor_cols = NULL) {
  train_panel <- panel[panel$vintage_year <= Y, ]
  assert_asof_panel_window(train_panel, Y)
  run_candidate(train_panel, config, nfolds = nfolds, seed = seed, predictor_cols = predictor_cols)
}
