# Expanding-window SPM weights for as-of (point-in-time) consumers.
#
# See FABLE-ASOF-EXPERIMENTS.md sec 4 for the design and rationale: hindsight
# in the pooled-RAPM SPM weights (season/date D's weights are normally fit on
# RAPM through the PRESENT, including everything at-or-after D) inflates
# retrospective backtests by a small but real amount (+0.003 Pearson,
# measured), and specifically threatens consumers that claim point-in-time
# semantics -- the career_panna_asof chain (H3) -- more than the retrospective
# seasonal ratings (which the doc's LOSO experiment found only need
# relabeling, sec 3). This file builds the expanding-window machinery: one SPM
# weight set per reference year Y, trained ONLY on seasons < Y, mirroring the
# doc's sec 5.2 reproduction scripts (Step A/B) generalized from "exclude
# season S" (LOSO, a measurement device) to "keep seasons < cutoff_year"
# (expanding window, the deployment shape sec 4 recommends).
#
# Orchestration (the resumable per-year loop + caching to
# cache-skills/03_skill_spm_asof.rds) lives in
# data-raw/estimated-skills/03_skill_spm.R; data-raw/estimated-skills/
# 09b_career_panna_asof.R selects the right cutoff-year model per snapshot
# date. These two functions are the reusable, testable fitting logic.


#' Row-subset a prepared pooled RAPM design to seasons strictly before a
#' cutoff year (optionally also at-or-after a minimum year), dropping
#' resulting all-zero player columns
#'
#' Mirrors FABLE-ASOF-EXPERIMENTS.md sec 5.2 Step A, generalized from
#' "exclude season S" (LOSO) to "keep seasons < cutoff_year" (expanding
#' window). BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.1 further generalizes this
#' to a bounded window (`min_year <= season_end_year < cutoff_year`) for the
#' windowed prior-free RAPM target -- `min_year = NULL` (default) preserves
#' the original expanding-window behaviour unchanged. Season-only players and
#' season-only league-season dummy columns become all-zero once their rows
#' are dropped; both are removed here (kept off/def-symmetric per player) so
#' the resulting design has no dead columns.
#'
#' @param rapm_data The `rapm_data` list as produced by `prepare_rapm_data()`
#'   / saved in `04_rapm.rds$rapm_data` (needs `X_full`, `y`, `weights`,
#'   `player_ids`, `covariate_names`, `player_mapping`, `row_data$splint_id`).
#' @param splint_season_map data.frame/data.table with `splint_id`,
#'   `season_end_year` (e.g. `03_splints.rds$splints[, c("splint_id",
#'   "season_end_year")]`).
#' @param cutoff_year Integer; rows from seasons `< cutoff_year` are kept.
#' @param min_year Integer or `NULL` (default). When supplied, rows from
#'   seasons `< min_year` are additionally dropped, bounding the window to
#'   `min_year <= season_end_year < cutoff_year`. `NULL` keeps the original
#'   "seasons < cutoff_year" (expanding-window) behaviour.
#' @return A `rapm_data`-shaped list (row- and column-subset), suitable for
#'   `fit_rapm()`.
#' @keywords internal
.subset_rapm_data_expanding <- function(rapm_data, splint_season_map, cutoff_year,
                                        min_year = NULL) {
  ssm <- data.table::as.data.table(splint_season_map)
  row_season <- ssm$season_end_year[match(rapm_data$row_data$splint_id, ssm$splint_id)]
  keep_rows <- !is.na(row_season) & row_season < cutoff_year
  if (!is.null(min_year)) {
    keep_rows <- keep_rows & row_season >= min_year
  }

  X <- rapm_data$X_full[keep_rows, , drop = FALSE]
  X <- methods::as(X, "CsparseMatrix")
  y <- rapm_data$y[keep_rows]
  w <- rapm_data$weights[keep_rows]
  cn <- colnames(X)

  # Drop all-zero columns (season-only players; season-only league-season
  # dummies), keeping off/def symmetric per player.
  nnz <- diff(X@p)
  keep_cols <- nnz > 0

  pids <- rapm_data$player_ids
  off_idx <- match(paste0(pids, "_off"), cn)
  def_idx <- match(paste0(pids, "_def"), cn)
  pkeep <- keep_cols[off_idx] | keep_cols[def_idx]
  keep_cols[off_idx] <- pkeep
  keep_cols[def_idx] <- pkeep

  new_cov <- rapm_data$covariate_names[keep_cols[match(rapm_data$covariate_names, cn)]]
  X <- X[, keep_cols, drop = FALSE]
  new_pids <- pids[pkeep]

  list(
    X_full = X,
    y = y,
    weights = w,
    player_mapping = rapm_data$player_mapping[rapm_data$player_mapping$player_id %in% new_pids, ],
    player_ids = new_pids,
    n_players = length(new_pids) - 1L,
    n_players_total = length(new_pids),
    covariate_names = new_cov,
    target_type = rapm_data$target_type %||% "xg"
  )
}


#' Fit a pooled RAPM restricted to seasons strictly before a cutoff year
#'
#' Mirrors the production step-04 pooled RAPM fit (panna#87 bracketed
#' mini-CV lambda grid), but on an expanding-window ROW SUBSET of the design
#' matrix instead of the full history -- the as-of deployment shape
#' recommended by FABLE-ASOF-EXPERIMENTS.md sec 4.
#'
#' @param rapm_data The pooled `rapm_data` list from `04_rapm.rds$rapm_data`.
#' @param splint_season_map data.frame/data.table with `splint_id`,
#'   `season_end_year`.
#' @param cutoff_year Integer; only seasons `< cutoff_year` are used to train.
#' @param min_year Integer or `NULL` (default). When supplied, bounds the
#'   training window to `min_year <= season_end_year < cutoff_year` instead
#'   of the full expanding history (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.1's
#'   windowed prior-free RAPM target, e.g. a 5-season window `min_year =
#'   cutoff_year - 5`). Passed straight through to
#'   `.subset_rapm_data_expanding()`.
#' @param lambda_formula `function(n_obs)` giving the mini-CV grid center
#'   (default the panna#87 sample-size formula, `16.67 * n_obs^-0.58`).
#' @param nfolds CV folds (default 5, matching the production pooled fit).
#' @param seed RNG seed for `fit_rapm()`'s CV fold assignment, for
#'   reproducible per-cutoff-year fits. `NULL` = no explicit seed.
#'
#' @return List: `ratings` (data.frame player_id/rapm/offense/defense),
#'   `lambda_min`, `n_obs`, `cutoff_year`, `min_year` (the argument as
#'   supplied, `NULL` for the unbounded expanding-window default). `NULL`
#'   (with a warning) if fewer than 1000 valid observations remain (too few
#'   prior seasons, or an empty/too-narrow window).
#' @keywords internal
fit_expanding_pooled_rapm <- function(rapm_data, splint_season_map, cutoff_year,
                                      min_year = NULL,
                                      lambda_formula = function(n) 16.67 * n^(-0.58),
                                      nfolds = 5, seed = NULL) {
  rapm_sub <- .subset_rapm_data_expanding(rapm_data, splint_season_map, cutoff_year,
                                          min_year = min_year)
  n_obs <- sum(!is.na(rapm_sub$y) & is.finite(rapm_sub$y))
  if (n_obs < 1000) {
    cli::cli_warn(paste0(
      "fit_expanding_pooled_rapm: only {n_obs} valid observations for ",
      "cutoff_year={cutoff_year} (too few prior seasons) -- skipping."))
    return(NULL)
  }

  lambda_grid <- lambda_formula(n_obs) * 2^seq(3, -3, by = -0.5)
  if (!is.null(seed)) set.seed(seed)
  model <- fit_rapm(rapm_sub, alpha = 0, nfolds = nfolds, use_weights = TRUE,
                    penalize_covariates = FALSE, parallel = FALSE,
                    lambda_seq = lambda_grid)
  ratings <- extract_rapm_ratings(model, lambda = "min")

  list(
    ratings = ratings[, c("player_id", "rapm", "offense", "defense")],
    lambda_min = model$lambda.min,
    n_obs = n_obs,
    cutoff_year = cutoff_year,
    min_year = min_year
  )
}


#' Fit expanding-window skill-SPM O/D models for one reference year
#'
#' Step B of the as-of fix: skill features are filtered to seasons strictly
#' before `cutoff_year` (an honest, point-in-time feature set), the
#' most-recent-per-player slice is taken (mirrors
#' `data-raw/estimated-skills/03_skill_spm.R`), and offense/defense
#' elastic-net + XGBoost SPM models are fit against the matching
#' `fit_expanding_pooled_rapm()` target -- so NEITHER the features NOR the
#' RAPM target the SPM predicts can see season `cutoff_year` or later.
#' Column sets come from the shared `.skill_spm_offense_cols()` /
#' `.skill_spm_defense_cols()` / `.skill_spm_defense_constraints()` (the same
#' definitions the all-history fit in `03_skill_spm.R` section 10 uses, so
#' the two can never drift apart).
#'
#' @param skill_features `02_skill_features.rds` (one row per player-season,
#'   needs `player_id`, `season_end_year`, `total_minutes`).
#' @param pooled_rapm_ratings Output of `fit_expanding_pooled_rapm()$ratings`
#'   for the SAME `cutoff_year` (needs `player_id`, `offense`, `defense`).
#' @param cutoff_year Integer; skill features from seasons `< cutoff_year` only.
#' @param nfolds CV folds for both glmnet and xgboost (default 5).
#'
#' @return List with `offense_spm_glmnet`, `offense_spm_xgb`,
#'   `defense_spm_glmnet`, `defense_spm_xgb`, `offense_spm_ratings`,
#'   `defense_spm_ratings` (same shape as `03_skill_spm.rds`), `cutoff_year`,
#'   `n_train`. `NULL` (with a warning) if fewer than 100 players are
#'   available to train on (e.g. the earliest season, no prior data).
#' @keywords internal
fit_expanding_skill_spm <- function(skill_features, pooled_rapm_ratings, cutoff_year,
                                    nfolds = 5) {
  player_stats <- skill_features %>%
    dplyr::filter(season_end_year < cutoff_year) %>%
    dplyr::group_by(player_id) %>%
    dplyr::slice_max(season_end_year, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  if (!"player_name" %in% names(player_stats)) player_stats$player_name <- player_stats$player_id
  if (!"mins_per_90" %in% names(player_stats)) {
    player_stats$mins_per_90 <- player_stats$total_minutes / 90
  }

  spm_train_data <- player_stats %>%
    dplyr::inner_join(
      pooled_rapm_ratings %>% dplyr::select(player_id, rapm, offense, defense),
      by = "player_id"
    )

  if (nrow(spm_train_data) < 100) {
    cli::cli_warn(paste0(
      "fit_expanding_skill_spm: only {nrow(spm_train_data)} training players ",
      "for cutoff_year={cutoff_year} -- skipping."))
    return(NULL)
  }

  offense_cols <- .skill_spm_offense_cols(spm_train_data)
  offense_train <- spm_train_data %>% dplyr::mutate(rapm = offense)
  offense_spm_glmnet <- fit_spm_model(offense_train, predictor_cols = offense_cols,
                                      alpha = 0.5, nfolds = nfolds, weight_by_minutes = TRUE)
  offense_spm_xgb <- fit_spm_xgb(offense_train, predictor_cols = offense_cols,
                                 nfolds = nfolds, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

  defense_cols <- .skill_spm_defense_cols(spm_train_data)
  constraints <- .skill_spm_defense_constraints()
  def_lower <- stats::setNames(rep(0, length(constraints$bad)), constraints$bad)
  def_upper <- stats::setNames(rep(0, length(constraints$good)), constraints$good)
  defense_train <- spm_train_data %>% dplyr::mutate(rapm = defense)
  defense_spm_glmnet <- fit_spm_model(defense_train, predictor_cols = defense_cols,
                                      alpha = 0.5, nfolds = nfolds, weight_by_minutes = TRUE,
                                      lower_limits = def_lower, upper_limits = def_upper)
  defense_spm_xgb <- fit_spm_xgb(defense_train, predictor_cols = defense_cols,
                                 nfolds = nfolds, max_depth = 4, eta = 0.02,
                                 nrounds = 1000, early_stopping_rounds = 20,
                                 weight_by_minutes = TRUE, weight_transform = "sqrt",
                                 verbose = 0)

  offense_glmnet_pred <- calculate_spm_ratings(player_stats, offense_spm_glmnet)
  offense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, offense_spm_xgb)
  offense_spm_ratings <- offense_glmnet_pred %>%
    dplyr::rename(off_glmnet = spm) %>%
    dplyr::inner_join(offense_xgb_pred %>% dplyr::select(player_id, off_xgb = spm), by = "player_id") %>%
    dplyr::mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

  defense_glmnet_pred <- calculate_spm_ratings(player_stats, defense_spm_glmnet)
  defense_xgb_pred <- calculate_spm_ratings_xgb(player_stats, defense_spm_xgb)
  defense_spm_ratings <- defense_glmnet_pred %>%
    dplyr::rename(def_glmnet = spm) %>%
    dplyr::inner_join(defense_xgb_pred %>% dplyr::select(player_id, def_xgb = spm), by = "player_id") %>%
    dplyr::mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

  list(
    offense_spm_glmnet = offense_spm_glmnet,
    offense_spm_xgb = offense_spm_xgb,
    defense_spm_glmnet = defense_spm_glmnet,
    defense_spm_xgb = defense_spm_xgb,
    offense_spm_ratings = offense_spm_ratings,
    defense_spm_ratings = defense_spm_ratings,
    cutoff_year = cutoff_year,
    n_train = nrow(spm_train_data)
  )
}


#' Season end year for a reference/match date
#'
#' Composes the two existing season helpers (`extract_season_from_date()` +
#' `extract_season_end_year()`) so as-of-date consumers can map a Date
#' straight to the `season_end_year` grain the expanding-window SPM models
#' (above) are keyed by, instead of re-deriving the Aug-July season boundary.
#'
#' @param date Date (or coercible)
#' @return Integer season end year (e.g. 2026 for a date in season "2025-2026")
#' @keywords internal
.season_end_year_for_date <- function(date) {
  extract_season_end_year(extract_season_from_date(date))
}


#' Abort unless a target artifact is provenance-stamped prior-free RAPM
#'
#' Static circularity guard (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.4.1):
#' `fit_rapm_with_prior()` (`R/rapm_model.R:389`) shrinks toward an SPM
#' prior, so xRAPM and career panna embed box-stat information -- regressing
#' box features onto them (directly or via any downstream SPM panel target)
#' would close the SPM -> prior -> posterior -> "prior-free" target loop.
#' Any box-score value training entry point (e.g. the planned
#' `fit_spm_panel()`) must call this on its target argument before fitting.
#'
#' Two accepted shapes: (1) a `04b_rapm_window_targets.R` vintage element (or
#' the top-level list), stamped `target_provenance = "prior_free_rapm_window"`
#' by that script and nowhere else; (2) a raw `fit_rapm()` model object
#' (legacy path) whose `panna_metadata$type == "rapm"` with no `used_prior`
#' field -- `fit_rapm_with_prior()` always sets `used_prior = TRUE`,
#' `fit_rapm()` never sets it, so its absence is the discriminator. Anything
#' else (including `type == "xrapm"`/`"xrapm_net"`, or no provenance at all)
#' aborts.
#'
#' @param target The candidate target artifact -- a `04b` vintage list, the
#'   top-level `04b` list, or a raw `fit_rapm()`/`fit_rapm_with_prior()`
#'   model object.
#' @return Invisibly `TRUE` if the target is accepted.
#' @family rapm
#' @export
assert_prior_free_target <- function(target) {
  provenance <- target$target_provenance %||% attr(target, "target_provenance")
  provenance_ok <- isTRUE(provenance == "prior_free_rapm_window")

  meta <- target$panna_metadata
  legacy_ok <- !is.null(meta) && identical(meta$type, "rapm") && is.null(meta$used_prior)

  if (!provenance_ok && !legacy_ok) {
    got_provenance <- provenance %||% NA_character_
    got_type <- if (!is.null(meta)) meta$type %||% NA_character_ else NA_character_
    cli::cli_abort(c(
      "assert_prior_free_target: target artifact is not a provenance-stamped prior-free RAPM.",
      "x" = "Got target_provenance = {.val {got_provenance}}, panna_metadata$type = {.val {got_type}}.",
      "i" = paste0(
        "Box-score value targets must be fit_rapm() output stamped ",
        "target_provenance = \"prior_free_rapm_window\" (04b_rapm_window_targets.R) ",
        "-- xRAPM/career panna embed the SPM prior and are banned as targets. ",
        "See BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.4."
      )
    ))
  }

  invisible(TRUE)
}
