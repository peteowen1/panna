# Career-trait Panna: decay-weighted multi-season xRAPM.
# See CLAUDE_TODO_CAREER_PANNA.md. Panna is the impact TRAIT (career, recency-
# weighted) — parallel to EPR/PSR — vs season `xrapm` (one season's contribution).

#' Fit career-trait Panna (decay-weighted multi-season xRAPM)
#'
#' Pools every splint across all seasons into one ridge plus-minus fit, weighting
#' each observation by exponential recency decay, and shrinks toward the
#' career-trait skill-SPM prior. Yields one rating per player as of
#' \code{reference_date} — the "how good is this player / next-game impact" trait
#' (parallel to EPR/PSR), distinct from the per-season \code{xrapm} contribution.
#'
#' @param splint_data Splint list (as in \code{cache-opta/03_splints.rds}); needs
#'   \code{splints} with \code{match_id}.
#' @param match_dates data.frame/data.table with \code{match_id} + \code{match_date}
#'   (e.g. from \code{opta_fixtures.parquet}); gives each splint its age for decay.
#' @param skill_spm Skills-pipeline SPM object (as in
#'   \code{cache-skills/03_skill_spm.rds}); its \code{offense_spm_ratings$offense_spm}
#'   and \code{defense_spm_ratings$defense_spm} (by \code{player_id}) are the prior.
#'   Ignored if \code{offense_prior}/\code{defense_prior} are supplied directly.
#' @param halflife_days Recency half-life in days: weight \code{= 0.5 ^ (age_days /
#'   halflife_days)}. Default 365 (~1 year) — tuned via \code{optimize_panna_decay}
#'   on held-out match prediction (365d was the best, monotone "shorter is better";
#'   the objective is near-flat, like EPR's, so the exact value is non-critical and
#'   365 matches the "best guess of next game" intent). 2026-06-09.
#' @param reference_date "As of" Date for ages; default = latest \code{match_date}.
#' @param min_minutes Minimum career minutes to be rated (else replacement pool).
#' @param nfolds CV folds for the ridge fit.
#' @param offense_prior,defense_prior Optional named (by \code{player_id}) prior
#'   vectors that override \code{skill_spm}.
#' @param fixed_lambda Optional single ridge lambda (skips \code{cv.glmnet}).
#'   Default \code{NULL} = cross-validated.
#' @param lambda_formula Optional \code{function(n_obs)} returning a lambda; used
#'   only when \code{fixed_lambda} is \code{NULL}. The as-of-date snapshot build
#'   passes the sample-size formula (\code{16.67 * n_obs^-0.58}) so each reference
#'   date gets a sample-appropriate lambda without re-running CV. \code{n_obs} is
#'   the count of valid (finite) splint observations actually fed to the fit.
#'
#' @return List with \code{model} (the xRAPM fit), \code{ratings} (data.table:
#'   \code{player_id, player_name, panna, panna_offense, panna_defense,
#'   total_minutes}), \code{halflife_days}, and \code{reference_date}.
#' @keywords internal
fit_career_rapm <- function(splint_data, match_dates, skill_spm = NULL,
                            halflife_days = 365, reference_date = NULL,
                            min_minutes = 200, nfolds = 5,
                            offense_prior = NULL, defense_prior = NULL,
                            fixed_lambda = NULL, lambda_formula = NULL) {
  stopifnot(is.list(splint_data), !is.null(splint_data$splints), halflife_days > 0)

  md <- data.table::as.data.table(match_dates)
  md <- unique(md[, .(match_id, match_date = as.Date(match_date))][!is.na(match_date)],
               by = "match_id")

  # 1. Pooled (all-season) design matrix — same builder as the per-season fit.
  rapm_data <- prepare_rapm_data(splint_data, min_minutes = min_minutes,
                                 include_covariates = TRUE)

  # 2. Per-observation recency decay, composed into the existing minutes weights.
  row_md <- data.table::data.table(match_id = rapm_data$row_data$match_id)
  row_md[md, match_date := i.match_date, on = "match_id"]
  if (is.null(reference_date)) reference_date <- max(row_md$match_date, na.rm = TRUE)
  reference_date <- as.Date(reference_date)
  age_days <- as.numeric(reference_date - row_md$match_date)
  n_missing <- sum(is.na(age_days))
  # Undated splints -> oldest observed age (heaviest decay) so they can't dominate.
  if (n_missing > 0) age_days[is.na(age_days)] <- max(age_days, na.rm = TRUE)
  decay <- 0.5 ^ (age_days / halflife_days)
  rapm_data$weights <- rapm_data$weights * decay
  cli::cli_alert_info(paste0(
    "Career decay: halflife {halflife_days}d | ref {as.character(reference_date)} | ",
    "age {round(min(age_days))}-{round(max(age_days))}d | ",
    "weight x{round(min(decay), 3)}-{round(max(decay), 3)} | undated rows: {n_missing}"))

  # 3. Career-trait prior from the skill-SPM (or caller-supplied vectors).
  if (is.null(offense_prior) || is.null(defense_prior)) {
    if (is.null(skill_spm)) {
      stop("Provide `skill_spm`, or both `offense_prior` and `defense_prior`.")
    }
    or <- data.table::as.data.table(skill_spm$offense_spm_ratings)
    dr <- data.table::as.data.table(skill_spm$defense_spm_ratings)
    offense_prior <- stats::setNames(or$offense_spm, or$player_id)
    defense_prior <- stats::setNames(dr$defense_spm, dr$player_id)
  }

  # 4. Fit career xRAPM = Panna. Derive lambda from the sample-size formula when
  #    requested (and no explicit fixed_lambda), using the actual valid-obs count.
  if (is.null(fixed_lambda) && !is.null(lambda_formula)) {
    n_obs <- sum(!is.na(rapm_data$y) & is.finite(rapm_data$y))
    fixed_lambda <- lambda_formula(n_obs)
    cli::cli_alert_info(
      "Fixed lambda from formula: n_obs={n_obs} -> lambda={round(fixed_lambda, 5)}")
  }
  model <- fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                               defense_prior = defense_prior, alpha = 0,
                               nfolds = nfolds, use_weights = TRUE,
                               penalize_covariates = FALSE,
                               fixed_lambda = fixed_lambda)

  ratings <- data.table::as.data.table(extract_xrapm_ratings(model))
  data.table::setnames(ratings, c("xrapm", "offense", "defense"),
                       c("panna", "panna_offense", "panna_defense"),
                       skip_absent = TRUE)

  list(model = model, ratings = ratings[],
       halflife_days = halflife_days, reference_date = reference_date)
}


#' Tune the career-Panna decay half-life on held-out match prediction
#'
#' Temporal hold-out (mirrors \code{optimize_epr_decay}): fit career-Panna on
#' splints before \code{train_end = ref_date - holdout_days}, then predict the
#' held-out splints' xG-difference target and pick the half-life that minimises
#' weighted hold-out MSE. One shared design matrix is built once and row-subset so
#' train/hold-out columns stay aligned (so \code{predict()} is valid).
#'
#' @param splint_data,match_dates,skill_spm As in \code{\link{fit_career_rapm}}.
#' @param halflife_grid Half-lives (days) to evaluate.
#' @param ref_date "Today" of the test (Date). Default = latest match_date.
#' @param holdout_days Width of the hold-out window (days back from ref_date).
#' @param min_minutes,nfolds As in \code{\link{fit_career_rapm}}.
#'
#' @return List: \code{results} (data.table halflife_days/holdout_wmse, sorted),
#'   \code{best_halflife}, \code{ref_date}, \code{train_end}, \code{n_train},
#'   \code{n_holdout}.
#' @keywords internal
optimize_panna_decay <- function(splint_data, match_dates, skill_spm,
                                 halflife_grid = c(180, 365, 545, 730, 1095, 1460),
                                 ref_date = NULL, holdout_days = 150L,
                                 min_minutes = 200, nfolds = 5) {
  md <- data.table::as.data.table(match_dates)
  md <- unique(md[, .(match_id, match_date = as.Date(match_date))][!is.na(match_date)],
               by = "match_id")

  rapm_data <- prepare_rapm_data(splint_data, min_minutes = min_minutes,
                                 include_covariates = TRUE)
  X <- if (!is.null(rapm_data$X_full)) rapm_data$X_full else rapm_data$X
  y <- rapm_data$y
  w0 <- rapm_data$weights

  row_dt <- data.table::data.table(match_id = rapm_data$row_data$match_id)
  row_dt[md, mdate := i.match_date, on = "match_id"]
  if (is.null(ref_date)) ref_date <- max(row_dt$mdate, na.rm = TRUE)
  ref_date <- as.Date(ref_date)
  train_end <- ref_date - as.integer(holdout_days)

  valid <- !is.na(y) & is.finite(y) & !is.na(row_dt$mdate)
  train_idx <- which(valid & row_dt$mdate < train_end)
  hold_idx  <- which(valid & row_dt$mdate >= train_end & row_dt$mdate < ref_date)
  cli::cli_alert_info(paste0("Decay tuning: ref {as.character(ref_date)} | train_end ",
    "{as.character(train_end)} | train {length(train_idx)} | holdout {length(hold_idx)} splints"))
  if (length(hold_idx) < 200L) cli::cli_warn("Hold-out has only {length(hold_idx)} splints")

  or <- data.table::as.data.table(skill_spm$offense_spm_ratings)
  dr <- data.table::as.data.table(skill_spm$defense_spm_ratings)
  offense_prior <- stats::setNames(or$offense_spm, or$player_id)
  defense_prior <- stats::setNames(dr$defense_spm, dr$player_id)

  age_train <- as.numeric(train_end - row_dt$mdate[train_idx])  # decay anchored at train_end
  Xh <- X[hold_idx, , drop = FALSE]; yh <- y[hold_idx]; wh <- w0[hold_idx]

  res <- data.table::rbindlist(lapply(halflife_grid, function(hl) {
    sub <- list(X_full = X[train_idx, , drop = FALSE], y = y[train_idx],
                weights = w0[train_idx] * (0.5 ^ (age_train / hl)),
                player_ids = rapm_data$player_ids,
                covariate_names = rapm_data$covariate_names,
                n_players = rapm_data$n_players,
                player_mapping = rapm_data$player_mapping)
    fit <- fit_rapm_with_prior(sub, offense_prior, defense_prior, alpha = 0,
                               nfolds = nfolds, use_weights = TRUE,
                               penalize_covariates = FALSE)
    yhat <- as.vector(stats::predict(fit, newx = Xh, s = "lambda.min")) +
      as.vector(Xh %*% fit$panna_metadata$prior_vec)
    mse <- stats::weighted.mean((yh - yhat)^2, wh)
    cli::cli_alert_success("halflife {hl}d -> holdout wMSE {round(mse, 6)}")
    data.table::data.table(halflife_days = hl, holdout_wmse = mse)
  }))

  list(results = res[order(holdout_wmse)],
       best_halflife = res$halflife_days[which.min(res$holdout_wmse)],
       ref_date = ref_date, train_end = train_end,
       n_train = length(train_idx), n_holdout = length(hold_idx))
}
