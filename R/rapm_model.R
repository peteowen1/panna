# RAPM model fitting functions for panna package
#
# Fits Regularized Adjusted Plus-Minus models using ridge regression.

#' Fit glmnet at a single pre-specified lambda (skips cross-validation)
#'
#' Used by the as-of-date career-Panna build, where the optimal lambda is
#' supplied by a sample-size formula (\code{lambda = 16.67 * n_obs^-0.58}) rather
#' than re-run via \code{cv.glmnet} for every reference date. Returns a
#' \code{glmnet} object augmented with \code{$lambda.min}/\code{$lambda.1se} set to
#' \code{fixed_lambda}, so downstream extractors (which read \code{model$lambda.min}
#' and \code{coef(model, s = lambda)}) work unchanged.
#'
#' A short decreasing lambda path is fit for warm-start numerical stability, then
#' coefficients are taken at \code{fixed_lambda} (which is in the path, so the
#' returned coefficients are the exact fitted point, not an interpolation).
#'
#' @keywords internal
#' @noRd
.glmnet_fixed_lambda <- function(x, y, weights, alpha, standardize,
                                 penalty_factor, fixed_lambda) {
  stopifnot(length(fixed_lambda) == 1, fixed_lambda > 0)
  lambda_path <- sort(unique(fixed_lambda * c(8, 4, 2, 1)), decreasing = TRUE)
  fit <- glmnet::glmnet(
    x = x, y = y, weights = weights, alpha = alpha,
    standardize = standardize, penalty.factor = penalty_factor,
    lambda = lambda_path
  )
  # Compatibility shim: downstream reads model$lambda.min / model$lambda.1se.
  fit$lambda.min <- fixed_lambda
  fit$lambda.1se <- fixed_lambda
  fit
}

#' Fit RAPM model
#'
#' Fits ridge regression on the design matrix with:
#' - Target: xgf90 (xG FOR per 90) or gf90 (goals FOR per 90)
#' - Player columns: playerX_off, playerX_def
#' - Covariates: gd, gf, ga, avg_min, is_home
#'
#' The target type is determined by the rapm_data (set in prepare_rapm_data).
#'
#' @param rapm_data List from prepare_rapm_data
#' @param alpha Elastic net mixing parameter (0 = ridge, 1 = lasso)
#' @param nfolds Number of CV folds for lambda selection
#' @param use_weights Whether to use splint duration weights
#' @param standardize Whether to standardize predictors
#' @param penalize_covariates Whether to penalize covariate coefficients
#' @param parallel Whether to use parallel processing for CV folds
#' @param n_cores Number of cores (default: half of available)
#' @param fixed_lambda Optional single lambda value. When supplied, skips
#'   \code{cv.glmnet} and fits at this lambda directly (see
#'   \code{.glmnet_fixed_lambda}). Default \code{NULL} = cross-validated (current
#'   behaviour). Used by the as-of-date career-Panna build to avoid re-running CV
#'   for every reference date.
#' @param lambda_seq Optional explicit lambda sequence for \code{cv.glmnet}
#'   (its \code{lambda} argument). The panna#87 cloud path passes a short
#'   grid bracketing the closed-form lambda (e.g. \code{lam * 2^seq(3, -3,
#'   0.5)}) so lambda is chosen BY CV from the data — adapting to sample
#'   size, weights, and design — at a fraction of the default 100-lambda
#'   path's time/memory. Ignored when \code{fixed_lambda} is supplied.
#'
#' @return Fitted model with metadata
#' @family rapm
#' @export
fit_rapm <- function(rapm_data, alpha = 0, nfolds = 10,
                         use_weights = TRUE, standardize = FALSE,
                         penalize_covariates = FALSE,
                         parallel = TRUE, n_cores = NULL,
                         fixed_lambda = NULL, lambda_seq = NULL) {
  # Validate input structure
  if (!is.list(rapm_data)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must be a list.",
      "x" = "Got {.cls {class(rapm_data)}} instead.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  has_X <- "X" %in% names(rapm_data) || "X_full" %in% names(rapm_data)
  if (!has_X) {
    cli::cli_abort(c(
      "{.arg rapm_data} must contain 'X' or 'X_full' matrix.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  if (!"y" %in% names(rapm_data)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must contain 'y' vector.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  # Support both X_full (production) and X (tests)
  X <- if (!is.null(rapm_data$X_full)) rapm_data$X_full else rapm_data$X
  y <- rapm_data$y
  weights <- if (use_weights) rapm_data$weights else NULL

  # Remove NA responses
  valid_idx <- !is.na(y) & is.finite(y)
  X <- X[valid_idx, , drop = FALSE]
  y <- y[valid_idx]
  if (!is.null(weights)) weights <- weights[valid_idx]

  if (length(y) == 0) {
    cli::cli_abort(c(
      "No valid observations after removing NA values.",
      "i" = "Check that {.arg rapm_data$y} contains non-NA values."
    ))
  }

  progress_msg(sprintf("Fitting RAPM: %d observations, %d columns",
                       length(y), ncol(X)))

  # Set up parallel processing (only relevant for the cv.glmnet fold loop;
  # the fixed-lambda path fits a single model, so skip the backend setup).
  if (parallel && is.null(fixed_lambda)) {
    .check_suggests("parallel", "Parallel RAPM fitting requires parallel.")
    .check_suggests("doParallel", "Parallel RAPM fitting requires doParallel.")
    if (is.null(n_cores)) {
      n_cores <- max(1, floor(parallel::detectCores() / 2))
    }
    # Respect R CMD check limits (typically 2 cores max)
    check_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
    if (nzchar(check_limit) && check_limit == "TRUE") {
      n_cores <- min(n_cores, 2L)
    }
    progress_msg(sprintf("Using %d cores for parallel CV", n_cores))
    doParallel::registerDoParallel(cores = n_cores)
    on.exit(doParallel::stopImplicitCluster(), add = TRUE)
  }

  # Penalty factor: don't penalize covariates if requested
  # Covariates are always the last columns (cbind(X_players, X_covariates) in prepare_rapm_data)
  if (!penalize_covariates && length(rapm_data$covariate_names) > 0) {
    n_cols <- ncol(X)
    n_cov <- length(rapm_data$covariate_names)
    penalty_factor <- c(rep(1, n_cols - n_cov), rep(0, n_cov))
  } else {
    penalty_factor <- rep(1, ncol(X))
  }

  # Fit ridge regression: cross-validated, or at a single supplied lambda.
  if (is.null(fixed_lambda)) {
    cv_fit <- glmnet::cv.glmnet(
      x = X,
      y = y,
      weights = weights,
      alpha = alpha,
      standardize = standardize,
      nfolds = nfolds,
      lambda = lambda_seq,
      type.measure = "mse",
      penalty.factor = penalty_factor,
      trace.it = if (interactive()) 1 else 0,
      parallel = parallel
    )
  } else {
    cv_fit <- .glmnet_fixed_lambda(X, y, weights, alpha, standardize,
                                   penalty_factor, fixed_lambda)
  }

  # Add metadata
  target_type <- if (!is.null(rapm_data$target_type)) rapm_data$target_type else "xg"
  cv_fit$panna_metadata <- list(
    type = "rapm",
    target_type = target_type,
    alpha = alpha,
    n_observations = length(y),
    n_player_cols = rapm_data$n_players * 2,
    n_covariates = length(rapm_data$covariate_names),
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    player_mapping = rapm_data$player_mapping,
    player_ids = rapm_data$player_ids,
    covariate_names = rapm_data$covariate_names
  )

  target_desc <- if (target_type == "xg") "xG-based" else "Goals-based"
  if (is.null(fixed_lambda)) {
    progress_msg(sprintf("RAPM fit complete (%s). Lambda.min: %.4f, R^2: %.3f",
                         target_desc, cv_fit$lambda.min,
                         1 - cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min] /
                           var(y)))
  } else {
    progress_msg(sprintf("RAPM fit complete (%s). Fixed lambda: %.4f (no CV)",
                         target_desc, fixed_lambda))
  }

  cv_fit
}


#' Extract RAPM ratings from fitted model
#'
#' Calculates player ratings as offense_coef - defense_coef.
#' Positive = good, negative = bad.
#'
#' @param model Fitted RAPM model from fit_rapm
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Data frame with player ratings
#' @family rapm
#' @export
extract_rapm_ratings <- function(model, lambda = "min") {
  # Get lambda value
  lambda_val <- if (lambda == "min") {
    model$lambda.min
  } else if (lambda == "1se") {
    model$lambda.1se
  } else {
    as.numeric(lambda)
  }

  # Extract all coefficients
  coef_mat <- stats::coef(model, s = lambda_val)
  all_coefs <- as.vector(coef_mat)[-1]  # Remove intercept
  col_names <- rownames(coef_mat)[-1]
  names(all_coefs) <- col_names

  # Separate player coefficients from covariates
  player_ids <- model$panna_metadata$player_ids
  covariate_names <- model$panna_metadata$covariate_names

  # Extract offense and defense coefficients
  off_cols <- paste0(player_ids, "_off")
  def_cols <- paste0(player_ids, "_def")

  off_coefs <- all_coefs[off_cols]
  def_coefs <- all_coefs[def_cols]

  # RAPM rating = offense - defense
  # Positive offense = creates more xG (good)
  # Positive defense = allows more xG (bad), so we subtract
  rapm <- off_coefs - def_coefs

  # Create results data frame
  ratings <- data.frame(
    player_id = player_ids,
    rapm = as.numeric(rapm),
    offense = as.numeric(off_coefs),
    defense = as.numeric(def_coefs)
  )

  # Join with player mapping
  if (!is.null(model$panna_metadata$player_mapping)) {
    mapping <- data.table::as.data.table(model$panna_metadata$player_mapping[, c("player_id", "player_name", "total_minutes")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  # Add covariate effects if available
  if (length(covariate_names) > 0) {
    cov_coefs <- all_coefs[covariate_names]
    attr(ratings, "covariate_effects") <- cov_coefs
  }

  ratings <- ratings[order(-ratings$rapm), ]

  ratings
}


#' Get covariate effects from fitted model
#'
#' Extracts the coefficients for game state covariates.
#'
#' @param model Fitted RAPM model from fit_rapm
#' @param lambda Which lambda to use
#'
#' @return Named vector of covariate coefficients
#' @keywords internal
get_covariate_effects <- function(model, lambda = "min") {
  lambda_val <- if (lambda == "min") model$lambda.min else model$lambda.1se

  coef_mat <- stats::coef(model, s = lambda_val)
  all_coefs <- as.vector(coef_mat)[-1]
  col_names <- rownames(coef_mat)[-1]
  names(all_coefs) <- col_names

  covariate_names <- model$panna_metadata$covariate_names

  if (length(covariate_names) == 0) {
    return(NULL)
  }

  all_coefs[covariate_names]
}


#' Fill one suffix's slice of a RAPM prior vector, validating a supplied prior matched
#'
#' C4 shared helper (FABLE-PRIOR-FIX-PLAN.md): \code{fit_rapm_with_prior()}'s
#' net branch and the two halves of its od branch (offense/defense) each
#' fill one suffixed slice of \code{prior_vec} from a named \code{prior}
#' vector and abort if a SUPPLIED prior matched 0 players -- previously
#' duplicated three times (net/off/def), parametrized only by suffix, arg
#' name, and message prefix. Preserves the exact abort text callers (and
#' \code{test-rapm-tripwires.R}'s regex assertions) depend on.
#'
#' @param prior_vec The full prior vector being built (named by X column).
#' @param col_names Column names of the design matrix X (confirms the
#'   suffixed column actually exists before filling it).
#' @param prior Named vector of per-player prior values (by player_id), or
#'   \code{NULL} for an explicit no-prior request (not an error).
#' @param player_ids Character vector of player IDs in rapm_data order.
#' @param suffix Column suffix: \code{"_net"}, \code{"_off"}, or \code{"_def"}.
#' @param arg_label Argument name for the abort message (\code{"offense_prior"}
#'   or \code{"defense_prior"}).
#' @param msg_prefix \code{"xRAPM"} (od mode) or \code{"xRAPM (net)"} (net
#'   mode) -- matches the message text callers depended on before this
#'   helper existed.
#'
#' @return List with the updated \code{prior_vec} and the match count.
#' @keywords internal
#' @noRd
.fill_prior <- function(prior_vec, col_names, prior, player_ids, suffix,
                         arg_label, msg_prefix) {
  cols <- paste0(player_ids, suffix)
  match_idx <- match(player_ids, names(prior))
  valid <- !is.na(match_idx) & cols %in% col_names
  if (any(valid)) {
    prior_vec[cols[valid]] <- prior[player_ids[valid]]
  }
  matched <- sum(valid)

  if (!is.null(prior) && matched == 0) {
    cli::cli_abort(c(
      "{msg_prefix}: {.arg {arg_label}} was supplied but matched 0 of {length(player_ids)} players.",
      "x" = "A supplied-but-unmatched prior is always a bug, never a valid zero-prior fallback.",
      "i" = "Pass {.code {arg_label} = NULL} for an explicit no-prior fit, or verify the prior vector is named by {.field player_id} (see {.fn build_prior_vector})."
    ))
  }

  list(prior_vec = prior_vec, matched = matched)
}


#' Fit RAPM with SPM prior (xRAPM)
#'
#' Fits RAPM model shrinking toward SPM predictions instead of zero.
#' This helps separate players who always appear together by using
#' box score statistics as a Bayesian prior.
#'
#' For the O/D design matrix (\code{mode = "od"}, default):
#' - offense_prior: SPM-predicted offensive contribution
#' - defense_prior: SPM-predicted defensive contribution
#'
#' For the net design matrix (\code{mode = "net"}, FABLE-PRIOR-FIX-PLAN.md D2/D4
#' -- e.g. WPA, whose off/def split is mechanically unidentified because the
#' target is zero-sum): a single per-player column exists, so
#' \code{offense_prior} alone carries the net SPM prior and
#' \code{defense_prior} has no meaning and must be \code{NULL}.
#'
#' @param rapm_data List from prepare_rapm_data
#' @param offense_prior Named vector of offensive SPM predictions (by
#'   player_id). In \code{mode = "net"} this is the single net SPM prior.
#' @param defense_prior Named vector of defensive SPM predictions (by
#'   player_id). Must be \code{NULL} when \code{mode = "net"}.
#' @param alpha Elastic net mixing parameter (0 = ridge)
#' @param nfolds Number of CV folds
#' @param use_weights Whether to use splint duration weights
#' @param penalize_covariates Whether to penalize covariate coefficients
#' @param fixed_lambda Optional single lambda value. When supplied, skips
#'   \code{cv.glmnet} and fits at this lambda directly (see
#'   \code{.glmnet_fixed_lambda}). Default \code{NULL} = cross-validated (current
#'   behaviour). Used by the as-of-date career-Panna build.
#' @param lambda_seq Optional explicit lambda sequence for \code{cv.glmnet}
#'   (see \code{\link{fit_rapm}}); the panna#87 cloud path passes a short
#'   grid bracketing the closed-form lambda. Ignored when
#'   \code{fixed_lambda} is supplied.
#' @param mode Design matrix mode matching the \code{rapm_data} the caller
#'   built. \code{"od"} (default) expects \verb{_off}/\verb{_def} player
#'   columns. \code{"net"} expects the single-column-per-player
#'   (\verb{_net}) design from \code{create_rapm_design_matrix(mode = "net")}
#'   and requires \code{defense_prior = NULL} (FABLE-PRIOR-FIX-PLAN.md D2/D4).
#'
#' @return Fitted model with prior adjustment metadata
#'
#' @family rapm
#' @export
fit_rapm_with_prior <- function(rapm_data, offense_prior, defense_prior,
                                 alpha = 0, nfolds = 10,
                                 use_weights = TRUE,
                                 penalize_covariates = FALSE,
                                 fixed_lambda = NULL, lambda_seq = NULL,
                                 mode = c("od", "net")) {
  mode <- match.arg(mode)

  # D2/D4 (FABLE-PRIOR-FIX-PLAN.md): net mode has a single per-player prior
  # dimension -- offense_prior carries it, defense_prior has no meaning
  # against a design with no offense/defense split and must be absent. This
  # is a pure argument-shape check, independent of rapm_data.
  if (mode == "net" && !is.null(defense_prior)) {
    cli::cli_abort(c(
      "xRAPM (net): {.arg defense_prior} must be {.code NULL} when {.code mode = \"net\"}.",
      "x" = "Net mode fits a single signed per-player column (no offense/defense split); {.arg offense_prior} alone carries the net prior.",
      "i" = "See FABLE-PRIOR-FIX-PLAN.md D2/D4."
    ))
  }

  # Validate input structure (matching fit_rapm())
  if (!is.list(rapm_data)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must be a list.",
      "x" = "Got {.cls {class(rapm_data)}} instead.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  has_X <- "X" %in% names(rapm_data) || "X_full" %in% names(rapm_data)
  if (!has_X || !"y" %in% names(rapm_data)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must contain 'X' (or 'X_full') and 'y'.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  # F5 (FABLE-PRIOR-FIX-PLAN.md review): a caller-supplied `mode` that
  # disagrees with the mode the design matrix was actually built with
  # (rapm_data$mode, set by create_rapm_design_matrix()) previously fit
  # silently with wrong metadata (e.g. calling mode = "net" priors against
  # an "od" design would fill "_net" columns that don't exist in X, matching
  # 0 players, and the D4 unmatched-prior guard above would catch a supplied
  # prior -- but an explicit NULL prior would sail through with a completely
  # mismatched panna_metadata$mode). Older rapm_data without a $mode element
  # (pre-mode-parameter fixtures) skip this check -- unchanged behavior.
  if (!is.null(rapm_data$mode) && rapm_data$mode != mode) {
    cli::cli_abort(c(
      "xRAPM: {.arg mode} ({.val {mode}}) does not match {.code rapm_data$mode} ({.val {rapm_data$mode}}).",
      "x" = "The design matrix mode and the {.fn fit_rapm_with_prior} {.arg mode} argument must agree.",
      "i" = "Pass {.code mode = \"{rapm_data$mode}\"} to match the design matrix, or rebuild {.arg rapm_data} with {.code mode = \"{mode}\"}."
    ))
  }

  # Support both X_full (production) and X (tests)
  X <- if (!is.null(rapm_data$X_full)) rapm_data$X_full else rapm_data$X
  y <- rapm_data$y
  weights <- if (use_weights) rapm_data$weights else NULL

  # Remove NA responses
  valid_idx <- !is.na(y) & is.finite(y)
  X <- X[valid_idx, , drop = FALSE]
  y <- y[valid_idx]
  if (!is.null(weights)) weights <- weights[valid_idx]

  # Get column names
  col_names <- colnames(X)
  player_ids <- rapm_data$player_ids
  # Support both covariate_names (production) and covariate_cols (tests)
  covariate_names <- if (!is.null(rapm_data$covariate_names)) {
    rapm_data$covariate_names
  } else {
    rapm_data$covariate_cols
  }

  # Build full prior vector (including covariates = 0)
  n_cols <- ncol(X)
  prior_vec <- rep(0, n_cols)
  names(prior_vec) <- col_names

  # D4 guard (FABLE-PRIOR-FIX-PLAN.md): a SUPPLIED prior that matches zero
  # players is always a bug -- e.g. an unnamed vector (the 06_xrapm.R
  # multi-target L3 bug: match(player_ids, names(offense_prior)) can only
  # succeed if offense_prior is named by player_id) -- never a legitimate
  # all-zero fallback. Distinguish that from an EXPLICIT no-prior request
  # (offense_prior/defense_prior passed as NULL), which is not an error: the
  # fit degrades gracefully to a zero prior, same as before this guard.
  # C4: fill + validate via the shared .fill_prior() helper (previously
  # duplicated three times: net/off/def).
  if (mode == "net") {
    # Single per-player column, no offense/defense split (D2). offense_prior
    # carries the net prior; defense_prior is already guaranteed NULL above.
    net_res <- .fill_prior(prior_vec, col_names, offense_prior, player_ids,
                           suffix = "_net", arg_label = "offense_prior",
                           msg_prefix = "xRAPM (net)")
    prior_vec <- net_res$prior_vec
    off_matched <- net_res$matched
    def_matched <- 0L

    progress_msg(sprintf("xRAPM (net): matched %d net priors", off_matched))
  } else {
    off_res <- .fill_prior(prior_vec, col_names, offense_prior, player_ids,
                           suffix = "_off", arg_label = "offense_prior",
                           msg_prefix = "xRAPM")
    prior_vec <- off_res$prior_vec
    off_matched <- off_res$matched

    def_res <- .fill_prior(prior_vec, col_names, defense_prior, player_ids,
                           suffix = "_def", arg_label = "defense_prior",
                           msg_prefix = "xRAPM")
    prior_vec <- def_res$prior_vec
    def_matched <- def_res$matched

    progress_msg(sprintf("xRAPM: matched %d offense priors, %d defense priors",
                         off_matched, def_matched))
  }

  # Transform: shrink toward prior instead of zero
  # Original: min ||y - X*beta||^2 + lambda*||beta - prior||^2
  # Substitute: gamma = beta - prior
  # Becomes: min ||(y - X*prior) - X*gamma||^2 + lambda*||gamma||^2
  y_adjusted <- as.vector(y - X %*% prior_vec)

  progress_msg(sprintf("Fitting xRAPM: %d observations, %d columns",
                       length(y_adjusted), ncol(X)))

  # Penalty factor: don't penalize covariates if requested
  # Covariates are always the last columns (cbind(X_players, X_covariates) in prepare_rapm_data)
  if (!penalize_covariates && length(covariate_names) > 0) {
    penalty_factor <- c(rep(1, n_cols - length(covariate_names)),
                        rep(0, length(covariate_names)))
  } else {
    penalty_factor <- rep(1, n_cols)
  }

  # Fit ridge on adjusted response: cross-validated, or at a single supplied lambda.
  if (is.null(fixed_lambda)) {
    cv_fit <- glmnet::cv.glmnet(
      x = X,
      y = y_adjusted,
      weights = weights,
      alpha = alpha,
      standardize = FALSE,
      nfolds = nfolds,
      lambda = lambda_seq,
      type.measure = "mse",
      penalty.factor = penalty_factor,
      trace.it = if (interactive()) 1 else 0
    )
  } else {
    cv_fit <- .glmnet_fixed_lambda(X, y_adjusted, weights, alpha,
                                   standardize = FALSE, penalty_factor,
                                   fixed_lambda)
  }

  # Store metadata including prior information
  cv_fit$panna_metadata <- list(
    type = if (mode == "net") "xrapm_net" else "xrapm",
    mode = mode,
    alpha = alpha,
    n_observations = length(y_adjusted),
    n_player_cols = if (mode == "net") rapm_data$n_players else rapm_data$n_players * 2,
    n_covariates = length(covariate_names),
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    player_mapping = rapm_data$player_mapping,
    player_ids = rapm_data$player_ids,
    covariate_names = covariate_names,
    used_prior = TRUE,
    prior_vec = prior_vec,
    offense_prior = offense_prior,
    defense_prior = defense_prior
  )

  progress_msg(sprintf("xRAPM fit complete. Lambda.min: %.4f",
                       cv_fit$lambda.min))

  cv_fit
}


#' Extract xRAPM ratings (with prior)
#'
#' Extracts player ratings from a model fit with SPM prior.
#' The final coefficient is gamma + prior, where gamma is the deviation.
#'
#' F4 (FABLE-PRIOR-FIX-PLAN.md review): mode-aware via the model's stored
#' metadata (\code{model$panna_metadata$mode}, or \code{type == "xrapm_net"}
#' for older/hand-built metadata without a \code{mode} field). In
#' \code{mode = "od"} (default), \code{xrapm = offense - defense} as before.
#' In \code{mode = "net"}, there is no offense/defense split (D2) -- the
#' single \verb{_net} coefficient per player IS the rating, and
#' \code{offense}/\code{defense}/\code{off_deviation}/\code{def_deviation}/
#' \code{off_prior}/\code{def_prior} are set \code{NA} (they have no meaning
#' against a design with no offense/defense split). Aborts if the model's
#' coefficient names don't actually match the declared mode -- indexing only
#' \verb{_off}/\verb{_def} names on a net-mode fit previously returned
#' silently all-NA ratings with no error (the bug this fixes).
#'
#' @param model Fitted xRAPM model from fit_rapm_with_prior
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Data frame with player ratings including deviation from prior.
#'   In \code{mode = "net"}, \code{offense}/\code{defense}/deviation/prior
#'   columns are \code{NA} and \code{xrapm} holds the net coefficient.
#' @family panna ratings
#' @export
extract_xrapm_ratings <- function(model, lambda = "min") {
  # Get lambda value
  lambda_val <- if (lambda == "min") {
    model$lambda.min
  } else if (lambda == "1se") {
    model$lambda.1se
  } else {
    as.numeric(lambda)
  }

  # Extract gamma (deviation from prior)
  coef_mat <- stats::coef(model, s = lambda_val)
  gamma <- as.vector(coef_mat)[-1]
  col_names <- rownames(coef_mat)[-1]
  names(gamma) <- col_names

  # Get prior vector
  prior_vec <- model$panna_metadata$prior_vec

  # Final coefficients: beta = gamma + prior
  beta_final <- gamma + prior_vec[col_names]

  player_ids <- model$panna_metadata$player_ids
  mode <- model$panna_metadata$mode
  if (is.null(mode)) {
    mode <- if (identical(model$panna_metadata$type, "xrapm_net")) "net" else "od"
  }

  if (mode == "net") {
    net_cols <- paste0(player_ids, "_net")
    if (!all(net_cols %in% names(beta_final))) {
      missing_cols <- net_cols[!net_cols %in% names(beta_final)]
      cli::cli_abort(c(
        "extract_xrapm_ratings: model coefficients don't match the declared {.val net} mode.",
        "x" = "Missing {length(missing_cols)} {.field _net} column{?s} (e.g. {.val {head(missing_cols, 3)}}).",
        "i" = "Was this model actually fit with {.code mode = \"net\"} via {.fn fit_rapm_with_prior}?"
      ))
    }

    net_coefs <- beta_final[net_cols]

    # Net mode has no offense/defense split (D2) -- the single _net
    # coefficient per player IS the rating. offense/defense (and their
    # deviation/prior) have no meaning here and are NA, per roxygen above.
    ratings <- data.frame(
      player_id = player_ids,
      xrapm = as.numeric(net_coefs),
      offense = NA_real_,
      defense = NA_real_,
      off_deviation = NA_real_,
      def_deviation = NA_real_,
      off_prior = NA_real_,
      def_prior = NA_real_
    )
  } else {
    off_cols <- paste0(player_ids, "_off")
    def_cols <- paste0(player_ids, "_def")
    if (!all(off_cols %in% names(beta_final)) || !all(def_cols %in% names(beta_final))) {
      missing_cols <- c(off_cols, def_cols)[!c(off_cols, def_cols) %in% names(beta_final)]
      cli::cli_abort(c(
        "extract_xrapm_ratings: model coefficients don't match the declared {.val od} mode.",
        "x" = "Missing {length(missing_cols)} {.field _off}/{.field _def} column{?s} (e.g. {.val {head(missing_cols, 3)}}).",
        "i" = "Was this model actually fit with {.code mode = \"net\"}? Net-mode models have only {.field _net} columns."
      ))
    }

    off_coefs <- beta_final[off_cols]
    def_coefs <- beta_final[def_cols]
    off_gamma <- gamma[off_cols]
    def_gamma <- gamma[def_cols]
    off_prior <- prior_vec[off_cols]
    def_prior <- prior_vec[def_cols]

    # xRAPM rating = offense - defense
    xrapm <- off_coefs - def_coefs

    ratings <- data.frame(
      player_id = player_ids,
      xrapm = as.numeric(xrapm),
      offense = as.numeric(off_coefs),
      defense = as.numeric(def_coefs),
      off_deviation = as.numeric(off_gamma),
      def_deviation = as.numeric(def_gamma),
      off_prior = as.numeric(off_prior),
      def_prior = as.numeric(def_prior)
    )
  }

  # Join with player mapping
  if (!is.null(model$panna_metadata$player_mapping)) {
    mapping <- data.table::as.data.table(model$panna_metadata$player_mapping[, c("player_id", "player_name", "total_minutes")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  ratings <- ratings[order(-ratings$xrapm), ]

  ratings
}


#' Abort if a multi-target RAPM/xRAPM fit shows a known degenerate-output signature
#'
#' D5 write-time tripwire (FABLE-PRIOR-FIX-PLAN.md): guards go before every
#' \code{saveRDS()} of a multi-target (EPV/WPA/PSV) artifact, because the
#' panna#87 heartbeat upload globs \code{0*.rds} and would publish whatever
#' exists. Catches two signatures confirmed via live evidence sweep on the
#' current cache vintage: (1) all-shrunk-to-zero coefficients (EPV, measured
#' sd ~ 6e-6 -- the whole-match-proration bug means the target cannot vary
#' within a lineup) and (2) mechanically mirrored offense/defense coefficients
#' from a near-zero-sum target (WPA, measured cor ~ -0.949 -- the O/D split is
#' unidentified by construction). A breach aborts loudly rather than silently
#' writing a degenerate artifact; base/xG sections are unaffected because this
#' is only called from multi-target code paths.
#'
#' @param ratings Data frame with (at least) numeric \code{offense}/
#'   \code{defense} columns, as returned by \code{extract_rapm_ratings()} /
#'   \code{extract_xrapm_ratings()}.
#' @param target_label Character, used only in the abort message (e.g. \code{"epv"}).
#' @param sd_threshold Minimum sd() for offense/defense coefficients (default
#'   \code{1e-4}, per D5).
#' @param cor_threshold Maximum \code{abs(cor(offense, defense))} (default
#'   \code{0.9}, per D5).
#'
#' @return Invisibly \code{TRUE} if no tripwire fired.
#' @keywords internal
#' @noRd
.check_degenerate_multi_target <- function(ratings, target_label,
                                           sd_threshold = 1e-4,
                                           cor_threshold = 0.9) {
  if (!is.data.frame(ratings) || !all(c("offense", "defense") %in% names(ratings))) {
    cli::cli_abort(c(
      "Multi-target tripwire for {.val {target_label}}: {.arg ratings} must be a data frame with {.field offense}/{.field defense} columns.",
      "x" = "Got {.cls {class(ratings)}} with columns {.field {names(ratings)}}."
    ))
  }

  sd_off <- stats::sd(ratings$offense, na.rm = TRUE)
  sd_def <- stats::sd(ratings$defense, na.rm = TRUE)
  if (!is.finite(sd_off) || sd_off <= sd_threshold ||
      !is.finite(sd_def) || sd_def <= sd_threshold) {
    cli::cli_abort(c(
      "Degenerate multi-target RAPM output for {.val {target_label}}: coefficients are all-shrunk.",
      "x" = "sd(offense) = {signif(sd_off, 3)}, sd(defense) = {signif(sd_def, 3)} (threshold {sd_threshold}).",
      "i" = "See FABLE-PRIOR-FIX-PLAN.md D5/C1 -- this artifact will NOT be written."
    ))
  }

  cor_od <- stats::cor(ratings$offense, ratings$defense, use = "complete.obs")
  if (is.finite(cor_od) && abs(cor_od) >= cor_threshold) {
    cli::cli_abort(c(
      "Degenerate multi-target RAPM output for {.val {target_label}}: offense/defense are mirrored.",
      "x" = "abs(cor(offense, defense)) = {signif(abs(cor_od), 3)} (threshold {cor_threshold}).",
      "i" = "See FABLE-PRIOR-FIX-PLAN.md D5/C1 -- this artifact will NOT be written."
    ))
  }

  invisible(TRUE)
}


#' Extract RAPM coefficients
#'
#' Gets player ratings from a fitted RAPM model.
#'
#' @param model Fitted RAPM model from fit_rapm
#' @param lambda Which lambda to use ("min" or "1se" or numeric)
#'
#' @return Data frame with player ratings
#' @keywords internal
extract_rapm_coefficients <- function(model, lambda = "min") {
  # Determine lambda value
  if (is.character(lambda)) {
    lambda_val <- if (lambda == "min") {
      model$lambda.min
    } else if (lambda == "1se") {
      model$lambda.1se
    } else {
      cli::cli_abort(c(
        "{.arg lambda} must be {.val min}, {.val 1se}, or numeric.",
        "x" = "Got {.val {lambda}} instead."
      ))
    }
  } else {
    lambda_val <- lambda
  }

  # Extract coefficients
  coefs <- as.vector(stats::coef(model, s = lambda_val))
  player_coefs <- coefs[-1]  # Remove intercept

  # Get player IDs from model
  player_ids <- rownames(stats::coef(model))[-1]

  # Create results data frame
  ratings <- data.frame(
    player_id = player_ids,
    rapm = player_coefs
  )

  # Join with player mapping if available
  if (!is.null(model$panna_metadata$player_mapping)) {
    mapping <- data.table::as.data.table(model$panna_metadata$player_mapping[, c("player_id", "player_name")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  ratings <- ratings[order(-ratings$rapm), ]

  ratings
}


#' Extract offensive/defensive RAPM coefficients
#'
#' Gets separate O-RAPM and D-RAPM from a model fit on O/D matrix.
#'
#' @param model Fitted RAPM model with O/D separated matrix
#' @param lambda Which lambda to use
#'
#' @return Data frame with offensive and defensive ratings
#' @keywords internal
extract_od_rapm_coefficients <- function(model, lambda = "min") {
  # Get all coefficients
  all_coefs <- extract_rapm_coefficients(model, lambda)

  # Split into offensive and defensive
  off_mask <- grepl("_off$", all_coefs$player_id)
  def_mask <- grepl("_def$", all_coefs$player_id)

  off_ratings <- all_coefs[off_mask, ]
  off_ratings$player_id <- gsub("_off$", "", off_ratings$player_id)
  off_ratings$o_rapm <- off_ratings$rapm
  off_ratings <- off_ratings[, c("player_id", "o_rapm"), drop = FALSE]

  def_ratings <- all_coefs[def_mask, ]
  def_ratings$player_id <- gsub("_def$", "", def_ratings$player_id)
  def_ratings$d_rapm <- def_ratings$rapm
  def_ratings <- def_ratings[, c("player_id", "d_rapm"), drop = FALSE]

  # Combine
  ratings <- data.table::as.data.table(def_ratings)[data.table::as.data.table(off_ratings), on = "player_id"]
  data.table::setDF(ratings)
  ratings$rapm <- ratings$o_rapm - ratings$d_rapm
  ratings <- ratings[order(-ratings$rapm), ]

  # Join with player mapping if available
  if (!is.null(model$panna_metadata$player_mapping)) {
    mapping <- data.table::as.data.table(model$panna_metadata$player_mapping[, c("player_id", "player_name")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  ratings
}


#' Aggregate RAPM by team
#'
#' Summarizes player ratings at team level.
#'
#' @param ratings Data frame of player ratings
#' @param player_data Data frame linking players to teams
#'
#' @return Data frame with team-level summaries
#' @keywords internal
aggregate_rapm_by_team <- function(ratings, player_data) {
  if (!"team" %in% names(player_data)) {
    cli::cli_warn("No {.field team} column in {.arg player_data}.")
    return(NULL)
  }

  merged <- data.table::as.data.table(player_data[, c("player_id", "team"), drop = FALSE])[data.table::as.data.table(ratings), on = "player_id"]
  result <- merged[, .(
    n_players = .N,
    mean_rapm = mean(rapm, na.rm = TRUE),
    total_rapm = sum(rapm, na.rm = TRUE),
    top_player_rapm = max(rapm, na.rm = TRUE)
  ), by = team]
  setorder(result, -mean_rapm)
  as.data.frame(result)
}
