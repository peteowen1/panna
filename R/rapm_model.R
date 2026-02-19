# RAPM model fitting functions for panna package
#
# Fits Regularized Adjusted Plus-Minus models using ridge regression.

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
#'
#' @return Fitted model with metadata
#' @export
fit_rapm <- function(rapm_data, alpha = 0, nfolds = 10,
                         use_weights = TRUE, standardize = FALSE,
                         penalize_covariates = FALSE,
                         parallel = TRUE, n_cores = NULL) {
  # Validate input structure
  if (!is.list(rapm_data)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must be a list.",
      "x" = "Got {.cls {class(rapm_data)}} instead.",
      "i" = "Use {.fn create_rapm_design_matrix} to generate valid rapm_data."
    ))
  }

  required_elements <- c("y")
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

  # Set up parallel processing
  if (parallel) {
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

  # Fit cross-validated ridge regression
  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    weights = weights,
    alpha = alpha,
    standardize = standardize,
    nfolds = nfolds,
    type.measure = "mse",
    penalty.factor = penalty_factor,
    trace.it = if (interactive()) 1 else 0,
    parallel = parallel
  )

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
  progress_msg(sprintf("RAPM fit complete (%s). Lambda.min: %.4f, R^2: %.3f",
                       target_desc, cv_fit$lambda.min,
                       1 - cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min] /
                         var(y)))

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
  all_coefs <- as.vector(stats::coef(model, s = lambda_val))[-1]  # Remove intercept
  col_names <- rownames(stats::coef(model, s = lambda_val))[-1]
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

  all_coefs <- as.vector(stats::coef(model, s = lambda_val))[-1]
  col_names <- rownames(stats::coef(model, s = lambda_val))[-1]
  names(all_coefs) <- col_names

  covariate_names <- model$panna_metadata$covariate_names

  if (length(covariate_names) == 0) {
    return(NULL)
  }

  all_coefs[covariate_names]
}


#' Fit RAPM with SPM prior (xRAPM)
#'
#' Fits RAPM model shrinking toward SPM predictions instead of zero.
#' This helps separate players who always appear together by using
#' box score statistics as a Bayesian prior.
#'
#' For the O/D design matrix:
#' - offense_prior: SPM-predicted offensive contribution
#' - defense_prior: SPM-predicted defensive contribution
#'
#' @param rapm_data List from prepare_rapm_data
#' @param offense_prior Named vector of offensive SPM predictions (by player_id)
#' @param defense_prior Named vector of defensive SPM predictions (by player_id)
#' @param alpha Elastic net mixing parameter (0 = ridge)
#' @param nfolds Number of CV folds
#' @param use_weights Whether to use splint duration weights
#' @param penalize_covariates Whether to penalize covariate coefficients
#'
#' @return Fitted model with prior adjustment metadata
#' @export
fit_rapm_with_prior <- function(rapm_data, offense_prior, defense_prior,
                                 alpha = 0, nfolds = 10,
                                 use_weights = TRUE,
                                 penalize_covariates = FALSE) {
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

  # Fill in offense priors (vectorized O(n) instead of O(n²) loop)
  off_cols <- paste0(player_ids, "_off")
  off_match_idx <- match(player_ids, names(offense_prior))
  off_valid <- !is.na(off_match_idx) & off_cols %in% col_names
  if (any(off_valid)) {
    prior_vec[off_cols[off_valid]] <- offense_prior[player_ids[off_valid]]
  }
  off_matched <- sum(off_valid)

  # Fill in defense priors (vectorized O(n) instead of O(n²) loop)
  def_cols <- paste0(player_ids, "_def")
  def_match_idx <- match(player_ids, names(defense_prior))
  def_valid <- !is.na(def_match_idx) & def_cols %in% col_names
  if (any(def_valid)) {
    prior_vec[def_cols[def_valid]] <- defense_prior[player_ids[def_valid]]
  }
  def_matched <- sum(def_valid)

  progress_msg(sprintf("xRAPM: matched %d offense priors, %d defense priors",
                       off_matched, def_matched))

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

  # Fit cross-validated ridge regression on adjusted response
  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y_adjusted,
    weights = weights,
    alpha = alpha,
    standardize = FALSE,
    nfolds = nfolds,
    type.measure = "mse",
    penalty.factor = penalty_factor,
    trace.it = if (interactive()) 1 else 0
  )

  # Store metadata including prior information
  cv_fit$panna_metadata <- list(
    type = "xrapm",
    alpha = alpha,
    n_observations = length(y_adjusted),
    n_player_cols = rapm_data$n_players * 2,
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
#' @param model Fitted xRAPM model from fit_rapm_with_prior
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Data frame with player ratings including deviation from prior
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
  gamma <- as.vector(stats::coef(model, s = lambda_val))[-1]
  col_names <- rownames(stats::coef(model, s = lambda_val))[-1]
  names(gamma) <- col_names

  # Get prior vector
  prior_vec <- model$panna_metadata$prior_vec

  # Final coefficients: beta = gamma + prior
  beta_final <- gamma + prior_vec[col_names]

  # Separate into offense and defense
  player_ids <- model$panna_metadata$player_ids
  off_cols <- paste0(player_ids, "_off")
  def_cols <- paste0(player_ids, "_def")

  off_coefs <- beta_final[off_cols]
  def_coefs <- beta_final[def_cols]
  off_gamma <- gamma[off_cols]
  def_gamma <- gamma[def_cols]
  off_prior <- prior_vec[off_cols]
  def_prior <- prior_vec[def_cols]

  # xRAPM rating = offense - defense
  xrapm <- off_coefs - def_coefs

  # Create results data frame
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

  # Join with player mapping
  if (!is.null(model$panna_metadata$player_mapping)) {
    mapping <- data.table::as.data.table(model$panna_metadata$player_mapping[, c("player_id", "player_name", "total_minutes")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  ratings <- ratings[order(-ratings$xrapm), ]

  ratings
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
  # Check if model used a prior and has final coefficients stored
  if (!is.null(model$panna_metadata$used_prior) &&
      model$panna_metadata$used_prior &&
      !is.null(model$panna_metadata$coefficients_final)) {
    # Use pre-computed final coefficients (gamma + prior)
    player_coefs <- model$panna_metadata$coefficients_final
    player_ids <- names(player_coefs)

    # Create results data frame
    ratings <- data.frame(
      player_id = player_ids,
      rapm = as.numeric(player_coefs),
      deviation_from_prior = as.numeric(model$panna_metadata$deviation_from_prior)
    )

    # Add prior values for reference
    if (!is.null(model$panna_metadata$prior)) {
      ratings$spm_prior <- as.numeric(model$panna_metadata$prior)
    }
  } else {
    # Standard extraction (no prior used)
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
  }

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
