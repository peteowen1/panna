# SPM (Statistical Plus-Minus) model functions for panna package
#
# SPM predicts RAPM ratings from box score statistics.
# This serves as the Bayesian prior for RAPM estimation, helping to
# separate players who always appear together (teammate confounding).
#
# For Opta-specific SPM functions, see spm_opta.R


#' Create SPM prior vector for RAPM
#'
#' Creates a prior vector aligned with RAPM player IDs.
#'
#' @param spm_predictions Named vector or data frame of SPM predictions
#' @param player_mapping Data frame with player_id and player_name
#' @param default_prior Value for players without SPM prediction
#'
#' @return Named vector of priors (keyed by player_id)
#' @keywords internal
create_spm_prior <- function(spm_predictions, player_mapping, default_prior = 0) {
  # Handle data frame input
  if (is.data.frame(spm_predictions)) {
    if ("spm" %in% names(spm_predictions) && "player_name" %in% names(spm_predictions)) {
      spm_predictions <- stats::setNames(spm_predictions$spm, spm_predictions$player_name)
    } else {
      cli::cli_abort(c(
        "{.arg spm_predictions} data frame must have {.field spm} and {.field player_name} columns.",
        "i" = "Use {.fn calculate_spm_ratings} to generate SPM predictions."
      ))
    }
  }

  # Create lookup from player_name to player_id
  name_to_id <- stats::setNames(
    player_mapping$player_id,
    player_mapping$player_name
  )

  # Initialize prior vector for all players in mapping
  all_player_ids <- unique(player_mapping$player_id)
  prior <- stats::setNames(rep(default_prior, length(all_player_ids)), all_player_ids)

  # Fill in SPM predictions where available (vectorized)
  common_names <- intersect(names(spm_predictions), names(name_to_id))
  matched_ids <- name_to_id[common_names]
  valid <- matched_ids %in% names(prior)
  if (any(valid)) {
    prior[matched_ids[valid]] <- spm_predictions[common_names[valid]]
  }
  matched <- sum(valid)

  progress_msg(sprintf("SPM prior: matched %d of %d players", matched, length(spm_predictions)))

  prior
}


#' Build prior vector for RAPM from SPM predictions
#'
#' Creates a named prior vector aligned with player IDs from SPM rating predictions.
#' This is a vectorized helper used by xRAPM and seasonal ratings to build priors
#' from SPM predictions without manual for-loops.
#'
#' @param spm_data Data frame with player_name and the SPM column to use
#' @param spm_col Name of the column containing SPM predictions
#' @param player_mapping Data frame with player_id and player_name from RAPM
#' @param default Value for players without SPM prediction (default 0)
#'
#' @return Named vector of priors keyed by player_id
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' offense_prior <- build_prior_vector(
#'   spm_data = offense_spm_ratings,
#'   spm_col = "offense_spm",
#'   player_mapping = rapm_data$player_mapping
#' )
#' }
build_prior_vector <- function(spm_data, spm_col, player_mapping, default = 0) {
  # Initialize prior vector for all players in mapping
  all_player_ids <- unique(player_mapping$player_id)
  prior <- stats::setNames(rep(default, length(all_player_ids)), all_player_ids)

  # Try direct player_id matching (Opta pipeline -- both use same numeric ID)
  join_method <- "player_name"
  if ("player_id" %in% names(spm_data) &&
      any(as.character(spm_data$player_id) %in% as.character(all_player_ids))) {
    join_method <- "player_id"
    spm_lookup <- stats::setNames(spm_data[[spm_col]], as.character(spm_data$player_id))
    matched_ids <- intersect(names(spm_lookup), names(prior))
    prior[matched_ids] <- spm_lookup[matched_ids]
  } else {
    # Name-based matching fallback (FBref pipeline -- different ID systems)
    name_to_id <- stats::setNames(
      player_mapping$player_id,
      player_mapping$player_name
    )
    matched_names <- intersect(spm_data$player_name, names(name_to_id))
    if (length(matched_names) > 0) {
      matched_ids <- name_to_id[matched_names]
      spm_idx <- match(matched_names, spm_data$player_name)
      valid <- !is.na(spm_idx) & matched_ids %in% names(prior)
      if (any(valid)) {
        prior[matched_ids[valid]] <- spm_data[[spm_col]][spm_idx[valid]]
      }
    }
  }

  n_matched <- sum(prior != default)
  progress_msg(sprintf("Prior '%s': matched %d of %d players [via %s]",
                       spm_col, n_matched, nrow(spm_data), join_method))

  prior
}


#' Prepare SPM regression data
#'
#' Joins player features with RAPM ratings for SPM model fitting.
#'
#' @param player_features Data frame from create_player_feature_matrix
#' @param rapm_ratings Data frame from extract_rapm_coefficients
#'
#' @return Data frame ready for SPM regression
#' @keywords internal
prepare_spm_regression_data <- function(player_features, rapm_ratings) {
  # Match on player name or ID
  if ("player_id" %in% names(player_features) && "player_id" %in% names(rapm_ratings)) {
    rapm_dt <- data.table::as.data.table(rapm_ratings[, c("player_id", "rapm"), drop = FALSE])
    data <- data.table::as.data.table(player_features)[rapm_dt, on = "player_id", nomatch = NULL]
    data.table::setDF(data)
  } else if ("player_name" %in% names(player_features) && "player_name" %in% names(rapm_ratings)) {
    rapm_dt <- data.table::as.data.table(rapm_ratings[, c("player_name", "rapm"), drop = FALSE])
    data <- data.table::as.data.table(player_features)[rapm_dt, on = "player_name", nomatch = NULL]
    data.table::setDF(data)
  } else {
    cli::cli_abort(c(
      "Cannot match {.arg player_features} and {.arg rapm_ratings}.",
      "x" = "No common ID column found (expected {.field player_id} or {.field player_name})."
    ))
  }

  data
}


#' Fit SPM model
#'
#' Fits an elastic net model predicting RAPM from box score statistics.
#' Weights observations by minutes played (sqrt transform) by default to reduce
#' influence of noisy low-minute players whose RAPM and per-90 stats are unreliable.
#'
#' @param data Data frame from prepare_spm_regression_data or aggregate_opta_stats
#'   joined with RAPM ratings
#' @param predictor_cols Character vector of predictor column names
#' @param alpha Elastic net mixing (0=ridge, 1=lasso, default 0.5)
#' @param nfolds Number of CV folds
#' @param weight_by_minutes Whether to weight observations by total_minutes (default TRUE).
#'   Reduces influence of noisy low-minute estimates on model coefficients.
#' @param weight_transform How to transform minutes for weighting:
#'   "sqrt" (default) - square root of minutes (moderate weighting)
#'   "linear" - raw minutes (strong weighting toward high-minute players)
#'   "log" - log of minutes (gentle weighting)
#'   "none" - equal weights
#' @param lower_limits,upper_limits Optional sign constraints on glmnet
#'   coefficients. Accepts a scalar (applied to all predictors), an unnamed
#'   numeric vector of length `ncol(X)`, or a named numeric vector keyed by
#'   predictor name (unmatched predictors default to `-Inf`/`Inf`). Use to
#'   enforce directional priors (e.g. negative defensive-tackle coefficient).
#'   `NULL` (default) = unconstrained.
#'
#' @return Fitted glmnet model with metadata
#' @family spm
#' @export
fit_spm_model <- function(data, predictor_cols = NULL, alpha = 0.5, nfolds = 10,
                          weight_by_minutes = TRUE, weight_transform = "sqrt",
                          lower_limits = NULL, upper_limits = NULL) {
  # Validate input
  validate_dataframe(data, required_cols = "rapm", arg_name = "data")

  # Default predictors: per-90 stats that predict impact
  if (is.null(predictor_cols)) {
    # Try _p90 columns first, then _p100 for backward compatibility
    predictor_cols <- names(data)[grepl("_p90$", names(data))]
    if (length(predictor_cols) == 0) {
      predictor_cols <- names(data)[grepl("_p100$", names(data))]
    }
  }

  available_cols <- intersect(predictor_cols, names(data))
  if (length(available_cols) == 0) {
    cli::cli_abort(c(
      "No valid predictor columns found in {.arg data}.",
      "i" = "Columns should end with '_p90' or '_p100'.",
      "i" = "Use {.fn aggregate_opta_stats} to generate predictor columns."
    ))
  }

  # Prepare data
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data$rapm

  # Calculate weights based on minutes played
  weights <- NULL
  if (weight_by_minutes && "total_minutes" %in% names(data)) {
    mins <- data$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      "none" = rep(1, length(mins)),
      sqrt(mins)  # default to sqrt
    )
    # Normalize weights to sum to n (so scale is comparable to unweighted)
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]
  if (!is.null(weights)) {
    weights <- weights[complete_idx]
  }

  progress_msg(paste("Fitting SPM model with", ncol(X), "predictors on", nrow(X), "players"))
  if (!is.null(weights)) {
    progress_msg(sprintf("  Weighting by minutes (%s transform)", weight_transform))
  }

  # Fit cross-validated elastic net.
  # lower_limits / upper_limits accept either:
  #   - a single scalar applied to every coefficient
  #   - a named numeric vector indexed by predictor names (used to enforce
  #     directional sign constraints -- e.g., tackles_won_p90 must have a
  #     non-positive coefficient because more tackles won = better defense
  #     in the negative-good defense convention)
  # Default (NULL) = unconstrained (-Inf to +Inf), matching glmnet defaults.
  resolve_limits <- function(lim, default) {
    if (is.null(lim)) return(rep(default, ncol(X)))
    if (length(lim) == 1) return(rep(lim, ncol(X)))
    if (is.null(names(lim))) {
      if (length(lim) != ncol(X)) {
        cli::cli_abort("Unnamed limits vector must have length = ncol(X) ({ncol(X)})")
      }
      return(lim)
    }
    out <- rep(default, ncol(X))
    matched <- intersect(names(lim), colnames(X))
    out[match(matched, colnames(X))] <- lim[matched]
    out
  }
  lower_vec <- resolve_limits(lower_limits, -Inf)
  upper_vec <- resolve_limits(upper_limits,  Inf)

  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    weights = weights,
    alpha = alpha,
    standardize = TRUE,
    nfolds = nfolds,
    type.measure = "mse",
    lower.limits = lower_vec,
    upper.limits = upper_vec
  )

  # Store feature SDs for standardised importance (glmnet standardize=TRUE
  # returns coefficients on original scale; multiply by SD for comparison)
  feature_sds <- apply(X, 2, stats::sd, na.rm = TRUE)
  feature_sds[feature_sds == 0 | is.na(feature_sds)] <- 1

  # Add metadata
  cv_fit$panna_metadata <- list(
    type = "spm",
    alpha = alpha,
    predictor_cols = available_cols,
    feature_sds = feature_sds,
    n_observations = length(y),
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    weight_by_minutes = weight_by_minutes,
    weight_transform = if (weight_by_minutes) weight_transform else "none"
  )

  # Calculate in-sample R-squared using actual predictions
  lambda_min <- cv_fit$lambda.min
  y_pred <- as.vector(stats::predict(cv_fit, newx = X, s = lambda_min))

  if (!is.null(weights)) {
    # Weighted R-squared for weighted models
    w <- weights / sum(weights)
    y_mean_w <- sum(w * y)
    ss_res <- sum(weights * (y - y_pred)^2)
    ss_tot <- sum(weights * (y - y_mean_w)^2)
    r_squared <- 1 - ss_res / ss_tot
    progress_msg(sprintf("SPM fit complete. R-squared: %.3f (weighted in-sample)", r_squared))
  } else {
    # Unweighted R-squared
    ss_res <- sum((y - y_pred)^2)
    ss_tot <- sum((y - mean(y))^2)
    r_squared <- 1 - ss_res / ss_tot
    progress_msg(sprintf("SPM fit complete. R-squared: %.3f (in-sample)", r_squared))
  }

  cv_fit
}


#' Fit SPM model using XGBoost
#'
#' Fits an XGBoost model predicting RAPM from box score statistics.
#' Uses xgb.cv to find optimal number of boosting rounds via early stopping.
#'
#' @param data Data frame from prepare_spm_regression_data or aggregate_opta_stats
#'   joined with RAPM ratings
#' @param predictor_cols Character vector of predictor column names
#' @param nfolds Number of CV folds (default 10)
#' @param max_depth Maximum tree depth (default 4)
#' @param eta Learning rate (default 0.1)
#' @param subsample Row subsampling ratio (default 0.8)
#' @param colsample_bytree Column subsampling ratio (default 0.8)
#' @param nrounds Maximum boosting rounds (default 500, uses early stopping)
#' @param early_stopping_rounds Stop if no improvement for this many rounds (default 20)
#' @param weight_by_minutes Whether to weight observations by total_minutes (default TRUE)
#' @param weight_transform How to transform minutes for weighting: "sqrt", "linear", "log"
#' @param verbose Print progress (0=silent, 1=performance, 2=details)
#'
#' @return List with xgb model, cv results, and metadata
#' @keywords internal
fit_spm_xgb <- function(data, predictor_cols = NULL, nfolds = 10,
                         max_depth = 4, eta = 0.1,
                         subsample = 0.8, colsample_bytree = 0.8,
                         nrounds = 500, early_stopping_rounds = 20,
                         weight_by_minutes = TRUE, weight_transform = "sqrt",
                         verbose = 1) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required. Install with: {.code install.packages('xgboost')}")
  }

  # Default predictors: per-90 stats — BOTH suffix spellings (`_p90` box-score,
  # `_per90` xMetrics), matching fit_spm_opta's detector. The old `_p90$`-only
  # default left the XGBoost half of the 50/50 SPM blend xMetrics-blind even
  # after the glmnet half was fixed (panna 2026-07-07 review finding). NB
  # 05_spm.R now passes predictor_cols explicitly for exact glmnet/xgb parity;
  # this default is the safety net for other callers.
  if (is.null(predictor_cols)) {
    predictor_cols <- names(data)[grepl("_p90$|_per90$", names(data))]
    if (length(predictor_cols) == 0) {
      predictor_cols <- names(data)[grepl("_p100$", names(data))]
    }
  }

  available_cols <- intersect(predictor_cols, names(data))
  if (length(available_cols) == 0) {
    cli::cli_abort(c(
      "No valid predictor columns found in {.arg data}.",
      "i" = "Columns should end with '_p90' or '_p100'.",
      "i" = "Use {.fn aggregate_opta_stats} to generate predictor columns."
    ))
  }

  # Prepare data
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data$rapm

  # Calculate weights
  weights <- NULL
  if (weight_by_minutes && "total_minutes" %in% names(data)) {
    mins <- data$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      "none" = rep(1, length(mins)),
      sqrt(mins)
    )
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]
  if (!is.null(weights)) {
    weights <- weights[complete_idx]
  }

  progress_msg(sprintf("Fitting XGBoost SPM with %d predictors on %d players", ncol(X), nrow(X)))

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = weights)

  # XGBoost parameters
  params <- list(
    objective = "reg:squarederror",
    max_depth = max_depth,
    eta = eta,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    eval_metric = "rmse"
  )

  # Cross-validation to find optimal nrounds
  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nfold = nfolds,
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose,
    print_every_n = 50
  )

  # Extract best iteration (handle different xgboost versions)
  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || length(best_nrounds) == 0) {
    # Fallback: find iteration with minimum test RMSE
    eval_log <- cv_result$evaluation_log
    best_nrounds <- which.min(eval_log$test_rmse_mean)
  }
  best_rmse <- cv_result$evaluation_log$test_rmse_mean[best_nrounds]

  progress_msg(sprintf("XGBoost CV: best iteration = %d, CV RMSE = %.4f", best_nrounds, best_rmse))

  # Fit final model with optimal nrounds
  final_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Calculate in-sample metrics
  y_pred <- stats::predict(final_model, dtrain)
  if (!is.null(weights)) {
    w <- weights / sum(weights)
    y_mean_w <- sum(w * y)
    ss_res <- sum(weights * (y - y_pred)^2)
    ss_tot <- sum(weights * (y - y_mean_w)^2)
    train_rmse <- sqrt(sum(weights * (y - y_pred)^2) / sum(weights))
  } else {
    ss_res <- sum((y - y_pred)^2)
    ss_tot <- sum((y - mean(y))^2)
    train_rmse <- sqrt(mean((y - y_pred)^2))
  }
  r_squared <- 1 - ss_res / ss_tot

  progress_msg(sprintf("XGBoost fit complete. Train RMSE: %.4f, CV RMSE: %.4f, R^2: %.3f",
                       train_rmse, best_rmse, r_squared))

  # Get feature importance
  importance <- xgboost::xgb.importance(
    feature_names = available_cols,
    model = final_model
  )

  # Return results
  result <- list(
    model = final_model,
    cv_result = cv_result,
    importance = importance,
    best_nrounds = best_nrounds,
    train_rmse = train_rmse,
    best_cv_rmse = best_rmse,
    r_squared = r_squared,
    panna_metadata = list(
      type = "spm_xgb",
      predictor_cols = available_cols,
      n_observations = length(y),
      params = params,
      best_nrounds = best_nrounds,
      weight_by_minutes = weight_by_minutes,
      weight_transform = if (weight_by_minutes) weight_transform else "none"
    )
  )

  class(result) <- c("spm_xgb", "list")
  result
}


#' Calculate SPM ratings using XGBoost model
#'
#' @param player_features Data frame of player features
#' @param spm_xgb_model Fitted XGBoost SPM model from fit_spm_xgb
#'
#' @return Data frame with SPM ratings
#' @family spm
#' @export
calculate_spm_ratings_xgb <- function(player_features, spm_xgb_model) {
  predictor_cols <- spm_xgb_model$panna_metadata$predictor_cols

  # Ensure data.frame (data.table subsetting interprets predictor_cols as column name)
  player_features <- as.data.frame(player_features)

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- stats::predict(spm_xgb_model$model, X)

  # Create output
  keep_cols <- intersect(c("player_id", "player_name", "n_matches", "total_minutes"),
                         names(player_features))
  result <- player_features[, keep_cols, drop = FALSE]
  result$spm <- spm_pred
  result <- result[order(-result$spm), ]

  result
}


#' Calculate blended SPM ratings from Elastic Net and XGBoost
#'
#' Combines predictions from both model types with configurable weighting.
#' The blend can improve robustness by capturing both linear (Elastic Net)
#' and non-linear (XGBoost) relationships between box scores and RAPM.
#'
#' @param player_features Data frame of player features
#' @param model_glmnet Fitted Elastic Net SPM model from fit_spm_model
#' @param model_xgb Fitted XGBoost SPM model from fit_spm_xgb
#' @param weight_glmnet Weight for Elastic Net predictions (default 0.5)
#'
#' @return Data frame with blended SPM ratings plus individual model predictions
#' @keywords internal
calculate_spm_blend <- function(player_features, model_glmnet, model_xgb,
                                weight_glmnet = 0.5) {
  # Get predictions from each model
  spm_glmnet <- calculate_spm_ratings(player_features, model_glmnet)
  spm_xgb <- calculate_spm_ratings_xgb(player_features, model_xgb)

  # Blend predictions
  names(spm_glmnet)[names(spm_glmnet) == "spm"] <- "spm_glmnet"
  xgb_df <- spm_xgb[, c("player_id", "spm"), drop = FALSE]
  names(xgb_df)[names(xgb_df) == "spm"] <- "spm_xgb"
  result <- data.table::as.data.table(spm_glmnet)[data.table::as.data.table(xgb_df), on = "player_id", nomatch = NULL]
  data.table::setDF(result)
  result$spm <- weight_glmnet * result$spm_glmnet + (1 - weight_glmnet) * result$spm_xgb
  result <- result[order(-result$spm), ]

  result
}


#' Extract SPM coefficients
#'
#' Gets feature weights from fitted SPM model.
#'
#' @param model Fitted SPM model from fit_spm_model
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Named vector of coefficients
#' @keywords internal
extract_spm_coefficients <- function(model, lambda = "min") {
  lambda_val <- if (lambda == "min") model$lambda.min else model$lambda.1se

  coefs <- stats::coef(model, s = lambda_val)
  coef_vec <- as.vector(coefs)
  names(coef_vec) <- rownames(coefs)

  # Remove intercept for display
  coef_vec
}


#' Export SPM coefficients to a CSV for live per-match scoring (panna#173)
#'
#' Writes a \code{stat_name, beta, sd} CSV in the same shape as
#' \code{inst/extdata/blend_{psr,osr,dsr}_coefficients.csv}, so
#' \code{scripts/build-stat-value-coefficients.mjs} (inthegame-blog) can pick
#' it up with the same pattern already used for PSR/OSR/DSR.
#'
#' Unlike PSR/OSR/DSR, whose training features are TEAM-SUMS (see
#' \code{\link{calculate_psr}}'s \code{coef_df} docs, panna#167) and therefore
#' need an exported \code{sd} to standardize a served per-player raw value,
#' \code{fit_spm_model()} trains directly on individual PLAYER-level per-90
#' features via \code{glmnet::cv.glmnet(x = X, ..., standardize = TRUE)} --
#' glmnet un-does its own internal standardization before \code{coef()}
#' returns, so \code{\link{extract_spm_coefficients}}'s output is already on
#' the raw per-player feature scale. \code{sd} is written as 1 for every row
#' (a harmless no-op divisor) purely for schema parity with the other
#' coefficient files, not because scoring needs it. Verified 2026-07-23:
#' hand-scoring \code{raw_value * beta} (summed + intercept) against this
#' export reproduces \code{\link{calculate_spm_ratings}}'s own predictions to
#' floating-point precision (~1e-16) on the full production player table.
#'
#' @param model Fitted SPM glmnet model (from \code{\link{fit_spm_model}} /
#'   \code{\link{fit_spm_opta}}), e.g. \code{spm_glmnet},
#'   \code{offense_spm_glmnet}, or \code{defense_spm_glmnet} from a saved
#'   \code{05_spm.rds}.
#' @param out_path File path to write the CSV to.
#' @param lambda Which lambda to use ("min" or "1se"), passed through to
#'   \code{\link{extract_spm_coefficients}}.
#'
#' @return Invisibly, the data frame that was written.
#' @keywords internal
export_spm_coefficients_csv <- function(model, out_path, lambda = "min") {
  coef_vec <- extract_spm_coefficients(model, lambda = lambda)
  coef_vec <- coef_vec[names(coef_vec) != "(Intercept)"]
  # Same convention as calculate_psr()/build-stat-value-coefficients.mjs:
  # drop zero-beta (glmnet-shrunk-out) features -- nothing to score with them.
  coef_vec <- coef_vec[coef_vec != 0]

  out <- data.frame(
    stat_name = names(coef_vec),
    beta = as.numeric(coef_vec),
    sd = 1,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, out_path, row.names = FALSE)
  invisible(out)
}


#' Calculate SPM ratings for all players
#'
#' Applies SPM model to predict RAPM for all players with features.
#'
#' @param player_features Data frame of player features
#' @param spm_model Fitted SPM model
#' @param lambda Which lambda to use
#'
#' @return Data frame with SPM ratings
#' @family spm
#' @export
calculate_spm_ratings <- function(player_features, spm_model, lambda = "min") {
  predictor_cols <- spm_model$panna_metadata$predictor_cols
  lambda_val <- if (lambda == "min") spm_model$lambda.min else spm_model$lambda.1se

  # Ensure data.frame (data.table subsetting interprets predictor_cols as column name)
  player_features <- as.data.frame(player_features)

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])

  # Handle missing values
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- as.vector(stats::predict(spm_model, newx = X, s = lambda_val))

  # Create output data frame
  keep_cols <- intersect(c("player_id", "player_name", "n_games", "total_minutes"),
                         names(player_features))
  result <- player_features[, keep_cols, drop = FALSE]
  result$spm <- spm_pred
  result <- result[order(-result$spm), ]

  result
}


#' Calculate offensive SPM
#'
#' Fits SPM model for offensive contribution only.
#'
#' @param data SPM regression data
#' @param offensive_cols Offensive predictor columns
#' @param alpha Elastic net mixing
#'
#' @return Fitted model for offensive SPM
#' @keywords internal
calculate_offensive_spm <- function(data, offensive_cols = NULL, alpha = 0.5) {
  if (is.null(offensive_cols)) {
    # Use _p90 naming (current), fall back to _p100 for backward compatibility
    suffix <- if (any(grepl("_p90$", names(data)))) "_p90" else "_p100"
    offensive_cols <- paste0(c("npxg", "xg", "shots", "shots_on_target",
                               "assists", "xa", "sca", "gca",
                               "progressive_passes", "progressive_carries", "carries"), suffix)
  }

  fit_spm_model(data, predictor_cols = offensive_cols, alpha = alpha)
}


#' Calculate defensive SPM
#'
#' Fits SPM model for defensive contribution only.
#'
#' @param data SPM regression data
#' @param defensive_cols Defensive predictor columns
#' @param alpha Elastic net mixing
#'
#' @return Fitted model for defensive SPM
#' @keywords internal
calculate_defensive_spm <- function(data, defensive_cols = NULL, alpha = 0.5) {
  if (is.null(defensive_cols)) {
    suffix <- if (any(grepl("_p90$", names(data)))) "_p90" else "_p100"
    defensive_cols <- paste0(c("tackles", "interceptions", "blocks",
                               "tackles_won", "clearances"), suffix)
  }

  fit_spm_model(data, predictor_cols = defensive_cols, alpha = alpha)
}


#' Validate SPM prediction accuracy
#'
#' Assesses how well SPM predicts RAPM. Supports weighted metrics to match
#' weighted model fitting - we care more about accuracy for high-minute players
#' whose RAPM estimates are more reliable.
#'
#' @param spm_ratings Data frame with SPM predictions (must include total_minutes for weighting)
#' @param rapm_ratings Data frame with actual RAPM
#' @param weight_by_minutes Whether to weight metrics by minutes (default TRUE)
#' @param weight_transform Transform for weights: "sqrt" (default), "linear", "log"
#'
#' @return List with validation metrics (both weighted and unweighted)
#' @keywords internal
validate_spm_prediction <- function(spm_ratings, rapm_ratings,
                                     weight_by_minutes = TRUE,
                                     weight_transform = "sqrt") {
  # Join predictions with actuals
  # Find common join columns that exist in BOTH dataframes
  possible_keys <- c("player_id", "player_name")
  join_cols <- intersect(intersect(names(spm_ratings), names(rapm_ratings)), possible_keys)

  if (length(join_cols) == 0) {
    cli::cli_warn(c(
      "No common join columns found.",
      "i" = "Expected {.field player_id} or {.field player_name} in both data frames."
    ))
    return(NULL)
  }

  rapm_keep <- c(join_cols, "rapm")
  rapm_dt <- data.table::as.data.table(rapm_ratings[, rapm_keep, drop = FALSE])
  comparison <- data.table::as.data.table(spm_ratings)[rapm_dt, on = join_cols, nomatch = NULL]
  data.table::setDF(comparison)

  if (nrow(comparison) == 0) {
    cli::cli_warn("No matching players between SPM and RAPM ratings.")
    return(NULL)
  }

  # Calculate weights
  weights <- rep(1, nrow(comparison))
  if (weight_by_minutes && "total_minutes" %in% names(comparison)) {
    mins <- comparison$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      sqrt(mins)
    )
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Unweighted metrics
  residuals <- comparison$rapm - comparison$spm
  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((comparison$rapm - mean(comparison$rapm, na.rm = TRUE))^2, na.rm = TRUE)
  rmse_unweighted <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae_unweighted <- mean(abs(residuals), na.rm = TRUE)

  # Weighted metrics
  weighted_mean_rapm <- sum(weights * comparison$rapm, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  ss_res_w <- sum(weights * residuals^2, na.rm = TRUE)
  ss_tot_w <- sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE)
  rmse_weighted <- sqrt(sum(weights * residuals^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  mae_weighted <- sum(weights * abs(residuals), na.rm = TRUE) / sum(weights, na.rm = TRUE)

  # Weighted correlation (handle zero variance edge case)
  cov_w <- sum(weights * (comparison$spm - mean(comparison$spm)) *
               (comparison$rapm - weighted_mean_rapm), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  sd_spm_w <- sqrt(sum(weights * (comparison$spm - mean(comparison$spm))^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  sd_rapm_w <- sqrt(sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  cor_weighted <- if (sd_spm_w > 0 && sd_rapm_w > 0) cov_w / (sd_spm_w * sd_rapm_w) else NA_real_

  # Unweighted correlation (handle zero variance edge case)
  sd_spm <- stats::sd(comparison$spm, na.rm = TRUE)
  sd_rapm <- stats::sd(comparison$rapm, na.rm = TRUE)
  cor_unweighted <- if (sd_spm > 0 && sd_rapm > 0) {
    stats::cor(comparison$spm, comparison$rapm, use = "complete.obs")
  } else {
    NA_real_
  }

  # Handle edge case where R-squared calculation has zero total variance
  r_squared_unweighted <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  r_squared_weighted <- if (ss_tot_w > 0) 1 - ss_res_w / ss_tot_w else NA_real_

  metrics <- list(
    n_players = nrow(comparison),
    # Unweighted
    r_squared = r_squared_unweighted,
    rmse = rmse_unweighted,
    mae = mae_unweighted,
    correlation = cor_unweighted,
    # Weighted
    r_squared_weighted = r_squared_weighted,
    rmse_weighted = rmse_weighted,
    mae_weighted = mae_weighted,
    correlation_weighted = cor_weighted,
    # Data
    comparison = comparison
  )

  progress_msg(sprintf("SPM validation: R^2 = %.3f (unweighted), R^2 = %.3f (weighted by %s mins)",
                       r_squared_unweighted, r_squared_weighted, weight_transform))

  metrics
}


#' Get top SPM feature importance
#'
#' Identifies the most important features in the SPM model.
#'
#' @param model Fitted SPM model
#' @param n Number of top features to return
#' @param lambda Which lambda to use
#'
#' @return Data frame of top features by absolute coefficient
#' @family spm
#' @export
get_spm_feature_importance <- function(model, n = 10, lambda = "min") {
  coefs <- extract_spm_coefficients(model, lambda)

  # Remove intercept
  coefs <- coefs[names(coefs) != "(Intercept)"]

  # Get feature SDs for standardised importance
  feature_sds <- model$panna_metadata$feature_sds

  importance <- data.frame(
    feature = names(coefs),
    coefficient = as.vector(coefs),
    abs_coef = abs(as.vector(coefs)),
    stringsAsFactors = FALSE
  )

  # Standardised importance: |beta * sd| = effect of 1-SD change
  if (!is.null(feature_sds)) {
    sd_vals <- feature_sds[importance$feature]
    sd_vals[is.na(sd_vals)] <- 1
    importance$sd <- as.numeric(sd_vals)
    importance$std_importance <- abs(importance$coefficient) * importance$sd
  } else {
    cli::cli_inform("feature_sds not found in model metadata; std_importance uses raw |coefficient|")
    importance$std_importance <- importance$abs_coef
  }

  importance <- importance[importance$coefficient != 0, ]
  importance <- importance[order(-importance$std_importance), ]
  importance <- head(importance, n)

  importance
}
