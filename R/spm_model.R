# SPM (Statistical Plus-Minus) model functions for panna package
#
# SPM predicts RAPM ratings from box score statistics.
# This serves as the Bayesian prior for RAPM estimation, helping to
# separate players who always appear together (teammate confounding).


#' Aggregate player statistics to per-90 rates
#'
#' Combines match-level statistics into per-90-minute rates for each player.
#' Handles varying column availability across different data sources.
#' Note: worldfootballR uses column names with suffixes like _expected, _sca, etc.
#'
#' @param stats_summary Summary stats data frame from process_all_data
#' @param stats_passing Passing stats data frame (optional)
#' @param stats_defense Defense stats data frame (optional)
#' @param stats_possession Possession stats data frame (optional)
#' @param min_minutes Minimum total minutes for inclusion
#'
#' @return Data frame with per-90 rates for each player
#' @export
aggregate_player_stats <- function(stats_summary,
                                    stats_passing = NULL,
                                    stats_defense = NULL,
                                    stats_possession = NULL,
                                    min_minutes = 450) {
  if (is.null(stats_summary) || nrow(stats_summary) == 0) {
    warning("No summary stats provided")
    return(NULL)
  }

  progress_msg(sprintf("Aggregating %d player-match rows...", nrow(stats_summary)))

  # Define columns to sum
  col_mapping <- c(
    total_minutes = "min",
    goals = "gls", assists = "ast", shots = "sh", shots_on_target = "so_t",
    tackles = "tkl", interceptions = "int", blocks = "blocks", touches = "touches",
    xg = "x_g_expected", npxg = "npx_g_expected", xa = "x_ag_expected",
    sca = "sca_sca", gca = "gca_sca",
    carries = "carries_carries", progressive_carries = "prg_c_carries",
    take_ons_att = "att_take_ons", take_ons_succ = "succ_take_ons",
    progressive_passes = "prg_p_passes"
  )

  # Find which columns exist
  existing_cols <- col_mapping[col_mapping %in% names(stats_summary)]

  # Simple aggregation - sum each column by player
  player_stats <- stats::aggregate(
    stats_summary[, existing_cols, drop = FALSE],
    by = list(player_name = stats_summary$player_name),
    FUN = function(x) sum(x, na.rm = TRUE)
  )

  # Also count matches
  match_counts <- stats::aggregate(
    stats_summary$player_name,
    by = list(player_name = stats_summary$player_name),
    FUN = length
  )
  names(match_counts)[2] <- "n_matches"

  player_stats <- merge(player_stats, match_counts, by = "player_name")

  # Rename columns
  for (new_name in names(existing_cols)) {
    old_name <- existing_cols[new_name]
    if (old_name %in% names(player_stats)) {
      names(player_stats)[names(player_stats) == old_name] <- new_name
    }
  }

  # Add zero columns for any missing stats
  missing_cols <- setdiff(names(col_mapping), names(player_stats))
  for (col in missing_cols) {
    player_stats[[col]] <- 0
  }

  # Filter by min minutes
  player_stats <- player_stats[player_stats$total_minutes >= min_minutes, ]

  # Calculate per-90 rates (base R)
  mins_per_90 <- player_stats$total_minutes / 90
  player_stats$mins_per_90 <- mins_per_90
  player_stats$goals_p90 <- player_stats$goals / mins_per_90
  player_stats$assists_p90 <- player_stats$assists / mins_per_90
  player_stats$shots_p90 <- player_stats$shots / mins_per_90
  player_stats$shots_on_target_p90 <- player_stats$shots_on_target / mins_per_90
  player_stats$tackles_p90 <- player_stats$tackles / mins_per_90
  player_stats$interceptions_p90 <- player_stats$interceptions / mins_per_90
  player_stats$blocks_p90 <- player_stats$blocks / mins_per_90
  player_stats$touches_p90 <- player_stats$touches / mins_per_90
  player_stats$xg_p90 <- player_stats$xg / mins_per_90
  player_stats$npxg_p90 <- player_stats$npxg / mins_per_90
  player_stats$xa_p90 <- player_stats$xa / mins_per_90
  player_stats$sca_p90 <- player_stats$sca / mins_per_90
  player_stats$gca_p90 <- player_stats$gca / mins_per_90
  player_stats$carries_p90 <- player_stats$carries / mins_per_90
  player_stats$progressive_carries_p90 <- player_stats$progressive_carries / mins_per_90
  player_stats$take_ons_p90 <- player_stats$take_ons_succ / mins_per_90
  player_stats$progressive_passes_p90 <- player_stats$progressive_passes / mins_per_90
  player_stats$npxg_plus_xa_p90 <- player_stats$npxg_p90 + player_stats$xa_p90

  # Add passing stats if available
  if (!is.null(stats_passing) && nrow(stats_passing) > 0) {
    pass_cols <- c("kp", "final_third", "ppa")
    pass_cols <- pass_cols[pass_cols %in% names(stats_passing)]

    if (length(pass_cols) > 0) {
      pass_agg <- stats::aggregate(
        stats_passing[, pass_cols, drop = FALSE],
        by = list(player_name = stats_passing$player_name),
        FUN = function(x) sum(x, na.rm = TRUE)
      )
      names(pass_agg)[names(pass_agg) == "kp"] <- "key_passes"
      names(pass_agg)[names(pass_agg) == "final_third"] <- "final_third_passes"
      names(pass_agg)[names(pass_agg) == "ppa"] <- "passes_into_box"

      player_stats <- merge(player_stats, pass_agg, by = "player_name", all.x = TRUE)

      if ("key_passes" %in% names(player_stats)) {
        player_stats$key_passes_p90 <- player_stats$key_passes / player_stats$mins_per_90
      }
      if ("final_third_passes" %in% names(player_stats)) {
        player_stats$final_third_passes_p90 <- player_stats$final_third_passes / player_stats$mins_per_90
      }
      if ("passes_into_box" %in% names(player_stats)) {
        player_stats$passes_into_box_p90 <- player_stats$passes_into_box / player_stats$mins_per_90
      }
    }
  }

  # Add defense stats if available
  if (!is.null(stats_defense) && nrow(stats_defense) > 0) {
    def_cols <- c("clr", "tkl_w_tackles")
    def_cols <- def_cols[def_cols %in% names(stats_defense)]

    if (length(def_cols) > 0) {
      def_agg <- stats::aggregate(
        stats_defense[, def_cols, drop = FALSE],
        by = list(player_name = stats_defense$player_name),
        FUN = function(x) sum(x, na.rm = TRUE)
      )
      names(def_agg)[names(def_agg) == "clr"] <- "clearances"
      names(def_agg)[names(def_agg) == "tkl_w_tackles"] <- "tackles_won"

      player_stats <- merge(player_stats, def_agg, by = "player_name", all.x = TRUE)

      if ("clearances" %in% names(player_stats)) {
        player_stats$clearances_p90 <- player_stats$clearances / player_stats$mins_per_90
      }
      if ("tackles_won" %in% names(player_stats)) {
        player_stats$tackles_won_p90 <- player_stats$tackles_won / player_stats$mins_per_90
      }
    }
  }

  progress_msg(sprintf("Aggregated stats for %d players with %d features",
                       nrow(player_stats), ncol(player_stats)))

  player_stats
}


#' Create SPM prior vector for RAPM
#'
#' Creates a prior vector aligned with RAPM player IDs.
#'
#' @param spm_predictions Named vector or data frame of SPM predictions
#' @param player_mapping Data frame with player_id and player_name
#' @param default_prior Value for players without SPM prediction
#'
#' @return Named vector of priors (keyed by player_id)
#' @export
create_spm_prior <- function(spm_predictions, player_mapping, default_prior = 0) {
  # Handle data frame input
  if (is.data.frame(spm_predictions)) {
    if ("spm" %in% names(spm_predictions) && "player_name" %in% names(spm_predictions)) {
      spm_predictions <- stats::setNames(spm_predictions$spm, spm_predictions$player_name)
    } else {
      stop("spm_predictions data frame must have 'spm' and 'player_name' columns")
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

  # Fill in SPM predictions where available
  matched <- 0
  for (player_name in names(spm_predictions)) {
    if (player_name %in% names(name_to_id)) {
      player_id <- name_to_id[player_name]
      if (player_id %in% names(prior)) {
        prior[player_id] <- spm_predictions[player_name]
        matched <- matched + 1
      }
    }
  }

  progress_msg(sprintf("SPM prior: matched %d of %d players", matched, length(spm_predictions)))

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
#' @export
prepare_spm_regression_data <- function(player_features, rapm_ratings) {
  # Match on player name or ID
  if ("player_id" %in% names(player_features) && "player_id" %in% names(rapm_ratings)) {
    data <- player_features %>%
      dplyr::inner_join(
        rapm_ratings %>% dplyr::select(player_id, rapm),
        by = "player_id"
      )
  } else if ("player_name" %in% names(player_features) && "player_name" %in% names(rapm_ratings)) {
    data <- player_features %>%
      dplyr::inner_join(
        rapm_ratings %>% dplyr::select(player_name, rapm),
        by = "player_name"
      )
  } else {
    stop("Cannot match player_features and rapm_ratings: no common ID column")
  }

  data
}


#' Fit SPM model
#'
#' Fits an elastic net model predicting RAPM from box score statistics.
#'
#' @param data Data frame from prepare_spm_regression_data or aggregate_player_stats
#'   joined with RAPM ratings
#' @param predictor_cols Character vector of predictor column names
#' @param alpha Elastic net mixing (0=ridge, 1=lasso, default 0.5)
#' @param nfolds Number of CV folds
#'
#' @return Fitted glmnet model with metadata
#' @export
fit_spm_model <- function(data, predictor_cols = NULL, alpha = 0.5, nfolds = 10) {
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
    stop("No valid predictor columns found")
  }

  # Prepare data
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data$rapm

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]

  progress_msg(paste("Fitting SPM model with", ncol(X), "predictors on", nrow(X), "players"))

  # Fit cross-validated elastic net
  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    alpha = alpha,
    standardize = TRUE,
    nfolds = nfolds,
    type.measure = "mse"
  )

  # Add metadata
  cv_fit$panna_metadata <- list(
    type = "spm",
    alpha = alpha,
    predictor_cols = available_cols,
    n_observations = length(y),
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se
  )

  progress_msg(paste("SPM fit complete. R-squared:",
                     round(1 - min(cv_fit$cvm) / var(y), 3)))

  cv_fit
}


#' Extract SPM coefficients
#'
#' Gets feature weights from fitted SPM model.
#'
#' @param model Fitted SPM model from fit_spm_model
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Named vector of coefficients
#' @export
extract_spm_coefficients <- function(model, lambda = "min") {
  lambda_val <- if (lambda == "min") model$lambda.min else model$lambda.1se

  coefs <- stats::coef(model, s = lambda_val)
  coef_vec <- as.vector(coefs)
  names(coef_vec) <- rownames(coefs)

  # Remove intercept for display
  coef_vec
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
#' @export
calculate_spm_ratings <- function(player_features, spm_model, lambda = "min") {
  predictor_cols <- spm_model$panna_metadata$predictor_cols
  lambda_val <- if (lambda == "min") spm_model$lambda.min else spm_model$lambda.1se

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])

  # Handle missing values
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- as.vector(stats::predict(spm_model, newx = X, s = lambda_val))

  # Create output data frame
  result <- player_features %>%
    dplyr::select(dplyr::any_of(c("player_id", "player_name", "n_games", "total_minutes"))) %>%
    dplyr::mutate(spm = spm_pred) %>%
    dplyr::arrange(dplyr::desc(.data$spm))

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
#' @export
calculate_offensive_spm <- function(data, offensive_cols = NULL, alpha = 0.5) {
  if (is.null(offensive_cols)) {
    offensive_cols <- c("npxG_p100", "xG_p100", "Sh_p100", "SoT_p100",
                        "Ast_p100", "xAG_p100", "SCA_p100", "GCA_p100",
                        "PrgP_p100", "PrgC_p100", "Carries_p100")
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
#' @export
calculate_defensive_spm <- function(data, defensive_cols = NULL, alpha = 0.5) {
  if (is.null(defensive_cols)) {
    defensive_cols <- c("Tkl_p100", "Int_p100", "Blocks_p100",
                        "TklWon_p100", "Clr_p100")
  }

  fit_spm_model(data, predictor_cols = defensive_cols, alpha = alpha)
}


#' Validate SPM prediction accuracy
#'
#' Assesses how well SPM predicts RAPM.
#'
#' @param spm_ratings Data frame with SPM predictions
#' @param rapm_ratings Data frame with actual RAPM
#'
#' @return List with validation metrics
#' @export
validate_spm_prediction <- function(spm_ratings, rapm_ratings) {
  # Join predictions with actuals
  comparison <- spm_ratings %>%
    dplyr::inner_join(
      rapm_ratings %>%
        dplyr::select(dplyr::any_of(c("player_id", "player_name")), rapm),
      by = intersect(names(spm_ratings), c("player_id", "player_name"))
    )

  if (nrow(comparison) == 0) {
    warning("No matching players between SPM and RAPM ratings")
    return(NULL)
  }

  # Calculate metrics
  residuals <- comparison$rapm - comparison$spm
  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((comparison$rapm - mean(comparison$rapm, na.rm = TRUE))^2, na.rm = TRUE)

  metrics <- list(
    n_players = nrow(comparison),
    r_squared = 1 - ss_res / ss_tot,
    rmse = sqrt(mean(residuals^2, na.rm = TRUE)),
    mae = mean(abs(residuals), na.rm = TRUE),
    correlation = stats::cor(comparison$spm, comparison$rapm, use = "complete.obs"),
    comparison = comparison
  )

  progress_msg(paste("SPM validation: R-squared =", round(metrics$r_squared, 3),
                     ", RMSE =", round(metrics$rmse, 4)))

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
#' @export
get_spm_feature_importance <- function(model, n = 10, lambda = "min") {
  coefs <- extract_spm_coefficients(model, lambda)

  # Remove intercept
  coefs <- coefs[names(coefs) != "(Intercept)"]

  # Sort by absolute value
  importance <- data.frame(
    feature = names(coefs),
    coefficient = as.vector(coefs),
    abs_coef = abs(as.vector(coefs)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(.data$coefficient != 0) %>%
    dplyr::arrange(dplyr::desc(.data$abs_coef)) %>%
    utils::head(n)

  importance
}
