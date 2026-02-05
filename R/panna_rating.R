# Panna rating calculation functions
#
# The panna rating combines single-season RAPM with SPM as a Bayesian prior.
# This provides stable, predictive player impact ratings.

#' Calculate panna rating
#'
#' Core panna rating calculation combining RAPM with SPM prior.
#' Formula: beta_panna = beta_diff + beta_spm
#' Where beta_diff is from: min ||y - X*beta||^2 + lambda * ||beta - beta_spm||^2
#'
#' @param rapm_data RAPM data from prepare_rapm_data
#' @param spm_ratings SPM ratings from calculate_spm_ratings
#' @param lambda_prior Regularization strength toward SPM prior
#' @param alpha Elastic net mixing (default 0 for ridge)
#'
#' @return List with panna ratings and model details
#' @export
calculate_panna_rating <- function(rapm_data, spm_ratings, lambda_prior = 1, alpha = 0) {
  X <- rapm_data$X
  y <- rapm_data$y

  # Remove NA responses
  valid_idx <- !is.na(y)
  X <- X[valid_idx, , drop = FALSE]
  y <- y[valid_idx]

  player_ids <- colnames(X)
  n_players <- length(player_ids)

  # Create prior vector from SPM
  spm_prior <- rep(0, n_players)
  names(spm_prior) <- player_ids

  # Match SPM ratings to player IDs (vectorized O(n) instead of O(n²) loop)
  if ("player_id" %in% names(spm_ratings)) {
    match_idx <- match(player_ids, spm_ratings$player_id)
    matched <- !is.na(match_idx)
    spm_prior[matched] <- spm_ratings$spm[match_idx[matched]]
  } else if ("player_name" %in% names(spm_ratings) &&
             !is.null(rapm_data$player_mapping)) {
    # Match via player mapping (vectorized)
    mapping_idx <- match(player_ids, rapm_data$player_mapping$player_id)
    player_names <- ifelse(is.na(mapping_idx), NA_character_,
                           rapm_data$player_mapping$player_name[mapping_idx])
    spm_idx <- match(player_names, spm_ratings$player_name)
    matched <- !is.na(spm_idx)
    spm_prior[matched] <- spm_ratings$spm[spm_idx[matched]]
  }

  # Transform regression to center on SPM prior
  # Adjusted response: y - X * spm_prior
  y_adjusted <- as.vector(y - X %*% spm_prior)

  # Fit ridge regression on adjusted response
  # lambda_prior controls shrinkage toward SPM: higher = more shrinkage to SPM
  progress_msg("Fitting panna model with SPM prior...")

  fit <- glmnet::glmnet(
    x = X,
    y = y_adjusted,
    alpha = alpha,
    lambda = lambda_prior,
    standardize = FALSE
  )

  # Extract deviation from prior (gamma = beta - spm_prior)
  # When fitting with single lambda, coef() returns that lambda's coefficients
  gamma <- as.vector(stats::coef(fit))[-1]

  # Final panna rating: deviation + prior
  panna_coef <- gamma + spm_prior

  # Create results data frame
  ratings <- data.frame(
    player_id = player_ids,
    panna = panna_coef,
    spm_prior = spm_prior,
    deviation = gamma,
    stringsAsFactors = FALSE
  )

  # Join with player names if available
  if (!is.null(rapm_data$player_mapping)) {
    ratings <- ratings %>%
      dplyr::left_join(
        rapm_data$player_mapping %>%
          dplyr::select(player_id, player_name),
        by = "player_id"
      )
  }

  ratings <- ratings %>%
    dplyr::arrange(dplyr::desc(.data$panna))

  result <- list(
    ratings = ratings,
    model = fit,
    lambda = lambda_prior,
    spm_prior = spm_prior,
    n_players = n_players,
    n_observations = length(y)
  )

  class(result) <- c("panna_model", "list")

  progress_msg(paste("Panna ratings calculated for", n_players, "players"))

  result
}


#' Fit end-to-end panna model
#'
#' Complete pipeline from splint data to panna ratings.
#'
#' @param splint_data Combined splint data
#' @param player_features Player feature matrix
#' @param min_minutes Minimum minutes for inclusion
#' @param lambda_prior Regularization strength
#' @param validate Whether to run validation
#'
#' @return List with panna model and all intermediate results
#' @export
fit_panna_model <- function(splint_data, player_features, min_minutes = 180,
                             lambda_prior = 1, validate = TRUE) {
  progress_msg("=== Fitting Panna Model ===")

  # Step 1: Prepare RAPM data
  progress_msg("Step 1: Preparing RAPM data...")
  rapm_data <- prepare_rapm_data(splint_data, min_minutes = min_minutes)

  # Step 2: Fit RAPM
  progress_msg("Step 2: Fitting RAPM...")
  rapm_model <- fit_rapm(rapm_data)
  rapm_ratings <- extract_rapm_coefficients(rapm_model)

  # Step 3: Fit SPM model
  progress_msg("Step 3: Fitting SPM model...")
  spm_data <- prepare_spm_regression_data(player_features, rapm_ratings)
  spm_model <- fit_spm_model(spm_data)
  spm_ratings <- calculate_spm_ratings(player_features, spm_model)

  # Step 4: Calculate panna ratings
  progress_msg("Step 4: Calculating panna ratings...")
  panna_result <- calculate_panna_rating(
    rapm_data = rapm_data,
    spm_ratings = spm_ratings,
    lambda_prior = lambda_prior
  )

  # Validation
  if (validate) {
    progress_msg("Step 5: Validating...")
    validation <- validate_panna_ratings(
      panna_result$ratings,
      rapm_ratings,
      spm_ratings
    )
    panna_result$validation <- validation
  }

  # Store all components
  panna_result$rapm_model <- rapm_model
  panna_result$rapm_ratings <- rapm_ratings
  panna_result$spm_model <- spm_model
  panna_result$spm_ratings <- spm_ratings
  panna_result$rapm_data <- rapm_data

  progress_msg("=== Panna Model Complete ===")

  panna_result
}


#' Get panna ratings from fitted model
#'
#' Gets the final panna ratings from a fitted panna model.
#'
#' @param panna_model Fitted panna model from fit_panna_model
#'
#' @return Data frame with player panna ratings
#' @export
get_panna_ratings <- function(panna_model) {
  panna_model$ratings
}


#' Rank players by panna rating
#'
#' Returns ranked list of players.
#'
#' @param ratings Data frame of panna ratings
#' @param top_n Number of top players to show
#' @param position Optional position filter
#'
#' @return Data frame of ranked players
#' @export
rank_players_panna <- function(ratings, top_n = NULL, position = NULL) {
  result <- ratings %>%
    dplyr::arrange(dplyr::desc(.data$panna)) %>%
    dplyr::mutate(rank = dplyr::row_number())

  if (!is.null(position) && "position" %in% names(result)) {
    result <- result %>%
      dplyr::filter(grepl(position, .data$position, ignore.case = TRUE))
  }

  if (!is.null(top_n)) {
    result <- head(result, top_n)
  }

  result
}


#' Compare panna, RAPM, and SPM ratings
#'
#' Analyzes differences between the three rating types.
#'
#' @param panna_ratings Data frame of panna ratings
#' @param rapm_ratings Data frame of RAPM ratings
#' @param spm_ratings Data frame of SPM ratings
#'
#' @return Data frame comparing all rating types
#' @export
compare_panna_rapm_spm <- function(panna_ratings, rapm_ratings, spm_ratings) {
  # Join all ratings
  id_col <- if ("player_id" %in% names(panna_ratings)) "player_id" else "player_name"

  comparison <- panna_ratings %>%
    dplyr::select(dplyr::any_of(c(id_col, "player_name", "panna"))) %>%
    dplyr::left_join(
      rapm_ratings %>% dplyr::select(dplyr::any_of(c(id_col, "rapm"))),
      by = id_col
    ) %>%
    dplyr::left_join(
      spm_ratings %>% dplyr::select(dplyr::any_of(c(id_col, "spm"))),
      by = id_col
    ) %>%
    dplyr::mutate(
      panna_vs_rapm = .data$panna - .data$rapm,
      panna_vs_spm = .data$panna - .data$spm,
      rapm_vs_spm = .data$rapm - .data$spm
    )

  comparison
}


#' Validate panna ratings
#'
#' Calculates validation metrics for panna ratings.
#'
#' @param panna_ratings Panna ratings
#' @param rapm_ratings RAPM ratings
#' @param spm_ratings SPM ratings
#'
#' @return List of validation metrics
#' @export
validate_panna_ratings <- function(panna_ratings, rapm_ratings, spm_ratings) {
  comparison <- compare_panna_rapm_spm(panna_ratings, rapm_ratings, spm_ratings)
  comparison <- comparison[stats::complete.cases(comparison), ]

  metrics <- list(
    n_players = nrow(comparison),
    panna_rapm_cor = stats::cor(comparison$panna, comparison$rapm),
    panna_spm_cor = stats::cor(comparison$panna, comparison$spm),
    rapm_spm_cor = stats::cor(comparison$rapm, comparison$spm),
    panna_mean = mean(comparison$panna),
    panna_sd = stats::sd(comparison$panna),
    rapm_mean = mean(comparison$rapm),
    rapm_sd = stats::sd(comparison$rapm),
    spm_mean = mean(comparison$spm),
    spm_sd = stats::sd(comparison$spm)
  )

  progress_msg(paste("Panna-RAPM correlation:", round(metrics$panna_rapm_cor, 3)))
  progress_msg(paste("Panna-SPM correlation:", round(metrics$panna_spm_cor, 3)))

  metrics
}


#' Generate prediction report
#'
#' Creates a formatted summary of panna ratings.
#'
#' @param panna_model Fitted panna model
#' @param top_n Number of top/bottom players to show
#'
#' @return Character string report
#' @export
generate_panna_report <- function(panna_model, top_n = 10) {
  ratings <- panna_model$ratings

  report <- c(
    "=======================================",
    "         PANNA RATINGS REPORT          ",
    "=======================================",
    "",
    paste("Total players rated:", nrow(ratings)),
    paste("Lambda (prior strength):", panna_model$lambda),
    "",
    "--- Top", top_n, "Players ---"
  )

  top_players <- head(ratings, top_n)
  for (i in seq_len(nrow(top_players))) {
    name <- if ("player_name" %in% names(top_players)) {
      top_players$player_name[i]
    } else {
      top_players$player_id[i]
    }
    report <- c(report, paste(i, name, round(top_players$panna[i], 3)))
  }

  report <- c(report, "",
              "--- Bottom", top_n, "Players ---")

  bottom_players <- tail(ratings, top_n)
  for (i in seq_len(nrow(bottom_players))) {
    name <- if ("player_name" %in% names(bottom_players)) {
      bottom_players$player_name[i]
    } else {
      bottom_players$player_id[i]
    }
    report <- c(report, paste(nrow(ratings) - top_n + i, name,
                              round(bottom_players$panna[i], 3)))
  }

  paste(report, collapse = "\n")
}


#' Validate predictive power
#'
#' Tests panna ratings against next season performance.
#'
#' @param panna_ratings Panna ratings from season N
#' @param next_season_rapm RAPM from season N+1
#'
#' @return Validation metrics
#' @export
validate_predictive_power <- function(panna_ratings, next_season_rapm) {
  # Match players
  id_col <- intersect(names(panna_ratings), names(next_season_rapm))
  id_col <- id_col[id_col %in% c("player_id", "player_name")][1]

  joined <- panna_ratings %>%
    dplyr::inner_join(
      next_season_rapm %>%
        dplyr::select(dplyr::any_of(c(id_col, "rapm"))) %>%
        dplyr::rename(next_rapm = .data$rapm),
      by = id_col
    )

  if (nrow(joined) < 10) {
    warning("Too few players matched for reliable validation")
    return(NULL)
  }

  metrics <- list(
    n_matched = nrow(joined),
    correlation = stats::cor(joined$panna, joined$next_rapm),
    rmse = sqrt(mean((joined$panna - joined$next_rapm)^2))
  )

  progress_msg(paste("Predictive correlation:", round(metrics$correlation, 3)))

  metrics
}
