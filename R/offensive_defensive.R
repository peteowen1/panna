# Offensive/Defensive panna rating separation
#
# Separates panna ratings into offensive (O-panna) and defensive (D-panna)
# components to understand player contribution profiles.

#' Calculate O-panna and D-panna
#'
#' Separates panna rating into offensive and defensive components.
#'
#' @param rapm_data RAPM data prepared with separate_od = TRUE
#' @param spm_ratings SPM ratings (or separate O-SPM/D-SPM)
#' @param lambda_prior Regularization strength
#'
#' @return List with separated offensive and defensive ratings
#' @export
#' @examples
#' \dontrun{
#' rapm_data <- create_rapm_design_matrix(splints, separate_od = TRUE)
#' od_result <- calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)
#' head(od_result$ratings[, c("player_name", "o_panna", "d_panna", "panna")])
#' }
calculate_od_panna <- function(rapm_data, spm_ratings, lambda_prior = 1) {
  if (is.null(rapm_data$X_od)) {
    cli::cli_abort(c(
      "{.arg rapm_data} must be prepared with {.code separate_od = TRUE}.",
      "i" = "Use {.fn create_rapm_design_matrix} with {.code separate_od = TRUE}."
    ))
  }

  X <- rapm_data$X_od
  y <- rapm_data$y

  # Remove NA responses
  valid_idx <- !is.na(y)
  X <- X[valid_idx, , drop = FALSE]
  y <- y[valid_idx]

  n_players <- rapm_data$n_players
  player_ids <- rapm_data$player_ids

  # Create O/D prior vectors
  o_prior <- rep(0, n_players)
  d_prior <- rep(0, n_players)
  names(o_prior) <- player_ids
  names(d_prior) <- player_ids

  # If we have O-SPM and D-SPM, use them
  if ("o_spm" %in% names(spm_ratings)) {
    for (i in seq_along(player_ids)) {
      pid <- player_ids[i]
      if (pid %in% spm_ratings$player_id) {
        o_prior[i] <- spm_ratings$o_spm[spm_ratings$player_id == pid]
        d_prior[i] <- spm_ratings$d_spm[spm_ratings$player_id == pid]
      }
    }
  } else if ("spm" %in% names(spm_ratings)) {
    # Split overall SPM evenly between O and D as approximation
    for (i in seq_along(player_ids)) {
      pid <- player_ids[i]
      if (pid %in% spm_ratings$player_id) {
        spm_val <- spm_ratings$spm[spm_ratings$player_id == pid]
        o_prior[i] <- spm_val / 2
        d_prior[i] <- spm_val / 2
      }
    }
  }

  # Combined prior for O/D model
  od_prior <- c(o_prior, d_prior)

  # Fit with prior
  y_adjusted <- as.vector(y - X %*% od_prior)

  fit <- glmnet::glmnet(
    x = X,
    y = y_adjusted,
    alpha = 0,
    lambda = lambda_prior,
    standardize = FALSE
  )

  # Extract coefficients
  gamma <- as.vector(stats::coef(fit, s = lambda_prior))[-1]
  od_coef <- gamma + od_prior

  # Split into O and D
  o_panna <- od_coef[1:n_players]
  d_panna <- od_coef[(n_players + 1):(2 * n_players)]

  # Create results
  ratings <- data.frame(
    player_id = player_ids,
    o_panna = o_panna,
    d_panna = d_panna,
    panna = o_panna + d_panna
  )

  # Add player names
  if (!is.null(rapm_data$player_mapping)) {
    mapping <- data.table::as.data.table(rapm_data$player_mapping[, c("player_id", "player_name")])
    ratings <- mapping[data.table::as.data.table(ratings), on = "player_id"]
    data.table::setDF(ratings)
  }

  ratings <- ratings[order(-ratings$panna), ]

  list(
    ratings = ratings,
    o_prior = o_prior,
    d_prior = d_prior,
    model = fit,
    lambda = lambda_prior
  )
}


#' Split O/D contributions from existing ratings
#'
#' Approximates O/D split from overall panna and feature data.
#'
#' @param panna_ratings Overall panna ratings
#' @param player_features Player features with offensive/defensive stats
#'
#' @return Data frame with estimated O-panna and D-panna
#' @export
#' @examples
#' \dontrun{
#' features <- create_player_feature_matrix(processed_data)
#' od_ratings <- split_od_contributions(panna_ratings, features)
#' head(od_ratings[, c("player_name", "panna", "o_panna", "d_panna")])
#' }
split_od_contributions <- function(panna_ratings, player_features) {
  # Calculate offensive/defensive feature indices
  off_cols <- names(player_features)[grepl("(xG|Sh|Ast|SCA|GCA|PrgP|PrgC)", names(player_features))]
  def_cols <- names(player_features)[grepl("(Tkl|Int|Block|Clr)", names(player_features))]

  if (length(off_cols) == 0 || length(def_cols) == 0) {
    cli::cli_warn(c(
      "Cannot split O/D contributions: missing feature columns.",
      "i" = "Returning original ratings without O/D split."
    ))
    return(panna_ratings)
  }

  # Calculate offensive and defensive z-scores
  features <- player_features
  off_present <- intersect(off_cols, names(features))
  def_present <- intersect(def_cols, names(features))
  off_scaled <- scale(as.matrix(features[, off_present, drop = FALSE]))
  def_scaled <- scale(as.matrix(features[, def_present, drop = FALSE]))
  features$off_zscore <- rowMeans(off_scaled, na.rm = TRUE)
  features$def_zscore <- rowMeans(def_scaled, na.rm = TRUE)
  features$total_zscore <- abs(features$off_zscore) + abs(features$def_zscore)
  features$off_weight <- safe_divide(abs(features$off_zscore), features$total_zscore)
  features$def_weight <- safe_divide(abs(features$def_zscore), features$total_zscore)

  # Join with ratings and split
  id_col <- intersect(names(panna_ratings), names(features))
  id_col <- id_col[id_col %in% c("player_id", "player_name")][1]

  weight_cols <- intersect(c(id_col, "off_weight", "def_weight"), names(features))
  result <- data.table::as.data.table(features[, weight_cols, drop = FALSE])[data.table::as.data.table(panna_ratings), on = id_col]
  data.table::setDF(result)
  result$off_weight <- ifelse(is.na(result$off_weight), 0.5, result$off_weight)
  result$def_weight <- ifelse(is.na(result$def_weight), 0.5, result$def_weight)
  result$o_panna <- result$panna * result$off_weight
  result$d_panna <- result$panna * result$def_weight
  result$off_weight <- NULL
  result$def_weight <- NULL

  result
}


#' Categorize player profile
#'
#' Classifies players based on their O/D rating balance.
#'
#' @param o_rating Offensive rating
#' @param d_rating Defensive rating
#'
#' @return Character vector of player types
#' @export
#' @examples
#' \dontrun{
#' categorize_player_profile(0.8, 0.2)
#' categorize_player_profile(c(0.6, 0.3, 0.5), c(0.4, 0.7, 0.5))
#' }
categorize_player_profile <- function(o_rating, d_rating) {
  total <- o_rating + d_rating
  o_pct <- safe_divide(o_rating, total + 0.001)  # Avoid division by zero

  data.table::fcase(
    o_pct > 0.7, "Offensive",
    o_pct > 0.55, "Balanced-Offensive",
    o_pct > 0.45, "Balanced",
    o_pct > 0.3, "Balanced-Defensive",
    default = "Defensive"
  )
}


#' Get top offensive players
#'
#' Returns players with highest offensive contribution.
#'
#' @param ratings O/D ratings data frame
#' @param n Number of players
#'
#' @return Data frame of top offensive players
#' @export
#' @examples
#' \dontrun{
#' od_result <- calculate_od_panna(rapm_data, spm_ratings)
#' top_attackers <- get_top_offensive(od_result$ratings, n = 20)
#' }
get_top_offensive <- function(ratings, n = 10) {
  if (!"o_panna" %in% names(ratings)) {
    cli::cli_abort(c(
      "{.arg ratings} must include {.field o_panna} column.",
      "i" = "Use {.fn calculate_od_panna} to generate offensive ratings."
    ))
  }

  head(ratings[order(-ratings$o_panna), ], n)
}


#' Get top defensive players
#'
#' Returns players with highest defensive contribution.
#'
#' @param ratings O/D ratings data frame
#' @param n Number of players
#'
#' @return Data frame of top defensive players
#' @export
#' @examples
#' \dontrun{
#' od_result <- calculate_od_panna(rapm_data, spm_ratings)
#' top_defenders <- get_top_defensive(od_result$ratings, n = 20)
#' }
get_top_defensive <- function(ratings, n = 10) {
  if (!"d_panna" %in% names(ratings)) {
    cli::cli_abort(c(
      "{.arg ratings} must include {.field d_panna} column.",
      "i" = "Use {.fn calculate_od_panna} to generate defensive ratings."
    ))
  }

  head(ratings[order(-ratings$d_panna), ], n)
}


#' Create O/D scatter plot data
#'
#' Prepares data for visualizing offensive vs defensive ratings.
#'
#' @param ratings O/D ratings data frame
#'
#' @return Data frame ready for ggplot2
#' @keywords internal
prepare_od_scatter_data <- function(ratings) {
  if (!all(c("o_panna", "d_panna") %in% names(ratings))) {
    cli::cli_abort(c(
      "{.arg ratings} must include {.field o_panna} and {.field d_panna} columns.",
      "i" = "Use {.fn calculate_od_panna} to generate O/D ratings."
    ))
  }

  has_player_name <- "player_name" %in% names(ratings)
  ratings$profile <- categorize_player_profile(ratings$o_panna, ratings$d_panna)
  ratings$label <- if (has_player_name) ratings$player_name else ratings$player_id
  ratings
}


#' Visualize O/D scatter
#'
#' Creates scatter plot of offensive vs defensive ratings.
#' Requires ggplot2.
#'
#' @param ratings O/D ratings data frame
#' @param highlight_top Number of top players to label
#'
#' @return ggplot object
#' @export
#' @examples
#' \dontrun{
#' od_result <- calculate_od_panna(rapm_data, spm_ratings)
#' visualize_od_scatter(od_result$ratings, highlight_top = 15)
#' }
visualize_od_scatter <- function(ratings, highlight_top = 10) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg ggplot2} is required for visualization.")
  }

  plot_data <- prepare_od_scatter_data(ratings)

  # Identify top players to label
  top_overall <- head(plot_data[order(-plot_data$panna), ], highlight_top)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$o_panna, y = .data$d_panna)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$profile), alpha = 0.7) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      x = "Offensive Panna (O-Panna)",
      y = "Defensive Panna (D-Panna)",
      title = "Player Offensive vs Defensive Impact",
      color = "Profile"
    ) +
    ggplot2::theme_minimal()

  # Add labels for top players if ggrepel available
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_text_repel(
      data = top_overall,
      ggplot2::aes(label = .data$label),
      size = 3
    )
  }

  p
}
