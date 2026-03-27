# Player Network Centrality
#
# centrality-based quality adjustment for player ratings. Detects "isolated
# cluster inflation" where a player's rating is inflated because they only
# play against weak opponents in a small league/circuit.
#
# Ported from bouncer (cricket) and adapted for football's team-based
# player networks. Instead of batter-vs-bowler (bipartite), football uses
# player-vs-opponent-team (adjacency via shared matches).
#
# Reference: Opsahl, Agneessens & Skvoretz (2010) 'Node centrality in weighted networks'

#' Calculate Player Centrality
#'
#' Builds a player interaction network from match data and computes
#' centrality centrality scores. Players who face diverse, high-quality
#' opponents get higher centrality. Players isolated in weak leagues
#' get lower centrality.
#'
#' @param player_matches Data frame with columns:
#'   - `player_id`: Player identifier
#'   - `team`: Player's team
#'   - `opponent`: Opposing team
#'   - `match_id`: Match identifier
#'   - `minutes` (optional): Minutes played (used as weight)
#' @param min_matches Integer. Minimum matches for inclusion. Default 5.
#' @param damping Numeric. centrality damping factor (0-1). Default 0.85.
#' @param max_iter Integer. Maximum centrality iterations. Default 100.
#' @param tol Numeric. Convergence tolerance. Default 1e-6.
#'
#' @return Data frame with player_id, centrality (0-1), unique_opponents,
#'   matches_played, component_id, component_size
#' @export
#'
#' @examples
#' \dontrun{
#' # Build from splint/lineup data
#' player_matches <- data.frame(
#'   player_id = c("p1", "p1", "p2", "p2"),
#'   team = c("Arsenal", "Arsenal", "Chelsea", "Chelsea"),
#'   opponent = c("Chelsea", "Liverpool", "Arsenal", "Man City"),
#'   match_id = c("m1", "m2", "m1", "m3"),
#'   minutes = c(90, 75, 90, 80)
#' )
#' centrality <- calculate_player_centrality(player_matches)
#' }
calculate_player_centrality <- function(player_matches,
                                         min_matches = 5L,
                                         damping = 0.85,
                                         max_iter = 100L,
                                         tol = 1e-6) {

  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg Matrix} is required for centrality calculation.")
  }

  # Filter to players with enough matches
  match_counts <- stats::aggregate(
    match_id ~ player_id, data = player_matches,
    FUN = function(x) length(unique(x))
  )
  names(match_counts)[2] <- "n_matches"
  valid_players <- match_counts$player_id[match_counts$n_matches >= min_matches]

  if (length(valid_players) < 2) {
    cli::cli_warn("Fewer than 2 players meet minimum matches threshold ({min_matches})")
    return(data.frame(
      player_id = valid_players,
      centrality = 1,
      unique_opponents = 0L,
      matches_played = min_matches,
      component_id = 1L,
      component_size = length(valid_players),
      stringsAsFactors = FALSE
    ))
  }

  pm <- player_matches[player_matches$player_id %in% valid_players, ]

  # Build player adjacency matrix
  # Two players are connected if they played in the same match (teammates or opponents)
  adj <- build_player_adjacency(pm)

  # Find connected components
  components <- find_components(adj)

  # Calculate centrality
  scores <- calculate_centrality_scores(adj, damping = damping, max_iter = max_iter, tol = tol)

  # Normalize centrality to 0-1 range
  pr_min <- min(scores)
  pr_max <- max(scores)
  if (pr_max > pr_min) {
    centrality <- (scores - pr_min) / (pr_max - pr_min)
  } else {
    centrality <- rep(1, length(scores))
  }

  # Count unique opponents per player
  opp_counts <- stats::aggregate(
    opponent ~ player_id, data = pm,
    FUN = function(x) length(unique(x))
  )
  names(opp_counts)[2] <- "unique_opponents"

  # Build result
  player_ids <- names(centrality)
  result <- data.frame(
    player_id = player_ids,
    centrality = round(as.numeric(centrality[player_ids]), 4),
    unique_opponents = opp_counts$unique_opponents[match(player_ids, opp_counts$player_id)],
    matches_played = match_counts$n_matches[match(player_ids, match_counts$player_id)],
    component_id = components$membership[player_ids],
    component_size = components$sizes[as.character(components$membership[player_ids])],
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # Penalize small components (isolated clusters)
  main_component_size <- max(components$sizes)
  result$centrality <- result$centrality * pmin(1, result$component_size / main_component_size)

  cli::cli_alert_success(
    "Centrality: {nrow(result)} players, {components$n_components} components (main: {main_component_size})"
  )

  result[order(-result$centrality), ]
}


#' Build Player Adjacency Matrix
#'
#' Creates a sparse adjacency matrix where players are connected if
#' they participated in the same match (as teammates or opponents).
#' Edge weight = number of shared matches (or sum of minutes if available).
#'
#' @param pm Data frame with player_id, match_id, and optionally minutes
#' @return Sparse symmetric matrix (dgCMatrix)
#' @keywords internal
build_player_adjacency <- function(pm) {
  player_ids <- sort(unique(pm$player_id))
  n <- length(player_ids)
  player_idx <- stats::setNames(seq_along(player_ids), player_ids)

  has_minutes <- "minutes" %in% names(pm)

  # For each match, connect all players who participated
  matches <- split(pm, pm$match_id)

  # Pre-allocate lists for sparse matrix triplets
  row_idx <- integer()
  col_idx <- integer()
  values <- numeric()

  for (match_data in matches) {
    pids <- unique(match_data$player_id)
    if (length(pids) < 2) next

    pidx <- player_idx[pids]
    # Create all pairs
    pairs <- utils::combn(pidx, 2)

    if (has_minutes) {
      # Weight by geometric mean of minutes
      mins <- stats::setNames(
        tapply(match_data$minutes, match_data$player_id, max, na.rm = TRUE),
        NULL
      )
      for (k in seq_len(ncol(pairs))) {
        i <- pairs[1, k]
        j <- pairs[2, k]
        w <- sqrt(mins[pids[which(pidx == i)[1]]] * mins[pids[which(pidx == j)[1]]])
        if (is.na(w)) w <- 1
        row_idx <- c(row_idx, i, j)
        col_idx <- c(col_idx, j, i)
        values <- c(values, w, w)
      }
    } else {
      for (k in seq_len(ncol(pairs))) {
        i <- pairs[1, k]
        j <- pairs[2, k]
        row_idx <- c(row_idx, i, j)
        col_idx <- c(col_idx, j, i)
        values <- c(values, 1, 1)
      }
    }
  }

  # Build sparse matrix, summing duplicates
  adj <- Matrix::sparseMatrix(
    i = row_idx, j = col_idx, x = values,
    dims = c(n, n), dimnames = list(player_ids, player_ids),
    giveCsparse = TRUE
  )

  adj
}


#' Find Connected Components
#'
#' @param adj Sparse adjacency matrix
#' @return List with membership, sizes, n_components
#' @keywords internal
find_components <- function(adj) {
  n <- nrow(adj)
  ids <- rownames(adj)
  parent <- seq_len(n)

  find_root <- function(i) {
    root <- i
    while (parent[root] != root) root <- parent[root]
    while (parent[i] != root) {
      next_i <- parent[i]
      parent[i] <<- root
      i <- next_i
    }
    root
  }

  # Union connected nodes
  adj_summary <- Matrix::summary(adj)
  for (k in seq_len(nrow(adj_summary))) {
    i <- adj_summary$i[k]
    j <- adj_summary$j[k]
    ri <- find_root(i)
    rj <- find_root(j)
    if (ri != rj) parent[ri] <<- rj
  }

  # Resolve all roots
  membership <- vapply(seq_len(n), find_root, integer(1))
  names(membership) <- ids

  # Component sizes
  comp_table <- table(membership)
  sizes <- as.integer(comp_table)
  names(sizes) <- names(comp_table)

  list(
    membership = membership,
    sizes = sizes,
    n_components = length(sizes)
  )
}


#' Calculate centrality
#'
#' Power iteration method for centrality on an adjacency matrix.
#'
#' @param adj Sparse adjacency matrix
#' @param damping Damping factor (0-1)
#' @param max_iter Maximum iterations
#' @param tol Convergence tolerance
#' @return Named numeric vector of centrality scores
#' @keywords internal
calculate_centrality_scores <- function(adj, damping = 0.85, max_iter = 100L, tol = 1e-6) {
  n <- nrow(adj)
  ids <- rownames(adj)

  # Normalize columns (transition matrix)
  col_sums <- Matrix::colSums(adj)
  col_sums[col_sums == 0] <- 1  # avoid division by zero (dangling nodes)
  trans <- adj %*% Matrix::Diagonal(n, 1 / col_sums)

  # Initialize uniform
  pr <- rep(1 / n, n)
  names(pr) <- ids

  teleport <- (1 - damping) / n

  for (iter in seq_len(max_iter)) {
    pr_new <- as.numeric(damping * (trans %*% pr)) + teleport
    pr_new <- pr_new / sum(pr_new)  # normalize

    delta <- max(abs(pr_new - pr))
    pr <- pr_new

    if (delta < tol) break
  }

  names(pr) <- ids
  pr
}
