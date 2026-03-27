# Season Simulation
#
# Monte Carlo simulation of football seasons using match prediction
# probabilities. Generates league table projections, title probabilities,
# relegation risks, and Champions League qualification odds.

#' Simulate Season
#'
#' Runs Monte Carlo simulations of remaining fixtures to project final
#' league standings. Uses match-level win/draw/loss probabilities from
#' the prediction model.
#'
#' @param predictions Data frame with columns: `home`, `away`, `prob_H`,
#'   `prob_D`, `prob_A`, and optionally `pred_home_goals`, `pred_away_goals`.
#'   Only unplayed fixtures should be included.
#' @param completed Data frame of completed matches with columns: `home`,
#'   `away`, `home_goals`, `away_goals`. These results are fixed across
#'   all simulations.
#' @param n_sims Integer. Number of Monte Carlo simulations. Default 10000.
#' @param points_win Integer. Points for a win. Default 3.
#' @param points_draw Integer. Points for a draw. Default 1.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A list with:
#'   \itemize{
#'     \item `table`: Summary table with mean points, title/UCL/relegation probabilities
#'     \item `simulations`: Raw simulation results (n_sims x n_teams matrix of points)
#'     \item `positions`: Position frequency matrix (n_teams x n_teams)
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Predictions for remaining fixtures
#' preds <- data.frame(
#'   home = c("Arsenal", "Liverpool"),
#'   away = c("Chelsea", "Man City"),
#'   prob_H = c(0.45, 0.35),
#'   prob_D = c(0.28, 0.30),
#'   prob_A = c(0.27, 0.35)
#' )
#' # Already-played matches
#' completed <- data.frame(
#'   home = "Arsenal", away = "Liverpool",
#'   home_goals = 2, away_goals = 1
#' )
#' result <- simulate_season(preds, completed, n_sims = 1000)
#' result$table
#' }
simulate_season <- function(predictions, completed,
                             n_sims = 10000L,
                             points_win = 3L,
                             points_draw = 1L,
                             verbose = TRUE) {

  # Validate inputs
  required_pred <- c("home", "away", "prob_H", "prob_D", "prob_A")
  missing <- setdiff(required_pred, names(predictions))
  if (length(missing) > 0) {
    cli::cli_abort("Missing columns in predictions: {paste(missing, collapse = ', ')}")
  }

  required_comp <- c("home", "away", "home_goals", "away_goals")
  missing_comp <- setdiff(required_comp, names(completed))
  if (length(missing_comp) > 0) {
    cli::cli_abort("Missing columns in completed: {paste(missing_comp, collapse = ', ')}")
  }

  # Get all teams
  all_teams <- sort(unique(c(
    completed$home, completed$away,
    predictions$home, predictions$away
  )))
  n_teams <- length(all_teams)

  if (n_teams == 0) {
    cli::cli_warn("No teams found in predictions or completed matches")
    return(list(table = data.frame(), simulations = matrix(nrow = 0, ncol = 0),
                goal_diff = matrix(nrow = 0, ncol = 0), positions = matrix(nrow = 0, ncol = 0),
                n_sims = n_sims, n_teams = 0L, n_remaining = 0L))
  }

  if (verbose) {
    cli::cli_alert_info("Simulating {n_sims} seasons for {n_teams} teams ({nrow(predictions)} remaining fixtures)")
  }

  # Calculate base points from completed matches
  base_points <- calculate_table(completed, all_teams, points_win, points_draw)

  # Pre-compute cumulative probabilities for sampling
  cum_H <- predictions$prob_H
  cum_D <- cum_H + predictions$prob_D
  # cum_A = 1.0 (implicit)

  n_fixtures <- nrow(predictions)

  # Team index lookup
  team_idx <- stats::setNames(seq_along(all_teams), all_teams)
  home_idx <- team_idx[predictions$home]
  away_idx <- team_idx[predictions$away]

  # Simulate: matrix of points (n_sims x n_teams)
  sim_points <- matrix(0L, nrow = n_sims, ncol = n_teams)
  sim_gd <- matrix(0L, nrow = n_sims, ncol = n_teams)

  # Add base points from completed matches
  for (i in seq_along(all_teams)) {
    sim_points[, i] <- base_points$points[i]
    sim_gd[, i] <- base_points$gd[i]
  }

  # Use predicted goals for GD simulation if available
  has_goals <- all(c("pred_home_goals", "pred_away_goals") %in% names(predictions))

  # Vectorized simulation across all sims
  for (f in seq_len(n_fixtures)) {
    # Draw random outcomes for all simulations at once
    draws <- stats::runif(n_sims)
    is_home_win <- draws < cum_H[f]
    is_draw <- draws >= cum_H[f] & draws < cum_D[f]
    is_away_win <- !is_home_win & !is_draw

    hi <- home_idx[f]
    ai <- away_idx[f]

    # Points
    sim_points[is_home_win, hi] <- sim_points[is_home_win, hi] + points_win
    sim_points[is_draw, hi] <- sim_points[is_draw, hi] + points_draw
    sim_points[is_draw, ai] <- sim_points[is_draw, ai] + points_draw
    sim_points[is_away_win, ai] <- sim_points[is_away_win, ai] + points_win

    # Goal difference (simplified: sample from Poisson if goals available)
    if (has_goals) {
      hg <- stats::rpois(n_sims, lambda = pmax(0.5, predictions$pred_home_goals[f]))
      ag <- stats::rpois(n_sims, lambda = pmax(0.5, predictions$pred_away_goals[f]))
      sim_gd[, hi] <- sim_gd[, hi] + (hg - ag)
      sim_gd[, ai] <- sim_gd[, ai] + (ag - hg)
    }
  }

  # Calculate positions per simulation
  position_counts <- matrix(0L, nrow = n_teams, ncol = n_teams)
  rownames(position_counts) <- all_teams
  colnames(position_counts) <- paste0("pos_", seq_len(n_teams))

  for (s in seq_len(n_sims)) {
    # Sort by points (desc), then GD (desc)
    ord <- order(-sim_points[s, ], -sim_gd[s, ])
    for (pos in seq_along(ord)) {
      position_counts[ord[pos], pos] <- position_counts[ord[pos], pos] + 1L
    }
  }

  # Build summary table
  mean_points <- colMeans(sim_points)
  mean_gd <- colMeans(sim_gd)
  sd_points <- apply(sim_points, 2, stats::sd)

  # Position probabilities
  title_prob <- position_counts[, 1] / n_sims
  top4_prob <- rowSums(position_counts[, 1:min(4, n_teams)]) / n_sims
  top6_prob <- rowSums(position_counts[, 1:min(6, n_teams)]) / n_sims
  bottom3_prob <- rowSums(position_counts[, max(1, n_teams-2):n_teams]) / n_sims

  table_df <- data.frame(
    team = all_teams,
    current_points = base_points$points,
    current_gd = base_points$gd,
    mean_points = round(mean_points, 1),
    sd_points = round(sd_points, 1),
    mean_gd = round(mean_gd, 1),
    title_pct = round(title_prob * 100, 1),
    top4_pct = round(top4_prob * 100, 1),
    top6_pct = round(top6_prob * 100, 1),
    bottom3_pct = round(bottom3_prob * 100, 1),
    stringsAsFactors = FALSE
  )

  # Sort by mean points descending
  table_df <- table_df[order(-table_df$mean_points, -table_df$mean_gd), ]
  rownames(table_df) <- NULL

  if (verbose) {
    cli::cli_alert_success("Simulation complete")
    top3 <- head(table_df, 3)
    for (i in seq_len(nrow(top3))) {
      cli::cli_alert_info("{top3$team[i]}: {top3$mean_points[i]} pts (title {top3$title_pct[i]}%)")
    }
  }

  list(
    table = table_df,
    simulations = sim_points,
    goal_diff = sim_gd,
    positions = position_counts,
    n_sims = n_sims,
    n_teams = n_teams,
    n_remaining = n_fixtures
  )
}


#' Calculate League Table from Results
#'
#' @param matches Data frame with home, away, home_goals, away_goals
#' @param teams Character vector of all teams
#' @param points_win Points for a win
#' @param points_draw Points for a draw
#'
#' @return Data frame with team, played, won, drawn, lost, gf, ga, gd, points
#' @keywords internal
calculate_table <- function(matches, teams, points_win = 3L, points_draw = 1L) {
  n <- length(teams)
  played <- won <- drawn <- lost <- gf <- ga <- integer(n)
  names(played) <- names(won) <- names(drawn) <- names(lost) <- teams
  names(gf) <- names(ga) <- teams

  for (i in seq_len(nrow(matches))) {
    h <- matches$home[i]
    a <- matches$away[i]
    hg <- matches$home_goals[i]
    ag <- matches$away_goals[i]

    if (is.na(hg) || is.na(ag)) next
    if (!h %in% teams || !a %in% teams) next

    played[h] <- played[h] + 1L
    played[a] <- played[a] + 1L
    gf[h] <- gf[h] + hg
    ga[h] <- ga[h] + ag
    gf[a] <- gf[a] + ag
    ga[a] <- ga[a] + hg

    if (hg > ag) {
      won[h] <- won[h] + 1L
      lost[a] <- lost[a] + 1L
    } else if (hg < ag) {
      lost[h] <- lost[h] + 1L
      won[a] <- won[a] + 1L
    } else {
      drawn[h] <- drawn[h] + 1L
      drawn[a] <- drawn[a] + 1L
    }
  }

  points <- won * points_win + drawn * points_draw
  gd <- gf - ga

  data.frame(
    team = teams,
    played = played,
    won = won,
    drawn = drawn,
    lost = lost,
    gf = gf,
    ga = ga,
    gd = gd,
    points = points,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
