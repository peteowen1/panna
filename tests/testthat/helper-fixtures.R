# Shared test fixtures for panna tests
#
# testthat automatically sources helper-*.R files before running tests.
# These fixtures deduplicate synthetic data generation across test files.

#' Create synthetic processed data (mimics output of data-raw/02)
#'
#' @param n_matches Number of matches to generate
#' @param players_per_team Players per team per match
#' @return List with results, lineups, shooting, events, stats_summary
create_synthetic_processed_data <- function(n_matches = 10, players_per_team = 11) {
  set.seed(42)

  home_teams <- paste0("Team_", LETTERS[1:5])
  away_teams <- paste0("Team_", LETTERS[6:10])
  match_ids <- paste0("match_", seq_len(n_matches))

  results <- data.frame(
    match_id = match_ids,
    home_team = rep(home_teams, length.out = n_matches),
    away_team = rep(away_teams, length.out = n_matches),
    home_score = sample(0:3, n_matches, replace = TRUE),
    away_score = sample(0:3, n_matches, replace = TRUE),
    home_xg = runif(n_matches, 0.5, 3),
    away_xg = runif(n_matches, 0.5, 3),
    stringsAsFactors = FALSE
  )

  all_player_ids <- paste0("player_", 1:50)
  all_player_names <- paste("Player", 1:50)

  lineup_rows <- list()
  for (i in seq_len(n_matches)) {
    home_idx <- sample(1:25, players_per_team)
    away_idx <- sample(26:50, players_per_team)

    for (j in seq_along(home_idx)) {
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = all_player_ids[home_idx[j]],
        player_name = all_player_names[home_idx[j]],
        team = results$home_team[i],
        is_home = TRUE, is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
    for (j in seq_along(away_idx)) {
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = all_player_ids[away_idx[j]],
        player_name = all_player_names[away_idx[j]],
        team = results$away_team[i],
        is_home = FALSE, is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
  }
  lineups <- do.call(rbind, lineup_rows)

  shot_rows <- list()
  for (i in seq_len(n_matches)) {
    n_shots <- sample(15:30, 1)
    match_lineups <- lineups[lineups$match_id == match_ids[i], ]
    shot_players <- match_lineups[sample(nrow(match_lineups), n_shots, replace = TRUE), ]
    shot_rows[[i]] <- data.frame(
      match_id = rep(match_ids[i], n_shots),
      minute = sort(sample(1:90, n_shots, replace = TRUE)),
      player_id = shot_players$player_id,
      player_name = shot_players$player_name,
      team = shot_players$team,
      is_home = shot_players$is_home,
      xg = pmin(runif(n_shots, 0.02, 0.5), 0.95),
      is_goal = rbinom(n_shots, 1, 0.1) == 1,
      is_penalty = FALSE, is_own_goal = FALSE,
      stringsAsFactors = FALSE
    )
  }
  shooting <- do.call(rbind, shot_rows)

  event_rows <- list()
  for (i in seq_len(n_matches)) {
    match_shots <- shooting[shooting$match_id == match_ids[i], ]
    goals <- match_shots[match_shots$is_goal, ]
    if (nrow(goals) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(goals)),
        team = goals$team, is_home = goals$is_home,
        event_type = "goal", minute = goals$minute,
        player_name = goals$player_name,
        is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }
    n_subs <- sample(2:3, 1)
    match_lineups <- lineups[lineups$match_id == match_ids[i] & lineups$is_starter, ]
    sub_players <- match_lineups[sample(nrow(match_lineups), min(n_subs, nrow(match_lineups))), ]
    if (nrow(sub_players) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(sub_players)),
        team = sub_players$team, is_home = sub_players$is_home,
        event_type = "substitution",
        minute = sample(45:75, nrow(sub_players), replace = TRUE),
        player_name = sub_players$player_name,
        is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  events <- do.call(rbind, event_rows)

  list(
    results = results, lineups = lineups, shooting = shooting,
    events = events, stats_summary = NULL
  )
}


#' Create synthetic player features (mimics output of aggregate_player_stats)
create_synthetic_player_features <- function(player_ids, player_names,
                                              total_minutes) {
  set.seed(42)
  n <- length(player_ids)

  data.frame(
    player_id = player_ids,
    player_name = player_names,
    total_minutes = total_minutes,
    n_matches = pmax(1, round(total_minutes / 90)),
    goals_p90 = runif(n, 0, 0.6),
    assists_p90 = runif(n, 0, 0.4),
    xg_p90 = runif(n, 0, 0.5),
    npxg_p90 = runif(n, 0, 0.45),
    xa_p90 = runif(n, 0, 0.35),
    shots_p90 = runif(n, 0.5, 3.5),
    shots_on_target_p90 = runif(n, 0.2, 1.8),
    sca_p90 = runif(n, 1, 4.5),
    gca_p90 = runif(n, 0.05, 0.6),
    tackles_p90 = runif(n, 0.5, 3.5),
    interceptions_p90 = runif(n, 0.3, 2),
    blocks_p90 = runif(n, 0.2, 1.5),
    clearances_p90 = runif(n, 0, 4),
    progressive_passes_p90 = runif(n, 1, 7),
    progressive_carries_p90 = runif(n, 0.5, 4),
    touches_p90 = runif(n, 35, 85),
    npxg_plus_xa_p90 = runif(n, 0, 0.7),
    stringsAsFactors = FALSE
  )
}
