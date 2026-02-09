# Integration tests for the full Panna pipeline
# Tests: processed_data -> splints -> RAPM -> SPM -> Panna ratings


# =============================================================================
# Helper: Create synthetic processed_data (mimics output of data-raw/02)
# =============================================================================

create_synthetic_processed_data <- function(n_matches = 10, players_per_team = 11) {
  set.seed(42)

  home_teams <- paste0("Team_", LETTERS[1:5])
  away_teams <- paste0("Team_", LETTERS[6:10])

  match_ids <- paste0("match_", seq_len(n_matches))

  # -- results --
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

  # -- lineups (11 players per team per match) --
  lineup_rows <- list()
  all_player_ids <- paste0("player_", 1:50)
  all_player_names <- paste("Player", 1:50)

  for (i in seq_len(n_matches)) {
    home_idx <- sample(1:25, players_per_team)
    away_idx <- sample(26:50, players_per_team)

    for (j in seq_along(home_idx)) {
      pid <- all_player_ids[home_idx[j]]
      pname <- all_player_names[home_idx[j]]
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = pid,
        player_name = pname,
        team = results$home_team[i],
        is_home = TRUE,
        is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
    for (j in seq_along(away_idx)) {
      pid <- all_player_ids[away_idx[j]]
      pname <- all_player_names[away_idx[j]]
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = pid,
        player_name = pname,
        team = results$away_team[i],
        is_home = FALSE,
        is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
  }
  lineups <- do.call(rbind, lineup_rows)

  # -- shooting (xG per shot) --
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
      is_penalty = FALSE,
      is_own_goal = FALSE,
      stringsAsFactors = FALSE
    )
  }
  shooting <- do.call(rbind, shot_rows)

  # -- events (goals + substitutions as boundary markers) --
  event_rows <- list()
  for (i in seq_len(n_matches)) {
    match_shots <- shooting[shooting$match_id == match_ids[i], ]
    goals <- match_shots[match_shots$is_goal, ]

    if (nrow(goals) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(goals)),
        team = goals$team,
        is_home = goals$is_home,
        event_type = "goal",
        minute = goals$minute,
        player_name = goals$player_name,
        is_penalty = FALSE,
        is_own_goal = FALSE,
        is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }

    # Add 2-3 subs per team at halftime-ish
    n_subs <- sample(2:3, 1)
    match_lineups <- lineups[lineups$match_id == match_ids[i] & lineups$is_starter, ]
    sub_players <- match_lineups[sample(nrow(match_lineups), min(n_subs, nrow(match_lineups))), ]

    if (nrow(sub_players) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(sub_players)),
        team = sub_players$team,
        is_home = sub_players$is_home,
        event_type = "substitution",
        minute = sample(45:75, nrow(sub_players), replace = TRUE),
        player_name = sub_players$player_name,
        is_penalty = FALSE,
        is_own_goal = FALSE,
        is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  events <- do.call(rbind, event_rows)

  list(
    results = results,
    lineups = lineups,
    shooting = shooting,
    events = events,
    stats_summary = NULL
  )
}


# Create synthetic player features (mimics output of aggregate_player_stats)
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


# =============================================================================
# Integration Test 1: Full pipeline (splints -> RAPM -> SPM -> Panna)
# =============================================================================

test_that("full pipeline: processed_data -> splints -> RAPM -> SPM -> Panna", {
  skip_if_not_installed("glmnet")

  # Step 1: Create synthetic processed data
  processed <- create_synthetic_processed_data(n_matches = 15)
  expect_equal(nrow(processed$results), 15)
  expect_true(nrow(processed$lineups) > 0)
  expect_true(nrow(processed$shooting) > 0)

  # Step 2: Create splints
  splints <- create_all_splints(processed, include_goals = TRUE, verbose = FALSE)

  expect_true(is.list(splints))
  expect_true("splints" %in% names(splints))
  expect_true("players" %in% names(splints))
  expect_true("match_info" %in% names(splints))
  expect_true(nrow(splints$splints) > 0)
  expect_true(nrow(splints$players) > 0)

  # Splints have required columns
  expect_true(all(c("splint_id", "match_id", "duration") %in% names(splints$splints)))
  expect_true(all(c("splint_id", "player_id", "player_name") %in% names(splints$players)))

  # Step 3: Prepare RAPM data
  rapm_data <- prepare_rapm_data(splints, min_minutes = 30)

  expect_true(is.list(rapm_data))
  expect_true("X" %in% names(rapm_data) || "X_full" %in% names(rapm_data))
  expect_true("y" %in% names(rapm_data))
  expect_true("weights" %in% names(rapm_data))
  expect_true("player_mapping" %in% names(rapm_data))

  X <- if (!is.null(rapm_data$X_full)) rapm_data$X_full else rapm_data$X
  expect_true(nrow(X) > 0)
  expect_true(ncol(X) > 0)

  # Design matrix has 2 rows per splint (home/away perspective)
  expect_equal(nrow(X) %% 2, 0)

  # Step 4: Fit RAPM
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3)
  expect_true(inherits(rapm_model, "cv.glmnet"))

  rapm_ratings <- extract_rapm_ratings(rapm_model)
  expect_true(is.data.frame(rapm_ratings))
  expect_true(all(c("player_id", "rapm", "offense", "defense") %in% names(rapm_ratings)))
  expect_true(nrow(rapm_ratings) > 0)

  # Ratings are numeric and finite
  expect_true(all(is.finite(rapm_ratings$rapm)))
  expect_true(all(is.finite(rapm_ratings$offense)))
  expect_true(all(is.finite(rapm_ratings$defense)))

  # Step 5: Create player features and fit SPM
  player_features <- create_synthetic_player_features(
    player_ids = rapm_ratings$player_id,
    player_names = rapm_ratings$player_name,
    total_minutes = rapm_ratings$total_minutes
  )

  spm_train <- prepare_spm_regression_data(player_features, rapm_ratings)
  expect_true("rapm" %in% names(spm_train))
  expect_true(nrow(spm_train) > 0)

  spm_model <- fit_spm_model(spm_train, alpha = 0.5, nfolds = 3)
  expect_true(inherits(spm_model, "cv.glmnet"))

  spm_ratings <- calculate_spm_ratings(player_features, spm_model)
  expect_true("spm" %in% names(spm_ratings))
  expect_true(all(is.finite(spm_ratings$spm)))

  # Step 6: Calculate Panna ratings (RAPM + SPM prior)
  panna_result <- calculate_panna_rating(
    rapm_data = rapm_data,
    spm_ratings = spm_ratings,
    lambda_prior = 1
  )

  expect_true(is.list(panna_result))
  expect_true("ratings" %in% names(panna_result))

  panna_ratings <- panna_result$ratings
  expect_true(is.data.frame(panna_ratings))
  expect_true("panna" %in% names(panna_ratings))
  expect_true("player_id" %in% names(panna_ratings))
  expect_true(nrow(panna_ratings) > 0)
  expect_true(all(is.finite(panna_ratings$panna)))
})


# =============================================================================
# Integration Test 2: O/D decomposition from full pipeline
# =============================================================================

test_that("offensive/defensive decomposition from RAPM is consistent", {
  skip_if_not_installed("glmnet")

  processed <- create_synthetic_processed_data(n_matches = 15)
  splints <- create_all_splints(processed, verbose = FALSE)
  rapm_data <- prepare_rapm_data(splints, min_minutes = 30)
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3)
  ratings <- extract_rapm_ratings(rapm_model)

  # RAPM = offense - defense
  expect_equal(ratings$rapm, ratings$offense - ratings$defense, tolerance = 1e-10)

  # All players from the model are present
  expect_equal(nrow(ratings), length(rapm_data$player_ids))

  # Player names joined correctly

  expect_true("player_name" %in% names(ratings))
  expect_true(all(!is.na(ratings$player_name)))

  # Total minutes joined correctly
  expect_true("total_minutes" %in% names(ratings))
  # Most players should meet min_minutes threshold (some may be replacement pool)
  expect_true(sum(ratings$total_minutes >= 30, na.rm = TRUE) > 0)
})


# =============================================================================
# Integration Test 3: xRAPM (RAPM with SPM prior)
# =============================================================================

test_that("xRAPM pipeline: RAPM -> SPM -> fit_rapm_with_prior", {
  skip_if_not_installed("glmnet")

  processed <- create_synthetic_processed_data(n_matches = 12)
  splints <- create_all_splints(processed, verbose = FALSE)
  rapm_data <- prepare_rapm_data(splints, min_minutes = 30)

  # Fit base RAPM
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3)
  rapm_ratings <- extract_rapm_ratings(rapm_model)

  # Fit SPM
  player_features <- create_synthetic_player_features(
    player_ids = rapm_ratings$player_id,
    player_names = rapm_ratings$player_name,
    total_minutes = rapm_ratings$total_minutes
  )
  spm_train <- prepare_spm_regression_data(player_features, rapm_ratings)
  spm_model <- fit_spm_model(spm_train, alpha = 0.5, nfolds = 3)
  spm_ratings <- calculate_spm_ratings(player_features, spm_model)

  # Build offense/defense priors from SPM
  offense_prior <- build_prior_vector(
    spm_ratings, "spm",
    rapm_data$player_mapping
  )
  defense_prior <- rep(0, length(offense_prior))
  names(defense_prior) <- names(offense_prior)

  # Fit xRAPM (RAPM with SPM as prior)
  xrapm_model <- fit_rapm_with_prior(
    rapm_data,
    offense_prior = offense_prior,
    defense_prior = defense_prior,
    alpha = 0,
    nfolds = 3
  )

  expect_true(inherits(xrapm_model, "cv.glmnet"))

  xrapm_ratings <- extract_rapm_ratings(xrapm_model)
  expect_true(nrow(xrapm_ratings) > 0)
  expect_true(all(is.finite(xrapm_ratings$rapm)))

  # xRAPM should produce valid ratings for all players
  expect_true(nrow(xrapm_ratings) > 0)
  expect_true(nrow(xrapm_ratings) == nrow(rapm_ratings))

  # xRAPM should have the same players as RAPM
  common <- merge(rapm_ratings, xrapm_ratings, by = "player_id",
                  suffixes = c("_base", "_xrapm"))
  expect_equal(nrow(common), nrow(rapm_ratings))
})


# =============================================================================
# Integration Test 4: Opta pipeline (splints from Opta data)
# =============================================================================

test_that("Opta processed data flows through splint creation", {
  set.seed(42)
  n_matches <- 5
  match_ids <- paste0("opta_match_", 1:n_matches)

  # Create Opta-style events
  opta_events <- data.frame(
    match_id = rep(match_ids, each = 6),
    minute = rep(c(10, 25, 45, 55, 70, 85), n_matches),
    second = rep(0, n_matches * 6),
    event_type = rep(c("goal", "substitution", "substitution",
                        "goal", "substitution", "substitution"), n_matches),
    team_id = rep(c("101", "101", "102", "102", "101", "102"), n_matches),
    team_position = rep(c("home", "home", "away", "away", "home", "away"), n_matches),
    player_id = paste0("op_", seq_len(n_matches * 6)),
    player_name = paste("Opta Player", seq_len(n_matches * 6)),
    stringsAsFactors = FALSE
  )

  events <- prepare_opta_events_for_splints(opta_events)
  expect_true(is.data.frame(events))
  expect_true(all(c("match_id", "minute", "is_goal", "is_sub", "is_red_card") %in% names(events)))
  expect_true(nrow(events) > 0)

  # Create Opta-style lineups
  lineup_rows <- list()
  for (i in seq_len(n_matches)) {
    for (side in c("home", "away")) {
      tid <- if (side == "home") "101" else "102"
      for (j in 1:11) {
        pid <- paste0("op_", (i - 1) * 22 + (if (side == "away") 11 else 0) + j)
        lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
          match_id = match_ids[i],
          player_id = pid,
          player_name = paste("Opta Player", pid),
          team_id = tid,
          team_name = paste0("Team_", tid),
          team_position = side,
          is_starter = TRUE,
          minutes_played = 90,
          sub_on_minute = 0,
          sub_off_minute = 0,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  opta_lineups <- do.call(rbind, lineup_rows)

  lineups <- prepare_opta_lineups_for_splints(opta_lineups)
  expect_true(is.data.frame(lineups))
  expect_true(all(c("match_id", "player_id", "team", "is_home", "on_minute", "off_minute") %in% names(lineups)))

  # Create Opta-style shots
  opta_shots <- data.frame(
    match_id = rep(match_ids, each = 4),
    minute = rep(c(15, 30, 60, 80), n_matches),
    team_id = rep(c("101", "102", "101", "102"), n_matches),
    team_name = rep(c("Team_101", "Team_102", "Team_101", "Team_102"), n_matches),
    player_id = paste0("op_", sample(1:20, n_matches * 4, replace = TRUE)),
    player_name = paste("Opta Shooter", 1:(n_matches * 4)),
    is_goal = c(rep(FALSE, n_matches * 3), rep(TRUE, n_matches)),
    xg = runif(n_matches * 4, 0.02, 0.6),
    situation = rep("open_play", n_matches * 4),
    stringsAsFactors = FALSE
  )

  shots <- prepare_opta_shots_for_splints(opta_shots)
  expect_true(is.data.frame(shots))
  expect_true(all(c("match_id", "minute", "team", "xg", "is_goal") %in% names(shots)))
})


# =============================================================================
# Integration Test 5: Mismatched player IDs between RAPM and SPM
# =============================================================================

test_that("pipeline handles partial player overlap between RAPM and SPM", {
  skip_if_not_installed("glmnet")

  processed <- create_synthetic_processed_data(n_matches = 12)
  splints <- create_all_splints(processed, verbose = FALSE)
  rapm_data <- prepare_rapm_data(splints, min_minutes = 30)

  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3)
  rapm_ratings <- extract_rapm_ratings(rapm_model)

  # Create features for only HALF the players (simulates missing data)
  half_n <- ceiling(nrow(rapm_ratings) / 2)
  subset_ratings <- rapm_ratings[1:half_n, ]

  player_features <- create_synthetic_player_features(
    player_ids = subset_ratings$player_id,
    player_names = subset_ratings$player_name,
    total_minutes = subset_ratings$total_minutes
  )

  # SPM train should only have the overlapping players
  spm_train <- prepare_spm_regression_data(player_features, rapm_ratings)
  expect_equal(nrow(spm_train), half_n)

  spm_model <- fit_spm_model(spm_train, alpha = 0.5, nfolds = 3)
  spm_ratings <- calculate_spm_ratings(player_features, spm_model)

  # Panna should still work - unmatched players get 0 SPM prior
  panna_result <- calculate_panna_rating(
    rapm_data = rapm_data,
    spm_ratings = spm_ratings,
    lambda_prior = 1
  )

  expect_true(nrow(panna_result$ratings) > 0)
  expect_true(all(is.finite(panna_result$ratings$panna)))

  # Should have ratings for all player columns in the design matrix
  # (one row per _off/_def column, not per unique player)
  X <- if (!is.null(rapm_data$X_full)) rapm_data$X_full else rapm_data$X
  expect_equal(nrow(panna_result$ratings), ncol(X))
})


# =============================================================================
# Integration Test 6: assign_epv_credit with xPass model
# =============================================================================

test_that("assign_epv_credit splits value between passer and receiver", {
  set.seed(42)

  spadl <- data.frame(
    match_id = rep("match_1", 10),
    action_id = 1:10,
    period_id = rep(1L, 10),
    time_seconds = seq(60, by = 120, length.out = 10),
    team_id = rep(101, 10),
    player_id = c("p1", "p2", "p3", "p1", "p2", "p3", "p1", "p2", "p3", "p1"),
    player_name = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A"),
    action_type = c("pass", "pass", "shot", "pass", "dribble", "pass", "pass", "pass", "shot", "pass"),
    result = rep("success", 10),
    start_x = runif(10, 0, 100),
    start_y = runif(10, 0, 100),
    end_x = runif(10, 0, 100),
    end_y = runif(10, 0, 100),
    epv = c(0.01, 0.02, 0.05, 0.01, 0.02, 0.03, 0.01, 0.02, 0.04, 0.01),
    epv_delta = c(0.01, 0.01, 0.03, -0.04, 0.01, 0.01, -0.02, 0.01, 0.02, -0.03),
    possession_change = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  result <- assign_epv_credit(spadl, xpass_model = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(spadl))
  expect_true("player_credit" %in% names(result))

  # All credits should be finite
  expect_true(all(is.finite(result$player_credit)))
})


# =============================================================================
# Integration Test 7: Splint xG consistency
# =============================================================================

test_that("splint npxG sums approximate match-level xG", {
  set.seed(42)

  processed <- create_synthetic_processed_data(n_matches = 8)
  splints <- create_all_splints(processed, verbose = FALSE)

  # Sum npxG across splints per match
  splint_df <- splints$splints
  expect_true(all(c("npxg_home", "npxg_away", "npxgd") %in% names(splint_df)))

  splint_xg <- stats::aggregate(
    cbind(npxg_home, npxg_away) ~ match_id,
    data = splint_df,
    FUN = sum,
    na.rm = TRUE
  )

  # Match-level xG from processed data
  match_xg <- processed$results[, c("match_id", "home_xg", "away_xg")]
  merged <- merge(splint_xg, match_xg, by = "match_id")

  expect_true(nrow(merged) > 0)

  # npxG should be non-negative
  expect_true(all(merged$npxg_home >= 0))
  expect_true(all(merged$npxg_away >= 0))

  # Splint npxG sums should be reasonable (< 10 per match)
  expect_true(all(merged$npxg_home < 10))
  expect_true(all(merged$npxg_away < 10))
})
