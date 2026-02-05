# Tests for EPV pipeline functions
# Covers: SPADL conversion, possession chains, EPV calculation

# =============================================================================
# Helper functions for creating mock data
# =============================================================================

#' Create mock Opta events for testing
create_mock_opta_events <- function(n_events = 20) {
  # Opta type_id reference:
  # 1 = pass, 3 = take on, 4 = foul, 10 = save, 13 = shot on target, 14 = shot off target
  # 15 = clearance, 16 = blocked pass
  # outcome: 1 = successful, 0 = unsuccessful

  data.frame(
    match_id = rep("test_match_1", n_events),
    event_id = 1:n_events,
    period_id = rep(1L, n_events),
    minute = seq(1, by = 2, length.out = n_events),
    second = rep(30, n_events),
    type_id = c(1, 1, 1, 3, 1, 13, 1, 1, 4, 1, 1, 1, 14, 1, 1, 1, 15, 1, 1, 1)[1:n_events],
    outcome = c(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1)[1:n_events],
    team_id = rep(c(101, 102), length.out = n_events),
    player_id = rep(c(1001, 1002, 1003, 2001, 2002, 2003), length.out = n_events),
    player_name = rep(c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F"), length.out = n_events),
    x = runif(n_events, 10, 90),
    y = runif(n_events, 10, 90),
    end_x = runif(n_events, 10, 90),
    end_y = runif(n_events, 10, 90),
    stringsAsFactors = FALSE
  )
}

#' Create mock SPADL actions for testing
create_mock_spadl_actions <- function(n_actions = 30, n_matches = 2) {
  actions_per_match <- n_actions %/% n_matches

  action_types <- c("pass", "dribble", "shot", "cross", "clearance", "tackle", "interception")
  bodyparts <- c("foot", "foot_right", "foot_left", "head")

  data.frame(
    match_id = rep(paste0("match_", 1:n_matches), each = actions_per_match),
    action_id = rep(1:actions_per_match, n_matches),
    period_id = rep(1L, n_actions),
    time_seconds = rep(seq(60, by = 60, length.out = actions_per_match), n_matches),
    team_id = rep(c(101, 102), length.out = n_actions),
    player_id = rep(c(1001, 1002, 1003, 2001, 2002, 2003), length.out = n_actions),
    player_name = rep(c("Player A", "Player B", "Player C", "Player D", "Player E", "Player F"), length.out = n_actions),
    start_x = runif(n_actions, 0, 100),
    start_y = runif(n_actions, 0, 100),
    end_x = runif(n_actions, 0, 100),
    end_y = runif(n_actions, 0, 100),
    action_type = sample(action_types, n_actions, replace = TRUE),
    result = sample(c("success", "fail"), n_actions, replace = TRUE, prob = c(0.7, 0.3)),
    bodypart = sample(bodyparts, n_actions, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' Create mock SPADL with possession chains
create_mock_spadl_with_chains <- function(n_actions = 50) {
  spadl <- create_mock_spadl_actions(n_actions, n_matches = 1)

  # Assign possession chains - change on turnovers
  chain_id <- 1
  chains <- numeric(n_actions)
  for (i in 1:n_actions) {
    chains[i] <- chain_id
    # Change chain on failed action or team change
    if (i < n_actions) {
      if (spadl$result[i] == "fail" || spadl$team_id[i] != spadl$team_id[i + 1]) {
        chain_id <- chain_id + 1
      }
    }
  }

  spadl$chain_id <- chains
  spadl
}


# =============================================================================
# Tests for SPADL conversion (spadl_conversion.R)
# =============================================================================

test_that("convert_opta_to_spadl converts events to SPADL format", {
  mock_events <- create_mock_opta_events(20)

  result <- convert_opta_to_spadl(mock_events)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)

  # Check required SPADL columns exist
  expected_cols <- c("match_id", "action_id", "period_id", "time_seconds",
                     "team_id", "player_id", "start_x", "start_y",
                     "end_x", "end_y", "action_type", "result", "bodypart")
  expect_true(all(expected_cols %in% names(result)))
})

test_that("convert_opta_to_spadl handles empty input", {
  empty_events <- data.frame(
    match_id = character(),
    event_id = integer(),
    type_id = integer(),
    stringsAsFactors = FALSE
  )

  # Function throws error on empty input
  expect_error(convert_opta_to_spadl(empty_events), "No events provided")
})

test_that("calculate_distance_to_goal computes correct distances", {
  # Center of goal line
  expect_equal(calculate_distance_to_goal(100, 50), 0)

  # Penalty spot (approximately 11 meters)
  dist <- calculate_distance_to_goal(88, 50)
  expect_true(dist > 10 && dist < 15)

  # Far corner
  dist <- calculate_distance_to_goal(0, 0)
  expect_true(dist > 100)

  # Handles vectors
  dists <- calculate_distance_to_goal(c(100, 50), c(50, 50))
  expect_length(dists, 2)
})

test_that("calculate_angle_to_goal computes reasonable angles", {
  # Direct center - maximum angle
  angle_center <- calculate_angle_to_goal(88, 50)

  # Wide position - smaller angle
  angle_wide <- calculate_angle_to_goal(88, 10)

  expect_true(angle_center > angle_wide)
  expect_true(angle_center > 0 && angle_center <= pi/2)
})

test_that("get_pitch_zone classifies positions correctly", {
  # Zones are numeric: 1-3 (def), 4-6 (mid), 7-9 (att)
  # With y component: 1 (left), 2 (center), 3 (right)

  # Defensive zone, center (x=10, y=50)
  zone_def <- get_pitch_zone(10, 50)
  expect_true(zone_def %in% 1:3)

  # Middle zone, center (x=50, y=50)
  zone_mid <- get_pitch_zone(50, 50)
  expect_true(zone_mid %in% 4:6)

  # Attacking zone, center (x=90, y=50)
  zone_att <- get_pitch_zone(90, 50)
  expect_true(zone_att %in% 7:9)

  # Attacking > Middle > Defensive
  expect_true(zone_att > zone_mid)
  expect_true(zone_mid > zone_def)

  # Handles vectors
  zones <- get_pitch_zone(c(10, 50, 90), c(50, 50, 50))
  expect_length(zones, 3)
})

test_that("is_in_penalty_area detects penalty area correctly", {
  # Inside attacking penalty area (x > 83, y between 21.1 and 78.9)
  expect_true(is_in_penalty_area(90, 50, attacking = TRUE))

  # Outside penalty area
  expect_false(is_in_penalty_area(70, 50, attacking = TRUE))

  # Inside defensive penalty area
  expect_true(is_in_penalty_area(10, 50, attacking = FALSE))
})

test_that("is_in_final_third detects final third correctly", {
  # Attacking final third
  expect_true(is_in_final_third(80, attacking = TRUE))
  expect_false(is_in_final_third(50, attacking = TRUE))

  # Defensive final third
  expect_true(is_in_final_third(20, attacking = FALSE))
})


# =============================================================================
# Tests for possession chains (possession_chains.R)
# =============================================================================

test_that("create_possession_chains assigns chain IDs", {
  spadl <- create_mock_spadl_actions(30, n_matches = 1)

  result <- create_possession_chains(spadl)

  expect_true("chain_id" %in% names(result))
  expect_true(all(!is.na(result$chain_id)))
  expect_true(min(result$chain_id) >= 1)
})

test_that("create_possession_chains handles team changes", {
  spadl <- data.frame(
    match_id = rep("match_1", 6),
    action_id = 1:6,
    period_id = rep(1L, 6),
    time_seconds = 1:6 * 60,
    team_id = c(101, 101, 101, 102, 102, 101),  # Team changes at action 4 and 6
    player_id = 1:6,
    player_name = paste0("Player_", 1:6),
    start_x = rep(50, 6),
    start_y = rep(50, 6),
    end_x = rep(60, 6),
    end_y = rep(50, 6),
    action_type = rep("pass", 6),
    result = rep("success", 6),
    bodypart = rep("foot", 6),
    stringsAsFactors = FALSE
  )

  result <- create_possession_chains(spadl)

  # Should have at least 3 different chains (team changes twice)
  expect_true(length(unique(result$chain_id)) >= 3)
})

test_that("classify_chain_outcomes labels chains correctly", {
  spadl <- create_mock_spadl_with_chains(30)

  # Add a shot to create a clear outcome
  spadl$action_type[10] <- "shot"
  spadl$result[10] <- "success"

  result <- classify_chain_outcomes(spadl)

  # Check for 'outcome' column (not 'chain_outcome')
  expect_true("outcome" %in% names(result))
  expect_true(all(result$outcome %in% c("goal", "shot", "turnover", "foul", "out_of_play")))
})

test_that("label_actions_with_outcomes adds outcome labels to actions", {
  spadl <- create_mock_spadl_with_chains(30)
  chain_outcomes <- classify_chain_outcomes(spadl)

  result <- label_actions_with_outcomes(spadl, chain_outcomes)

  expect_true("chain_outcome" %in% names(result))
  expect_equal(nrow(result), nrow(spadl))
})

test_that("calculate_chain_stats summarizes chains correctly", {
  spadl <- create_mock_spadl_with_chains(50)
  chain_outcomes <- classify_chain_outcomes(spadl)

  result <- calculate_chain_stats(chain_outcomes)

  # Returns a list with total_chains (not n_chains)
  expect_type(result, "list")
  expect_true("total_chains" %in% names(result))
  expect_true("goal_rate" %in% names(result))
  expect_true(result$total_chains > 0)
})


# =============================================================================
# Tests for EPV labels (epv_model.R)
# =============================================================================

test_that("create_next_goal_labels generates valid labels", {
  spadl <- create_mock_spadl_with_chains(50)

  result <- create_next_goal_labels(spadl)

  expect_true("next_goal_label" %in% names(result))
  # Labels should be 0 (no goal), 1 (team scores), 2 (opponent scores)
  expect_true(all(result$next_goal_label %in% c(0, 1, 2) | is.na(result$next_goal_label)))
})

test_that("create_next_xg_labels generates numeric labels", {
  spadl <- create_mock_spadl_with_chains(50)

  result <- create_next_xg_labels(spadl)

  expect_true("next_xg_label" %in% names(result))
  expect_type(result$next_xg_label, "double")
})

test_that("estimate_simple_xg returns values in valid range", {
  # Inside penalty area
  xg_close <- estimate_simple_xg(90, 50)
  expect_true(xg_close >= 0.01 && xg_close <= 0.75)

  # Far from goal
  xg_far <- estimate_simple_xg(50, 50)
  expect_true(xg_far >= 0.01 && xg_far <= 0.75)

  # Close shot should have higher xG
  expect_true(xg_close > xg_far)

  # Handles vectors
  xg_vec <- estimate_simple_xg(c(90, 50), c(50, 50))
  expect_length(xg_vec, 2)
})


# =============================================================================
# Tests for EPV model fitting (epv_model.R)
# =============================================================================

test_that("fit_epv_model requires xgboost package", {
  skip_if_not_installed("xgboost")

  # Create features with all expected columns from EPV feature set
  n <- 200
  features <- data.frame(
    match_id = rep("match_1", n),
    action_id = 1:n,
    # Location features
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    distance_to_goal = runif(n, 0, 120),
    angle_to_goal = runif(n, 0, 1),
    zone_id = sample(1:9, n, replace = TRUE),
    in_penalty_area = sample(0:1, n, replace = TRUE),
    in_final_third = sample(0:1, n, replace = TRUE),
    in_own_third = sample(0:1, n, replace = TRUE),
    in_mid_third = sample(0:1, n, replace = TRUE),
    y_left = sample(0:1, n, replace = TRUE),
    y_center = sample(0:1, n, replace = TRUE),
    y_right = sample(0:1, n, replace = TRUE),
    # Movement features
    dx = rnorm(n, 5, 10),
    dy = rnorm(n, 0, 5),
    move_distance = runif(n, 0, 50),
    dist_delta = rnorm(n, 0, 20),
    # Action features
    result_success = sample(0:1, n, replace = TRUE),
    is_foot = sample(0:1, n, replace = TRUE),
    is_head = sample(0:1, n, replace = TRUE),
    is_pass = sample(0:1, n, replace = TRUE),
    is_shot = sample(0:1, n, replace = TRUE),
    is_take_on = sample(0:1, n, replace = TRUE),
    is_tackle = sample(0:1, n, replace = TRUE),
    is_interception = sample(0:1, n, replace = TRUE),
    is_clearance = sample(0:1, n, replace = TRUE),
    is_aerial = sample(0:1, n, replace = TRUE),
    is_foul = sample(0:1, n, replace = TRUE),
    is_ball_recovery = sample(0:1, n, replace = TRUE),
    # Chain features
    seconds_since_chain_start = runif(n, 0, 60),
    action_in_chain = sample(1:10, n, replace = TRUE),
    # Context
    time_normalized = runif(n, 0, 1),
    period_id = sample(1:2, n, replace = TRUE),
    # Label
    next_goal_label = sample(c(0, 1, 2), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Should work with goal method
  model <- fit_epv_model(
    features = features,
    labels = features,
    method = "goal",
    nrounds = 10,
    early_stopping_rounds = 5,
    verbose = 0
  )

  expect_type(model, "list")
  expect_true("model" %in% names(model))
  expect_true("method" %in% names(model))
  expect_equal(model$method, "goal")
})

test_that("fit_epv_model works with xg method", {
  skip_if_not_installed("xgboost")

  # Create features with all expected columns
  n <- 200
  features <- data.frame(
    match_id = rep("match_1", n),
    action_id = 1:n,
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    distance_to_goal = runif(n, 0, 120),
    angle_to_goal = runif(n, 0, 1),
    zone_id = sample(1:9, n, replace = TRUE),
    in_penalty_area = sample(0:1, n, replace = TRUE),
    in_final_third = sample(0:1, n, replace = TRUE),
    in_own_third = sample(0:1, n, replace = TRUE),
    in_mid_third = sample(0:1, n, replace = TRUE),
    y_left = sample(0:1, n, replace = TRUE),
    y_center = sample(0:1, n, replace = TRUE),
    y_right = sample(0:1, n, replace = TRUE),
    dx = rnorm(n, 5, 10),
    dy = rnorm(n, 0, 5),
    move_distance = runif(n, 0, 50),
    dist_delta = rnorm(n, 0, 20),
    result_success = sample(0:1, n, replace = TRUE),
    is_foot = sample(0:1, n, replace = TRUE),
    is_head = sample(0:1, n, replace = TRUE),
    is_pass = sample(0:1, n, replace = TRUE),
    is_shot = sample(0:1, n, replace = TRUE),
    is_take_on = sample(0:1, n, replace = TRUE),
    is_tackle = sample(0:1, n, replace = TRUE),
    is_interception = sample(0:1, n, replace = TRUE),
    is_clearance = sample(0:1, n, replace = TRUE),
    is_aerial = sample(0:1, n, replace = TRUE),
    is_foul = sample(0:1, n, replace = TRUE),
    is_ball_recovery = sample(0:1, n, replace = TRUE),
    seconds_since_chain_start = runif(n, 0, 60),
    action_in_chain = sample(1:10, n, replace = TRUE),
    time_normalized = runif(n, 0, 1),
    period_id = sample(1:2, n, replace = TRUE),
    next_xg_label = rnorm(n, 0, 0.1),
    stringsAsFactors = FALSE
  )

  model <- fit_epv_model(
    features = features,
    labels = features,
    method = "xg",
    nrounds = 10,
    early_stopping_rounds = 5,
    verbose = 0
  )

  expect_equal(model$method, "xg")
})


# =============================================================================
# Tests for EPV calculation (epv_model.R)
# =============================================================================

test_that("calculate_action_epv computes EPV values", {
  skip_if_not_installed("xgboost")
  skip("calculate_action_epv requires complex integration - tested via integration tests")
})


# =============================================================================
# Tests for EPV credit assignment (epv_model.R)
# =============================================================================

test_that("assign_epv_credit handles passes correctly", {
  # Create SPADL with EPV values - need epv_delta not delta_epv
  spadl <- data.frame(
    match_id = rep("match_1", 10),
    action_id = 1:10,
    team_id = rep(101, 10),
    player_id = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
    player_name = c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A"),
    action_type = c("pass", "pass", "shot", "pass", "dribble", "pass", "pass", "pass", "shot", "pass"),
    result = rep("success", 10),
    start_x = runif(10, 0, 100),
    start_y = runif(10, 0, 100),
    end_x = runif(10, 0, 100),
    end_y = runif(10, 0, 100),
    epv = c(0.01, 0.02, 0.05, 0.01, 0.02, 0.03, 0.01, 0.02, 0.04, 0.01),
    epv_delta = c(0.01, 0.01, 0.03, -0.04, 0.01, 0.01, -0.02, 0.01, 0.02, -0.03),
    stringsAsFactors = FALSE
  )

  result <- assign_epv_credit(spadl, xpass_model = NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(spadl))
})


# =============================================================================
# Tests for player EPV aggregation (epv_model.R)
# =============================================================================

test_that("aggregate_player_epv summarizes by player", {
  # Create SPADL with EPV values including player_credit
  # Use character player_id for consistency with data.table merge
  spadl <- data.frame(
    match_id = rep(c("match_1", "match_2"), each = 50),
    action_id = rep(1:50, 2),
    team_id = rep(c(101, 102), length.out = 100),
    player_id = rep(c("p1", "p2", "p3", "p4", "p5"), length.out = 100),
    player_name = rep(c("Player A", "Player B", "Player C", "Player D", "Player E"), length.out = 100),
    action_type = sample(c("pass", "dribble", "shot", "tackle"), 100, replace = TRUE),
    result = sample(c("success", "fail"), 100, replace = TRUE),
    start_x = runif(100, 0, 100),
    start_y = runif(100, 0, 100),
    end_x = runif(100, 0, 100),
    end_y = runif(100, 0, 100),
    epv = runif(100, 0, 0.1),
    epv_delta = rnorm(100, 0, 0.02),
    player_credit = rnorm(100, 0, 0.02),  # Required by aggregate_player_epv
    stringsAsFactors = FALSE
  )

  # Create lineups with minutes - player_id must be same type as in spadl
  lineups <- data.frame(
    match_id = rep(c("match_1", "match_2"), each = 5),
    player_id = rep(c("p1", "p2", "p3", "p4", "p5"), 2),
    player_name = rep(c("Player A", "Player B", "Player C", "Player D", "Player E"), 2),
    minutes_played = rep(90, 10),
    stringsAsFactors = FALSE
  )

  result <- aggregate_player_epv(spadl, lineups, min_minutes = 0)

  expect_s3_class(result, "data.frame")
  expect_true("player_id" %in% names(result) || "player_name" %in% names(result))
  expect_true("epv_as_actor" %in% names(result) || "total_epv" %in% names(result))
})


# =============================================================================
# Tests for EPV model persistence (epv_model.R)
# =============================================================================

test_that("save_epv_model and load_epv_model work correctly", {
  skip_if_not_installed("xgboost")

  # Create features with all expected columns
  n <- 100
  features <- data.frame(
    match_id = rep("match_1", n),
    action_id = 1:n,
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    distance_to_goal = runif(n, 0, 120),
    angle_to_goal = runif(n, 0, 1),
    zone_id = sample(1:9, n, replace = TRUE),
    in_penalty_area = sample(0:1, n, replace = TRUE),
    in_final_third = sample(0:1, n, replace = TRUE),
    in_own_third = sample(0:1, n, replace = TRUE),
    in_mid_third = sample(0:1, n, replace = TRUE),
    y_left = sample(0:1, n, replace = TRUE),
    y_center = sample(0:1, n, replace = TRUE),
    y_right = sample(0:1, n, replace = TRUE),
    dx = rnorm(n, 5, 10),
    dy = rnorm(n, 0, 5),
    move_distance = runif(n, 0, 50),
    dist_delta = rnorm(n, 0, 20),
    result_success = sample(0:1, n, replace = TRUE),
    is_foot = sample(0:1, n, replace = TRUE),
    is_head = sample(0:1, n, replace = TRUE),
    is_pass = sample(0:1, n, replace = TRUE),
    is_shot = sample(0:1, n, replace = TRUE),
    is_take_on = sample(0:1, n, replace = TRUE),
    is_tackle = sample(0:1, n, replace = TRUE),
    is_interception = sample(0:1, n, replace = TRUE),
    is_clearance = sample(0:1, n, replace = TRUE),
    is_aerial = sample(0:1, n, replace = TRUE),
    is_foul = sample(0:1, n, replace = TRUE),
    is_ball_recovery = sample(0:1, n, replace = TRUE),
    seconds_since_chain_start = runif(n, 0, 60),
    action_in_chain = sample(1:10, n, replace = TRUE),
    time_normalized = runif(n, 0, 1),
    period_id = sample(1:2, n, replace = TRUE),
    next_goal_label = sample(c(0, 1, 2), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  model <- fit_epv_model(
    features = features,
    labels = features,
    method = "goal",
    nrounds = 5,
    early_stopping_rounds = 3,
    verbose = 0
  )

  # Save to temp file
  temp_path <- tempfile(fileext = ".rds")
  save_epv_model(model, path = temp_path)

  expect_true(file.exists(temp_path))

  # Load it back
  loaded_model <- load_epv_model(path = temp_path)

  expect_equal(loaded_model$method, model$method)
  expect_equal(loaded_model$feature_cols, model$feature_cols)

  # Clean up
  unlink(temp_path)
})


# =============================================================================
# Tests for EPV model validation (epv_model.R)
# =============================================================================

test_that("validate_epv_model computes metrics", {
  # Create SPADL with EPV and actual outcomes
  spadl <- data.frame(
    match_id = rep("match_1", 100),
    action_id = 1:100,
    team_id = rep(c(101, 102), 50),
    epv = runif(100, -0.1, 0.1),
    delta_epv = rnorm(100, 0, 0.02),
    chain_outcome = sample(c("goal", "shot", "turnover", "other"), 100, replace = TRUE),
    action_type = sample(c("pass", "shot", "dribble"), 100, replace = TRUE),
    stringsAsFactors = FALSE
  )

  result <- validate_epv_model(spadl)

  expect_type(result, "list")
})


# =============================================================================
# Tests for deprecated functions
# =============================================================================

test_that("deprecated EPV functions emit warnings", {
  skip_if_not_installed("xgboost")

  # Create features with all expected columns
  n <- 100
  features <- data.frame(
    match_id = rep("match_1", n),
    action_id = 1:n,
    x = runif(n, 0, 100),
    y = runif(n, 0, 100),
    distance_to_goal = runif(n, 0, 120),
    angle_to_goal = runif(n, 0, 1),
    zone_id = sample(1:9, n, replace = TRUE),
    in_penalty_area = sample(0:1, n, replace = TRUE),
    in_final_third = sample(0:1, n, replace = TRUE),
    in_own_third = sample(0:1, n, replace = TRUE),
    in_mid_third = sample(0:1, n, replace = TRUE),
    y_left = sample(0:1, n, replace = TRUE),
    y_center = sample(0:1, n, replace = TRUE),
    y_right = sample(0:1, n, replace = TRUE),
    dx = rnorm(n, 5, 10),
    dy = rnorm(n, 0, 5),
    move_distance = runif(n, 0, 50),
    dist_delta = rnorm(n, 0, 20),
    result_success = sample(0:1, n, replace = TRUE),
    is_foot = sample(0:1, n, replace = TRUE),
    is_head = sample(0:1, n, replace = TRUE),
    is_pass = sample(0:1, n, replace = TRUE),
    is_shot = sample(0:1, n, replace = TRUE),
    is_take_on = sample(0:1, n, replace = TRUE),
    is_tackle = sample(0:1, n, replace = TRUE),
    is_interception = sample(0:1, n, replace = TRUE),
    is_clearance = sample(0:1, n, replace = TRUE),
    is_aerial = sample(0:1, n, replace = TRUE),
    is_foul = sample(0:1, n, replace = TRUE),
    is_ball_recovery = sample(0:1, n, replace = TRUE),
    seconds_since_chain_start = runif(n, 0, 60),
    action_in_chain = sample(1:10, n, replace = TRUE),
    time_normalized = runif(n, 0, 1),
    period_id = sample(1:2, n, replace = TRUE),
    next_goal_label = sample(c(0, 1, 2), n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # fit_epv_scoring_model should warn
  expect_warning(
    fit_epv_scoring_model(
      features = features,
      labels = features,
      nrounds = 5,
      early_stopping_rounds = 3,
      verbose = 0
    ),
    "deprecated"
  )
})
