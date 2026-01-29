# Tests for Opta pipeline functions
# Tests the complete Opta data flow: loading -> splint creation -> SPM

# Access internal functions for testing
prepare_opta_events_for_splints <- panna:::prepare_opta_events_for_splints
prepare_opta_lineups_for_splints <- panna:::prepare_opta_lineups_for_splints
prepare_opta_shots_for_splints <- panna:::prepare_opta_shots_for_splints
extract_season_from_date <- panna:::extract_season_from_date
count_events_before <- panna:::count_events_before
count_events_in_splint <- panna:::count_events_in_splint

# ============================================================================
# OPTA DATA ADAPTER TESTS
# ============================================================================

test_that("prepare_opta_events_for_splints handles empty data", {
  result <- prepare_opta_events_for_splints(NULL)
  expect_equal(nrow(result), 0)
  expect_true(all(c("match_id", "minute", "is_goal", "is_sub", "is_red_card") %in% names(result)))

  result2 <- prepare_opta_events_for_splints(data.frame())
  expect_equal(nrow(result2), 0)
})

test_that("prepare_opta_events_for_splints correctly classifies event types", {
  opta_events <- data.frame(
    match_id = rep("match1", 4),
    event_type = c("goal", "substitution", "red_card", "yellow_card"),
    minute = c(30, 60, 75, 45),
    second = c(0, 30, 0, 15),
    team_id = c("team1", "team1", "team2", "team2"),
    team_position = c("home", "home", "away", "away"),
    player_id = paste0("player", 1:4),
    player_name = paste0("Player ", 1:4),
    stringsAsFactors = FALSE
  )

  result <- prepare_opta_events_for_splints(opta_events)

  expect_equal(nrow(result), 4)
  expect_equal(sum(result$is_goal), 1)
  expect_equal(sum(result$is_sub), 1)
  expect_equal(sum(result$is_red_card), 1)

  # Check is_home detection
  expect_equal(sum(result$is_home), 2)
  expect_equal(sum(!result$is_home), 2)
})

test_that("prepare_opta_lineups_for_splints handles empty data", {
  result <- prepare_opta_lineups_for_splints(NULL)
  expect_equal(nrow(result), 0)
  expect_true(all(c("match_id", "player_id", "is_starter", "on_minute", "off_minute") %in% names(result)))
})

test_that("prepare_opta_lineups_for_splints correctly calculates on/off minutes", {
  opta_lineups <- data.frame(
    match_id = rep("match1", 4),
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("Starter 1", "Starter 2", "Sub 1", "Sub 2"),
    team_id = c("team1", "team1", "team1", "team1"),
    team_name = rep("Home FC", 4),
    team_position = rep("home", 4),
    is_starter = c(TRUE, TRUE, FALSE, FALSE),
    minutes_played = c(90, 60, 30, 20),
    sub_on_minute = c(0, 0, 60, 70),
    sub_off_minute = c(0, 60, 0, 0),  # 0 = played to end
    stringsAsFactors = FALSE
  )

  result <- prepare_opta_lineups_for_splints(opta_lineups)

  expect_equal(nrow(result), 4)

  # Starters should have on_minute = 0
  starters <- result[result$is_starter, ]
  expect_true(all(starters$on_minute == 0))

  # Subs should have on_minute from sub_on_minute
  subs <- result[!result$is_starter, ]
  expect_equal(subs$on_minute[subs$player_id == "p3"], 60)
  expect_equal(subs$on_minute[subs$player_id == "p4"], 70)

  # Players who played to end should have off_minute = 90
  expect_equal(result$off_minute[result$player_id == "p1"], 90)
})

test_that("prepare_opta_shots_for_splints handles empty data", {
  result <- prepare_opta_shots_for_splints(NULL)
  expect_equal(nrow(result), 0)
  expect_true(all(c("match_id", "minute", "xg", "is_goal", "is_penalty") %in% names(result)))
})

test_that("prepare_opta_shots_for_splints correctly uses goals as xG proxy", {
  opta_shots <- data.frame(
    match_id = rep("match1", 4),
    event_id = 1:4,
    minute = c(10, 30, 55, 80),
    second = c(0, 15, 30, 45),
    player_id = paste0("p", 1:4),
    player_name = paste0("Player ", 1:4),
    team_id = rep("team1", 4),
    team_name = rep("Home FC", 4),
    x = c(85, 90, 88, 92),
    y = c(40, 50, 45, 55),
    outcome = c(0, 1, 0, 1),
    is_goal = c(FALSE, TRUE, FALSE, TRUE),
    type_id = c(15, 16, 15, 16),  # 15=miss, 16=goal
    situation = c("OpenPlay", "OpenPlay", "Penalty", "OpenPlay"),
    stringsAsFactors = FALSE
  )

  # Without use_goals_as_xg
  result <- prepare_opta_shots_for_splints(opta_shots, use_goals_as_xg = FALSE)
  expect_true(all(is.na(result$xg)))

  # With use_goals_as_xg
  result2 <- prepare_opta_shots_for_splints(opta_shots, use_goals_as_xg = TRUE)
  expect_equal(sum(result2$xg), 2)  # Two goals = 2 xG
  expect_true(result2$xg[result2$is_goal][1] == 1)
  expect_true(result2$xg[!result2$is_goal][1] == 0)

  # Check penalty detection
  expect_equal(sum(result2$is_penalty), 1)
})

# ============================================================================
# OPTA SPLINT CREATION TESTS
# ============================================================================

test_that("extract_season_from_date works correctly", {
  # Aug-Dec = same year starts season
  expect_equal(extract_season_from_date("2024-08-15"), "2024-2025")
  expect_equal(extract_season_from_date("2024-12-25"), "2024-2025")

  # Jan-Jul = previous year started season
  expect_equal(extract_season_from_date("2025-01-10"), "2024-2025")
  expect_equal(extract_season_from_date("2025-05-20"), "2024-2025")
  expect_equal(extract_season_from_date("2025-07-31"), "2024-2025")

  # Edge case: August starts new season
  expect_equal(extract_season_from_date("2025-08-01"), "2025-2026")

  # Handle NULL/NA
  expect_true(is.na(extract_season_from_date(NULL)))
})

test_that("create_opta_processed_data creates valid structure", {
  # Mock data
  mock_lineups <- data.frame(
    match_id = rep("match1", 22),
    match_date = rep("2024-09-15", 22),
    player_id = paste0("p", 1:22),
    player_name = paste0("Player ", 1:22),
    team_id = c(rep("team1", 11), rep("team2", 11)),
    team_name = c(rep("Home FC", 11), rep("Away FC", 11)),
    team_position = c(rep("home", 11), rep("away", 11)),
    is_starter = rep(TRUE, 22),
    minutes_played = rep(90, 22),
    sub_on_minute = rep(0, 22),
    sub_off_minute = rep(0, 22),
    stringsAsFactors = FALSE
  )

  result <- create_opta_processed_data(mock_lineups)

  expect_true(is.list(result))
  expect_true(all(c("lineups", "events", "shooting", "results") %in% names(result)))
  expect_equal(nrow(result$lineups), 22)
  expect_equal(nrow(result$results), 1)
  expect_equal(result$results$home_team[1], "Home FC")
  expect_equal(result$results$away_team[1], "Away FC")
})

test_that("create_all_splints works with Opta processed data", {
  # Mock complete Opta data
  mock_lineups <- data.frame(
    match_id = rep("match1", 22),
    match_date = rep("2024-09-15", 22),
    player_id = paste0("p", 1:22),
    player_name = paste0("Player ", 1:22),
    team_id = c(rep("team1", 11), rep("team2", 11)),
    team_name = c(rep("Home FC", 11), rep("Away FC", 11)),
    team_position = c(rep("home", 11), rep("away", 11)),
    is_starter = rep(TRUE, 22),
    minutes_played = rep(90, 22),
    sub_on_minute = rep(0, 22),
    sub_off_minute = rep(0, 22),
    stringsAsFactors = FALSE
  )

  mock_events <- data.frame(
    match_id = rep("match1", 2),
    event_type = c("goal", "substitution"),
    minute = c(30, 60),
    second = c(0, 0),
    team_id = c("team1", "team2"),
    team_position = c("home", "away"),
    player_id = c("p1", "p12"),
    player_name = c("Player 1", "Player 12"),
    stringsAsFactors = FALSE
  )

  mock_shots <- data.frame(
    match_id = rep("match1", 3),
    event_id = 1:3,
    minute = c(10, 30, 70),
    second = c(0, 0, 0),
    player_id = c("p1", "p1", "p12"),
    player_name = c("Player 1", "Player 1", "Player 12"),
    team_id = c("team1", "team1", "team2"),
    team_name = c("Home FC", "Home FC", "Away FC"),
    x = c(85, 90, 88),
    y = c(40, 50, 45),
    outcome = c(0, 1, 0),
    is_goal = c(FALSE, TRUE, FALSE),
    type_id = c(15, 16, 15),
    situation = rep("OpenPlay", 3),
    stringsAsFactors = FALSE
  )

  processed <- create_opta_processed_data(
    mock_lineups,
    mock_events,
    mock_shots,
    use_goals_as_xg = TRUE
  )

  splints <- create_all_splints(processed, verbose = FALSE)

  expect_true(is.list(splints))
  expect_true(all(c("splints", "players", "match_info") %in% names(splints)))
  expect_gt(nrow(splints$splints), 0)
  expect_gt(nrow(splints$players), 0)

  # Check splint structure
  expect_true("npxg_home" %in% names(splints$splints))
  expect_true("npxg_away" %in% names(splints$splints))
  expect_true("splint_id" %in% names(splints$splints))
})

# ============================================================================
# OPTA SPM TESTS
# ============================================================================

test_that("aggregate_opta_stats handles minimum data requirements", {
  # Empty data should warn and return NULL
  expect_warning(result <- aggregate_opta_stats(NULL))
  expect_null(result)

  # Data with insufficient minutes should warn
  small_data <- data.frame(
    match_id = "match1",
    player_name = "Player 1",
    minsPlayed = 100,  # Below default 450 threshold
    goals = 1,
    totalScoringAtt = 3,
    stringsAsFactors = FALSE
  )
  expect_warning(result <- aggregate_opta_stats(small_data, min_minutes = 450))
  expect_null(result)
})

test_that("aggregate_opta_stats creates per-90 features", {
  # Mock Opta stats
  opta_stats <- data.frame(
    match_id = c("m1", "m2", "m3", "m4", "m5"),
    player_name = rep("Test Player", 5),
    minsPlayed = c(90, 90, 90, 90, 90),  # 450 total
    goals = c(1, 0, 2, 0, 1),
    totalScoringAtt = c(3, 2, 4, 1, 3),
    accuratePass = c(50, 45, 55, 40, 48),
    totalPass = c(60, 55, 65, 50, 58),
    wonTackle = c(2, 3, 1, 2, 2),
    totalTackle = c(3, 4, 2, 3, 3),
    aerialWon = c(1, 2, 1, 0, 2),
    aerialLost = c(1, 0, 1, 1, 0),
    touches = c(50, 48, 55, 45, 52),
    dispossessed = c(1, 2, 1, 0, 1),
    stringsAsFactors = FALSE
  )

  result <- aggregate_opta_stats(opta_stats, min_minutes = 400)

  expect_equal(nrow(result), 1)

  # Check totals
  expect_equal(result$total_minutes, 450)
  expect_equal(result$n_matches, 5)
  expect_equal(result$goals, 4)  # 1+0+2+0+1

  # Check per-90 rates
  expect_equal(result$goals_p90, 4 / (450/90))  # 0.8 goals per 90
  expect_equal(result$shots_p90, 13 / (450/90))  # 13 shots / 5 matches

  # Check derived features
  expect_true("pass_accuracy" %in% names(result))
  expect_true("tackle_success" %in% names(result))
  expect_true("aerial_success" %in% names(result))

  # Verify success rate calculations
  expected_pass_acc <- (50+45+55+40+48) / (60+55+65+50+58)
  expect_equal(result$pass_accuracy, expected_pass_acc, tolerance = 0.001)
})

test_that("fit_spm_opta fits model with correct features", {
  # Create mock aggregated data
  set.seed(42)
  n <- 50
  mock_data <- data.frame(
    player_id = paste0("p", 1:n),
    player_name = paste0("Player ", 1:n),
    total_minutes = runif(n, 500, 3000),
    n_matches = sample(10:38, n, replace = TRUE),
    # Per-90 features
    goals_p90 = runif(n, 0, 0.8),
    assists_p90 = runif(n, 0, 0.5),
    shots_p90 = runif(n, 0, 4),
    passes_p90 = runif(n, 30, 80),
    tackles_p90 = runif(n, 0, 4),
    interceptions_p90 = runif(n, 0, 3),
    # Success rates
    shot_accuracy = runif(n, 0.2, 0.6),
    pass_accuracy = runif(n, 0.6, 0.9),
    tackle_success = runif(n, 0.3, 0.8),
    # Mock RAPM
    rapm = rnorm(n, 0, 0.5),
    stringsAsFactors = FALSE
  )
  # Add some correlation between goals and RAPM
  mock_data$rapm <- mock_data$rapm + 0.5 * scale(mock_data$goals_p90)

  model <- fit_spm_opta(mock_data, nfolds = 3)

  expect_s3_class(model, "cv.glmnet")
  expect_true(!is.null(model$panna_metadata))
  expect_equal(model$panna_metadata$type, "spm")

  # Check that per-90 features were used
  p90_features <- sum(grepl("_p90$", model$panna_metadata$predictor_cols))
  expect_gt(p90_features, 0)
})

# ============================================================================
# COUNT HELPER FUNCTION TESTS
# ============================================================================

test_that("count_events_before correctly counts cumulative events at splint START", {
  events <- data.frame(
    minute = c(10, 30, 60, 80),
    is_home = c(TRUE, FALSE, TRUE, FALSE)
  )
  boundaries <- c(0, 25, 50, 75, 91)

  counts <- count_events_before(events, boundaries)

  # Counts events BEFORE the start of each splint (boundaries[i])
  # Splint 1 starts at 0: 0 events before minute 0
  expect_equal(counts$home[1], 0)
  expect_equal(counts$away[1], 0)

  # Splint 2 starts at 25: event at 10 is before 25
  expect_equal(counts$home[2], 1)
  expect_equal(counts$away[2], 0)

  # Splint 3 starts at 50: events at 10, 30 are before 50
  expect_equal(counts$home[3], 1)
  expect_equal(counts$away[3], 1)

  # Splint 4 starts at 75: events at 10, 30, 60 are before 75
  expect_equal(counts$home[4], 2)
  expect_equal(counts$away[4], 1)
})

test_that("count_events_in_splint correctly counts per-splint events", {
  events <- data.frame(
    minute = c(10, 30, 60, 80),
    is_home = c(TRUE, FALSE, TRUE, FALSE)
  )
  boundaries <- c(0, 25, 50, 75, 91)

  counts <- count_events_in_splint(events, boundaries)

  # Splint 1 (0-25): 1 home event (at 10)
  expect_equal(counts$home[1], 1)
  expect_equal(counts$away[1], 0)

  # Splint 2 (25-50): 1 away event (at 30)
  expect_equal(counts$home[2], 0)
  expect_equal(counts$away[2], 1)

  # Splint 3 (50-75): 1 home event (at 60)
  expect_equal(counts$home[3], 1)
  expect_equal(counts$away[3], 0)

  # Splint 4 (75-91): 1 away event (at 80)
  expect_equal(counts$home[4], 0)
  expect_equal(counts$away[4], 1)
})

test_that("count helper functions handle empty data", {
  boundaries <- c(0, 45, 90)

  counts_before <- count_events_before(NULL, boundaries)
  expect_equal(counts_before$home, c(0, 0))
  expect_equal(counts_before$away, c(0, 0))

  counts_in <- count_events_in_splint(NULL, boundaries)
  expect_equal(counts_in$home, c(0, 0))
  expect_equal(counts_in$away, c(0, 0))

  empty_df <- data.frame(minute = numeric(0), is_home = logical(0))
  counts_before2 <- count_events_before(empty_df, boundaries)
  expect_equal(counts_before2$home, c(0, 0))
})
