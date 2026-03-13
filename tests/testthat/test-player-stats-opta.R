# Tests for Opta player statistics functions (player_stats_opta.R)
#
# Covers the public API functions and internal .aggregate_opta_player_stats()
# helper. Uses synthetic data with mocked loaders to avoid network calls.

# =============================================================================
# Synthetic Data Helpers
# =============================================================================

# Minimal Opta stats data with controlled values for deterministic tests
create_controlled_opta_data <- function() {
  data.frame(
    player_name = c("Player A", "Player A", "Player A",
                    "Player B", "Player B",
                    "Player C"),
    team_name   = c("Team1", "Team1", "Team2",
                    "Team1", "Team1",
                    "Team2"),
    position    = c("Forward", "Forward", "Forward",
                    "Midfielder", "Midfielder",
                    "Defender"),
    minsPlayed = c(90, 90, 45, 90, 60, 10),
    goals = c(1, 2, 0, 0, 1, 0),
    goalAssist = c(0, 1, 1, 2, 0, 0),
    totalScoringAtt = c(3, 4, 1, 1, 2, 0),
    ontargetScoringAtt = c(2, 3, 0, 0, 1, 0),
    bigChanceCreated = c(0, 1, 0, 1, 0, 0),
    bigChanceScored = c(1, 1, 0, 0, 0, 0),
    bigChanceMissed = c(0, 0, 0, 0, 1, 0),
    accuratePass = c(40, 50, 30, 55, 45, 10),
    totalPass = c(50, 60, 40, 65, 55, 15),
    accurateLongBalls = c(2, 3, 1, 4, 3, 1),
    totalLongBalls = c(3, 5, 2, 6, 4, 2),
    accurateCross = c(1, 2, 0, 0, 1, 0),
    totalCross = c(2, 3, 1, 1, 2, 0),
    accurateThroughBall = c(0, 1, 0, 1, 0, 0),
    totalThroughBall = c(1, 2, 0, 2, 1, 0),
    totalAttAssist = c(1, 2, 0, 3, 1, 0),
    successfulFinalThirdPasses = c(8, 10, 5, 12, 9, 2),
    totalTackle = c(1, 2, 1, 3, 2, 2),
    wonTackle = c(1, 1, 0, 2, 1, 1),
    interception = c(0, 1, 0, 1, 2, 1),
    outfielderBlock = c(0, 0, 1, 1, 0, 1),
    totalClearance = c(0, 1, 0, 2, 1, 3),
    ballRecovery = c(2, 3, 1, 4, 3, 2),
    duelWon = c(3, 4, 2, 5, 3, 2),
    duelLost = c(2, 3, 1, 3, 2, 1),
    aerialWon = c(1, 2, 0, 1, 1, 2),
    aerialLost = c(1, 1, 1, 0, 1, 1),
    possWonDef3rd = c(0, 1, 0, 1, 1, 1),
    possWonMid3rd = c(1, 0, 1, 2, 1, 0),
    touches = c(50, 60, 35, 70, 55, 15),
    touchesInFinalThird = c(15, 20, 10, 8, 12, 2),
    touchesInOppBox = c(5, 6, 2, 1, 3, 0),
    carries = c(20, 25, 15, 30, 22, 5),
    progressiveCarries = c(3, 4, 2, 5, 3, 0),
    finalThirdEntries = c(2, 3, 1, 1, 2, 0),
    penAreaEntries = c(1, 2, 0, 0, 1, 0),
    dispossessed = c(1, 2, 0, 0, 1, 0),
    turnover = c(1, 1, 1, 2, 1, 0),
    timesTackled = c(1, 2, 0, 1, 1, 0),
    attemptsIbox = c(2, 3, 1, 0, 1, 0),
    attemptsObox = c(1, 1, 0, 1, 1, 0),
    shotOffTarget = c(1, 1, 0, 0, 1, 0),
    blockedScoringAtt = c(0, 1, 0, 1, 0, 0),
    attIboxGoal = c(1, 2, 0, 0, 1, 0),
    attOboxGoal = c(0, 0, 0, 0, 0, 0),
    attHdGoal = c(0, 1, 0, 0, 0, 0),
    attLfGoal = c(0, 0, 0, 0, 0, 0),
    attRfGoal = c(1, 1, 0, 0, 1, 0),
    attPenGoal = c(0, 0, 0, 0, 0, 0),
    hitWoodwork = c(0, 0, 0, 0, 0, 0),
    cornerTaken = c(0, 1, 0, 3, 2, 0),
    accurateCornersIntobox = c(0, 1, 0, 2, 1, 0),
    wonCorners = c(0, 0, 0, 1, 0, 0),
    attFreekickTotal = c(0, 0, 0, 1, 0, 0),
    attFreekickGoal = c(0, 0, 0, 0, 0, 0),
    attFreekickTarget = c(0, 0, 0, 1, 0, 0),
    freekickCross = c(0, 0, 0, 1, 0, 0),
    accurateFreekickCross = c(0, 0, 0, 1, 0, 0),
    penaltyWon = c(0, 0, 0, 0, 0, 0),
    penaltyConceded = c(0, 0, 0, 0, 0, 0),
    attSetpiece = c(0, 0, 0, 0, 0, 0),
    goalAssistSetplay = c(0, 0, 0, 0, 0, 0),
    goalAssistDeadball = c(0, 0, 0, 0, 0, 0),
    totalThrows = c(0, 0, 0, 1, 0, 0),
    accurateThrows = c(0, 0, 0, 1, 0, 0),
    stringsAsFactors = FALSE
  )
}

# Minimal xmetrics data
create_controlled_xmetrics_data <- function() {
  data.frame(
    player_name = c("Player A", "Player A", "Player B"),
    team_name   = c("Team1", "Team1", "Team2"),
    minutes = c(90, 90, 90),
    shots = c(4, 3, 2),
    shots_on_target = c(3, 2, 1),
    goals = c(2, 1, 0),
    npgoals = c(1, 1, 0),
    xg = c(1.2, 0.8, 0.5),
    npxg = c(0.9, 0.7, 0.5),
    key_passes = c(2, 1, 3),
    assists = c(1, 0, 1),
    xa = c(0.4, 0.2, 0.5),
    passes_attempted = c(50, 55, 60),
    passes_completed = c(42, 47, 52),
    sum_xpass = c(40.5, 45.0, 50.0),
    xpass_overperformance = c(1.5, 2.0, 2.0),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Tests for .aggregate_opta_player_stats (internal helper)
# =============================================================================

test_that(".aggregate_opta_player_stats validates source argument", {
  expect_error(
    panna:::.aggregate_opta_player_stats(
      player = NULL, league = "ENG", season = "2024-2025",
      min_minutes = 0, by_team = FALSE, source = "invalid",
      col_spec = list(minutes = "minsPlayed"),
      derive_fn = identity, col_order = c("player", "team", "minutes"),
      loader = function(...) data.frame()
    ),
    "should be one of"
  )
})


test_that(".aggregate_opta_player_stats validates min_minutes", {
  expect_error(
    panna:::.aggregate_opta_player_stats(
      player = NULL, league = "ENG", season = "2024-2025",
      min_minutes = -10, by_team = FALSE, source = "local",
      col_spec = list(minutes = "minsPlayed"),
      derive_fn = identity, col_order = c("player", "team", "minutes"),
      loader = function(...) data.frame()
    ),
    "min_minutes"
  )
})


test_that(".aggregate_opta_player_stats returns empty df for NULL loader data", {
  result <- suppressWarnings(
    panna:::.aggregate_opta_player_stats(
      player = NULL, league = "ENG", season = "2024-2025",
      min_minutes = 0, by_team = FALSE, source = "local",
      col_spec = list(minutes = "minsPlayed"),
      derive_fn = identity, col_order = c("player", "team", "minutes"),
      loader = function(...) NULL
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that(".aggregate_opta_player_stats returns empty df for empty loader data", {
  result <- suppressWarnings(
    panna:::.aggregate_opta_player_stats(
      player = NULL, league = "ENG", season = "2024-2025",
      min_minutes = 0, by_team = FALSE, source = "local",
      col_spec = list(minutes = "minsPlayed"),
      derive_fn = identity, col_order = c("player", "team", "minutes"),
      loader = function(...) data.frame()
    )
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})


test_that(".aggregate_opta_player_stats aggregates by player (default)", {
  mock <- create_controlled_opta_data()
  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed", goals = "goals"),
    derive_fn = identity,
    col_order = c("player", "team", "matches", "minutes", "goals"),
    loader = function(...) mock
  )

  expect_s3_class(result, "data.frame")
  # Player A appears in 3 rows, Player B in 2, Player C in 1
  expect_equal(nrow(result), 3)

  pa <- result[result$player == "Player A", ]
  expect_equal(pa$matches, 3)
  expect_equal(pa$minutes, 90 + 90 + 45)
  expect_equal(pa$goals, 1 + 2 + 0)
})


test_that(".aggregate_opta_player_stats aggregates by player+team", {
  mock <- create_controlled_opta_data()
  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = TRUE, source = "local",
    col_spec = list(minutes = "minsPlayed", goals = "goals"),
    derive_fn = identity,
    col_order = c("player", "team", "matches", "minutes", "goals"),
    loader = function(...) mock
  )

  # Player A on Team1 (2 rows) + Team2 (1 row) = 2 entries
  # Player B on Team1 (2 rows) = 1 entry
  # Player C on Team2 (1 row) = 1 entry
  # Total = 4
  expect_equal(nrow(result), 4)

  pa_t1 <- result[result$player == "Player A" & result$team == "Team1", ]
  expect_equal(pa_t1$matches, 2)
  expect_equal(pa_t1$minutes, 180)
  expect_equal(pa_t1$goals, 3)

  pa_t2 <- result[result$player == "Player A" & result$team == "Team2", ]
  expect_equal(pa_t2$matches, 1)
  expect_equal(pa_t2$minutes, 45)
  expect_equal(pa_t2$goals, 0)
})


test_that(".aggregate_opta_player_stats assigns modal team when not by_team", {
  mock <- create_controlled_opta_data()
  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed"),
    derive_fn = identity,
    col_order = c("player", "team", "minutes"),
    loader = function(...) mock
  )

  # Player A: 2 rows on Team1, 1 on Team2 -> modal = Team1
  pa <- result[result$player == "Player A", ]
  expect_equal(pa$team, "Team1")
})


test_that(".aggregate_opta_player_stats filters by player name (case-insensitive)", {
  mock <- create_controlled_opta_data()
  result <- panna:::.aggregate_opta_player_stats(
    player = "player a", league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed"),
    derive_fn = identity,
    col_order = c("player", "team", "minutes"),
    loader = function(...) mock
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$player, "Player A")
})


test_that(".aggregate_opta_player_stats warns for unknown player", {
  mock <- create_controlled_opta_data()
  expect_warning(
    result <- panna:::.aggregate_opta_player_stats(
      player = "Nonexistent XYZ", league = "ENG", season = "2024-2025",
      min_minutes = 0, by_team = FALSE, source = "local",
      col_spec = list(minutes = "minsPlayed"),
      derive_fn = identity,
      col_order = c("player", "team", "minutes"),
      loader = function(...) mock
    ),
    "No data found for player"
  )
  expect_equal(nrow(result), 0)
})


test_that(".aggregate_opta_player_stats applies min_minutes filter", {
  mock <- create_controlled_opta_data()
  # Player A: 225 min, Player B: 150 min, Player C: 10 min
  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 100, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed"),
    derive_fn = identity,
    col_order = c("player", "team", "minutes"),
    loader = function(...) mock
  )

  # Player C (10 min) should be excluded
  expect_equal(nrow(result), 2)
  expect_false("Player C" %in% result$player)
  expect_true(all(result$minutes >= 100))
})


test_that(".aggregate_opta_player_stats skips min_minutes filter when player is specified", {
  mock <- create_controlled_opta_data()
  # Player C has only 10 minutes, but when searching for specific player,
  # min_minutes is not applied
  result <- panna:::.aggregate_opta_player_stats(
    player = "Player C", league = "ENG", season = "2024-2025",
    min_minutes = 1000, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed"),
    derive_fn = identity,
    col_order = c("player", "team", "minutes"),
    loader = function(...) mock
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$minutes, 10)
})


test_that(".aggregate_opta_player_stats handles missing columns via .get_col", {
  # Data without some columns referenced in col_spec
  mock <- data.frame(
    player_name = c("Player A", "Player A"),
    team_name = c("Team1", "Team1"),
    minsPlayed = c(90, 90),
    goals = c(1, 2),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(
    panna:::.aggregate_opta_player_stats(
      player = NULL, league = "ENG", season = "2024-2025",
      min_minutes = 0, by_team = FALSE, source = "local",
      col_spec = list(
        minutes = "minsPlayed",
        goals = "goals",
        assists = "nonexistent_column"
      ),
      derive_fn = identity,
      col_order = c("player", "team", "minutes", "goals", "assists"),
      loader = function(...) mock
    )
  )

  expect_equal(result$goals, 3)
  expect_equal(result$assists, 0)  # Missing col defaults to 0
})


test_that(".aggregate_opta_player_stats applies derive_fn", {
  mock <- data.frame(
    player_name = c("Player A", "Player A"),
    team_name = c("Team1", "Team1"),
    minsPlayed = c(90, 90),
    goals = c(1, 2),
    stringsAsFactors = FALSE
  )

  derive <- function(r) {
    r$goals_per90 <- round(panna:::per_90(r$goals, r$minutes), 2)
    r
  }

  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed", goals = "goals"),
    derive_fn = derive,
    col_order = c("player", "team", "minutes", "goals", "goals_per90"),
    loader = function(...) mock
  )

  # 3 goals / 180 min * 90 = 1.5
  expect_equal(result$goals_per90, 1.5)
})


test_that(".aggregate_opta_player_stats orders result by minutes descending", {
  mock <- data.frame(
    player_name = c("Low", "High", "Mid"),
    team_name = c("T1", "T2", "T3"),
    minsPlayed = c(45, 270, 90),
    stringsAsFactors = FALSE
  )

  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed"),
    derive_fn = identity,
    col_order = c("player", "team", "minutes"),
    loader = function(...) mock
  )

  expect_equal(result$player, c("High", "Mid", "Low"))
})


test_that(".aggregate_opta_player_stats only keeps col_order columns", {
  mock <- data.frame(
    player_name = c("Player A"),
    team_name = c("Team1"),
    minsPlayed = c(90),
    goals = c(1),
    stringsAsFactors = FALSE
  )

  result <- panna:::.aggregate_opta_player_stats(
    player = NULL, league = "ENG", season = "2024-2025",
    min_minutes = 0, by_team = FALSE, source = "local",
    col_spec = list(minutes = "minsPlayed", goals = "goals"),
    derive_fn = identity,
    col_order = c("player", "minutes"),  # team and goals excluded
    loader = function(...) mock
  )

  expect_equal(names(result), c("player", "minutes"))
})


# =============================================================================
# Tests for player_opta_summary
# =============================================================================

test_that("player_opta_summary returns correct columns and aggregation", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_summary(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expected_cols <- c("player", "team", "matches", "minutes",
                        "goals", "assists", "shots", "shots_on_target",
                        "big_chances_created", "big_chances_scored",
                        "big_chances_missed",
                        "goals_per90", "assists_per90", "shots_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_summary calculates per-90 rates correctly", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_summary(
        player = "Player A", league = "ENG", min_minutes = 0, source = "local"
      )

      # Player A: 225 min, 3 goals, 2 assists, 8 shots
      expect_equal(result$minutes, 225)
      expect_equal(result$goals, 3)
      expect_equal(result$assists, 2)
      expect_equal(result$shots, 8)
      expect_equal(result$goals_per90, round(3 * 90 / 225, 2))
      expect_equal(result$assists_per90, round(2 * 90 / 225, 2))
      expect_equal(result$shots_per90, round(8 * 90 / 225, 2))
    }
  )
})


test_that("player_opta_summary uses load_opta_big5 when league is NULL", {
  mock <- create_controlled_opta_data()
  big5_called <- FALSE

  with_mocked_bindings(
    load_opta_stats = function(...) stop("should not be called"),
    load_opta_big5 = function(...) { big5_called <<- TRUE; mock },
    .package = "panna",
    {
      result <- player_opta_summary(league = NULL, min_minutes = 0, source = "local")
      expect_true(big5_called)
    }
  )
})


test_that("player_opta_summary handles zero-minutes player", {
  mock <- data.frame(
    player_name = c("Zero Player"),
    team_name = c("Team1"),
    minsPlayed = c(0),
    goals = c(0), goalAssist = c(0),
    totalScoringAtt = c(0), ontargetScoringAtt = c(0),
    bigChanceCreated = c(0), bigChanceScored = c(0), bigChanceMissed = c(0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_summary(
        player = "Zero", league = "ENG", min_minutes = 0, source = "local"
      )

      expect_equal(nrow(result), 1)
      expect_equal(result$goals_per90, 0)
      expect_false(is.nan(result$goals_per90))
      expect_false(is.infinite(result$goals_per90))
    }
  )
})


# =============================================================================
# Tests for player_opta_shots
# =============================================================================

test_that("player_opta_shots returns correct columns and derived stats", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_shots(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expected_cols <- c("player", "team", "matches", "minutes",
                        "shots_inside_box", "shots_outside_box",
                        "total_shots", "conversion_rate", "shot_accuracy",
                        "goals_per90", "shots_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_shots calculates total_shots and conversion correctly", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_shots(
        player = "Player A", league = "ENG", min_minutes = 0, source = "local"
      )

      # Player A: ibox=6, obox=2, total=8, goals=3
      expect_equal(result$shots_inside_box, 6)
      expect_equal(result$shots_outside_box, 2)
      expect_equal(result$total_shots, 8)
      expect_equal(result$goals, 3)
      expect_equal(result$conversion_rate, round(3 / 8 * 100, 1))
    }
  )
})


test_that("player_opta_shots is sorted by goals then total_shots", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_shots(league = "ENG", min_minutes = 0, source = "local")

      if (nrow(result) > 1) {
        # Goals should be non-increasing
        for (i in 2:nrow(result)) {
          expect_true(result$goals[i] <= result$goals[i - 1])
        }
      }
    }
  )
})


# =============================================================================
# Tests for player_opta_xg
# =============================================================================

test_that("player_opta_xg returns correct columns with xmetrics loader", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xg(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expected_cols <- c("player", "team", "minutes",
                        "shots", "shots_on_target", "goals", "npgoals",
                        "xg", "npxg", "goals_minus_xg",
                        "xg_per90", "npxg_per90",
                        "key_passes", "assists", "xa", "xa_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_xg calculates derived stats correctly", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xg(
        player = "Player A", league = "ENG", min_minutes = 0, source = "local"
      )

      # Player A: 180 min, 3 goals, 2.0 xg
      expect_equal(result$minutes, 180)
      expect_equal(result$goals, 3)
      expect_equal(result$xg, 2.0)
      expect_equal(result$goals_minus_xg, 1.0)
      expect_equal(result$xg_per90, round(2.0 * 90 / 180, 2))
    }
  )
})


test_that("player_opta_xg is sorted by xg descending", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xg(league = "ENG", min_minutes = 0, source = "local")

      if (nrow(result) > 1) {
        for (i in 2:nrow(result)) {
          expect_true(result$xg[i] <= result$xg[i - 1])
        }
      }
    }
  )
})


# =============================================================================
# Tests for player_opta_xpass
# =============================================================================

test_that("player_opta_xpass returns correct columns", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xpass(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expected_cols <- c("player", "team", "minutes",
                        "passes_attempted", "passes_completed", "pass_pct",
                        "sum_xpass", "xpass_overperformance",
                        "xpass_overperformance_per90", "xpass_avg")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_xpass calculates derived stats correctly", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xpass(
        player = "Player A", league = "ENG", min_minutes = 0, source = "local"
      )

      # Player A: 105 passes attempted, 89 completed, 85.5 sum_xpass, 3.5 overperf
      expect_equal(result$passes_attempted, 105)
      expect_equal(result$passes_completed, 89)
      expect_equal(result$pass_pct, round(89 / 105 * 100, 1))
      expect_equal(result$sum_xpass, 85.5)
      expect_equal(result$xpass_overperformance, 3.5)
      expect_equal(result$xpass_avg, round(85.5 / 105, 3))
    }
  )
})


test_that("player_opta_xpass is sorted by xpass_overperformance descending", {
  mock_xm <- create_controlled_xmetrics_data()

  with_mocked_bindings(
    load_opta_xmetrics = function(...) mock_xm,
    .package = "panna",
    {
      result <- player_opta_xpass(league = "ENG", min_minutes = 0, source = "local")

      if (nrow(result) > 1) {
        for (i in 2:nrow(result)) {
          expect_true(result$xpass_overperformance[i] <=
                       result$xpass_overperformance[i - 1])
        }
      }
    }
  )
})


# =============================================================================
# Tests for player_opta_defense
# =============================================================================

test_that("player_opta_defense returns correct columns", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_defense(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expected_cols <- c("player", "team", "matches", "minutes",
                        "tackles", "tackles_won", "interceptions", "blocks",
                        "clearances", "ball_recoveries",
                        "duels_won", "duels_lost", "aerials_won", "aerials_lost",
                        "tackles_per90", "interceptions_per90",
                        "tackle_win_pct", "aerial_win_pct")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_defense calculates derived percentages correctly", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_defense(
        player = "Player A", league = "ENG", min_minutes = 0, source = "local"
      )

      # Player A: tackles=4, won=2, aerials_won=3, aerials_lost=3
      expect_equal(result$tackles, 4)
      expect_equal(result$tackles_won, 2)
      expect_equal(result$tackle_win_pct, round(2 / 4 * 100, 1))
      expect_equal(result$aerials_won, 3)
      expect_equal(result$aerials_lost, 3)
      expect_equal(result$aerial_win_pct, round(3 / 6 * 100, 1))
    }
  )
})


# =============================================================================
# Tests for aggregate_opta_stats (the SPM pipeline function)
# =============================================================================

test_that("aggregate_opta_stats returns NULL for empty input", {
  expect_warning(
    result <- aggregate_opta_stats(data.frame()),
    "No Opta stats"
  )
  expect_null(result)
})


test_that("aggregate_opta_stats returns NULL for NULL input", {
  expect_warning(
    result <- aggregate_opta_stats(NULL),
    "No Opta stats"
  )
  expect_null(result)
})


test_that("aggregate_opta_stats returns NULL when no players meet min_minutes", {
  mock <- data.frame(
    match_id = c("m1", "m2"),
    player_name = c("A", "A"),
    player_id = c("p1", "p1"),
    team_name = c("T1", "T1"),
    position = c("Forward", "Forward"),
    minsPlayed = c(10, 10),
    goals = c(1, 0),
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- aggregate_opta_stats(mock, min_minutes = 450),
    "minimum minutes"
  )
  expect_null(result)
})


test_that("aggregate_opta_stats creates player_id from name when missing", {
  mock <- data.frame(
    match_id = c("m1", "m2", "m3", "m4", "m5"),
    player_name = rep("Test Player", 5),
    team_name = rep("Team1", 5),
    position = rep("Forward", 5),
    minsPlayed = rep(90, 5),
    goals = c(1, 0, 2, 0, 1),
    stringsAsFactors = FALSE
  )

  result <- suppressWarnings(aggregate_opta_stats(mock, min_minutes = 0))

  expect_true(!is.null(result))
  expect_true("player_id" %in% names(result))
  expect_equal(nrow(result), 1)
})


# =============================================================================
# Edge Cases
# =============================================================================

test_that("all player_opta_* functions handle single-row data", {
  mock <- data.frame(
    player_name = "Solo Player",
    team_name = "Team1",
    position = "Midfielder",
    minsPlayed = 90,
    goals = 1, goalAssist = 1,
    totalScoringAtt = 3, ontargetScoringAtt = 2,
    bigChanceCreated = 1, bigChanceScored = 1, bigChanceMissed = 0,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result <- player_opta_summary(league = "ENG", min_minutes = 0, source = "local")
      expect_equal(nrow(result), 1)
      expect_equal(result$matches, 1)
      expect_equal(result$minutes, 90)
      expect_equal(result$goals_per90, round(1 * 90 / 90, 2))
    }
  )
})


test_that("all player_opta_* functions handle empty data gracefully", {
  with_mocked_bindings(
    load_opta_stats = function(...) data.frame(),
    load_opta_big5 = function(...) data.frame(),
    .package = "panna",
    {
      expect_equal(nrow(suppressWarnings(player_opta_summary(league = "ENG", source = "local"))), 0)
      expect_equal(nrow(suppressWarnings(player_opta_shots(league = "ENG", source = "local"))), 0)
      expect_equal(nrow(suppressWarnings(player_opta_defense(league = "ENG", source = "local"))), 0)
    }
  )
})


test_that("player_opta_xg and player_opta_xpass handle empty xmetrics data", {
  with_mocked_bindings(
    load_opta_xmetrics = function(...) data.frame(),
    .package = "panna",
    {
      expect_equal(nrow(suppressWarnings(player_opta_xg(league = "ENG", source = "local"))), 0)
      expect_equal(nrow(suppressWarnings(player_opta_xpass(league = "ENG", source = "local"))), 0)
    }
  )
})


test_that("player_opta_summary by_team=TRUE separates multi-team players", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      result_by_team <- player_opta_summary(
        league = "ENG", min_minutes = 0, by_team = TRUE, source = "local"
      )
      result_combined <- player_opta_summary(
        league = "ENG", min_minutes = 0, by_team = FALSE, source = "local"
      )

      # Player A on 2 teams = extra row in by_team
      expect_gt(nrow(result_by_team), nrow(result_combined))
    }
  )
})


test_that("min_minutes validation rejects invalid values", {
  mock <- create_controlled_opta_data()

  with_mocked_bindings(
    load_opta_stats = function(...) mock,
    load_opta_big5 = function(...) mock,
    .package = "panna",
    {
      expect_error(player_opta_summary(league = "ENG", min_minutes = -1, source = "local"))
      expect_error(player_opta_summary(league = "ENG", min_minutes = "abc", source = "local"))
      expect_error(player_opta_summary(league = "ENG", min_minutes = NA, source = "local"))
    }
  )
})
