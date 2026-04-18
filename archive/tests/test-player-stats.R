# Tests for player statistics functions
#
# These tests cover the player aggregation functions in player_stats.R,
# testing aggregation logic, edge cases, and derived statistics.

# =============================================================================
# Test Data Helpers
# =============================================================================

# Helper to create mock FBref summary data
create_mock_fbref_summary <- function(n_rows = 50) {
  set.seed(42)
  players <- c("Mohamed Salah", "Erling Haaland", "Kevin De Bruyne",
               "Bukayo Saka", "Bruno Fernandes")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player = sample(players, n_rows, replace = TRUE),
    team = sample(teams, n_rows, replace = TRUE),
    min = sample(60:90, n_rows, replace = TRUE),
    gls = sample(0:2, n_rows, replace = TRUE),
    ast = sample(0:2, n_rows, replace = TRUE),
    sh = sample(1:5, n_rows, replace = TRUE),
    so_t = sample(0:3, n_rows, replace = TRUE),
    x_g = round(runif(n_rows, 0, 1.5), 2),
    npx_g = round(runif(n_rows, 0, 1.3), 2),
    x_ag = round(runif(n_rows, 0, 0.5), 2),
    sca = sample(2:8, n_rows, replace = TRUE),
    gca = sample(0:2, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock FBref passing data
create_mock_fbref_passing <- function(n_rows = 50) {
  set.seed(42)
  players <- c("Mohamed Salah", "Erling Haaland", "Kevin De Bruyne",
               "Bukayo Saka", "Bruno Fernandes")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player = sample(players, n_rows, replace = TRUE),
    team = sample(teams, n_rows, replace = TRUE),
    cmp = sample(20:60, n_rows, replace = TRUE),
    att = sample(30:80, n_rows, replace = TRUE),
    prg_p = sample(2:10, n_rows, replace = TRUE),
    kp = sample(0:5, n_rows, replace = TRUE),
    x1_3 = sample(2:10, n_rows, replace = TRUE),
    ppa = sample(0:4, n_rows, replace = TRUE),
    crs_pa = sample(0:5, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock FBref defense data
create_mock_fbref_defense <- function(n_rows = 50) {
  set.seed(42)
  players <- c("Virgil van Dijk", "Ruben Dias", "William Saliba",
               "Lisandro Martinez", "Gabriel")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player = sample(players, n_rows, replace = TRUE),
    team = sample(teams, n_rows, replace = TRUE),
    tkl = sample(1:5, n_rows, replace = TRUE),
    tkl_w = sample(0:3, n_rows, replace = TRUE),
    int = sample(0:4, n_rows, replace = TRUE),
    blocks = sample(0:3, n_rows, replace = TRUE),
    clr = sample(1:8, n_rows, replace = TRUE),
    err = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock FBref keeper data
create_mock_fbref_keeper <- function(n_rows = 20) {
  set.seed(42)
  keepers <- c("Alisson", "Ederson", "David Raya", "Andre Onana")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player = sample(keepers, n_rows, replace = TRUE),
    team = sample(teams, n_rows, replace = TRUE),
    so_ta = sample(2:8, n_rows, replace = TRUE),
    saves = sample(1:6, n_rows, replace = TRUE),
    ga = sample(0:3, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock Opta data
create_mock_opta_data <- function(n_rows = 50) {
  set.seed(42)
  players <- c("Mohamed Salah", "Erling Haaland", "Kevin De Bruyne",
               "Bukayo Saka", "Bruno Fernandes")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")
  positions <- c("Forward", "Midfielder", "Defender", "Goalkeeper")

  data.frame(
    player_name = sample(players, n_rows, replace = TRUE),
    team_name = sample(teams, n_rows, replace = TRUE),
    position = sample(positions[1:3], n_rows, replace = TRUE),
    minsPlayed = sample(60:90, n_rows, replace = TRUE),
    goals = sample(0:2, n_rows, replace = TRUE),
    goalAssist = sample(0:2, n_rows, replace = TRUE),
    totalScoringAtt = sample(1:5, n_rows, replace = TRUE),
    ontargetScoringAtt = sample(0:3, n_rows, replace = TRUE),
    bigChanceCreated = sample(0:2, n_rows, replace = TRUE),
    bigChanceScored = sample(0:1, n_rows, replace = TRUE),
    bigChanceMissed = sample(0:1, n_rows, replace = TRUE),
    accuratePass = sample(20:60, n_rows, replace = TRUE),
    totalPass = sample(30:80, n_rows, replace = TRUE),
    accurateLongBalls = sample(1:5, n_rows, replace = TRUE),
    totalLongBalls = sample(2:8, n_rows, replace = TRUE),
    accurateCross = sample(0:3, n_rows, replace = TRUE),
    totalCross = sample(1:6, n_rows, replace = TRUE),
    accurateThroughBall = sample(0:2, n_rows, replace = TRUE),
    totalThroughBall = sample(0:3, n_rows, replace = TRUE),
    totalAttAssist = sample(0:4, n_rows, replace = TRUE),
    successfulFinalThirdPasses = sample(5:15, n_rows, replace = TRUE),
    totalTackle = sample(1:5, n_rows, replace = TRUE),
    wonTackle = sample(0:3, n_rows, replace = TRUE),
    interception = sample(0:3, n_rows, replace = TRUE),
    outfielderBlock = sample(0:2, n_rows, replace = TRUE),
    totalClearance = sample(0:6, n_rows, replace = TRUE),
    ballRecovery = sample(2:8, n_rows, replace = TRUE),
    duelWon = sample(2:10, n_rows, replace = TRUE),
    duelLost = sample(1:8, n_rows, replace = TRUE),
    aerialWon = sample(0:5, n_rows, replace = TRUE),
    aerialLost = sample(0:4, n_rows, replace = TRUE),
    possWonDef3rd = sample(0:3, n_rows, replace = TRUE),
    possWonMid3rd = sample(0:3, n_rows, replace = TRUE),
    touches = sample(30:80, n_rows, replace = TRUE),
    touchesInFinalThird = sample(5:25, n_rows, replace = TRUE),
    touchesInOppBox = sample(0:8, n_rows, replace = TRUE),
    carries = sample(10:40, n_rows, replace = TRUE),
    progressiveCarries = sample(1:8, n_rows, replace = TRUE),
    finalThirdEntries = sample(0:5, n_rows, replace = TRUE),
    penAreaEntries = sample(0:3, n_rows, replace = TRUE),
    dispossessed = sample(0:3, n_rows, replace = TRUE),
    turnover = sample(0:4, n_rows, replace = TRUE),
    timesTackled = sample(0:4, n_rows, replace = TRUE),
    attemptsIbox = sample(0:4, n_rows, replace = TRUE),
    attemptsObox = sample(0:2, n_rows, replace = TRUE),
    shotOffTarget = sample(0:2, n_rows, replace = TRUE),
    blockedScoringAtt = sample(0:1, n_rows, replace = TRUE),
    attIboxGoal = sample(0:1, n_rows, replace = TRUE),
    attOboxGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    attHdGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    attLfGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.85, 0.15)),
    attRfGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.85, 0.15)),
    attPenGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    hitWoodwork = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    cornerTaken = sample(0:5, n_rows, replace = TRUE),
    accurateCornersIntobox = sample(0:3, n_rows, replace = TRUE),
    wonCorners = sample(0:2, n_rows, replace = TRUE),
    attFreekickTotal = sample(0:2, n_rows, replace = TRUE),
    attFreekickGoal = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    attFreekickTarget = sample(0:1, n_rows, replace = TRUE),
    freekickCross = sample(0:2, n_rows, replace = TRUE),
    accurateFreekickCross = sample(0:1, n_rows, replace = TRUE),
    penaltyWon = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    penaltyConceded = sample(0:1, n_rows, replace = TRUE, prob = c(0.98, 0.02)),
    attSetpiece = sample(0:1, n_rows, replace = TRUE),
    goalAssistSetplay = sample(0:1, n_rows, replace = TRUE, prob = c(0.9, 0.1)),
    goalAssistDeadball = sample(0:1, n_rows, replace = TRUE, prob = c(0.95, 0.05)),
    totalThrows = sample(0:5, n_rows, replace = TRUE),
    accurateThrows = sample(0:4, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock Opta keeper data
create_mock_opta_keeper <- function(n_rows = 20) {
  set.seed(42)
  keepers <- c("Alisson", "Ederson", "David Raya", "Andre Onana")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player_name = sample(keepers, n_rows, replace = TRUE),
    team_name = sample(teams, n_rows, replace = TRUE),
    position = rep("Goalkeeper", n_rows),
    minsPlayed = sample(60:90, n_rows, replace = TRUE),
    saves = sample(1:6, n_rows, replace = TRUE),
    savedIbox = sample(1:4, n_rows, replace = TRUE),
    savedObox = sample(0:2, n_rows, replace = TRUE),
    goalsConceded = sample(0:3, n_rows, replace = TRUE),
    goalsConcededIbox = sample(0:2, n_rows, replace = TRUE),
    attemptsConcededIbox = sample(2:6, n_rows, replace = TRUE),
    attemptsConcededObox = sample(1:4, n_rows, replace = TRUE),
    cleanSheet = sample(0:1, n_rows, replace = TRUE),
    divingSave = sample(0:3, n_rows, replace = TRUE),
    goodHighClaim = sample(0:2, n_rows, replace = TRUE),
    punches = sample(0:2, n_rows, replace = TRUE),
    bigChanceSaves = sample(0:1, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

# Helper to create mock Understat roster data
create_mock_understat_roster <- function(n_rows = 50) {
  set.seed(42)
  players <- c("Mohamed Salah", "Erling Haaland", "Kevin De Bruyne",
               "Bukayo Saka", "Bruno Fernandes")
  teams <- c("Liverpool", "Manchester City", "Arsenal", "Manchester United")

  data.frame(
    player = sample(players, n_rows, replace = TRUE),
    team = sample(teams, n_rows, replace = TRUE),
    time = sample(60:90, n_rows, replace = TRUE),
    goals = sample(0:2, n_rows, replace = TRUE),
    assists = sample(0:2, n_rows, replace = TRUE),
    shots = sample(1:5, n_rows, replace = TRUE),
    key_passes = sample(0:4, n_rows, replace = TRUE),
    xG = round(runif(n_rows, 0, 1.5), 2),
    xA = round(runif(n_rows, 0, 0.5), 2),
    xGChain = round(runif(n_rows, 0, 2), 2),
    xGBuildup = round(runif(n_rows, 0, 1), 2),
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Tests for FBref Summary
# =============================================================================

test_that("player_fbref_summary aggregates statistics correctly", {
  mock_data <- create_mock_fbref_summary(50)

  # Mock load_summary to return our test data
  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result <- player_fbref_summary(min_minutes = 0)

      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)

      # Check expected columns
      expected_cols <- c("player", "team", "matches", "minutes",
                        "goals", "assists", "shots", "shots_on_target",
                        "xg", "npxg", "xag", "sca", "gca",
                        "goals_minus_xg", "goals_per90", "xg_per90",
                        "npxg_per90", "xag_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_fbref_summary filters by player name", {
  mock_data <- create_mock_fbref_summary(50)

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      # Search for player with partial match
      result <- player_fbref_summary(player = "Salah", min_minutes = 0)

      expect_s3_class(result, "data.frame")
      if (nrow(result) > 0) {
        expect_true(all(grepl("Salah", result$player, ignore.case = TRUE)))
      }
    }
  )
})


test_that("player_fbref_summary applies min_minutes filter", {
  mock_data <- create_mock_fbref_summary(100)

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result_low <- player_fbref_summary(min_minutes = 100)
      result_high <- player_fbref_summary(min_minutes = 500)

      # Higher threshold should have fewer or equal rows
      expect_lte(nrow(result_high), nrow(result_low))

      # All results should meet the threshold
      if (nrow(result_high) > 0) {
        expect_true(all(result_high$minutes >= 500))
      }
    }
  )
})


test_that("player_fbref_summary by_team groups correctly", {
  # Create data with same player on multiple teams
  mock_data <- data.frame(
    player = rep(c("Player A", "Player A", "Player B"), each = 5),
    team = rep(c("Team1", "Team2", "Team1"), each = 5),
    min = rep(90, 15),
    gls = rep(1, 15),
    ast = rep(0, 15),
    sh = rep(2, 15),
    so_t = rep(1, 15),
    x_g = rep(0.5, 15),
    npx_g = rep(0.4, 15),
    x_ag = rep(0.1, 15),
    sca = rep(3, 15),
    gca = rep(0, 15),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result_by_team <- player_fbref_summary(by_team = TRUE, min_minutes = 0)
      result_combined <- player_fbref_summary(by_team = FALSE, min_minutes = 0)

      # by_team should have more rows (Player A on 2 teams)
      expect_gt(nrow(result_by_team), nrow(result_combined))

      # Player A should appear twice with by_team=TRUE
      player_a_rows <- result_by_team[result_by_team$player == "Player A", ]
      expect_equal(nrow(player_a_rows), 2)

      # Combined should have Player A once with correct total
      player_a_combined <- result_combined[result_combined$player == "Player A", ]
      expect_equal(nrow(player_a_combined), 1)
      expect_equal(player_a_combined$matches, 10)  # 5 + 5 matches
    }
  )
})


test_that("player_fbref_summary calculates derived stats correctly", {
  # Create controlled data where we know the expected results
  mock_data <- data.frame(
    player = rep("Test Player", 3),
    team = rep("Test Team", 3),
    min = c(90, 90, 90),  # 270 minutes total
    gls = c(2, 1, 0),     # 3 goals total
    ast = c(1, 0, 1),     # 2 assists total
    sh = c(4, 2, 3),      # 9 shots total
    so_t = c(3, 1, 1),    # 5 SOT total
    x_g = c(1.0, 0.5, 0.5), # 2.0 xG total
    npx_g = c(0.8, 0.4, 0.4), # 1.6 npxG total
    x_ag = c(0.3, 0.2, 0.1), # 0.6 xAG total
    sca = c(4, 3, 2),     # 9 SCA total
    gca = c(1, 0, 0),     # 1 GCA total
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result <- player_fbref_summary(min_minutes = 0)

      expect_equal(nrow(result), 1)
      expect_equal(result$matches, 3)
      expect_equal(result$minutes, 270)
      expect_equal(result$goals, 3)
      expect_equal(result$xg, 2.0)

      # Check derived stats
      expect_equal(result$goals_minus_xg, 1.0)  # 3 - 2 = 1
      expect_equal(result$goals_per90, 1.0)  # 3 goals / 270 min * 90 = 1.0
      expect_equal(result$xg_per90, round(2.0 * 90 / 270, 2))
    }
  )
})


test_that("player_fbref_summary handles empty data", {
  with_mocked_bindings(
    load_summary = function(...) data.frame(),
    .package = "panna",
    {
      result <- suppressWarnings(player_fbref_summary())
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    }
  )
})


test_that("player_fbref_summary handles NULL data", {
  with_mocked_bindings(
    load_summary = function(...) NULL,
    .package = "panna",
    {
      result <- suppressWarnings(player_fbref_summary())
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    }
  )
})


test_that("player_fbref_summary handles player not found", {
  mock_data <- create_mock_fbref_summary(50)

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result <- suppressWarnings(player_fbref_summary(player = "Nonexistent Player XYZ"))
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
    }
  )
})


# =============================================================================
# Tests for FBref Passing
# =============================================================================

test_that("player_fbref_passing aggregates statistics correctly", {
  mock_summary <- create_mock_fbref_summary(50)
  mock_passing <- create_mock_fbref_passing(50)

  with_mocked_bindings(
    load_passing = function(...) mock_passing,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_passing(min_minutes = 0)

      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)

      # Check expected columns
      expected_cols <- c("player", "team", "matches", "minutes",
                        "passes_completed", "passes_attempted", "pass_pct",
                        "progressive_passes", "key_passes",
                        "passes_per90", "progressive_passes_per90", "key_passes_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_fbref_passing calculates pass percentage correctly", {
  mock_summary <- data.frame(
    player = rep("Test Player", 3),
    team = rep("Test Team", 3),
    min = c(90, 90, 90),
    stringsAsFactors = FALSE
  )

  mock_passing <- data.frame(
    player = rep("Test Player", 3),
    team = rep("Test Team", 3),
    cmp = c(40, 50, 60),  # 150 completed
    att = c(50, 60, 70),  # 180 attempted
    prg_p = c(5, 6, 4),   # 15 progressive
    kp = c(2, 1, 3),      # 6 key passes
    x1_3 = c(4, 5, 3),
    ppa = c(1, 2, 1),
    crs_pa = c(2, 1, 2),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_passing = function(...) mock_passing,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_passing(min_minutes = 0)

      expect_equal(nrow(result), 1)
      expect_equal(result$passes_completed, 150)
      expect_equal(result$passes_attempted, 180)
      expect_equal(result$pass_pct, round(150 / 180 * 100, 1))
    }
  )
})


# =============================================================================
# Tests for FBref Defense
# =============================================================================

test_that("player_fbref_defense aggregates statistics correctly", {
  mock_summary <- create_mock_fbref_summary(50)
  mock_defense <- create_mock_fbref_defense(50)

  with_mocked_bindings(
    load_defense = function(...) mock_defense,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_defense(min_minutes = 0)

      expect_s3_class(result, "data.frame")

      # Check expected columns
      expected_cols <- c("player", "team", "matches", "minutes",
                        "tackles", "tackles_won", "interceptions",
                        "blocks", "clearances", "errors",
                        "tackles_per90", "interceptions_per90",
                        "blocks_per90", "tackle_win_pct")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_fbref_defense calculates tackle win pct correctly", {
  mock_summary <- data.frame(
    player = rep("Test Defender", 3),
    team = rep("Test Team", 3),
    min = c(90, 90, 90),
    stringsAsFactors = FALSE
  )

  mock_defense <- data.frame(
    player = rep("Test Defender", 3),
    team = rep("Test Team", 3),
    tkl = c(4, 5, 6),     # 15 tackles
    tkl_w = c(3, 4, 5),   # 12 won
    int = c(2, 2, 1),
    blocks = c(1, 1, 1),
    clr = c(3, 4, 3),
    err = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_defense = function(...) mock_defense,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_defense(min_minutes = 0)

      expect_equal(result$tackles, 15)
      expect_equal(result$tackles_won, 12)
      expect_equal(result$tackle_win_pct, 80.0)  # 12/15 * 100
    }
  )
})


# =============================================================================
# Tests for FBref Keeper
# =============================================================================

test_that("player_fbref_keeper aggregates statistics correctly", {
  mock_summary <- data.frame(
    player = rep(c("Alisson", "Ederson"), each = 5),
    team = rep(c("Liverpool", "Manchester City"), each = 5),
    min = rep(90, 10),
    pos = rep("GK", 10),
    stringsAsFactors = FALSE
  )

  mock_keeper <- data.frame(
    player = rep(c("Alisson", "Ederson"), each = 5),
    team = rep(c("Liverpool", "Manchester City"), each = 5),
    so_ta = sample(3:6, 10, replace = TRUE),
    saves = sample(2:5, 10, replace = TRUE),
    ga = sample(0:2, 10, replace = TRUE),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_keeper = function(...) mock_keeper,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_keeper(min_minutes = 0)

      expect_s3_class(result, "data.frame")

      # Check expected columns
      expected_cols <- c("player", "team", "matches", "minutes",
                        "shots_on_target_against", "saves",
                        "goals_against", "clean_sheets",
                        "save_pct", "goals_against_per90", "clean_sheet_pct")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_fbref_keeper calculates clean sheets correctly", {
  mock_summary <- data.frame(
    player = rep("Test Keeper", 5),
    team = rep("Test Team", 5),
    min = rep(90, 5),
    pos = rep("GK", 5),
    stringsAsFactors = FALSE
  )

  mock_keeper <- data.frame(
    player = rep("Test Keeper", 5),
    team = rep("Test Team", 5),
    so_ta = c(5, 4, 3, 5, 4),
    saves = c(5, 4, 3, 4, 3),
    ga = c(0, 0, 0, 1, 1),  # 3 clean sheets out of 5
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_keeper = function(...) mock_keeper,
    load_summary = function(...) mock_summary,
    .package = "panna",
    {
      result <- player_fbref_keeper(min_minutes = 0)

      expect_equal(result$clean_sheets, 3)
      expect_equal(result$matches, 5)
      expect_equal(result$clean_sheet_pct, 60.0)  # 3/5 * 100
    }
  )
})


# =============================================================================
# Tests for Opta Summary
# =============================================================================

test_that("player_opta_summary aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_summary(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)

      # Check expected columns
      expected_cols <- c("player", "team", "matches", "minutes",
                        "goals", "assists", "shots", "shots_on_target",
                        "goals_per90", "assists_per90", "shots_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_opta_summary handles missing columns gracefully", {
  # Create minimal data without some optional columns
  mock_data <- data.frame(
    player_name = c("Player A", "Player A", "Player B"),
    team_name = c("Team1", "Team1", "Team2"),
    minsPlayed = c(90, 90, 90),
    goals = c(1, 0, 2),
    goalAssist = c(0, 1, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_summary(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      # Missing columns should be 0
      expect_equal(sum(result$shots), 0)
      expect_equal(sum(result$big_chances_created), 0)
    }
  )
})


# =============================================================================
# Tests for Opta Passing
# =============================================================================

test_that("player_opta_passing aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_passing(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("passes_completed" %in% names(result))
      expect_true("passes_attempted" %in% names(result))
      expect_true("pass_pct" %in% names(result))
      expect_true("passes_per90" %in% names(result))
    }
  )
})


# =============================================================================
# Tests for Opta Defense
# =============================================================================

test_that("player_opta_defense aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_defense(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("tackles" %in% names(result))
      expect_true("interceptions" %in% names(result))
      expect_true("aerial_win_pct" %in% names(result))
    }
  )
})


test_that("player_opta_defense calculates aerial win pct correctly", {
  mock_data <- data.frame(
    player_name = rep("Test Player", 3),
    team_name = rep("Test Team", 3),
    minsPlayed = c(90, 90, 90),
    totalTackle = c(2, 3, 4),
    wonTackle = c(1, 2, 3),
    interception = c(1, 1, 1),
    outfielderBlock = c(1, 0, 1),
    totalClearance = c(2, 3, 2),
    ballRecovery = c(3, 4, 3),
    duelWon = c(5, 6, 4),
    duelLost = c(3, 4, 3),
    aerialWon = c(3, 4, 3),    # 10 won
    aerialLost = c(2, 1, 2),   # 5 lost
    possWonDef3rd = c(1, 1, 1),
    possWonMid3rd = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_defense(league = "ENG", min_minutes = 0, source = "local")

      expect_equal(result$aerials_won, 10)
      expect_equal(result$aerials_lost, 5)
      # 10 / (10 + 5) * 100 = 66.7%
      expect_equal(result$aerial_win_pct, round(10 / 15 * 100, 1))
    }
  )
})


# =============================================================================
# Tests for Opta Possession
# =============================================================================

test_that("player_opta_possession aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_possession(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("touches" %in% names(result))
      expect_true("progressive_carries" %in% names(result))
      expect_true("touches_per90" %in% names(result))
    }
  )
})


# =============================================================================
# Tests for Opta Keeper
# =============================================================================

test_that("player_opta_keeper aggregates statistics correctly", {
  mock_data <- create_mock_opta_keeper(20)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_keeper(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("saves" %in% names(result))
      expect_true("goals_conceded" %in% names(result))
      expect_true("save_pct" %in% names(result))
      expect_true("clean_sheet_pct" %in% names(result))
    }
  )
})


test_that("player_opta_keeper filters for goalkeepers only", {
  # Mix of keepers and outfielders
  mock_data <- data.frame(
    player_name = c("Alisson", "Salah", "Ederson", "Haaland"),
    team_name = c("Liverpool", "Liverpool", "Man City", "Man City"),
    position = c("Goalkeeper", "Forward", "Goalkeeper", "Forward"),
    minsPlayed = c(90, 90, 90, 90),
    saves = c(5, 0, 4, 0),
    savedIbox = c(3, 0, 2, 0),
    savedObox = c(2, 0, 2, 0),
    goalsConceded = c(1, 0, 0, 0),
    goalsConcededIbox = c(1, 0, 0, 0),
    attemptsConcededIbox = c(4, 0, 3, 0),
    attemptsConcededObox = c(2, 0, 1, 0),
    cleanSheet = c(0, 0, 1, 0),
    divingSave = c(2, 0, 1, 0),
    goodHighClaim = c(1, 0, 0, 0),
    punches = c(1, 0, 1, 0),
    bigChanceSaves = c(1, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_keeper(league = "ENG", min_minutes = 0, source = "local")

      # Should only have 2 keepers
      expect_equal(nrow(result), 2)
      expect_true(all(result$player %in% c("Alisson", "Ederson")))
    }
  )
})


# =============================================================================
# Tests for Opta Shots
# =============================================================================

test_that("player_opta_shots aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_shots(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("total_shots" %in% names(result))
      expect_true("conversion_rate" %in% names(result))
      expect_true("shot_accuracy" %in% names(result))
    }
  )
})


test_that("player_opta_shots calculates conversion rate correctly", {
  mock_data <- data.frame(
    player_name = rep("Test Striker", 3),
    team_name = rep("Test Team", 3),
    minsPlayed = c(90, 90, 90),
    attemptsIbox = c(3, 4, 3),    # 10 shots inside box
    attemptsObox = c(1, 1, 0),    # 2 shots outside box = 12 total shots
    ontargetScoringAtt = c(2, 3, 2),
    shotOffTarget = c(1, 1, 0),
    blockedScoringAtt = c(1, 1, 1),
    goals = c(1, 2, 0),          # 3 goals
    attIboxGoal = c(1, 2, 0),
    attOboxGoal = c(0, 0, 0),
    attHdGoal = c(0, 1, 0),
    attLfGoal = c(0, 0, 0),
    attRfGoal = c(1, 1, 0),
    attPenGoal = c(0, 0, 0),
    bigChanceScored = c(1, 1, 0),
    bigChanceMissed = c(0, 1, 0),
    hitWoodwork = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_shots(league = "ENG", min_minutes = 0, source = "local")

      expect_equal(result$total_shots, 12)
      expect_equal(result$goals, 3)
      # Conversion rate: 3/12 * 100 = 25%
      expect_equal(result$conversion_rate, 25.0)
    }
  )
})


# =============================================================================
# Tests for Opta Set Piece
# =============================================================================

test_that("player_opta_setpiece aggregates statistics correctly", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      result <- player_opta_setpiece(league = "ENG", min_minutes = 0, source = "local")

      expect_s3_class(result, "data.frame")
      expect_true("corners_taken" %in% names(result))
      expect_true("corner_accuracy" %in% names(result))
      expect_true("penalties_won" %in% names(result))
    }
  )
})


# =============================================================================
# Tests for Understat Summary
# =============================================================================

test_that("player_understat_summary aggregates statistics correctly", {
  mock_data <- create_mock_understat_roster(50)

  with_mocked_bindings(
    load_understat_roster = function(...) mock_data,
    .package = "panna",
    {
      result <- player_understat_summary(min_minutes = 0)

      expect_s3_class(result, "data.frame")
      expect_true(nrow(result) > 0)

      # Check expected columns including Understat-specific ones
      expected_cols <- c("player", "team", "matches", "minutes",
                        "goals", "assists", "shots", "key_passes",
                        "xg", "xa", "xg_chain", "xg_buildup",
                        "goals_minus_xg", "goals_per90", "xg_per90", "xa_per90")
      expect_true(all(expected_cols %in% names(result)))
    }
  )
})


test_that("player_understat_summary calculates xG metrics correctly", {
  mock_data <- data.frame(
    player = rep("Test Player", 3),
    team = rep("Test Team", 3),
    time = c(90, 90, 90),
    goals = c(2, 1, 1),       # 4 goals
    assists = c(0, 1, 0),
    shots = c(4, 3, 3),
    key_passes = c(1, 2, 1),
    x_g = c(1.0, 0.8, 0.7),    # 2.5 xG
    x_a = c(0.1, 0.2, 0.1),
    x_g_chain = c(1.5, 1.2, 1.0),
    x_g_buildup = c(0.5, 0.4, 0.3),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_understat_roster = function(...) mock_data,
    .package = "panna",
    {
      result <- player_understat_summary(min_minutes = 0)

      expect_equal(result$goals, 4)
      expect_equal(result$xg, 2.5)
      expect_equal(result$goals_minus_xg, 1.5)  # 4 - 2.5
    }
  )
})


# =============================================================================
# Edge Case Tests
# =============================================================================

test_that("aggregation handles NA values correctly", {
  mock_data <- data.frame(
    player = rep("Player A", 5),
    team = rep("Team1", 5),
    min = c(90, 90, NA, 90, 90),
    gls = c(1, NA, 1, 0, 1),
    ast = c(0, 1, 0, NA, 0),
    sh = c(2, 3, 2, 3, NA),
    so_t = c(1, 1, 1, 1, 1),
    x_g = c(0.5, 0.4, NA, 0.3, 0.2),
    npx_g = c(0.4, 0.3, 0.2, NA, 0.1),
    x_ag = c(0.1, 0.1, 0.1, 0.1, NA),
    sca = c(2, 2, 2, 2, 2),
    gca = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      # Should not error with NA values
      result <- player_fbref_summary(min_minutes = 0)
      expect_s3_class(result, "data.frame")
    }
  )
})


test_that("aggregation handles single match correctly", {
  mock_data <- data.frame(
    player = "Solo Player",
    team = "Team1",
    min = 90,
    gls = 1,
    ast = 0,
    sh = 3,
    so_t = 2,
    x_g = 0.5,
    npx_g = 0.4,
    x_ag = 0.1,
    sca = 3,
    gca = 1,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      result <- player_fbref_summary(min_minutes = 0)

      expect_equal(nrow(result), 1)
      expect_equal(result$matches, 1)
      expect_equal(result$minutes, 90)
      expect_equal(result$goals_per90, 1.0)
    }
  )
})


test_that("player filter is case-insensitive", {
  mock_data <- data.frame(
    player = c("MOHAMED SALAH", "mohamed salah", "Mohamed Salah"),
    team = rep("Liverpool", 3),
    min = rep(90, 3),
    gls = c(1, 1, 1),
    ast = c(0, 0, 0),
    sh = c(3, 3, 3),
    so_t = c(2, 2, 2),
    x_g = c(0.5, 0.5, 0.5),
    npx_g = c(0.4, 0.4, 0.4),
    x_ag = c(0.1, 0.1, 0.1),
    sca = c(3, 3, 3),
    gca = c(0, 0, 0),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      # Search with different case should match all
      result_lower <- player_fbref_summary(player = "salah", min_minutes = 0)
      result_upper <- player_fbref_summary(player = "SALAH", min_minutes = 0)
      result_mixed <- player_fbref_summary(player = "Salah", min_minutes = 0)

      expect_equal(nrow(result_lower), nrow(result_upper))
      expect_equal(nrow(result_lower), nrow(result_mixed))
    }
  )
})


test_that("zero division is handled in derived stats", {
  mock_data <- data.frame(
    player = "Zero Minutes Player",
    team = "Team1",
    min = 0,  # Zero minutes!
    gls = 0,
    ast = 0,
    sh = 0,
    so_t = 0,
    x_g = 0,
    npx_g = 0,
    x_ag = 0,
    sca = 0,
    gca = 0,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    load_summary = function(...) mock_data,
    .package = "panna",
    {
      # Should not error with zero minutes (division by zero)
      result <- player_fbref_summary(player = "Zero", min_minutes = 0)

      expect_s3_class(result, "data.frame")
      if (nrow(result) > 0) {
        # per90 stats should be 0, not Inf or NaN
        expect_false(is.infinite(result$goals_per90))
        expect_false(is.nan(result$goals_per90))
      }
    }
  )
})


# =============================================================================
# Internal Helper Tests
# =============================================================================

test_that(".load_opta_data handles NULL league", {
  mock_data <- create_mock_opta_data(50)

  with_mocked_bindings(
    load_opta_stats = function(...) mock_data,
    load_opta_big5 = function(...) mock_data,
    .package = "panna",
    {
      # With league = NULL, should call load_opta_big5
      result <- .load_opta_data(league = NULL, season = "2024-2025", source = "local")
      expect_s3_class(result, "data.frame")
    }
  )
})


test_that(".load_opta_data returns data.frame", {
  mock_dt <- data.table::data.table(
    player_name = c("A", "B"),
    team_name = c("T1", "T2"),
    minsPlayed = c(90, 90)
  )

  with_mocked_bindings(
    load_opta_stats = function(...) mock_dt,
    load_opta_big5 = function(...) mock_dt,
    .package = "panna",
    {
      result <- .load_opta_data(league = "ENG", season = "2024-2025", source = "local")

      # Should convert to data.frame for stats::aggregate compatibility
      expect_s3_class(result, "data.frame")
    }
  )
})
