# Tests for feature_engineering.R
# Covers: calculate_team_sequences, calculate_per_100_sequences,
#         create_player_feature_matrix, apply_bayesian_padding,
#         calculate_finishing_modifier

# Helper: minimal match stats for feature engineering tests
make_match_stats <- function(n = 20) {
  data.frame(
    match_id = rep(paste0("m", 1:2), each = n / 2),
    team = rep(c("TeamA", "TeamB"), each = n / 4, times = 2),
    is_home = rep(c(TRUE, FALSE), each = n / 4, times = 2),
    player_name = paste0("Player_", seq_len(n)),
    touches = sample(30:80, n, replace = TRUE),
    gls = sample(0:2, n, replace = TRUE),
    ast = sample(0:2, n, replace = TRUE),
    sh = sample(0:5, n, replace = TRUE),
    so_t = sample(0:3, n, replace = TRUE),
    x_g = runif(n, 0, 0.5),
    npx_g = runif(n, 0, 0.4),
    x_ag = runif(n, 0, 0.3),
    tkl = sample(0:5, n, replace = TRUE),
    int = sample(0:4, n, replace = TRUE),
    blocks = sample(0:3, n, replace = TRUE),
    sca = sample(0:6, n, replace = TRUE),
    gca = sample(0:2, n, replace = TRUE),
    cmp = sample(10:50, n, replace = TRUE),
    prg_p = sample(0:10, n, replace = TRUE),
    carries = sample(10:40, n, replace = TRUE),
    prg_c = sample(0:8, n, replace = TRUE),
    min = sample(60:90, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}


# ===========================================================================
# calculate_team_sequences
# ===========================================================================

test_that("calculate_team_sequences returns sequences per team-match", {
  stats <- make_match_stats()
  result <- calculate_team_sequences(stats)

  expect_true(is.data.frame(result))
  expect_true("estimated_sequences" %in% names(result))
  expect_true("match_id" %in% names(result))
  expect_true("team" %in% names(result))

  # Sequences should be >= MIN_SEQUENCES_PER_MATCH
  expect_true(all(result$estimated_sequences >= MIN_SEQUENCES_PER_MATCH))

  # One row per team per match
  expect_equal(nrow(result), 4)  # 2 matches x 2 teams
})

test_that("calculate_team_sequences handles zero touches", {
  stats <- make_match_stats()
  stats$touches <- 0
  result <- calculate_team_sequences(stats)

  # Should still return MIN_SEQUENCES_PER_MATCH
  expect_true(all(result$estimated_sequences >= MIN_SEQUENCES_PER_MATCH))
})


# ===========================================================================
# calculate_per_100_sequences
# ===========================================================================

test_that("calculate_per_100_sequences creates rate columns", {
  stats <- make_match_stats()
  team_seq <- calculate_team_sequences(stats)
  result <- calculate_per_100_sequences(stats, team_seq)

  # Should have new _p100 columns
  p100_cols <- grep("_p100$", names(result), value = TRUE)
  expect_true(length(p100_cols) > 0)

  # Rate columns should be non-negative
  for (col in p100_cols) {
    vals <- result[[col]]
    expect_true(all(is.na(vals) | vals >= 0),
                info = paste("Column", col, "has negative values"))
  }
})

test_that("calculate_per_100_sequences accepts custom stat_cols", {
  stats <- make_match_stats()
  team_seq <- calculate_team_sequences(stats)
  result <- calculate_per_100_sequences(stats, team_seq, stat_cols = c("gls", "ast"))

  expect_true("gls_p100" %in% names(result))
  expect_true("ast_p100" %in% names(result))
  # Other _p100 columns should NOT exist
  expect_false("sh_p100" %in% names(result))
})


# ===========================================================================
# apply_bayesian_padding
# ===========================================================================

test_that("apply_bayesian_padding shrinks low-game players toward mean", {
  set.seed(42)
  df <- data.frame(
    player = c("A", "B", "C"),
    n_games = c(1, 5, 20),
    goals_p90 = c(2.0, 0.5, 0.3),
    stringsAsFactors = FALSE
  )
  pop_mean <- mean(df$goals_p90)

  result <- apply_bayesian_padding(df, stat_cols = "goals_p90",
                                    min_games = 10, weight_col = "n_games")

  # Player A (1 game) should be shrunk heavily toward pop mean
  expect_true(abs(result$goals_p90[1] - pop_mean) < abs(2.0 - pop_mean))

  # Player C (20 games >= min) should be unchanged
  expect_equal(result$goals_p90[3], 0.3)
})

test_that("apply_bayesian_padding warns on missing weight_col", {
  df <- data.frame(player = "A", goals_p90 = 0.5)
  expect_warning(
    apply_bayesian_padding(df, stat_cols = "goals_p90", weight_col = "n_games"),
    "not found"
  )
})


# ===========================================================================
# calculate_finishing_modifier
# ===========================================================================

test_that("calculate_finishing_modifier computes finishing ratio", {
  shooting <- data.frame(
    player_name = rep(c("Good", "Avg", "Bad"), each = 25),
    is_goal = c(rep(TRUE, 15), rep(FALSE, 10),   # Good: 15/25
                rep(TRUE, 5), rep(FALSE, 20),     # Avg: 5/25
                rep(TRUE, 2), rep(FALSE, 23)),    # Bad: 2/25
    xg = rep(0.2, 75),
    is_penalty = rep(FALSE, 75),
    stringsAsFactors = FALSE
  )

  result <- calculate_finishing_modifier(shooting, min_shots = 20)

  expect_true(is.data.frame(result))
  expect_true("finishing_modifier" %in% names(result))
  expect_true(all(result$finishing_modifier > 0))

  # Good finisher should have modifier > 1
  good <- result[result$player_name == "Good", ]
  expect_true(good$finishing_modifier > 1)

  # Bad finisher should have modifier < 1
  bad <- result[result$player_name == "Bad", ]
  expect_true(bad$finishing_modifier < 1)
})

test_that("calculate_finishing_modifier filters by min_shots", {
  shooting <- data.frame(
    player_name = rep("Few", 10),
    is_goal = c(rep(TRUE, 3), rep(FALSE, 7)),
    xg = rep(0.2, 10),
    is_penalty = rep(FALSE, 10),
    stringsAsFactors = FALSE
  )

  result <- calculate_finishing_modifier(shooting, min_shots = 20)
  expect_equal(nrow(result), 0)
})

test_that("calculate_finishing_modifier excludes penalties", {
  shooting <- data.frame(
    player_name = rep("PenTaker", 30),
    is_goal = c(rep(TRUE, 10), rep(FALSE, 20)),
    xg = rep(0.2, 30),
    is_penalty = c(rep(TRUE, 5), rep(FALSE, 25)),
    stringsAsFactors = FALSE
  )

  result <- calculate_finishing_modifier(shooting, min_shots = 20)
  # Only 25 non-penalty shots should be counted
  expect_equal(result$total_shots, 25)
})
