# Tests for skill optimization functions

# Helper: create match stats with enough players/matches for optimization
make_optimization_match_stats <- function(n_players = 20, n_matches = 15) {
  players <- seq_len(n_players)
  dates <- seq.Date(as.Date("2023-01-01"), by = "week", length.out = n_matches)
  positions <- c("Defender", "Midfielder", "Striker")

  rows <- expand.grid(player_id = players, match_idx = seq_len(n_matches))
  rows$player_name <- paste0("Player_", rows$player_id)
  rows$match_id <- paste0("match_", rows$match_idx)
  rows$match_date <- dates[rows$match_idx]
  rows$total_minutes <- 90
  rows$position <- positions[(rows$player_id - 1) %% length(positions) + 1]

  set.seed(123)
  # Rate stats: player-specific means with noise
  player_means <- runif(n_players, 0.1, 1.5)
  rows$goals_p90 <- pmax(0, player_means[rows$player_id] + rnorm(nrow(rows), 0, 0.3))

  # Efficiency stat
  rows$pass_accuracy <- runif(nrow(rows), 0.6, 0.95)
  rows$passes <- sample(30:60, nrow(rows), replace = TRUE)

  as.data.frame(rows)
}


# === get_stat_tiers() ===

test_that("get_stat_tiers returns correctly structured list", {
  tiers <- get_stat_tiers()

  expect_true(is.list(tiers))
  expect_named(tiers, c("tier1", "tier2", "efficiency", "xmetrics"))
  expect_true(length(tiers$tier1) > 0)
  expect_true(length(tiers$tier2) > 0)
  expect_true(length(tiers$efficiency) > 0)
  expect_true(length(tiers$xmetrics) > 0)
})

test_that("get_stat_tiers has no duplicate stat names across tiers", {
  tiers <- get_stat_tiers()
  all_stats <- c(tiers$tier1, tiers$tier2, tiers$efficiency, tiers$xmetrics)
  expect_equal(length(all_stats), length(unique(all_stats)))
})

test_that("get_stat_tiers tier1 rate stats end in _p90", {
  tiers <- get_stat_tiers()
  expect_true(all(grepl("_p90$", tiers$tier1)))
})

test_that("get_stat_tiers efficiency stats don't end in _p90", {
  tiers <- get_stat_tiers()
  expect_false(any(grepl("_p90$", tiers$efficiency)))
})


# === optimize_stat_prior() ===

test_that("optimize_stat_prior finds a valid prior for rate stats", {
  ms <- make_optimization_match_stats()
  result <- optimize_stat_prior(
    match_stats = ms, stat_name = "goals_p90",
    lambda = 0.003, min_history = 3, sample_n = 20
  )

  expect_true(!is.null(result))
  expect_equal(result$stat, "goals_p90")
  expect_true(result$optimal_prior > 0)
  expect_true(is.finite(result$loss))
  expect_equal(result$loss_type, "mse")
  expect_false(result$is_efficiency)
})

test_that("optimize_stat_prior handles efficiency stats", {
  ms <- make_optimization_match_stats()
  result <- optimize_stat_prior(
    match_stats = ms, stat_name = "pass_accuracy",
    lambda = 0.002, is_efficiency = TRUE,
    denom_col = "passes", min_history = 3, sample_n = 20
  )

  expect_true(!is.null(result))
  expect_equal(result$loss_type, "logloss")
  expect_true(result$is_efficiency)
  expect_true(result$optimal_prior > 0)
  expect_true(is.finite(result$loss))
})

test_that("optimize_stat_prior returns NULL for missing stat", {
  ms <- make_optimization_match_stats()
  expect_warning(
    result <- optimize_stat_prior(
      match_stats = ms, stat_name = "nonexistent_stat_p90",
      lambda = 0.003, min_history = 3
    ),
    "not found"
  )
  expect_null(result)
})

test_that("optimize_stat_prior errors when no data provided", {
  expect_error(
    optimize_stat_prior(stat_name = "goals_p90"),
    "must be provided"
  )
})

test_that("optimize_stat_prior with joint lambda optimization returns lambda", {
  ms <- make_optimization_match_stats()
  result <- optimize_stat_prior(
    match_stats = ms, stat_name = "goals_p90",
    lambda = 0.003, optimize_lambda = TRUE,
    min_history = 3, sample_n = 20
  )

  expect_true(!is.null(result))
  expect_true(!is.null(result$optimal_lambda))
  expect_true(result$optimal_lambda >= 0)
})

test_that("optimized prior beats extreme prior values", {
  ms <- make_optimization_match_stats()
  optimized <- optimize_stat_prior(
    match_stats = ms, stat_name = "goals_p90",
    lambda = 0.003, min_history = 3, sample_n = 20
  )

  # Very extreme prior (near-zero strength = no shrinkage)
  extreme <- optimize_stat_prior(
    match_stats = ms, stat_name = "goals_p90",
    lambda = 0.003, min_history = 3, sample_n = 20,
    prior_bounds = c(0.1, 0.1)
  )

  # Optimized should be at least as good as extreme
  expect_true(optimized$loss <= extreme$loss + 1e-4)
})
