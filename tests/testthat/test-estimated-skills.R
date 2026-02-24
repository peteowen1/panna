# Tests for estimated skills functions

# Helper: create minimal match_stats data for testing
# Uses Opta-style position names: Goalkeeper, Defender, Midfielder, Striker
make_test_match_stats <- function(n_players = 3, n_matches = 5) {
  players <- paste0("Player_", seq_len(n_players))
  dates <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = n_matches)
  positions <- c("Defender", "Midfielder", "Striker")

  rows <- expand.grid(player_id = seq_len(n_players), match_idx = seq_len(n_matches))
  rows$player_name <- players[rows$player_id]
  rows$match_id <- paste0("match_", rows$match_idx)
  rows$match_date <- dates[rows$match_idx]
  rows$total_minutes <- 90
  rows$position <- positions[(rows$player_id - 1) %% length(positions) + 1]

  # Rate stats: vary by position (using player_id as proxy)
  set.seed(42)
  base_goals <- c(0.1, 0.3, 0.8)
  base_tackles <- c(2.5, 1.5, 0.5)
  rows$goals_p90 <- base_goals[(rows$player_id - 1) %% 3 + 1]
  rows$tackles_won_p90 <- base_tackles[(rows$player_id - 1) %% 3 + 1]

  # Efficiency stats (requires denominator columns)
  rows$pass_accuracy <- 0.8
  rows$passes <- 50

  as.data.frame(rows)
}


test_that("estimate_player_skills returns correct structure", {
  ms <- make_test_match_stats()
  params <- get_default_decay_params()

  result <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = c("goals_p90", "tackles_won_p90"))

  expect_s3_class(result, "data.table")
  expect_true("player_id" %in% names(result))
  expect_true("player_name" %in% names(result))
  expect_true("primary_position" %in% names(result))
  expect_true("weighted_90s" %in% names(result))
  expect_true("goals_p90" %in% names(result))
  expect_true("tackles_won_p90" %in% names(result))
  expect_equal(nrow(result), 3)
  expect_false(any(is.na(result$goals_p90)))
})


test_that("Gamma-Poisson posterior converges to observed rate with large sample", {
  # With weak prior and many matches: posterior ≈ observed rate
  n <- 50
  ms <- data.frame(
    player_id = rep(1, n),
    player_name = rep("TestPlayer", n),
    match_id = paste0("m_", seq_len(n)),
    match_date = as.Date("2024-01-01") + (seq_len(n) - 1) * 7,
    total_minutes = 90,
    position = "Striker",
    goals_p90 = 0.6
  )

  params <- get_default_decay_params()
  params$prior_90s <- 0.01  # very weak prior
  params$rate <- 0           # no decay

  result <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = "goals_p90",
                                    min_weighted_90s = 0)

  # Posterior should be very close to 0.6 with weak prior and lots of data
  expect_equal(result$goals_p90, 0.6, tolerance = 0.05)
})


test_that("prior dominates with very small sample", {
  # 1 match with 45 minutes → posterior should be prior-dominated
  ms <- data.frame(
    player_id = 1, player_name = "Solo Player", match_id = "m1",
    match_date = as.Date("2024-06-01"), total_minutes = 45,
    position = "Striker", goals_p90 = 5.0  # extreme observed rate
  )

  params <- get_default_decay_params()
  params$prior_90s <- 10  # strong prior
  # Set a realistic prior center (grand_mean) so it actually shrinks
  params$prior_centers <- c(goals_p90 = 0.3)

  result <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = "goals_p90",
                                    min_weighted_90s = 0)

  # With strong prior centered at 0.3, posterior should be shrunk well below 5.0
  # Gamma-Poisson: (0.3*10 + 5.0*0.5) / (10 + 0.5) = 5.5 / 10.5 ≈ 0.52
  expect_true(result$goals_p90 < 2.0)
})


test_that("time decay makes recent data weigh more", {
  # Player with old high rate, recent low rate
  ms_old <- data.frame(
    player_id = 1, player_name = "Decayer", match_id = paste0("old_", 1:10),
    match_date = as.Date("2022-01-01") + (0:9) * 7,
    total_minutes = 90, position = "Striker",
    goals_p90 = 1.0
  )
  ms_new <- data.frame(
    player_id = 1, player_name = "Decayer", match_id = paste0("new_", 1:10),
    match_date = as.Date("2024-06-01") + (0:9) * 7,
    total_minutes = 90, position = "Striker",
    goals_p90 = 0.1
  )
  ms <- rbind(ms_old, ms_new)

  # With decay: result should lean toward 0.1 (recent)
  params_decay <- get_default_decay_params()
  params_decay$rate <- 0.005  # moderate decay
  params_decay$prior_90s <- 0.01

  result_decay <- estimate_player_skills(ms, decay_params = params_decay,
                                          stat_cols = "goals_p90",
                                          min_weighted_90s = 0)

  # Without decay: result should be closer to midpoint (0.55)
  params_nodecay <- get_default_decay_params()
  params_nodecay$rate <- 0
  params_nodecay$prior_90s <- 0.01

  result_nodecay <- estimate_player_skills(ms, decay_params = params_nodecay,
                                            stat_cols = "goals_p90",
                                            min_weighted_90s = 0)

  # Decayed result should be lower (closer to recent 0.1) than no-decay result
  expect_true(result_decay$goals_p90 < result_nodecay$goals_p90)
})


test_that("position affects prior center via multipliers", {
  # GK vs Striker: create enough data for position_multipliers to differentiate
  # Need multiple players per position so compute_position_multipliers() works
  n <- 10
  ms_gk <- data.frame(
    player_id = rep(1:2, each = n),
    player_name = rep(c("GK1", "GK2"), each = n),
    match_id = paste0("m_gk_", rep(1:n, 2)),
    match_date = as.Date("2024-01-01") + rep((seq_len(n) - 1) * 7, 2),
    total_minutes = 90,
    position = "Goalkeeper",
    goals_p90 = 0.01  # GKs rarely score
  )
  ms_fwd <- data.frame(
    player_id = rep(3:4, each = n),
    player_name = rep(c("FWD1", "FWD2"), each = n),
    match_id = paste0("m_fwd_", rep(1:n, 2)),
    match_date = as.Date("2024-01-01") + rep((seq_len(n) - 1) * 7, 2),
    total_minutes = 90,
    position = "Striker",
    goals_p90 = 0.5  # strikers score often
  )
  ms <- rbind(ms_gk, ms_fwd)

  params <- get_default_decay_params()
  params$prior_90s <- 50  # very strong prior → prior-dominated

  result <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = "goals_p90",
                                    min_weighted_90s = 0)

  gk_avg <- mean(result[player_id %in% 1:2]$goals_p90)
  fwd_avg <- mean(result[player_id %in% 3:4]$goals_p90)

  # FWD prior center should be higher than GK
  expect_true(fwd_avg > gk_avg)
})


test_that("estimate_player_skills returns NULL for empty input", {
  ms <- make_test_match_stats()
  # Use target_date before any matches
  result <- estimate_player_skills(ms, target_date = "2020-01-01",
                                    stat_cols = "goals_p90")
  expect_null(result)
})


test_that("efficiency stat uses Beta-Binomial posterior", {
  # pass_accuracy with known denominator (passes)
  ms <- data.frame(
    player_id = rep(1, 20),
    player_name = rep("Passer", 20),
    match_id = paste0("m", 1:20),
    match_date = as.Date("2024-01-01") + (0:19) * 7,
    total_minutes = 90,
    position = "Midfielder",
    pass_accuracy = 0.90,
    passes = 50
  )

  params <- get_default_decay_params()
  params$prior_attempts <- 1  # weak prior
  params$efficiency <- 0       # no decay

  result <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = "pass_accuracy",
                                    min_weighted_90s = 0)

  # With 20 matches x 50 passes x 0.90 accuracy and weak prior,
  # posterior should be close to 0.90
  expect_equal(result$pass_accuracy, 0.90, tolerance = 0.02)
})


test_that(".compute_denominator errors on missing column", {
  dt <- data.frame(goals_p90 = c(0.5, 0.3), tackles = c(10, 8))
  expect_error(
    .compute_denominator(dt, "passes"),
    "not found"
  )
})

test_that(".compute_denominator errors on all missing compound columns", {
  dt <- data.frame(goals_p90 = c(0.5, 0.3))
  expect_error(
    .compute_denominator(dt, "passes+crosses"),
    "No denominator columns found"
  )
})


# -- player_skill_profile --

test_that("player_skill_profile accepts pre-computed skills", {
  ms <- make_test_match_stats(n_players = 5, n_matches = 10)
  params <- get_default_decay_params()
  skills <- estimate_player_skills(ms, decay_params = params,
                                    stat_cols = c("goals_p90", "tackles_won_p90"))

  profile <- player_skill_profile("Player_3", match_stats = ms,
                                   decay_params = params, skills = skills)
  expect_s3_class(profile, "data.table")
  expect_true("stat" %in% names(profile))
  expect_true("skill" %in% names(profile))
  expect_true(nrow(profile) > 0)
})

test_that("player_skill_profile has source parameter", {
  # Verify the function signature includes source parameter
  args <- formals(player_skill_profile)
  expect_true("source" %in% names(args))
})

test_that("player_skill_profile works with pre-computed skills from parquet format", {
  # Simulate the format from load_opta_skills() (aggregate_skills_for_spm output)
  skills <- data.table::data.table(
    player_id = c("p1", "p2", "p3", "p1"),
    player_name = c("H. Kane", "K. Mbappe", "V. van Dijk", "H. Kane"),
    primary_position = c("Striker", "Striker", "Defender", "Striker"),
    season_end_year = c(2025, 2025, 2025, 2024),
    weighted_90s = c(15.0, 12.0, 18.0, 10.0),
    total_minutes = c(1350, 1080, 1620, 900),
    n_matches = c(15L, 12L, 18L, 10L),
    is_gk = c(0L, 0L, 0L, 0L),
    is_df = c(0L, 0L, 1L, 0L),
    is_mf = c(0L, 0L, 0L, 0L),
    is_fw = c(1L, 1L, 0L, 1L),
    goals_p90 = c(0.7, 0.8, 0.05, 0.6),
    tackles_won_p90 = c(0.3, 0.4, 2.5, 0.3)
  )

  profile <- player_skill_profile("Kane", skills = skills)
  expect_s3_class(profile, "data.table")
  expect_true(nrow(profile) > 0)
  # Should pick the row with most weighted_90s (2025 season, 15.0)
  expect_true("stat" %in% names(profile))

  # Metadata columns should NOT appear as stats
  expect_false("season_end_year" %in% profile$stat)
  expect_false("is_gk" %in% profile$stat)
  expect_false("total_minutes" %in% profile$stat)
})
