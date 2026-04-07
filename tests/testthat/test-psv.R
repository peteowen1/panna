# Tests for calculate_psv() and calculate_psv_components()

test_that("calculate_psv applies coefficients to raw stats", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(1, 0),
    tackles_p90 = c(2, 5),
    minutes_played = c(90, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.5, 0.1),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)

  expect_true("psv" %in% names(result))
  expect_true("psv_raw" %in% names(result))

  # Alice: 1*0.5 + 2*0.1 = 0.7
  # Bob:   0*0.5 + 5*0.1 = 0.5
  expect_equal(result$psv[result$player_id == "p1"], 0.7)
  expect_equal(result$psv[result$player_id == "p2"], 0.5)
})

test_that("calculate_psv minutes adjustment works", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 1),
    minutes_played = c(45, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = TRUE, center = FALSE)

  # Alice: goals_p90=2, mins=45, adjusted = 2 / (45/90) = 4
  # Bob:   goals_p90=1, mins=90, adjusted = 1 / (90/90) = 1
  expect_equal(result$psv[result$player_id == "p1"], 4.0)
  expect_equal(result$psv[result$player_id == "p2"], 1.0)
})

test_that("calculate_psv centering within round", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("A", "B", "C", "D"),
    match_id = c("m1", "m1", "m2", "m2"),
    season = c("2024", "2024", "2024", "2024"),
    round = c(1, 1, 2, 2),
    goals_p90 = c(2, 4, 10, 20),
    minutes_played = c(90, 90, 90, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE)

  # Round 1: mean=3, A=2-3=-1, B=4-3=1
  # Round 2: mean=15, C=10-15=-5, D=20-15=5
  expect_equal(result$psv[result$player_id == "p1"], -1)
  expect_equal(result$psv[result$player_id == "p2"], 1)
  expect_equal(result$psv[result$player_id == "p3"], -5)
  expect_equal(result$psv[result$player_id == "p4"], 5)
})

test_that("calculate_psv standardizes with sd column", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 2,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    sd = 2.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  # 2 / 2.0 * 1.0 = 1.0
  expect_equal(result$psv, 1.0)
})

test_that("calculate_psv excludes efficiency stats by default", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 1,
    pass_accuracy = 0.85,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = c("goals_p90", "pass_accuracy"),
    beta = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )

  # With efficiency exclusion (default): only goals_p90 used
  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                           exclude_efficiency = TRUE)
  expect_equal(result$psv, 1.0)

  # Without efficiency exclusion: both used
  result2 <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                            exclude_efficiency = FALSE)
  expect_equal(result2$psv, 1.0 + 0.85 * 2.0)
})

test_that("calculate_psv_components ensures osv + dsv = psv", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 1),
    tackles_p90 = c(1, 4),
    minutes_played = c(90, 90),
    stringsAsFactors = FALSE
  )

  margin_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.5, 0.1),
    stringsAsFactors = FALSE
  )
  osr_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.6, 0.0),
    stringsAsFactors = FALSE
  )
  dsr_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.0, 0.15),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv_components(stats, margin_coef, osr_coef, dsr_coef,
                                      min_adjust = FALSE, center = FALSE)

  expect_true("osv" %in% names(result))
  expect_true("dsv" %in% names(result))

  # osv + dsv must equal psv
  expect_equal(result$osv + result$dsv, result$psv)
})

test_that("calculate_psv handles zero coefficients gracefully", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 1,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  expect_equal(result$psv, 0)
})

test_that("calculate_psv errors on no matching columns", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "nonexistent_stat",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  expect_error(calculate_psv(stats, coef_df), "No matching stat columns")
})
