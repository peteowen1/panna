# Tests for skill_config.R and _rating naming in estimated_skills.R

test_that("soccer_stat_rating_definitions returns complete definitions", {
  defs <- soccer_stat_rating_definitions()

  expect_true(is.data.frame(defs))
  expect_true(all(c("stat_name", "type", "category", "pos_adjusted") %in% names(defs)))

  # Should have both types
  expect_true("rate" %in% defs$type)
  expect_true("efficiency" %in% defs$type)

  # Should have key categories
  expect_true("offensive" %in% defs$category)
  expect_true("defensive" %in% defs$category)
  expect_true("goalkeeper" %in% defs$category)
  expect_true("xmetrics" %in% defs$category)

  # Should have a reasonable number of stats

  expect_gt(nrow(defs), 80)

  # No duplicates
  expect_equal(length(unique(defs$stat_name)), nrow(defs))
})

test_that("stat_rating_names filters correctly", {
  all_names <- stat_rating_names()
  rate_names <- stat_rating_names(type = "rate")
  eff_names <- stat_rating_names(type = "efficiency")
  off_names <- stat_rating_names(category = "offensive")
  def_names <- stat_rating_names(category = "defensive")

  expect_equal(length(all_names), length(rate_names) + length(eff_names))
  expect_true(all(rate_names %in% all_names))
  expect_true(all(eff_names %in% all_names))
  expect_gt(length(off_names), 0)
  expect_gt(length(def_names), 0)
})

test_that("soccer_position_map covers standard positions", {
  pm <- soccer_position_map()
  expect_true(all(c("GK", "DEF", "MID", "FWD") %in% pm))
  expect_equal(pm["GK"], c(GK = "GK"))
  expect_equal(pm["CB"], c(CB = "DEF"))
  expect_equal(pm["ST"], c(ST = "FWD"))
})

test_that("default_stat_rating_params matches get_default_decay_params", {
  params <- default_stat_rating_params()
  dp <- get_default_decay_params()

  expect_equal(params$rate, dp$rate)
  expect_equal(params$efficiency, dp$efficiency)
  expect_equal(params$prior_90s, dp$prior_90s)
})

test_that("estimate_player_skills with rating_names=TRUE adds suffix", {
  match_stats <- data.frame(
    player_id = rep("p1", 3),
    player_name = rep("Alice", 3),
    match_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    total_minutes = c(90, 90, 90),
    position = c("Midfielder", "Midfielder", "Midfielder"),
    goals_p90 = c(0.5, 0.3, 0.7),
    stringsAsFactors = FALSE
  )

  result <- estimate_player_skills(match_stats, stat_cols = "goals_p90",
                                    rating_names = TRUE)
  expect_true("goals_p90_rating" %in% names(result))
  expect_false("goals_p90" %in% names(result))
})

test_that("estimate_player_skills with compute_ci=TRUE adds intervals", {
  match_stats <- data.frame(
    player_id = rep(c("p1", "p2"), each = 5),
    player_name = rep(c("Alice", "Bob"), each = 5),
    match_date = rep(as.Date("2024-01-01") + seq(0, 120, 30), 2),
    total_minutes = rep(90, 10),
    position = rep(c("Midfielder", "Defender"), each = 5),
    goals_p90 = c(0.5, 0.3, 0.7, 0.4, 0.6, 0.1, 0.0, 0.2, 0.0, 0.1),
    stringsAsFactors = FALSE
  )

  result <- estimate_player_skills(match_stats, stat_cols = "goals_p90",
                                    compute_ci = TRUE)

  expect_true("goals_p90_rating" %in% names(result))
  expect_true("goals_p90_rating_lower" %in% names(result))
  expect_true("goals_p90_rating_upper" %in% names(result))

  # Lower <= rating <= upper
  expect_true(all(result$goals_p90_rating_lower <= result$goals_p90_rating))
  expect_true(all(result$goals_p90_rating_upper >= result$goals_p90_rating))
})

test_that("estimate_player_skills default (no flags) preserves old behavior", {
  match_stats <- data.frame(
    player_id = rep("p1", 3),
    player_name = rep("Alice", 3),
    match_date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    total_minutes = c(90, 90, 90),
    position = c("Midfielder", "Midfielder", "Midfielder"),
    goals_p90 = c(0.5, 0.3, 0.7),
    stringsAsFactors = FALSE
  )

  result <- estimate_player_skills(match_stats, stat_cols = "goals_p90")

  # Should use raw name (backward compat)
  expect_true("goals_p90" %in% names(result))
  expect_false("goals_p90_rating" %in% names(result))
})
