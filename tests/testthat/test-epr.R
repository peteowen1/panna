# Tests for calculate_epr() and calculate_epr_batch()

test_that("calculate_epr produces one row per player", {
  pgd <- data.frame(
    player_id = rep(c("p1", "p2"), each = 3),
    player_name = rep(c("Alice", "Bob"), each = 3),
    match_id = paste0("m", 1:6),
    match_date = as.Date("2024-01-01") + c(0, 30, 60, 0, 30, 60),
    minutes_played = rep(90, 6),
    epv_offensive = c(0.5, 0.3, 0.7, 0.1, 0.2, 0.15),
    epv_defensive = c(-0.1, 0.0, -0.05, 0.2, 0.1, 0.15),
    stringsAsFactors = FALSE
  )

  result <- calculate_epr(pgd, ref_date = "2024-04-01")

  expect_equal(nrow(result), 2)
  expect_true("epr" %in% names(result))
  expect_true("epr_offensive" %in% names(result))
  expect_true("epr_defensive" %in% names(result))

  # Alice has higher offensive EPV → higher epr_offensive
  alice <- result[result$player_id == "p1", ]
  bob <- result[result$player_id == "p2", ]
  expect_gt(alice$epr_offensive, bob$epr_offensive)
})

test_that("calculate_epr applies decay weighting", {
  # Two identical EPV values but at different times
  pgd <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m2"),
    match_date = as.Date(c("2023-01-01", "2024-01-01")),
    minutes_played = c(90, 90),
    epv_offensive = c(1.0, 1.0),
    epv_defensive = c(0, 0),
    stringsAsFactors = FALSE
  )

  result <- calculate_epr(pgd, ref_date = "2024-03-01")

  alice <- result[result$player_id == "p1", ]
  bob <- result[result$player_id == "p2", ]

  # Bob's match is more recent → higher EPR (less decay)
  expect_gt(bob$epr_offensive, alice$epr_offensive)
})

test_that("calculate_epr shrinks toward prior for low-sample players", {
  # Player with 1 game vs player with 20 games, same per-90 EPV
  pgd <- data.frame(
    player_id = c("p1", rep("p2", 20)),
    player_name = c("Alice", rep("Bob", 20)),
    match_id = paste0("m", 1:21),
    match_date = as.Date("2024-01-01") + c(0, seq(0, 190, 10)),
    minutes_played = rep(90, 21),
    epv_offensive = rep(0.5, 21),
    epv_defensive = rep(0, 21),
    stringsAsFactors = FALSE
  )

  result <- calculate_epr(pgd, ref_date = "2024-12-01")

  alice <- result[result$player_id == "p1", ]
  bob <- result[result$player_id == "p2", ]

  # Alice (1 game) should be shrunk more toward prior (negative)
  # Bob (20 games) should be closer to his true 0.5
  expect_lt(alice$epr_offensive, bob$epr_offensive)
})

test_that("calculate_epr epr = epr_offensive + epr_defensive", {
  pgd <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    match_date = as.Date("2024-01-01"),
    minutes_played = 90,
    epv_offensive = 0.5,
    epv_defensive = -0.1,
    stringsAsFactors = FALSE
  )

  result <- calculate_epr(pgd, ref_date = "2024-06-01")
  expect_equal(result$epr, result$epr_offensive + result$epr_defensive)
})

test_that("calculate_epr adjusts for minutes played", {
  # Same total EPV but different minutes → different per-90 rates
  # Use many games to overwhelm the prior
  pgd <- data.frame(
    player_id = c(rep("p1", 20), rep("p2", 20)),
    player_name = c(rep("Alice", 20), rep("Bob", 20)),
    match_id = paste0("m", 1:40),
    match_date = rep(as.Date("2024-01-01") + seq(0, 190, 10), 2),
    minutes_played = c(rep(45, 20), rep(90, 20)),
    epv_offensive = rep(0.5, 40),  # Same total EPV per game
    epv_defensive = rep(0, 40),
    stringsAsFactors = FALSE
  )

  result <- calculate_epr(pgd, ref_date = "2024-12-01")

  alice <- result[result$player_id == "p1", ]
  bob <- result[result$player_id == "p2", ]

  # Alice played 45 min with same total EPV → higher per-90 rate → higher EPR
  expect_gt(alice$epr_offensive, bob$epr_offensive)
})

test_that("calculate_epr returns empty for no data before ref_date", {
  pgd <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    match_date = as.Date("2024-06-01"),
    minutes_played = 90,
    epv_offensive = 0.5,
    epv_defensive = 0,
    stringsAsFactors = FALSE
  )

  expect_warning(
    result <- calculate_epr(pgd, ref_date = "2024-01-01"),
    "No matches before"
  )
  expect_equal(nrow(result), 0)
})

test_that("calculate_epr_batch produces multiple dates", {
  pgd <- data.frame(
    player_id = rep("p1", 5),
    player_name = rep("Alice", 5),
    match_id = paste0("m", 1:5),
    match_date = as.Date("2024-01-01") + seq(0, 120, 30),
    minutes_played = rep(90, 5),
    epv_offensive = c(0.3, 0.5, 0.4, 0.6, 0.2),
    epv_defensive = rep(0, 5),
    stringsAsFactors = FALSE
  )

  ref_dates <- as.Date(c("2024-03-01", "2024-05-01", "2024-07-01"))
  result <- calculate_epr_batch(pgd, ref_dates)

  expect_true("ref_date" %in% names(result))
  expect_equal(length(unique(result$ref_date)), 3)

  # Later dates should use more data → different EPR
  epr_early <- result$epr[result$ref_date == "2024-03-01"]
  epr_late <- result$epr[result$ref_date == "2024-07-01"]
  expect_false(identical(epr_early, epr_late))
})
