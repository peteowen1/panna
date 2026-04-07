# Tests for multi-target RAPM extensions

test_that("add_value_metrics_to_splints adds EPV columns", {
  splints <- data.frame(
    splint_id = c("s1", "s2"),
    match_id = c("m1", "m1"),
    duration = c(45, 45),
    home_team_id = c("t1", "t1"),
    stringsAsFactors = FALSE
  )

  players <- data.frame(
    splint_id = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    match_id = rep("m1", 8),
    player_id = c("p1", "p2", "p3", "p4", "p1", "p2", "p3", "p4"),
    team_id = c("t1", "t1", "t2", "t2", "t1", "t1", "t2", "t2"),
    is_home = c(1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L),
    stringsAsFactors = FALSE
  )

  player_game_epv <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    match_id = rep("m1", 4),
    epv_total = c(0.5, 0.3, -0.2, 0.1),
    stringsAsFactors = FALSE
  )

  splint_data <- list(splints = splints, players = players)
  result <- add_value_metrics_to_splints(splint_data, player_game_epv = player_game_epv)

  expect_true("epv_home" %in% names(result$splints))
  expect_true("epv_away" %in% names(result$splints))

  # Splint s1: duration=45 out of match total 90, prorate=0.5
  # Home EPV = (0.5 + 0.3) * 0.5 = 0.4
  # Away EPV = (-0.2 + 0.1) * 0.5 = -0.05
  expect_equal(result$splints$epv_home[1], 0.4)
  expect_equal(result$splints$epv_away[1], -0.05)
})

test_that("add_value_metrics_to_splints handles NULL inputs", {
  splints <- data.frame(
    splint_id = "s1", match_id = "m1", duration = 90,
    home_team_id = "t1", stringsAsFactors = FALSE
  )
  players <- data.frame(
    splint_id = "s1", match_id = "m1", player_id = "p1",
    team_id = "t1", is_home = 1L, stringsAsFactors = FALSE
  )

  splint_data <- list(splints = splints, players = players)
  result <- add_value_metrics_to_splints(splint_data)

  # No value metric columns should be added
  expect_false("epv_home" %in% names(result$splints))
  expect_false("wpa_home" %in% names(result$splints))
  expect_false("psv_home" %in% names(result$splints))
})

test_that(".build_rapm_row_data handles epv target_type", {
  splints <- data.frame(
    splint_id = c("s1", "s2"),
    match_id = c("m1", "m1"),
    duration = c(45, 45),
    epv_home = c(0.5, 0.3),
    epv_away = c(-0.1, 0.2),
    gf_home = c(0, 0),
    ga_home = c(0, 0),
    avg_min = c(22, 67),
    n_players_home = c(11, 11),
    n_players_away = c(11, 11),
    stringsAsFactors = FALSE
  )

  result <- .build_rapm_row_data(splints, "epv")
  expect_equal(result$target_per90_name, "epvf90")

  # First row = home team in splint s1: epv_home=0.5
  expect_equal(result$row_data$target[1], 0.5)
  # Second row = away team in splint s1: epv_away=-0.1
  expect_equal(result$row_data$target[2], -0.1)
})

test_that(".build_rapm_row_data errors on missing columns", {
  splints <- data.frame(
    splint_id = "s1", match_id = "m1", duration = 90,
    gf_home = 0, ga_home = 0,
    stringsAsFactors = FALSE
  )

  expect_error(.build_rapm_row_data(splints, "epv"), "missing column")
})

test_that("create_rapm_design_matrix rejects invalid target types", {
  expect_error(
    create_rapm_design_matrix(list(), target_type = "invalid_target"),
    "should be one of"
  )
})

test_that("create_rapm_design_matrix formals include new target types", {
  # Verify new types are in the valid set by checking the function
  # accepts them without match.arg error (will fail on structure instead)
  valid_types <- as.character(formals(create_rapm_design_matrix)$target_type)[-1]
  expect_true("epv" %in% valid_types)
  expect_true("wpa" %in% valid_types)
  expect_true("psv" %in% valid_types)
})
