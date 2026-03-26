# Tests for rapm_matrix.R
# Covers: create_rapm_design_matrix, prepare_rapm_data

# Helper: create minimal splint_data for RAPM matrix tests
create_test_splint_data <- function(n_splints = 20, n_players = 10) {
  set.seed(42)
  splint_ids <- paste0("s", seq_len(n_splints))
  player_ids <- paste0("player_", seq_len(n_players))
  player_names <- paste("Player", seq_len(n_players))

  splints <- data.frame(
    splint_id = splint_ids,
    match_id = rep(paste0("m", 1:5), each = n_splints / 5),
    duration = runif(n_splints, 5, 30),
    npxg_home = runif(n_splints, 0, 0.5),
    npxg_away = runif(n_splints, 0, 0.5),
    goals_home = sample(0:1, n_splints, replace = TRUE, prob = c(0.8, 0.2)),
    goals_away = sample(0:1, n_splints, replace = TRUE, prob = c(0.8, 0.2)),
    gf_home = sample(0:2, n_splints, replace = TRUE),
    ga_home = sample(0:2, n_splints, replace = TRUE),
    avg_min = runif(n_splints, 10, 80),
    n_players_home = rep(11L, n_splints),
    n_players_away = rep(11L, n_splints),
    stringsAsFactors = FALSE
  )

  # Create player entries (4-5 per side per splint)
  player_rows <- list()
  for (i in seq_len(n_splints)) {
    home_p <- sample(player_ids[1:5], 4)
    away_p <- sample(player_ids[6:10], 4)
    for (pid in home_p) {
      idx <- match(pid, player_ids)
      player_rows[[length(player_rows) + 1]] <- data.frame(
        splint_id = splint_ids[i],
        player_id = pid,
        player_name = player_names[idx],
        is_home = TRUE,
        stringsAsFactors = FALSE
      )
    }
    for (pid in away_p) {
      idx <- match(pid, player_ids)
      player_rows[[length(player_rows) + 1]] <- data.frame(
        splint_id = splint_ids[i],
        player_id = pid,
        player_name = player_names[idx],
        is_home = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    splints = splints,
    players = do.call(rbind, player_rows)
  )
}


# ===========================================================================
# create_rapm_design_matrix
# ===========================================================================

test_that("create_rapm_design_matrix returns expected structure", {
  splint_data <- create_test_splint_data()
  result <- create_rapm_design_matrix(splint_data, min_minutes = 10)

  expect_true(is.list(result))
  expect_true("X_players" %in% names(result))
  expect_true("y" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_true("player_mapping" %in% names(result))
  expect_true("row_data" %in% names(result))
  expect_true("player_ids" %in% names(result))
  expect_true("n_players" %in% names(result))

  # Design matrix should have 2 * n_splints rows (home + away)
  n_valid <- sum(splint_data$splints$duration > 0)
  expect_equal(nrow(result$X_players), 2 * n_valid)
})

test_that("create_rapm_design_matrix rejects invalid input", {
  expect_error(create_rapm_design_matrix("not_a_list"), "must be a list")
  # Empty list triggers cli_abort for missing elements
  expect_error(create_rapm_design_matrix(list()))
})

test_that("design matrix has correct column structure", {
  splint_data <- create_test_splint_data(n_players = 10)
  result <- create_rapm_design_matrix(splint_data, min_minutes = 10)

  n_cols <- ncol(result$X_players)
  # Each player (including replacement) gets offense + defense columns
  # n_players = number of player IDs in player_ids (includes replacement)
  n_total_players <- length(result$player_ids)
  expect_equal(n_cols, 2 * n_total_players)
})

test_that("weights are non-negative", {
  splint_data <- create_test_splint_data()
  result <- create_rapm_design_matrix(splint_data, min_minutes = 10)

  expect_true(all(result$weights >= 0))
})

test_that("player_mapping contains expected columns", {
  splint_data <- create_test_splint_data()
  result <- create_rapm_design_matrix(splint_data, min_minutes = 10)

  expect_true("player_id" %in% names(result$player_mapping))
  expect_true("player_name" %in% names(result$player_mapping))
  expect_true("total_minutes" %in% names(result$player_mapping))
})

test_that("replacement player is included", {
  splint_data <- create_test_splint_data()
  result <- create_rapm_design_matrix(splint_data, min_minutes = 10)

  expect_true("replacement" %in% result$player_ids)
})


# ===========================================================================
# prepare_rapm_data
# ===========================================================================

test_that("prepare_rapm_data adds covariates", {
  splint_data <- create_test_splint_data()
  result <- prepare_rapm_data(splint_data, min_minutes = 10,
                               include_covariates = TRUE)

  expect_true("X_full" %in% names(result))
  expect_true("covariate_names" %in% names(result))
  expect_true(length(result$covariate_names) > 0)

  # X_full should have more columns than X_players
  expect_gt(ncol(result$X_full), ncol(result$X_players))
})

test_that("prepare_rapm_data without covariates matches design matrix", {
  splint_data <- create_test_splint_data()
  result <- prepare_rapm_data(splint_data, min_minutes = 10,
                               include_covariates = FALSE)

  expect_equal(ncol(result$X_full), ncol(result$X_players))
  expect_equal(length(result$covariate_names), 0)
})

test_that("prepare_rapm_data supports xg and goals target types", {
  splint_data <- create_test_splint_data()

  xg_result <- prepare_rapm_data(splint_data, min_minutes = 10, target_type = "xg")
  goals_result <- prepare_rapm_data(splint_data, min_minutes = 10, target_type = "goals")

  # Both should produce valid output

  expect_true(!is.null(xg_result$y))
  expect_true(!is.null(goals_result$y))

  # y values may differ between xg and goals targets
  expect_equal(length(xg_result$y), length(goals_result$y))
})

test_that("prepare_rapm_data with league covariates", {
  splint_data <- create_test_splint_data()
  splint_data$splints$league <- rep(c("ENG", "ESP"), length.out = nrow(splint_data$splints))
  splint_data$splints$season_end_year <- rep(2025, nrow(splint_data$splints))

  result <- prepare_rapm_data(splint_data, min_minutes = 10,
                               include_covariates = TRUE)

  # Should have league-season dummies in covariate_names
  ls_covs <- grep("^ls_", result$covariate_names, value = TRUE)
  expect_true(length(ls_covs) > 0)
})
