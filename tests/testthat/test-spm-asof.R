# Tests for R/spm_asof.R -- expanding-window SPM weights for as-of consumers
# (FABLE-ASOF-EXPERIMENTS.md sec 4).


# ===========================================================================
# .season_end_year_for_date
# ===========================================================================

test_that(".season_end_year_for_date maps mid-season dates to the season they fall in", {
  # Aug-Dec -> season starts THIS year, ends next
  expect_equal(.season_end_year_for_date(as.Date("2025-09-15")), 2026)
  expect_equal(.season_end_year_for_date(as.Date("2025-08-01")), 2026)
  # Jan-Jul -> season started LAST year, ends this year
  expect_equal(.season_end_year_for_date(as.Date("2025-03-01")), 2025)
  expect_equal(.season_end_year_for_date(as.Date("2025-07-01")), 2025)
})


# ===========================================================================
# .subset_rapm_data_expanding
# ===========================================================================

# Deterministic fixture: p1 appears in every splint, p4 ONLY in the newest
# season (2024), so cutoff filtering has an unambiguous, hand-checkable answer.
make_tiny_rapm_fixture <- function() {
  player_ids <- c("p1", "p2", "p3", "p4")
  col_names <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), "is_home")
  X <- matrix(0, nrow = 6, ncol = length(col_names), dimnames = list(NULL, col_names))
  # splint A (season 2022): p1 (home, attacking) vs p2 (away, defending)
  X[1, c("p1_off", "p2_def")] <- 1; X[1, "is_home"] <- 1
  X[2, c("p2_off", "p1_def")] <- 1
  # splint B (season 2023): p1 vs p3
  X[3, c("p1_off", "p3_def")] <- 1; X[3, "is_home"] <- 1
  X[4, c("p3_off", "p1_def")] <- 1
  # splint C (season 2024): p1 vs p4 -- p4 ONLY appears here
  X[5, c("p1_off", "p4_def")] <- 1; X[5, "is_home"] <- 1
  X[6, c("p4_off", "p1_def")] <- 1

  list(
    rapm_data = list(
      X_full = methods::as(X, "CsparseMatrix"),
      y = rnorm(6), weights = runif(6, 5, 30),
      row_data = data.frame(splint_id = c("A", "A", "B", "B", "C", "C")),
      player_ids = player_ids,
      player_mapping = data.frame(
        player_id = player_ids, player_name = player_ids,
        total_minutes = c(1000, 900, 800, 700), stringsAsFactors = FALSE
      ),
      covariate_names = "is_home",
      target_type = "xg"
    ),
    splint_season_map = data.frame(
      splint_id = c("A", "B", "C"), season_end_year = c(2022, 2023, 2024)
    )
  )
}

test_that(".subset_rapm_data_expanding drops future-season rows and season-only players", {
  fx <- make_tiny_rapm_fixture()

  sub <- .subset_rapm_data_expanding(fx$rapm_data, fx$splint_season_map, cutoff_year = 2024)

  # season 2024 (splint C, rows 5-6) dropped; 2022+2023 (rows 1-4) kept
  expect_equal(nrow(sub$X_full), 4)
  expect_false("p4" %in% sub$player_ids)
  expect_true(all(c("p1", "p2", "p3") %in% sub$player_ids))
  expect_equal(sub$n_players, length(sub$player_ids) - 1L)
  expect_equal(sub$n_players_total, length(sub$player_ids))
  expect_true("is_home" %in% sub$covariate_names)
  expect_equal(nrow(sub$player_mapping), length(sub$player_ids))
  expect_setequal(sub$player_mapping$player_id, sub$player_ids)
})

test_that(".subset_rapm_data_expanding with an earlier cutoff drops more players", {
  fx <- make_tiny_rapm_fixture()

  # keep only season 2022 (rows 1-2) -- only p1, p2 survive
  sub <- .subset_rapm_data_expanding(fx$rapm_data, fx$splint_season_map, cutoff_year = 2023)

  expect_equal(nrow(sub$X_full), 2)
  expect_setequal(sub$player_ids, c("p1", "p2"))
})

test_that(".subset_rapm_data_expanding with no seasons before cutoff returns zero rows", {
  fx <- make_tiny_rapm_fixture()

  sub <- .subset_rapm_data_expanding(fx$rapm_data, fx$splint_season_map, cutoff_year = 2022)

  expect_equal(nrow(sub$X_full), 0)
  expect_equal(length(sub$player_ids), 0)
})


# ===========================================================================
# fit_expanding_pooled_rapm
# ===========================================================================

# Larger synthetic fixture (needs >=1000 valid observations to clear
# fit_expanding_pooled_rapm's too-few-observations guard).
make_expanding_pooled_fixture <- function(n_splints = 800, n_players = 20,
                                          seasons = 2015:2020, seed = 99) {
  set.seed(seed)
  player_ids <- paste0("player_", seq_len(n_players))
  n_rows <- n_splints * 2
  splint_ids <- paste0("splint_", seq_len(n_splints))
  splint_seasons <- sample(seasons, n_splints, replace = TRUE)

  i_idx <- integer(0); j_idx <- integer(0)
  for (s in seq_len(n_splints)) {
    home <- sample.int(n_players, 5)
    away <- sample(setdiff(seq_len(n_players), home), 5)
    row_home <- (s - 1) * 2 + 1
    row_away <- row_home + 1
    i_idx <- c(i_idx, rep(row_home, 10), rep(row_away, 10))
    j_idx <- c(j_idx, home, n_players + away, away, n_players + home)
  }
  X_players <- Matrix::sparseMatrix(i = i_idx, j = j_idx, x = 1,
                                    dims = c(n_rows, n_players * 2))
  col_names <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"))
  colnames(X_players) <- col_names

  is_home <- rep(c(1, 0), n_splints)
  X_full <- cbind(X_players, Matrix::Matrix(is_home, ncol = 1, sparse = TRUE))
  colnames(X_full) <- c(col_names, "is_home")

  list(
    rapm_data = list(
      X_full = X_full,
      y = stats::rnorm(n_rows, 1.5, 0.5),
      weights = stats::runif(n_rows, 5, 30),
      row_data = data.frame(splint_id = rep(splint_ids, each = 2)),
      player_ids = player_ids,
      player_mapping = data.frame(
        player_id = player_ids, player_name = paste("Player", player_ids),
        total_minutes = sample(500:3000, n_players, replace = TRUE),
        stringsAsFactors = FALSE
      ),
      covariate_names = "is_home",
      n_players = n_players,
      target_type = "xg"
    ),
    splint_season_map = data.frame(splint_id = splint_ids, season_end_year = splint_seasons)
  )
}

test_that("fit_expanding_pooled_rapm fits ridge RAPM on the expanding-window subset", {
  skip_if_not_installed("glmnet")

  fx <- make_expanding_pooled_fixture()
  n_2020_rows <- sum(fx$splint_season_map$season_end_year == 2020) * 2

  result <- fit_expanding_pooled_rapm(fx$rapm_data, fx$splint_season_map,
                                      cutoff_year = 2020, nfolds = 3, seed = 1)

  expect_false(is.null(result))
  expect_equal(result$cutoff_year, 2020)
  expect_true(result$n_obs >= 1000)
  # season 2020 rows are fully excluded from the training volume
  expect_equal(result$n_obs, nrow(fx$rapm_data$row_data) - n_2020_rows)
  expect_true(all(c("player_id", "rapm", "offense", "defense") %in% names(result$ratings)))
  expect_true(is.finite(result$lambda_min) && result$lambda_min > 0)
})

test_that("fit_expanding_pooled_rapm returns NULL when no prior seasons exist", {
  fx <- make_expanding_pooled_fixture(n_splints = 40, n_players = 12, seasons = 2019:2020)

  expect_condition(
    result <- fit_expanding_pooled_rapm(fx$rapm_data, fx$splint_season_map,
                                        cutoff_year = 2019, nfolds = 3),
    class = "warning"
  )
  expect_null(result)
})


# ===========================================================================
# .skill_spm_offense_cols / .skill_spm_defense_cols / .skill_spm_defense_constraints
# ===========================================================================

test_that(".skill_spm_offense_cols intersects the canonical set with available data", {
  data <- data.frame(
    goals_p90 = 1, shots_p90 = 1, xg_per90 = 1, npxg_per90 = 1,
    xa_per90_xmetrics = 1, unrelated_col = 1
  )
  off <- .skill_spm_offense_cols(data)

  expect_true("goals_p90" %in% off)
  expect_true("xg_per90" %in% off)          # gated on xg_per90 presence
  expect_true("npxg_per90" %in% off)
  expect_false("unrelated_col" %in% off)
  # box-score-only column (in 05_spm.R's set, NOT the skill-SPM set)
  expect_false("hit_woodwork_p90" %in% off)
})

test_that(".skill_spm_offense_cols omits xmetrics columns when xg_per90 is absent", {
  data <- data.frame(goals_p90 = 1, npxg_per90 = 1)  # no xg_per90
  off <- .skill_spm_offense_cols(data)

  expect_true("goals_p90" %in% off)
  expect_false("npxg_per90" %in% off)
})

test_that(".skill_spm_defense_cols intersects the canonical set with available data", {
  data <- data.frame(tackles_p90 = 1, fouls_p90 = 1, gsaa_per90 = 1, unrelated_col = 1)
  def_cols <- .skill_spm_defense_cols(data)

  expect_true(all(c("tackles_p90", "fouls_p90", "gsaa_per90") %in% def_cols))
  expect_false("unrelated_col" %in% def_cols)
})

test_that(".skill_spm_defense_constraints partitions good/bad features with no overlap", {
  cons <- .skill_spm_defense_constraints()

  expect_true(length(intersect(cons$good, cons$bad)) == 0)
  expect_true("tackles_p90" %in% cons$good)
  expect_true("fouls_p90" %in% cons$bad)
})


# ===========================================================================
# fit_expanding_skill_spm
# ===========================================================================

make_skill_spm_fixture <- function(n = 200, seed = 7) {
  set.seed(seed)
  player_id <- paste0("p", seq_len(n))
  skill_features <- data.frame(
    player_id = player_id,
    player_name = paste("Player", player_id),
    season_end_year = sample(2020:2024, n, replace = TRUE),
    total_minutes = sample(500:3000, n, replace = TRUE),
    goals_p90 = runif(n, 0, 0.8),
    assists_p90 = runif(n, 0, 0.5),
    key_passes_p90 = runif(n, 0.2, 3),
    tackles_p90 = runif(n, 0.5, 4),
    interceptions_p90 = runif(n, 0.3, 2.5),
    fouls_p90 = runif(n, 0, 2)
  )
  pooled_rapm_ratings <- data.frame(player_id = unique(player_id))
  pooled_rapm_ratings$rapm <- rnorm(nrow(pooled_rapm_ratings), 0, 0.1)
  pooled_rapm_ratings$offense <- pooled_rapm_ratings$rapm * 0.6 +
    rnorm(nrow(pooled_rapm_ratings), 0, 0.05)
  pooled_rapm_ratings$defense <- pooled_rapm_ratings$rapm * -0.4 +
    rnorm(nrow(pooled_rapm_ratings), 0, 0.05)

  list(skill_features = skill_features, pooled_rapm_ratings = pooled_rapm_ratings)
}

test_that("fit_expanding_skill_spm fits O/D models using only pre-cutoff seasons", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("xgboost")

  fx <- make_skill_spm_fixture()
  fit <- fit_expanding_skill_spm(fx$skill_features, fx$pooled_rapm_ratings,
                                 cutoff_year = 2024, nfolds = 3)

  expect_false(is.null(fit))
  expect_equal(fit$cutoff_year, 2024)
  expect_true(all(c("offense_spm_glmnet", "offense_spm_xgb",
                    "defense_spm_glmnet", "defense_spm_xgb",
                    "offense_spm_ratings", "defense_spm_ratings") %in% names(fit)))
  expect_true("offense_spm" %in% names(fit$offense_spm_ratings))
  expect_true("defense_spm" %in% names(fit$defense_spm_ratings))
  expect_true(fit$n_train > 0)
  expect_true(fit$n_train <= sum(fx$skill_features$season_end_year < 2024))
})

test_that("fit_expanding_skill_spm returns NULL when too few players precede the cutoff", {
  skill_features <- data.frame(
    player_id = c("p1", "p2"), player_name = c("A", "B"),
    season_end_year = c(2023, 2023), total_minutes = c(1000, 1200),
    goals_p90 = c(0.2, 0.3)
  )
  pooled_rapm_ratings <- data.frame(
    player_id = c("p1", "p2"), rapm = c(0.1, -0.1),
    offense = c(0.1, -0.1), defense = c(0, 0)
  )

  expect_condition(
    result <- fit_expanding_skill_spm(skill_features, pooled_rapm_ratings,
                                      cutoff_year = 2024, nfolds = 3),
    class = "warning"
  )
  expect_null(result)
})

test_that("fit_expanding_skill_spm excludes the cutoff season itself from training", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("xgboost")

  fx <- make_skill_spm_fixture()
  # cutoff_year = 2021 -> only season 2020 is eligible
  fit <- fit_expanding_skill_spm(fx$skill_features, fx$pooled_rapm_ratings,
                                 cutoff_year = 2021, nfolds = 3)

  eligible <- fx$skill_features$player_id[fx$skill_features$season_end_year < 2021]
  if (length(unique(eligible)) < 100) {
    expect_null(fit)  # too few players is the expected (correct) outcome here
  } else {
    expect_false(is.null(fit))
    expect_true(all(fit$offense_spm_ratings$player_id %in% eligible))
  }
})
