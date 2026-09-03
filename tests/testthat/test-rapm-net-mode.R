# Tests for Step 4 (FABLE-PRIOR-FIX-PLAN.md): mode = "net" support in the
# RAPM matrix builders (.build_rapm_row_data / .build_rapm_sparse_matrix /
# create_rapm_design_matrix / prepare_rapm_data) and fit_rapm_with_prior()'s
# net branch. Deliberately self-contained fixtures (not shared with other
# test-rapm-*.R files) per the convention in test-rapm-tripwires.R.

# ===========================================================================
# Tiny deterministic fixture: 2 splints, 1 home player, 1 away player, no
# replacement pool. Small enough to hand-verify every matrix entry.
# ===========================================================================

.net_mode_splints <- data.frame(
  splint_id = c("s1", "s2"),
  match_id = c("m1", "m1"),
  duration = c(30, 60),
  npxg_home = c(0.3, 0.2),
  npxg_away = c(0.1, 0.4),
  # epv_home/away exist only so the F3 "net + non-wpa target_type" abort
  # test can exercise a second target_type (epv) without first tripping the
  # unrelated "missing column" check -- values are irrelevant since that
  # abort fires before target_home/target_away are ever used.
  epv_home = c(0.1, 0.05),
  epv_away = c(0.05, 0.02),
  # wpa_home/wpa_away are exact negatives (zero-sum), as F3's net-mode
  # tripwire requires -- WPA is the only target net mode may use.
  wpa_home = c(0.04, -0.015),
  wpa_away = c(-0.04, 0.015),
  gf_home = c(1, 1),
  ga_home = c(0, 1),
  avg_min = c(15, 45),
  n_players_home = c(11L, 11L),
  n_players_away = c(11L, 11L),
  stringsAsFactors = FALSE
)

# F3 fixture: wpa_home/wpa_away deliberately do NOT sum to zero (s1: 0.04 +
# -0.03 = 0.01 != 0), for the zero-sum tripwire abort test.
.net_mode_splints_bad_wpa <- .net_mode_splints
.net_mode_splints_bad_wpa$wpa_away <- c(-0.03, 0.015)

.net_mode_players <- data.frame(
  splint_id = c("s1", "s1", "s2", "s2"),
  player_id = c("p1", "p2", "p1", "p2"),
  player_name = c("Player One", "Player Two", "Player One", "Player Two"),
  is_home = c(TRUE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

.net_mode_player_ids <- c("p1", "p2")


# ===========================================================================
# .build_rapm_row_data: net mode structure
# ===========================================================================

test_that(".build_rapm_row_data net mode creates 1 row per splint (home perspective)", {
  # F3: net mode requires target_type = "wpa" (the only zero-sum target) --
  # this fixture's wpa_home/wpa_away are exact negatives.
  result <- .build_rapm_row_data(.net_mode_splints, "wpa", mode = "net")
  rd <- result$row_data

  expect_equal(nrow(rd), nrow(.net_mode_splints))
  expect_equal(rd$splint_id, c("s1", "s2"))
  expect_equal(rd$home_away, c("home", "home"))

  # Target is the home-perspective value only (wpa_home)
  expect_equal(rd$target, c(0.04, -0.015))
  expect_equal(rd$target_per_90, c(0.04 * 90 / 30, -0.015 * 90 / 60))
  expect_equal(result$target_per90_name, "wpaf90")
})


# ===========================================================================
# F3 (FABLE-PRIOR-FIX-PLAN.md review): net mode must reject non-zero-sum
# targets -- both structurally (target_type != "wpa") and via a runtime
# zero-sum tripwire (splint columns that claim to be WPA but aren't
# zero-sum, e.g. before Step 3 rewires them).
# ===========================================================================

test_that(".build_rapm_row_data aborts for mode = net with a non-wpa target_type", {
  expect_error(
    .build_rapm_row_data(.net_mode_splints, "xg", mode = "net"),
    "requires.*wpa"
  )
  expect_error(
    .build_rapm_row_data(.net_mode_splints, "epv", mode = "net"),
    "requires.*wpa"
  )
})

test_that(".build_rapm_row_data net mode with target_type = xg does NOT abort in od mode (regression guard)", {
  expect_no_error(.build_rapm_row_data(.net_mode_splints, "xg", mode = "od"))
})

test_that(".build_rapm_row_data aborts for mode = net when the wpa target is not zero-sum", {
  expect_error(
    .build_rapm_row_data(.net_mode_splints_bad_wpa, "wpa", mode = "net"),
    "zero-sum"
  )
})

test_that(".build_rapm_row_data net mode succeeds when the wpa target IS zero-sum", {
  expect_no_error(.build_rapm_row_data(.net_mode_splints, "wpa", mode = "net"))
})

test_that("create_rapm_design_matrix propagates the F3 net+non-wpa abort", {
  splint_data <- list(splints = .net_mode_splints, players = .net_mode_players)
  expect_error(
    create_rapm_design_matrix(splint_data, min_minutes = 0, target_type = "xg", mode = "net"),
    "requires.*wpa"
  )
})

test_that("create_rapm_design_matrix propagates the F3 zero-sum tripwire", {
  splint_data <- list(splints = .net_mode_splints_bad_wpa, players = .net_mode_players)
  expect_error(
    create_rapm_design_matrix(splint_data, min_minutes = 0, target_type = "wpa", mode = "net"),
    "zero-sum"
  )
})


# ===========================================================================
# .build_rapm_sparse_matrix: net mode -- one column per player, home +1/away -1
# ===========================================================================

test_that("net mode design matrix has exactly one column per player (+1 replacement)", {
  sm <- .build_rapm_sparse_matrix(
    .net_mode_players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = character(0), n_rows = 2, mode = "net"
  )

  expect_equal(ncol(sm$X_players), length(.net_mode_player_ids) + 1)
  expect_equal(sm$col_names, c("p1_net", "p2_net", "replacement_net"))
  expect_equal(nrow(sm$X_players), 2)
})

test_that("net mode design matrix values are +1 for home players, -1 for away", {
  sm <- .build_rapm_sparse_matrix(
    .net_mode_players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = character(0), n_rows = 2, mode = "net"
  )

  dense <- as.matrix(sm$X_players)
  expected <- matrix(
    c(1, -1, 0,
      1, -1, 0),
    nrow = 2, byrow = TRUE,
    dimnames = list(NULL, c("p1_net", "p2_net", "replacement_net"))
  )
  expect_equal(dense, expected)
})

test_that("net mode replacement column nets home minus away replacement contribution", {
  # Add a 3rd, low-minute player who only appears at home in s1.
  players <- rbind(
    .net_mode_players,
    data.frame(splint_id = "s1", player_id = "p3", player_name = "Player Three",
               is_home = TRUE, stringsAsFactors = FALSE)
  )
  sm <- .build_rapm_sparse_matrix(
    players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = "p3", n_rows = 2, mode = "net"
  )

  dense <- as.matrix(sm$X_players)
  # s1: p3 contributes +1 (home) to replacement_net; s2: no replacement present -> 0
  expect_equal(dense[, "replacement_net"], c(1, 0))
  # Regular player columns unaffected by the replacement player
  expect_equal(dense[, "p1_net"], c(1, 1))
  expect_equal(dense[, "p2_net"], c(-1, -1))
})


# ===========================================================================
# O/D mode regression guard: byte-identical to the pre-mode-parameter formula
# ===========================================================================

test_that("od mode row data is unaffected by the mode parameter (default == explicit od)", {
  result_default <- .build_rapm_row_data(.net_mode_splints, "xg")
  result_od <- .build_rapm_row_data(.net_mode_splints, "xg", mode = "od")
  expect_identical(result_default, result_od)
})

test_that("od mode row data matches the documented pre-change xg formula exactly", {
  rd <- .build_rapm_row_data(.net_mode_splints, "xg", mode = "od")$row_data

  # 2 rows per splint: home-attacking then away-attacking
  expect_equal(nrow(rd), 4)
  expect_equal(rd$splint_id, c("s1", "s1", "s2", "s2"))
  expect_equal(rd$home_away, c("home", "away", "home", "away"))

  # target = home/away npxg alternating; target_per_90 = target * 90 / duration
  expect_equal(rd$target, c(0.3, 0.1, 0.2, 0.4))
  expect_equal(rd$target_per_90,
               c(0.3 * 90 / 30, 0.1 * 90 / 30, 0.2 * 90 / 60, 0.4 * 90 / 60))

  # gd = gf_home - ga_home from home row's perspective, flipped for away row
  expect_equal(rd$gd, c(1 - 0, 0 - 1, 1 - 1, 1 - 1))
  expect_equal(rd$gf, c(1, 0, 1, 1))
  expect_equal(rd$ga, c(0, 1, 1, 1))
  expect_equal(rd$avg_min, c(15, 15, 45, 45))
  expect_equal(rd$n_offense, c(11, 11, 11, 11))
  expect_equal(rd$n_defense, c(11, 11, 11, 11))
  expect_equal(rd$net_players, c(0, 0, 0, 0))
})

test_that("od mode sparse matrix is unaffected by the mode parameter (default == explicit od)", {
  sm_default <- .build_rapm_sparse_matrix(
    .net_mode_players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = character(0), n_rows = 4
  )
  sm_od <- .build_rapm_sparse_matrix(
    .net_mode_players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = character(0), n_rows = 4, mode = "od"
  )
  expect_identical(as.matrix(sm_default$X_players), as.matrix(sm_od$X_players))
  expect_identical(sm_default$col_names, sm_od$col_names)
})

test_that("od mode sparse matrix matches the documented pre-change off/def formula exactly", {
  sm <- .build_rapm_sparse_matrix(
    .net_mode_players, .net_mode_splints, .net_mode_player_ids,
    replacement_player_ids = character(0), n_rows = 4, mode = "od"
  )

  expect_equal(sm$col_names,
               c("p1_off", "p2_off", "replacement_off", "p1_def", "p2_def", "replacement_def"))
  expect_equal(dim(sm$X_players), c(4, 6))

  dense <- as.matrix(sm$X_players)
  expected <- matrix(
    c(1, 0, 0, 0, 1, 0,   # row 1: s1 home-attacking -- p1 offense, p2 defense
      0, 1, 0, 1, 0, 0,   # row 2: s1 away-attacking -- p2 offense, p1 defense
      1, 0, 0, 0, 1, 0,   # row 3: s2 home-attacking
      0, 1, 0, 1, 0, 0),  # row 4: s2 away-attacking
    nrow = 4, byrow = TRUE,
    dimnames = list(NULL, sm$col_names)
  )
  expect_equal(dense, expected)
})

test_that("create_rapm_design_matrix wires mode = \"net\" through row data + sparse matrix", {
  splint_data <- list(splints = .net_mode_splints, players = .net_mode_players)
  result <- create_rapm_design_matrix(splint_data, min_minutes = 0,
                                       target_type = "wpa", mode = "net")

  expect_equal(result$mode, "net")
  expect_equal(nrow(result$X_players), 2)  # 1 row per splint
  expect_equal(ncol(result$X_players), length(.net_mode_player_ids) + 1)
  expect_equal(result$n_rows, 2)
  expect_length(result$y, 2)
})

test_that("create_rapm_design_matrix default mode is unchanged (\"od\")", {
  splint_data <- list(splints = .net_mode_splints, players = .net_mode_players)
  result <- create_rapm_design_matrix(splint_data, min_minutes = 0, target_type = "xg")

  expect_equal(result$mode, "od")
  expect_equal(nrow(result$X_players), 4)
})


# ===========================================================================
# F6 (FABLE-PRIOR-FIX-PLAN.md review): prepare_rapm_data() must NOT add an
# is_home covariate in net mode -- net mode's row_data$home_away is constant
# "home" (1 row per splint), so is_home would be an all-ones column:
# unpenalized and collinear with the intercept.
# ===========================================================================

test_that("prepare_rapm_data net mode drops the is_home covariate (F6)", {
  splint_data <- list(splints = .net_mode_splints, players = .net_mode_players)
  result <- prepare_rapm_data(splint_data, min_minutes = 0,
                               target_type = "wpa", mode = "net")

  expect_false("is_home" %in% result$covariate_names)
  expect_false("is_home" %in% colnames(result$X_full))
})

test_that("prepare_rapm_data od mode still includes is_home covariate (regression guard, F6)", {
  splint_data <- list(splints = .net_mode_splints, players = .net_mode_players)
  result <- prepare_rapm_data(splint_data, min_minutes = 0,
                               target_type = "xg", mode = "od")

  expect_true("is_home" %in% result$covariate_names)
  expect_true("is_home" %in% colnames(result$X_full))
})


# ===========================================================================
# fit_rapm_with_prior: net branch (D4) -- offense_prior carries the net
# prior, defense_prior must be absent.
# ===========================================================================

.net_mode_fit_data <- function(n_splints = 30, n_players = 15) {
  set.seed(11)
  player_ids <- paste0("net_player_", seq_len(n_players))
  n_rows <- n_splints

  X_players <- matrix(0, nrow = n_rows, ncol = n_players)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 5)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 5)
    X_players[i, home_players] <- 1
    X_players[i, away_players] <- -1
  }

  covariates <- c("is_home")
  X_covariates <- matrix(rep(1, n_rows), nrow = n_rows)
  X <- cbind(X_players, X_covariates)
  colnames(X) <- c(paste0(player_ids, "_net"), covariates)

  y <- rnorm(n_rows, mean = 0, sd = 0.3)
  weights <- runif(n_rows, 5, 30)
  minutes <- sample(500:3000, n_players, replace = TRUE)

  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("Net Player", seq_len(n_players)),
    total_minutes = minutes,
    stringsAsFactors = FALSE
  )

  list(
    X = X, y = y, weights = weights,
    player_ids = player_ids, player_mapping = player_mapping,
    covariate_cols = covariates, covariate_names = covariates,
    n_players = n_players
  )
}

test_that("fit_rapm_with_prior aborts when defense_prior is supplied in mode = \"net\"", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )
  defense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                        defense_prior = defense_prior, alpha = 0, nfolds = 3,
                        mode = "net"),
    "defense_prior"
  )
})

test_that("fit_rapm_with_prior net mode fits with offense_prior only (defense_prior = NULL)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  model <- fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                               defense_prior = NULL, alpha = 0, nfolds = 3,
                               mode = "net")

  expect_true(inherits(model, "cv.glmnet"))
  expect_equal(model$panna_metadata$type, "xrapm_net")
  expect_equal(model$panna_metadata$mode, "net")
  expect_equal(model$panna_metadata$n_player_cols, rapm_data$n_players)
  expect_true(model$panna_metadata$used_prior)

  # Prior vector is filled on the "_net" columns
  prior_vec <- model$panna_metadata$prior_vec
  net_cols <- paste0(rapm_data$player_ids, "_net")
  expect_true(all(net_cols %in% names(prior_vec)))
  expect_equal(unname(prior_vec[net_cols]), unname(offense_prior[rapm_data$player_ids]))
})

test_that("fit_rapm_with_prior net mode aborts when offense_prior is supplied but unnamed", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()

  bad_offense_prior <- rep(0, rapm_data$n_players)  # unnamed -> 0 matches

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = bad_offense_prior,
                        defense_prior = NULL, alpha = 0, nfolds = 3, mode = "net"),
    "offense_prior.*supplied but matched 0"
  )
})

test_that("fit_rapm_with_prior net mode does NOT abort on explicit NULL offense_prior", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()

  expect_no_error(
    model <- fit_rapm_with_prior(rapm_data, offense_prior = NULL,
                                 defense_prior = NULL, alpha = 0, nfolds = 3,
                                 mode = "net")
  )
  expect_equal(model$panna_metadata$type, "xrapm_net")
})

test_that("fit_rapm_with_prior default mode (\"od\") is unaffected by the mode parameter", {
  skip_if_not_installed("glmnet")
  set.seed(42)
  n_players <- 15
  player_ids <- paste0("player_", seq_len(n_players))
  n_splints <- 30
  n_rows <- n_splints * 2
  n_player_cols <- n_players * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_player_cols)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 5)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 5)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
  }
  covariates <- c("is_home")
  X_covariates <- matrix(rep(c(1, 0), n_splints), nrow = n_rows)
  X <- cbind(X_players, X_covariates)
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), covariates)
  y <- rnorm(n_rows, mean = 1.2, sd = 0.4)
  weights <- runif(n_rows, 5, 30)
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("Player", seq_len(n_players)),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rapm_data <- list(
    X = X, y = y, weights = weights, player_ids = player_ids,
    player_mapping = player_mapping, covariate_cols = covariates,
    covariate_names = covariates, n_players = n_players
  )

  offense_prior <- stats::setNames(rnorm(n_players, 0, 0.3), player_ids)
  defense_prior <- stats::setNames(rnorm(n_players, 0, 0.3), player_ids)

  set.seed(1)
  model_default <- fit_rapm_with_prior(rapm_data, offense_prior, defense_prior,
                                       alpha = 0, nfolds = 3)
  set.seed(1)
  model_od <- fit_rapm_with_prior(rapm_data, offense_prior, defense_prior,
                                  alpha = 0, nfolds = 3, mode = "od")

  expect_equal(model_default$panna_metadata$type, "xrapm")
  expect_equal(model_od$panna_metadata$type, "xrapm")
  expect_equal(as.vector(coef(model_default, s = "lambda.min")),
               as.vector(coef(model_od, s = "lambda.min")))
})


# ===========================================================================
# F5 (FABLE-PRIOR-FIX-PLAN.md review): fit_rapm_with_prior() must abort when
# its `mode` argument disagrees with rapm_data$mode (the mode the design
# matrix was actually built with). Older rapm_data without a $mode element
# (e.g. .net_mode_fit_data()'s bare list, and every od-mode fixture above)
# skip the check unchanged.
# ===========================================================================

test_that("fit_rapm_with_prior aborts when mode disagrees with rapm_data$mode (F5)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "od"  # design matrix claims od; caller asks for net

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                        defense_prior = NULL, alpha = 0, nfolds = 3,
                        mode = "net"),
    "does not match"
  )
})

test_that("fit_rapm_with_prior aborts the other direction: net rapm_data, od call (F5)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "net"

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )
  defense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  expect_error(
    fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                        defense_prior = defense_prior, alpha = 0, nfolds = 3,
                        mode = "od"),
    "does not match"
  )
})

test_that("fit_rapm_with_prior does not abort when rapm_data$mode matches mode (F5)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "net"

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  expect_no_error(
    fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                        defense_prior = NULL, alpha = 0, nfolds = 3,
                        mode = "net")
  )
})

test_that("fit_rapm_with_prior does not abort when rapm_data has no $mode element (F5)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()  # no $mode field -- older fixture shape
  expect_null(rapm_data$mode)

  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  expect_no_error(
    fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                        defense_prior = NULL, alpha = 0, nfolds = 3,
                        mode = "net")
  )
})


# ===========================================================================
# F4 (FABLE-PRIOR-FIX-PLAN.md review): extract_xrapm_ratings() must be
# mode-aware. A net-mode fit only has "_net" coefficients; indexing "_off"/
# "_def" names against it previously returned silently all-NA ratings.
# ===========================================================================

test_that("extract_xrapm_ratings net mode extracts _net coefficients, offense/defense NA (F4)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )

  model <- fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                               defense_prior = NULL, alpha = 0, nfolds = 3,
                               mode = "net")
  ratings <- extract_xrapm_ratings(model)

  expect_equal(nrow(ratings), rapm_data$n_players)
  expect_setequal(ratings$player_id, rapm_data$player_ids)
  expect_true(all(is.finite(ratings$xrapm)))
  expect_true(all(is.na(ratings$offense)))
  expect_true(all(is.na(ratings$defense)))
  expect_true(all(is.na(ratings$off_deviation)))
  expect_true(all(is.na(ratings$def_deviation)))
  expect_true(all(is.na(ratings$off_prior)))
  expect_true(all(is.na(ratings$def_prior)))
  # Sorted descending by xrapm, same as od mode
  expect_equal(ratings$xrapm, sort(ratings$xrapm, decreasing = TRUE))
})

test_that("extract_xrapm_ratings od mode is unaffected by mode-awareness (regression guard, F4)", {
  skip_if_not_installed("glmnet")
  set.seed(7)
  n_players <- 10
  player_ids <- paste0("odp_", seq_len(n_players))
  n_splints <- 20
  n_rows <- n_splints * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_players * 2)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 4)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 4)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
  }
  covariates <- c("is_home")
  X <- cbind(X_players, matrix(rep(c(1, 0), n_splints), nrow = n_rows))
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), covariates)
  y <- rnorm(n_rows, mean = 1, sd = 0.4)
  weights <- runif(n_rows, 5, 30)
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("OD Player", seq_len(n_players)),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rapm_data <- list(
    X = X, y = y, weights = weights, player_ids = player_ids,
    player_mapping = player_mapping, covariate_cols = covariates,
    covariate_names = covariates, n_players = n_players, mode = "od"
  )
  offense_prior <- stats::setNames(rnorm(n_players, 0, 0.3), player_ids)
  defense_prior <- stats::setNames(rnorm(n_players, 0, 0.3), player_ids)

  model <- fit_rapm_with_prior(rapm_data, offense_prior, defense_prior,
                               alpha = 0, nfolds = 3, mode = "od")
  ratings <- extract_xrapm_ratings(model)

  expect_false(any(is.na(ratings$offense)))
  expect_false(any(is.na(ratings$defense)))
  # defense positive=good (2026-09-03), so xrapm is additive now.
  expect_equal(ratings$xrapm, ratings$offense + ratings$defense)
})

test_that("extract_xrapm_ratings aborts if coefficients don't match the declared mode (F4)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  offense_prior <- stats::setNames(
    rnorm(rapm_data$n_players, 0, 0.3), rapm_data$player_ids
  )
  model <- fit_rapm_with_prior(rapm_data, offense_prior = offense_prior,
                               defense_prior = NULL, alpha = 0, nfolds = 3,
                               mode = "net")

  # Corrupt the declared mode to "od" while the coefficients are still
  # net-only ("_net" columns) -- simulates a metadata/design mismatch that
  # should abort loudly, not silently return all-NA ratings.
  model$panna_metadata$mode <- "od"
  model$panna_metadata$type <- "xrapm"

  expect_error(extract_xrapm_ratings(model), "don't match")
})


# ===========================================================================
# Step 5 (FABLE-PRIOR-FIX-PLAN.md): fit_rapm()/extract_rapm_ratings() (the
# BASE, no-prior path) must also be mode-aware. 04_rapm.R's multi-target
# section fits WPA's base RAPM (the SPM training target) with mode = "net" --
# empirically confirmed necessary: fitting a true zero-sum target in mode =
# "od" drives cor(offense, defense) to EXACTLY -1 (a ridge fit on a zero-sum
# target is symmetric under the row/off-def/sign swap, so the unique
# ridge-regularized solution is a fixed point of that symmetry), which would
# trip the Step-1 D5 tripwire unconditionally and abort the whole script.
# ===========================================================================

test_that("fit_rapm stores mode in panna_metadata (net)", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "net"

  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)

  expect_equal(model$panna_metadata$mode, "net")
  expect_equal(model$panna_metadata$n_player_cols, rapm_data$n_players)
})

test_that("fit_rapm defaults mode to \"od\" when rapm_data has no $mode element (regression guard)", {
  skip_if_not_installed("glmnet")
  set.seed(7)
  n_players <- 10
  player_ids <- paste0("odp2_", seq_len(n_players))
  n_splints <- 20
  n_rows <- n_splints * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_players * 2)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 4)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 4)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
  }
  covariates <- c("is_home")
  X <- cbind(X_players, matrix(rep(c(1, 0), n_splints), nrow = n_rows))
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), covariates)
  y <- rnorm(n_rows, mean = 1, sd = 0.4)
  weights <- runif(n_rows, 5, 30)
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("OD2 Player", seq_len(n_players)),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rapm_data <- list(
    X = X, y = y, weights = weights, player_ids = player_ids,
    player_mapping = player_mapping, covariate_cols = covariates,
    covariate_names = covariates, n_players = n_players
    # no $mode element -- older fixture shape
  )

  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)
  expect_equal(model$panna_metadata$mode, "od")
  expect_equal(model$panna_metadata$n_player_cols, n_players * 2)
})

test_that("extract_rapm_ratings net mode extracts _net coefficients, offense/defense NA", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "net"

  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)
  ratings <- extract_rapm_ratings(model)

  expect_equal(nrow(ratings), rapm_data$n_players)
  expect_setequal(ratings$player_id, rapm_data$player_ids)
  expect_true(all(is.finite(ratings$rapm)))
  expect_true(all(is.na(ratings$offense)))
  expect_true(all(is.na(ratings$defense)))
})

test_that("extract_rapm_ratings od mode is unaffected by mode-awareness (regression guard)", {
  skip_if_not_installed("glmnet")
  # Self-contained od-mode fixture (not create_test_rapm_data() from
  # test-rapm-model.R -- testthat does not share top-level helpers defined
  # inside another test-*.R file across files, only tests/testthat/helper-*.R).
  set.seed(7)
  n_players <- 8
  player_ids <- paste0("odp3_", seq_len(n_players))
  n_splints <- 20
  n_rows <- n_splints * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_players * 2)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 3)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 3)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
  }
  X <- cbind(X_players, matrix(rep(c(1, 0), n_splints), nrow = n_rows))
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), "is_home")
  y <- rnorm(n_rows, mean = 1, sd = 0.4)
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("OD3 Player", seq_len(n_players)),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rapm_data <- list(
    X = X, y = y, weights = runif(n_rows, 5, 30), player_ids = player_ids,
    player_mapping = player_mapping, covariate_cols = "is_home",
    covariate_names = "is_home", n_players = n_players, mode = "od"
  )

  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)
  ratings <- extract_rapm_ratings(model)

  expect_false(any(is.na(ratings$offense)))
  expect_false(any(is.na(ratings$defense)))
  # defense positive=good (2026-09-03), so rapm is additive now.
  expect_equal(ratings$rapm, ratings$offense + ratings$defense)
})

test_that("extract_rapm_ratings aborts if coefficients don't match the declared mode", {
  skip_if_not_installed("glmnet")
  rapm_data <- .net_mode_fit_data()
  rapm_data$mode <- "net"
  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)

  model$panna_metadata$mode <- "od"
  expect_error(extract_rapm_ratings(model), "don't match")
})

test_that("od-mode ridge fit on a TRUE zero-sum target mirrors offense/defense hard enough to trip the D5 tripwire (empirical proof mode=\"net\" is required for WPA)", {
  skip_if_not_installed("glmnet")
  set.seed(123)
  n_players <- 16
  player_ids <- paste0("mirror_p", seq_len(n_players))
  n_splints <- 60
  n_rows <- n_splints * 2
  X_players <- matrix(0, nrow = n_rows, ncol = n_players * 2)
  target <- numeric(n_rows)
  for (i in seq_len(n_splints)) {
    home_players <- sample(seq_len(n_players), 5)
    away_players <- sample(setdiff(seq_len(n_players), home_players), 5)
    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2
    for (p in home_players) X_players[row_home, p] <- 1
    for (p in away_players) X_players[row_home, n_players + p] <- 1
    for (p in away_players) X_players[row_away, p] <- 1
    for (p in home_players) X_players[row_away, n_players + p] <- 1
    t_home <- rnorm(1, 0, 0.4)
    target[row_home] <- t_home
    target[row_away] <- -t_home  # exactly zero-sum, like WPA (D2)
  }
  X <- cbind(X_players, matrix(rep(c(1, 0), n_splints), nrow = n_rows))
  colnames(X) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"), "is_home")
  player_mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("Mirror Player", seq_len(n_players)),
    total_minutes = sample(500:3000, n_players, replace = TRUE),
    stringsAsFactors = FALSE
  )
  rapm_data <- list(
    X = X, y = target, weights = runif(n_rows, 5, 30), player_ids = player_ids,
    player_mapping = player_mapping, covariate_cols = "is_home",
    covariate_names = "is_home", n_players = n_players, mode = "od"
  )

  model <- fit_rapm(rapm_data, alpha = 0, nfolds = 3, parallel = FALSE)
  ratings <- extract_rapm_ratings(model)

  # A ridge fit on a truly zero-sum target is symmetric under the
  # (row, off/def role, target sign) swap, so its unique regularized solution
  # is (very close to) a fixed point of that symmetry: offense and defense
  # are mirror images (perfectly anti-correlated) for every player -- true
  # regardless of which sign convention `defense` is published under, which
  # is why the assertion below uses abs(cor(...)) rather than a signed
  # equality. Assert against the ACTUAL consequence -- this is exactly
  # what would abort 04_rapm.R's multi-target WPA fit (D5's cor_threshold =
  # 0.9) if it were left in mode = "od" instead of "net".
  expect_gt(abs(cor(ratings$offense, ratings$defense)), 0.9)
  expect_error(.check_degenerate_multi_target(ratings, "wpa"), "mirrored")
})


# ===========================================================================
# Step 5 (FABLE-PRIOR-FIX-PLAN.md) end-to-end chain-wiring test: proves the
# L2/L3 class of bug (a dead SPM prior silently never reaching
# fit_rapm_with_prior()) cannot silently return. Synthetic splints with a
# zero-sum WPA target -> prepare_rapm_data(mode = "net") ->
# build_prior_vector()-built named net prior -> fit_rapm_with_prior(mode =
# "net") -> extract_xrapm_ratings() returns non-NA xrapm AND the prior
# demonstrably matched > 0 players (the exact quantity L2/L3 silently zeroed).
# ===========================================================================

.e2e_net_wpa_splint_data <- function(n_matches = 15, splints_per_match = 4, n_players = 16) {
  set.seed(99)
  player_ids <- paste0("e2e_p", seq_len(n_players))
  splints_list <- list()
  players_list <- list()
  ctr <- 0L

  for (m in seq_len(n_matches)) {
    match_id <- paste0("e2e_m", m)
    home_pool <- sample(player_ids, 6)
    away_pool <- sample(setdiff(player_ids, home_pool), 6)
    start_min <- 0
    for (s in seq_len(splints_per_match)) {
      ctr <- ctr + 1L
      splint_id <- paste0("e2e_s", ctr)
      dur <- runif(1, 5, 15)
      end_min <- start_min + dur
      wpa_home <- rnorm(1, 0, 0.02)

      splints_list[[ctr]] <- data.frame(
        splint_id = splint_id, match_id = match_id, duration = dur,
        start_minute = start_min, end_minute = end_min,
        gf_home = 0, ga_home = 0, avg_min = (start_min + end_min) / 2,
        n_players_home = 11, n_players_away = 11,
        wpa_home = wpa_home, wpa_away = -wpa_home,
        stringsAsFactors = FALSE
      )

      home_players <- sample(home_pool, 4)
      away_players <- sample(away_pool, 4)
      players_list[[ctr]] <- rbind(
        data.frame(splint_id = splint_id, match_id = match_id, player_id = home_players,
                   player_name = home_players, is_home = 1L, stringsAsFactors = FALSE),
        data.frame(splint_id = splint_id, match_id = match_id, player_id = away_players,
                   player_name = away_players, is_home = 0L, stringsAsFactors = FALSE)
      )
      start_min <- end_min
    }
  }

  list(splints = do.call(rbind, splints_list), players = do.call(rbind, players_list))
}

test_that("Step 5 end-to-end: build_prior_vector() priors demonstrably reach fit_rapm_with_prior (net mode, WPA)", {
  skip_if_not_installed("glmnet")
  splint_data <- .e2e_net_wpa_splint_data()

  rapm_data <- prepare_rapm_data(splint_data, min_minutes = 10,
                                 target_type = "wpa", include_covariates = TRUE,
                                 mode = "net")

  # A synthetic net-SPM prediction table, exactly the shape 05_spm.R's single
  # net-SPM fit produces -- the input build_prior_vector() expects.
  real_ids <- rapm_data$player_mapping$player_id[rapm_data$player_mapping$player_id != "replacement"]
  spm_ratings <- data.frame(player_id = real_ids, stringsAsFactors = FALSE)
  spm_ratings$spm <- stats::rnorm(nrow(spm_ratings), 0, 0.05)

  net_prior <- build_prior_vector(
    spm_data = spm_ratings, spm_col = "spm",
    player_mapping = rapm_data$player_mapping
  )

  # The L2/L3 bug this proves fixed: the old bespoke `pid %in% names(player_map)`
  # loop (player_map a data.frame -> names() are COLUMN names, never a
  # player_id) matched 0 players; even patched, an unnamed aligned vector
  # still matches 0 against fit_rapm_with_prior()'s match(player_ids,
  # names(offense_prior)). A correctly-named build_prior_vector() output
  # matches every regular player.
  expect_gt(sum(net_prior != 0), 0)
  expect_equal(sum(net_prior != 0), nrow(spm_ratings))

  model <- fit_rapm_with_prior(rapm_data, offense_prior = net_prior,
                               defense_prior = NULL, alpha = 0, nfolds = 3,
                               mode = "net")
  expect_equal(model$panna_metadata$type, "xrapm_net")

  ratings <- extract_xrapm_ratings(model)
  # n_players_total (not n_players): extract_xrapm_ratings() returns one row
  # per rapm_data$player_ids entry, which includes the "replacement" pool row.
  expect_equal(nrow(ratings), rapm_data$n_players_total)
  expect_true(all(is.finite(ratings$xrapm)))
  expect_true(all(is.na(ratings$offense)))
  expect_true(all(is.na(ratings$defense)))

  # The D5 tripwire this chain must survive without a false-positive abort
  # (Step 5's net-mode-aware .check_degenerate_multi_target()).
  expect_true(isTRUE(.check_degenerate_multi_target(ratings, "wpa")))
})
