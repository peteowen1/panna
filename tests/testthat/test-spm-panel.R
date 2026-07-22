# Tests for the SPM panel-training machinery
# (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/3.1, Wave 2).
#
# Self-contained fixtures -- deliberately NOT shared with other test files'
# fixtures (test-rapm-tripwires.R / test-spm-circularity.R convention: no
# dependency on cross-file execution order).

# ===========================================================================
# Fixture helpers
# ===========================================================================

#' Minimal match-grain box-stat fixture (compute_match_level_opta_stats()
#' shape): a handful of raw counting columns is enough -- .calculate_opta_per90()
#' / .calculate_opta_derived_features() default any column they can't find
#' to 0 via .safe_col() (a warning, not an error).
.panel_test_match_stats <- function() {
  # p1: outfield CB, matches in season_end_year 2021, 2022 (both inside a
  # [2021, 2023) window) AND 2023 (must be EXCLUDED from a vintage-2023
  # panel row -- window is strictly < cutoff_year).
  # p2: GK, same season spread -- must be excluded from the panel by default
  # (include_gk = FALSE).
  mk_rows <- function(player_id, position, position_side, season_end_year, n, minutes_each) {
    data.frame(
      match_id = paste0(player_id, "_", season_end_year, "_", seq_len(n)),
      player_id = player_id, player_name = player_id,
      position = position, position_side = position_side,
      team_id = "t1", team_name = "Team A", league = "TESTLG",
      season = as.character(season_end_year), season_end_year = season_end_year,
      total_minutes = minutes_each,
      goals = 1, assists = 0, tackles = 2, touches = 40, passes = 30,
      clearances = 3, aerial_won = 1, crosses = 1,
      stringsAsFactors = FALSE
    )
  }
  rbind(
    mk_rows("p1", "Defender", "Centre", 2021, 10, 90),
    mk_rows("p1", "Defender", "Centre", 2022, 10, 90),
    mk_rows("p1", "Defender", "Centre", 2023, 5, 90),   # must be excluded from vintage 2023
    mk_rows("p2", "Goalkeeper", "Centre", 2021, 10, 90),
    mk_rows("p2", "Goalkeeper", "Centre", 2022, 10, 90)
  )
}

#' A correctly-stamped rapm_window_targets fixture for one vintage.
.panel_test_targets <- function(vintage = 2023L, window = c(2021L, 2023L),
                                player_ids = c("p1", "p2"), provenance = "prior_free_rapm_window") {
  ratings <- data.frame(
    player_id = player_ids,
    rapm = c(0.10, -0.05)[seq_along(player_ids)],
    offense = c(0.06, 0.0)[seq_along(player_ids)],
    defense = c(0.04, -0.05)[seq_along(player_ids)]
  )
  vintage_entry <- list(ratings = ratings, lambda_min = 0.3, n_obs = 500,
                        window = window, target_provenance = provenance)
  out <- stats::setNames(list(vintage_entry), as.character(vintage))
  attr(out, "target_provenance") <- provenance
  out
}


# ===========================================================================
# Panel row-count / window-alignment invariants
# ===========================================================================

test_that("build_spm_panel aligns the feature window to the target window and excludes out-of-window matches", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  targets <- .panel_test_targets()

  panel <- build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                           include_xmetrics = FALSE)

  expect_equal(nrow(panel), 1L)  # only p1 -- p2 is GK, excluded by default
  expect_equal(panel$player_id, "p1")
  expect_equal(panel$vintage_year, 2023)
  expect_equal(panel$window_min_year, 2021)
  expect_equal(panel$window_max_year, 2023)
  # window_minutes must reflect ONLY the 2021+2022 matches (20 x 90 = 1800),
  # NOT the 2023-season matches (5 x 90 = 450) that fall outside [2021, 2023).
  expect_equal(panel$window_minutes, 1800)
  expect_equal(panel$role_group, "CB")
  expect_equal(panel$offense_target, 0.06)
  expect_equal(panel$defense_target, 0.04)
})


test_that("build_spm_panel excludes GK rows by default and includes them when include_gk = TRUE", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  targets <- .panel_test_targets()

  panel_no_gk <- build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                                 include_xmetrics = FALSE, include_gk = FALSE)
  expect_false("p2" %in% panel_no_gk$player_id)

  panel_with_gk <- build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                                   include_xmetrics = FALSE, include_gk = TRUE)
  expect_true("p2" %in% panel_with_gk$player_id)
  expect_equal(panel_with_gk[player_id == "p2"]$role_group, "GK")
})


test_that("build_spm_panel aborts on a feature/target window mismatch (strict_window_check)", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  # Target stamped with window [2020, 2023) but window_years = 2 computes
  # [2021, 2023) -- a real misalignment.
  targets <- .panel_test_targets(window = c(2020L, 2023L))

  expect_error(
    build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                    include_xmetrics = FALSE, strict_window_check = TRUE),
    class = "rlang_error"
  )
  # Explicit opt-out still works.
  expect_no_error(
    build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                    include_xmetrics = FALSE, strict_window_check = FALSE)
  )
})


test_that("build_spm_panel skips (warns, does not abort) a vintage with no target entry", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  targets <- .panel_test_targets()  # only vintage 2023

  expect_warning(
    panel <- build_spm_panel(match_stats, targets, vintage_years = c(2023L, 2024L), window_years = 2L,
                             include_xmetrics = FALSE),
    "no target"
  )
  expect_true(all(panel$vintage_year == 2023))
})


test_that("build_spm_panel calls assert_prior_free_target and aborts on an unstamped target", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  bad_targets <- .panel_test_targets(provenance = "xrapm_something")

  expect_error(
    build_spm_panel(match_stats, bad_targets, vintage_years = 2023L, window_years = 2L,
                    include_xmetrics = FALSE),
    class = "rlang_error"
  )
})


# ===========================================================================
# Grouped-fold machinery (R5)
# ===========================================================================

test_that("make_grouped_player_foldid keeps every player in exactly one fold", {
  player_id <- rep(paste0("p", 1:15), each = 3)  # 15 players x 3 vintage rows each
  foldid <- make_grouped_player_foldid(player_id, nfolds = 4, seed = 42)

  expect_true(isTRUE(assert_grouped_player_folds(foldid, player_id)))
  dt <- data.table::data.table(player_id = player_id, foldid = foldid)
  n_folds_per_player <- dt[, data.table::uniqueN(foldid), by = player_id]$V1
  expect_true(all(n_folds_per_player == 1L))
  expect_true(all(foldid %in% 1:4))
})


test_that("assert_grouped_player_folds fires (aborts) when a player straddles folds", {
  player_id <- c("a", "a", "b", "b", "c", "c")
  foldid <- c(1, 2, 1, 1, 2, 2)  # player 'a' straddles folds 1 and 2

  expect_error(assert_grouped_player_folds(foldid, player_id), class = "rlang_error")
  # A fixed (grouped) version of the same data passes.
  foldid_fixed <- c(1, 1, 1, 1, 2, 2)
  expect_true(isTRUE(assert_grouped_player_folds(foldid_fixed, player_id)))
})


# ===========================================================================
# fit_spm_panel(): sign constraints + circularity guard
# ===========================================================================

.panel_test_fit_data <- function(n = 200, seed = 7) {
  set.seed(seed)
  goals_p90 <- stats::runif(n, 0, 1.2)
  touches_p90 <- stats::runif(n, 20, 90)
  # Adversarial: the TRUE relationship wants a NEGATIVE goals_p90 coefficient
  # for the offense target -- an unconstrained fit should pick that up; a
  # sign-constrained fit (lower bound 0, sec 3.1) must NOT.
  offense_target <- -0.8 * goals_p90 + 0.3 * touches_p90 / 90 + stats::rnorm(n, 0, 0.05)
  panel <- data.table::data.table(
    player_id = paste0("p", seq_len(n)),
    offense_target = offense_target,
    defense_target = stats::rnorm(n, 0, 0.1),
    rapm_target = stats::rnorm(n, 0, 0.1),
    window_minutes = stats::runif(n, 900, 4000),
    role_group = "CB",
    goals_p90 = goals_p90,
    touches_p90 = touches_p90
  )
  attr(panel, "target_provenance") <- "prior_free_rapm_window"
  panel
}

test_that("fit_spm_panel respects offense sign constraints (goals_p90 coefficient >= 0)", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_fit_data()

  unconstrained <- fit_spm_panel(panel, target = "offense", role_pooling = FALSE,
                                 sign_constraints = FALSE,
                                 predictor_cols = c("goals_p90", "touches_p90"),
                                 weight_transform = "sqrt", nfolds = 3, seed = 1)
  co_unc <- as.matrix(stats::coef(unconstrained, s = "lambda.min"))
  # Sanity: the adversarial data DOES pull the unconstrained fit negative
  # (otherwise this test isn't exercising the constraint at all).
  expect_lt(co_unc["goals_p90", 1], 0)

  constrained <- fit_spm_panel(panel, target = "offense", role_pooling = FALSE,
                               sign_constraints = TRUE,
                               predictor_cols = c("goals_p90", "touches_p90"),
                               weight_transform = "sqrt", nfolds = 3, seed = 1)
  co_con <- as.matrix(stats::coef(constrained, s = "lambda.min"))
  expect_gte(co_con["goals_p90", 1], -1e-8)
})


test_that("fit_spm_panel respects defense sign constraints (tackles_p90 upper bound 0)", {
  skip_if_not_installed("glmnet")
  set.seed(11)
  n <- 200
  tackles_p90 <- stats::runif(n, 0, 4)
  touches_p90 <- stats::runif(n, 20, 90)  # unconstrained filler (glmnet needs >= 2 predictor cols)
  panel <- data.table::data.table(
    player_id = paste0("p", seq_len(n)),
    offense_target = stats::rnorm(n, 0, 0.1),
    # Adversarial: defense convention is negative = good, so a genuinely
    # POSITIVE relationship between tackles_p90 and defense_target here
    # (more tackles -> WORSE defense number) is the direction the
    # defense_good_features upper-bound-0 constraint must suppress.
    defense_target = 0.5 * tackles_p90 + stats::rnorm(n, 0, 0.05),
    rapm_target = stats::rnorm(n, 0, 0.1),
    window_minutes = stats::runif(n, 900, 4000),
    role_group = "CB",
    tackles_p90 = tackles_p90,
    touches_p90 = touches_p90
  )
  attr(panel, "target_provenance") <- "prior_free_rapm_window"

  constrained <- fit_spm_panel(panel, target = "defense", role_pooling = FALSE,
                               sign_constraints = TRUE,
                               predictor_cols = c("tackles_p90", "touches_p90"),
                               weight_transform = "sqrt", nfolds = 3, seed = 1)
  co_con <- as.matrix(stats::coef(constrained, s = "lambda.min"))
  expect_lte(co_con["tackles_p90", 1], 1e-8)
})


test_that("predict_spm_panel_net combines as offense MINUS defense (raw internal convention)", {
  skip_if_not_installed("glmnet")
  # Net RAPM = offense - defense (extract_rapm_ratings, R/rapm_model.R:
  # "RAPM rating = offense - defense"); defense_target is stored raw
  # (positive = concedes more = bad), so the net combiner must subtract.
  # Regression pin for the 2026-07-22 bake-off bug where a `+` here flipped
  # the defense half's contribution for every candidate at eval time.
  panel <- .panel_test_fit_data()
  fits <- list(
    offense = fit_spm_panel(panel, target = "offense", role_pooling = FALSE,
                            sign_constraints = FALSE,
                            predictor_cols = c("goals_p90", "touches_p90"),
                            weight_transform = "sqrt", nfolds = 3, seed = 1),
    defense = fit_spm_panel(panel, target = "defense", role_pooling = FALSE,
                            sign_constraints = FALSE,
                            predictor_cols = c("goals_p90", "touches_p90"),
                            weight_transform = "sqrt", nfolds = 3, seed = 1)
  )
  out <- predict_spm_panel_net(fits, panel)
  expect_equal(out$pred_net, out$pred_offense - out$pred_defense)
})


test_that("fit_spm_panel calls assert_prior_free_target and aborts on an unstamped panel", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_fit_data()
  attr(panel, "target_provenance") <- NULL

  expect_error(
    fit_spm_panel(panel, target = "offense", predictor_cols = c("goals_p90", "touches_p90"), nfolds = 3),
    class = "rlang_error"
  )
})


test_that("fit_spm_panel aborts on a panel stamped with a non-prior-free provenance", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_fit_data()
  attr(panel, "target_provenance") <- "xrapm_something"

  expect_error(
    fit_spm_panel(panel, target = "offense", predictor_cols = c("goals_p90", "touches_p90"), nfolds = 3),
    class = "rlang_error"
  )
})


# ===========================================================================
# CRITICAL leak fix: pooled (whole-panel) fit vs as-of (vintage_year <= Y)
# fit -- proves the old design (one global fit scored against every eval
# vintage) could leak a later vintage's own target into predictions for an
# earlier one, and that restricting training to vintage_year <= Y closes it.
# ===========================================================================

#' Two-vintage fixture with a signal PLANTED ONLY in vintage Y+1's rows:
#' `leak_feature_p90` is an EXACT copy of `offense_target` for Y+1 rows
#' (a perfect predictor, standing in for "this row's features are similar
#' enough to a later vintage of the SAME player that a pooled fit partially
#' recovers the later target"), but pure noise (no relationship to that
#' row's own target) for vintage Y rows. A model that never sees the Y+1
#' rows during training cannot have learned the leak_feature/target
#' relationship; a model trained on the pooled (Y and Y+1) panel can.
.panel_test_leak_fixture <- function(n_players = 60, Y = 2023L, seed = 99) {
  set.seed(seed)
  player_ids <- paste0("lp", seq_len(n_players))
  offense_target_Y <- stats::rnorm(n_players, 0, 0.1)
  offense_target_Y1 <- stats::rnorm(n_players, 0, 0.1)

  row_Y <- data.table::data.table(
    player_id = player_ids, vintage_year = Y,
    offense_target = offense_target_Y,
    defense_target = stats::rnorm(n_players, 0, 0.1),
    rapm_target = stats::rnorm(n_players, 0, 0.1),
    window_minutes = stats::runif(n_players, 900, 4000),
    role_group = "CB",
    leak_feature_p90 = stats::rnorm(n_players, 0, 0.1),  # pure noise in vintage Y
    filler_p90 = stats::rnorm(n_players, 0, 1)
  )
  row_Y1 <- data.table::data.table(
    player_id = player_ids, vintage_year = Y + 1L,
    offense_target = offense_target_Y1,
    defense_target = stats::rnorm(n_players, 0, 0.1),
    rapm_target = stats::rnorm(n_players, 0, 0.1),
    window_minutes = stats::runif(n_players, 900, 4000),
    role_group = "CB",
    leak_feature_p90 = offense_target_Y1,  # EXACT copy of the Y+1 target -- the plant
    filler_p90 = stats::rnorm(n_players, 0, 1)
  )
  panel <- data.table::rbindlist(list(row_Y, row_Y1))
  attr(panel, "target_provenance") <- "prior_free_rapm_window"
  panel
}

test_that("assert_asof_panel_window aborts when training rows exceed the eval vintage", {
  panel <- .panel_test_leak_fixture()
  Y <- 2023L

  train_asof <- panel[panel$vintage_year <= Y, ]
  expect_true(isTRUE(assert_asof_panel_window(train_asof, Y)))
  expect_true(all(train_asof$vintage_year <= Y))
  # Sanity: the fixture actually HAS a later vintage to exclude (otherwise
  # this test would pass trivially without exercising the guard).
  expect_true(any(panel$vintage_year > Y))

  expect_error(assert_asof_panel_window(panel, Y), class = "rlang_error")  # whole panel includes Y+1
})

test_that("LEAK PROOF: a pooled (whole-panel) fit learns a signal planted only in vintage Y+1, an as-of (vintage_year <= Y) fit cannot", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_leak_fixture()
  Y <- 2023L

  # OLD (buggy) shape: fit on the WHOLE panel, as the pre-fix
  # eval_candidate_nextwindow() effectively did (one global fit reused for
  # every eval vintage).
  fit_pooled <- fit_spm_panel(panel, target = "offense", role_pooling = FALSE,
                              sign_constraints = FALSE,
                              predictor_cols = c("leak_feature_p90", "filler_p90"),
                              weight_transform = "sqrt", nfolds = 4, seed = 1)

  # NEW (fixed) shape: fit ONLY on vintage_year <= Y (05c_candidates.R's
  # run_candidate_asof() does exactly this subsetting).
  train_asof <- panel[panel$vintage_year <= Y, ]
  assert_asof_panel_window(train_asof, Y)
  fit_asof <- fit_spm_panel(train_asof, target = "offense", role_pooling = FALSE,
                            sign_constraints = FALSE,
                            predictor_cols = c("leak_feature_p90", "filler_p90"),
                            weight_transform = "sqrt", nfolds = 4, seed = 1)

  co_pooled <- as.matrix(stats::coef(fit_pooled, s = "lambda.min"))["leak_feature_p90", 1]
  co_asof <- as.matrix(stats::coef(fit_asof, s = "lambda.min"))["leak_feature_p90", 1]

  # The pooled fit DOES pick up the planted Y+1-only signal (proves the old
  # whole-panel design was structurally capable of leaking a later vintage's
  # target into a model scored against an earlier one).
  expect_gt(abs(co_pooled), 0.15)
  # The as-of fit, trained ONLY on vintage-Y rows (where leak_feature is
  # pure noise), cannot have learned that relationship -- its coefficient
  # must be far smaller than the pooled fit's, not just numerically
  # different (guards against a flaky near-equal comparison).
  expect_lt(abs(co_asof), abs(co_pooled) / 3)
})


# ===========================================================================
# season -> season_end_year fallback (repo history with this exact bug
# class: extract_season_end_year() must be used, never exact "YYYY-YYYY"
# string match -- panna/CLAUDE.md's "Season subsetting" convention)
# ===========================================================================

test_that("build_spm_panel falls back to extract_season_end_year() when season_end_year is absent", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  match_stats[, season_end_year := NULL]  # force the fallback branch
  targets <- .panel_test_targets()

  panel <- build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                           include_xmetrics = FALSE)

  # Same expected result as the season_end_year-native test -- the fallback
  # must reproduce identical window filtering.
  expect_equal(nrow(panel), 1L)
  expect_equal(panel$player_id, "p1")
  expect_equal(panel$window_minutes, 1800)
  expect_equal(panel$window_min_year, 2021)
  expect_equal(panel$window_max_year, 2023)
})

test_that("build_spm_panel aborts when match_stats has neither season_end_year nor season", {
  match_stats <- data.table::as.data.table(.panel_test_match_stats())
  match_stats[, c("season_end_year", "season") := NULL]
  targets <- .panel_test_targets()

  expect_error(
    build_spm_panel(match_stats, targets, vintage_years = 2023L, window_years = 2L,
                    include_xmetrics = FALSE),
    class = "rlang_error"
  )
})


# ===========================================================================
# LOW fixes: player_id NA handling, predict-path role_group guard
# ===========================================================================

test_that("make_grouped_player_foldid coerces player_id and aborts on NA", {
  expect_error(
    make_grouped_player_foldid(c("a", "b", NA, "a"), nfolds = 2, seed = 1),
    class = "rlang_error"
  )
  # Non-character (e.g. factor/numeric) player_id is coerced, not an error.
  foldid <- make_grouped_player_foldid(factor(c("a", "b", "a", "c")), nfolds = 2, seed = 1)
  expect_type(foldid, "integer")
  expect_length(foldid, 4)
})

test_that("fit_spm_panel aborts with a clear message on NA player_id", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_fit_data()
  panel$player_id[3] <- NA_character_

  expect_error(
    fit_spm_panel(panel, target = "offense", role_pooling = FALSE,
                  predictor_cols = c("goals_p90", "touches_p90"), nfolds = 3),
    class = "rlang_error"
  )
})

test_that("predict_spm_panel gives a clear error when a role-pooled model is scored on newdata lacking role_group", {
  skip_if_not_installed("glmnet")
  panel <- .panel_test_fit_data()
  panel$role_group <- rep(c("CB", "FBWB", "DMCM"), length.out = nrow(panel))

  fit <- fit_spm_panel(panel, target = "offense", role_pooling = TRUE,
                       role_ambivalent_cols = "touches_p90",
                       predictor_cols = c("goals_p90", "touches_p90"),
                       sign_constraints = FALSE, nfolds = 3, seed = 1)

  newdata_no_role <- data.frame(player_id = panel$player_id,
                                goals_p90 = panel$goals_p90, touches_p90 = panel$touches_p90)
  expect_error(predict_spm_panel(fit, newdata_no_role), class = "rlang_error")

  # With role_group present, prediction succeeds.
  newdata_with_role <- newdata_no_role
  newdata_with_role$role_group <- panel$role_group
  expect_no_error(predict_spm_panel(fit, newdata_with_role))
})
