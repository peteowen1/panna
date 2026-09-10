# Tests for the cup pairwise knockout lookup (R/cup_pairwise_model.R)
#
# Mirrors test-knockout-model.R's synthetic-fixture style: a tiny goals model
# whose output is a known, separable function of home_field and elo_diff, so
# the three header-comment claims (nearest-row state, unnegated home_field,
# unaveraged legs) are directly observable rather than assumed.

.cup_fixture <- function() {
  skip_if_not_installed("xgboost")

  # Rows: A vs B is the only real league-phase pairing needed to seed the
  # team pool. Z vs W is a Jul/Aug "qualifying round" sharing the same league
  # code -- must NOT appear in the team pool.
  #
  # Team A gets THREE rows at different dates/leagues so the "nearest to
  # as_of, any league" rule is distinguishable from "most recent played" and
  # from "this competition only":
  #   UCL league-phase (2025-09-10, elo=1500) -- 127 days from as_of
  #   ENG domestic      (2025-11-01, elo=1600) -- 75 days from as_of
  #   ENG domestic      (2026-01-12, elo=1650) -- 3 days from as_of (nearest)
  as_of <- as.Date("2026-01-15")
  rows <- data.frame(
    league     = c("UCL", "UCL", "UCL", "ENG", "ENG"),
    season     = c("2025-2026", "2025-2026", "2025-2026", "2025-2026", "2025-2026"),
    match_date = as.Date(c("2025-08-01", "2025-09-10", "2025-09-10", "2025-11-01", "2026-01-12")),
    home_team  = c("Z", "A", "C", "A", "A"),
    away_team  = c("W", "B", "D", "Opponent1", "Opponent2"),
    home_elo   = c(1400, 1500, 1520, 1600, 1650),
    away_elo   = c(1410, 1510, 1530, 1300, 1310),
    diff_elo   = c(-10, -10, -10, 300, 340),
    home_field = c(1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  )
  feature_cols <- c("home_elo", "away_elo", "diff_elo", "home_field")

  # Deterministic-enough goals model: home goals = 5 + 3*home_field (elo_diff
  # deliberately near-constant across training rows so XGBoost's split on
  # home_field is the dominant, cleanly-recoverable signal).
  set.seed(11)
  n <- 300L
  hf <- sample(c(-1, 0, 1), n, replace = TRUE)
  ed <- stats::rnorm(n, 0, 5)
  X <- cbind(home_elo = 1500, away_elo = 1500, diff_elo = ed, home_field = hf)
  y_home <- 5 + 3 * hf + 0.01 * ed
  y_away <- 5 - 3 * hf - 0.01 * ed

  fit_reg <- function(target) {
    xgboost::xgb.train(
      params = list(objective = "reg:squarederror", max_depth = 3L, eta = 0.3),
      data = xgboost::xgb.DMatrix(data = X, label = target),
      nrounds = 40L, verbose = 0L)
  }
  gm <- list(home = list(model = fit_reg(y_home)), away = list(model = fit_reg(y_away)))

  aug <- c(feature_cols, "pred_home_goals", "pred_away_goals", "pred_goal_diff", "pred_total_goals")
  hg <- stats::predict(gm$home$model, xgboost::xgb.DMatrix(X))
  ag <- stats::predict(gm$away$model, xgboost::xgb.DMatrix(X))
  Xa <- cbind(X, pred_home_goals = hg, pred_away_goals = ag,
              pred_goal_diff = hg - ag, pred_total_goals = hg + ag)[, aug]
  cls <- as.integer(hf > 0)  # crude but enough to fit a 2-class-ish softprob
  cls[seq(1, n, 41)] <- 2L
  om <- list(model = list(model = xgboost::xgb.train(
    params = list(objective = "multi:softprob", num_class = 3L, max_depth = 3L, eta = 0.3),
    data = xgboost::xgb.DMatrix(data = Xa, label = cls), nrounds = 40L, verbose = 0L)))

  goals_models   <- list(feature_cols = feature_cols, pooled = gm)
  outcome_result <- list(augmented_features = aug, pooled = om)

  list(match_dataset = rows, goals_models = goals_models, outcome_result = outcome_result, as_of = as_of)
}

test_that("qualifying-round rows (month < 9) are excluded from the team pool", {
  fx <- .cup_fixture()
  lk <- build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                                   "UCL", "2025-2026", as_of = fx$as_of, verbose = FALSE)
  expect_setequal(unique(c(lk$probs$t1, lk$probs$t2)), c("A", "B", "C", "D"))
  expect_false(any(c("Z", "W") %in% c(lk$probs$t1, lk$probs$t2)))
})

test_that("team state is the row NEAREST as_of, any league -- not most-recent-played or cup-only", {
  fx <- .cup_fixture()
  lk <- build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                                   "UCL", "2025-2026", as_of = fx$as_of, verbose = FALSE)
  # The Jan-12 ENG row (3 days from as_of) must win over the Sept UCL
  # participation row (127 days) and the Nov ENG row (75 days).
  expect_identical(lk$team_as_of[["A"]], as.Date("2026-01-12"))
})

test_that("home_field is +1 for whichever team hosts EACH leg -- never negated", {
  fx <- .cup_fixture()
  lk <- build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                                   "UCL", "2025-2026", as_of = fx$as_of, verbose = FALSE)
  row <- lk$probs[(t1 == "A" & t2 == "B") | (t1 == "B" & t2 == "A")]
  expect_equal(nrow(row), 1L)
  # Training target was home = 5 + 3*home_field: home_field=+1 -> ~8,
  # home_field=-1 -> ~2. If leg2 incorrectly negated home_field (the WC
  # host-NATION convention this function deliberately does NOT use), leg2's
  # home-team goals would read near 2 instead of near 8.
  expect_gt(row$leg1_home_goals, 6)
  expect_gt(row$leg2_home_goals, 6)
})

test_that("the two legs are genuinely different predictions, not one averaged into both", {
  fx <- .cup_fixture()
  lk <- build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                                   "UCL", "2025-2026", as_of = fx$as_of, verbose = FALSE)
  row <- lk$probs[(t1 == "A" & t2 == "B") | (t1 == "B" & t2 == "A")]
  # A and B have different elo state, so their two legs' scorelines must
  # differ from each other (leg1 = t1 home, leg2 = t2 home) -- averaging them
  # into one symmetrised number (WC's convention, wrong for a two-legged tie)
  # would make leg1_home_goals == leg2_away_goals exactly, which this is NOT
  # testing for; it's testing that leg1 and leg2 are each their own real
  # prediction rather than trivially mirrored copies.
  expect_false(isTRUE(all.equal(
    c(row$leg1_home_goals, row$leg1_away_goals),
    c(row$leg2_away_goals, row$leg2_home_goals))))
})

test_that("build_cup_pairwise_lookup rejects a non-UEFA-cup league", {
  fx <- .cup_fixture()
  expect_error(
    build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                               "ENG", "2025-2026", as_of = fx$as_of, verbose = FALSE),
    "UCL/UEL/UECL")
})

test_that("build_cup_pairwise_lookup aborts on zero league-phase rows", {
  fx <- .cup_fixture()
  expect_error(
    build_cup_pairwise_lookup(fx$match_dataset, fx$goals_models, fx$outcome_result,
                               "UEL", "2025-2026", as_of = fx$as_of, verbose = FALSE),
    "no league-phase rows")
})
