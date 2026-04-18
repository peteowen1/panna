# Tests for the 5-min boundary merge added to create_splint_boundaries_fast
# in the 2026-04-18 splint rebuild. Hard boundaries (kickoff, halftime,
# match-end) are always kept; soft boundaries (subs/goals/reds) are dropped
# if they fall within min_splint_duration of the most recently kept boundary.

test_that("boundary merge enforces min splint duration on soft boundaries", {
  events <- data.frame(
    minute    = c(45, 47),     # halftime + sub 2 min later — sub should drop
    added_time = c(0, 30/60),
    is_goal   = c(FALSE, FALSE),
    is_sub    = c(FALSE, TRUE),
    is_home   = c(TRUE, TRUE)
  )
  b <- create_splint_boundaries_fast(events, include_goals = TRUE,
                                      include_halftime = TRUE,
                                      min_splint_duration = 5)
  # Halftime boundary (45 or 46) kept; sub at ~47.5 dropped because it's
  # within 5 min of halftime. Resulting splint runs from halftime to match-end.
  durations <- b$end_minute - b$start_minute
  expect_true(all(durations[durations > 0.001] >= 5 - 1e-6 |
                  b$end_minute %in% c(45, 46, 91)))  # tolerance for FP
})

test_that("boundary merge keeps hard boundaries even when close together", {
  # Events near match end: a sub at 89 and full-time at 91 (only 2 min apart).
  # Match-end (91) is HARD — must stay even though it's <5 min from the sub.
  events <- data.frame(
    minute    = c(89),
    added_time = c(0),
    is_goal   = c(FALSE),
    is_sub    = c(TRUE),
    is_home   = c(TRUE)
  )
  b <- create_splint_boundaries_fast(events, include_halftime = FALSE,
                                      min_splint_duration = 5)
  # Match end is in boundaries
  expect_true(91 %in% b$end_minute)
})

test_that("boundary merge with min_splint_duration = 0 acts as no-op", {
  events <- data.frame(
    minute    = c(45, 45.5, 46),  # three subs all within 1 min of each other
    added_time = c(0, 0, 0),
    is_goal   = c(FALSE, FALSE, FALSE),
    is_sub    = c(TRUE, TRUE, TRUE),
    is_home   = c(TRUE, TRUE, TRUE)
  )
  b0 <- create_splint_boundaries_fast(events, include_halftime = FALSE,
                                       include_goals = FALSE,
                                       min_splint_duration = 0)
  b5 <- create_splint_boundaries_fast(events, include_halftime = FALSE,
                                       include_goals = FALSE,
                                       min_splint_duration = 5)
  # With merge off, all three sub boundaries kept → 4 splints (0-45,
  # 45-45.5, 45.5-46, 46-91). With 5-min merge, only the first sub kept.
  expect_gt(nrow(b0), nrow(b5))
})

test_that("boundary merge applies second-precision sub timing from added_time", {
  # A sub at 67:42 (encoded as added_time = 42/60 = 0.7) — should produce
  # a boundary at 67.7, not 67.0.
  events <- data.frame(
    minute    = c(67),
    added_time = c(42/60),
    is_goal   = c(FALSE),
    is_sub    = c(TRUE),
    is_home   = c(TRUE)
  )
  b <- create_splint_boundaries_fast(events, include_halftime = TRUE,
                                      min_splint_duration = 0)  # no merge
  expect_true(any(abs(c(b$start_minute, b$end_minute) - 67.7) < 1e-6))
})


# Tests for fractional player shares in assign_players_to_splints_fast
test_that("assign_players_to_splints_fast computes share = overlap/duration", {
  boundaries <- data.frame(
    splint_num = 1:2,
    start_minute = c(0, 60),
    end_minute   = c(60, 90)
  )
  # Player A: starter (on=0, off=70) — full splint 1, partial splint 2
  # Player B: sub (on=70, off=90) — not in splint 1, partial splint 2
  lineups <- data.frame(
    match_id   = "M1",
    team       = "HOME",
    is_home    = TRUE,
    player_name = c("A", "B"),
    player_id   = c("A", "B"),
    minutes    = c(70, 20),
    on_minute  = c(0, 70),
    off_minute = c(70, 90)
  )
  out <- panna:::assign_players_to_splints_fast(boundaries, lineups, "M1")

  # Player A in splint 1: share = (60-0)/60 = 1.0
  a_s1 <- out[out$player_id == "A" & out$splint_num == 1, ]
  expect_equal(a_s1$share, 1)
  # Player A in splint 2: share = (70-60)/30 = 0.333
  a_s2 <- out[out$player_id == "A" & out$splint_num == 2, ]
  expect_equal(a_s2$share, 10/30, tolerance = 1e-6)
  # Player B in splint 1: NOT included (no overlap — came on at 70, splint ended at 60)
  expect_equal(nrow(out[out$player_id == "B" & out$splint_num == 1, ]), 0)
  # Player B in splint 2: share = (90-70)/30 = 0.667
  b_s2 <- out[out$player_id == "B" & out$splint_num == 2, ]
  expect_equal(b_s2$share, 20/30, tolerance = 1e-6)
})

test_that("share-sum across players in a splint = number of teams × 11 (full lineup)", {
  # 22 players (11 home + 11 away), all on for the full splint
  boundaries <- data.frame(splint_num = 1L, start_minute = 0, end_minute = 90)
  ids <- sprintf("P%02d", 1:22)
  lineups <- data.frame(
    match_id   = "M1",
    team       = c(rep("HOME", 11), rep("AWAY", 11)),
    is_home    = c(rep(TRUE, 11), rep(FALSE, 11)),
    player_name = ids,
    player_id   = ids,
    minutes    = 90,
    on_minute  = 0,
    off_minute = 90
  )
  out <- panna:::assign_players_to_splints_fast(boundaries, lineups, "M1")
  expect_equal(sum(out$share), 22)  # 22 players × share=1 each
})


# Tests for the directional sign constraints in fit_spm_model
test_that("fit_spm_model respects upper_limits constraint", {
  set.seed(123)
  n <- 200
  data <- data.frame(
    rapm = rnorm(n),
    total_minutes = sample(500:3000, n, replace = TRUE),
    feature_a_p90 = rnorm(n),
    feature_b_p90 = rnorm(n)
  )
  # Force coefficients to be <= 0 for both features
  fit <- fit_spm_model(
    data,
    predictor_cols = c("feature_a_p90", "feature_b_p90"),
    weight_by_minutes = FALSE,
    upper_limits = c(feature_a_p90 = 0, feature_b_p90 = 0)
  )
  fit_obj <- fit$glmnet.fit
  lam_idx <- which.min(abs(fit_obj$lambda - fit$lambda.min))
  beta <- as.numeric(fit_obj$beta[, lam_idx])
  expect_true(all(beta <= 0 + 1e-8))
})

test_that("fit_spm_model respects lower_limits constraint", {
  set.seed(456)
  n <- 200
  data <- data.frame(
    rapm = rnorm(n),
    total_minutes = sample(500:3000, n, replace = TRUE),
    bad_a_p90 = rnorm(n),
    bad_b_p90 = rnorm(n)
  )
  fit <- fit_spm_model(
    data,
    predictor_cols = c("bad_a_p90", "bad_b_p90"),
    weight_by_minutes = FALSE,
    lower_limits = c(bad_a_p90 = 0, bad_b_p90 = 0)
  )
  fit_obj <- fit$glmnet.fit
  lam_idx <- which.min(abs(fit_obj$lambda - fit$lambda.min))
  beta <- as.numeric(fit_obj$beta[, lam_idx])
  expect_true(all(beta >= 0 - 1e-8))
})

test_that("fit_spm_model NULL limits = unconstrained (default behavior)", {
  set.seed(789)
  n <- 200
  data <- data.frame(
    rapm = rnorm(n),
    total_minutes = sample(500:3000, n, replace = TRUE),
    feature_a_p90 = rnorm(n),
    feature_b_p90 = rnorm(n)
  )
  fit_default <- fit_spm_model(data, predictor_cols = c("feature_a_p90","feature_b_p90"),
                                weight_by_minutes = FALSE)
  fit_open    <- fit_spm_model(data, predictor_cols = c("feature_a_p90","feature_b_p90"),
                                weight_by_minutes = FALSE,
                                lower_limits = -Inf, upper_limits = Inf)
  # Both should produce the same lambda.min (within floating point)
  expect_equal(fit_default$lambda.min, fit_open$lambda.min, tolerance = 1e-10)
})
