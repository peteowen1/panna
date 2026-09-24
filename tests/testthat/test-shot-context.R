# Pre-shot context for xG v5 / xGOT v3 (R/shot_context.R). The full-data check
# (identical to the training features on 54,948 A-League and 147,120 EPL shots)
# lives in the plan; these pin the rules on a match small enough to read.

ev_fixture <- function() {
  q <- function(...) paste0("{", paste(sprintf('"%s":null', c(...)), collapse = ","), "}")
  data.frame(
    match_id = "M1",
    event_id = c(10, 11, 12, 13, 14, 15, 16, 17),
    type_id  = c(1L, 1L, 1L, 15L, 16L, 1L, 16L, 13L),
    team_id  = c("A", "B", "B", "B", "B", "A", "A", "B"),
    period_id = 1L,
    minute   = c(1L, 2L, 2L, 2L, 2L, 5L, 6L, 8L),
    second   = c(0L, 0L, 10L, 20L, 23L, 0L, 0L, 0L),
    outcome  = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L),
    x = c(50, 60, 80, 90, 92, 40, 30, 85), y = 50,
    qualifier_json = c(q("5"), q("2"), q("4", "210"), q("23"), q("89"), q("1"), q("28"), q("215")),
    stringsAsFactors = FALSE)
}

test_that(".shot_context reads assist, possession, rebound and score correctly", {
  cx <- panna:::.shot_context(ev_fixture())
  data.table::setkey(cx, event_id)
  expect_equal(nrow(cx), 4L)
  s13 <- cx["13"]; s14 <- cx["14"]; s16 <- cx["16"]; s17 <- cx["17"]
  # shot 13: through ball (tagged 210) 10 s earlier by the same team; fast break tag
  expect_true(s13$has_assist); expect_equal(s13$a4, 1L); expect_equal(s13$a2, 0L); expect_equal(s13$q23, 1L)
  # B last lost the ball to A at 01:00 -> 80 s of possession, two completed passes since
  expect_equal(s13$poss_secs, 80); expect_equal(s13$poss_passes, 2L)
  # shot 14: 3 s after shot 13 -> rebound; the 210 pass is 13 s back, still the assist
  expect_equal(s14$rebound, 1L); expect_equal(s13$rebound, 0L); expect_true(s14$has_assist)
  # shot 16 is A's OWN goal (qualifier 28): it counts for B. Shot 17 is B's,
  # after B's goal (14) and that own goal: B lead 2-0
  expect_equal(s17$goals_for, 2L); expect_equal(s17$goals_against, 0L); expect_equal(s17$score_diff, 2L)
  # shot 17: no assist within 20 s, so every assist column is NA, not 0
  expect_false(s17$has_assist); expect_true(is.na(s17$a4)); expect_true(is.na(s17$a_len))
  # outcome-revealing tags are never produced
  expect_false(any(c("q217", "q468", "q214") %in% names(cx)))
})

test_that(".shot_context orders same-second events by the NUMERIC event id", {
  ev <- ev_fixture()
  # two events in the same second: the pass must count as BEFORE the shot even
  # though "9" sorts after "13" as text
  ev <- rbind(ev, transform(ev[ev$event_id == 12, ], event_id = 9, minute = 2L, second = 20L))
  cx <- panna:::.shot_context(ev)
  expect_equal(cx[event_id == "13", poss_passes], 3L)
})

test_that(".shot_foot_history counts only EARLIER matches", {
  se <- data.frame(player_id = "P", match_id = c("m1", "m1", "m2", "m3"),
                   body_part = c("RightFoot", "LeftFoot", "RightFoot", "RightFoot"),
                   match_date = as.Date(c("2025-01-01", "2025-01-01", "2025-01-08", "2025-01-15")))
  fh <- panna:::.shot_foot_history(se)
  data.table::setkey(fh, match_id)
  expect_equal(fh["m1"]$n_prev, 0L)                     # nothing before the first match
  expect_equal(fh["m2"]$n_prev, 2L); expect_equal(fh["m2"]$r_prev, 1L)
  expect_equal(fh["m3"]$n_prev, 3L); expect_equal(fh["m3"]$r_prev, 2L)   # m3's own shot is not counted
  expect_equal(panna:::.foot_share(0, 1, r_prev = 1, n_prev = 3, min_n = 2), 1 / 3)
  expect_true(is.na(panna:::.foot_share(0, 1, r_prev = 1, n_prev = 3, min_n = 10)))
  expect_true(is.na(panna:::.foot_share(1, 0, r_prev = 1, n_prev = 30)))   # headers: no foot
})

test_that("a context model refuses to score without events or foot history", {
  shots <- data.frame(match_id = "M1", original_event_id = 13, player_id = "P")
  feats <- data.frame(is_header = 0, is_right_foot = 1)
  expect_error(panna:::.add_shot_context_features(feats, shots, c("poss_secs", "rebound")), "no .*events")
  expect_error(panna:::.add_shot_context_features(feats, shots, "foot_share"), "foot_history")
  # with events: filled from .shot_context, matched on original_event_id
  out <- panna:::.add_shot_context_features(feats, shots, c("poss_secs", "rebound"), events = ev_fixture())
  expect_equal(out$poss_secs, 80); expect_equal(out$rebound, 0)
  # events for the wrong match abort instead of scoring blind
  shots_x <- data.frame(match_id = "OTHER", original_event_id = 13, player_id = "P")
  expect_error(panna:::.add_shot_context_features(feats, shots_x, "rebound", events = ev_fixture()), "only 0%")
})

test_that("predict_xg keeps NA for na_is_missing models and zero-fills older ones", {
  skip_if_not_installed("xgboost")
  set.seed(1)
  n <- 400
  X <- cbind(a = c(rep(NA, n / 2), runif(n / 2)), b = runif(n))
  y <- as.numeric(is.na(X[, "a"]))                        # NA itself is the signal
  m <- xgboost::xgb.train(params = list(objective = "binary:logistic", max_depth = 2, nthread = 1),
                          data = xgboost::xgb.DMatrix(X, label = y, missing = NA), nrounds = 20, verbose = 0)
  newx <- data.frame(a = NA_real_, b = 0.5)
  new_model <- list(model = m, panna_metadata = list(feature_cols = c("a", "b"), na_is_missing = TRUE))
  old_model <- list(model = m, panna_metadata = list(feature_cols = c("a", "b")))
  expect_gt(panna::predict_xg(new_model, newx), 0.8)   # NA branch -> high
  expect_lt(panna::predict_xg(old_model, newx), 0.2)   # zero-filled -> the non-NA branch
})

test_that(".penalty_xg_for uses the model's by-season table, else PENALTY_XG", {
  m <- list(panna_metadata = list(penalty_xg_by_season = c(`2025` = 0.7783, `2026` = 0.7783, `2027` = 0.7775)))
  expect_equal(panna:::.penalty_xg_for(m, "2026-2027"), 0.7775)
  expect_equal(panna:::.penalty_xg_for(m, "2030"), 0.7775)          # latest known season
  expect_equal(panna:::.penalty_xg_for(m, "2020-2021"), 0.7783)     # before the table: its first season
  expect_equal(panna:::.penalty_xg_for(list(panna_metadata = list()), "2026-2027"), PENALTY_XG)
  expect_equal(panna:::.penalty_xg_for(m, NULL), PENALTY_XG)
})
