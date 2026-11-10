# Structural (home-frame) WP model, panna#273.

# Synthetic matches: the home side wins more when it leads, when it is stronger,
# and (deliberately) NOT because it has the ball.
wps_fx <- function(n_match = 300L, rows = 60L, seed = 1L) {
  set.seed(seed)
  rbindlist <- data.table::rbindlist
  rbindlist(lapply(seq_len(n_match), function(i) {
    elo <- stats::rnorm(1, 0, 150)
    hg <- stats::rpois(1, 1.5 * exp(elo / 600)); ag <- stats::rpois(1, 1.2 * exp(-elo / 600))
    t <- sort(stats::runif(rows))
    # goals land at random times; score_diff is the home margin so far
    hgt <- sort(stats::runif(hg)); agt <- sort(stats::runif(ag))
    sd <- vapply(t, function(s) sum(hgt < s) - sum(agt < s), numeric(1))
    is_home <- stats::rbinom(rows, 1, 0.5)
    res <- if (hg > ag) 1 else if (hg < ag) 0 else 0.5
    data.table::data.table(
      match_id = sprintf("m%03d", i), team_id = ifelse(is_home == 1L, "H", "A"),
      time_seconds = t * 5400, period_id = ifelse(t < 0.5, 1L, 2L),
      time_remaining = 1 - t, score_diff = sd, red_card_diff = 0L,
      is_second_half = as.integer(t >= 0.5), is_extra_time = 0L, is_home = is_home,
      epv = stats::runif(rows, -0.02, 0.05), elo_diff = elo,
      wp_label = ifelse(is_home == 1L, res, 1 - res))
  }))
}

test_that("add_wp_match_elo joins pre-match Elo, reports and drops uncovered matches", {
  wf <- data.table::data.table(match_id = c("a", "a", "b", "c"), x = 1:4)
  me <- data.frame(match_id = c("a", "b"), home_elo = c(1600, 1500), away_elo = c(1500, 1550))
  out <- add_wp_match_elo(wf, me, min_coverage = 0.5)
  expect_equal(out$elo_diff, c(100, 100, -50))
  expect_error(add_wp_match_elo(wf, me, min_coverage = 0.9), "covers only")
  kept <- add_wp_match_elo(wf, me, min_coverage = 0.5, drop_missing = FALSE)
  expect_true(is.na(kept$elo_diff[4]))
})

test_that("train/predict: possession POV, monotone in lead and strength, agnostic sets the gap to 0", {
  skip_if_not_installed("xgboost")
  d <- wps_fx()
  m <- suppressMessages(train_wp_structural(d, nrounds = 60L, nfolds = 3L))
  expect_s3_class(m, "panna_wp_structural")
  expect_false("home_has_ball" %in% m$feature_names)
  probe <- data.table::data.table(match_id = "p", team_id = "H", time_remaining = 0.3,
                                  score_diff = c(-1, 0, 1, 0, 0), red_card_diff = 0L,
                                  is_second_half = 1L, is_extra_time = 0L, is_home = 1L,
                                  epv = 0, elo_diff = c(0, 0, 0, -300, 300))
  p <- predict_wp(m, probe)
  expect_true(p[1] <= p[2] && p[2] <= p[3])          # more home goals, never lower
  expect_true(p[4] <= p[2] && p[2] <= p[5])          # stronger home side, never lower
  # Same state seen from the away side is the complement (no possession input).
  away <- data.table::copy(probe)[, `:=`(is_home = 0L, team_id = "A")]
  expect_equal(predict_wp(m, away), 1 - p, tolerance = 1e-6)
  # Team-agnostic: the Elo gap is ignored.
  ag <- predict_wp(m, probe, team_agnostic = TRUE)
  expect_equal(ag[4], ag[2], tolerance = 1e-6); expect_equal(ag[5], ag[2], tolerance = 1e-6)
  # Threat: more epv for the possessor never lowers its win chance.
  thr <- data.table::copy(probe[2])[rep(1, 3)][, epv := c(-0.1, 0, 0.3)]
  pt <- predict_wp(m, thr)
  expect_true(pt[1] <= pt[2] && pt[2] <= pt[3])
})

test_that("use_possession adds home_has_ball; a published-style model refuses team_agnostic", {
  skip_if_not_installed("xgboost")
  d <- wps_fx(n_match = 120L)
  m <- suppressMessages(train_wp_structural(d, use_possession = TRUE, nrounds = 30L, nfolds = 3L))
  expect_true("home_has_ball" %in% m$feature_names)
  expect_length(predict_wp(m, d[1:5]), 5L)
  expect_error(predict_wp(list(type = NULL, feature_names = "x"), d[1:2], team_agnostic = TRUE),
               "structural")
})

test_that("extra_features are stored, used at prediction, NA-tolerant, and required", {
  skip_if_not_installed("xgboost")
  d <- wps_fx(n_match = 100L)
  d[, stoppage_min := pmax(0, time_seconds - 5400) / 60 + ifelse(seq_len(.N) %% 7 == 0, NA, 0)]
  m <- suppressMessages(train_wp_structural(d, nrounds = 20L, nfolds = 2L, extra_features = "stoppage_min"))
  expect_identical(m$extra_features, "stoppage_min")
  expect_true("stoppage_min" %in% m$feature_names)
  expect_length(predict_wp(m, d[1:10]), 10L)
  expect_error(predict_wp(m, d[, !"stoppage_min"]), "stoppage_min")
  expect_error(train_wp_structural(d, extra_features = "elo_diff"), "repeats")
})

test_that("add_wp_xi_strength uses the snapshot before match day, x11 mean, NA under min_rated", {
  wf <- data.table::data.table(match_id = c("m1", "m1", "m2", "m2"), team_id = c("H", "A", "H", "A"),
                               is_home = c(1L, 0L, 1L, 0L))
  lu <- data.table::rbindlist(list(
    data.table::data.table(match_id = "m1", team_id = "H", player_id = paste0("h", 1:11), match_date = "2026-03-01", is_starter = TRUE),
    data.table::data.table(match_id = "m1", team_id = "A", player_id = paste0("a", 1:11), match_date = "2026-03-01", is_starter = TRUE),
    data.table::data.table(match_id = "m2", team_id = "H", player_id = paste0("h", 1:11), match_date = "2026-01-15", is_starter = TRUE),
    data.table::data.table(match_id = "m2", team_id = "A", player_id = paste0("a", 1:11), match_date = "2026-01-15", is_starter = TRUE),
    data.table::data.table(match_id = "m1", team_id = "H", player_id = "sub1", match_date = "2026-03-01", is_starter = FALSE)))
  rt <- data.table::rbindlist(list(
    data.table::data.table(player_id = paste0("h", 1:11), ref_date = as.Date("2026-02-01"), panna = 0.10),
    data.table::data.table(player_id = paste0("h", 1:11), ref_date = as.Date("2026-03-01"), panna = 9),   # match day: never used
    data.table::data.table(player_id = paste0("a", 1:10), ref_date = as.Date("2026-02-01"), panna = -0.05),
    data.table::data.table(player_id = "sub1", ref_date = as.Date("2026-02-01"), panna = 5)))
  out <- suppressMessages(add_wp_xi_strength(wf, lu, rt))
  # m1: home 11 x 0.10 = 1.1; away 10 rated x -0.05 -> 11 x mean = -0.55; diff 1.65
  expect_equal(out[match_id == "m1", unique(xi_diff)], 1.65, tolerance = 1e-12)
  # m2 (2026-01-15): no snapshot on or before 2026-01-14 -> NA, not 0
  expect_true(all(is.na(out[match_id == "m2", xi_diff])))
  out9 <- suppressMessages(add_wp_xi_strength(wf, lu, rt, min_rated = 11L))
  expect_true(all(is.na(out9[match_id == "m1", xi_diff])))   # away has only 10 rated
})

test_that("missing Elo or epv is refused, not filled", {
  skip_if_not_installed("xgboost")
  d <- wps_fx(n_match = 80L)
  m <- suppressMessages(train_wp_structural(d, nrounds = 20L, nfolds = 2L))
  expect_error(predict_wp(m, d[, !"elo_diff"]), "elo_diff")
  expect_error(predict_wp(m, d[, !"epv"]), "epv")
  expect_error(train_wp_structural(d[, !"elo_diff"]), "elo_diff")
})

test_that("add_wp_vars with a structural model: raw match total = result minus opening chance", {
  skip_if_not_installed("xgboost")
  d <- wps_fx(n_match = 80L)
  m <- suppressMessages(train_wp_structural(d, nrounds = 20L, nfolds = 2L))
  one <- d[match_id == "m001"]
  out <- add_wp_vars(data.table::copy(one), m, centre = FALSE)
  # Telescoping: summed in the HOME frame, the changes add to home result - home opening chance.
  home_frame <- out[, data.table::fifelse(is_home == 1L, wpa, -wpa)]
  open_home <- if (out$is_home[1] == 1L) out$wp[1] else 1 - out$wp[1]
  res_home <- if (one$is_home[1] == 1L) one$wp_label[1] else 1 - one$wp_label[1]
  expect_equal(sum(home_frame), res_home - open_home, tolerance = 1e-6)
})
