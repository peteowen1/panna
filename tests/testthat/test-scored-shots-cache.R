# Tests for the shared event -> scored-shots path (issue #505): one scoring
# function for the EPV and RAPM jobs, and a cache whose entries are valid only
# while the events, shot lookup and xG model they were built from are unchanged.

.toy_events <- function() {
  data.frame(
    match_id = c("m1", "m1", "m1", "m2"),
    event_id = c(1, 2, 3, 1),
    type_id = c(1L, 16L, 15L, 13L),
    team_id = c("t1", "t1", "t2", "t2"),
    player_id = c("p1", "p1", "p2", "p3"),
    player_name = c("A", "A", "B", "C"),
    minute = c(1L, 10L, 20L, 5L),
    second = c(0L, 5L, 7L, 9L),
    x = c(50, 90, 88, 85), y = c(50, 50, 40, 45),
    end_x = c(60, NA, NA, NA), end_y = c(50, NA, NA, NA),
    outcome = c(1L, 1L, 0L, 0L), period_id = c(1L, 1L, 1L, 1L),
    qualifier_json = c("{}", "{\"9\":null}", "{\"56\":\"Center\"}", "{}"),
    stringsAsFactors = FALSE
  )
}

.toy_xg_model <- function(seed_shift = 0) {
  skip_if_not_installed("xgboost")
  set.seed(1)
  X <- matrix(stats::rnorm(200), ncol = 2)
  y <- as.integer(X[, 1] + seed_shift * X[, 2] > 0)
  m <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", nthread = 1),
    data = xgboost::xgb.DMatrix(X, label = y), nrounds = 3, verbose = 0)
  list(model = m, panna_metadata = list(feature_cols = c("a", "b")))
}

test_that("events fingerprint ignores row order and container but sees any change", {
  ev <- .toy_events()
  fp <- opta_events_fingerprint(ev)
  expect_identical(fp, opta_events_fingerprint(ev[rev(seq_len(nrow(ev))), ]))
  expect_identical(fp, opta_events_fingerprint(data.table::as.data.table(ev)))
  ev_dbl <- ev; ev_dbl$minute <- as.numeric(ev_dbl$minute)
  expect_identical(fp, opta_events_fingerprint(ev_dbl))

  moved <- ev; moved$x[2] <- 91
  expect_false(identical(fp, opta_events_fingerprint(moved)))
  tagged <- ev; tagged$qualifier_json[3] <- "{\"9\":null}"
  expect_false(identical(fp, opta_events_fingerprint(tagged)))
  expect_false(identical(fp, opta_events_fingerprint(ev[-4, ])))
  extra <- rbind(ev, transform(ev[1, ], event_id = 99))
  expect_false(identical(fp, opta_events_fingerprint(extra)))
})

test_that("xG model fingerprint changes when the weights change", {
  a <- .toy_xg_model(0)
  b <- .toy_xg_model(2)
  expect_identical(xg_model_fingerprint(a), xg_model_fingerprint(a))
  expect_false(identical(xg_model_fingerprint(a), xg_model_fingerprint(b)))
  a2 <- a; a2$panna_metadata$feature_cols <- c("a", "c")
  expect_false(identical(xg_model_fingerprint(a), xg_model_fingerprint(a2)))
})

test_that("cache key tracks events, shot lookup, xG model and scoring version", {
  ev <- .toy_events()
  mdl <- .toy_xg_model(0)
  lk <- data.frame(match_id = "m1", event_id = 2, body_part = "Head",
                   situation = "OpenPlay", stringsAsFactors = FALSE)
  key <- scored_shots_key(ev, mdl, lk)
  expect_identical(key, scored_shots_key(ev, mdl, lk))

  ev2 <- ev; ev2$outcome[3] <- 1L
  expect_false(identical(key, scored_shots_key(ev2, mdl, lk)))
  expect_false(identical(key, scored_shots_key(ev, .toy_xg_model(2), lk)))
  lk2 <- lk; lk2$body_part <- "RightFoot"
  expect_false(identical(key, scored_shots_key(ev, mdl, lk2)))
  expect_false(identical(key, scored_shots_key(ev, mdl, NULL)))

  testthat::local_mocked_bindings(SCORED_SHOTS_VERSION = 2L)
  expect_false(identical(key, scored_shots_key(ev, mdl, lk)))
})

test_that("score_spadl_shots flags penalties from SPADL and fixes their xG", {
  spadl <- data.frame(
    match_id = "m1", action_type = c("pass", "shot", "shot"),
    player_id = "p1", time_seconds = c(1, 600, 900), is_penalty = c(0L, 1L, 0L),
    stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    add_xg_to_spadl = function(spadl_actions, ...) {
      spadl_actions$xg <- ifelse(spadl_actions$action_type == "shot", 0.1, 0)
      spadl_actions
    })
  out <- score_spadl_shots(spadl, events = .toy_events(), xg_model = list(),
                           season = "2024-2025")
  expect_equal(out$xg, c(0, PENALTY_XG, 0.1))
  expect_type(out$is_penalty, "integer")
})

test_that("score_spadl_shots derives is_penalty from events on a stale SPADL", {
  spadl <- data.frame(
    match_id = "m1", action_type = c("shot", "shot"), player_id = "p1",
    time_seconds = c(10 * 60 + 5, 30 * 60), stringsAsFactors = FALSE)
  testthat::local_mocked_bindings(
    add_xg_to_spadl = function(spadl_actions, ...) { spadl_actions$xg <- 0.1; spadl_actions })
  out <- suppressMessages(score_spadl_shots(spadl, .toy_events(), list(), "2024-2025"))
  expect_equal(out$is_penalty, c(1L, 0L))
  # A rebound by the penalty taker in the same minute (10:30, penalty at 10:05) is not a penalty.
  reb <- spadl; reb$time_seconds <- c(10 * 60 + 5, 10 * 60 + 30)
  out2 <- suppressMessages(score_spadl_shots(reb, .toy_events(), list(), "2024-2025"))
  expect_equal(out2$is_penalty, c(1L, 0L))
})

test_that("a cached league-season is reused only under an unchanged key", {
  ev <- .toy_events()
  mdl <- .toy_xg_model(0)
  key <- scored_shots_key(ev, mdl, NULL)
  spadl <- data.frame(
    match_id = c("m1", "m1"), original_event_id = c(2, 3), period_id = 1L,
    time_seconds = c(600, 1200), team_id = c("t1", "t2"), player_id = c("p1", "p2"),
    player_name = c("A", "B"), action_type = c("shot", "shot"),
    result = c("success", "fail"), xg = c(0.76, 0.1), is_penalty = c(1L, 0L),
    stringsAsFactors = FALSE)
  rows_a <- scored_shots_rows(spadl, "ENG", "2024-2025", key)
  rows_b <- scored_shots_rows(spadl, "ESP", "2024-2025", "some-other-key")

  path <- file.path(withr::local_tempdir(), "opta_scored_shots.parquet")
  write_scored_shots(list(a = rows_a, b = rows_b), path)
  tbl <- read_scored_shots(path)

  hit <- lookup_scored_shots(tbl, "ENG", "2024-2025", key)
  expect_equal(nrow(hit), 2)
  expect_equal(hit$xg, c(0.76, 0.1))
  expect_false(any(c("league", "season", "cache_key") %in% names(hit)))
  # A hit feeds extract_shots_from_spadl() unchanged.
  shots <- suppressMessages(extract_shots_from_spadl(hit, NULL))
  expect_equal(shots$is_penalty, c(TRUE, FALSE))

  # Changed event file, changed model, other league-season, other season: miss.
  ev2 <- ev; ev2$x[2] <- 70
  expect_null(lookup_scored_shots(tbl, "ENG", "2024-2025", scored_shots_key(ev2, mdl, NULL)))
  expect_null(lookup_scored_shots(tbl, "ENG", "2024-2025", scored_shots_key(ev, .toy_xg_model(2), NULL)))
  expect_null(lookup_scored_shots(tbl, "ESP", "2024-2025", key))
  expect_null(lookup_scored_shots(tbl, "ENG", "2023-2024", key))
})

test_that("a missing or unreadable cache file is a miss, not an error", {
  expect_null(read_scored_shots(NULL))
  expect_null(read_scored_shots(""))
  expect_null(read_scored_shots(file.path(tempdir(), "nope.parquet")))
  bad <- withr::local_tempfile(fileext = ".parquet")
  writeLines("not parquet", bad)
  expect_null(suppressMessages(read_scored_shots(bad)))
  expect_null(lookup_scored_shots(NULL, "ENG", "2024-2025", "k"))
})
