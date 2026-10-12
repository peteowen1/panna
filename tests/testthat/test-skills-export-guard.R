make_skills_tbl <- function(n_per_season = 10, seasons = 2020:2022) {
  d <- expand.grid(i = seq_len(n_per_season), season_end_year = seasons)
  d$player_id <- paste0("p", seq_len(nrow(d)))
  d$goals_p90 <- seq_len(nrow(d)) / 10
  d$shots_p90 <- seq_len(nrow(d)) / 5
  d$total_minutes <- 900
  d$i <- NULL
  d
}

test_that("a healthy table passes with and without a previous table", {
  new <- make_skills_tbl()
  expect_no_error(suppressMessages(assert_skills_export_ok(new, new)))
  res <- suppressMessages(assert_skills_export_ok(new, NULL))
  expect_false(res$prev_checked)
  expect_equal(res$skill_cols, c("goals_p90", "shots_p90"))
})

test_that("a missing previous table is reported, not silently ignored", {
  expect_message(assert_skills_export_ok(make_skills_tbl(), NULL), "SKIPPED")
})

test_that("a 100% NA skill column aborts", {
  new <- make_skills_tbl()
  new$shots_p90 <- NA_real_
  expect_error(suppressMessages(assert_skills_export_ok(new, NULL)),
               "100% NA: shots_p90")
  # one NA is fine
  new$shots_p90 <- c(NA, rep(1, nrow(new) - 1))
  expect_no_error(suppressMessages(assert_skills_export_ok(new, NULL)))
})

test_that("a table with no skill columns aborts (wrong table)", {
  new <- make_skills_tbl()[, c("player_id", "season_end_year", "total_minutes")]
  expect_error(suppressMessages(assert_skills_export_ok(new, NULL)), "no columns match")
})

test_that("row count below 90% of the published table aborts; exactly 90% passes", {
  prev <- make_skills_tbl(10)             # 30 rows
  expect_error(suppressMessages(assert_skills_export_ok(prev[seq_len(26), ], prev)),
               "below 90%")
  expect_no_error(suppressMessages(assert_skills_export_ok(prev[seq_len(27), ], prev)))
})

test_that("dropping a published season aborts; adding one is fine", {
  prev <- make_skills_tbl(10, 2020:2022)
  expect_error(suppressMessages(assert_skills_export_ok(prev[prev$season_end_year != 2020, ] |>
                                                          rbind(prev[11:13, ]), prev,
                                                        min_row_frac = 0.1)),
               "season_end_year.*2020")
  grown <- rbind(prev, make_skills_tbl(10, 2023))
  expect_no_error(suppressMessages(assert_skills_export_ok(grown, prev)))
})

test_that("league coverage is compared only when both tables carry it", {
  prev <- make_skills_tbl(10)
  prev$league <- rep(c("ENG", "ESP"), length.out = nrow(prev))
  new <- prev
  new$league <- "ENG"
  expect_error(suppressMessages(assert_skills_export_ok(new, prev)), "league.*ESP")
  # previous has no league column: nothing to compare
  prev$league <- NULL
  expect_no_error(suppressMessages(assert_skills_export_ok(new, prev)))
})

test_that("all problems are reported together", {
  prev <- make_skills_tbl(10)
  new <- prev[prev$season_end_year == 2022, ][1:5, ]
  new$goals_p90 <- NA_real_
  err <- tryCatch(suppressMessages(assert_skills_export_ok(new, prev)),
                  error = function(e) conditionMessage(e))
  expect_match(err, "100% NA")
  expect_match(err, "below 90%")
  expect_match(err, "season_end_year")
})
