toy_match_stats <- function() {
  set.seed(1)
  n <- 400L
  pid <- sprintf("p%02d", sample(1:40, n, replace = TRUE))
  season <- sample(c("2022-2023", "2023-2024", "2024-2025", "2025-2026", "2026"), n, replace = TRUE)
  pos_by_player <- setNames(sample(c("Goalkeeper", "Defender", "Midfielder", "Forward"), 40, replace = TRUE),
                            sprintf("p%02d", 1:40))
  data.table::data.table(
    player_id = pid, player_name = paste("Player", pid),
    match_id = sprintf("m%03d", seq_len(n)), team_id = sample(c("t1", "t2"), n, replace = TRUE),
    league = "ENG", season = season,
    match_date = as.Date("2023-01-01") + seq_len(n),
    position = unname(pos_by_player[pid]), position_side = sample(c("Left", "Right", "Centre"), n, replace = TRUE),
    total_minutes = sample(10:90, n, replace = TRUE),
    passes_p90 = runif(n, 10, 60), tackles_p90 = runif(n, 0, 5),
    unused_wide_col = runif(n)
  )
}

test_that("narrow side file keeps the step-02 and resolver columns and drops the rest", {
  ms <- toy_match_stats()
  cols <- .predictions_match_stats_cols(ms)
  expect_true(all(c("player_id", "position", "position_side", "total_minutes", "league",
                    "season", "match_id", "team_id", "passes_p90", "tackles_p90") %in% cols))
  expect_false("unused_wide_col" %in% cols)
  # Narrowing the narrow table again gives what narrowing the full one gives.
  narrow <- ms[, cols, with = FALSE]
  expect_identical(names(.narrow_match_stats_for_skills(narrow)),
                   names(.narrow_match_stats_for_skills(ms)))
})

test_that("side files: recent rows, row numbers, and 10b's resolution matches the full table", {
  ms <- toy_match_stats()
  d <- withr::local_tempdir()
  path <- file.path(d, "01_match_stats.rds")
  saveRDS(ms, path)
  Sys.sleep(1.1)  # mtime resolution: the side files must be newer than the table
  min_sey <- suppressMessages(.write_predictions_match_stats_sidecars(ms, path))
  expect_equal(min_sey, 2024L)  # latest end year 2026 minus 2

  p_n <- .predictions_match_stats_sidecar(path, "narrow")
  p_r <- .predictions_match_stats_sidecar(path, "recent")
  expect_false(is.null(p_n)); expect_false(is.null(p_r))
  narrow <- readRDS(p_n); recent <- readRDS(p_r)
  expect_identical(narrow$.ms_row, seq_len(nrow(ms)))
  expect_identical(attr(narrow, "recent_min_season_end_year"), 2024L)

  sey <- as.integer(vapply(ms$season, extract_season_end_year, numeric(1)))
  expect_identical(recent$.ms_row, which(sey >= 2024L))
  # The recent rows are the full table's rows, every column.
  expect_equal(as.data.frame(recent[, !".ms_row"]), as.data.frame(ms[recent$.ms_row]),
               ignore_attr = TRUE)

  # 10b: resolve on the full table vs on the narrow file mapped by .ms_row.
  ms_full <- data.table::copy(ms)[, season_end_year := sey]
  rc <- c("player_id", "position", "total_minutes", "season_end_year")
  gk_full <- .detect_gk_rows(ms_full[, rc, with = FALSE])
  pg_full <- .psv_pos_grp(ms_full[, rc, with = FALSE], gk_full)
  narrow[, season_end_year := sey]
  gk_n <- .detect_gk_rows(narrow[, rc, with = FALSE])
  pg_n <- .psv_pos_grp(narrow[, rc, with = FALSE], gk_n)
  expect_identical(gk_n[recent$.ms_row], gk_full[recent$.ms_row])
  expect_identical(pg_n[recent$.ms_row], pg_full[recent$.ms_row])
})

test_that("a side file older than the full table is ignored", {
  ms <- toy_match_stats()
  d <- withr::local_tempdir()
  path <- file.path(d, "01_match_stats.rds")
  suppressMessages(.write_predictions_match_stats_sidecars(ms, path))
  Sys.sleep(1.1)
  saveRDS(ms, path)  # table rebuilt after the side files
  expect_null(suppressMessages(.predictions_match_stats_sidecar(path, "narrow")))
  expect_null(suppressMessages(.predictions_match_stats_sidecar(path, "recent")))
  expect_null(.predictions_match_stats_sidecar(file.path(d, "missing.rds"), "narrow"))
})
