# Tests for the shared DuckDB query / corrupt-parquet handler
# (.opta_duckdb_query) and the release-file loader body behind
# load_opta_skills(), load_opta_match_stats() and load_opta_psr_weekly().

skip_if_not_installed("duckdb")

# Write a data frame to parquet with DuckDB (no arrow needed).
.write_fixture_parquet <- function(df, path) {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(conn, "fx", df)
  DBI::dbExecute(conn, sprintf("COPY fx TO '%s' (FORMAT PARQUET)",
                               normalizePath(path, winslash = "/", mustWork = FALSE)))
  invisible(path)
}

.corrupt_file <- function() {
  path <- tempfile(fileext = ".parquet")
  writeBin(charToRaw("this is not a parquet file at all"), path)
  path
}

test_that(".opta_duckdb_query removes the cached file and aborts on a corrupt parquet", {
  path <- .corrupt_file()
  key <- "test_corrupt_key"
  assign(key, path, envir = .opta_remote_env)
  on.exit(if (exists(key, envir = .opta_remote_env)) rm(list = key, envir = .opta_remote_env), add = TRUE)

  label <- "XYZ"
  err <- expect_error(
    .with_duckdb(function(conn) {
      .opta_duckdb_query(conn, sprintf("SELECT * FROM '%s'", normalizePath(path, winslash = "/")),
                         c("Parquet file is corrupt for {label}."),
                         cache_key = key, error_class = "vb_error_integrity")
    }),
    "corrupt for XYZ",
    class = "vb_error_integrity"
  )
  expect_false(file.exists(path))
  expect_false(exists(key, envir = .opta_remote_env))
})

test_that(".opta_duckdb_query with no cache_key leaves the file alone", {
  path <- .corrupt_file()
  expect_error(
    .with_duckdb(function(conn) {
      .opta_duckdb_query(conn, sprintf("SELECT * FROM '%s'", normalizePath(path, winslash = "/")),
                         "Parquet file is corrupt.")
    }),
    "Parquet file is corrupt"
  )
  expect_true(file.exists(path))
})

test_that(".opta_duckdb_query reports other DuckDB errors unclassed", {
  err <- expect_error(
    .with_duckdb(function(conn) {
      .opta_duckdb_query(conn, "SELECT no_such_col FROM range(3)", "corrupt")
    }),
    "DuckDB query failed"
  )
  expect_false(inherits(err, "vb_error_integrity"))
  expect_false(inherits(err, "vb_error_absent"))
})

test_that("release-file loaders filter and select through the shared body", {
  skills_path <- file.path(tempdir(), "fx_opta_skills.parquet")
  .write_fixture_parquet(data.frame(player_id = c("a", "b", "c"),
                                    season_end_year = c(2024L, 2025L, 2025L),
                                    goals_p90 = c(0.1, 0.2, 0.3)), skills_path)
  psr_path <- file.path(tempdir(), "fx_opta_psr_weekly.parquet")
  .write_fixture_parquet(data.frame(
    snapshot_date = as.Date(c("2026-01-01", "2026-01-01", "2026-01-08", "2026-01-08", "2026-01-08")),
    player_id = c("a", "b", "a", "b", "c"),
    psr = c(1, 2, 3, 4, 5)), psr_path)

  local_mocked_bindings(download_opta_release_file = function(file_name, ...) {
    if (grepl("psr", file_name)) psr_path else skills_path
  })

  sk <- suppressMessages(load_opta_skills(season = 2025, source = "local"))
  expect_equal(sort(sk$player_id), c("b", "c"))
  sk_all <- suppressMessages(load_opta_skills(columns = c("player_id"), source = "local"))
  expect_equal(names(sk_all), "player_id")
  expect_equal(nrow(sk_all), 3L)

  ms <- suppressMessages(load_opta_match_stats(season = 2024, source = "local"))
  expect_equal(ms$player_id, "a")

  latest <- suppressMessages(load_opta_psr_weekly(source = "local"))
  expect_equal(nrow(latest), 3L)
  expect_true(all(latest$snapshot_date == as.Date("2026-01-08")))
  asof <- suppressMessages(load_opta_psr_weekly(date = "2026-01-05", source = "local"))
  expect_equal(sort(asof$player_id), c("a", "b"))
  expect_true(all(asof$snapshot_date == as.Date("2026-01-01")))
})

test_that("a corrupt remote release file is dropped from the session cache", {
  path <- .corrupt_file()
  key <- paste0("opta_skills.parquet", "_", "peteowen1/pannadata", "_", "opta-latest")
  had_key <- exists(key, envir = .opta_remote_env)
  old <- if (had_key) get(key, envir = .opta_remote_env)
  on.exit({
    if (had_key) assign(key, old, envir = .opta_remote_env)
    else if (exists(key, envir = .opta_remote_env)) rm(list = key, envir = .opta_remote_env)
  }, add = TRUE)
  assign(key, path, envir = .opta_remote_env)
  local_mocked_bindings(download_opta_release_file = function(...) path)

  expect_error(load_opta_skills(), "The corrupt file has been removed")
  expect_false(file.exists(path))
  expect_false(exists(key, envir = .opta_remote_env))
})

test_that("a corrupt local release file is kept (only remote caches are invalidated)", {
  path <- .corrupt_file()
  local_mocked_bindings(download_opta_release_file = function(...) path)
  expect_error(load_opta_match_stats(source = "local"), "Parquet file is corrupt")
  expect_true(file.exists(path))
})
