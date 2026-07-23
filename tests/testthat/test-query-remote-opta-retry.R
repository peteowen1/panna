# panna#157: query_remote_opta_parquet() retries a bounded number of times on
# a corrupt/incomplete read (the daily epv/predictions concurrent-write race
# on opta_xmetrics_bymatch.parquet) instead of aborting immediately.

test_that("retries once on a corrupt-file error, then succeeds", {
  call_count <- 0L
  local_mocked_bindings(
    .query_remote_opta_parquet_once = function(...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) {
        cli::cli_abort("Cached parquet file is corrupt (no magic bytes).")
      }
      data.frame(x = 1)
    }
  )
  result <- query_remote_opta_parquet("player_stats", "ENG",
                                       retry_backoff_sec = 0)
  expect_equal(call_count, 2L)
  expect_equal(result, data.frame(x = 1))
})

test_that("gives up after max_retries and rethrows the corruption error", {
  call_count <- 0L
  local_mocked_bindings(
    .query_remote_opta_parquet_once = function(...) {
      call_count <<- call_count + 1L
      cli::cli_abort("Downloaded opta_shots.parquet is corrupt (incomplete download).")
    }
  )
  expect_error(
    query_remote_opta_parquet("shots", "ENG", max_retries = 2L,
                               retry_backoff_sec = 0),
    "corrupt"
  )
  expect_equal(call_count, 3L)  # first attempt + 2 retries
})

test_that("does NOT retry on a non-corruption error", {
  call_count <- 0L
  local_mocked_bindings(
    .query_remote_opta_parquet_once = function(...) {
      call_count <<- call_count + 1L
      cli::cli_abort("Package 'piggyback' is required for remote Opta loading.")
    }
  )
  expect_error(
    query_remote_opta_parquet("fixtures", "ENG", retry_backoff_sec = 0),
    "piggyback"
  )
  expect_equal(call_count, 1L)
})

test_that("match_events dispatches to query_remote_opta_match_events without retry wrapping", {
  local_mocked_bindings(
    query_remote_opta_match_events = function(...) "match_events_result"
  )
  expect_equal(
    query_remote_opta_parquet("match_events", "ENG"),
    "match_events_result"
  )
})
