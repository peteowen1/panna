# Tests for panna H-GATE (ECOSYSTEM-FIX-PLAN.md PA4): check_events_coverage()'s
# is_missing_source_err() classifier must anchor to known local-miss messages
# (or the typed vb_error_absent class) only -- a DuckDB binder/IO error must
# propagate as a real load failure, never collapse to an empty data.frame that
# gets reclassified "source_missing" and silently skipped.

test_that("a DuckDB binder-style error is NOT classified missing -- it propagates", {
  local_mocked_bindings(
    load_opta_fixtures = function(...) data.frame(match_id = character(0)),
    load_opta_match_events = function(...) data.frame(match_id = character(0)),
    load_opta_eventless_ids = function(...) character(0),
    .package = "panna"
  )
  local_mocked_bindings(
    load_opta_stats = function(...) {
      stop('DuckDB query failed: Binder Error: column "foo" does not exist')
    },
    .package = "panna"
  )

  expect_error(
    check_events_coverage("EPL", "2025-2026", source = "local"),
    'column "foo" does not exist'
  )
})

test_that("a corrupt-parquet integrity error is NOT classified missing -- it propagates", {
  local_mocked_bindings(
    load_opta_fixtures = function(...) data.frame(match_id = character(0)),
    load_opta_match_events = function(...) data.frame(match_id = character(0)),
    load_opta_eventless_ids = function(...) character(0),
    .package = "panna"
  )
  local_mocked_bindings(
    load_opta_stats = function(...) {
      cli::cli_abort("Parquet file is corrupt for EPL player_stats.",
                      class = "vb_error_integrity")
    },
    .package = "panna"
  )

  expect_error(
    check_events_coverage("EPL", "2025-2026", source = "local"),
    class = "vb_error_integrity"
  )
})

test_that("'No data found for' is classified missing -- check_events_coverage proceeds with 0 rows", {
  local_mocked_bindings(
    load_opta_fixtures = function(...) data.frame(match_id = character(0)),
    load_opta_stats = function(...) {
      cli::cli_abort("No data found for EPL season 2025-2026.",
                      class = "vb_error_absent")
    },
    load_opta_match_events = function(...) data.frame(match_id = character(0)),
    load_opta_eventless_ids = function(...) character(0),
    .package = "panna"
  )

  result <- check_events_coverage("EPL", "2025-2026", source = "local")
  expect_equal(result$n_player_stats, 0L)
  expect_equal(result$gap, 0L)
})

test_that("an untyped 'No data found for' message (legacy caller) is also classified missing", {
  local_mocked_bindings(
    load_opta_fixtures = function(...) data.frame(match_id = character(0)),
    load_opta_stats = function(...) stop("No data found for EPL season 2025-2026."),
    load_opta_match_events = function(...) data.frame(match_id = character(0)),
    load_opta_eventless_ids = function(...) character(0),
    .package = "panna"
  )

  result <- check_events_coverage("EPL", "2025-2026", source = "local")
  expect_equal(result$n_player_stats, 0L)
})
