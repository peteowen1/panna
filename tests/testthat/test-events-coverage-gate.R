# Review R006 (2026-10-11): the events-coverage gate was vacuous on the GHA
# publishing path. A runner downloads only the consolidated Opta files, so the
# LOCAL events file is absent, every league was classed "source_missing"
# ("lazy-loaded downstream") and the abort could never fire, while step 10b
# then loaded the REMOTE events. These tests pin the repaired behaviour.

mock_coverage <- function(n_stats, n_events_local, n_events_remote) {
  # Universe (player_stats) of n_stats matches; events present for the first
  # n_events_* of them, depending on which source is asked for.
  ids <- sprintf("m%03d", seq_len(n_stats))
  local_mocked_bindings(
    load_opta_fixtures = function(...) data.frame(match_id = ids),
    load_opta_stats = function(...) data.frame(match_id = ids),
    load_opta_eventless_ids = function(...) character(0),
    load_opta_match_events = function(league, season = NULL, columns = NULL,
                                      source = "remote") {
      n <- if (identical(source, "local")) n_events_local else n_events_remote
      data.frame(match_id = ids[seq_len(n)])
    },
    .package = "panna",
    .env = parent.frame()
  )
}

test_that("source_missing applies only to local; remote 0 events is a hard gap", {
  mock_coverage(n_stats = 100, n_events_local = 0, n_events_remote = 0)

  loc <- assert_events_coverage("ENG", season = "2025-2026", source = "local")
  expect_equal(loc$reports[[1]]$status, "source_missing")

  expect_error(
    assert_events_coverage("ENG", season = "2025-2026",
                           abort_threshold = 20L, source = "remote"),
    "Refusing to proceed"
  )
})

test_that("publish guard aborts when local events are absent and remote is short", {
  # The GHA case: nothing local, cloud file truncated (60 of 100 matches).
  mock_coverage(n_stats = 100, n_events_local = 0, n_events_remote = 60)
  expect_error(
    .assert_events_coverage_publish(list(list(league = "ENG", season = "2025-2026")),
                                    abort_threshold = 20L),
    "Refusing to proceed"
  )
})

test_that("publish guard passes when local is absent but remote is complete", {
  mock_coverage(n_stats = 100, n_events_local = 0, n_events_remote = 100)
  res <- .assert_events_coverage_publish(
    list(list(league = "ENG", season = "2025-2026")), abort_threshold = 20L)
  expect_equal(res$local$n_source_missing, 1L)
  expect_equal(res$remote$reports[[1]]$status, "ok")
})

test_that("publish guard does not re-check remote when local events are present", {
  mock_coverage(n_stats = 100, n_events_local = 100, n_events_remote = 0)
  res <- .assert_events_coverage_publish(
    list(list(league = "ENG", season = "2025-2026")), abort_threshold = 20L)
  expect_null(res$remote)
})
