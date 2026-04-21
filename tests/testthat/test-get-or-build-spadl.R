# Tests for get_or_build_spadl() — the schema-aware SPADL disk cache.

test_that("get_or_build_spadl rebuilds when cached SPADL is missing required columns", {

  tmp_cache <- withr::local_tempdir()

  # Plant a stale cache without `original_event_id` (schema from before that
  # column was added). get_or_build_spadl should detect the missing column
  # and rebuild.
  stale_spadl <- data.frame(
    match_id       = c("m1", "m1", "m2"),
    action_id      = 1:3,
    # original_event_id intentionally absent
    player_id      = c("p1", "p2", "p1"),
    action_type    = c("pass", "shot", "pass"),
    start_x        = c(50, 80, 60),
    start_y        = c(50, 40, 50),
    result         = c("success", "success", "fail"),
    stringsAsFactors = FALSE
  )
  cache_path <- file.path(tmp_cache, "spadl_TEST_2024-2025.rds")
  saveRDS(stale_spadl, cache_path)

  # Track whether convert_opta_to_spadl gets called (rebuild path) or not (hit).
  rebuild_count <- 0L
  mock_fresh <- data.frame(
    match_id          = c("m1", "m1", "m2"),
    action_id         = 1:3,
    original_event_id = c("e1", "e2", "e3"),
    player_id         = c("p1", "p2", "p1"),
    action_type       = c("pass", "shot", "pass"),
    start_x           = c(50, 80, 60),
    start_y           = c(50, 40, 50),
    result            = c("success", "success", "fail"),
    stringsAsFactors  = FALSE
  )
  testthat::local_mocked_bindings(
    convert_opta_to_spadl = function(events) {
      rebuild_count <<- rebuild_count + 1L
      mock_fresh
    }
  )

  events <- data.frame(match_id = c("m1", "m2"))
  result <- get_or_build_spadl(events, "TEST", "2024-2025",
                                cache_dir = tmp_cache)

  expect_equal(rebuild_count, 1L)
  expect_true("original_event_id" %in% names(result))
  expect_equal(nrow(result), 3L)
})

test_that("get_or_build_spadl rebuilds when cache doesn't cover all requested match_ids", {

  tmp_cache <- withr::local_tempdir()

  # Cache has only m1 — but we'll request events for m1 AND m2.
  partial_spadl <- data.frame(
    match_id          = "m1",
    action_id         = 1L,
    original_event_id = "e1",
    player_id         = "p1",
    action_type       = "pass",
    start_x           = 50,
    start_y           = 50,
    result            = "success",
    stringsAsFactors  = FALSE
  )
  cache_path <- file.path(tmp_cache, "spadl_TEST_2024-2025.rds")
  saveRDS(partial_spadl, cache_path)

  rebuild_count <- 0L
  mock_fresh <- rbind(partial_spadl,
                      data.frame(match_id = "m2", action_id = 2L,
                                 original_event_id = "e2", player_id = "p2",
                                 action_type = "shot", start_x = 80, start_y = 40,
                                 result = "success"))
  testthat::local_mocked_bindings(
    convert_opta_to_spadl = function(events) {
      rebuild_count <<- rebuild_count + 1L
      mock_fresh
    }
  )

  events <- data.frame(match_id = c("m1", "m2"))
  result <- get_or_build_spadl(events, "TEST", "2024-2025",
                                cache_dir = tmp_cache)

  expect_equal(rebuild_count, 1L)
  expect_setequal(unique(result$match_id), c("m1", "m2"))
})

test_that("get_or_build_spadl returns cache when schema + coverage are fine", {

  tmp_cache <- withr::local_tempdir()

  fresh_spadl <- data.frame(
    match_id          = c("m1", "m1", "m2"),
    action_id         = 1:3,
    original_event_id = c("e1", "e2", "e3"),
    player_id         = c("p1", "p2", "p1"),
    action_type       = c("pass", "shot", "pass"),
    start_x           = c(50, 80, 60),
    start_y           = c(50, 40, 50),
    result            = c("success", "success", "fail"),
    stringsAsFactors  = FALSE
  )
  cache_path <- file.path(tmp_cache, "spadl_TEST_2024-2025.rds")
  saveRDS(fresh_spadl, cache_path)

  rebuild_count <- 0L
  testthat::local_mocked_bindings(
    convert_opta_to_spadl = function(events) {
      rebuild_count <<- rebuild_count + 1L
      fresh_spadl
    }
  )

  events <- data.frame(match_id = c("m1", "m2"))
  result <- get_or_build_spadl(events, "TEST", "2024-2025",
                                cache_dir = tmp_cache)

  # No rebuild — cache hit.
  expect_equal(rebuild_count, 0L)
  expect_equal(nrow(result), 3L)
})

test_that("get_or_build_spadl honours force_rebuild even when cache is valid", {

  tmp_cache <- withr::local_tempdir()

  fresh_spadl <- data.frame(
    match_id          = "m1",
    action_id         = 1L,
    original_event_id = "e1",
    player_id         = "p1",
    action_type       = "pass",
    start_x           = 50, start_y = 50,
    result            = "success",
    stringsAsFactors  = FALSE
  )
  saveRDS(fresh_spadl, file.path(tmp_cache, "spadl_TEST_2024-2025.rds"))

  rebuild_count <- 0L
  testthat::local_mocked_bindings(
    convert_opta_to_spadl = function(events) {
      rebuild_count <<- rebuild_count + 1L
      fresh_spadl
    }
  )

  events <- data.frame(match_id = "m1")
  get_or_build_spadl(events, "TEST", "2024-2025",
                      cache_dir = tmp_cache, force_rebuild = TRUE)

  expect_equal(rebuild_count, 1L)
})
