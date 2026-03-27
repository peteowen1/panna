# Tests for data-raw/pipeline_utils.R
# Covers: run_step, check_critical_step, clear_cache_files

# Source the pipeline utils (not part of the package, lives in data-raw/)
source(file.path(testthat::test_path("..", ".."), "data-raw", "pipeline_utils.R"))

# ===========================================================================
# run_step — step key generation
# ===========================================================================

test_that("run_step generates correct key for numeric steps", {
  run_steps <- list(step_01_load_data = TRUE)
  result <- run_step("load_data", 1, function() "ok", run_steps)

  expect_equal(result$status, "SUCCESS")
  expect_equal(result$name, "load_data")
  expect_equal(result$step, 1)
})

test_that("run_step generates correct key for lettered steps", {
  run_steps <- list(step_02b_optimize = TRUE)
  result <- run_step("optimize", "2b", function() "ok", run_steps)

  expect_equal(result$status, "SUCCESS")
  expect_equal(result$step, "2b")
})

test_that("run_step zero-pads single-digit lettered steps", {
  # "8b" should look up "step_08b_export" not "step_8b_export"
  run_steps <- list(step_08b_export = TRUE)
  result <- run_step("export", "8b", function() "ok", run_steps)

  expect_equal(result$status, "SUCCESS")
})

test_that("run_step returns NULL for disabled steps", {
  run_steps <- list(step_01_load_data = FALSE)
  result <- run_step("load_data", 1, function() "ok", run_steps)

  expect_null(result)
})

test_that("run_step returns NULL for missing step keys", {
  run_steps <- list(step_01_other = TRUE)
  result <- run_step("load_data", 1, function() "ok", run_steps)

  expect_null(result)
})

test_that("run_step returns FAILED on error", {
  run_steps <- list(step_01_fail = TRUE)
  result <- run_step("fail", 1, function() stop("test error"), run_steps)

  expect_equal(result$status, "FAILED")
  expect_true(result$duration_secs >= 0)
})

test_that("run_step returns SKIPPED when pipeline_failed is TRUE", {
  run_steps <- list(step_01_skip = TRUE)
  result <- run_step("skip", 1, function() "ok", run_steps, pipeline_failed = TRUE)

  expect_equal(result$status, "SKIPPED")
  expect_equal(result$duration_secs, 0)
})

test_that("run_step records timing", {
  run_steps <- list(step_01_timed = TRUE)
  result <- run_step("timed", 1, function() Sys.sleep(0.1), run_steps)

  expect_equal(result$status, "SUCCESS")
  expect_true(result$duration_secs >= 0.05)
  expect_true(nchar(result$duration_formatted) > 0)
})


# ===========================================================================
# check_critical_step — 1-arg form
# ===========================================================================

test_that("check_critical_step returns FALSE for NULL", {
  expect_false(check_critical_step(NULL))
})

test_that("check_critical_step returns FALSE for SUCCESS result", {
  result <- list(step = 1, name = "test", status = "SUCCESS")
  expect_false(check_critical_step(result))
})

test_that("check_critical_step returns TRUE for FAILED result", {
  result <- list(step = 1, name = "test", status = "FAILED")
  expect_true(check_critical_step(result))
})

test_that("check_critical_step returns FALSE for SKIPPED result", {
  result <- list(step = 1, name = "test", status = "SKIPPED")
  expect_false(check_critical_step(result))
})

# ===========================================================================
# check_critical_step — 3-arg (legacy) form
# ===========================================================================

test_that("check_critical_step legacy form detects failure", {
  step_results <- list(
    list(step = 1, name = "s1", status = "SUCCESS"),
    list(step = 2, name = "s2", status = "FAILED")
  )
  expect_false(check_critical_step(1, "s1", step_results))
  expect_true(check_critical_step(2, "s2", step_results))
})

test_that("check_critical_step legacy form handles out-of-range step", {
  step_results <- list(list(step = 1, name = "s1", status = "SUCCESS"))
  expect_false(check_critical_step(5, "s5", step_results))
})


# ===========================================================================
# clear_cache_files
# ===========================================================================

test_that("clear_cache_files does nothing for NULL", {
  expect_invisible(clear_cache_files(NULL, tempdir(), list("1" = "a.rds"), 3))
})

test_that("clear_cache_files warns on invalid step", {
  expect_warning(
    clear_cache_files("abc", tempdir(), list("1" = "a.rds"), 3),
    "Invalid"
  )
})

test_that("clear_cache_files warns on out-of-range step", {
  expect_warning(
    clear_cache_files(99, tempdir(), list("1" = "a.rds"), 3),
    "Invalid"
  )
})

test_that("clear_cache_files deletes correct files from step onwards", {
  tmp <- tempfile("cache_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Create test files
  writeLines("a", file.path(tmp, "step1.rds"))
  writeLines("b", file.path(tmp, "step2.rds"))
  writeLines("c", file.path(tmp, "step3.rds"))

  cache_map <- list(
    "1" = "step1.rds",
    "2" = "step2.rds",
    "3" = "step3.rds"
  )

  clear_cache_files(2, tmp, cache_map, max_step = 3)

  # Step 1 should survive, steps 2-3 deleted
 expect_true(file.exists(file.path(tmp, "step1.rds")))
  expect_false(file.exists(file.path(tmp, "step2.rds")))
  expect_false(file.exists(file.path(tmp, "step3.rds")))
})

test_that("clear_cache_files handles lettered steps", {
  tmp <- tempfile("cache_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  writeLines("a", file.path(tmp, "step2.rds"))
  writeLines("b", file.path(tmp, "step2b.rds"))
  writeLines("c", file.path(tmp, "step3.rds"))

  cache_map <- list(
    "1" = "step1.rds",
    "2" = "step2.rds",
    "2b" = "step2b.rds",
    "3" = "step3.rds"
  )

  clear_cache_files(2, tmp, cache_map, max_step = 3)

  # Steps 2, 2b, 3 should all be deleted
  expect_false(file.exists(file.path(tmp, "step2.rds")))
  expect_false(file.exists(file.path(tmp, "step2b.rds")))
  expect_false(file.exists(file.path(tmp, "step3.rds")))
})
