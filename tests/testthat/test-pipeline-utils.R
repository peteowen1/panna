# Tests for data-raw/pipeline_utils.R
# Covers: run_step, check_critical_step, clear_cache_files

# Source the pipeline utils (not part of the package, lives in data-raw/)
# data-raw/ is excluded from the built tarball, so this file is unavailable during R CMD check
pipeline_utils_path <- file.path(testthat::test_path("..", ".."), "data-raw", "pipeline_utils.R")
if (!file.exists(pipeline_utils_path)) {
  testthat::skip("pipeline_utils.R not available (likely running under R CMD check)")
}
source(pipeline_utils_path)

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

test_that("run_step returns a DISABLED result for disabled steps", {
  run_steps <- list(step_01_load_data = FALSE)
  result <- run_step("load_data", 1, function() "ok", run_steps)

  # run_step always returns a list (never NULL) so orchestrator code doing
  # step_results[[i]] <- run_step(...) doesn't drop list entries.
  expect_equal(result$status, "DISABLED")
})

test_that("run_step returns a DISABLED result for missing step keys", {
  run_steps <- list(step_01_other = TRUE)
  result <- run_step("load_data", 1, function() "ok", run_steps)

  expect_equal(result$status, "DISABLED")
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


# ===========================================================================
# save_cache_with_meta — growth tripwire (panna#128/#133)
# ===========================================================================

test_that("save_cache_with_meta records n_rows and size_bytes in the sidecar", {
  tmp <- tempfile("growth_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path <- file.path(tmp, "cache.rds")

  save_cache_with_meta(data.frame(x = 1:100), path, pipeline = "test")

  meta <- jsonlite::fromJSON(paste0(path, ".meta.json"))
  expect_equal(meta$n_rows, 100)
  expect_true(is.numeric(meta$size_bytes) && meta$size_bytes > 0)
  expect_equal(meta$pipeline, "test")
})

test_that("save_cache_with_meta warns when rows grow past growth_warn_frac", {
  tmp <- tempfile("growth_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path <- file.path(tmp, "cache.rds")

  save_cache_with_meta(data.frame(x = 1:100), path, pipeline = "test")
  # +58% rows — the panna#127 -> #133 incident shape
  expect_warning(
    save_cache_with_meta(data.frame(x = 1:158), path, pipeline = "test"),
    "\\[growth\\].*rows"
  )
  # Sidecar now reflects the new size, so re-saving the SAME data is quiet
  expect_no_warning(
    save_cache_with_meta(data.frame(x = 1:158), path, pipeline = "test")
  )
})

test_that("save_cache_with_meta stays quiet under the threshold and when disabled", {
  tmp <- tempfile("growth_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path <- file.path(tmp, "cache.rds")

  save_cache_with_meta(data.frame(x = 1:100), path, pipeline = "test")
  # +10% < 20% threshold -> no warning
  expect_no_warning(
    save_cache_with_meta(data.frame(x = 1:110), path, pipeline = "test")
  )
  # Huge growth but tripwire disabled -> no warning
  expect_no_warning(
    save_cache_with_meta(data.frame(x = 1:1000), path, pipeline = "test",
                         growth_warn_frac = NULL)
  )
})

test_that("save_cache_with_meta tolerates valid-JSON-but-wrong-shape sidecars", {
  # Review finding (panna#135): tryCatch only guarded PARSE errors. A sidecar
  # that parses to a bare atomic ("123") crashed on prev_meta$n_rows, and an
  # array n_rows crashed inside the || chain — both AFTER the RDS write,
  # failing the very step the advisory tripwire must never block.
  tmp <- tempfile("growth_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path <- file.path(tmp, "cache.rds")

  # Bare atomic sidecar (valid JSON, not an object)
  writeLines("123", paste0(path, ".meta.json"))
  expect_no_error(save_cache_with_meta(data.frame(x = 1:50), path))

  # Array n_rows (valid JSON object, non-scalar field)
  writeLines('{"n_rows": [100, 200], "size_bytes": 10}', paste0(path, ".meta.json"))
  expect_no_error(save_cache_with_meta(data.frame(x = 1:50), path))

  # After surviving both, the sidecar is healthy again
  meta <- jsonlite::fromJSON(paste0(path, ".meta.json"))
  expect_equal(meta$n_rows, 50)
})

test_that("read_meta_sidecar returns NULL for missing/corrupt/non-list sidecars", {
  tmp <- tempfile("meta_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  p <- file.path(tmp, "x.meta.json")

  expect_null(read_meta_sidecar(p))                    # missing
  writeLines("not json{", p)
  expect_null(read_meta_sidecar(p))                    # corrupt
  writeLines("123", p)
  expect_null(read_meta_sidecar(p))                    # bare atomic
  writeLines('{"n_rows": 5}', p)
  expect_equal(read_meta_sidecar(p)$n_rows, 5)         # healthy
})

test_that("save_cache_with_meta tolerates a corrupt or legacy sidecar", {
  tmp <- tempfile("growth_test_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  path <- file.path(tmp, "cache.rds")

  # Corrupt sidecar: growth check silently skipped, save still works
  writeLines("not json{", paste0(path, ".meta.json"))
  expect_no_warning(save_cache_with_meta(data.frame(x = 1:50), path))

  # Legacy sidecar without size_bytes (pre-tripwire format): rows still checked
  meta <- jsonlite::fromJSON(paste0(path, ".meta.json"))
  meta$size_bytes <- NULL
  meta$n_rows <- 10
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE), paste0(path, ".meta.json"))
  expect_warning(
    save_cache_with_meta(data.frame(x = 1:50), path),
    "\\[growth\\].*rows"
  )
})
