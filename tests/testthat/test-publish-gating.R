# Tests for ECOSYSTEM-FIX-PLAN.md PA5 (panna H-TORN) / PA6 (M-RATINGS-PAIR):
# release publishing must be gated so a failure never leaves a torn release.
#
# vb_publish()'s own "no manifest on any upload failure" contract is already
# covered generically by test-versebus.R. These tests cover the two SITES
# that route through it: 13_publish_release_data.R (the single gated
# predictions-latest / blog-latest publish that replaced steps 09/10/10b/
# 10c/10d/12's independent uploads) and 09_export_ratings.R (the xRAPM+SPM
# ratings-data pair). vb_publish() itself is mocked here (rather than
# piggyback/gh) so these tests exercise ORCHESTRATION -- which files get
# batched into which tag's vb_publish() call, and that a failure propagates
# to the pipeline runner instead of being swallowed -- without waiting out
# vb_publish's real retry/backoff delays.

test_that("13_publish_release_data.R calls vb_publish once per non-empty tag with the registered files", {
  dir <- withr::local_tempdir()
  pred_file <- file.path(dir, "predictions.parquet")
  blog1 <- file.path(dir, "panna_ratings.parquet")
  blog2 <- file.path(dir, "match_predictions.parquet")
  writeLines("pred", pred_file)
  writeLines("b1", blog1)
  writeLines("b2", blog2)

  assign("publish_files", list(
    predictions_latest = pred_file,
    blog_latest = c(blog1, blog2)
  ), envir = .GlobalEnv)
  withr::defer(rm(list = "publish_files", envir = .GlobalEnv))

  calls <- list()
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      calls[[tag]] <<- paths
      list(generation = paste0("test-gen-", tag))
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                 "13_publish_release_data.R")
  skip_if_not(file.exists(script), "13_publish_release_data.R not found")
  source(script, local = TRUE)

  expect_setequal(names(calls), c("predictions-latest", "blog-latest"))
  expect_setequal(calls[["predictions-latest"]], pred_file)
  expect_setequal(calls[["blog-latest"]], c(blog1, blog2))
})

test_that("13_publish_release_data.R: a blog-latest failure propagates without being swallowed, after predictions-latest was attempted", {
  dir <- withr::local_tempdir()
  pred_file <- file.path(dir, "predictions.parquet")
  blog1 <- file.path(dir, "panna_ratings.parquet")
  writeLines("pred", pred_file)
  writeLines("b1", blog1)

  assign("publish_files", list(
    predictions_latest = pred_file,
    blog_latest = blog1
  ), envir = .GlobalEnv)
  withr::defer(rm(list = "publish_files", envir = .GlobalEnv))

  attempted_tags <- character(0)
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      attempted_tags <<- c(attempted_tags, tag)
      if (tag == "blog-latest") {
        cli::cli_abort("simulated blog-latest upload failure",
                        class = "vb_error_transient")
      }
      list(generation = "test-gen")
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                 "13_publish_release_data.R")
  skip_if_not(file.exists(script), "13_publish_release_data.R not found")

  # H-TORN: this error must propagate all the way out (uncaught here) so the
  # pipeline runner's run_pred_step() marks step 13 FAILED and the workflow's
  # quit(status = 1) stops the job BEFORE the "Trigger blog data build"
  # workflow step (gated if: success()) can dispatch predictions-complete
  # against a release that never got a fresh blog-latest manifest.
  expect_error(source(script, local = TRUE), class = "vb_error_transient")

  # predictions-latest is independent of blog-latest and was already
  # attempted (and would have succeeded) before blog-latest's failure.
  expect_true("predictions-latest" %in% attempted_tags)
  expect_true("blog-latest" %in% attempted_tags)
})

test_that("09_export_ratings.R publishes xRAPM+SPM in ONE vb_publish call (M-RATINGS-PAIR)", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  cache_dir <- dir
  seasonal_results <- list(
    seasonal_xrapm = data.frame(player_id = "p1", xrapm = 0.1),
    seasonal_spm   = data.frame(player_id = "p1", spm = 0.2)
  )
  saveRDS(seasonal_results, file.path(cache_dir, "07_seasonal_ratings.rds"))

  local_mocked_bindings(
    pb_list = function(repo, tag) data.frame(file_name = character(0)),
    .package = "piggyback"
  )

  captured_paths <- NULL
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      captured_paths <<- paths
      list(generation = "test-gen")
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "player-ratings-opta",
                                 "09_export_ratings.R")
  skip_if_not(file.exists(script), "09_export_ratings.R not found")
  source(script, local = TRUE)

  expect_length(captured_paths, 2)
  expect_true(any(grepl("seasonal_xrapm\\.parquet$", captured_paths)))
  expect_true(any(grepl("seasonal_spm\\.parquet$", captured_paths)))
})

test_that("09_export_ratings.R: a vb_publish failure aborts the script (both-or-neither)", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  cache_dir <- dir
  seasonal_results <- list(
    seasonal_xrapm = data.frame(player_id = "p1", xrapm = 0.1),
    seasonal_spm   = data.frame(player_id = "p1", spm = 0.2)
  )
  saveRDS(seasonal_results, file.path(cache_dir, "07_seasonal_ratings.rds"))

  local_mocked_bindings(
    pb_list = function(repo, tag) data.frame(file_name = character(0)),
    .package = "piggyback"
  )
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      cli::cli_abort("simulated seasonal_spm upload failure",
                      class = "vb_error_transient")
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "player-ratings-opta",
                                 "09_export_ratings.R")
  skip_if_not(file.exists(script), "09_export_ratings.R not found")

  expect_error(source(script, local = TRUE), class = "vb_error_transient")
})
