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
#
# Every pipeline script sourced below opens with `devtools::load_all()` (steps
# 01-13 all carry that header, so a step can be run standalone). Under
# devtools::test() that call RELOADS THE PACKAGE NAMESPACE MID-SUITE, which:
#   1. discards the local_mocked_bindings() installed by the very test doing
#      the sourcing -- so `vb_publish` un-mocks itself partway through and the
#      test errors; and
#   2. replaces the package's session-state environments (.opta_env,
#      .opta_remote_env, .vb_state, .get_col_warned) with fresh ones, breaking
#      every test file that runs AFTER this one in the same session.
# Confirmed by address: the .opta_remote_env binding is a different environment
# before and after sourcing a script with that header. This was the source of
# 13 order-dependent failures across four test files -- each of which passes in
# isolation, which is exactly why it went unnoticed.
#
# The package is already loaded by the time testthat runs, so the script's own
# load_all() is redundant here -- the same situation as the pipeline
# orchestrator, which the script header explicitly anticipates. Stub it out.
local_no_reload <- function(env = parent.frame()) {
  # No-op when devtools is absent. It is not a dependency of this package, so
  # it is NOT installed in the R CMD check environment, and mocking a binding
  # in a package that cannot be loaded is a hard error rather than a skip.
  # Nothing is lost by skipping the stub there: `data-raw/` is excluded from
  # the built tarball, so the scripts these tests source do not exist under
  # R CMD check and every one of them skips on the `file.exists(script)` guard
  # below. The stub only ever matters for a local devtools::test() run, which
  # is precisely where the mid-suite reload does its damage.
  if (!requireNamespace("devtools", quietly = TRUE)) return(invisible(NULL))
  testthat::local_mocked_bindings(
    load_all = function(...) invisible(NULL),
    .package = "devtools",
    .env = env
  )
}

test_that("13_publish_release_data.R calls vb_publish once per non-empty tag with the registered files", {
  local_no_reload()
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
  local_no_reload()
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

# Fixtures for 09_export_ratings.R. It reads TWO caches and publishes FOUR
# files: seasonal_xrapm + seasonal_spm (the original M-RATINGS-PAIR) and, since
# panna#165, seasonal_rapm_raw + pooled_rapm_raw, which must advance together
# with them. The fixture below was stale at two of those four -- it supplied
# only xrapm/spm, so the script aborted on `seasonal_rapm is empty or NULL`.
# That abort was invisible because these tests were ALSO failing on the
# namespace reload described at the top of this file; fixing that surfaced it.
.pg_rapm_frame <- function(n = 2L, season = FALSE) {
  df <- data.frame(
    player_id = c("p1", "replacement")[seq_len(n)],
    player_name = c("Player One", "Replacement Level")[seq_len(n)],
    rapm = seq_len(n) / 10,
    offense = seq_len(n) / 20,
    defense = -seq_len(n) / 20,
    total_minutes = 900 * seq_len(n),
    stringsAsFactors = FALSE
  )
  if (season) df$season_end_year <- 2025L
  df
}

.pg_write_ratings_caches <- function(cache_dir) {
  saveRDS(list(
    seasonal_xrapm = data.frame(player_id = "p1", xrapm = 0.1),
    seasonal_spm   = data.frame(player_id = "p1", spm = 0.2),
    seasonal_rapm  = .pg_rapm_frame(season = TRUE)
  ), file.path(cache_dir, "07_seasonal_ratings.rds"))
  saveRDS(list(ratings = .pg_rapm_frame()),
          file.path(cache_dir, "04_rapm.rds"))
}

test_that("09_export_ratings.R publishes the whole ratings set in ONE vb_publish call (M-RATINGS-PAIR)", {
  local_no_reload()
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  cache_dir <- dir
  .pg_write_ratings_caches(cache_dir)

  local_mocked_bindings(
    pb_list = function(repo, tag) data.frame(file_name = character(0)),
    .package = "piggyback"
  )

  captured_paths <- NULL
  captured_rows <- NULL
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      captured_paths <<- paths
      # Read row counts HERE, inside the mock: the script unlinks its temp dir
      # as soon as vb_publish returns, so by assertion time the files are gone.
      captured_rows <<- vapply(paths, function(p) nrow(arrow::read_parquet(p)),
                               integer(1))
      list(generation = "test-gen")
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "player-ratings-opta",
                                 "09_export_ratings.R")
  skip_if_not(file.exists(script), "09_export_ratings.R not found")
  source(script, local = TRUE)

  # All four in ONE call -- the point of the invariant is that the shrunk and
  # raw ratings can never advance independently of each other.
  expect_length(captured_paths, 4)
  for (f in c("seasonal_xrapm", "seasonal_spm",
              "seasonal_rapm_raw", "pooled_rapm_raw")) {
    expect_true(any(grepl(paste0(f, "\\.parquet$"), captured_paths)),
                info = paste(f, "missing from the published set"))
  }

  # The fixture feeds 2 rows to each raw export, one of them the synthetic
  # `replacement` player the script drops at the export boundary. Assert the
  # drop actually happened -- without this the replacement row is inert
  # scenery, present in the fixture but verifying nothing.
  raw_idx <- grep("(seasonal_rapm_raw|pooled_rapm_raw)\\.parquet$", captured_paths)
  expect_length(raw_idx, 2)
  expect_equal(unname(captured_rows[raw_idx]), c(1L, 1L))
})

test_that("09_export_ratings.R: a vb_publish failure aborts the script (both-or-neither)", {
  local_no_reload()
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  cache_dir <- dir
  .pg_write_ratings_caches(cache_dir)

  local_mocked_bindings(
    pb_list = function(repo, tag) data.frame(file_name = character(0)),
    .package = "piggyback"
  )
  reached_publish <- FALSE
  local_mocked_bindings(
    vb_publish = function(paths, repo, tag, ...) {
      reached_publish <<- TRUE
      cli::cli_abort("simulated seasonal_spm upload failure",
                      class = "vb_error_transient")
    },
    .package = "panna"
  )

  script <- testthat::test_path("..", "..", "data-raw", "player-ratings-opta",
                                 "09_export_ratings.R")
  skip_if_not(file.exists(script), "09_export_ratings.R not found")

  expect_error(source(script, local = TRUE), class = "vb_error_transient")
  # The error must come FROM vb_publish, not from the script aborting earlier
  # on a missing input. Without this the test passes vacuously the moment a
  # fixture goes stale -- which is exactly what happened here.
  expect_true(reached_publish)
})
