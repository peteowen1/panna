# Tests for panna H-STALE (ECOSYSTEM-FIX-PLAN.md PA3):
# pb_download_predictions()/load_predictions(source = "remote") must never
# silently serve a stale pre-existing predictions.parquet when the remote
# download fails or produces a corrupt file. Both are now routed through
# vb_download()'s temp -> verify -> atomic-rename discipline. Mocks
# piggyback + gh at the package namespace boundary; no network is hit.

.stale_fake_release <- function(assets = list()) {
  # Minimal shape vb_list_assets() expects back from gh::gh() for a
  # "GET /repos/{owner}/{repo}/releases/tags/{tag}" call.
  list(assets = assets)
}

test_that("pb_download_predictions errors (does not serve stale file) when the remote download silently no-ops", {
  dir <- withr::local_tempdir()
  dest_path <- file.path(dir, "predictions.parquet")
  writeLines("STALE-BUT-VALID-LOOKING-CONTENT", dest_path)
  prior <- readLines(dest_path)

  # No manifest on the tag -- legacy mode.
  local_mocked_bindings(gh = function(...) .stale_fake_release(), .package = "gh")
  # piggyback silently no-ops: it can warn "not found in repo" WITHOUT
  # erroring (the documented pb_download_opta() gotcha) -- simulate that by
  # not writing anything to the tempdir.
  local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) {
      invisible(NULL)
    },
    .package = "piggyback"
  )

  expect_error(
    pb_download_predictions(repo = "test/fixture", tag = "predictions-latest",
                             dest = dir),
    class = "vb_error_transient"
  )
  # The stale file must be untouched, not deleted-and-left-absent either.
  expect_true(file.exists(dest_path))
  expect_identical(readLines(dest_path), prior)
})

test_that("pb_download_predictions errors on corrupt magic bytes and leaves the prior file untouched", {
  dir <- withr::local_tempdir()
  dest_path <- file.path(dir, "predictions.parquet")
  writeLines("PRIOR-GOOD-PREDICTIONS", dest_path)
  prior <- readLines(dest_path)

  local_mocked_bindings(gh = function(...) .stale_fake_release(), .package = "gh")
  local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) {
      # Corrupt download: no PAR1 magic bytes.
      writeBin(charToRaw("not-a-real-parquet-file"), file.path(dest, file))
    },
    .package = "piggyback"
  )

  expect_error(
    pb_download_predictions(repo = "test/fixture", tag = "predictions-latest",
                             dest = dir),
    class = "vb_error_integrity"
  )
  expect_identical(readLines(dest_path), prior)
  leftovers <- list.files(dir, pattern = "^\\.vb_dl_", all.files = TRUE)
  expect_length(leftovers, 0)
})

test_that("pb_download_predictions replaces a stale file on a verified successful download", {
  dir <- withr::local_tempdir()
  dest_path <- file.path(dir, "predictions.parquet")
  writeLines("OLD-STALE-CONTENT", dest_path)

  local_mocked_bindings(gh = function(...) .stale_fake_release(), .package = "gh")
  local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) {
      con <- file(file.path(dest, file), "wb")
      writeBin(charToRaw("PAR1FRESH-DATA-HEREPAR1"), con)
      close(con)
    },
    .package = "piggyback"
  )

  result <- pb_download_predictions(repo = "test/fixture", tag = "predictions-latest",
                                     dest = dir)
  expect_identical(result, dest_path)
  expect_identical(readBin(dest_path, "raw", 4L), charToRaw("PAR1"))
  expect_true(file.exists(paste0(dest_path, ".sha256")))
})

test_that("load_predictions(source = 'remote') propagates a typed error instead of returning stale data", {
  dir <- withr::local_tempdir()
  pred_dir <- file.path(dir, "predictions")
  dir.create(pred_dir)
  dest_path <- file.path(pred_dir, "predictions.parquet")
  writeLines("STALE", dest_path)

  old_pannadata_dir <- pannadata_dir()
  pannadata_dir(dir)
  withr::defer(pannadata_dir(old_pannadata_dir))

  local_mocked_bindings(gh = function(...) .stale_fake_release(), .package = "gh")
  local_mocked_bindings(
    pb_download = function(file, dest, repo, tag, overwrite = TRUE, ...) invisible(NULL),
    .package = "piggyback"
  )

  expect_error(
    load_predictions(source = "remote"),
    class = "vb_error_transient"
  )
})
