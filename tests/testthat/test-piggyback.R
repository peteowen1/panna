# Tests for piggyback.R
# Covers: .pb_download_file, pb_download_source, get_source_tag,
#         get_source_archive_name, clear_remote_cache

# ===========================================================================
# get_source_tag / get_source_archive_name
# ===========================================================================

test_that("get_source_tag returns correct tags", {
  expect_equal(get_source_tag("opta"), "opta-latest")
  expect_equal(get_source_tag("fbref"), "fbref-latest")
  expect_equal(get_source_tag("understat"), "understat-latest")
  expect_equal(get_source_tag("all"), "latest")
})

test_that("get_source_archive_name returns correct filenames", {
  expect_true(grepl("\\.tar\\.gz$", get_source_archive_name("opta")))
  expect_true(grepl("\\.tar\\.gz$", get_source_archive_name("fbref")))
  expect_true(grepl("\\.tar\\.gz$", get_source_archive_name("all")))
})


# ===========================================================================
# .pb_download_file
# ===========================================================================

test_that(".pb_download_file errors without piggyback package", {
  skip_if(requireNamespace("piggyback", quietly = TRUE),
          "Test requires piggyback to NOT be installed")
  expect_error(.pb_download_file("test.rds", "owner/repo", "v1"), "piggyback")
})

test_that(".pb_download_file calls piggyback::pb_download with correct args", {
  skip_if_not_installed("piggyback")

  # Mock pb_download to write a file instead of downloading
  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, overwrite, show_progress) {
      writeLines("mock data", file.path(dest, file))
    },
    .package = "piggyback"
  )

  result <- .pb_download_file("test.rds", "peteowen1/pannadata", "v1",
                               show_progress = FALSE)
  on.exit(unlink(result))

  expect_true(file.exists(result))
  expect_equal(basename(result), "test.rds")
})

test_that(".pb_download_file errors on download failure", {
  skip_if_not_installed("piggyback")

  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, ...) {
      stop("HTTP 404: Not Found")
    },
    .package = "piggyback"
  )

  expect_error(
    .pb_download_file("missing.rds", "owner/repo", "v1"),
    "Failed to download"
  )
})


# ===========================================================================
# pb_download_source
# ===========================================================================

test_that("pb_download_source validates source_type", {
  expect_error(pb_download_source("invalid"), "should be one of")
})

test_that("pb_download_source calls .pb_download_file and extracts", {
  skip_if_not_installed("piggyback")

  # Create a real tar.gz archive for the mock to "download"
  tmp_src <- tempfile("source_test_")
  dir.create(file.path(tmp_src, "opta"), recursive = TRUE)
  writeLines("mock", file.path(tmp_src, "opta", "test.parquet"))
  tar_file <- tempfile(fileext = ".tar.gz")
  tar(tar_file, files = "opta", compression = "gzip",
      extra_flags = paste("-C", shQuote(tmp_src)))
  on.exit(unlink(c(tmp_src, tar_file), recursive = TRUE))

  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, overwrite, show_progress) {
      file.copy(tar_file, file.path(dest, file))
    },
    .package = "piggyback"
  )

  dest <- tempfile("dest_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  suppressMessages(
    result <- pb_download_source("opta", dest = dest, verbose = FALSE)
  )

  expect_equal(result, dest)
})


# ===========================================================================
# pb_status
# ===========================================================================

test_that("pb_status function exists and accepts repo parameter", {
  expect_true(is.function(pb_status))
  expect_true("repo" %in% names(formals(pb_status)))
})


# ===========================================================================
# pb_list_sources
# ===========================================================================

test_that("pb_list_sources function exists", {
  expect_true(is.function(pb_list_sources))
})


# ===========================================================================
# pb_download_predictions
# ===========================================================================

test_that("pb_download_predictions function exists with expected params", {
  expect_true(is.function(pb_download_predictions))
  fn_args <- names(formals(pb_download_predictions))
  expect_true("repo" %in% fn_args)
  expect_true("dest" %in% fn_args)
})
