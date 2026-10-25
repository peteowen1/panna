# Tests for piggyback.R
# Covers: .pb_download_file, pb_download_predictions

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

  # pb_list mocked absent (NULL) -- size verification degrades to "skip",
  # matching the real behaviour when the tag/asset isn't listed.
  local_mocked_bindings(
    pb_list = function(repo, tag) NULL,
    # Mock pb_download to write a file instead of downloading
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
    pb_list = function(repo, tag) NULL,
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

test_that(".pb_download_file rejects a truncated download when the release lists a different size (PA7)", {
  skip_if_not_installed("piggyback")

  local_mocked_bindings(
    pb_list = function(repo, tag) {
      data.frame(file_name = "test.rds", size = 999999, stringsAsFactors = FALSE)
    },
    pb_download = function(file, repo, tag, dest, overwrite, show_progress) {
      writeLines("short", file.path(dest, file))  # far smaller than 999999
    },
    .package = "piggyback"
  )

  expect_error(
    .pb_download_file("test.rds", "peteowen1/pannadata", "v1", show_progress = FALSE),
    class = "vb_error_integrity"
  )
  expect_false(file.exists(file.path(tempdir(), "test.rds")))
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
