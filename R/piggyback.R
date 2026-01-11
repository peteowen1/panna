# piggyback.R
#
# Functions for syncing data with GitHub Releases using the piggyback package.
# Data is stored as a single zip file (pannadata.zip) to preserve directory structure.

#' Download data from GitHub Releases
#'
#' Downloads the pannadata.zip file from a GitHub Release and extracts it
#' to the local pannadata directory.
#'
#' @param repo GitHub repository in "owner/repo" format (default: "peteowen1/pannadata")
#' @param tag Release tag to download from (default: "latest")
#' @param dest Destination directory (default: pannadata_dir())
#' @param overwrite Overwrite existing files (default: TRUE)
#' @param show_progress Show download progress (default: TRUE)
#'
#' @return Invisible path to destination directory
#' @export
#'
#' @examples
#' \dontrun{
#' # Download all data
#' pb_download_data()
#'
#' # Download to custom location
#' pb_download_data(dest = "my/data/path")
#' }
pb_download_data <- function(repo = "peteowen1/pannadata",
                              tag = "latest",
                              dest = NULL,
                              overwrite = TRUE,
                              show_progress = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- pannadata_dir()
  }

  progress_msg(sprintf("Downloading data from %s (tag: %s)...", repo, tag))

  # Create temp directory for download
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "pannadata.zip")

  # Download the zip file
  tryCatch({
    piggyback::pb_download(
      file = "pannadata.zip",
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE,
      show_progress = show_progress
    )
  }, error = function(e) {
    stop("Failed to download from ", repo, ": ", e$message,
         "\nMake sure the 'latest' release exists with pannadata.zip")
  })

  if (!file.exists(zip_file)) {
    stop("Download failed - pannadata.zip not found in release")
  }

  zip_size <- file.size(zip_file) / (1024 * 1024)
  progress_msg(sprintf("Downloaded pannadata.zip (%.1f MB)", zip_size))

  # Extract to destination
  progress_msg(sprintf("Extracting to %s...", dest))

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  # Extract - the zip contains a 'data' folder
  unzip(zip_file, exdir = dest, overwrite = overwrite)

  # Cleanup
  file.remove(zip_file)

  # Count extracted files
  data_dir <- file.path(dest, "data")
  if (dir.exists(data_dir)) {
    n_files <- length(list.files(data_dir, recursive = TRUE, pattern = "\\.rds$"))
    progress_msg(sprintf("Extracted %d RDS files", n_files))
  }

  progress_msg("Download complete")
  invisible(dest)
}


#' Upload data to GitHub Releases
#'
#' Zips the local data directory and uploads it to a GitHub Release.
#' Creates the release if it doesn't exist.
#'
#' @param repo GitHub repository in "owner/repo" format (default: "peteowen1/pannadata")
#' @param tag Release tag to upload to (default: "latest")
#' @param source Source directory containing 'data' folder (default: pannadata_dir())
#'
#' @return Invisible path to uploaded zip file
#' @export
#'
#' @examples
#' \dontrun{
#' # Upload all data
#' pb_upload_data()
#' }
pb_upload_data <- function(repo = "peteowen1/pannadata",
                            tag = "latest",
                            source = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  data_dir <- file.path(source, "data")
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist: ", data_dir)
  }

  progress_msg(sprintf("Preparing to upload data to %s (tag: %s)...", repo, tag))

  # Ensure release exists
  tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
    progress_msg("Release exists")
  }, error = function(e) {
    progress_msg("Creating new release...")
    piggyback::pb_new_release(repo = repo, tag = tag)
  })

  # Create zip file
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "pannadata.zip")

  progress_msg("Zipping data directory...")

  # Zip from within source directory to preserve structure

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(source)

  # Remove old zip if exists
  if (file.exists(zip_file)) file.remove(zip_file)

  # Create zip with directory structure
  zip(zip_file, files = "data", extras = "-r")

  zip_size <- file.size(zip_file) / (1024 * 1024)
  progress_msg(sprintf("Created pannadata.zip (%.1f MB)", zip_size))

  # Upload
  progress_msg("Uploading to GitHub Releases...")
  piggyback::pb_upload(
    file = zip_file,
    repo = repo,
    tag = tag,
    overwrite = TRUE
  )

  progress_msg("Upload complete")
  invisible(zip_file)
}


#' List files in GitHub Release
#'
#' Shows what files are available in a GitHub Release.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "latest")
#'
#' @return Data frame with file information
#' @export
#'
#' @examples
#' \dontrun{
#' pb_list_data()
#' }
pb_list_data <- function(repo = "peteowen1/pannadata", tag = "latest") {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  piggyback::pb_list(repo = repo, tag = tag)
}


#' Sync local data with GitHub Releases
#'
#' Convenience function that uploads local data to GitHub Releases.
#' This replaces the existing data in the release.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "latest")
#' @param source Source directory (default: pannadata_dir())
#'
#' @return Invisible NULL
#' @export
pb_sync_data <- function(repo = "peteowen1/pannadata",
                          tag = "latest",
                          source = NULL) {
  pb_upload_data(repo = repo, tag = tag, source = source)
}


#' Check if local data is in sync with GitHub Releases
#'
#' Compares local data count with what's in the release.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "latest")
#' @param source Source directory (default: pannadata_dir())
#'
#' @return List with sync status information
#' @export
pb_status <- function(repo = "peteowen1/pannadata",
                       tag = "latest",
                       source = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  # Check remote
  remote_info <- tryCatch({
    files <- piggyback::pb_list(repo = repo, tag = tag)
    if ("pannadata.zip" %in% files$file_name) {
      row <- files[files$file_name == "pannadata.zip", ]
      list(
        exists = TRUE,
        size_mb = row$size / (1024 * 1024),
        uploaded = row$timestamp
      )
    } else {
      list(exists = FALSE, size_mb = 0, uploaded = NA)
    }
  }, error = function(e) {
    list(exists = FALSE, size_mb = 0, uploaded = NA, error = e$message)
  })

  # Check local
  data_dir <- file.path(source, "data")
  local_files <- if (dir.exists(data_dir)) {
    list.files(data_dir, recursive = TRUE, pattern = "\\.rds$")
  } else {
    character(0)
  }

  list(
    remote = remote_info,
    local = list(
      exists = length(local_files) > 0,
      n_files = length(local_files),
      data_dir = data_dir
    )
  )
}
