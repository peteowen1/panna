# piggyback.R
#
# Functions for syncing data with GitHub Releases using the piggyback package.
# This allows sharing large data files that can't be stored in git.

#' Download data from GitHub Releases
#'
#' Downloads all data files from a GitHub Release to the local pannadata directory.
#'
#' @param repo GitHub repository in "owner/repo" format (default: "peteowen/pannadata")
#' @param tag Release tag to download from (default: "data")
#' @param dest Destination directory (default: pannadata_dir())
#' @param overwrite Overwrite existing files (default: FALSE)
#' @param show_progress Show download progress (default: TRUE)
#'
#' @return Invisible vector of downloaded file paths
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
pb_download_data <- function(repo = "peteowen/pannadata",
                              tag = "data",
                              dest = NULL,
                              overwrite = FALSE,
                              show_progress = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- pannadata_dir()
  }

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  progress_msg(sprintf("Downloading data from %s (tag: %s)...", repo, tag))

  # List available files
  files <- tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    stop("Failed to list files from ", repo, ": ", e$message)
  })

  if (nrow(files) == 0) {
    message("No files found in release")
    return(invisible(character(0)))
  }

  progress_msg(sprintf("Found %d files to download", nrow(files)))

  # Download files
  downloaded <- piggyback::pb_download(
    repo = repo,
    tag = tag,
    dest = dest,
    overwrite = overwrite,
    show_progress = show_progress
  )

  progress_msg("Download complete")
  invisible(downloaded)
}


#' Upload data to GitHub Releases
#'
#' Uploads local data files to a GitHub Release. Creates the release if it
#' doesn't exist. Files are organized by table type.
#'
#' @param repo GitHub repository in "owner/repo" format (default: "peteowen/pannadata")
#' @param tag Release tag to upload to (default: "data")
#' @param source Source directory (default: pannadata_dir())
#' @param table_types Which table types to upload (default: all)
#' @param league Optional league filter
#' @param season Optional season filter
#' @param overwrite Overwrite existing files in release (default: TRUE)
#'
#' @return Invisible vector of uploaded file paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Upload all data
#' pb_upload_data()
#'
#' # Upload only ENG 2024-2025
#' pb_upload_data(league = "ENG", season = "2024-2025")
#' }
pb_upload_data <- function(repo = "peteowen/pannadata",
                            tag = "data",
                            source = NULL,
                            table_types = NULL,
                            league = NULL,
                            season = NULL,
                            overwrite = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  if (!dir.exists(source)) {
    stop("Source directory does not exist: ", source)
  }

  # Default table types
  if (is.null(table_types)) {
    table_types <- c("metadata", "summary", "passing", "passing_types",
                     "defense", "possession", "misc", "keeper", "shots", "fixtures")
  }

  progress_msg(sprintf("Preparing to upload data to %s (tag: %s)...", repo, tag))

  # Ensure release exists
  tryCatch({
    releases <- piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    progress_msg("Creating new release...")
    piggyback::pb_new_release(repo = repo, tag = tag)
  })

  # Collect files to upload
  files_to_upload <- character(0)

  for (tt in table_types) {
    tt_dir <- file.path(source, tt)
    if (!dir.exists(tt_dir)) next

    # Build pattern based on filters
    if (!is.null(league) && !is.null(season)) {
      # Specific league/season
      search_dir <- file.path(tt_dir, league, season)
      if (dir.exists(search_dir)) {
        files <- list.files(search_dir, pattern = "\\.rds$", full.names = TRUE)
        files_to_upload <- c(files_to_upload, files)
      }
    } else if (!is.null(league)) {
      # All seasons for a league
      league_dir <- file.path(tt_dir, league)
      if (dir.exists(league_dir)) {
        files <- list.files(league_dir, pattern = "\\.rds$",
                            full.names = TRUE, recursive = TRUE)
        files_to_upload <- c(files_to_upload, files)
      }
    } else {
      # All files for this table type
      files <- list.files(tt_dir, pattern = "\\.rds$",
                          full.names = TRUE, recursive = TRUE)
      files_to_upload <- c(files_to_upload, files)
    }
  }

  if (length(files_to_upload) == 0) {
    message("No files found to upload")
    return(invisible(character(0)))
  }

  progress_msg(sprintf("Uploading %d files...", length(files_to_upload)))

  # Upload in batches to avoid timeout
  batch_size <- 100
  n_batches <- ceiling(length(files_to_upload) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(files_to_upload))
    batch <- files_to_upload[start_idx:end_idx]

    progress_msg(sprintf("  Batch %d/%d (%d files)", i, n_batches, length(batch)))

    piggyback::pb_upload(
      file = batch,
      repo = repo,
      tag = tag,
      overwrite = overwrite
    )
  }

  progress_msg("Upload complete")
  invisible(files_to_upload)
}


#' List files in GitHub Release
#'
#' Shows what data files are available in a GitHub Release.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "data")
#'
#' @return Data frame with file information
#' @export
#'
#' @examples
#' \dontrun{
#' pb_list_data()
#' }
pb_list_data <- function(repo = "peteowen/pannadata", tag = "data") {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  piggyback::pb_list(repo = repo, tag = tag)
}


#' Sync local data with GitHub Releases
#'
#' Uploads any local files that aren't in the release yet.
#' This is a more targeted upload than pb_upload_data.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "data")
#' @param source Source directory (default: pannadata_dir())
#' @param table_types Which table types to sync (default: all)
#'
#' @return Number of new files uploaded
#' @export
pb_sync_data <- function(repo = "peteowen/pannadata",
                          tag = "data",
                          source = NULL,
                          table_types = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    stop("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  if (is.null(table_types)) {
    table_types <- c("metadata", "summary", "passing", "passing_types",
                     "defense", "possession", "misc", "keeper", "shots", "fixtures")
  }

  progress_msg("Checking for new files to sync...")

  # Get remote files
  remote_files <- tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    progress_msg("Creating new release...")
    piggyback::pb_new_release(repo = repo, tag = tag)
    data.frame(file_name = character(0))
  })

  remote_names <- if (nrow(remote_files) > 0) remote_files$file_name else character(0)

  # Get local files
  local_files <- character(0)
  for (tt in table_types) {
    tt_dir <- file.path(source, tt)
    if (dir.exists(tt_dir)) {
      files <- list.files(tt_dir, pattern = "\\.rds$",
                          full.names = TRUE, recursive = TRUE)
      local_files <- c(local_files, files)
    }
  }

  local_names <- basename(local_files)

  # Find new files
  new_files <- local_files[!local_names %in% remote_names]

  if (length(new_files) == 0) {
    progress_msg("All files already synced")
    return(invisible(0))
  }

  progress_msg(sprintf("Uploading %d new files...", length(new_files)))

  piggyback::pb_upload(
    file = new_files,
    repo = repo,
    tag = tag
  )

  progress_msg("Sync complete")
  invisible(length(new_files))
}
