# piggyback.R
#
# Functions for syncing data with GitHub Releases using the piggyback package.
# Data is stored as a single zip file (pannadata.zip) to preserve directory structure.
#
# NOTE: For LOADING data, prefer the new DuckDB-based functions in data_loaders.R:
#   - load_summary(), load_events(), load_shots(), etc.
#   - query_remote_parquet() for custom SQL queries
#
# These functions download individual parquet files and run SQL queries on them,
# which is much more efficient than downloading the entire ZIP archive.
#
# The piggyback functions below are still useful for:
#   - UPLOADING data to GitHub releases (pb_upload_parquet)
#   - Downloading the entire dataset for local development (pb_download_parquet)

# Internal helper: download a file from GitHub Releases via piggyback.
# Returns the path to the downloaded file in tempdir(). Caller is responsible
# for cleanup (e.g., on.exit(unlink(path), add = TRUE)).
.pb_download_file <- function(file_name, repo, tag, label = file_name,
                               show_progress = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  temp_dir <- tempdir()
  local_path <- file.path(temp_dir, file_name)

  tryCatch({
    piggyback::pb_download(
      file = file_name,
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE,
      show_progress = show_progress
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download {label}.",
      "x" = conditionMessage(e),
      "i" = "Make sure {.file {file_name}} exists in the {.val {tag}} release."
    ))
  })

  if (!file.exists(local_path)) {
    cli::cli_abort("Download failed - {.file {file_name}} not found in release.")
  }

  local_path
}


#' Download data from GitHub Releases
#'
#' Downloads the pannadata.zip file from a GitHub Release and extracts it
#' to the local pannadata directory.
#'
#' @section Deprecation Notice:
#' For loading data, prefer the new DuckDB-based functions which are more efficient:
#' \itemize{
#'   \item \code{load_summary()}, \code{load_events()}, \code{load_shots()}, etc.
#'   \item These download only what's needed and filter via SQL
#' }
#'
#' This function is still useful for downloading the complete dataset for local development.
#'
#' @param repo GitHub repository in "owner/repo" format (default: "peteowen1/pannadata")
#' @param tag Release tag to download from (default: "latest")
#' @param dest Destination directory (default: pannadata_dir())
#' @param overwrite Overwrite existing files (default: TRUE)
#' @param show_progress Show download progress (default: TRUE)
#'
#' @return Invisible path to destination directory
#' @export
#' @importFrom utils unzip
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
  if (is.null(dest)) dest <- pannadata_dir()

  progress_msg(sprintf("Downloading data from %s (tag: %s)...", repo, tag))
  zip_file <- .pb_download_file("pannadata.zip", repo, tag,
                                 show_progress = show_progress)
  on.exit(unlink(zip_file), add = TRUE)

  zip_size <- file.size(zip_file) / (1024 * 1024)
  progress_msg(sprintf("Downloaded pannadata.zip (%.1f MB)", zip_size))

  progress_msg(sprintf("Extracting to %s...", dest))
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  unzip(zip_file, exdir = dest, overwrite = overwrite)

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
#' @importFrom utils zip
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
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  data_dir <- file.path(source, "data")
  if (!dir.exists(data_dir)) {
    cli::cli_abort("Data directory does not exist: {.val {data_dir}}")
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

  # Remove old zip if exists
  if (file.exists(zip_file)) file.remove(zip_file)

  # Create zip with directory structure (use withr to avoid process-wide setwd)
  zip_status <- withr::with_dir(source, {
    zip(zip_file, files = "data", extras = "-r")
  })
  if (zip_status != 0) cli::cli_abort("zip() failed with exit code {zip_status}")

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
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
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
#'
#' @examples
#' \dontrun{
#' pb_sync_data()
#' }
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
#'
#' @examples
#' \dontrun{
#' status <- pb_status()
#' status$remote$exists
#' status$local$n_files
#' }
pb_status <- function(repo = "peteowen1/pannadata",
                       tag = "latest",
                       source = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
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


# Parquet-specific upload/download ----

#' Upload parquet files to GitHub Releases
#'
#' Uploads only parquet files (not RDS) to a GitHub Release.
#' This is the preferred upload method for efficient storage.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "latest")
#' @param source Source directory containing data folder (default: pannadata_dir())
#' @param verbose Print progress messages
#'
#' @return Invisible data frame with uploaded file info
#' @export
#' @importFrom utils zip
#'
#' @examples
#' \dontrun{
#' pb_upload_parquet()
#' }
pb_upload_parquet <- function(repo = "peteowen1/pannadata",
                              tag = "latest",
                              source = NULL,
                              verbose = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  # Find all parquet files
  parquet_files <- list.files(
    source,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(parquet_files) == 0) {
    cli::cli_abort(c(
      "No parquet files found in {source}.",
      "i" = "Run {.code build_all_parquet()} first to create parquet files from RDS."
    ))
  }

  if (verbose) {
    total_size <- sum(file.size(parquet_files)) / (1024 * 1024)
    message(sprintf("Found %d parquet files (%.1f MB total)",
                    length(parquet_files), total_size))
  }

  # Create zip of parquet files only
  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "pannadata-parquet.zip")

  if (file.exists(zip_file)) file.remove(zip_file)

  if (verbose) message("Zipping parquet files...")

  # Create relative paths for zip (use withr to avoid process-wide setwd)
  rel_files <- gsub(paste0("^", normalizePath(source, winslash = "/"), "/?"), "",
                    normalizePath(parquet_files, winslash = "/"))

  # Use R's zip function (works on all platforms, handles long file lists)
  result <- tryCatch({
    withr::with_dir(source, {
      zip(zip_file, files = rel_files, flags = "-rq")
    })
    TRUE
  }, error = function(e) {
    cli::cli_warn("zip() with -rq failed, retrying without quiet flag: {conditionMessage(e)}")
    FALSE
  }, warning = function(w) {
    cli::cli_warn("zip() warning (proceeding): {conditionMessage(w)}")
    invokeRestart("muffleWarning")
  })

  if (!result || !file.exists(zip_file)) {
    tryCatch({
      withr::with_dir(source, {
        zip(zip_file, files = rel_files, flags = "-r")
      })
    }, error = function(e) {
      cli::cli_abort("Failed to create zip: {conditionMessage(e)}")
    })
  }

  if (!file.exists(zip_file)) {
    cli::cli_abort("Failed to create zip file")
  }

  zip_size <- file.size(zip_file) / (1024 * 1024)
  if (verbose) message(sprintf("Created zip (%.1f MB)", zip_size))

  # Ensure release exists
  tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    if (verbose) message("Creating new release...")
    piggyback::pb_new_release(repo = repo, tag = tag)
  })

  # Upload
  if (verbose) message("Uploading to GitHub Releases...")
  piggyback::pb_upload(
    file = zip_file,
    repo = repo,
    tag = tag,
    name = "pannadata-parquet.zip",
    overwrite = TRUE
  )

  if (verbose) message("Upload complete")

  invisible(data.frame(
    file = basename(zip_file),
    size_mb = zip_size,
    n_parquet = length(parquet_files)
  ))
}


#' Download parquet files from GitHub Releases
#'
#' Downloads the parquet zip file from a GitHub Release and extracts it.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "latest")
#' @param dest Destination directory (default: pannadata_dir())
#' @param verbose Print progress messages
#'
#' @return Invisible path to destination directory
#' @export
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#' pb_download_parquet()
#' pb_download_parquet(dest = "~/football-data")
#' }
pb_download_parquet <- function(repo = "peteowen1/pannadata",
                                tag = "latest",
                                dest = NULL,
                                verbose = TRUE) {
  if (is.null(dest)) dest <- pannadata_dir()

  if (verbose) message(sprintf("Downloading from %s (tag: %s)...", repo, tag))
  zip_file <- .pb_download_file("pannadata-parquet.zip", repo, tag, label = "parquet data")
  on.exit(unlink(zip_file), add = TRUE)

  if (verbose) {
    zip_size <- file.size(zip_file) / (1024 * 1024)
    message(sprintf("Downloaded (%.1f MB)", zip_size))
  }

  if (verbose) message(sprintf("Extracting to %s...", dest))
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  unzip(zip_file, exdir = dest, overwrite = TRUE)

  n_parquet <- length(list.files(dest, pattern = "\\.parquet$", recursive = TRUE))
  if (verbose) message(sprintf("Extracted %d parquet files", n_parquet))

  invisible(dest)
}


# Source-specific upload/download ----

#' Get release tag for data source
#'
#' Maps source type to GitHub release tag name.
#'
#' @param source_type One of "fbref", "understat", "opta", or "all"
#'
#' @return Character release tag name
#' @keywords internal
get_source_tag <- function(source_type) {
  switch(source_type,
    fbref = "fbref-latest",
    understat = "understat-latest",
    opta = "opta-latest",
    all = "latest",
    cli::cli_abort("Unknown source_type: {.val {source_type}}")
  )
}


#' Get archive filename for data source
#'
#' Maps source type to tar.gz archive filename.
#'
#' @param source_type One of "fbref", "understat", "opta", or "all"
#'
#' @return Character archive filename (tar.gz)
#' @keywords internal
get_source_archive_name <- function(source_type) {
  switch(source_type,
    fbref = "fbref-parquet.tar.gz",
    understat = "understat-parquet.tar.gz",
    opta = "opta-parquet.tar.gz",
    all = "pannadata-parquet.tar.gz",
    cli::cli_abort("Unknown source_type: {.val {source_type}}")
  )
}


#' Get parquet file pattern for data source
#'
#' Returns regex pattern to match parquet files for a source.
#'
#' @param source_type One of "fbref", "understat", "opta", or "all"
#'
#' @return Character regex pattern
#' @keywords internal
get_source_pattern <- function(source_type) {
  switch(source_type,
    fbref = "^(?!understat_).*\\.parquet$",
    understat = "^understat_.*\\.parquet$|/understat_",
    all = "\\.parquet$",
    cli::cli_abort("Unknown source_type: {.val {source_type}}")
  )
}


#' Upload parquet files by source type
#'
#' Uploads parquet files to source-specific GitHub releases.
#' - "fbref": FBref data to fbref-latest tag
#' - "understat": Understat data to understat-latest tag
#' - "all": All data to latest tag (legacy behavior)
#'
#' @param source_type Data source: "fbref", "understat", "opta", or "all"
#' @param repo GitHub repository in "owner/repo" format
#' @param source Source directory containing data folder (default: pannadata_dir())
#' @param verbose Print progress messages
#'
#' @return Invisible data frame with uploaded file info
#' @export
#'
#' @examples
#' \dontrun{
#' # Upload FBref data only
#' pb_upload_source("fbref")
#'
#' # Upload Understat data only
#' pb_upload_source("understat")
#'
#' # Upload all data (legacy)
#' pb_upload_source("all")
#' }
pb_upload_source <- function(source_type = c("fbref", "understat", "opta", "all"),
                              repo = "peteowen1/pannadata",
                              source = NULL,
                              verbose = TRUE) {
  source_type <- match.arg(source_type)

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source)) {
    source <- pannadata_dir()
  }

  tag <- get_source_tag(source_type)
  archive_name <- get_source_archive_name(source_type)

  # Find parquet files matching the source type
  # New structure: data/{source_type}/{table_type}/{league}/{season}.parquet
  if (source_type == "all") {
    # All data - search entire source directory
    all_parquet <- list.files(
      source,
      pattern = "\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
    parquet_files <- all_parquet
  } else {
    # Specific source type - only look in that directory
    source_dir <- file.path(source, source_type)
    if (!dir.exists(source_dir)) {
      cli::cli_abort("Source directory not found: {.val {source_dir}}")
    }
    parquet_files <- list.files(
      source_dir,
      pattern = "\\.parquet$",
      recursive = TRUE,
      full.names = TRUE
    )
  }

  if (length(parquet_files) == 0) {
    cli::cli_abort(c(
      "No {source_type} parquet files found in {source}.",
      "i" = "Run {.code build_all_parquet()} first to create parquet files from RDS."
    ))
  }

  if (verbose) {
    total_size <- sum(file.size(parquet_files)) / (1024 * 1024)
    message(sprintf("Found %d %s parquet files (%.1f MB total)",
                    length(parquet_files), source_type, total_size))
  }

  # Create tar.gz archive
  temp_dir <- tempdir()
  archive_file <- file.path(temp_dir, archive_name)

  if (file.exists(archive_file)) file.remove(archive_file)

  if (verbose) message("Creating tar.gz archive...")

  # Create relative paths for tar (use withr to avoid process-wide setwd)
  rel_files <- gsub(paste0("^", normalizePath(source, winslash = "/"), "/?"), "",
                    normalizePath(parquet_files, winslash = "/"))

  result <- tryCatch({
    withr::with_dir(source, {
      tar(archive_file, files = rel_files, compression = "gzip")
    })
    TRUE
  }, error = function(e) {
    cli::cli_warn("tar() failed: {conditionMessage(e)}")
    FALSE
  })

  if (!result || !file.exists(archive_file)) {
    cli::cli_abort("Failed to create tar.gz archive")
  }

  archive_size <- file.size(archive_file) / (1024 * 1024)
  if (verbose) message(sprintf("Created %s (%.1f MB)", archive_name, archive_size))

  # Ensure release exists
  tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    if (verbose) message("Creating new release: ", tag)
    piggyback::pb_new_release(repo = repo, tag = tag)
    # Wait for GitHub to propagate the new release
    Sys.sleep(3)
  })

  # Upload
  if (verbose) message(sprintf("Uploading to %s...", tag))
  piggyback::pb_upload(
    file = archive_file,
    repo = repo,
    tag = tag,
    name = archive_name,
    overwrite = TRUE
  )

  if (verbose) message("Upload complete")

  invisible(data.frame(
    source_type = source_type,
    tag = tag,
    file = archive_name,
    size_mb = archive_size,
    n_parquet = length(parquet_files)
  ))
}


#' Download parquet files by source type
#'
#' Downloads parquet files from source-specific GitHub releases.
#'
#' @param source_type Data source: "fbref", "understat", "opta", or "all"
#' @param repo GitHub repository in "owner/repo" format
#' @param dest Destination directory (default: pannadata_dir())
#' @param verbose Print progress messages
#'
#' @return Invisible path to destination directory
#' @export
#'
#' @examples
#' \dontrun{
#' # Download FBref data only
#' pb_download_source("fbref")
#'
#' # Download Understat data only
#' pb_download_source("understat")
#' }
pb_download_source <- function(source_type = c("fbref", "understat", "opta", "all"),
                                repo = "peteowen1/pannadata",
                                dest = NULL,
                                verbose = TRUE) {
  source_type <- match.arg(source_type)
  if (is.null(dest)) dest <- pannadata_dir()

  tag <- get_source_tag(source_type)
  archive_name <- get_source_archive_name(source_type)

  if (verbose) message(sprintf("Downloading %s from %s (tag: %s)...",
                               source_type, repo, tag))
  archive_file <- .pb_download_file(archive_name, repo, tag,
                                     label = paste(source_type, "parquet data"))
  on.exit(unlink(archive_file), add = TRUE)

  if (verbose) {
    archive_size <- file.size(archive_file) / (1024 * 1024)
    message(sprintf("Downloaded (%.1f MB)", archive_size))
  }

  if (verbose) message(sprintf("Extracting to %s...", dest))
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)
  untar(archive_file, exdir = dest)

  if (source_type == "all") {
    n_parquet <- length(list.files(dest, pattern = "\\.parquet$", recursive = TRUE))
  } else {
    source_dir <- file.path(dest, source_type)
    n_parquet <- if (dir.exists(source_dir)) {
      length(list.files(source_dir, pattern = "\\.parquet$", recursive = TRUE))
    } else 0
  }

  if (verbose) message(sprintf("Extracted %d parquet files", n_parquet))

  invisible(dest)
}


#' Incrementally download release assets that are missing or stale
#'
#' Syncs a GitHub release to a local directory by downloading only the assets
#' that are **missing**, a **different size**, or **updated more recently** on
#' the release than the local copy. Avoids re-pulling the full multi-GB dataset
#' when only a few consolidated files changed (e.g. the daily Opta scrape
#' refreshes ~10 of ~125 assets). Unlike \code{\link{pb_download_source}} it does
#' not require a tarball asset — it operates on the individual release files.
#'
#' @param dest Destination directory (default: the Opta data dir,
#'   \code{opta_data_dir()}). Files are written flat into this directory,
#'   matching the consolidated layout the loaders read.
#' @param repo GitHub repository in "owner/repo" format.
#' @param tag Release tag (default \code{"opta-latest"}).
#' @param pattern Optional regex on asset names to restrict the sync
#'   (e.g. \code{"^events_|^opta_"}). \code{NULL} considers all assets.
#' @param check_timestamp Logical. If \code{TRUE}, also re-download assets whose
#'   release timestamp is newer than the local file even when the size matches
#'   (stricter; catches same-size content changes but re-pulls files that were
#'   merely re-uploaded). Default \code{FALSE} — size + presence only, which is
#'   the efficient "what actually changed" sync for these append-growing files.
#' @param force Re-download every matching asset regardless of local state.
#' @param dry_run Report what would be downloaded without downloading.
#' @param verbose Print per-file status.
#'
#' @return Invisibly, a data.frame with one row per asset: \code{file_name},
#'   \code{action} ("download"/"skip"), \code{reason}, and \code{size}.
#' @export
#'
#' @examples
#' \dontrun{
#' # See what's out of date without downloading
#' pb_download_opta(dry_run = TRUE)
#'
#' # Pull only the changed consolidated files
#' pb_download_opta()
#'
#' # Just the event files
#' pb_download_opta(pattern = "^events_")
#' }
pb_download_opta <- function(dest = NULL,
                              repo = "peteowen1/pannadata",
                              tag = "opta-latest",
                              pattern = NULL,
                              check_timestamp = FALSE,
                              force = FALSE,
                              dry_run = FALSE,
                              verbose = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }
  if (is.null(dest)) dest <- opta_data_dir()
  if (!dir.exists(dest)) dir.create(dest, recursive = TRUE)

  assets <- piggyback::pb_list(repo = repo, tag = tag)
  if (is.null(assets) || nrow(assets) == 0) {
    cli::cli_abort("No assets found in the {.val {tag}} release of {repo}.")
  }
  if (!is.null(pattern)) {
    assets <- assets[grepl(pattern, assets$file_name), , drop = FALSE]
  }
  if (nrow(assets) == 0) {
    cli::cli_alert_info("No assets match pattern {.val {pattern}}.")
    return(invisible(assets))
  }

  has_time <- "timestamp" %in% names(assets)

  # Decide per asset: download if missing / size-changed / remote-newer.
  res <- data.frame(
    file_name = assets$file_name,
    size = assets$size,
    action = "skip", reason = "up-to-date",
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(assets))) {
    lp <- file.path(dest, assets$file_name[i])
    if (force) {
      res$action[i] <- "download"; res$reason[i] <- "force"; next
    }
    if (!file.exists(lp)) {
      res$action[i] <- "download"; res$reason[i] <- "missing"; next
    }
    fi <- file.info(lp)
    rsize <- assets$size[i]
    if (!is.na(rsize) && !is.na(fi$size) && fi$size != rsize) {
      res$action[i] <- "download"; res$reason[i] <- "size-changed"; next
    }
    if (check_timestamp && has_time && !is.na(assets$timestamp[i]) &&
        as.numeric(assets$timestamp[i]) > as.numeric(fi$mtime)) {
      res$action[i] <- "download"; res$reason[i] <- "remote-newer"
    }
  }

  to_get <- res$file_name[res$action == "download"]
  dl_mb <- sum(res$size[res$action == "download"], na.rm = TRUE) / (1024 * 1024)
  if (verbose) {
    cli::cli_alert_info(
      "{tag}: {length(to_get)} to download ({sprintf('%.1f', dl_mb)} MB), {sum(res$action=='skip')} up to date")
  }

  if (dry_run) {
    if (length(to_get) > 0 && verbose) {
      for (f in to_get) cli::cli_text("  {.file {f}} ({res$reason[res$file_name==f]})")
    }
    return(invisible(res))
  }

  failed <- character(0)
  for (f in to_get) {
    if (verbose) {
      cli::cli_alert("Downloading {.file {f}} ({res$reason[res$file_name==f]})...")
    }
    rsize <- assets$size[assets$file_name == f][1]
    ok <- tryCatch({
      suppressWarnings(piggyback::pb_download(
        file = f, repo = repo, tag = tag, dest = dest,
        overwrite = TRUE, show_progress = verbose))
      lp <- file.path(dest, f)
      # Verify it actually landed at the expected size — piggyback can warn
      # "not found in repo" without erroring, so trust the file, not the return.
      file.exists(lp) && (is.na(rsize) || abs(file.size(lp) - rsize) <= 1)
    }, error = function(e) FALSE)
    if (!ok) {
      failed <- c(failed, f)
      res$action[res$file_name == f] <- "failed"
    }
  }

  n_ok <- length(to_get) - length(failed)
  if (length(failed) > 0) {
    cli::cli_warn(c(
      "Synced {n_ok}/{length(to_get)} file{?s}; {length(failed)} failed to download.",
      "x" = "Not retrieved: {.file {failed}}",
      "i" = "These assets may be missing from the release or unfetchable via piggyback; try {.code gh release download {tag}}."
    ))
  } else if (verbose) {
    if (length(to_get) > 0) {
      cli::cli_alert_success("Synced {n_ok} file{?s} ({sprintf('%.1f', dl_mb)} MB) to {.path {dest}}")
    } else {
      cli::cli_alert_success("Already up to date.")
    }
  }
  invisible(res)
}


#' List releases by source type
#'
#' Shows available releases for different data sources.
#'
#' @param repo GitHub repository in "owner/repo" format
#'
#' @return Data frame with release information by source
#' @export
#'
#' @examples
#' \dontrun{
#' pb_list_sources()
#' }
pb_list_sources <- function(repo = "peteowen1/pannadata") {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  sources <- c("fbref", "understat", "opta", "all")
  # Predictions uses a dedicated download path (pb_download_predictions),
  # not the generic tar.gz pipeline, so handle it separately here.
  pred_sources <- list(predictions = list(tag = "predictions-latest", file = "predictions.parquet"))
  results <- list()

  for (src in c(sources, names(pred_sources))) {
    if (src %in% names(pred_sources)) {
      tag <- pred_sources[[src]]$tag
      archive_name <- pred_sources[[src]]$file
    } else {
      tag <- get_source_tag(src)
      archive_name <- get_source_archive_name(src)
    }

    info <- tryCatch({
      files <- piggyback::pb_list(repo = repo, tag = tag)
      if (archive_name %in% files$file_name) {
        row <- files[files$file_name == archive_name, ]
        data.frame(
          source = src,
          tag = tag,
          file = archive_name,
          size_mb = round(row$size / (1024 * 1024), 1),
          uploaded = row$timestamp
        )
      } else {
        data.frame(
          source = src,
          tag = tag,
          file = NA_character_,
          size_mb = NA_real_,
          uploaded = NA_character_
        )
      }
    }, error = function(e) {
      data.frame(
        source = src,
        tag = tag,
        file = NA_character_,
        size_mb = NA_real_,
        uploaded = NA_character_
      )
    })

    results[[src]] <- info
  }

  do.call(rbind, results)
}


# Predictions download/load ----

#' Download match predictions from GitHub Releases
#'
#' Downloads predictions.parquet from the predictions-latest release
#' on peteowen1/pannadata.
#'
#' @param repo GitHub repository in "owner/repo" format.
#' @param tag Release tag (default: "predictions-latest").
#' @param dest Destination directory. If NULL, uses pannadata_dir()/predictions.
#'
#' @return Invisibly returns the path to the downloaded file.
#' @export
#'
#' @examples
#' \dontrun{
#' pb_download_predictions()
#' }
pb_download_predictions <- function(repo = "peteowen1/pannadata",
                                     tag = "predictions-latest",
                                     dest = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- tryCatch({
      file.path(pannadata_dir(), "predictions")
    }, error = function(e) {
      cli::cli_abort(c(
        "Could not determine pannadata directory.",
        "i" = "Set dest explicitly or configure pannadata_dir() first."
      ))
    })
  }

  dir.create(dest, showWarnings = FALSE, recursive = TRUE)

  parquet_path <- file.path(dest, "predictions.parquet")

  # Pre-delete the existing file: piggyback::pb_download() can warn "not
  # found in repo" without erroring (documented at pb_download_source() for
  # the sibling tarball path), which would otherwise leave a stale
  # predictions.parquet in place and pass the file.exists() check below as
  # if the download had succeeded.
  if (file.exists(parquet_path)) unlink(parquet_path)

  cli::cli_alert_info("Downloading predictions from {repo} ({tag})...")

  tryCatch({
    piggyback::pb_download(
      file = "predictions.parquet",
      repo = repo,
      tag = tag,
      dest = dest,
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download predictions.parquet from {repo} ({tag})",
      "x" = e$message,
      "i" = "Make sure the predictions-latest release exists."
    ))
  })

  if (!file.exists(parquet_path)) {
    cli::cli_abort("Download failed - predictions.parquet not found after download.")
  }

  if (isFALSE(validate_parquet_file(parquet_path))) {
    unlink(parquet_path)
    cli::cli_abort(c(
      "Downloaded predictions.parquet is corrupt (bad magic bytes).",
      "i" = "The corrupt file has been removed. Please re-run to re-download."
    ))
  }

  size_mb <- round(file.info(parquet_path)$size / (1024 * 1024), 2)
  cli::cli_alert_success("Downloaded predictions.parquet ({size_mb} MB)")

  invisible(parquet_path)
}


#' Load match predictions
#'
#' Loads match predictions from local cache or downloads from GitHub Releases.
#'
#' @param source Data source: "local" loads from pannadata/predictions/ (errors
#'   if not found), "remote" always downloads latest from GitHub release first.
#' @param filter_future If TRUE, returns only matches with match_date >= today.
#'
#' @return Data frame of match predictions.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load from remote (always downloads latest)
#' preds <- load_predictions(source = "remote")
#'
#' # Load only future matches
#' preds <- load_predictions(source = "remote", filter_future = TRUE)
#'
#' # Load from local cache
#' preds <- load_predictions(source = "local")
#' }
load_predictions <- function(source = c("remote", "local"),
                              filter_future = FALSE) {
  source <- match.arg(source)

  pred_dir <- tryCatch({
    file.path(pannadata_dir(), "predictions")
  }, error = function(e) {
    cli::cli_abort(c(
      "Could not determine pannadata directory.",
      "i" = "Set dest in {.fn pb_download_predictions} explicitly."
    ))
  })

  parquet_path <- file.path(pred_dir, "predictions.parquet")

  if (source == "remote") {
    pb_download_predictions(dest = pred_dir)
  }

  if (!file.exists(parquet_path)) {
    cli::cli_abort(c(
      "predictions.parquet not found at {.path {parquet_path}}.",
      "i" = "Run {.code pb_download_predictions()} first, or use {.code source = \"remote\"}."
    ))
  }

  predictions <- tryCatch(
    arrow::read_parquet(parquet_path),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to read {.path {parquet_path}}.",
        "x" = e$message,
        "i" = "The file may be corrupt. Try {.code pb_download_predictions()} to re-download."
      ))
    }
  )

  if (filter_future) {
    if (!"match_date" %in% names(predictions)) {
      cli::cli_abort("Column {.val match_date} not found in predictions data.")
    }
    if (!inherits(predictions$match_date, "Date")) {
      predictions$match_date <- as.Date(predictions$match_date)
    }
    predictions <- predictions[predictions$match_date >= Sys.Date(), ]
  }

  predictions
}
