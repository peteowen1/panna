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
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
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
    cli::cli_abort(c(
      "Failed to download from {repo}.",
      "x" = conditionMessage(e),
      "i" = "Make sure the 'latest' release exists with pannadata.zip."
    ))
  })

  if (!file.exists(zip_file)) {
    cli::cli_abort("Download failed - pannadata.zip not found in release")
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

  # Create relative paths for zip
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(source)

  rel_files <- gsub(paste0("^", normalizePath(source, winslash = "/"), "/?"), "",
                    normalizePath(parquet_files, winslash = "/"))

  # Use R's zip function (works on all platforms, handles long file lists)
  result <- tryCatch({
    zip(zip_file, files = rel_files, flags = "-rq")
    TRUE
  }, error = function(e) {
    cli::cli_warn("zip() failed: {conditionMessage(e)}")
    FALSE
  }, warning = function(w) {
    # zip() may warn but still succeed
    TRUE
  })

  if (!result || !file.exists(zip_file)) {
    # Fallback: try without -q flag
    tryCatch({
      zip(zip_file, files = rel_files, flags = "-r")
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
pb_download_parquet <- function(repo = "peteowen1/pannadata",
                                tag = "latest",
                                dest = NULL,
                                verbose = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- pannadata_dir()
  }

  if (verbose) message(sprintf("Downloading from %s (tag: %s)...", repo, tag))

  temp_dir <- tempdir()
  zip_file <- file.path(temp_dir, "pannadata-parquet.zip")

  tryCatch({
    piggyback::pb_download(
      file = "pannadata-parquet.zip",
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download parquet data.",
      "x" = conditionMessage(e),
      "i" = "Make sure pannadata-parquet.zip exists in the release."
    ))
  })

  if (!file.exists(zip_file)) {
    cli::cli_abort("Download failed - pannadata-parquet.zip not found in release")
  }

  if (verbose) {
    zip_size <- file.size(zip_file) / (1024 * 1024)
    message(sprintf("Downloaded (%.1f MB)", zip_size))
  }

  # Extract
  if (verbose) message(sprintf("Extracting to %s...", dest))

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  unzip(zip_file, exdir = dest, overwrite = TRUE)
  file.remove(zip_file)

  # Count extracted files
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
#' @param source_type One of "fbref", "understat", or "all"
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
#' @param source_type Data source: "fbref", "understat", or "all"
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

  # Create relative paths for tar
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(source)

  rel_files <- gsub(paste0("^", normalizePath(source, winslash = "/"), "/?"), "",
                    normalizePath(parquet_files, winslash = "/"))

  # Use R's tar function with gzip compression
  result <- tryCatch({
    tar(archive_file, files = rel_files, compression = "gzip")
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
#' @param source_type Data source: "fbref", "understat", or "all"
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

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(dest)) {
    dest <- pannadata_dir()
  }

  tag <- get_source_tag(source_type)
  archive_name <- get_source_archive_name(source_type)

  if (verbose) message(sprintf("Downloading %s from %s (tag: %s)...",
                               source_type, repo, tag))

  temp_dir <- tempdir()
  archive_file <- file.path(temp_dir, archive_name)

  tryCatch({
    piggyback::pb_download(
      file = archive_name,
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download {source_type} parquet data.",
      "x" = conditionMessage(e),
      "i" = "Make sure {archive_name} exists in the '{tag}' release."
    ))
  })

  if (!file.exists(archive_file)) {
    cli::cli_abort("Download failed - {archive_name} not found in release.")
  }

  if (verbose) {
    archive_size <- file.size(archive_file) / (1024 * 1024)
    message(sprintf("Downloaded (%.1f MB)", archive_size))
  }

  # Extract tar.gz
  if (verbose) message(sprintf("Extracting to %s...", dest))

  if (!dir.exists(dest)) {
    dir.create(dest, recursive = TRUE)
  }

  untar(archive_file, exdir = dest)
  file.remove(archive_file)

  # Count extracted files
  # New structure: data/{source_type}/{table_type}/{league}/{season}.parquet
  if (source_type == "all") {
    n_parquet <- length(list.files(dest, pattern = "\\.parquet$", recursive = TRUE))
  } else {
    source_dir <- file.path(dest, source_type)
    if (dir.exists(source_dir)) {
      n_parquet <- length(list.files(source_dir, pattern = "\\.parquet$", recursive = TRUE))
    } else {
      n_parquet <- 0
    }
  }

  if (verbose) message(sprintf("Extracted %d parquet files", n_parquet))

  invisible(dest)
}


#' List releases by source type
#'
#' Shows available releases for different data sources.
#'
#' @param repo GitHub repository in "owner/repo" format
#'
#' @return Data frame with release information by source
#' @export
pb_list_sources <- function(repo = "peteowen1/pannadata") {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  sources <- c("fbref", "understat", "opta", "all")
  results <- list()

  for (src in sources) {
    tag <- get_source_tag(src)
    archive_name <- get_source_archive_name(src)

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
