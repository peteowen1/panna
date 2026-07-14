# piggyback.R
#
# Functions for syncing data with GitHub Releases using the piggyback package.
#
# NOTE: For LOADING data, prefer the new DuckDB-based functions in data_loaders.R:
#   - load_summary(), load_events(), load_shots(), etc.
#   - query_remote_parquet() for custom SQL queries
#
# These functions download individual parquet files and run SQL queries on them,
# which is much more efficient than downloading the entire ZIP archive.
#
# The functions below cover: .pb_download_file (generic single-asset downloader,
# used by data-raw/bootstrap.R), pb_download_opta() (incremental Opta sync, the
# primary data source), and predictions download/load.

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

  # Size-verify against the live asset list before the caller uses the file
  # (panna M-ARCH-INT, 2026-07-08 review): the since-removed tarball downloads
  # never checked this, so a truncated archive could extract a silent subset
  # with no error until some file inside it turned out missing later.
  # Generalizes the size check pb_download_opta() already does for individual
  # parquet assets. Best-effort: if the listing itself fails, skip
  # verification rather than block a download that might otherwise succeed.
  expected_size <- tryCatch({
    assets <- piggyback::pb_list(repo = repo, tag = tag)
    if (!is.null(assets) && file_name %in% assets$file_name) {
      assets$size[assets$file_name == file_name][1]
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

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

  if (!is.na(expected_size)) {
    actual_size <- file.size(local_path)
    if (!isTRUE(all.equal(as.numeric(actual_size), as.numeric(expected_size)))) {
      unlink(local_path)
      cli::cli_abort(c(
        "Downloaded {label} is truncated/corrupt.",
        "x" = "Size {actual_size} bytes != listed {expected_size} bytes on the release.",
        "i" = "Re-run to re-download; the partial file has been removed."
      ), class = "vb_error_integrity")
    }
  }

  local_path
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



#' Incrementally download release assets that are missing or stale
#'
#' Syncs a GitHub release to a local directory by downloading only the assets
#' that are **missing**, a **different size**, or **updated more recently** on
#' the release than the local copy. Avoids re-pulling the full multi-GB dataset
#' when only a few consolidated files changed (e.g. the daily Opta scrape
#' refreshes ~10 of ~125 assets). Unlike the old tarball-based downloads, it does
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
#' @family data distribution
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
#' @family data distribution
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
#' @family data distribution
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

  cli::cli_alert_info("Downloading predictions from {repo} ({tag})...")

  # Routed through vb_download() (panna H-STALE, 2026-07-08 review): downloads
  # to a tempfile in dest's own directory, verifies parquet magic bytes + the
  # tag's bus_manifest.json sha256 (when one exists; legacy mode falls back to
  # size vs the live asset list), THEN atomically renames into place. A
  # pre-existing predictions.parquet is NEVER served as a silent fallback on
  # failure -- the typed vb_error (absent/transient/integrity) propagates
  # instead. The old implementation pre-deleted dest, then called
  # piggyback::pb_download() straight into it: piggyback can warn "not found
  # in repo" WITHOUT erroring, so a failed re-download used to leave dest
  # simply absent (file.exists() below would then also fail) rather than
  # raising a typed, retryable error the caller could dispatch on -- and
  # there was no verification at all when a manifest existed.
  vb_download(repo, tag, "predictions.parquet", parquet_path)

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
#' @family data distribution
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
