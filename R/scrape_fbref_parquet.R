# Parquet building and data loading for FBref
#
# Handles conversion of RDS cache to parquet format, consolidated file building,
# GitHub releases upload, and convenience data loading functions.
# Depends on scrape_fbref_cache.R for cache operations.



# ============================================================================
# Remote Parquet Cache
# ============================================================================

# Session cache for remote parquet data (internal, not exported)
.remote_parquet_cache <- new.env(parent = emptyenv())


#' Get remote parquet cache directory
#'
#' Downloads and extracts parquet zip from GitHub releases to a temp directory.
#' Cached for the R session - subsequent calls return the cached path.
#'
#' @param repo GitHub repo (default: "peteowen1/pannadata")
#' @param tag Release tag (default: "latest")
#' @param force Re-download even if cached
#'
#' @return Path to extracted parquet directory
#' @keywords internal
get_remote_parquet_cache <- function(repo = "peteowen1/pannadata",
                                     tag = "latest",
                                     force = FALSE) {
  cache_key <- paste0(repo, "@", tag)


  # Return cached path if available

if (!force && exists(cache_key, envir = .remote_parquet_cache)) {
    cached_path <- get(cache_key, envir = .remote_parquet_cache)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
  }

  # Download and extract
  message("Downloading parquet data from GitHub releases...")

  temp_dir <- file.path(tempdir(), paste0("pannadata_remote_", tag))
  zip_file <- file.path(tempdir(), "pannadata-parquet.zip")

  # Clean up old temp dir if exists
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)
  dir.create(temp_dir, recursive = TRUE)

  # Clean up old zip if exists
 if (file.exists(zip_file)) file.remove(zip_file)

  # Download using piggyback
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' required for remote loading. Install with: install.packages('piggyback')")
  }

  tryCatch({
    piggyback::pb_download(
      file = "pannadata-parquet.zip",
      repo = repo,
      tag = tag,
      dest = tempdir(),
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort("Failed to download from GitHub releases: {conditionMessage(e)}")
  })

  if (!file.exists(zip_file)) {
    cli::cli_abort("Download failed - pannadata-parquet.zip not found in {tempdir()}")
  }

  # Extract
  unzip(zip_file, exdir = temp_dir, overwrite = TRUE)
  file.remove(zip_file)

  n_files <- length(list.files(temp_dir, pattern = "\\.parquet$", recursive = TRUE))
  message(sprintf("Cached %d parquet files for this session", n_files))

  # Cache the path
  assign(cache_key, temp_dir, envir = .remote_parquet_cache)

  temp_dir
}


# ============================================================================
# Cache Aggregation
# ============================================================================

#' Aggregate cached match data
#'
#' Combines all cached data for a table type into a single data frame.
#' Uses parquet files if available (fast), falls back to RDS files (slower).
#'
#' @param table_type Type of table to aggregate (e.g., "summary", "shots")
#' @param league Optional league filter (e.g., "ENG")
#' @param season Optional season filter (e.g., "2024-2025")
#' @param prefer_parquet If TRUE (default), use parquet when available
#' @param source "local" (default) or "remote" to download from GitHub releases
#'
#' @return Combined data frame, or NULL if no cached data
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Load all cached summary stats (uses parquet if available)
#' all_summary <- aggregate_cached_matches("summary")
#'
#' # Load only Premier League 2024-2025
#' pl_summary <- aggregate_cached_matches("summary", league = "ENG",
#'                                         season = "2024-2025")
#' }
aggregate_cached_matches <- function(table_type, league = NULL, season = NULL,
                                     prefer_parquet = TRUE,
                                     source = c("local", "remote")) {
  source <- match.arg(source)

  # Determine base directory
  if (source == "remote") {
    base_dir <- get_remote_parquet_cache()
  } else {
    base_dir <- pannadata_dir()
  }

  tt_dir <- file.path(base_dir, "fbref", table_type)

  # Parquet fast paths
  if (prefer_parquet && dir.exists(tt_dir)) {

    # Case 1: league AND season specified -> read one parquet
    if (!is.null(league) && !is.null(season)) {
      parquet_path <- file.path(tt_dir, league, paste0(season, ".parquet"))
      if (file.exists(parquet_path)) {
        return(arrow::read_parquet(parquet_path))
      }
    }

    # Case 2: league only -> read all seasons for that league
    if (!is.null(league) && is.null(season)) {
      league_dir <- file.path(tt_dir, league)
      if (dir.exists(league_dir)) {
        parquet_files <- list.files(league_dir, pattern = "\\.parquet$",
                                    full.names = TRUE)
        if (length(parquet_files) > 0) {
          all_data <- lapply(parquet_files, arrow::read_parquet)
          return(rbindlist(all_data, use.names = TRUE, fill = TRUE))
        }
      }
    }

    # Case 3: season only -> read all leagues for that season
    if (is.null(league) && !is.null(season)) {
      parquet_file <- paste0(season, ".parquet")
      leagues <- list.dirs(tt_dir, recursive = FALSE, full.names = FALSE)
      parquet_files <- file.path(tt_dir, leagues, parquet_file)
      parquet_files <- parquet_files[file.exists(parquet_files)]
      if (length(parquet_files) > 0) {
        all_data <- lapply(parquet_files, arrow::read_parquet)
        return(rbindlist(all_data, use.names = TRUE, fill = TRUE))
      }
    }

    # Case 4: no filters -> read all parquet files
    if (is.null(league) && is.null(season)) {
      parquet_files <- list.files(tt_dir, pattern = "\\.parquet$",
                                  recursive = TRUE, full.names = TRUE)
      if (length(parquet_files) > 0) {
        all_data <- lapply(parquet_files, arrow::read_parquet)
        return(rbindlist(all_data, use.names = TRUE, fill = TRUE))
      }
    }
  }

  # Fallback: read from RDS files (original behavior)
  cache_dir <- get_fbref_match_cache_dir(table_type, create = FALSE)

  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  # List matching files
  cached <- list_cached_matches(table_type, league, season)

  if (nrow(cached) == 0) {
    return(NULL)
  }

  # Load and combine
  all_data <- lapply(seq_len(nrow(cached)), function(i) {
    load_match_table(
      cached$league[i],
      cached$season[i],
      cached$fbref_id[i],
      table_type
    )
  })

  all_data <- all_data[!sapply(all_data, is.null)]

  if (length(all_data) == 0) {
    return(NULL)
  }

  rbindlist(all_data, use.names = TRUE, fill = TRUE)
}


# ============================================================================
# Parquet Building Functions
# ============================================================================

#' Get parquet file path for a league-season
#'
#' Returns the path where parquet files are stored:
#' \code{{pannadata_dir}/{table_type}/{league}/{season}.parquet}
#'
#' @param table_type Table type (e.g., "summary", "events")
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2024-2025")
#' @param create If TRUE, create parent directory if missing
#'
#' @return Path to parquet file
#' @keywords internal
get_parquet_path <- function(table_type, league, season, create = FALSE) {
  base_dir <- pannadata_dir()
  parquet_dir <- file.path(base_dir, "fbref", table_type, league)

  if (create && !dir.exists(parquet_dir)) {
    dir.create(parquet_dir, recursive = TRUE)
  }

  file.path(parquet_dir, paste0(season, ".parquet"))
}


#' Build parquet file from RDS files for a league-season
#'
#' Reads all RDS files for a table_type/league/season and combines them
#' into a single parquet file. If a parquet file already exists, new RDS
#' data is merged with existing parquet data (for incremental updates).
#'
#' @param table_type Table type (e.g., "summary", "events")
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2024-2025")
#' @param verbose Print progress messages
#'
#' @return Path to created parquet file, or NULL if no data
#' @export
build_parquet <- function(table_type, league, season, verbose = TRUE) {
  .check_suggests("arrow", "Building parquet files requires arrow.")
  if (verbose) message(sprintf("  %s/%s:", league, season))

  parquet_path <- get_parquet_path(table_type, league, season, create = TRUE)

  # Load existing parquet data if it exists AND has fbref_id column
  # (parquet without fbref_id is invalid and will be rebuilt from RDS)
  existing_data <- NULL
  existing_ids <- character(0)
  if (file.exists(parquet_path)) {
    temp_data <- tryCatch({
      arrow::read_parquet(parquet_path)
    }, error = function(e) NULL)

    if (!is.null(temp_data) && "fbref_id" %in% names(temp_data)) {
      existing_data <- temp_data
      existing_ids <- unique(existing_data$fbref_id)
      if (verbose) message(sprintf("    Existing parquet: %d matches", length(existing_ids)))
    } else if (!is.null(temp_data)) {
      # Parquet exists but lacks fbref_id - will be rebuilt
      if (verbose) message("    Existing parquet missing fbref_id, rebuilding...")
    }
  }

  # Get all RDS files for this combination
  cached <- list_cached_matches(table_type, league, season)

  # Load new RDS data (only files not already in parquet)
  new_data <- NULL
  if (nrow(cached) > 0) {
    new_ids <- setdiff(cached$fbref_id, existing_ids)

    if (length(new_ids) > 0) {
      if (verbose) message(sprintf("    Loading %d new from RDS", length(new_ids)))

      new_cached <- cached[cached$fbref_id %in% new_ids, ]
      new_data_list <- lapply(seq_len(nrow(new_cached)), function(i) {
        df <- load_match_table(new_cached$league[i], new_cached$season[i],
                               new_cached$fbref_id[i], table_type)
        # Add fbref_id from filename if not already present
        if (!is.null(df) && !"fbref_id" %in% names(df)) {
          df$fbref_id <- new_cached$fbref_id[i]
        }
        df
      })
      new_data_list <- new_data_list[!sapply(new_data_list, is.null)]

      if (length(new_data_list) > 0) {
        new_data <- rbindlist(new_data_list, use.names = TRUE, fill = TRUE)
      }
    }
  }

  # Combine existing and new data
  if (is.null(existing_data) && is.null(new_data)) {
    if (verbose) message(sprintf("No data for %s/%s/%s", table_type, league, season))
    return(NULL)
  }

  combined <- rbindlist(list(existing_data, new_data), use.names = TRUE, fill = TRUE)

  if (nrow(combined) == 0) {
    if (verbose) message("No valid data found")
    return(NULL)
  }

  # Write parquet
  arrow::write_parquet(combined, parquet_path)

  if (verbose) {
    size_mb <- file.size(parquet_path) / (1024 * 1024)
    n_matches <- length(unique(combined$fbref_id))
    message(sprintf("    -> %d matches, %.2f MB", n_matches, size_mb))
  }

  invisible(parquet_path)
}


#' Build all parquet files
#'
#' Iterates through all table_type/league/season combinations and creates
#' parquet files. Discovers combinations from both existing RDS files and
#' existing parquet files (for incremental updates on CI).
#'
#' @param table_types Character vector of table types to process.
#'   Default: all standard table types.
#' @param leagues Optional character vector of leagues to process.
#'   If NULL, processes all leagues found.
#' @param seasons Optional character vector of seasons to process.
#'   If NULL, processes all seasons found.
#' @param verbose Print progress messages
#'
#' @return Data frame with table_type, league, season, n_matches, size_mb columns
#' @export
build_all_parquet <- function(table_types = NULL, leagues = NULL,
                              seasons = NULL, verbose = TRUE) {
  .check_suggests("arrow", "Building parquet files requires arrow.")
  # Default table types
  if (is.null(table_types)) {
    table_types <- c("summary", "passing", "passing_types", "defense",
                     "possession", "misc", "keeper", "shots", "events", "metadata")
  }

  results <- list()
  base_dir <- pannadata_dir()

  for (tt in table_types) {
    if (verbose) message(sprintf("\nProcessing table type: %s", tt))

    tt_dir <- file.path(base_dir, tt)
    if (!dir.exists(tt_dir)) next

    # Get leagues from both RDS subdirs and parquet files
    available_leagues <- if (!is.null(leagues)) {
      leagues
    } else {
      unique(list.dirs(tt_dir, recursive = FALSE, full.names = FALSE))
    }

    for (lg in available_leagues) {
      league_dir <- file.path(tt_dir, lg)
      parquet_league_dir <- file.path(base_dir, "fbref", tt, lg)
      if (!dir.exists(league_dir) && !dir.exists(parquet_league_dir)) next

      # Get seasons from both RDS subdirs and parquet files (parquet now in fbref/)
      rds_seasons <- if (dir.exists(league_dir)) {
        list.dirs(league_dir, recursive = FALSE, full.names = FALSE)
      } else character(0)
      parquet_files <- if (dir.exists(parquet_league_dir)) {
        list.files(parquet_league_dir, pattern = "\\.parquet$")
      } else character(0)
      parquet_seasons <- gsub("\\.parquet$", "", parquet_files)

      available_seasons <- if (!is.null(seasons)) {
        seasons
      } else {
        unique(c(rds_seasons, parquet_seasons))
      }

      for (sn in available_seasons) {
        # Check if this season has RDS files OR existing parquet
        season_dir <- file.path(league_dir, sn)
        has_rds <- dir.exists(season_dir) &&
                   length(list.files(season_dir, pattern = "\\.rds$")) > 0
        has_parquet <- file.exists(get_parquet_path(tt, lg, sn))

        if (!has_rds && !has_parquet) next

        parquet_path <- tryCatch({
          build_parquet(tt, lg, sn, verbose = verbose)
        }, error = function(e) {
          if (verbose) cli::cli_warn("Error building {tt}/{lg}/{sn}: {conditionMessage(e)}")
          NULL
        })

        if (!is.null(parquet_path) && file.exists(parquet_path)) {
          # Count matches from parquet
          n_matches <- tryCatch({
            df <- arrow::read_parquet(parquet_path, col_select = "fbref_id")
            length(unique(df$fbref_id))
          }, error = function(e) 0)

          results[[length(results) + 1]] <- data.frame(
            table_type = tt,
            league = lg,
            season = sn,
            n_matches = n_matches,
            size_mb = round(file.size(parquet_path) / (1024 * 1024), 2),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      table_type = character(0),
      league = character(0),
      season = character(0),
      n_matches = integer(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result_df <- do.call(rbind, results)

  if (verbose) {
    total_mb <- sum(result_df$size_mb)
    message(sprintf("\nBuilt %d parquet files (%.1f MB total)",
                    nrow(result_df), total_mb))
  }

  result_df
}


#' Build Consolidated Parquet Files for Remote Queries
#'
#' Creates a single parquet file per table type containing ALL leagues and seasons.
#' These consolidated files are uploaded to GitHub releases for fast remote querying
#' (like bouncer's approach). Users can then filter by league/season in SQL.
#'
#' @param table_types Character vector of table types to consolidate.
#'   Defaults to all FBref table types.
#' @param output_dir Directory to write consolidated parquet files.
#'   Defaults to pannadata_dir()/consolidated.
#' @param verbose Print progress messages.
#'
#' @return Data frame with table_type, n_rows, size_mb columns.
#'
#' @details
#' This function reads all the individual league/season parquet files for each
#' table type and combines them into a single file. The resulting files can be
#' uploaded directly to GitHub releases as individual assets, enabling fast
#' remote queries via \code{query_remote_parquet()}.
#'
#' Output files are named \code{{table_type}.parquet} (e.g., \code{summary.parquet}).
#'
#' @export
#' @examples
#' \dontrun{
#' # Build all consolidated parquets
#' build_consolidated_parquet()
#'
#' # Build only summary and events
#' build_consolidated_parquet(table_types = c("summary", "events"))
#' }
build_consolidated_parquet <- function(table_types = NULL, output_dir = NULL,
                                        verbose = TRUE) {
  .check_suggests("arrow", "Building parquet files requires arrow.")

  # Default table types
  if (is.null(table_types)) {
    table_types <- c("summary", "passing", "passing_types", "defense",
                     "possession", "misc", "keeper", "shots", "events", "metadata")
  }


  # Output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(pannadata_dir(), "consolidated")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  base_dir <- pannadata_dir()
  results <- list()

  for (tt in table_types) {
    if (verbose) message(sprintf("\nConsolidating %s...", tt))

    # Find all parquet files for this table type
    tt_dir <- file.path(base_dir, "fbref", tt)
    if (!dir.exists(tt_dir)) {
      if (verbose) message(sprintf("  Skipping %s - directory not found", tt))
      next
    }

    parquet_files <- list.files(tt_dir, pattern = "\\.parquet$",
                                 recursive = TRUE, full.names = TRUE)

    if (length(parquet_files) == 0) {
      if (verbose) message(sprintf("  Skipping %s - no parquet files found", tt))
      next
    }

    if (verbose) message(sprintf("  Found %d parquet files", length(parquet_files)))

    # Read and combine all parquet files
    all_data <- tryCatch({
      dfs <- lapply(parquet_files, function(f) {
        tryCatch({
          arrow::read_parquet(f)
        }, error = function(e) {
          if (verbose) cli::cli_warn("Error reading {basename(f)}: {conditionMessage(e)}")
          NULL
        })
      })
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0) return(NULL)
      # Use rbindlist with fill=TRUE to handle different column schemas
      as.data.frame(data.table::rbindlist(dfs, fill = TRUE))
    }, error = function(e) {
      if (verbose) cli::cli_warn("Error combining {tt}: {conditionMessage(e)}")
      NULL
    })

    if (is.null(all_data) || nrow(all_data) == 0) {
      if (verbose) message(sprintf("  Skipping %s - no data after combining", tt))
      next
    }

    # Write consolidated parquet
    output_path <- file.path(output_dir, paste0(tt, ".parquet"))
    arrow::write_parquet(all_data, output_path)

    size_mb <- round(file.size(output_path) / (1024 * 1024), 2)
    if (verbose) {
      message(sprintf("  Wrote %s: %s rows, %.1f MB",
                      basename(output_path),
                      format(nrow(all_data), big.mark = ","),
                      size_mb))
    }

    results[[length(results) + 1]] <- data.frame(
      table_type = tt,
      n_rows = nrow(all_data),
      size_mb = size_mb,
      stringsAsFactors = FALSE
    )
  }

  if (length(results) == 0) {
    return(data.frame(
      table_type = character(0),
      n_rows = integer(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result_df <- do.call(rbind, results)

  if (verbose) {
    total_mb <- sum(result_df$size_mb)
    message(sprintf("\nCreated %d consolidated parquet files (%.1f MB total)",
                    nrow(result_df), total_mb))
    message(sprintf("Output directory: %s", output_dir))
  }

  result_df
}


#' Upload Consolidated Parquet Files to GitHub Releases
#'
#' Uploads individual consolidated parquet files to a GitHub release.
#' This enables fast remote queries via \code{query_remote_parquet()}.
#'
#' @param source_dir Directory containing consolidated parquet files.
#'   Defaults to pannadata_dir()/consolidated.
#' @param repo GitHub repository in "owner/repo" format.
#' @param tag Release tag to upload to.
#' @param verbose Print progress messages.
#'
#' @return Invisible data frame with upload results.
#'
#' @export
#' @examples
#' \dontrun{
#' # Build and upload consolidated parquets
#' build_consolidated_parquet()
#' pb_upload_consolidated()
#' }
pb_upload_consolidated <- function(source_dir = NULL,
                                    repo = "peteowen1/pannadata",
                                    tag = "fbref-latest",
                                    verbose = TRUE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  if (is.null(source_dir)) {
    source_dir <- file.path(pannadata_dir(), "consolidated")
  }

  if (!dir.exists(source_dir)) {
    cli::cli_abort(c(
      "Consolidated directory not found: {.path {source_dir}}",
      "i" = "Run {.code build_consolidated_parquet()} first."
    ))
  }

  parquet_files <- list.files(source_dir, pattern = "\\.parquet$", full.names = TRUE)

  if (length(parquet_files) == 0) {
    cli::cli_abort("No parquet files found in {.val {source_dir}}")
  }

  if (verbose) {
    total_size <- sum(file.size(parquet_files)) / (1024 * 1024)
    message(sprintf("Found %d consolidated parquet files (%.1f MB total)",
                    length(parquet_files), total_size))
  }

  # Ensure release exists
  tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
    if (verbose) message("Release exists: ", tag)
  }, error = function(e) {
    if (verbose) message("Creating new release: ", tag)
    piggyback::pb_new_release(repo = repo, tag = tag)
    Sys.sleep(3)
  })

  # Upload each parquet file individually
  results <- list()
  for (pf in parquet_files) {
    fname <- basename(pf)
    if (verbose) message(sprintf("Uploading %s...", fname))

    tryCatch({
      piggyback::pb_upload(
        file = pf,
        repo = repo,
        tag = tag,
        name = fname,
        overwrite = TRUE
      )
      results[[length(results) + 1]] <- data.frame(
        file = fname,
        size_mb = round(file.size(pf) / (1024 * 1024), 2),
        status = "success",
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      if (verbose) cli::cli_warn("Failed to upload {fname}: {conditionMessage(e)}")
      results[[length(results) + 1]] <- data.frame(
        file = fname,
        size_mb = round(file.size(pf) / (1024 * 1024), 2),
        status = paste("error:", e$message),
        stringsAsFactors = FALSE
      )
    })
  }

  result_df <- do.call(rbind, results)
  if (verbose) {
    n_success <- sum(result_df$status == "success")
    message(sprintf("\nUploaded %d/%d files successfully", n_success, nrow(result_df)))
  }

  invisible(result_df)
}


# ============================================================================
# Convenience Loaders
# ============================================================================

#' Load summary data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) downloads from GitHub releases using DuckDB,
#'   "local" reads from local pannadata_dir().
#' @return Data frame of player summary stats or NULL
#' @examples
#' \dontrun{
#' # Load from GitHub releases (default, efficient with SQL filtering)
#' load_summary("ENG", "2024-2025")
#' load_summary("ENG")
#' load_summary(season = "2024-2025")
#' load_summary()
#'
#' # Load from local files
#' load_summary("ENG", "2024-2025", source = "local")
#' }
#' @export
load_summary <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("summary", league, season, source)
}

#' Load events data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of match events or NULL
#' @export
load_events <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("events", league, season, source)
}

#' Load shooting data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of shots or NULL
#' @export
load_shots <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("shots", league, season, source)
}

#' Load metadata from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of match metadata or NULL
#' @export
load_metadata <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("metadata", league, season, source)
}

#' Load passing data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of passing stats or NULL
#' @export
load_passing <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("passing", league, season, source)
}

#' Load defense data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of defensive stats or NULL
#' @export
load_defense <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("defense", league, season, source)
}

#' Load possession data from pannadata
#'
#' @param league League code (e.g., "ENG"). NULL for all leagues.
#' @param season Season string (e.g., "2023-2024"). NULL for all seasons.
#' @param source "remote" (default) or "local".
#' @return Data frame of possession stats or NULL
#' @export
load_possession <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("possession", league, season, source)
}


# ============================================================================
# Migration Utilities
# ============================================================================

#' Migrate old metadata files to include tables_available field
#'
#' Updates metadata files that were created before the tables_available
#' field was added. Checks which table types actually exist in the cache
#' and updates the metadata accordingly.
#'
#' Uses hierarchical path: \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param league Optional league filter
#' @param season Optional season filter
#' @param verbose Print progress (default TRUE)
#'
#' @return Number of metadata files updated
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Migrate all old metadata
#' migrate_metadata_tables_available()
#'
#' # Migrate only ENG 2017-2018
#' migrate_metadata_tables_available(league = "ENG", season = "2017-2018")
#' }
migrate_metadata_tables_available <- function(league = NULL, season = NULL,
                                               verbose = TRUE) {
  # All possible table types (excluding metadata itself)
  all_table_types <- c("summary", "passing", "passing_types", "defense",
                       "possession", "misc", "keeper", "shots", "events")

  # List all metadata files
  meta_cached <- list_cached_matches("metadata", league, season)

  if (nrow(meta_cached) == 0) {
    if (verbose) message("No metadata files found to migrate")
    return(0)
  }

  n_updated <- 0
  n_skipped <- 0

  for (i in seq_len(nrow(meta_cached))) {
    lg <- meta_cached$league[i]
    sn <- meta_cached$season[i]
    fbref_id <- meta_cached$fbref_id[i]

    # Load metadata
    meta <- load_match_table(lg, sn, fbref_id, "metadata")

    if (is.null(meta)) next

    # Check if already has tables_available
    if ("tables_available" %in% names(meta) && !is.na(meta$tables_available[1])) {
      n_skipped <- n_skipped + 1
      next
    }

    # Check which tables exist for this match (hierarchical path)
    available <- character(0)
    filename <- make_match_filename(fbref_id)

    for (tt in all_table_types) {
      tt_dir <- get_fbref_match_cache_dir(tt, lg, sn, create = FALSE)
      if (dir.exists(tt_dir) && file.exists(file.path(tt_dir, filename))) {
        available <- c(available, tt)
      }
    }

    # Add metadata itself to the list
    available <- c("metadata", available)

    # Update and save
    meta$tables_available <- paste(available, collapse = ",")
    save_match_table(meta, lg, sn, fbref_id, "metadata")
    n_updated <- n_updated + 1

    if (verbose && (n_updated %% 100 == 0)) {
      progress_msg(sprintf("Migrated %d metadata files...", n_updated))
    }
  }

  if (verbose) {
    progress_msg(sprintf("Migration complete: %d updated, %d already current",
                         n_updated, n_skipped))
  }

  n_updated
}
