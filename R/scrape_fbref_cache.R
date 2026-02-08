# Cache management for FBref scraping
#
# Handles directory management, file naming, cache read/write operations.
# Provides hierarchical storage structure: {table_type}/{league}/{season}/{id}.rds


# ============================================================================
# Cache Management
# ============================================================================

# Environment for storing pannadata path
.panna_env <- new.env(parent = emptyenv())

#' Get or set pannadata directory
#'
#' Gets or sets the base directory for parquet/RDS data storage.
#'
#' Resolution order (first match wins):
#' \enumerate{
#'   \item Explicitly set via \code{pannadata_dir("path")}
#'   \item \code{PANNADATA_DIR} environment variable
#'   \item \code{../pannadata/data} if it exists (pannaverse developers)
#'   \item R's standard user data directory via \code{tools::R_user_dir("panna", "data")}
#' }
#'
#' The default (\code{R_user_dir}) gives OS-appropriate persistent storage:
#' \itemize{
#'   \item Windows: \code{C:/Users/you/AppData/Local/R/panna/data}
#'   \item Mac: \code{~/Library/Application Support/org.R-project.R/panna/data}
#'   \item Linux: \code{~/.local/share/R/panna/data}
#' }
#'
#' @param path Optional new path to set. If NULL, returns current path.
#'
#' @return Current pannadata directory path (invisibly when setting)
#' @export
#'
#' @examples
#' # Get current path
#' pannadata_dir()
#'
#' # Set custom path
#' pannadata_dir("~/my/football/data")
pannadata_dir <- function(path = NULL) {

  if (!is.null(path)) {
    .panna_env$pannadata_dir <- normalizePath(path, mustWork = FALSE)
    return(invisible(.panna_env$pannadata_dir))
  }

  # 1. Return cached value if explicitly set
  if (exists("pannadata_dir", envir = .panna_env)) {
    return(.panna_env$pannadata_dir)
  }

  # 2. Check environment variable
  env_path <- Sys.getenv("PANNADATA_DIR", "")
  if (env_path != "") {
    return(env_path)
  }

  # 3. Check for pannaverse structure (for developers)
  # Look for ../pannadata/data relative to working directory
  pannaverse_path <- file.path(dirname(getwd()), "pannadata", "data")
  if (dir.exists(pannaverse_path)) {
    return(normalizePath(pannaverse_path))
  }

  # 4. Default: R's standard user data directory (works across sessions)
  # This gives OS-appropriate paths:
  #   Windows: C:/Users/you/AppData/Local/R/panna/data
  #   Mac: ~/Library/Application Support/org.R-project.R/panna/data
  #   Linux: ~/.local/share/R/panna/data
  tools::R_user_dir("panna", "data")
}


#' Get FBref match cache directory
#'
#' Returns path using hierarchical structure:
#' \code{\{pannadata_dir\}/\{table_type\}/\{league\}/\{season\}/}
#'
#' @param table_type Optional table type for subdirectory
#' @param league Optional league for subdirectory (new hierarchical structure)
#' @param season Optional season for subdirectory (new hierarchical structure)
#' @param create Whether to create directory if missing (default TRUE)
#'
#' @return Path to cache directory
#' @keywords internal
get_fbref_match_cache_dir <- function(table_type = NULL, league = NULL,
                                       season = NULL, create = TRUE) {
  base_dir <- pannadata_dir()

  # Build path hierarchy
  cache_dir <- base_dir

  if (!is.null(table_type)) {
    cache_dir <- file.path(cache_dir, table_type)
  }

  if (!is.null(league)) {
    cache_dir <- file.path(cache_dir, league)
  }

  if (!is.null(season)) {
    cache_dir <- file.path(cache_dir, season)
  }

  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_dir
}


#' Create match filename for caching
#'
#' With the hierarchical structure, filename is just the fbref_id.
#' League and season are encoded in the directory path.
#'
#' @param fbref_id FBref match ID (8-char hex)
#'
#' @return Filename string (without path)
#' @keywords internal
make_match_filename <- function(fbref_id) {
  # Sanitize for filename safety

  safe_id <- gsub("[^a-zA-Z0-9_-]", "_", fbref_id)
  paste0(safe_id, ".rds")
}


#' Save match table to cache
#'
#' Saves to hierarchical path: \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param data Data frame to save
#' @param league League code
#' @param season Season string
#' @param fbref_id FBref match ID
#' @param table_type Type of table
#'
#' @return Invisible path to saved file
#' @keywords internal
save_match_table <- function(data, league, season, fbref_id, table_type) {
  cache_dir <- get_fbref_match_cache_dir(table_type, league, season)
  filename <- make_match_filename(fbref_id)
  file_path <- file.path(cache_dir, filename)
  saveRDS(data, file_path)
  invisible(file_path)
}


#' Load match table from cache
#'
#' Loads from hierarchical path: \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param league League code
#' @param season Season string
#' @param fbref_id FBref match ID
#' @param table_type Type of table
#'
#' @return Data frame or NULL if not cached
#' @keywords internal
load_match_table <- function(league, season, fbref_id, table_type) {
  cache_dir <- get_fbref_match_cache_dir(table_type, league, season, create = FALSE)

  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  filename <- make_match_filename(fbref_id)
  file_path <- file.path(cache_dir, filename)

  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    NULL
  }
}


#' Get match IDs from parquet file
#'
#' Reads a parquet file and extracts unique match IDs. Results are cached
#' in memory for the session to avoid repeated file reads.
#'
#' @param table_type Table type (e.g., "metadata")
#' @param league League code
#' @param season Season string
#'
#' @return Character vector of match IDs, or NULL if no parquet
#' @keywords internal
get_parquet_fbref_ids <- function(table_type, league, season) {
  # Check cache first

  cache_key <- paste(table_type, league, season, sep = "_")
  if (exists("parquet_id_cache", envir = .panna_env)) {
    cache <- get("parquet_id_cache", envir = .panna_env)
    if (cache_key %in% names(cache)) {
      return(cache[[cache_key]])
    }
  } else {
    assign("parquet_id_cache", list(), envir = .panna_env)
  }

  # Read parquet and extract IDs
  parquet_path <- get_parquet_path(table_type, league, season)
  if (!file.exists(parquet_path)) {
    return(NULL)
  }

  ids <- tryCatch({
    df <- arrow::read_parquet(parquet_path, col_select = "fbref_id")
    unique(df$fbref_id)
  }, error = function(e) NULL)

  # Cache result
  cache <- get("parquet_id_cache", envir = .panna_env)
  cache[[cache_key]] <- ids
  assign("parquet_id_cache", cache, envir = .panna_env)

  ids
}


#' Check if match is cached
#'
#' Checks if a match has been fully scraped. First checks RDS files,
#' then falls back to checking parquet files (for CI/CD environments
#' where only parquet is available).
#'
#' @param league League code
#' @param season Season string
#' @param fbref_id FBref match ID
#' @param table_types Character vector of table types (unused, kept for compatibility)
#'
#' @return Logical - TRUE if match has been fully scraped
#' @keywords internal
is_match_cached <- function(league, season, fbref_id, table_types = "metadata") {
  # Quick check: metadata RDS file must exist (hierarchical path)
  cache_dir <- get_fbref_match_cache_dir("metadata", league, season, create = FALSE)
  filename <- make_match_filename(fbref_id)

  if (dir.exists(cache_dir)) {
    metadata_path <- file.path(cache_dir, filename)
    if (file.exists(metadata_path)) {
      # Read metadata to check tables_available
      metadata <- tryCatch(readRDS(metadata_path), error = function(e) NULL)
      if (!is.null(metadata)) {
        # If tables_available field exists, verify all those tables are cached
        if ("tables_available" %in% names(metadata) && !is.na(metadata$tables_available[1])) {
          available_tables <- strsplit(metadata$tables_available[1], ",")[[1]]
          if (length(available_tables) > 0) {
            # Check each available table exists (hierarchical path)
            all_exist <- TRUE
            for (tt in available_tables) {
              tt_dir <- get_fbref_match_cache_dir(tt, league, season, create = FALSE)
              if (!dir.exists(tt_dir) || !file.exists(file.path(tt_dir, filename))) {
                all_exist <- FALSE
                break
              }
            }
            if (all_exist) return(TRUE)
          }
        }
      }
    }
  }

  # Fallback: check if match exists in parquet files (for CI/CD)
  # Only need to check metadata parquet - if match is there, it was fully scraped
  parquet_ids <- get_parquet_fbref_ids("metadata", league, season)
  if (!is.null(parquet_ids) && fbref_id %in% parquet_ids) {
    return(TRUE)
  }

  FALSE
}


#' Get cached match IDs for a league-season (fast batch version)
#'
#' Returns all fully-cached match IDs for a league-season. A match is
#' considered cached if:
#' 1. All 9 table type files exist (fast path for Big 5 leagues), OR
#' 2. The metadata has `tables_available` field and all those tables exist
#'
#' Uses hierarchical path: \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param league League code
#' @param season Season string
#'
#' @return Character vector of cached fbref_ids
#' @keywords internal
get_cached_fbref_ids <- function(league, season) {
  # All 10 table types for a complete match (events added)
  all_table_types <- c("metadata", "summary", "passing", "passing_types",
                       "defense", "possession", "misc", "keeper", "shots", "events")

  # Check metadata dir exists (hierarchical: metadata/league/season/)
  cache_dir <- get_fbref_match_cache_dir("metadata", league, season, create = FALSE)
  if (!dir.exists(cache_dir)) return(character(0))

  # Find all metadata files in this directory (just {id}.rds files)
  files <- list.files(cache_dir, pattern = "^[a-f0-9]{8}\\.rds$", full.names = TRUE)

  if (length(files) == 0) return(character(0))

  cached_ids <- character(0)

  for (fpath in files) {
    fname <- basename(fpath)
    fbref_id <- gsub("\\.rds$", "", fname)

    # Fast path: check if all 9 table files exist
    all_nine_exist <- TRUE
    for (tt in all_table_types) {
      tt_dir <- get_fbref_match_cache_dir(tt, league, season, create = FALSE)
      if (!dir.exists(tt_dir) || !file.exists(file.path(tt_dir, fname))) {
        all_nine_exist <- FALSE
        break
      }
    }

    if (all_nine_exist) {
      # All 9 tables exist - definitely cached
      cached_ids <- c(cached_ids, fbref_id)
      next
    }

    # Slow path: check tables_available in metadata for partial matches
    metadata <- tryCatch(readRDS(fpath), error = function(e) NULL)
    if (is.null(metadata)) next

    if ("tables_available" %in% names(metadata) && !is.na(metadata$tables_available[1])) {
      available_tables <- strsplit(metadata$tables_available[1], ",")[[1]]
      if (length(available_tables) == 0) next

      # Verify all expected tables exist (hierarchical path)
      all_exist <- TRUE
      for (tt in available_tables) {
        tt_dir <- get_fbref_match_cache_dir(tt, league, season, create = FALSE)
        if (!dir.exists(tt_dir) || !file.exists(file.path(tt_dir, fname))) {
          all_exist <- FALSE
          break
        }
      }
      if (all_exist) {
        cached_ids <- c(cached_ids, fbref_id)
      }
    }
  }

  cached_ids
}


#' List cached matches
#'
#' Scans the hierarchical directory structure:
#' \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param table_type Table type to check
#' @param league Optional league filter
#' @param season Optional season filter
#'
#' @return Data frame with league, season, fbref_id columns
#' @keywords internal
list_cached_matches <- function(table_type = "metadata", league = NULL,
                                 season = NULL) {
  base_dir <- get_fbref_match_cache_dir(table_type, create = FALSE)

  if (!dir.exists(base_dir)) {
    return(data.frame(
      league = character(0),
      season = character(0),
      fbref_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # If league and season are both specified, go directly to that directory
  if (!is.null(league) && !is.null(season)) {
    cache_dir <- file.path(base_dir, league, season)
    if (!dir.exists(cache_dir)) {
      return(data.frame(
        league = character(0),
        season = character(0),
        fbref_id = character(0),
        stringsAsFactors = FALSE
      ))
    }
    files <- list.files(cache_dir, pattern = "^[a-f0-9]{8}\\.rds$")
    if (length(files) == 0) {
      return(data.frame(
        league = character(0),
        season = character(0),
        fbref_id = character(0),
        stringsAsFactors = FALSE
      ))
    }
    return(data.frame(
      league = rep(league, length(files)),
      season = rep(season, length(files)),
      fbref_id = gsub("\\.rds$", "", files),
      stringsAsFactors = FALSE
    ))
  }

  # Get list of leagues to scan
  if (!is.null(league)) {
    leagues_to_scan <- league
  } else {
    leagues_to_scan <- list.dirs(base_dir, recursive = FALSE, full.names = FALSE)
  }

  if (length(leagues_to_scan) == 0) {
    return(data.frame(
      league = character(0),
      season = character(0),
      fbref_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # Collect results
  all_results <- list()

  for (lg in leagues_to_scan) {
    league_dir <- file.path(base_dir, lg)
    if (!dir.exists(league_dir)) next

    # Get seasons for this league
    if (!is.null(season)) {
      seasons_to_scan <- season
    } else {
      seasons_to_scan <- list.dirs(league_dir, recursive = FALSE, full.names = FALSE)
    }

    for (sn in seasons_to_scan) {
      season_dir <- file.path(league_dir, sn)
      if (!dir.exists(season_dir)) next

      files <- list.files(season_dir, pattern = "^[a-f0-9]{8}\\.rds$")
      if (length(files) > 0) {
        all_results[[length(all_results) + 1]] <- data.frame(
          league = rep(lg, length(files)),
          season = rep(sn, length(files)),
          fbref_id = gsub("\\.rds$", "", files),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(all_results) == 0) {
    return(data.frame(
      league = character(0),
      season = character(0),
      fbref_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, all_results)
}
