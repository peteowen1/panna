# Data Loader Functions
#
# User-facing functions for loading panna data from local parquet files or remote
# GitHub releases. Uses DuckDB for both - local queries the parquet directory,
# remote downloads individual parquet files and runs SQL queries on them.
#
# This is the RECOMMENDED way to load data - filters happen at query time via SQL,
# avoiding loading entire datasets into R memory.
#
# All functions use source = "remote" (default) or "local".

#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom tools file_path_sans_ext
NULL

# Session-level cache for remote release info
.panna_remote_cache <- new.env(parent = emptyenv())


#' Query Remote Parquet with SQL (Primary Method)
#'
#' Downloads a parquet file from GitHub releases to a temp file, then runs
#' a SQL query on it using DuckDB. This is much faster than downloading
#' entire ZIP files because:
#' \enumerate{
#'   \item Download is done once with optimized HTTP
#'   \item DuckDB queries local file (no network latency per query)
#'   \item Only filtered/aggregated results come into R memory
#' }
#'
#' @param table_name Character. Name of the parquet file (without .parquet).
#' @param sql_template Character. SQL query template with \code{\{table\}} placeholder.
#' @param release Optional. Release info from \code{get_latest_release()}.
#' @param tag Character. GitHub release tag to download from. Defaults to "fbref-latest".
#'
#' @return Data frame with query results.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all summary data
#' sql <- "SELECT * FROM {table}"
#' result <- query_remote_parquet("summary", sql)
#'
#' # Load filtered data (filtering happens in SQL, not R!)
#' sql <- "SELECT * FROM {table} WHERE league = 'ENG' AND season = '2023-2024'"
#' eng_2023 <- query_remote_parquet("summary", sql)
#' }
query_remote_parquet <- function(table_name, sql_template, release = NULL,
                                  tag = "fbref-latest") {
  # Get release info (use cache, keyed by tag)
  cache_key <- paste0("release_", tag)

  if (is.null(release)) {
    if (!exists(cache_key, envir = .panna_remote_cache)) {
      cli::cli_alert_info("Finding GitHub release: {tag}...")
      release <- get_latest_release(tag = tag)
      assign(cache_key, release, envir = .panna_remote_cache)
    } else {
      release <- get(cache_key, envir = .panna_remote_cache)
    }
  }

  # Build URL
  parquet_url <- sprintf(
    "https://github.com/peteowen1/pannadata/releases/download/%s/%s.parquet",
    release$tag_name, table_name
  )

  # Download to temp file
  temp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(temp_file), add = TRUE)

  cli::cli_alert_info("Downloading {table_name}.parquet...")

  httr2::request(parquet_url) |>
    httr2::req_timeout(300) |>
    httr2::req_perform(path = temp_file)

  # Normalize path for DuckDB (use forward slashes on all platforms)
  temp_file_normalized <- normalizePath(temp_file, winslash = "/", mustWork = TRUE)

  # Run SQL query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  # Replace {table} placeholder with actual file path
  sql <- gsub("\\{table\\}", sprintf("'%s'", temp_file_normalized), sql_template)

  result <- DBI::dbGetQuery(conn, sql)

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows")
  result
}


#' Query Local Parquet with SQL
#'
#' Queries local parquet files using DuckDB SQL. This enables efficient filtering
#' without loading entire datasets into R memory.
#'
#' @param table_type Table type (e.g., "summary", "events", "shots")
#' @param sql_template SQL query template with \code{\{table\}} placeholder.
#'   The placeholder will be replaced with the appropriate file pattern.
#' @param league Optional league filter to narrow the parquet files searched.
#' @param season Optional season filter to narrow the parquet files searched.
#'
#' @return Data frame with query results.
#'
#' @export
#' @examples
#' \dontrun{
#' # Query summary data for England
#' sql <- "SELECT * FROM {table} WHERE league = 'ENG'"
#' eng_data <- query_local_parquet("summary", sql, league = "ENG")
#' }
query_local_parquet <- function(table_type, sql_template, league = NULL, season = NULL) {
  base_dir <- pannadata_dir()
  tt_dir <- file.path(base_dir, "fbref", table_type)

  if (!dir.exists(tt_dir)) {
    cli::cli_abort("Table type directory not found: {tt_dir}")
  }

  # Determine which parquet files to query
  if (!is.null(league) && !is.null(season)) {
    # Specific file
    parquet_path <- file.path(tt_dir, league, paste0(season, ".parquet"))
    if (!file.exists(parquet_path)) {
      cli::cli_abort("Parquet file not found: {parquet_path}")
    }
    parquet_pattern <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s'", parquet_pattern)
  } else if (!is.null(league)) {
    # All seasons for a league
    league_dir <- file.path(tt_dir, league)
    if (!dir.exists(league_dir)) {
      cli::cli_abort("League directory not found: {league_dir}")
    }
    parquet_pattern <- normalizePath(league_dir, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s/*.parquet'", parquet_pattern)
  } else {
    # All data - use glob pattern
    parquet_pattern <- normalizePath(tt_dir, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s/**/*.parquet'", parquet_pattern)
  }

  # Run SQL query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  # Replace {table} placeholder with actual file pattern
  sql <- gsub("\\{table\\}", parquet_pattern, sql_template)

  result <- DBI::dbGetQuery(conn, sql)

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows from local parquet")
  result
}


#' Get Release Info
#'
#' Fetches information about a GitHub release for pannadata.
#'
#' @param repo GitHub repository in "owner/repo" format.
#' @param tag Release tag to fetch. Use "fbref-latest", "understat-latest",
#'   "opta-latest", or NULL for the most recent release.
#'
#' @return List with release information (tag_name, assets, etc.).
#'
#' @export
#' @examples
#' \dontrun{
#' # Get FBref release
#' release <- get_latest_release(tag = "fbref-latest")
#' print(release$tag_name)
#'
#' # Get most recent release
#' release <- get_latest_release()
#' }
get_latest_release <- function(repo = "peteowen1/pannadata", tag = NULL) {
  if (is.null(tag)) {
    url <- sprintf("https://api.github.com/repos/%s/releases/latest", repo)
  } else {
    url <- sprintf("https://api.github.com/repos/%s/releases/tags/%s", repo, tag)
  }

  resp <- httr2::request(url) |>
    httr2::req_headers(Accept = "application/vnd.github.v3+json") |>
    httr2::req_user_agent("panna R package") |>
    httr2::req_timeout(30) |>
    httr2::req_perform()

  httr2::resp_body_json(resp)
}


#' Get Available Remote Tables
#'
#' Returns list of parquet files available in the GitHub release.
#' Useful for discovering what data is available for remote queries.
#'
#' @return Character vector of table names (without .parquet extension).
#'
#' @export
#' @examples
#' \dontrun{
#' # See what tables are available remotely
#' tables <- get_remote_tables()
#' print(tables)
#' }
get_remote_tables <- function() {
  if (!exists("release_info", envir = .panna_remote_cache)) {
    release <- get_latest_release()
    assign("release_info", release, envir = .panna_remote_cache)
  } else {
    release <- get("release_info", envir = .panna_remote_cache)
  }

  assets <- sapply(release$assets, function(a) a$name)
  parquets <- assets[grepl("\\.parquet$", assets)]
  tools::file_path_sans_ext(parquets)
}


#' Clear Remote Data Cache
#'
#' Clears the session-level cache of remote release info, forcing
#' the next load to re-fetch from GitHub.
#'
#' @return Invisible NULL.
#'
#' @export
#' @examples
#' \dontrun{
#' clear_remote_cache()
#' }
clear_remote_cache <- function() {
  rm(list = ls(envir = .panna_remote_cache), envir = .panna_remote_cache)
  cli::cli_alert_success("Remote data cache cleared")
  invisible(NULL)
}


# High-Level Data Loading Functions ----

#' Load Summary Data
#'
#' Loads player summary statistics from pannadata.
#' Uses DuckDB to query parquet files - filtering happens at query time.
#'
#' @param league Optional league filter (e.g., "ENG", "ESP", "GER").
#' @param season Optional season filter (e.g., "2023-2024").
#' @param source Data source: "remote" (default) downloads from GitHub,
#'   "local" uses local parquet files.
#'
#' @return Data frame of player summary statistics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all summary data from remote
#' all_summary <- load_summary()
#'
#' # Load England 2023-2024 only (efficient - filters in SQL)
#' eng_2023 <- load_summary(league = "ENG", season = "2023-2024")
#'
#' # Load from local files
#' local_data <- load_summary(source = "local")
#' }
load_summary <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("summary", league, season, source)
}


#' Load Events Data
#'
#' Loads match event data (goals, substitutions, cards, etc.) from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of match events.
#'
#' @export
load_events <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("events", league, season, source)
}


#' Load Shots Data
#'
#' Loads shot-level data with expected goals (xG) from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of shot data.
#'
#' @export
load_shots <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("shots", league, season, source)
}


#' Load Metadata
#'
#' Loads match metadata from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of match metadata.
#'
#' @export
load_metadata <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("metadata", league, season, source)
}


#' Load Passing Data
#'
#' Loads passing statistics from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of passing statistics.
#'
#' @export
load_passing <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("passing", league, season, source)
}


#' Load Defense Data
#'
#' Loads defensive statistics from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of defensive statistics.
#'
#' @export
load_defense <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("defense", league, season, source)
}


#' Load Possession Data
#'
#' Loads possession statistics from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of possession statistics.
#'
#' @export
load_possession <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("possession", league, season, source)
}


#' Load Keeper Data
#'
#' Loads goalkeeper statistics from pannadata.
#'
#' @inheritParams load_summary
#'
#' @return Data frame of goalkeeper statistics.
#'
#' @export
load_keeper <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_table_data("keeper", league, season, source)
}


#' Internal function to load table data
#'
#' Common implementation for all load_* functions.
#' For remote loading, first tries individual parquet files (fast).
#' If those don't exist (404), falls back to ZIP-based loading.
#'
#' @param table_type The type of table to load (e.g., "summary", "events")
#' @param league Optional league filter
#' @param season Optional season filter
#' @param source "remote" or "local"
#'
#' @return Data frame
#' @keywords internal
load_table_data <- function(table_type, league, season, source) {
  # Build SQL WHERE clause
  where_clauses <- character()

  if (!is.null(league)) {
    where_clauses <- c(where_clauses, sprintf("league = '%s'", league))
  }
  if (!is.null(season)) {
    where_clauses <- c(where_clauses, sprintf("season = '%s'", season))
  }

  where_sql <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }

  sql_template <- sprintf("SELECT * FROM {table} %s", where_sql)

  if (source == "remote") {
    cli::cli_alert_info("Loading {table_type} from remote...")

    # Try individual parquet first (fast path) - uses fbref-latest tag
    result <- tryCatch({
      query_remote_parquet(table_type, sql_template, tag = "fbref-latest")
    }, error = function(e) {
      # Check if it's a 404 (individual parquet not available)
      if (grepl("404|Not Found", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("Individual parquet not available, using ZIP fallback...")
        # Fall back to ZIP-based loading (aggregate_cached_matches handles this)
        tryCatch({
          aggregate_cached_matches(table_type, league, season, source = "remote")
        }, error = function(e2) {
          cli::cli_abort("Remote loading failed: {e2$message}")
        })
      } else {
        cli::cli_abort("Remote query failed: {e$message}")
      }
    })
  } else {
    cli::cli_alert_info("Loading {table_type} from local...")
    result <- tryCatch({
      query_local_parquet(table_type, sql_template, league, season)
    }, error = function(e) {
      # Fallback to legacy arrow-based loading if DuckDB fails
      cli::cli_alert_warning("DuckDB query failed, falling back to arrow: {e$message}")
      aggregate_cached_matches(table_type, league, season, source = "local")
    })
  }

  if (is.null(result) || nrow(result) == 0) {
    cli::cli_warn("No {table_type} data found for the specified filters")
    return(data.frame())
  }

  result
}


# Understat Data Loading Functions ----

#' Load Understat Roster Data
#'
#' Loads player statistics from Understat matches.
#' Includes xG, xA, xG chain, xG buildup, and other advanced metrics.
#'
#' @param league Optional league filter (e.g., "ENG", "ESP", "RUS").
#' @param season Optional season filter (e.g., "2024" or 2024).
#'   Note: Understat uses single year format, not YYYY-YYYY.
#' @param source Data source: "remote" (default) downloads from GitHub,
#'   "local" uses local parquet/RDS files.
#'
#' @return Data frame of player statistics from Understat.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all Understat roster data from remote
#' all_roster <- load_understat_roster()
#'
#' # Load England 2024 only
#' eng_2024 <- load_understat_roster(league = "ENG", season = "2024")
#'
#' # Load from local files
#' local_data <- load_understat_roster(source = "local")
#' }
load_understat_roster <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_understat_table_data("roster", league, season, source)
}


#' Load Understat Shots Data
#'
#' Loads shot-level data with xG from Understat matches.
#' Each row represents a single shot with coordinates, xG, and outcome.
#'
#' @inheritParams load_understat_roster
#'
#' @return Data frame of shot data from Understat.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all Understat shots from remote
#' all_shots <- load_understat_shots()
#'
#' # Load Spain 2023 only
#' esp_2023 <- load_understat_shots(league = "ESP", season = "2023")
#' }
load_understat_shots <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_understat_table_data("shots", league, season, source)
}


#' Load Understat Metadata
#'
#' Loads match metadata from Understat including teams, scores, and xG totals.
#'
#' @inheritParams load_understat_roster
#'
#' @return Data frame of match metadata from Understat.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all Understat metadata from remote
#' all_meta <- load_understat_metadata()
#'
#' # Load Germany 2024 only
#' ger_2024 <- load_understat_metadata(league = "GER", season = "2024")
#' }
load_understat_metadata <- function(league = NULL, season = NULL, source = c("remote", "local")) {
  source <- match.arg(source)
  load_understat_table_data("metadata", league, season, source)
}


#' Internal function to load Understat table data
#'
#' Common implementation for all load_understat_* functions.
#' Handles both remote (GitHub releases) and local (parquet/RDS) sources.
#'
#' @param table_type The type of Understat table (roster, shots, metadata)
#' @param league Optional league filter
#' @param season Optional season filter (single year format)
#' @param source "remote" or "local"
#'
#' @return Data frame
#' @keywords internal
load_understat_table_data <- function(table_type, league, season, source) {
  # Understat tables use understat/{table_type} path structure
  full_table_type <- table_type

  # Convert season to Understat format if needed (YYYY-YYYY -> YYYY)
  if (!is.null(season) && grepl("-", as.character(season))) {
    season <- substr(season, 1, 4)
  }

  # Build SQL WHERE clause
  # Use league_code and season_input columns (our input values)
  # not league/season (Understat's values like EPL/2025)
  where_clauses <- character()

  if (!is.null(league)) {
    where_clauses <- c(where_clauses, sprintf("league_code = '%s'", league))
  }
  if (!is.null(season)) {
    where_clauses <- c(where_clauses, sprintf("season_input = '%s'", season))
  }

  where_sql <- if (length(where_clauses) > 0) {
    paste("WHERE", paste(where_clauses, collapse = " AND "))
  } else {
    ""
  }

  sql_template <- sprintf("SELECT * FROM {table} %s", where_sql)

  if (source == "remote") {
    cli::cli_alert_info("Loading {full_table_type} from remote...")

    # Try individual parquet first
    result <- tryCatch({
      query_remote_understat_parquet(full_table_type, sql_template)
    }, error = function(e) {
      # Check if it's a 404 (individual parquet not available)
      if (grepl("404|Not Found", e$message, ignore.case = TRUE)) {
        cli::cli_alert_warning("Individual parquet not available, trying local fallback...")
        # Fall back to local RDS aggregation
        tryCatch({
          aggregate_understat_data(table_type, league, season)
        }, error = function(e2) {
          cli::cli_abort("Remote loading failed: {e2$message}")
        })
      } else {
        cli::cli_abort("Remote query failed: {e$message}")
      }
    })
  } else {
    cli::cli_alert_info("Loading {full_table_type} from local...")
    result <- tryCatch({
      query_local_understat_parquet(full_table_type, sql_template, league, season)
    }, error = function(e) {
      # Fallback to RDS aggregation
      cli::cli_alert_warning("Parquet query failed, falling back to RDS: {e$message}")
      aggregate_understat_data(table_type, league, season)
    })
  }

  if (is.null(result) || nrow(result) == 0) {
    cli::cli_warn("No {full_table_type} data found for the specified filters")
    return(data.frame())
  }

  result
}


#' Query Remote Understat Parquet
#'
#' Downloads an Understat parquet file from GitHub releases and queries it.
#'
#' @param table_name Full table name including understat_ prefix
#' @param sql_template SQL query template with \code{\{table\}} placeholder
#'
#' @return Data frame with query results
#' @keywords internal
query_remote_understat_parquet <- function(table_name, sql_template) {
  # Get release info for Understat tag
  release <- tryCatch({
    url <- "https://api.github.com/repos/peteowen1/pannadata/releases/tags/understat-latest"
    resp <- httr2::request(url) |>
      httr2::req_headers(Accept = "application/vnd.github.v3+json") |>
      httr2::req_user_agent("panna R package") |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  }, error = function(e) {
    stop("Failed to get Understat release info: ", e$message)
  })

  # Build URL
  parquet_url <- sprintf(
    "https://github.com/peteowen1/pannadata/releases/download/%s/%s.parquet",
    release$tag_name, table_name
  )

  # Download to temp file
  temp_file <- tempfile(fileext = ".parquet")
  on.exit(unlink(temp_file), add = TRUE)

  cli::cli_alert_info("Downloading {table_name}.parquet...")

  httr2::request(parquet_url) |>
    httr2::req_timeout(300) |>
    httr2::req_perform(path = temp_file)

  # Normalize path for DuckDB
  temp_file_normalized <- normalizePath(temp_file, winslash = "/", mustWork = TRUE)

  # Run SQL query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  sql <- gsub("\\{table\\}", sprintf("'%s'", temp_file_normalized), sql_template)

  result <- DBI::dbGetQuery(conn, sql)

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows")
  result
}


#' Query Local Understat Parquet
#'
#' Queries local Understat parquet files using DuckDB SQL.
#'
#' @param table_type Table type (metadata, roster, shots, events)
#' @param sql_template SQL query template with \code{\{table\}} placeholder
#' @param league Optional league filter
#' @param season Optional season filter
#'
#' @return Data frame with query results
#' @keywords internal
query_local_understat_parquet <- function(table_type, sql_template, league = NULL, season = NULL) {
  base_dir <- pannadata_dir()
  tt_dir <- file.path(base_dir, "understat", table_type)

  if (!dir.exists(tt_dir)) {
    cli::cli_abort("Understat table directory not found: {tt_dir}")
  }

  # Determine which parquet files to query
  if (!is.null(league) && !is.null(season)) {
    parquet_path <- file.path(tt_dir, league, paste0(season, ".parquet"))
    if (!file.exists(parquet_path)) {
      cli::cli_abort("Parquet file not found: {parquet_path}")
    }
    parquet_pattern <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s'", parquet_pattern)
  } else if (!is.null(league)) {
    league_dir <- file.path(tt_dir, league)
    if (!dir.exists(league_dir)) {
      cli::cli_abort("League directory not found: {league_dir}")
    }
    parquet_pattern <- normalizePath(league_dir, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s/*.parquet'", parquet_pattern)
  } else {
    parquet_pattern <- normalizePath(tt_dir, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s/**/*.parquet'", parquet_pattern)
  }

  # Run SQL query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  sql <- gsub("\\{table\\}", parquet_pattern, sql_template)

  result <- DBI::dbGetQuery(conn, sql)

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows from local parquet")
  result
}
