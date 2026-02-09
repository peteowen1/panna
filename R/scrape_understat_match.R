# Understat single-match scraping and cache management
#
# Handles fixture scraping, single-match scraping, and parquet cache operations.
# HTTP layer and data extraction functions are in scrape_understat_api.R.
# Batch orchestration and manifest system are in scrape_understat.R.


# ============================================================================
# Fixtures Scraping
# ============================================================================

#' Scrape fixtures from Understat league page
#'
#' Gets list of matches with Understat IDs for a league and season.
#'
#' @note As of 2024, Understat loads fixture data via JavaScript. This function
#'   will return NULL because the data is not in the initial HTML response.
#'   Use \code{scrape_understat_match_range()} as an alternative to scrape
#'   matches by ID range, or use RSelenium/chromote for JS-rendered content.
#'
#' @param league League code (e.g., "ENG", "ESP")
#' @param season Season year (e.g., 2024 or "2024")
#' @param completed_only If TRUE, only return completed matches (default TRUE)
#'
#' @return Data frame with match info including understat_id, or NULL if
#'   data cannot be extracted (expected for current Understat site structure)
#' @export
#'
#' @examples
#' \dontrun{
#' # This will likely return NULL due to JS-loaded content
#' fixtures <- scrape_understat_fixtures("ENG", 2024)
#'
#' # Alternative: use match ID range
#' results <- scrape_understat_match_range(28900, 28910, "ENG", 2024)
#' }
scrape_understat_fixtures <- function(league, season, completed_only = TRUE) {
  .check_suggests("httr", "Scraping Understat requires httr.")
  .check_suggests("rvest", "Scraping Understat requires rvest.")
  .check_suggests("jsonlite", "Scraping Understat requires jsonlite.")
  .check_suggests("stringi", "Scraping Understat requires stringi.")
  # Get league URL
  url <- get_understat_league_url(league, season)

  progress_msg(sprintf("Fetching Understat fixtures: %s %s", league, season))

  # Fetch page
  page <- fetch_understat_page(url)

  if (is.null(page)) {
    cli::cli_warn("Failed to fetch Understat league page")
    return(NULL)
  }

  # Extract datesData which contains all fixtures
  # NOTE: This data is loaded via JavaScript and may not be present
  scripts <- get_understat_scripts(page)
  dates_data <- extract_json_element(scripts, "datesData")

  if (is.null(dates_data)) {
    cli::cli_warn(c(
      "Could not extract datesData from league page.",
      "i" = "Understat loads fixtures via JavaScript.",
      "i" = "Use {.fn scrape_understat_match_range} as an alternative."
    ))
    return(NULL)
  }

  # Convert to data frame
  if (is.list(dates_data) && !is.data.frame(dates_data)) {
    fixtures <- tryCatch({
      rbindlist(lapply(dates_data, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }), use.names = TRUE, fill = TRUE)
    }, error = function(e) {
      do.call(rbind, lapply(dates_data, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
    })
  } else {
    fixtures <- as.data.frame(dates_data, stringsAsFactors = FALSE)
  }

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    cli::cli_warn("No fixtures found")
    return(NULL)
  }

  # Clean column names
  fixtures <- clean_column_names(fixtures)

  # Filter to completed matches if requested
  if (completed_only && "is_result" %in% names(fixtures)) {
    fixtures <- fixtures[fixtures$is_result == TRUE, ]
  }

  # Add league and season context
  fixtures$league <- league
  fixtures$season <- as.character(season)

  progress_msg(sprintf("  Found %d matches", nrow(fixtures)))

  fixtures
}


#' Scrape matches by Understat ID range
#'
#' Iteratively scrapes matches within a given ID range. This is useful when
#' fixture lists cannot be obtained directly (since Understat loads them via JS).
#'
#' Understat match IDs are sequential integers. Recent seasons tend to have
#' higher IDs. For example, 2024-25 Premier League matches are in the ~28000 range.
#'
#' @param start_id Starting match ID
#' @param end_id Ending match ID (inclusive)
#' @param league League code for caching (e.g., "ENG")
#' @param season Season for caching (e.g., 2024)
#' @param delay Seconds between requests (default 3)
#' @param skip_invalid If TRUE (default), silently skip invalid match IDs
#'
#' @return List of match results (metadata only, roster/shots require JS)
#' @export
#'
#' @examples
#' \dontrun{
#' # Scrape 10 matches starting from ID 28900
#' results <- scrape_understat_match_range(28900, 28909, "ENG", 2024)
#'
#' # Extract just the metadata
#' metadata <- dplyr::bind_rows(lapply(results, function(x) x$metadata))
#' }
scrape_understat_match_range <- function(start_id, end_id, league, season,
                                          delay = 3, skip_invalid = TRUE) {
  start_id <- as.integer(start_id)
  end_id <- as.integer(end_id)

  if (start_id > end_id) {
    cli::cli_abort("start_id must be <= end_id")
  }

  results <- list()
  n_total <- end_id - start_id + 1
  n_success <- 0
  n_failed <- 0

  progress_msg(sprintf("Scraping Understat matches %d to %d (%d total)",
                       start_id, end_id, n_total))

  for (match_id in start_id:end_id) {
    # Rate limiting
    if (match_id > start_id) {
      Sys.sleep(delay)
    }

    result <- tryCatch({
      scrape_understat_match(match_id, league, season, save_cache = TRUE)
    }, error = function(e) {
      if (!skip_invalid) {
        cli::cli_warn("Match {match_id}: {conditionMessage(e)}")
      }
      NULL
    })

    if (!is.null(result) && !is.null(result$metadata)) {
      n_success <- n_success + 1
      results[[as.character(match_id)]] <- result
      progress_msg(sprintf("  [%d/%d] Match %d: %s vs %s",
                           match_id - start_id + 1, n_total, match_id,
                           result$metadata$home_team, result$metadata$away_team))
    } else {
      n_failed <- n_failed + 1
      if (!skip_invalid) {
        progress_msg(sprintf("  [%d/%d] Match %d: invalid or no data",
                             match_id - start_id + 1, n_total, match_id))
      }
    }
  }

  progress_msg(sprintf("Completed: %d successful, %d failed/invalid",
                       n_success, n_failed))

  results
}


# ============================================================================
# Cache Management (Parquet-based)
# ============================================================================

#' Get Understat parquet file path
#'
#' Returns path using hierarchical structure:
#' \code{\{pannadata_dir\}/understat/\{table_type\}/\{league\}/\{season\}.parquet}
#'
#' @param table_type Table type (metadata, roster, shots, events)
#' @param league League code
#' @param season Season string
#' @param create Whether to create parent directory if missing (default TRUE)
#'
#' @return Path to parquet file
#' @keywords internal
get_understat_parquet_path <- function(table_type, league, season, create = TRUE) {
  base_dir <- pannadata_dir()

  # Use understat/{table_type} structure
  cache_dir <- file.path(base_dir, "understat", table_type, league)

  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file.path(cache_dir, paste0(as.character(season), ".parquet"))
}


#' Save Understat match table to parquet cache
#'
#' Appends match data to existing parquet file or creates new one.
#' Matches are identified by understat_id to avoid duplicates.
#' Uses temp file + rename to avoid Windows file locking issues.
#'
#' @param data Data frame to save (single match)
#' @param league League code
#' @param season Season string
#' @param table_type Type of table (metadata, roster, shots)
#'
#' @return Invisible path to saved file
#' @keywords internal
save_understat_table <- function(data, league, season, table_type) {
  if (is.null(data) || nrow(data) == 0) {
    return(invisible(NULL))
  }

  parquet_path <- get_understat_parquet_path(table_type, league, season)

  # Read existing data if parquet exists
  existing_data <- NULL
  if (file.exists(parquet_path)) {
    existing_data <- tryCatch({
      # Read into memory and immediately release file handle
      df <- as.data.frame(arrow::read_parquet(parquet_path))
      gc()  # Help release file handles
      df
    }, error = function(e) NULL)
  }

  # Combine: remove old version of this match if exists, then append new
  if (!is.null(existing_data) && nrow(existing_data) > 0) {
    # Get the understat_id from new data
    new_id <- data$understat_id[1]
    # Filter out old version of this match
    existing_data <- existing_data[existing_data$understat_id != new_id, ]
    # Combine
    combined <- rbindlist(list(existing_data, data), use.names = TRUE, fill = TRUE)
  } else {
    combined <- data
  }

  # Write to temp file first, then rename (avoids Windows file locking)
  temp_path <- paste0(parquet_path, ".tmp")
  arrow::write_parquet(combined, temp_path)

  # Remove old file and rename temp
  if (file.exists(parquet_path)) {
    tryCatch({
      file.remove(parquet_path)
    }, error = function(e) {
      # If can't remove, wait a bit and try again
      Sys.sleep(0.5)
      gc()
      file.remove(parquet_path)
    })
  }
  file.rename(temp_path, parquet_path)

  invisible(parquet_path)
}


#' Load Understat data from parquet cache
#'
#' @param league League code
#' @param season Season string
#' @param understat_id Optional specific match ID to load
#' @param table_type Type of table
#'
#' @return Data frame or NULL if not cached
#' @keywords internal
load_understat_table <- function(league, season, understat_id = NULL, table_type) {
  parquet_path <- get_understat_parquet_path(table_type, league, season, create = FALSE)

  if (!file.exists(parquet_path)) {
    return(NULL)
  }

  data <- tryCatch({
    arrow::read_parquet(parquet_path)
  }, error = function(e) NULL)

  if (is.null(data)) {
    return(NULL)
  }

  # Filter to specific match if requested

  if (!is.null(understat_id)) {
    data <- data[data$understat_id == as.character(understat_id), ]
    if (nrow(data) == 0) {
      return(NULL)
    }
  }

  data
}


#' Check if Understat match is cached
#'
#' @param league League code
#' @param season Season string
#' @param understat_id Understat match ID
#'
#' @return Logical - TRUE if match has been scraped
#' @keywords internal
is_understat_cached <- function(league, season, understat_id) {
  parquet_path <- get_understat_parquet_path("metadata", league, season, create = FALSE)

  if (!file.exists(parquet_path)) {
    return(FALSE)
  }

  # Check if this specific match ID is in the parquet
  data <- tryCatch({
    arrow::read_parquet(parquet_path)
  }, error = function(e) NULL)

  if (is.null(data)) {
    return(FALSE)
  }

  as.character(understat_id) %in% as.character(data$understat_id)
}


#' Get cached Understat match IDs
#'
#' @param league League code
#' @param season Season string
#'
#' @return Character vector of cached understat_ids
#' @keywords internal
get_cached_understat_ids <- function(league, season) {
  parquet_path <- get_understat_parquet_path("metadata", league, season, create = FALSE)

  if (!file.exists(parquet_path)) {
    return(character(0))
  }

  data <- tryCatch({
    arrow::read_parquet(parquet_path)
  }, error = function(e) NULL)

  if (is.null(data) || !"understat_id" %in% names(data)) {
    return(character(0))
  }

  as.character(unique(data$understat_id))
}


# ============================================================================
# Main Scraping Functions
# ============================================================================

#' Scrape single Understat match
#'
#' Fetches and parses match data from Understat. Uses HTML page for metadata
#' and events, and the API endpoint for roster and shots data.
#'
#' @param understat_id Understat match ID
#' @param league League code (for caching)
#' @param season Season (for caching)
#' @param save_cache Whether to save to cache (default TRUE)
#'
#' @return List with metadata, events, roster, and shots data frames
#' @export
#'
#' @examples
#' \dontrun{
#' match_data <- scrape_understat_match(28988, "ENG", 2024)
#' print(match_data$metadata)   # Match info, xG totals
#' print(match_data$events)     # Goals, subs, cards timeline
#' print(match_data$roster)     # Player stats (xG, xA, etc.)
#' print(match_data$shots)      # Shot-level xG
#' }
scrape_understat_match <- function(understat_id, league, season, save_cache = TRUE) {
  .check_suggests("httr", "Scraping Understat requires httr.")
  .check_suggests("rvest", "Scraping Understat requires rvest.")
  .check_suggests("jsonlite", "Scraping Understat requires jsonlite.")
  .check_suggests("stringi", "Scraping Understat requires stringi.")
  url <- get_understat_match_url(understat_id)

  # Fetch HTML page for metadata and events
  page <- fetch_understat_page(url)

  if (is.null(page)) {
    return(NULL)
  }

  # Extract metadata and events from HTML (always available)
  metadata <- extract_understat_metadata(page, understat_id)
  events <- extract_understat_events(page, understat_id)

  # Fetch API endpoint for roster and shots data
  api_data <- fetch_understat_match_api(understat_id)

  # Extract roster and shots from API response
  roster <- extract_understat_roster(api_data, understat_id)
  shots <- extract_understat_shots(api_data, understat_id)

  # Add league/season context to all data frames
  if (!is.null(metadata)) {
    metadata$league_code <- league
    metadata$season_input <- as.character(season)
  }
  if (!is.null(events)) {
    events$league_code <- league
    events$season_input <- as.character(season)
  }
  if (!is.null(roster)) {
    roster$league_code <- league
    roster$season_input <- as.character(season)
  }
  if (!is.null(shots)) {
    shots$league_code <- league
    shots$season_input <- as.character(season)
  }

  # Save to cache (parquet format)
  if (save_cache && !is.null(metadata)) {
    save_understat_table(metadata, league, season, "metadata")

    if (!is.null(events)) {
      save_understat_table(events, league, season, "events")
    }
    if (!is.null(roster)) {
      save_understat_table(roster, league, season, "roster")
    }
    if (!is.null(shots)) {
      save_understat_table(shots, league, season, "shots")
    }
  }

  list(
    metadata = metadata,
    events = events,
    roster = roster,
    shots = shots
  )
}
