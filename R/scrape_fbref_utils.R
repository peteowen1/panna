# FBref scraping utilities
#
# HTTP layer, cache management, and data utilities for FBref scraping.
# Used by scrape_fbref_batch.R and scrape_fbref_parquet.R.


# ============================================================================
# HTTP Layer
# ============================================================================

#' Get browser headers for FBref requests
#'
#' Returns headers that mimic a real browser to avoid Cloudflare blocking.
#' Rotates through different User-Agent strings for safety.
#'
#' @param referer Optional referer URL (default: FBref competitions page)
#'
#' @return Named character vector of HTTP headers
#' @keywords internal
get_fbref_headers <- function(referer = "https://fbref.com/en/comps/") {
  # Pool of realistic User-Agent strings (recent Chrome/Firefox/Edge on Windows/Mac)
  user_agents <- c(
    # Chrome on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
    # Chrome on Mac
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
    # Firefox on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:132.0) Gecko/20100101 Firefox/132.0",
    # Firefox on Mac
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0",
    # Edge on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36 Edg/131.0.0.0"
  )

  # Pick a random User-Agent
  ua <- sample(user_agents, 1)

  # Extract browser version for sec-ch-ua header
  chrome_version <- regmatches(ua, regexpr("Chrome/([0-9]+)", ua))
  chrome_version <- gsub("Chrome/", "", chrome_version)
  if (length(chrome_version) == 0 || chrome_version == "") {
    chrome_version <- "131"
  }

  # Determine platform from UA
  platform <- if (grepl("Macintosh", ua)) '"macOS"' else '"Windows"'

  c(
    "User-Agent" = ua,
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    "Accept-Language" = sample(c("en-US,en;q=0.9", "en-GB,en;q=0.9,en-US;q=0.8", "en;q=0.9"), 1),
    "Accept-Encoding" = "gzip, deflate, br",
    "sec-ch-ua" = sprintf('"Chromium";v="%s", "Not?A_Brand";v="99"', chrome_version),
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = platform,
    "sec-fetch-dest" = "document",
    "sec-fetch-mode" = "navigate",
    "sec-fetch-site" = "same-origin",
    "sec-fetch-user" = "?1",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = referer,
    "DNT" = "1"
  )
}


#' Add random jitter to delay
#'
#' Returns the base delay plus random jitter to appear more human.
#'
#' @param base_delay Base delay in seconds
#' @param jitter_pct Percentage of jitter (default 0.3 = +/- 30%)
#'
#' @return Delay in seconds with jitter applied
#' @keywords internal
add_delay_jitter <- function(base_delay, jitter_pct = 0.3) {
  jitter <- base_delay * jitter_pct
  base_delay + stats::runif(1, -jitter, jitter)
}


# Environment for session storage (avoids namespace locking issues)
.fbref_env <- new.env(parent = emptyenv())

#' Get or create FBref session
#'
#' Returns a persistent httr session handle that maintains cookies
#' across requests, like a real browser session.
#'
#' @param reset If TRUE, creates a new session (default FALSE)
#'
#' @return httr handle object
#' @keywords internal
get_fbref_session <- function(reset = FALSE) {
  if (!exists("session", envir = .fbref_env) || reset) {
    .fbref_env$session <- httr::handle("https://fbref.com")
  }
  .fbref_env$session
}

#' Reset FBref session
#'
#' Clears cookies and creates fresh session. Use after changing VPN/IP.
#'
#' @keywords internal
reset_fbref_session <- function() {
  .fbref_env$session <- httr::handle("https://fbref.com")
  message("FBref session reset - cookies cleared")
}


#' Fetch FBref match page HTML
#'
#' Makes HTTP request with browser headers to avoid Cloudflare blocking.
#'
#' @param match_url FBref match URL
#' @param timeout Request timeout in seconds (default 30)
#'
#' @return Parsed HTML document (rvest xml_document) or NULL on failure
#' @keywords internal
fetch_match_page <- function(match_url, timeout = 30) {
  # Validate URL
if (!grepl("fbref\\.com/en/matches/", match_url)) {
    cli::cli_abort("Invalid FBref match URL: {.val {match_url}}")
  }

  # Make request with session cookies and retry logic

  response <- fetch_with_retry(
    match_url,
    httr::add_headers(.headers = get_fbref_headers()),
    httr::timeout(timeout),
    handle = get_fbref_session()
  )

  # Check for errors returned by fetch_with_retry
  if (is.null(response)) {
    if (isTRUE(attr(response, "rate_limited"))) {
      cli::cli_warn("Rate limited by FBref (429). Stopping.")
    } else if (isTRUE(attr(response, "blocked"))) {
      cli::cli_warn("Blocked by Cloudflare (403). Stopping.")
    } else if (isTRUE(attr(response, "connection_error"))) {
      cli::cli_warn("Connection error: {attr(response, 'error_message')}")
    } else {
      cli::cli_warn("Failed to fetch {match_url}")
    }
    return(response)
  }

  # Parse HTML
  html_content <- httr::content(response, "text", encoding = "UTF-8")
  rvest::read_html(html_content)
}


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
      fbref_id = character(0)
    ))
  }

  # If league and season are both specified, go directly to that directory
  if (!is.null(league) && !is.null(season)) {
    cache_dir <- file.path(base_dir, league, season)
    if (!dir.exists(cache_dir)) {
      return(data.frame(
        league = character(0),
        season = character(0),
        fbref_id = character(0)
      ))
    }
    files <- list.files(cache_dir, pattern = "^[a-f0-9]{8}\\.rds$")
    if (length(files) == 0) {
      return(data.frame(
        league = character(0),
        season = character(0),
        fbref_id = character(0)
      ))
    }
    return(data.frame(
      league = rep(league, length(files)),
      season = rep(season, length(files)),
      fbref_id = gsub("\\.rds$", "", files)
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
      fbref_id = character(0)
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
          fbref_id = gsub("\\.rds$", "", files)
        )
      }
    }
  }

  if (length(all_results) == 0) {
    return(data.frame(
      league = character(0),
      season = character(0),
      fbref_id = character(0)
    ))
  }

  do.call(rbind, all_results)
}


# ============================================================================
# Data Utilities
# ============================================================================

#' Big 5 European league configurations
#'
#' @return Data frame with league info
#' @export
get_big5_leagues <- function() {
  data.frame(
    country = c("ENG", "ESP", "GER", "ITA", "FRA"),
    league = c("Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1"),
    tier = rep("1st", 5)
  )
}


#' Get cache directory path
#'
#' @param create Logical, whether to create if doesn't exist
#' @return Path to cache directory
#' @keywords internal
get_cache_dir <- function(create = TRUE) {
  cache_dir <- file.path("data-raw", "cache")
  if (create && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}


#' Load cached data for a specific type
#'
#' @param data_type Type of data (e.g., "lineups", "events", "shooting")
#' @return Data frame of cached data, or NULL if none exists
#' @keywords internal
load_cached_data <- function(data_type) {
  cache_file <- file.path(get_cache_dir(), paste0("cached_", data_type, ".rds"))
  if (file.exists(cache_file)) {
    readRDS(cache_file)
  } else {
    NULL
  }
}


#' Save data to cache
#'
#' @param data Data frame to cache
#' @param data_type Type of data
#' @keywords internal
save_cached_data <- function(data, data_type) {
  cache_file <- file.path(get_cache_dir(), paste0("cached_", data_type, ".rds"))
  saveRDS(data, cache_file)
  progress_msg(paste("Saved", nrow(data), "rows to", data_type, "cache"))
}


#' Get already-scraped match URLs from legacy cache
#'
#' @param data_type Type of data to check
#' @return Character vector of match URLs already in cache
#' @keywords internal
get_cached_match_urls_legacy <- function(data_type) {
  cached <- load_cached_data(data_type)
  if (!is.null(cached) && "MatchURL" %in% names(cached)) {
    unique(cached$MatchURL)
  } else if (!is.null(cached) && "match_url" %in% names(cached)) {
    unique(cached$match_url)
  } else {
    character(0)
  }
}


#' Derive goal events from shooting data
#'
#' When match summary data isn't available in the pre-scraped cache,
#' we can derive goal events from the shooting data (which IS available).
#' This gives us goal timing for splint creation, though not substitution timing.
#'
#' @param shooting_data Shooting data from load_fb_match_shooting (already snake_case)
#'
#' @return Data frame of goal events with timing
#' @keywords internal
derive_events_from_shooting <- function(shooting_data) {
  if (is.null(shooting_data) || nrow(shooting_data) == 0) {
    return(NULL)
  }

  # Column names should already be snake_case
  # Filter to goals only (outcome == "Goal")
  outcome_col <- if ("outcome" %in% names(shooting_data)) "outcome" else "Outcome"
  goals <- shooting_data[shooting_data[[outcome_col]] == "Goal", ]

  if (nrow(goals) == 0) {
    return(NULL)
  }

  # Build events data frame matching expected structure
  # Use snake_case column names consistently
  events <- data.frame(
    match_url = goals$match_url,
    team = goals$squad,
    event_type = "Goal",
    minute = as.numeric(goals$minute),
    player = goals$player,
    is_goal = TRUE,
    is_sub = FALSE,
    is_penalty = grepl("Penalty", goals$notes, ignore.case = TRUE),
    is_own_goal = grepl("Own Goal", goals$notes, ignore.case = TRUE)
  )

  # Sort by match and minute
  events <- events[order(events$match_url, events$minute), ]
  events
}


#' Get match lineups from advanced stats (no scraping needed)
#'
#' Derives lineup information from advanced match stats data.
#' This avoids rate limiting issues by using pre-scraped data.
#' Can use any stat type (summary, passing, defense, possession) - all contain
#' the core player/match info needed for lineups.
#'
#' @param stats_data Stats data from pannadata (already snake_case).
#'   Can be any stat type - passing/defense/possession have better historical coverage.
#'
#' @return Data frame of match lineups derived from stats
#' @keywords internal
derive_lineups_from_stats <- function(stats_data) {
  if (is.null(stats_data) || nrow(stats_data) == 0) {
    cli::cli_warn("No stats data to derive lineups from")
    return(NULL)
  }

  # Players who appear in stats played in the match
  # min column tells us how many minutes they played
  # Column names are already snake_case from clean_column_names()
  keep_cols <- intersect(
    c("match_url", "team", "player", "player_href", "nation", "pos", "age", "min",
      "home_away", "season_end_year"),
    names(stats_data)
  )
  lineups <- stats_data[, keep_cols, drop = FALSE]
  lineups$is_starter <- as.numeric(lineups$min) >= 45
  lineups$minutes <- as.numeric(lineups$min)
  lineups$is_home <- lineups$home_away == "Home"
  names(lineups)[names(lineups) == "player"] <- "player_name"

  lineups
}


#' Add season_end_year column to a data frame based on match_url lookup
#'
#' Joins season information from match results to any data frame with match_url.
#'
#' @param data Data frame to add season to
#' @param match_season_lookup Named vector mapping match_url to season_end_year
#'
#' @return Data frame with season_end_year column added
#' @keywords internal
add_season_column <- function(data, match_season_lookup) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }

  # Already has season_end_year? Return as-is
  if ("season_end_year" %in% names(data)) {
    return(data)
  }

  # Find the match_url column
  url_col <- if ("match_url" %in% names(data)) {
    "match_url"
  } else if ("MatchURL" %in% names(data)) {
    "MatchURL"
  } else {
    return(data)  # No URL column to join on
  }

  # Add season_end_year by lookup
  data$season_end_year <- match_season_lookup[data[[url_col]]]
  data
}


#' Extract season from FBref match URL
#'
#' @param urls Character vector of match URLs
#'
#' @return Character vector of seasons
#' @keywords internal
extract_season_from_url <- function(urls) {
  # FBref URLs contain the season in the path
  # Example: https://fbref.com/en/matches/abc123/2023-2024-Premier-League-...
  pattern <- "(\\d{4}-\\d{4})"
  matches <- regmatches(urls, regexpr(pattern, urls))
  matches
}


#' Extract season range from a data frame
#'
#' Extracts min/max season_end_year from a data frame.
#' All data from pannadata will have this column.
#'
#' @param data Data frame to extract season range from
#' @param data_name Name of the data for reporting (unused, kept for compatibility)
#'
#' @return Named list with min_season, max_season, and n_seasons
#' @keywords internal
extract_season_range <- function(data, data_name = "data") {
  if (is.null(data) || nrow(data) == 0) {
    return(list(min_season = NA, max_season = NA, n_seasons = 0))
  }

  # Look for season_end_year column (standard across all data)
  if ("season_end_year" %in% names(data)) {
    seasons <- unique(data$season_end_year)
    seasons <- seasons[!is.na(seasons)]
    if (length(seasons) == 0) {
      return(list(min_season = NA, max_season = NA, n_seasons = 0))
    }
    sorted_seasons <- sort(seasons)
    return(list(
      min_season = sorted_seasons[1],
      max_season = sorted_seasons[length(sorted_seasons)],
      n_seasons = length(sorted_seasons)
    ))
  }

  # Fallback: no season column found
  list(min_season = NA, max_season = NA, n_seasons = 0)
}


#' Report season ranges for all data types
#'
#' Prints a summary table showing min/max seasons for each data component.
#'
#' @param data List of data frames from pannadata
#'
#' @return Invisible data frame with season range info
#' @export
report_season_ranges <- function(data) {
  # Define data components and their display names
  components <- list(
    results = "Match Results",
    stats_summary = "Summary Stats",
    stats_passing = "Passing Stats",
    stats_defense = "Defense Stats",
    stats_possession = "Possession Stats",
    lineups = "Lineups",
    shooting = "Shooting",
    events = "Events"
  )

  # Build summary table
  summary_rows <- lapply(names(components), function(name) {
    df <- data[[name]]
    range_info <- extract_season_range(df, name)

    data.frame(
      data_type = components[[name]],
      rows = if (!is.null(df)) nrow(df) else 0,
      min_season = as.character(range_info$min_season),
      max_season = as.character(range_info$max_season),
      n_seasons = range_info$n_seasons
    )
  })

  summary_df <- do.call(rbind, summary_rows)

  # Print formatted table
  message("\n========================================")
  message("SEASON COVERAGE BY DATA TYPE")
  message("========================================")

  # Calculate column widths
  max_name <- max(nchar(summary_df$data_type))
  max_rows <- max(nchar(format(summary_df$rows, big.mark = ",")))

  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    rows_fmt <- format(row$rows, big.mark = ",", width = 8)

    if (is.na(row$min_season) || row$n_seasons == 0) {
      season_str <- "NO DATA"
    } else if (row$min_season == row$max_season) {
      season_str <- paste0("Season: ", row$min_season)
    } else {
      season_str <- paste0(row$min_season, " to ", row$max_season, " (", row$n_seasons, " seasons)")
    }

    message(sprintf("%-17s %s rows | %s",
                    paste0(row$data_type, ":"),
                    rows_fmt,
                    season_str))
  }

  message("========================================\n")

  invisible(summary_df)
}
