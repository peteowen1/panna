# Batch scraping operations for FBref
#
# Handles fixture scraping, batch match processing, and competition-season scraping.
# Depends on scrape_fbref_http.R for HTTP operations and scrape_fbref_cache.R for caching.


# ============================================================================
# Fixtures/Schedule Scraping
# ============================================================================
# Competition metadata is in fbref_competitions.R (single source of truth)
# Use: fbref_competitions, list_competitions(), get_fbref_comp_id(),
#      get_fbref_schedule_url(), get_seasons_since(), get_tournament_years()

#' Scrape match URLs from fixtures page
#'
#' Fetches a league's fixtures page and extracts all match URLs.
#'
#' @param league League code (e.g., "ENG", "ESP")
#' @param season Season string (e.g., "2024-2025")
#' @param completed_only If TRUE, only return matches with scores (default TRUE)
#'
#' @return Data frame with match_url, home_team, away_team, date columns
#' @export
#'
#' @examples
#' \dontrun{
#' fixtures <- scrape_fixtures("ENG", "2024-2025")
#' head(fixtures)
#' }
scrape_fixtures <- function(league, season, completed_only = TRUE) {
  .check_suggests("httr", "Scraping FBref requires httr.")
  .check_suggests("rvest", "Scraping FBref requires rvest.")
  url <- get_fbref_schedule_url(league, season)

  progress_msg(sprintf("Fetching fixtures: %s %s", league, season))

  # Fetch page with session cookies and retry logic
  response <- fetch_with_retry(
    url = url,
    max_retries = 3,
    base_delay = 1,
    max_delay = 30,
    httr::add_headers(.headers = get_fbref_headers()),
    httr::timeout(30),
    handle = get_fbref_session()
  )

  # Check for errors
  if (inherits(response, "fetch_error") || is.null(response)) {
    if (isTRUE(attr(response, "rate_limited")) || isTRUE(attr(response, "blocked"))) {
      cli::cli_abort("Rate limited or blocked. Stopping scraper.")
    }
    cli::cli_warn("Failed to fetch fixtures for {league} {season}.")
    return(NULL)
  }

  html_content <- httr::content(response, "text", encoding = "UTF-8")
  page <- rvest::read_html(html_content)

  # Find the fixtures table - it has class "stats_table" and id containing "sched"
  fixtures_table <- rvest::html_node(page, "table.stats_table")

  if (is.na(fixtures_table)) {
    cli::cli_warn("Could not find fixtures table for {league} {season}.")
    return(NULL)
  }

  # Parse table
  df <- tryCatch({
    rvest::html_table(fixtures_table, fill = TRUE)
  }, error = function(e) {
    cli::cli_warn("Error parsing fixtures table: {conditionMessage(e)}")
    return(NULL)
  })

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # Clean column names
  df <- clean_column_names(df)


  # Extract match URLs directly from all match report links
 # This avoids row count issues with spacer/header rows in cup tables
  match_links <- rvest::html_nodes(fixtures_table, "td[data-stat='match_report'] a")

  match_urls <- sapply(match_links, function(link) {
    href <- rvest::html_attr(link, "href")
    if (!is.na(href) && grepl("/matches/", href)) {
      return(paste0("https://fbref.com", href))
    }
    return(NA_character_)
  })
  match_urls <- match_urls[!is.na(match_urls)]

  # Add URLs to data frame
  if (length(match_urls) == nrow(df)) {
    df$match_url <- match_urls
  } else {
    # Row mismatch (common with cup tables that have round headers)
    # Try to match by filtering df to only rows with actual match data
    df$match_url <- NA_character_

    # For cups, filter to rows that have team names (not header rows)
    if ("home" %in% names(df) && length(match_urls) > 0) {
      valid_rows <- !is.na(df$home) & df$home != "" & df$home != "Home"
      if (sum(valid_rows) == length(match_urls)) {
        df$match_url[valid_rows] <- match_urls
      } else {
        # Last resort: assign what we can
        n_assign <- min(length(match_urls), nrow(df))
        df$match_url[1:n_assign] <- match_urls[1:n_assign]
      }
    }
  }

  # Filter to completed matches if requested
  if (completed_only) {
    # Completed matches have scores (check for "score" column or non-empty score)
    if ("score" %in% names(df)) {
      df <- df[!is.na(df$score) & df$score != "", ]
    }
  }

  # Filter to rows with valid match URLs
  df <- df[!is.na(df$match_url), ]

  # Select and rename key columns
  result <- data.frame(
    match_url = df$match_url,
    stringsAsFactors = FALSE
  )

  # Add optional columns if available
  if ("home" %in% names(df)) result$home_team <- df$home
  if ("away" %in% names(df)) result$away_team <- df$away
  if ("date" %in% names(df)) result$date <- df$date
  if ("score" %in% names(df)) result$score <- df$score
  if ("wk" %in% names(df)) result$matchweek <- df$wk

  progress_msg(sprintf("  Found %d completed matches", nrow(result)))

  result
}


# ============================================================================
# Main Scraping Function
# ============================================================================

#' Scrape FBref match data directly
#'
#' Master function for scraping match-level data from FBref.
#' Uses direct HTTP requests with browser headers to bypass Cloudflare.
#' Implements polite scraping with rate limiting and incremental caching.
#'
#' @param match_urls Character vector of FBref match URLs
#' @param league League code for file naming (e.g., "ENG", "ESP", "GER", "ITA", "FRA")
#' @param season Season string for file naming (e.g., "2024-2025")
#' @param table_types Character vector of table types to scrape.
#'   Options: "summary", "passing", "passing_types", "defense",
#'   "possession", "misc", "keeper", "shots", "metadata"
#'   Default: all of the above
#' @param delay Seconds between requests (default 5, minimum 3)
#' @param use_cache Whether to use/update cache (default TRUE)
#' @param verbose Print progress messages (default TRUE)
#' @param max_matches Maximum number of matches to scrape (default Inf for all)
#'
#' @return List containing data frames for each table type:
#'   \item{metadata}{Match metadata (teams, scores, IDs, manager, captain, venue, etc.)}
#'   \item{summary}{Player summary stats}
#'   \item{passing}{Passing stats}
#'   \item{defense}{Defensive stats}
#'   \item{possession}{Possession stats}
#'   \item{shots}{Shot data}
#'   \item{events}{Match events timeline (goals, cards, substitutions)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' urls <- c(
#'   "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"
#' )
#' data <- scrape_fbref_matches(urls, league = "ESP", season = "2025-2026")
#' data$summary  # Player stats
#' data$shots    # Shot-level data
#' data$events   # Match timeline
#' }
scrape_fbref_matches <- function(
    match_urls,
    league,
    season,
    table_types = c("summary", "passing", "passing_types", "defense",
                    "possession", "misc", "keeper", "shots", "events", "metadata"),
    delay = 5,
    use_cache = TRUE,
    verbose = TRUE,
    max_matches = Inf
) {
  .check_suggests("httr", "Scraping FBref requires httr.")
  .check_suggests("rvest", "Scraping FBref requires rvest.")
  # Validate inputs
  if (length(match_urls) == 0) {
    cli::cli_abort("No match URLs provided")
  }

  if (missing(league) || missing(season)) {
    cli::cli_abort("league and season are required for file naming")
  }

  # Enforce minimum delay for polite scraping
  delay <- max(delay, 3)

  # Limit matches if max_matches is set
  if (is.finite(max_matches) && max_matches < length(match_urls)) {
    if (verbose) {
      progress_msg(sprintf("Limiting to first %d of %d matches", max_matches, length(match_urls)))
    }
    match_urls <- match_urls[1:max_matches]
  }

  if (verbose) {
    save_dir <- pannadata_dir()
    progress_msg(sprintf(
      "Scraping %d matches (%s %s) with %ds delay",
      length(match_urls), league, season, delay
    ))
    progress_msg(sprintf("Saving to: %s", save_dir))
  }

  # Initialize result containers
  results <- list()
  for (tt in table_types) {
    results[[tt]] <- list()
  }

  # Track progress
  n_cached <- 0
  n_scraped <- 0
  n_failed <- 0

  for (i in seq_along(match_urls)) {
    url <- match_urls[i]

    # Extract fbref_id from URL
    fbref_id <- regmatches(url, regexpr("[a-f0-9]{8}", url))

    if (length(fbref_id) == 0) {
      cli::cli_warn("Could not extract match ID from URL: {url}")
      n_failed <- n_failed + 1
      next
    }

    # Check cache - only skip if ALL requested table types are cached
    if (use_cache && is_match_cached(league, season, fbref_id, table_types)) {
      if (verbose && (i %% 10 == 0 || i == 1)) {
        progress_msg(sprintf("  [%d/%d] Loading from cache: %s",
                             i, length(match_urls), fbref_id))
      }

      # Load cached data for each table type
      for (tt in table_types) {
        cached <- load_match_table(league, season, fbref_id, tt)
        if (!is.null(cached)) {
          results[[tt]][[fbref_id]] <- cached
        }
      }
      n_cached <- n_cached + 1
      next
    }

    # Fetch and parse
    if (verbose) {
      progress_msg(sprintf("  [%d/%d] Fetching: %s",
                           i, length(match_urls), fbref_id))
    }

    page <- tryCatch({
      fetch_match_page(url)
    }, error = function(e) {
      cli::cli_warn("Error fetching {url}: {conditionMessage(e)}")
      NULL
    })

    # Check for rate limiting or blocking - stop early
    if (isTRUE(attr(page, "rate_limited")) || isTRUE(attr(page, "blocked"))) {
      n_failed <- n_failed + 1
      progress_msg(sprintf("  STOPPED: Rate limited or blocked after %d matches", i))
      progress_msg(sprintf("Complete: %d scraped, %d cached, %d failed",
                           n_scraped, n_cached, n_failed + length(match_urls) - i))
      # Return partial results
      return(lapply(results, function(x) {
        if (length(x) == 0) return(NULL)
        rbindlist(x, use.names = TRUE, fill = TRUE)
      }))
    }

    if (is.null(page)) {
      n_failed <- n_failed + 1
      if (verbose) {
        progress_msg(sprintf("    FAILED: %s - fetch error", fbref_id))
      }
      # Wait longer after failure (might be rate limited)
      if (i < length(match_urls)) {
        Sys.sleep(add_delay_jitter(delay * 2))
      }
      next
    }

    # Parse page
    parsed <- tryCatch({
      parse_match_page(page, url)
    }, error = function(e) {
      if (verbose) {
        progress_msg(sprintf("    FAILED: %s - parse error: %s", fbref_id, e$message))
      }
      NULL
    })

    if (is.null(parsed)) {
      n_failed <- n_failed + 1
      if (i < length(match_urls)) {
        Sys.sleep(add_delay_jitter(delay))
      }
      next
    }

    # Process and cache each table type
    tables_saved <- 0
    tables_missing <- character(0)

    for (tt in table_types) {
      data <- NULL

      if (tt == "metadata") {
        data <- parsed$metadata
      } else if (tt == "shots") {
        data <- parsed$shots
      } else if (tt == "events") {
        data <- parsed$events
      } else if (tt == "keeper") {
        data <- combine_team_tables(parsed, "keeper")
      } else {
        data <- combine_team_tables(parsed, tt)
      }

      if (!is.null(data) && nrow(data) > 0) {
        # Add league and season to data
        data$league <- league
        data$season <- season

        results[[tt]][[fbref_id]] <- data

        # Always save scraped data (use_cache only controls whether we skip cached matches)
        save_match_table(data, league, season, fbref_id, tt)
        if (verbose) {
          progress_msg(sprintf("      [%s] %d rows", tt, nrow(data)))
        }
        tables_saved <- tables_saved + 1
      } else {
        tables_missing <- c(tables_missing, tt)
      }
    }

    # Save which tables were available in metadata (so we don't re-scrape)
    tables_found <- setdiff(table_types, tables_missing)
    if ("metadata" %in% names(results) && fbref_id %in% names(results[["metadata"]])) {
      results[["metadata"]][[fbref_id]]$tables_available <- paste(tables_found, collapse = ",")
      if (length(tables_found) > 0) {
        # Re-save metadata with tables_available field
        save_match_table(results[["metadata"]][[fbref_id]], league, season, fbref_id, "metadata")
      }
    }

    # Report on tables saved
    if (tables_saved == 0) {
      n_failed <- n_failed + 1
      if (verbose) {
        progress_msg(sprintf("    FAILED: %s - no tables found", fbref_id))
      }
    } else if (tables_saved < length(table_types)) {
      n_scraped <- n_scraped + 1
      if (verbose && length(tables_missing) > 0) {
        progress_msg(sprintf("    OK: %s - %d/%d tables (missing: %s)",
                             fbref_id, tables_saved, length(table_types),
                             paste(tables_missing, collapse = ", ")))
      }
    } else {
      n_scraped <- n_scraped + 1
    }

    # Rate limiting - wait before next request (with jitter)
    if (i < length(match_urls)) {
      Sys.sleep(add_delay_jitter(delay))
    }
  }

  if (verbose) {
    progress_msg(sprintf("Complete: %d scraped, %d cached, %d failed",
                         n_scraped, n_cached, n_failed))
  }

  # Combine results into data frames
  final <- list()
  for (tt in table_types) {
    if (length(results[[tt]]) > 0) {
      final[[tt]] <- rbindlist(results[[tt]], use.names = TRUE, fill = TRUE)
    } else {
      final[[tt]] <- NULL
    }
  }

  # Add scrape stats as attributes for tracking
  attr(final, "n_scraped") <- n_scraped
  attr(final, "n_cached") <- n_cached
  attr(final, "n_failed") <- n_failed

  final
}


# ============================================================================
# Batch Scraping Helpers
# ============================================================================

#' Get cached match URLs from metadata
#'
#' Reads all cached metadata files for a league-season and extracts
#' the match URLs. Useful for re-scraping or updating cached matches.
#'
#' @param league League code (e.g., "ENG", "ESP")
#' @param season Season string (e.g., "2023-2024")
#'
#' @return Character vector of match URLs
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' urls <- get_cached_match_urls("ENG", "2023-2024")
#' length(urls)  # Number of cached matches
#' }
get_cached_match_urls <- function(league, season) {
  cache_dir <- get_fbref_match_cache_dir("metadata", league, season, create = FALSE)

  if (!dir.exists(cache_dir)) return(character(0))

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(character(0))

  urls <- vapply(files, function(f) {
    meta <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(meta) && "match_url" %in% names(meta)) {
      return(meta$match_url)
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  urls[!is.na(urls)]
}


#' Scrape a competition-season
#'
#' Scrapes all matches for a competition-season, either from cache or FBref.
#' Handles fixture fetching, caching logic, and progress reporting.
#'
#' @param comp Competition code (e.g., "ENG", "UCL")
#' @param season Season string (e.g., "2023-2024")
#' @param table_types Character vector of table types to scrape
#' @param delay Seconds between requests
#' @param force_rescrape If TRUE, ignore cache and rescrape all
#' @param max_matches Maximum matches to scrape (default Inf)
#'
#' @return Number of matches scraped (for tracking session totals)
#' @export
#'
#' @examples
#' \dontrun{
#' n <- scrape_comp_season("ENG", "2023-2024",
#'                         table_types = c("summary", "events"),
#'                         delay = 5, force_rescrape = FALSE)
#' }
scrape_comp_season <- function(comp, season, table_types, delay,
                                force_rescrape, max_matches = Inf) {
  .check_suggests("httr", "Scraping FBref requires httr.")
  .check_suggests("rvest", "Scraping FBref requires rvest.")

  cat(sprintf("\n%s %s\n", comp, season))
  cat(strrep("-", 40), "\n")

  if (max_matches <= 0) {
    cat("  Skipping (session limit reached)\n")
    return(0)

  }

  # Check cache first
  cached_urls <- get_cached_match_urls(comp, season)

  if (length(cached_urls) > 0 && !force_rescrape) {
    cat(sprintf("  Cache: %d matches found\n", length(cached_urls)))

    result <- tryCatch({
      scrape_fbref_matches(
        match_urls = cached_urls,
        league = comp,
        season = season,
        table_types = table_types,
        delay = delay,
        use_cache = TRUE,
        verbose = TRUE,
        max_matches = max_matches
      )
    }, error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n")
      NULL
    })

    n_scraped <- if (!is.null(result)) attr(result, "n_scraped") else 0
    return(if (is.null(n_scraped)) 0 else n_scraped)
  }

  # No cache - fetch fixtures from FBref
  cat("  Fetching fixtures from FBref...\n")
  Sys.sleep(delay)

  fixtures <- tryCatch(
    scrape_fixtures(comp, season, completed_only = TRUE),
    error = function(e) {
      cat("  ERROR fetching fixtures:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    cat("  No fixtures found\n")
    return(0)
  }

  urls <- fixtures$match_url
  cat(sprintf("  Found %d matches\n", length(urls)))

  result <- tryCatch({
    scrape_fbref_matches(
      match_urls = urls,
      league = comp,
      season = season,
      table_types = table_types,
      delay = delay,
      use_cache = !force_rescrape,
      verbose = TRUE,
      max_matches = max_matches
    )
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
    NULL
  })

  n_scraped <- if (!is.null(result)) attr(result, "n_scraped") else 0
  if (is.null(n_scraped)) 0 else n_scraped
}
