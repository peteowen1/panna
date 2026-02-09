# Understat scraping orchestration
#
# Batch scraping, manifest system, and smart scraping.
# Single-match scraping and cache management are in scrape_understat_match.R.
# HTTP layer and data extraction functions are in scrape_understat_api.R.


#' Scrape multiple Understat matches
#'
#' Scrapes multiple matches with rate limiting and caching.
#'
#' @param understat_ids Vector of Understat match IDs
#' @param league League code
#' @param season Season
#' @param delay_seconds Delay between requests (default 3)
#' @param skip_cached Skip already cached matches (default TRUE)
#' @param max_matches Maximum matches to scrape (default NULL = all)
#'
#' @return List with counts of scraped, cached, and failed matches
#' @export
scrape_understat_matches <- function(understat_ids,
                                      league,
                                      season,
                                      delay_seconds = 3,
                                      skip_cached = TRUE,
                                      max_matches = NULL) {
  n_total <- length(understat_ids)

  if (!is.null(max_matches) && max_matches < n_total) {
    progress_msg(sprintf("Limiting to first %d of %d matches", max_matches, n_total))
    understat_ids <- understat_ids[1:max_matches]
    n_total <- max_matches
  }

  progress_msg(sprintf("Scraping %d Understat matches: %s %s", n_total, league, season))

  n_scraped <- 0
  n_cached <- 0
  n_failed <- 0

  for (i in seq_along(understat_ids)) {
    id <- understat_ids[i]

    # Check cache
    if (skip_cached && is_understat_cached(league, season, id)) {
      progress_msg(sprintf("  [%d/%d] Cached: %s", i, n_total, id))
      n_cached <- n_cached + 1
      next
    }

    # Scrape match
    progress_msg(sprintf("  [%d/%d] Scraping: %s", i, n_total, id))

    result <- tryCatch({
      scrape_understat_match(id, league, season)
    }, error = function(e) {
      cli::cli_warn("Error scraping match {id}: {conditionMessage(e)}")
      NULL
    })

    if (is.null(result) || is.null(result$metadata)) {
      n_failed <- n_failed + 1
      progress_msg(sprintf("    FAILED: %s", id))
    } else {
      n_scraped <- n_scraped + 1
      # Report metadata success - roster/shots require JS
      progress_msg(sprintf("    OK: %s - metadata extracted", id))
    }

    # Rate limiting
    if (i < n_total) {
      jitter <- delay_seconds * 0.3
      actual_delay <- delay_seconds + stats::runif(1, -jitter, jitter)
      Sys.sleep(actual_delay)
    }
  }

  progress_msg(sprintf("Complete: %d scraped, %d cached, %d failed",
                       n_scraped, n_cached, n_failed))

  invisible(list(
    scraped = n_scraped,
    cached = n_cached,
    failed = n_failed
  ))
}


#' Scrape full Understat season
#'
#' Scrapes all matches for a league-season, first fetching fixtures.
#'
#' @param league League code (e.g., "ENG")
#' @param season Season year (e.g., 2024)
#' @param delay_seconds Delay between requests (default 3)
#' @param skip_cached Skip already cached matches (default TRUE)
#' @param max_matches Maximum matches to scrape (default NULL = all)
#'
#' @return List with counts of scraped, cached, and failed matches
#' @export
#'
#' @examples
#' \dontrun{
#' # Scrape all 2024 Premier League matches from Understat
#' scrape_understat_season("ENG", 2024)
#'
#' # Scrape with limit for testing
#' scrape_understat_season("ENG", 2024, max_matches = 5)
#' }
scrape_understat_season <- function(league,
                                     season,
                                     delay_seconds = 3,
                                     skip_cached = TRUE,
                                     max_matches = NULL) {
  # Validate league
  if (!is_understat_league(league)) {
    cli::cli_abort(c(
      "League {.val {league}} is not available on Understat.",
      "i" = "Valid leagues: {paste(list_understat_competitions(), collapse = ', ')}"
    ))
  }

  # Get fixtures
  fixtures <- scrape_understat_fixtures(league, season)

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    cli::cli_warn("No fixtures found for {league} {season}.")
    return(invisible(list(scraped = 0, cached = 0, failed = 0)))
  }

  # Extract match IDs
  id_col <- if ("id" %in% names(fixtures)) "id" else "match_id"
  if (!id_col %in% names(fixtures)) {
    cli::cli_warn("Could not find match ID column in fixtures")
    return(invisible(list(scraped = 0, cached = 0, failed = 0)))
  }

  match_ids <- as.character(fixtures[[id_col]])

  # Wait before scraping matches
  Sys.sleep(delay_seconds)

  # Scrape matches
  scrape_understat_matches(
    match_ids,
    league = league,
    season = season,
    delay_seconds = delay_seconds,
    skip_cached = skip_cached,
    max_matches = max_matches
  )
}


# ============================================================================
# Aggregation Functions
# ============================================================================

#' Aggregate cached Understat data
#'
#' Loads and combines all cached parquet files for a table type.
#'
#' @param table_type Table type (metadata, roster, shots)
#' @param league Optional league filter
#' @param season Optional season filter
#'
#' @return Combined data frame
#' @keywords internal
aggregate_understat_data <- function(table_type, league = NULL, season = NULL) {
  base_dir <- file.path(pannadata_dir(), "understat", table_type)

  if (!dir.exists(base_dir)) {
    return(data.frame())
  }

  # Find parquet files based on filters
  if (!is.null(league) && !is.null(season)) {
    # Specific league and season
    parquet_path <- get_understat_parquet_path(table_type, league, season, create = FALSE)
    if (!file.exists(parquet_path)) return(data.frame())
    files <- parquet_path
  } else if (!is.null(league)) {
    # All seasons for a league
    league_dir <- file.path(base_dir, league)
    if (!dir.exists(league_dir)) return(data.frame())
    files <- list.files(league_dir, pattern = "\\.parquet$", full.names = TRUE)
  } else {
    # All leagues and seasons
    files <- list.files(base_dir, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  }

  if (length(files) == 0) {
    return(data.frame())
  }

  # Load and combine
  all_data <- lapply(files, function(f) {
    tryCatch(as.data.frame(arrow::read_parquet(f)), error = function(e) NULL)
  })

  all_data <- all_data[!sapply(all_data, is.null)]

  if (length(all_data) == 0) {
    return(data.frame())
  }

  rbindlist(all_data, use.names = TRUE, fill = TRUE)
}


#' Bulk scrape Understat matches with auto-detection
#'
#' Scrapes matches by ID range, auto-detecting league and season from metadata.
#' This is useful for scraping all matches across all leagues without knowing
#' which IDs belong to which league.
#'
#' @param start_id Starting match ID
#' @param end_id Ending match ID (inclusive)
#' @param delay Seconds between requests (default 3)
#' @param skip_cached Skip already cached matches (default TRUE)
#' @param verbose Print progress messages (default TRUE)
#'
#' @return Data frame with scraping results (match_id, league, season, status)
#' @export
#'
#' @examples
#' \dontrun{
#' # Scrape recent matches (2024-25 season IDs are ~27000-29000)
#' results <- bulk_scrape_understat(28900, 28999)
#'
#' # Check results
#' table(results$league, results$status)
#' }
bulk_scrape_understat <- function(start_id, end_id, delay = 3,
                                   skip_cached = TRUE, verbose = TRUE) {
  start_id <- as.integer(start_id)
  end_id <- as.integer(end_id)

  if (start_id > end_id) {
    cli::cli_abort("start_id must be <= end_id")
  }

  n_total <- end_id - start_id + 1

  if (verbose) {
    progress_msg(sprintf("Bulk scraping Understat matches %d to %d (%d total)",
                         start_id, end_id, n_total))
  }

  results <- vector("list", n_total)
  n_success <- 0
  n_failed <- 0
  n_skipped <- 0

  for (i in seq_len(n_total)) {
    match_id <- start_id + i - 1

    # Rate limiting (skip on first iteration)
    if (i > 1) {
      jitter <- delay * 0.3
      actual_delay <- delay + stats::runif(1, -jitter, jitter)
      Sys.sleep(actual_delay)
    }

    # First, quickly fetch just the metadata to check league/season
    url <- get_understat_match_url(match_id)

    page <- tryCatch({
      fetch_understat_page(url)
    }, error = function(e) NULL)

    if (is.null(page)) {
      n_failed <- n_failed + 1
      results[[i]] <- data.frame(
        match_id = match_id,
        league = NA_character_,
        season = NA_character_,
        status = "failed_fetch",
        stringsAsFactors = FALSE
      )
      if (verbose) {
        progress_msg(sprintf("  [%d/%d] %d: failed to fetch", i, n_total, match_id))
      }
      next
    }

    # Extract metadata to get league/season
    metadata <- tryCatch({
      extract_understat_metadata(page, match_id)
    }, error = function(e) NULL)

    if (is.null(metadata)) {
      n_failed <- n_failed + 1
      results[[i]] <- data.frame(
        match_id = match_id,
        league = NA_character_,
        season = NA_character_,
        status = "failed_metadata",
        stringsAsFactors = FALSE
      )
      if (verbose) {
        progress_msg(sprintf("  [%d/%d] %d: no metadata", i, n_total, match_id))
      }
      next
    }

    # Map Understat league name to our code
    league_code <- understat_league_to_code(metadata$league)
    season <- metadata$season

    if (is.na(league_code)) {
      n_failed <- n_failed + 1
      results[[i]] <- data.frame(
        match_id = match_id,
        league = metadata$league,
        season = season,
        status = "unknown_league",
        stringsAsFactors = FALSE
      )
      if (verbose) {
        progress_msg(sprintf("  [%d/%d] %d: unknown league '%s'",
                             i, n_total, match_id, metadata$league))
      }
      next
    }

    # Check if already cached
    if (skip_cached && is_understat_cached(league_code, season, match_id)) {
      n_skipped <- n_skipped + 1
      results[[i]] <- data.frame(
        match_id = match_id,
        league = league_code,
        season = season,
        status = "cached",
        stringsAsFactors = FALSE
      )
      if (verbose) {
        progress_msg(sprintf("  [%d/%d] %d: cached (%s %s)",
                             i, n_total, match_id, league_code, season))
      }
      next
    }

    # Now get the full data (events from HTML, roster/shots from API)
    events <- tryCatch({
      extract_understat_events(page, match_id)
    }, error = function(e) NULL)

    api_data <- tryCatch({
      fetch_understat_match_api(match_id)
    }, error = function(e) NULL)

    roster <- tryCatch({
      extract_understat_roster(api_data, match_id)
    }, error = function(e) NULL)

    shots <- tryCatch({
      extract_understat_shots(api_data, match_id)
    }, error = function(e) NULL)

    # Add league/season context
    metadata$league_code <- league_code
    metadata$season_input <- as.character(season)

    if (!is.null(events)) {
      events$league_code <- league_code
      events$season_input <- as.character(season)
    }
    if (!is.null(roster)) {
      roster$league_code <- league_code
      roster$season_input <- as.character(season)
    }
    if (!is.null(shots)) {
      shots$league_code <- league_code
      shots$season_input <- as.character(season)
    }

    # Save to cache
    save_understat_table(metadata, league_code, season, "metadata")
    if (!is.null(events)) save_understat_table(events, league_code, season, "events")
    if (!is.null(roster)) save_understat_table(roster, league_code, season, "roster")
    if (!is.null(shots)) save_understat_table(shots, league_code, season, "shots")

    n_success <- n_success + 1
    results[[i]] <- data.frame(
      match_id = match_id,
      league = league_code,
      season = season,
      status = "success",
      stringsAsFactors = FALSE
    )

    if (verbose) {
      progress_msg(sprintf("  [%d/%d] %d: %s vs %s (%s %s)",
                           i, n_total, match_id,
                           metadata$home_team, metadata$away_team,
                           league_code, season))
    }
  }

  if (verbose) {
    progress_msg(sprintf("\nCompleted: %d success, %d cached, %d failed",
                         n_success, n_skipped, n_failed))
  }

  rbindlist(results, use.names = TRUE, fill = TRUE)
}


#' Build Consolidated Understat Parquet Files
#'
#' Creates a single parquet file per table type containing ALL leagues and seasons.
#' These consolidated files are uploaded to GitHub releases for fast remote querying.
#'
#' @param table_types Character vector of table types to consolidate.
#'   Defaults to c("roster", "shots", "metadata").
#' @param output_dir Directory to write consolidated parquet files.
#'   Defaults to pannadata_dir()/consolidated.
#' @param verbose Print progress messages.
#'
#' @return Data frame with table_type, n_rows, size_mb columns.
#'
#' @export
#' @examples
#' \dontrun{
#' # Build all consolidated Understat parquets
#' build_consolidated_understat_parquet()
#' }
build_consolidated_understat_parquet <- function(table_types = NULL, output_dir = NULL,
                                                  verbose = TRUE) {
  .check_suggests("arrow", "Building parquet files requires arrow.")

  # Default table types for Understat
  if (is.null(table_types)) {
    table_types <- c("roster", "shots", "metadata")
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
    if (verbose) message(sprintf("\nConsolidating understat_%s...", tt))

    # Find all parquet files for this table type
    tt_dir <- file.path(base_dir, "understat", tt)
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
      do.call(rbind, dfs)
    }, error = function(e) {
      if (verbose) cli::cli_warn("Error combining {tt}: {conditionMessage(e)}")
      NULL
    })

    if (is.null(all_data) || nrow(all_data) == 0) {
      if (verbose) message(sprintf("  Skipping %s - no data after combining", tt))
      next
    }

    # Write consolidated parquet with understat_ prefix
    output_path <- file.path(output_dir, paste0("understat_", tt, ".parquet"))
    arrow::write_parquet(all_data, output_path)

    size_mb <- round(file.size(output_path) / (1024 * 1024), 2)
    if (verbose) {
      message(sprintf("  Wrote %s: %s rows, %.1f MB",
                      basename(output_path),
                      format(nrow(all_data), big.mark = ","),
                      size_mb))
    }

    results[[length(results) + 1]] <- data.frame(
      table_type = paste0("understat_", tt),
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
    message(sprintf("\nCreated %d consolidated Understat files (%.1f MB total)",
                    nrow(result_df), total_mb))
  }

  result_df
}


# ============================================================================
# Manifest-Based Scraping System
# ============================================================================

#' Default league ID starting points for 2025/2026 season
#'
#' Understat match IDs are interleaved across leagues. Each league occupies
#' a distinct ID band (~200-300 IDs apart). These defaults are verified via
#' live scraping on 2026-02-05.
#'
#' @keywords internal
DEFAULT_LEAGUE_STARTS <- list(
  RUS = 28600,   # RFPL - lowest IDs
  ENG = 28850,   # EPL
  ESP = 29150,   # La Liga
  FRA = 29550,   # Ligue 1
  ITA = 29850,   # Serie A
  GER = 30250    # Bundesliga - highest IDs
)


#' Load Understat manifest from parquet file
#'
#' The manifest tracks all scraped match IDs with their league and season.
#'
#' @param path Path to manifest parquet file
#'
#' @return Data frame with columns: match_id, league, season, scraped_at
#' @export
load_understat_manifest <- function(path) {
  if (!file.exists(path)) {
    # Return empty manifest structure
    return(data.frame(
      match_id = integer(0),
      league = character(0),
      season = character(0),
      scraped_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  tryCatch({
    as.data.frame(arrow::read_parquet(path))
  }, error = function(e) {
    cli::cli_warn("Error reading manifest: {conditionMessage(e)}")
    data.frame(
      match_id = integer(0),
      league = character(0),
      season = character(0),
      scraped_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    )
  })
}


#' Save Understat manifest to parquet file
#'
#' @param manifest Data frame with manifest data
#' @param path Path to save manifest parquet file
#'
#' @return Invisible path to saved file
#' @export
save_understat_manifest <- function(manifest, path) {
  # Ensure directory exists
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }

  # Write with temp file pattern to avoid corruption
  temp_path <- paste0(path, ".tmp")
  arrow::write_parquet(manifest, temp_path)

  if (file.exists(path)) {
    file.remove(path)
  }
  file.rename(temp_path, path)

  invisible(path)
}


#' Get maximum cached ID for each league from manifest
#'
#' @param manifest Data frame from load_understat_manifest
#'
#' @return Named list with max ID per league (NA if no data for league)
#' @keywords internal
get_league_max_ids <- function(manifest) {
  leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "RUS")

  result <- setNames(
    rep(NA_integer_, length(leagues)),
    leagues
  )

  if (nrow(manifest) == 0) {
    return(as.list(result))
  }

  for (league in leagues) {
    league_ids <- manifest$match_id[manifest$league == league]
    if (length(league_ids) > 0) {
      result[league] <- max(league_ids, na.rm = TRUE)
    }
  }

  as.list(result)
}


#' Build manifest from existing cached Understat data
#'
#' Scans existing parquet files and builds a manifest. Use this to bootstrap
#' the manifest when migrating from the old system.
#'
#' @param data_dir Base data directory (defaults to pannadata_dir())
#'
#' @return Data frame with manifest structure
#' @keywords internal
build_understat_manifest_from_cache <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    data_dir <- pannadata_dir()
  }

  metadata_dir <- file.path(data_dir, "understat", "metadata")

  if (!dir.exists(metadata_dir)) {
    message("No cached Understat metadata found at: ", metadata_dir)
    return(data.frame(
      match_id = integer(0),
      league = character(0),
      season = character(0),
      scraped_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  # Find all metadata parquet files
  parquet_files <- list.files(
    metadata_dir,
    pattern = "\\.parquet$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(parquet_files) == 0) {
    message("No parquet files found in: ", metadata_dir)
    return(data.frame(
      match_id = integer(0),
      league = character(0),
      season = character(0),
      scraped_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  message(sprintf("Building manifest from %d parquet files...", length(parquet_files)))

  all_records <- list()

  for (f in parquet_files) {
    tryCatch({
      df <- arrow::read_parquet(f)

      if (nrow(df) > 0 && "understat_id" %in% names(df)) {
        # Extract league and season from path or data
        league_code <- if ("league_code" %in% names(df)) {
          df$league_code[1]
        } else {
          # Try to extract from file path
          path_parts <- strsplit(f, "/|\\\\")[[1]]
          league_idx <- which(path_parts == "metadata") + 1
          if (league_idx <= length(path_parts)) path_parts[league_idx] else NA_character_
        }

        season <- if ("season_input" %in% names(df)) {
          df$season_input[1]
        } else if ("season" %in% names(df)) {
          df$season[1]
        } else {
          # Try to extract from file name
          gsub("\\.parquet$", "", basename(f))
        }

        record <- data.frame(
          match_id = as.integer(unique(df$understat_id)),
          league = as.character(league_code),
          season = as.character(season),
          scraped_at = Sys.time(),
          stringsAsFactors = FALSE
        )

        all_records[[length(all_records) + 1]] <- record
      }
    }, error = function(e) {
      cli::cli_warn("Error reading {f}: {conditionMessage(e)}")
    })
  }

  if (length(all_records) == 0) {
    return(data.frame(
      match_id = integer(0),
      league = character(0),
      season = character(0),
      scraped_at = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  manifest <- rbindlist(all_records, use.names = TRUE, fill = TRUE)

  # Remove duplicates (keep first occurrence)
  manifest <- manifest[!duplicated(manifest$match_id), ]

  message(sprintf("Built manifest with %d matches across %d leagues",
                  nrow(manifest), length(unique(manifest$league))))

  manifest
}


#' Smart scrape Understat with per-league tracking
#'
#' Scrapes Understat matches using per-league max ID tracking. Each league
#' is scanned independently from its own max ID, which handles the interleaved
#' nature of Understat match IDs across leagues.
#'
#' @param manifest_path Path to manifest parquet file
#' @param leagues Character vector of leagues to scrape (default: all 6)
#' @param lookback Number of IDs to look back from max (handles gaps)
#' @param max_misses Stop scanning a league after this many consecutive misses
#' @param delay Seconds between requests
#' @param verbose Print progress messages
#'
#' @return Data frame with scraping results (match_id, league, status)
#' @export
#'
#' @examples
#' \dontrun{
#' # Run smart scraper
#' results <- smart_scrape_understat(
#'   manifest_path = "data/understat-manifest.parquet",
#'   lookback = 20,
#'   max_misses = 50,
#'   delay = 3
#' )
#'
#' # Check results
#' table(results$league, results$status)
#' }
smart_scrape_understat <- function(manifest_path,
                                    leagues = c("RUS", "ENG", "ESP", "FRA", "ITA", "GER"),
                                    lookback = 20,
                                    max_misses = 50,
                                    delay = 3,
                                    verbose = TRUE) {

  # Load manifest
  manifest <- load_understat_manifest(manifest_path)
  if (verbose) {
    message(sprintf("Loaded manifest with %d existing matches", nrow(manifest)))
  }

  # Get max ID per league
  league_max_ids <- get_league_max_ids(manifest)

  results <- list()
  total_new <- 0

  # Process each league in order (RUS has lowest IDs, GER has highest)
  for (league in leagues) {
    if (verbose) {
      cat(sprintf("\n=== Scanning %s ===\n", league))
    }

    # Get max ID for this league, or use default
    max_id <- league_max_ids[[league]]
    if (is.na(max_id)) {
      max_id <- DEFAULT_LEAGUE_STARTS[[league]]
      if (verbose) {
        cat(sprintf("No cached data for %s, starting from default: %d\n", league, max_id))
      }
    } else if (verbose) {
      cat(sprintf("Max cached ID for %s: %d\n", league, max_id))
    }

    # Start scanning from (max - lookback) to handle gaps
    start_id <- max(1, max_id - lookback)
    current_id <- start_id
    consecutive_misses <- 0
    new_matches <- 0
    league_results <- list()

    while (consecutive_misses < max_misses) {
      # Rate limiting (skip on first iteration)
      if (current_id > start_id) {
        jitter <- delay * 0.3
        actual_delay <- delay + stats::runif(1, -jitter, jitter)
        Sys.sleep(actual_delay)
      }

      # Try to scrape match metadata first to check league
      url <- get_understat_match_url(current_id)

      page <- tryCatch({
        fetch_understat_page(url)
      }, error = function(e) NULL)

      if (is.null(page)) {
        # Invalid ID - count as miss
        consecutive_misses <- consecutive_misses + 1
        league_results[[length(league_results) + 1]] <- data.frame(
          match_id = current_id,
          league = NA_character_,
          season = NA_character_,
          status = "invalid",
          stringsAsFactors = FALSE
        )
        current_id <- current_id + 1
        next
      }

      # Extract metadata to check league
      metadata <- tryCatch({
        extract_understat_metadata(page, current_id)
      }, error = function(e) NULL)

      if (is.null(metadata)) {
        consecutive_misses <- consecutive_misses + 1
        league_results[[length(league_results) + 1]] <- data.frame(
          match_id = current_id,
          league = NA_character_,
          season = NA_character_,
          status = "no_metadata",
          stringsAsFactors = FALSE
        )
        current_id <- current_id + 1
        next
      }

      # Map Understat league name to our code
      match_league <- understat_league_to_code(metadata$league)
      match_season <- metadata$season

      if (is.na(match_league)) {
        consecutive_misses <- consecutive_misses + 1
        league_results[[length(league_results) + 1]] <- data.frame(
          match_id = current_id,
          league = metadata$league,
          season = match_season,
          status = "unknown_league",
          stringsAsFactors = FALSE
        )
        current_id <- current_id + 1
        next
      }

      # Check if this match is for our target league
      if (match_league != league) {
        # Valid match but different league - count as miss for THIS league
        # This prevents us from scanning through 1000+ IDs of other leagues
        consecutive_misses <- consecutive_misses + 1
        league_results[[length(league_results) + 1]] <- data.frame(
          match_id = current_id,
          league = match_league,
          season = match_season,
          status = "different_league",
          stringsAsFactors = FALSE
        )
        current_id <- current_id + 1
        next
      }

      # Found a match for this league! Reset miss counter
      consecutive_misses <- 0

      # Check if already in manifest
      if (current_id %in% manifest$match_id) {
        league_results[[length(league_results) + 1]] <- data.frame(
          match_id = current_id,
          league = match_league,
          season = match_season,
          status = "cached",
          stringsAsFactors = FALSE
        )
        if (verbose) {
          cat(sprintf("  [%d] Cached: %s vs %s\n",
                      current_id, metadata$home_team, metadata$away_team))
        }
        current_id <- current_id + 1
        next
      }

      # Scrape full match data
      events <- tryCatch({
        extract_understat_events(page, current_id)
      }, error = function(e) NULL)

      api_data <- tryCatch({
        fetch_understat_match_api(current_id)
      }, error = function(e) NULL)

      roster <- tryCatch({
        extract_understat_roster(api_data, current_id)
      }, error = function(e) NULL)

      shots <- tryCatch({
        extract_understat_shots(api_data, current_id)
      }, error = function(e) NULL)

      # Add league/season context
      metadata$league_code <- match_league
      metadata$season_input <- as.character(match_season)

      if (!is.null(events)) {
        events$league_code <- match_league
        events$season_input <- as.character(match_season)
      }
      if (!is.null(roster)) {
        roster$league_code <- match_league
        roster$season_input <- as.character(match_season)
      }
      if (!is.null(shots)) {
        shots$league_code <- match_league
        shots$season_input <- as.character(match_season)
      }

      # Save to cache
      save_understat_table(metadata, match_league, match_season, "metadata")
      if (!is.null(events)) save_understat_table(events, match_league, match_season, "events")
      if (!is.null(roster)) save_understat_table(roster, match_league, match_season, "roster")
      if (!is.null(shots)) save_understat_table(shots, match_league, match_season, "shots")

      # Add to manifest
      manifest <- rbind(manifest, data.frame(
        match_id = as.integer(current_id),
        league = match_league,
        season = as.character(match_season),
        scraped_at = Sys.time(),
        stringsAsFactors = FALSE
      ))

      new_matches <- new_matches + 1
      total_new <- total_new + 1

      league_results[[length(league_results) + 1]] <- data.frame(
        match_id = current_id,
        league = match_league,
        season = match_season,
        status = "success",
        stringsAsFactors = FALSE
      )

      if (verbose) {
        cat(sprintf("  [%d] NEW: %s vs %s (%s %s)\n",
                    current_id, metadata$home_team, metadata$away_team,
                    match_league, match_season))
      }

      current_id <- current_id + 1
    }

    # League scan complete
    scanned_count <- current_id - start_id
    if (verbose) {
      cat(sprintf("\n%s: scraped %d new matches (scanned %d IDs, stopped after %d consecutive misses)\n",
                  league, new_matches, scanned_count, max_misses))
    }

    results[[league]] <- rbindlist(league_results, use.names = TRUE, fill = TRUE)
  }

  # Save updated manifest
  save_understat_manifest(manifest, manifest_path)
  if (verbose) {
    message(sprintf("\nSaved manifest with %d total matches", nrow(manifest)))
    message(sprintf("Total new matches scraped: %d", total_new))
  }

  rbindlist(results, use.names = TRUE, fill = TRUE)
}


# Import null coalescing operator from rlang
#' @importFrom rlang %||%
NULL
