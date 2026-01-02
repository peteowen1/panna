# Data collection functions for football data from FBref via worldfootballR
#
# Supports multiple leagues (Big 5 European leagues):
# - ENG: England (Premier League)
# - ESP: Spain (La Liga)
# - GER: Germany (Bundesliga)
# - ITA: Italy (Serie A)
# - FRA: France (Ligue 1)

#' Big 5 European league configurations
#'
#' @return Data frame with league info
#' @export
get_big5_leagues <- function() {
  data.frame(
    country = c("ENG", "ESP", "GER", "ITA", "FRA"),
    league = c("Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1"),
    tier = rep("1st", 5),
    stringsAsFactors = FALSE
  )
}


#' Scrape comprehensive data for multiple leagues
#'
#' Master function that collects all required data for multiple leagues.
#' Combines data across leagues with league identifiers for cross-league analysis.
#'
#' @param leagues Character vector of country codes (e.g., c("ENG", "ESP")) or
#'   "big5" for all Big 5 leagues
#' @param seasons Character vector of seasons (e.g., c("2022-2023", "2023-2024"))
#' @param delay Seconds between requests (default 3)
#' @param verbose Logical, print progress messages
#'
#' @return List containing all data frames with league column added
#' @export
scrape_multi_league <- function(leagues = "big5", seasons, delay = 3, verbose = TRUE) {
  # Handle "big5" shortcut
 if (length(leagues) == 1 && leagues == "big5") {
    league_info <- get_big5_leagues()
  } else {
    all_leagues <- get_big5_leagues()
    league_info <- all_leagues[all_leagues$country %in% leagues, ]
  }

  if (nrow(league_info) == 0) {
    stop("No valid leagues specified. Use country codes: ENG, ESP, GER, ITA, FRA")
  }

  validate_seasons(seasons)
  season_years <- as.numeric(substr(seasons, 6, 9))

  if (verbose) {
    progress_msg(sprintf("Collecting data for %d leagues, %d seasons",
                         nrow(league_info), length(seasons)))
    progress_msg(paste("Leagues:", paste(league_info$league, collapse = ", ")))
  }

  # Initialize combined data
  combined <- list(
    results = NULL,
    stats_summary = NULL,
    stats_passing = NULL,
    stats_defense = NULL,
    stats_possession = NULL,
    lineups = NULL,
    shooting = NULL,
    events = NULL
  )

  # Collect data for each league
  for (i in seq_len(nrow(league_info))) {
    country <- league_info$country[i]
    league_name <- league_info$league[i]
    tier <- league_info$tier[i]

    if (verbose) {
      progress_msg(sprintf("\n=== %s (%s) ===", league_name, country))
    }

    # Collect data for this league
    league_data <- scrape_league_data(
      country = country,
      tier = tier,
      seasons = seasons,
      delay = delay,
      verbose = verbose
    )

    # Add league column to all data frames
    for (name in names(league_data)) {
      if (!is.null(league_data[[name]]) && nrow(league_data[[name]]) > 0) {
        league_data[[name]]$league <- league_name
        league_data[[name]]$country <- country

        # Ensure consistent column types before binding (some leagues have different types)
        if (!is.null(combined[[name]]) && nrow(combined[[name]]) > 0) {
          # Get common columns
          common_cols <- intersect(names(combined[[name]]), names(league_data[[name]]))
          for (col in common_cols) {
            # If types differ, convert both to character
            if (class(combined[[name]][[col]])[1] != class(league_data[[name]][[col]])[1]) {
              combined[[name]][[col]] <- as.character(combined[[name]][[col]])
              league_data[[name]][[col]] <- as.character(league_data[[name]][[col]])
            }
          }
        }

        # Combine with existing data
        combined[[name]] <- dplyr::bind_rows(combined[[name]], league_data[[name]])
      }
    }
  }

  # Create league_season column for all data
  for (name in names(combined)) {
    if (!is.null(combined[[name]]) && nrow(combined[[name]]) > 0) {
      if ("season_end_year" %in% names(combined[[name]]) && "league" %in% names(combined[[name]])) {
        combined[[name]]$league_season <- paste(
          combined[[name]]$league,
          combined[[name]]$season_end_year,
          sep = "_"
        )
      }
    }
  }

  if (verbose) {
    progress_msg("\n=== Multi-League Collection Complete ===")
    for (name in names(combined)) {
      if (!is.null(combined[[name]])) {
        n_leagues <- length(unique(combined[[name]]$league))
        progress_msg(sprintf("  %s: %d rows across %d leagues",
                             name, nrow(combined[[name]]), n_leagues))
      }
    }
  }

  combined
}


#' Scrape data for a single league
#'
#' Internal function to collect all data for one league.
#'
#' @param country Country code (ENG, ESP, GER, ITA, FRA)
#' @param tier League tier (usually "1st")
#' @param seasons Character vector of seasons
#' @param delay Seconds between requests
#' @param verbose Print progress
#'
#' @return List of data frames for this league
#' @keywords internal
scrape_league_data <- function(country, tier, seasons, delay = 3, verbose = TRUE) {
  season_years <- as.numeric(substr(seasons, 6, 9))

  data <- list()

  # Match results
  if (verbose) progress_msg("Loading match results...")
  tryCatch({
    results <- worldfootballR::load_match_results(
      country = country,
      gender = "M",
      season_end_year = season_years,
      tier = tier
    )
    if (!is.null(results) && nrow(results) > 0) {
      data$results <- clean_column_names(results)
    }
  }, error = function(e) {
    if (verbose) progress_msg(paste("  Error:", e$message))
  })

  if (is.null(data$results) || nrow(data$results) == 0) {
    warning(paste("No match results for", country))
    return(data)
  }

  match_urls <- unique(data$results$match_url)
  if (verbose) progress_msg(sprintf("  Found %d matches", length(match_urls)))

  # Advanced stats
  stat_types <- c("summary", "passing", "defense", "possession")
  for (stat_type in stat_types) {
    if (verbose) progress_msg(paste("Loading", stat_type, "stats..."))
    tryCatch({
      stats <- worldfootballR::load_fb_advanced_match_stats(
        country = country,
        gender = "M",
        tier = tier,
        stat_type = stat_type,
        team_or_player = "player",
        season_end_year = season_years
      )
      if (!is.null(stats) && nrow(stats) > 0) {
        stats <- clean_column_names(stats)
        stats <- stats[stats$match_url %in% match_urls, ]
        data[[paste0("stats_", stat_type)]] <- stats
        if (verbose) progress_msg(sprintf("  Loaded %d rows", nrow(stats)))
      }
    }, error = function(e) {
      if (verbose) progress_msg(paste("  Error:", e$message))
    })
  }

  # Lineups from passing stats
  if (!is.null(data$stats_passing) && nrow(data$stats_passing) > 0) {
    if (verbose) progress_msg("Deriving lineups from passing stats...")
    data$lineups <- derive_lineups_from_stats(data$stats_passing)
  }

  # Shooting data
  if (verbose) progress_msg("Loading shooting data...")
  tryCatch({
    shooting <- worldfootballR::load_fb_match_shooting(
      country = country,
      gender = "M",
      tier = tier
    )
    if (!is.null(shooting) && nrow(shooting) > 0) {
      shooting <- clean_column_names(shooting)
      shooting <- shooting[shooting$match_url %in% match_urls, ]
      data$shooting <- shooting
      if (verbose) progress_msg(sprintf("  Loaded %d shots", nrow(shooting)))
    }
  }, error = function(e) {
    if (verbose) progress_msg(paste("  Error:", e$message))
  })

  # Events/match summary
  if (verbose) progress_msg("Loading match events...")
  tryCatch({
    events <- worldfootballR::load_fb_match_summary(
      country = country,
      gender = "M",
      tier = tier
    )
    if (!is.null(events) && nrow(events) > 0) {
      events <- clean_column_names(events)
      events <- events[events$match_url %in% match_urls, ]
      data$events <- events
      if (verbose) progress_msg(sprintf("  Loaded %d events", nrow(events)))
    }
  }, error = function(e) {
    if (verbose) progress_msg(paste("  Error:", e$message))
  })

  # Derive events from shooting if needed
  if ((is.null(data$events) || nrow(data$events) == 0) &&
      !is.null(data$shooting) && nrow(data$shooting) > 0) {
    if (verbose) progress_msg("Deriving goal events from shooting data...")
    data$events <- derive_events_from_shooting(data$shooting)
  }

  # Add season_end_year from results
  if (!is.null(data$results)) {
    match_season_lookup <- stats::setNames(
      data$results$season_end_year,
      data$results$match_url
    )
    for (name in names(data)) {
      data[[name]] <- add_season_column(data[[name]], match_season_lookup)
    }
  }

  data
}

#' Get cache directory path
#'
#' @param create Logical, whether to create if doesn't exist
#' @return Path to cache directory
#' @keywords internal
get_cache_dir <- function(create = TRUE)

{

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
#' @export
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
#' @export
save_cached_data <- function(data, data_type) {
  cache_file <- file.path(get_cache_dir(), paste0("cached_", data_type, ".rds"))
  saveRDS(data, cache_file)
  progress_msg(paste("Saved", nrow(data), "rows to", data_type, "cache"))
}


#' Get already-scraped match URLs from cache
#'
#' @param data_type Type of data to check
#' @return Character vector of match URLs already in cache
#' @keywords internal
get_cached_match_urls <- function(data_type) {
  cached <- load_cached_data(data_type)
  if (!is.null(cached) && "MatchURL" %in% names(cached)) {
    unique(cached$MatchURL)
  } else if (!is.null(cached) && "match_url" %in% names(cached)) {
    unique(cached$match_url)
  } else {
    character(0)
  }
}


#' Scrape Premier League match results
#'
#' Fetches match results including scores and expected goals for specified seasons.
#'
#' @param seasons Character vector of seasons (e.g., c("2022-2023", "2023-2024"))
#' @param use_cached Logical, whether to use pre-scraped data if available (default TRUE)
#'
#' @return Data frame of match results
#' @export
#'
#' @examples
#' \dontrun{
#' results <- scrape_pl_match_results("2023-2024")
#' }
scrape_pl_match_results <- function(seasons, use_cached = TRUE) {
  validate_seasons(seasons)

  if (use_cached) {
    # Try to load pre-scraped data first (from worldfootballR's cache)
    tryCatch({
      data <- worldfootballR::load_match_results(
        country = "ENG",
        gender = "M",
        season_end_year = as.numeric(substr(seasons, 6, 9)),
        tier = "1st"
      )
      progress_msg("Loaded cached match results from worldfootballR")
      # Clean column names to snake_case
      data <- clean_column_names(data)
      return(data)
    }, error = function(e) {
      progress_msg("worldfootballR cache not available, scraping fresh data...")
    })
  }

  # Scrape fresh data
  all_results <- purrr::map_dfr(seasons, function(season) {
    end_year <- as.numeric(substr(season, 6, 9))
    progress_msg(paste("Scraping match results for", season))

    worldfootballR::fb_match_results(
      country = "ENG",
      gender = "M",
      season_end_year = end_year,
      tier = "1st"
    )
  })

  all_results
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
#' @export
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
    is_own_goal = grepl("Own Goal", goals$notes, ignore.case = TRUE),
    stringsAsFactors = FALSE
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
#' @param stats_data Stats data from load_fb_advanced_match_stats (already snake_case).
#'   Can be any stat type - passing/defense/possession have better historical coverage
#'   than summary in the worldfootballR cache.
#'
#' @return Data frame of match lineups derived from stats
#' @export
derive_lineups_from_stats <- function(stats_data) {
 if (is.null(stats_data) || nrow(stats_data) == 0) {
    warning("No stats data to derive lineups from")
    return(NULL)
  }

  # Players who appear in stats played in the match
  # min column tells us how many minutes they played
  # Column names are already snake_case from clean_column_names()
  lineups <- stats_data %>%
    dplyr::select(dplyr::any_of(c(
      "match_url", "team", "player", "player_href", "nation", "pos", "age", "min",
      "home_away", "season_end_year"
    ))) %>%
    dplyr::mutate(
      is_starter = as.numeric(.data$min) >= 45,  # Approximate: 45+ min = likely starter
      minutes = as.numeric(.data$min),
      is_home = .data$home_away == "Home"
    ) %>%
    dplyr::rename(
      player_name = .data$player
    )

  lineups
}


#' Scrape match lineups with incremental caching
#'
#' Fetches starting lineups and substitutes for matches.
#' Skips matches that have already been scraped and cached.
#' NOTE: Consider using derive_lineups_from_stats() instead to avoid rate limits.
#'
#' @param match_urls Character vector of FBref match URLs
#' @param delay Seconds to wait between requests (default 3)
#' @param use_cache Whether to use/update incremental cache (default TRUE)
#'
#' @return Data frame of match lineups
#' @export
#'
#' @examples
#' \dontrun{
#' lineups <- scrape_pl_match_lineups(match_urls)
#' }
scrape_pl_match_lineups <- function(match_urls, delay = 3, use_cache = TRUE) {
  # Load existing cached data
  cached_data <- NULL
  cached_urls <- character(0)

  if (use_cache) {
    cached_data <- load_cached_data("lineups")
    if (!is.null(cached_data)) {
      cached_urls <- unique(cached_data$MatchURL)
      progress_msg(paste("Found", length(cached_urls), "matches already cached for lineups"))
    }
  }

  # Filter to only new matches
  new_urls <- setdiff(match_urls, cached_urls)

  if (length(new_urls) == 0) {
    progress_msg("All lineups already cached, skipping scrape")
    return(cached_data)
  }

  progress_msg(paste("Scraping lineups for", length(new_urls), "new matches (skipping", length(cached_urls), "cached)"))

  new_lineups <- purrr::map_dfr(seq_along(new_urls), function(i) {
    if (i %% 10 == 0) {
      progress_msg(paste("  Processing match", i, "of", length(new_urls)))
    }

    tryCatch({
      result <- worldfootballR::fb_match_lineups(new_urls[i])
      Sys.sleep(delay)
      result
    }, error = function(e) {
      # Don't warn for known missing data (older seasons)
      NULL
    })
  })

  # Combine with cached data
  if (!is.null(cached_data) && !is.null(new_lineups) && nrow(new_lineups) > 0) {
    all_lineups <- dplyr::bind_rows(cached_data, new_lineups)
  } else if (!is.null(new_lineups) && nrow(new_lineups) > 0) {
    all_lineups <- new_lineups
  } else {
    all_lineups <- cached_data
  }

  # Save updated cache
  if (use_cache && !is.null(all_lineups)) {
    save_cached_data(all_lineups, "lineups")
  }

  all_lineups
}


#' Scrape match summary (events) with incremental caching
#'
#' Fetches match events including goals and substitutions with timing.
#' Skips matches that have already been scraped and cached.
#'
#' @param match_urls Character vector of FBref match URLs
#' @param delay Seconds to wait between requests (default 3)
#' @param use_cache Whether to use/update incremental cache (default TRUE)
#'
#' @return Data frame of match events
#' @export
#'
#' @examples
#' \dontrun{
#' events <- scrape_pl_match_summary(match_urls)
#' }
scrape_pl_match_summary <- function(match_urls, delay = 3, use_cache = TRUE) {
  # Load existing cached data
  cached_data <- NULL
  cached_urls <- character(0)

  if (use_cache) {
    cached_data <- load_cached_data("events")
    if (!is.null(cached_data)) {
      cached_urls <- unique(cached_data$MatchURL)
      progress_msg(paste("Found", length(cached_urls), "matches already cached for events"))
    }
  }

  # Filter to only new matches
  new_urls <- setdiff(match_urls, cached_urls)

  if (length(new_urls) == 0) {
    progress_msg("All events already cached, skipping scrape")
    return(cached_data)
  }

  progress_msg(paste("Scraping events for", length(new_urls), "new matches (skipping", length(cached_urls), "cached)"))

  new_events <- purrr::map_dfr(seq_along(new_urls), function(i) {
    if (i %% 10 == 0) {
      progress_msg(paste("  Processing match", i, "of", length(new_urls)))
    }

    tryCatch({
      result <- worldfootballR::fb_match_summary(new_urls[i])
      Sys.sleep(delay)
      result
    }, error = function(e) {
      NULL
    })
  })

  # Combine with cached data
  if (!is.null(cached_data) && !is.null(new_events) && nrow(new_events) > 0) {
    all_events <- dplyr::bind_rows(cached_data, new_events)
  } else if (!is.null(new_events) && nrow(new_events) > 0) {
    all_events <- new_events
  } else {
    all_events <- cached_data
  }

  # Save updated cache
  if (use_cache && !is.null(all_events)) {
    save_cached_data(all_events, "events")
  }

  all_events
}


#' Scrape match shooting data with incremental caching
#'
#' Fetches shot-level data including xG for each shot.
#' Uses worldfootballR pre-scraped data when available, with incremental caching.
#'
#' @param match_urls Character vector of FBref match URLs
#' @param delay Seconds to wait between requests (default 3)
#' @param use_cache Whether to use/update incremental cache (default TRUE)
#'
#' @return Data frame of shot data
#' @export
#'
#' @examples
#' \dontrun{
#' shots <- scrape_pl_match_shooting(match_urls)
#' }
scrape_pl_match_shooting <- function(match_urls, delay = 3, use_cache = TRUE) {
  # First try worldfootballR's pre-scraped data (much faster)
  tryCatch({
    data <- worldfootballR::load_fb_match_shooting(
      country = "ENG",
      gender = "M",
      tier = "1st"
    )
    if (!is.null(data) && nrow(data) > 0) {
      progress_msg("Loaded shooting data from worldfootballR cache")
      # Filter to requested matches
      data <- data[data$MatchURL %in% match_urls, ]
      # Clean column names to snake_case
      data <- clean_column_names(data)
      return(data)
    }
  }, error = function(e) {
    progress_msg("worldfootballR shooting cache not available...")
  })

  # Fall back to incremental scraping with local cache
  cached_data <- NULL
  cached_urls <- character(0)

  if (use_cache) {
    cached_data <- load_cached_data("shooting")
    if (!is.null(cached_data)) {
      cached_urls <- unique(cached_data$MatchURL)
      progress_msg(paste("Found", length(cached_urls), "matches already cached for shooting"))
    }
  }

  # Filter to only new matches
  new_urls <- setdiff(match_urls, cached_urls)

  if (length(new_urls) == 0) {
    progress_msg("All shooting data already cached, skipping scrape")
    return(cached_data[cached_data$MatchURL %in% match_urls, ])
  }

  progress_msg(paste("Scraping shooting for", length(new_urls), "new matches"))

  new_shots <- purrr::map_dfr(seq_along(new_urls), function(i) {
    if (i %% 10 == 0) {
      progress_msg(paste("  Processing match", i, "of", length(new_urls)))
    }

    tryCatch({
      result <- worldfootballR::fb_match_shooting(new_urls[i])
      Sys.sleep(delay)
      result
    }, error = function(e) {
      NULL
    })
  })

  # Combine with cached data
  if (!is.null(cached_data) && !is.null(new_shots) && nrow(new_shots) > 0) {
    all_shots <- dplyr::bind_rows(cached_data, new_shots)
  } else if (!is.null(new_shots) && nrow(new_shots) > 0) {
    all_shots <- new_shots
  } else {
    all_shots <- cached_data
  }

  # Save updated cache
  if (use_cache && !is.null(all_shots)) {
    save_cached_data(all_shots, "shooting")
  }

  # Return only requested matches
  if (!is.null(all_shots)) {
    all_shots[all_shots$MatchURL %in% match_urls, ]
  } else {
    all_shots
  }
}


#' Scrape advanced match stats with incremental caching
#'
#' Fetches advanced player statistics for matches.
#' Uses worldfootballR pre-scraped data when available.
#'
#' @param match_urls Character vector of FBref match URLs
#' @param stat_type Type of stats: "summary", "passing", "passing_types",
#'   "defense", "possession", "misc", "keeper"
#' @param delay Seconds to wait between requests (default 3)
#' @param use_cache Whether to use/update incremental cache (default TRUE)
#' @param season_end_years Optional integer vector of season end years for cache lookup
#'
#' @return Data frame of advanced stats
#' @export
#'
#' @examples
#' \dontrun{
#' passing <- scrape_pl_advanced_match_stats(match_urls, "passing")
#' }
scrape_pl_advanced_match_stats <- function(match_urls, stat_type = "summary", delay = 3,
                                            use_cache = TRUE, season_end_years = NULL) {
  valid_types <- c("summary", "passing", "passing_types", "defense",
                   "possession", "misc", "keeper")
  if (!stat_type %in% valid_types) {
    stop("stat_type must be one of: ", paste(valid_types, collapse = ", "))
  }

  # First try worldfootballR's pre-scraped data (much faster)
  tryCatch({
    data <- worldfootballR::load_fb_advanced_match_stats(
      country = "ENG",
      gender = "M",
      tier = "1st",
      stat_type = stat_type,
      team_or_player = "player",
      season_end_year = season_end_years  # Request specific seasons
    )
    if (!is.null(data) && nrow(data) > 0) {
      progress_msg(paste("Loaded", stat_type, "data from worldfootballR cache:", nrow(data), "rows"))
      # Filter to requested matches
      data <- data[data$MatchURL %in% match_urls, ]
      progress_msg(paste("After filtering to requested matches:", nrow(data), "rows"))
      if (nrow(data) > 0) {
        # Clean column names to snake_case
        data <- clean_column_names(data)
        return(data)
      }
    }
  }, error = function(e) {
    progress_msg(paste("worldfootballR", stat_type, "cache not available:", e$message))
  })

  # Fall back to incremental scraping with local cache
  cache_name <- paste0("stats_", stat_type)
  cached_data <- NULL
  cached_urls <- character(0)

  if (use_cache) {
    cached_data <- load_cached_data(cache_name)
    if (!is.null(cached_data)) {
      cached_urls <- unique(cached_data$MatchURL)
      progress_msg(paste("Found", length(cached_urls), "matches already cached for", stat_type))
    }
  }

  # Filter to only new matches
  new_urls <- setdiff(match_urls, cached_urls)

  if (length(new_urls) == 0) {
    progress_msg(paste("All", stat_type, "data already cached, skipping scrape"))
    return(cached_data[cached_data$MatchURL %in% match_urls, ])
  }

  progress_msg(paste("Scraping", stat_type, "for", length(new_urls), "new matches"))

  new_stats <- purrr::map_dfr(seq_along(new_urls), function(i) {
    if (i %% 10 == 0) {
      progress_msg(paste("  Processing match", i, "of", length(new_urls)))
    }

    tryCatch({
      result <- worldfootballR::fb_advanced_match_stats(
        new_urls[i],
        stat_type = stat_type
      )
      Sys.sleep(delay)
      result
    }, error = function(e) {
      NULL
    })
  })

  # Combine with cached data
  if (!is.null(cached_data) && !is.null(new_stats) && nrow(new_stats) > 0) {
    all_stats <- dplyr::bind_rows(cached_data, new_stats)
  } else if (!is.null(new_stats) && nrow(new_stats) > 0) {
    all_stats <- new_stats
  } else {
    all_stats <- cached_data
  }

  # Save updated cache
  if (use_cache && !is.null(all_stats)) {
    save_cached_data(all_stats, cache_name)
  }

  # Return only requested matches
  if (!is.null(all_stats)) {
    all_stats[all_stats$MatchURL %in% match_urls, ]
  } else {
    all_stats
  }
}


#' Scrape season player stats
#'
#' Fetches aggregated player statistics for full seasons.
#'
#' @param seasons Character vector of seasons
#' @param stat_type Type of stats: "standard", "shooting", "passing",
#'   "passing_types", "gca", "defense", "possession", "misc"
#'
#' @return Data frame of player season stats
#' @export
#'
#' @examples
#' \dontrun{
#' stats <- scrape_pl_player_stats("2023-2024", "standard")
#' }
scrape_pl_player_stats <- function(seasons, stat_type = "standard") {
  validate_seasons(seasons)

  valid_types <- c("standard", "shooting", "passing", "passing_types",
                   "gca", "defense", "possession", "misc", "playing_time")
  if (!stat_type %in% valid_types) {
    stop("stat_type must be one of: ", paste(valid_types, collapse = ", "))
  }

  all_stats <- purrr::map_dfr(seasons, function(season) {
    end_year <- as.numeric(substr(season, 6, 9))
    progress_msg(paste("Scraping", stat_type, "stats for", season))

    worldfootballR::fb_season_team_stats(
      country = "ENG",
      gender = "M",
      season_end_year = end_year,
      tier = "1st",
      stat_type = stat_type
    )
  })

  all_stats
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


#' Scrape comprehensive Premier League data
#'
#' Master function that collects all required data types for panna ratings.
#' Prioritizes worldfootballR pre-scraped data to avoid rate limiting.
#' Falls back to direct scraping only when pre-scraped data unavailable.
#'
#' @param seasons Character vector of seasons to scrape
#' @param delay Seconds between requests (default 3)
#' @param verbose Logical, print progress messages
#' @param use_cache Whether to use incremental caching (default TRUE)
#' @param prefer_prescraped Whether to prefer pre-scraped data over scraping (default TRUE)
#'
#' @return List containing all data frames, each with season_end_year column
#' @export
#'
#' @examples
#' \dontrun{
#' data <- scrape_pl_comprehensive(c("2022-2023", "2023-2024"))
#' }
scrape_pl_comprehensive <- function(seasons, delay = 3, verbose = TRUE,
                                     use_cache = TRUE, prefer_prescraped = TRUE) {
  validate_seasons(seasons)

  if (verbose) progress_msg("Starting comprehensive data collection")

  # Get match results first (uses worldfootballR cache - instant!)
  # Note: results come back with snake_case column names
  results <- scrape_pl_match_results(seasons, use_cached = TRUE)

  # Filter to requested seasons only (column is snake_case)
  season_years <- as.numeric(substr(seasons, 6, 9))
  results <- results[results$season_end_year %in% season_years, ]

  # Extract unique match URLs - keep BOTH formats for filtering
  # (some functions need original MatchURL, others need snake_case)
  match_urls_snake <- unique(results$match_url)
  # Reconstruct original format for filtering pre-cleaned data
  match_urls <- match_urls_snake  # They should be the same URLs, just column name differs
  if (verbose) progress_msg(paste("Found", length(match_urls), "matches for requested seasons"))

  # Initialize data list
  data <- list(results = results)

  # ============================================================
  # ADVANCED STATS - Use worldfootballR pre-scraped data (instant!)
  # ============================================================
  stat_types <- c("summary", "passing", "defense", "possession")

  for (stat_type in stat_types) {
    if (verbose) progress_msg(paste("Loading", stat_type, "stats..."))
    data[[paste0("stats_", stat_type)]] <- scrape_pl_advanced_match_stats(
      match_urls, stat_type, delay = delay, use_cache = use_cache,
      season_end_years = season_years  # Pass the season years for cache lookup
    )
  }

  # ============================================================
  # LINEUPS - Derive from passing stats (has better historical coverage than summary!)
  # Summary stats only has 2 seasons in worldfootballR cache, but passing has 9
  # ============================================================
  if (verbose) progress_msg("Deriving lineups from passing stats...")
  data$lineups <- derive_lineups_from_stats(data$stats_passing)

  # ============================================================
  # SHOOTING - Use worldfootballR pre-scraped data
  # ============================================================
  if (verbose) progress_msg("Loading shooting data...")
  data$shooting <- scrape_pl_match_shooting(match_urls, delay = delay, use_cache = use_cache)

  # ============================================================
  # EVENTS - Try worldfootballR cache, then scrape, then derive from shots
  # fb_match_summary release has ENG_M_1st_match_summary.rds (25.57 MB)
  # ============================================================
  data$events <- NULL

  if (prefer_prescraped) {
    if (verbose) progress_msg("Checking worldfootballR cache for match events...")
    tryCatch({
      # Load all available data first (don't filter by season_end_year in the call)
      # The worldfootballR cache file contains all seasons in one file
      events <- worldfootballR::load_fb_match_summary(
        country = "ENG",
        gender = "M",
        tier = "1st"
      )
      if (!is.null(events) && nrow(events) > 0) {
        events <- clean_column_names(events)
        # Filter to requested matches
        data$events <- events[events$match_url %in% match_urls, ]
        if (verbose) progress_msg(paste("Loaded", nrow(data$events), "events from cache"))
      }
    }, error = function(e) {
      if (verbose) progress_msg(paste("Match summary cache error:", e$message))
    })
  }

  # If no cached events, try local cache or scrape
  if (is.null(data$events) || nrow(data$events) == 0) {
    # Check if we have locally cached events
    local_events <- load_cached_data("events")
    if (!is.null(local_events) && nrow(local_events) > 0) {
      local_events <- clean_column_names(local_events)
      match_col <- if ("match_url" %in% names(local_events)) "match_url" else "MatchURL"
      data$events <- local_events[local_events[[match_col]] %in% match_urls, ]
      if (verbose && nrow(data$events) > 0) {
        progress_msg(paste("Loaded", nrow(data$events), "events from local cache"))
      }
    }
  }

  # If still no events, derive goal events from shooting data
  if ((is.null(data$events) || nrow(data$events) == 0) && !is.null(data$shooting) && nrow(data$shooting) > 0) {
    if (verbose) progress_msg("Deriving goal events from shooting data...")
    data$events <- derive_events_from_shooting(data$shooting)
    if (verbose) progress_msg(paste("Derived", nrow(data$events), "goal events from shots"))
  }

  # ============================================================
  # ENSURE ALL DATA HAS season_end_year COLUMN
  # ============================================================
  # Create lookup from match results (the authoritative source)
  match_season_lookup <- stats::setNames(
    results$season_end_year,
    results$match_url
  )

  # Add season_end_year to all data components that don't have it
  data_names <- names(data)
  for (name in data_names) {
    data[[name]] <- add_season_column(data[[name]], match_season_lookup)
  }

  if (verbose) {
    progress_msg("Data collection complete!")
    report_season_ranges(data)
  }
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
#' All data from scrape_pl_comprehensive will have this column.
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
#' @param data List of data frames from scrape_pl_comprehensive
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
      n_seasons = range_info$n_seasons,
      stringsAsFactors = FALSE
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
