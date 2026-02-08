# Data utilities for FBref football data
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
  lineups <- stats_data |>
    dplyr::select(dplyr::any_of(c(
      "match_url", "team", "player", "player_href", "nation", "pos", "age", "min",
      "home_away", "season_end_year"
    ))) |>
    dplyr::mutate(
      is_starter = as.numeric(.data$min) >= 45,  # Approximate: 45+ min = likely starter
      minutes = as.numeric(.data$min),
      is_home = .data$home_away == "Home"
    ) |>
    dplyr::rename(
      player_name = .data$player
    )

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
