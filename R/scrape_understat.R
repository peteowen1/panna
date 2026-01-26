# Understat scraping functions
#
# These functions scrape match data from Understat, which embeds JSON data
# in <script> tags. Understat covers Big 5 leagues + Russia with detailed
# xG data at the shot level.

#' @importFrom jsonlite fromJSON
#' @importFrom stringi stri_unescape_unicode
NULL

# ============================================================================
# HTTP Layer
# ============================================================================

#' Get browser headers for Understat requests
#'
#' Returns headers that mimic a real browser.
#'
#' @param referer Optional referer URL (default: Understat home)
#'
#' @return Named character vector of HTTP headers
#' @keywords internal
get_understat_headers <- function(referer = "https://understat.com/") {
  # Pool of realistic User-Agent strings
  user_agents <- c(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0"
  )

  ua <- sample(user_agents, 1)

  c(
    "User-Agent" = ua,
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = referer,
    "DNT" = "1"
  )
}


# Environment for Understat session storage
.understat_env <- new.env(parent = emptyenv())

#' Get or create Understat session
#'
#' Returns a persistent httr session handle that maintains cookies.
#'
#' @param reset If TRUE, creates a new session (default FALSE)
#'
#' @return httr handle object
#' @export
get_understat_session <- function(reset = FALSE) {
  if (!exists("session", envir = .understat_env) || reset) {
    .understat_env$session <- httr::handle("https://understat.com")
  }
  .understat_env$session
}


#' Reset Understat session
#'
#' Clears cookies and creates fresh session.
#'
#' @export
reset_understat_session <- function() {
  .understat_env$session <- httr::handle("https://understat.com")
  message("Understat session reset - cookies cleared")
}


#' Fetch Understat page HTML
#'
#' Makes HTTP request with browser headers.
#'
#' @param url Understat URL
#' @param timeout Request timeout in seconds (default 30)
#'
#' @return Parsed HTML document (rvest xml_document) or NULL on failure
#' @export
fetch_understat_page <- function(url, timeout = 30) {
  # Validate URL
  if (!grepl("understat\\.com", url)) {
    stop("Invalid Understat URL: ", url)
  }

  # Make request with session cookies
  response <- httr::GET(
    url,
    httr::add_headers(.headers = get_understat_headers()),
    httr::timeout(timeout),
    handle = get_understat_session()
  )

  status <- httr::status_code(response)

  if (status == 429) {
    warning("Rate limited by Understat (429). Stopping.")
    result <- NULL
    attr(result, "rate_limited") <- TRUE
    return(result)
  }

  if (status != 200) {
    warning("Failed to fetch ", url, " - Status: ", status)
    return(NULL)
  }

  # Parse HTML
  html_content <- httr::content(response, "text", encoding = "UTF-8")
  rvest::read_html(html_content)
}


# ============================================================================
# JSON Extraction Layer
# ============================================================================

#' Get all script tags from Understat page
#'
#' Extracts the text content of all script tags.
#'
#' @param page Parsed HTML document
#'
#' @return Character vector of script contents
#' @keywords internal
get_understat_scripts <- function(page) {
  scripts <- rvest::html_nodes(page, "script")
  rvest::html_text(scripts)
}


#' Extract JSON data from Understat script
#'
#' Understat embeds data as JSON in script tags with pattern:
#' \code{varName = JSON.parse('...')}
#'
#' @param scripts Character vector of script contents
#' @param var_name Variable name to extract (e.g., "rostersData", "shotsData")
#'
#' @return Parsed JSON as R list, or NULL if not found
#' @keywords internal
extract_json_element <- function(scripts, var_name) {
  # Pattern: varName = JSON.parse('...')
  pattern <- paste0(var_name, "\\s*=\\s*JSON\\.parse\\('(.+?)'\\)")

  for (script in scripts) {
    match <- regmatches(script, regexpr(pattern, script, perl = TRUE))
    if (length(match) > 0 && nchar(match) > 0) {
      # Extract the JSON string
      json_str <- sub(paste0(var_name, "\\s*=\\s*JSON\\.parse\\('"), "", match)
      json_str <- sub("'\\)$", "", json_str)

      # Unescape unicode and special characters
      json_str <- parse_understat_json(json_str)

      # Parse JSON
      tryCatch({
        return(jsonlite::fromJSON(json_str, simplifyVector = TRUE))
      }, error = function(e) {
        warning("Failed to parse ", var_name, " JSON: ", e$message)
        return(NULL)
      })
    }
  }

  NULL
}


#' Parse and clean Understat JSON string
#'
#' Unescapes hex and unicode characters and fixes JSON formatting issues.
#' Understat uses \\xNN hex escapes which need to be converted to unicode.
#'
#' @param json_str Raw JSON string from Understat
#'
#' @return Cleaned JSON string
#' @keywords internal
parse_understat_json <- function(json_str) {
  # Convert hex escapes (\\xNN) to unicode escapes (\\u00NN)
  # Understat uses hex format: \x7B for { instead of unicode \u007B
  json_str <- gsub("\\\\x([0-9A-Fa-f]{2})", "\\\\u00\\1", json_str)

  # Unescape unicode sequences
  json_str <- stringi::stri_unescape_unicode(json_str)

  # Fix escaped quotes
  json_str <- gsub("\\\\'", "'", json_str)

  json_str
}


# ============================================================================
# Match Data Extraction
# ============================================================================

#' Extract match metadata from Understat page
#'
#' Parses match info including teams, scores, and xG values.
#' Note: Understat uses a flat JSON structure for match_info.
#'
#' @param page Parsed HTML document
#' @param understat_id Understat match ID
#'
#' @return Data frame with match metadata (single row)
#' @keywords internal
extract_understat_metadata <- function(page, understat_id) {
  scripts <- get_understat_scripts(page)

  # Extract match_info
  match_info <- extract_json_element(scripts, "match_info")

  if (is.null(match_info)) {
    warning("Could not extract match_info for match ", understat_id)
    return(NULL)
  }

  # Understat uses flat structure:
  # team_h, team_a for names
  # h, a for team IDs
  # h_goals, a_goals for scores
  # h_xg, a_xg for expected goals

  # Build metadata data frame
  data.frame(
    understat_id = as.character(match_info$id %||% understat_id),
    match_url = get_understat_match_url(understat_id),
    home_team = as.character(match_info$team_h %||% NA_character_),
    away_team = as.character(match_info$team_a %||% NA_character_),
    home_team_id = as.character(match_info$h %||% NA_character_),
    away_team_id = as.character(match_info$a %||% NA_character_),
    home_score = as.integer(match_info$h_goals %||% NA_integer_),
    away_score = as.integer(match_info$a_goals %||% NA_integer_),
    home_xg = as.numeric(match_info$h_xg %||% NA_real_),
    away_xg = as.numeric(match_info$a_xg %||% NA_real_),
    match_date = as.character(match_info$date %||% NA_character_),
    league = as.character(match_info$league %||% NA_character_),
    season = as.character(match_info$season %||% NA_character_),
    home_shots = as.integer(match_info$h_shot %||% NA_integer_),
    away_shots = as.integer(match_info$a_shot %||% NA_integer_),
    home_shots_on_target = as.integer(match_info$h_shotOnTarget %||% NA_integer_),
    away_shots_on_target = as.integer(match_info$a_shotOnTarget %||% NA_integer_),
    home_deep = as.integer(match_info$h_deep %||% NA_integer_),
    away_deep = as.integer(match_info$a_deep %||% NA_integer_),
    home_ppda = as.numeric(match_info$h_ppda %||% NA_real_),
    away_ppda = as.numeric(match_info$a_ppda %||% NA_real_),
    stringsAsFactors = FALSE
  )
}


#' Fetch match data from Understat API
#'
#' Calls the undocumented API endpoint that returns full match data.
#' This endpoint provides rosters and shots data that isn't in the main HTML.
#'
#' @param understat_id Understat match ID
#'
#' @return List with rosters and shots data, or NULL on failure
#' @keywords internal
fetch_understat_match_api <- function(understat_id) {
  url <- sprintf("https://understat.com/getMatchData/%s", understat_id)

  response <- httr::GET(
    url,
    httr::add_headers(.headers = c(
      get_understat_headers(),
      "Accept" = "application/json, text/javascript, */*; q=0.01",
      "X-Requested-With" = "XMLHttpRequest"
    )),
    httr::timeout(30),
    handle = get_understat_session()
  )

  if (httr::status_code(response) != 200) {
    return(NULL)
  }

  content <- httr::content(response, "text", encoding = "UTF-8")

  # Parse JSON
  tryCatch({
    jsonlite::fromJSON(content, simplifyVector = FALSE)
  }, error = function(e) {
    warning("Failed to parse match API response: ", e$message)
    NULL
  })
}


#' Extract roster data from Understat API response
#'
#' Parses player statistics from the API rosters data.
#'
#' @param api_data List returned from fetch_understat_match_api
#' @param understat_id Understat match ID
#'
#' @return Data frame with player stats, or NULL if not found
#' @keywords internal
extract_understat_roster <- function(api_data, understat_id) {
  if (is.null(api_data) || is.null(api_data$rosters)) {
    return(NULL)
  }

  rosters <- api_data$rosters

  if (is.null(rosters)) {
    return(NULL)
  }

  # Process home and away rosters
  process_team_roster <- function(team_data, is_home) {
    if (is.null(team_data) || length(team_data) == 0) {
      return(NULL)
    }

    # Handle both list and data frame formats
    if (is.data.frame(team_data)) {
      df <- team_data
    } else if (is.list(team_data)) {
      df <- tryCatch({
        dplyr::bind_rows(lapply(team_data, as.data.frame, stringsAsFactors = FALSE))
      }, error = function(e) {
        do.call(rbind, lapply(team_data, function(x) {
          as.data.frame(x, stringsAsFactors = FALSE)
        }))
      })
    } else {
      return(NULL)
    }

    if (nrow(df) == 0) return(NULL)

    df$is_home <- is_home
    df$understat_id <- as.character(understat_id)
    df
  }

  home_roster <- process_team_roster(rosters$h, TRUE)
  away_roster <- process_team_roster(rosters$a, FALSE)

  # Combine
  result <- dplyr::bind_rows(home_roster, away_roster)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Clean column names
  result <- clean_column_names(result)

  # Ensure required columns exist
  result$understat_id <- as.character(understat_id)

  result
}


#' Extract shots data from Understat API response
#'
#' Parses shot-level xG data from the API shots data.
#'
#' @param api_data List returned from fetch_understat_match_api
#' @param understat_id Understat match ID
#'
#' @return Data frame with shot data, or NULL if not found
#' @keywords internal
extract_understat_shots <- function(api_data, understat_id) {
  if (is.null(api_data) || is.null(api_data$shots)) {
    return(NULL)
  }

  shots <- api_data$shots

  if (is.null(shots)) {
    return(NULL)
  }

  # Process home and away shots
  process_team_shots <- function(team_shots, is_home) {
    if (is.null(team_shots) || length(team_shots) == 0) {
      return(NULL)
    }

    # Convert list of shots to data frame
    # Need to handle NULL/missing fields (e.g., player_assisted is often missing)
    if (is.data.frame(team_shots)) {
      df <- team_shots
    } else if (is.list(team_shots)) {
      # Normalize each shot: replace NULL with NA, convert to list
      normalized <- lapply(team_shots, function(shot) {
        # Convert each field, replacing NULL/empty with NA
        lapply(shot, function(val) {
          if (is.null(val) || length(val) == 0) {
            NA_character_
          } else {
            as.character(val)
          }
        })
      })

      df <- tryCatch({
        dplyr::bind_rows(lapply(normalized, as.data.frame, stringsAsFactors = FALSE))
      }, error = function(e) {
        # Fallback: manually create data frame
        all_cols <- unique(unlist(lapply(normalized, names)))
        rows <- lapply(normalized, function(shot) {
          row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
          for (col in names(shot)) {
            row[col] <- shot[[col]]
          }
          as.data.frame(as.list(row), stringsAsFactors = FALSE)
        })
        do.call(rbind, rows)
      })
    } else {
      return(NULL)
    }

    if (is.null(df) || nrow(df) == 0) return(NULL)

    df$is_home <- is_home
    df
  }

  home_shots <- process_team_shots(shots$h, TRUE)
  away_shots <- process_team_shots(shots$a, FALSE)

  # Combine
  result <- dplyr::bind_rows(home_shots, away_shots)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Clean column names
  result <- clean_column_names(result)

  # Add match ID
  result$understat_id <- as.character(understat_id)

  result
}


#' Extract timeline events from Understat match page
#'
#' Parses the HTML timeline to extract goals, substitutions, and cards.
#' This data IS available in the initial HTML (unlike roster/shots).
#'
#' @param page Parsed HTML document
#' @param understat_id Understat match ID
#'
#' @return Data frame with events (goals, subs, cards), or NULL if not found
#' @keywords internal
extract_understat_events <- function(page, understat_id) {
  # Find all timeline items
  timeline_items <- rvest::html_nodes(page, ".timeline-item")

  if (length(timeline_items) == 0) {
    return(NULL)
  }

  events <- lapply(timeline_items, function(item) {
    # Get minute
    minute_node <- rvest::html_node(item, ".timeline-time span")
    minute <- if (!is.na(minute_node)) {
      gsub("'", "", rvest::html_text(minute_node, trim = TRUE))
    } else {
      NA_character_
    }

    # Determine team (home = no -right class, away = has -right class)
    item_class <- rvest::html_attr(item, "class")
    is_away <- grepl("timeline-item-right", item_class)
    team_side <- if (is_away) "away" else "home"

    # Check for different event types
    content <- rvest::html_node(item, ".timeline-content")

    # Goal
    goal_icon <- rvest::html_node(content, ".fa-futbol")
    if (!is.na(goal_icon)) {
      player_node <- rvest::html_node(content, ".timeline-player-name")
      player_name <- if (!is.na(player_node)) rvest::html_text(player_node, trim = TRUE) else NA_character_
      player_url <- if (!is.na(player_node)) rvest::html_attr(player_node, "href") else NA_character_
      player_id <- if (!is.na(player_url)) gsub(".*/player/", "", player_url) else NA_character_

      score_node <- rvest::html_node(content, ".timeline-match-score")
      score <- if (!is.na(score_node)) rvest::html_text(score_node, trim = TRUE) else NA_character_

      return(data.frame(
        minute = minute,
        event_type = "goal",
        team_side = team_side,
        player = player_name,
        player_id = player_id,
        player_off = NA_character_,
        player_off_id = NA_character_,
        card_type = NA_character_,
        score = score,
        stringsAsFactors = FALSE
      ))
    }

    # Yellow card
    yellow_card <- rvest::html_node(content, ".yellow-card")
    if (!is.na(yellow_card)) {
      player_node <- rvest::html_node(content, ".timeline-player-name")
      player_name <- if (!is.na(player_node)) rvest::html_text(player_node, trim = TRUE) else NA_character_
      player_url <- if (!is.na(player_node)) rvest::html_attr(player_node, "href") else NA_character_
      player_id <- if (!is.na(player_url)) gsub(".*/player/", "", player_url) else NA_character_

      return(data.frame(
        minute = minute,
        event_type = "card",
        team_side = team_side,
        player = player_name,
        player_id = player_id,
        player_off = NA_character_,
        player_off_id = NA_character_,
        card_type = "yellow",
        score = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    # Red card
    red_card <- rvest::html_node(content, ".red-card")
    if (!is.na(red_card)) {
      player_node <- rvest::html_node(content, ".timeline-player-name")
      player_name <- if (!is.na(player_node)) rvest::html_text(player_node, trim = TRUE) else NA_character_
      player_url <- if (!is.na(player_node)) rvest::html_attr(player_node, "href") else NA_character_
      player_id <- if (!is.na(player_url)) gsub(".*/player/", "", player_url) else NA_character_

      return(data.frame(
        minute = minute,
        event_type = "card",
        team_side = team_side,
        player = player_name,
        player_id = player_id,
        player_off = NA_character_,
        player_off_id = NA_character_,
        card_type = "red",
        score = NA_character_,
        stringsAsFactors = FALSE
      ))
    }

    # Substitution (has both arrow-down and arrow-up icons)
    sub_off <- rvest::html_node(content, ".fa-long-arrow-alt-down")
    if (!is.na(sub_off)) {
      # Find all timeline-row elements (there can be multiple subs at same minute)
      rows <- rvest::html_nodes(content, ".timeline-row")

      sub_events <- lapply(rows, function(row) {
        # Player going off (before the down arrow)
        off_group <- rvest::html_node(row, "[title='Substituted off']")
        if (is.na(off_group)) off_group <- row

        player_off_node <- rvest::html_node(off_group, ".timeline-player-name")
        player_off <- if (!is.na(player_off_node)) rvest::html_text(player_off_node, trim = TRUE) else NA_character_
        player_off_url <- if (!is.na(player_off_node)) rvest::html_attr(player_off_node, "href") else NA_character_
        player_off_id <- if (!is.na(player_off_url)) gsub(".*/player/", "", player_off_url) else NA_character_

        # Player coming on
        on_group <- rvest::html_node(row, "[title='Substituted on']")
        player_on_node <- if (!is.na(on_group)) rvest::html_node(on_group, ".timeline-player-name") else NA
        player_on <- if (!is.na(player_on_node)) rvest::html_text(player_on_node, trim = TRUE) else NA_character_
        player_on_url <- if (!is.na(player_on_node)) rvest::html_attr(player_on_node, "href") else NA_character_
        player_on_id <- if (!is.na(player_on_url)) gsub(".*/player/", "", player_on_url) else NA_character_

        data.frame(
          minute = minute,
          event_type = "substitution",
          team_side = team_side,
          player = player_on,
          player_id = player_on_id,
          player_off = player_off,
          player_off_id = player_off_id,
          card_type = NA_character_,
          score = NA_character_,
          stringsAsFactors = FALSE
        )
      })

      return(dplyr::bind_rows(sub_events))
    }

    # No recognized event
    NULL
  })

  # Combine all events
  result <- dplyr::bind_rows(events)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Add match ID
  result$understat_id <- as.character(understat_id)

  # Clean column names
  result <- clean_column_names(result)

  result
}


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
  # Get league URL
  url <- get_understat_league_url(league, season)

  progress_msg(sprintf("Fetching Understat fixtures: %s %s", league, season))

  # Fetch page
  page <- fetch_understat_page(url)

  if (is.null(page)) {
    warning("Failed to fetch Understat league page")
    return(NULL)
  }

  # Extract datesData which contains all fixtures
  # NOTE: This data is loaded via JavaScript and may not be present
  scripts <- get_understat_scripts(page)
  dates_data <- extract_json_element(scripts, "datesData")

  if (is.null(dates_data)) {
    warning("Could not extract datesData from league page. ",
            "Understat loads fixtures via JavaScript. ",
            "Use scrape_understat_match_range() as an alternative.")
    return(NULL)
  }

  # Convert to data frame
  if (is.list(dates_data) && !is.data.frame(dates_data)) {
    fixtures <- tryCatch({
      dplyr::bind_rows(lapply(dates_data, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
    }, error = function(e) {
      do.call(rbind, lapply(dates_data, function(x) {
        as.data.frame(x, stringsAsFactors = FALSE)
      }))
    })
  } else {
    fixtures <- as.data.frame(dates_data, stringsAsFactors = FALSE)
  }

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    warning("No fixtures found")
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
    stop("start_id must be <= end_id")
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
        warning("Match ", match_id, ": ", e$message)
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
    combined <- dplyr::bind_rows(existing_data, data)
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
#' @export
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
      warning("Error scraping match ", id, ": ", e$message)
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
    stop("League '", league, "' is not available on Understat. ",
         "Valid leagues: ", paste(list_understat_competitions(), collapse = ", "))
  }

  # Get fixtures
  fixtures <- scrape_understat_fixtures(league, season)

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    warning("No fixtures found for ", league, " ", season)
    return(invisible(list(scraped = 0, cached = 0, failed = 0)))
  }

  # Extract match IDs
  id_col <- if ("id" %in% names(fixtures)) "id" else "match_id"
  if (!id_col %in% names(fixtures)) {
    warning("Could not find match ID column in fixtures")
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
#' @export
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

  dplyr::bind_rows(all_data)
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
    stop("start_id must be <= end_id")
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

  dplyr::bind_rows(results)
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
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Package 'arrow' is required. Install with: install.packages('arrow')")
  }

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
          if (verbose) warning(sprintf("  Error reading %s: %s", basename(f), e$message))
          NULL
        })
      })
      dfs <- Filter(Negate(is.null), dfs)
      if (length(dfs) == 0) return(NULL)
      do.call(rbind, dfs)
    }, error = function(e) {
      if (verbose) warning(sprintf("  Error combining %s: %s", tt, e$message))
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


# Import null coalescing operator from rlang
#' @importFrom rlang %||%
NULL
