# Understat API / HTTP layer
#
# HTTP requests, JSON extraction, session management, and single-match
# data extraction. Used by scrape_understat.R for batch operations.

#' @importFrom rlang %||%
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
fetch_understat_page <- function(url, timeout = 30) {
  # Validate URL
  if (!grepl("understat\\.com", url)) {
    cli::cli_abort("Invalid Understat URL: {.val {url}}")
  }

  # Make request with session cookies and retry logic
  # Note: must use httr::config() to combine configs, or name args explicitly
  # to avoid positional matching to max_retries/base_delay/max_delay
  response <- fetch_with_retry(
    url = url,
    max_retries = 3,
    base_delay = 1,
    max_delay = 30,
    httr::add_headers(.headers = get_understat_headers()),
    httr::timeout(timeout),
    handle = get_understat_session()
  )

  # Check for errors returned by fetch_with_retry
  # Errors are returned as list with class "fetch_error"
  if (inherits(response, "fetch_error") || is.null(response)) {
    if (isTRUE(attr(response, "rate_limited"))) {
      cli::cli_warn("Rate limited by Understat (429). Stopping.")
    } else if (isTRUE(attr(response, "connection_error"))) {
      cli::cli_warn("Connection error: {attr(response, 'error_message')}")
    } else if (isTRUE(attr(response, "not_found"))) {
      # 404 - silently return NULL (invalid match ID)
    } else {
      cli::cli_warn("Failed to fetch {url}")
    }
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
        cli::cli_warn("Failed to parse {var_name} JSON: {conditionMessage(e)}")
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
  .check_suggests("stringi", "Understat JSON parsing requires stringi.")

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
    cli::cli_warn("Could not extract match_info for match {understat_id}")
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
    away_ppda = as.numeric(match_info$a_ppda %||% NA_real_)
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

  response <- fetch_with_retry(
    url = url,
    max_retries = 3,
    base_delay = 1,
    max_delay = 30,
    httr::add_headers(.headers = c(
      get_understat_headers(),
      "Accept" = "application/json, text/javascript, */*; q=0.01",
      "X-Requested-With" = "XMLHttpRequest"
    )),
    httr::timeout(30),
    handle = get_understat_session()
  )

  if (inherits(response, "fetch_error") || is.null(response)) {
    return(NULL)
  }

  content <- httr::content(response, "text", encoding = "UTF-8")

  # Parse JSON
  tryCatch({
    jsonlite::fromJSON(content, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_warn("Failed to parse match API response: {conditionMessage(e)}")
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
        rbindlist(lapply(team_data, as.data.frame))
      }, error = function(e) {
        do.call(rbind, lapply(team_data, function(x) {
          as.data.frame(x)
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
  result <- rbindlist(list(home_roster, away_roster), use.names = TRUE, fill = TRUE)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Clean column names
  result <- clean_column_names(result)

  # Ensure required columns exist
  result$understat_id <- as.character(understat_id)

  as.data.frame(result)
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
        rbindlist(lapply(normalized, as.data.frame))
      }, error = function(e) {
        # Fallback: manually create data frame
        all_cols <- unique(unlist(lapply(normalized, names)))
        rows <- lapply(normalized, function(shot) {
          row <- setNames(rep(NA_character_, length(all_cols)), all_cols)
          for (col in names(shot)) {
            row[col] <- shot[[col]]
          }
          as.data.frame(as.list(row))
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
  result <- rbindlist(list(home_shots, away_shots), use.names = TRUE, fill = TRUE)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Clean column names
  result <- clean_column_names(result)

  # Add match ID
  result$understat_id <- as.character(understat_id)

  as.data.frame(result)
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
        score = score
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
        score = NA_character_
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
        score = NA_character_
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
          score = NA_character_
        )
      })

      return(rbindlist(sub_events))
    }

    # No recognized event
    NULL
  })

  # Combine all events
  result <- rbindlist(events)

  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  }

  # Add match ID
  result$understat_id <- as.character(understat_id)

  # Clean column names
  result <- clean_column_names(result)

  result
}
