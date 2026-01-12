# Direct FBref scraping functions
#
# These functions bypass worldfootballR (which gets blocked by Cloudflare)
# by using direct httr requests with browser headers.

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
#' @export
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
#' @export
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
#' @export
fetch_match_page <- function(match_url, timeout = 30) {
  # Validate URL
if (!grepl("fbref\\.com/en/matches/", match_url)) {
    stop("Invalid FBref match URL: ", match_url)
  }

  # Make request with session cookies
  response <- httr::GET(
    match_url,
    httr::add_headers(.headers = get_fbref_headers()),
    httr::timeout(timeout),
    handle = get_fbref_session()
  )

  status <- httr::status_code(response)

  # Check for rate limiting
  if (status == 429) {
    warning("Rate limited by FBref (429). Stopping.")
    result <- NULL
    attr(result, "rate_limited") <- TRUE
    return(result)
  }

  # Check for Cloudflare block
  if (status == 403) {
    warning("Blocked by Cloudflare (403). Stopping.")
    result <- NULL
    attr(result, "blocked") <- TRUE
    return(result)
  }

  # Check other errors
  if (status != 200) {
    warning("Failed to fetch ", match_url, " - Status: ", status)
    return(NULL)
  }

  # Parse HTML
  html_content <- httr::content(response, "text", encoding = "UTF-8")
  rvest::read_html(html_content)
}


# ============================================================================
# Parsing Layer
# ============================================================================

#' Extract match metadata from FBref page
#'
#' Parses scorebox to get teams, score, date, manager, captain, venue,
#' attendance, officials, and formations.
#'
#' @param page Parsed HTML document
#' @param match_url Original match URL for ID extraction
#'
#' @return Data frame with match metadata (single row)
#' @keywords internal
extract_match_metadata <- function(page, match_url) {
  scorebox <- rvest::html_node(page, ".scorebox")

  # Extract match ID from URL (8-character hex code)
  fbref_id <- regmatches(match_url, regexpr("[a-f0-9]{8}", match_url))

  # Team names from scorebox
  teams <- character(0)
  if (!is.na(scorebox)) {
    teams <- rvest::html_text(rvest::html_nodes(scorebox, "strong a"))
  }

  # Scores
  scores <- numeric(0)
  if (!is.na(scorebox)) {
    score_text <- rvest::html_text(rvest::html_nodes(scorebox, ".score"))
    scores <- suppressWarnings(as.numeric(score_text))
  }

  # Find all table IDs to identify team IDs
  table_nodes <- rvest::html_nodes(page, "table")
  table_ids <- rvest::html_attr(table_nodes, "id")
  table_ids <- table_ids[!is.na(table_ids)]

  # Extract team IDs from table names (e.g., "stats_18bb7c10_summary")
  team_id_matches <- regmatches(table_ids, regexpr("stats_([a-f0-9]{8})_", table_ids))

  team_ids <- unique(gsub("stats_|_", "", team_id_matches))

  # Extract date from URL (format: Month-Day-Year in URL)
  date_match <- regmatches(
    match_url,
    regexpr("[A-Z][a-z]+-\\d{1,2}-\\d{4}", match_url)
  )
  match_date <- NA_character_
  if (length(date_match) > 0) {
    match_date <- tryCatch(
      as.character(as.Date(date_match, format = "%B-%d-%Y")),
      error = function(e) NA_character_
    )
  }

  # ============================================================================
  # Extended metadata extraction
  # ============================================================================

  # Helper to extract text after "Label:" from datapoint divs
  extract_datapoint <- function(team_node, label) {
    if (is.na(team_node)) return(NA_character_)
    datapoints <- rvest::html_nodes(team_node, ".datapoint")
    for (dp in datapoints) {
      text <- rvest::html_text(dp)
      if (grepl(paste0("^", label, ":"), text, ignore.case = TRUE)) {
        # Extract text after the label
        value <- sub(paste0("^", label, ":\\s*"), "", text, ignore.case = TRUE)
        return(trimws(value))
      }
    }
    NA_character_
  }

  # Helper to extract player ID from captain link
  extract_captain_id <- function(team_node) {
    if (is.na(team_node)) return(NA_character_)
    datapoints <- rvest::html_nodes(team_node, ".datapoint")
    for (dp in datapoints) {
      text <- rvest::html_text(dp)
      if (grepl("^Captain:", text, ignore.case = TRUE)) {
        link <- rvest::html_node(dp, "a")
        if (!is.na(link)) {
          href <- rvest::html_attr(link, "href")
          # Extract player ID from href (e.g., "/en/players/d5f2f82b/Marquinhos")
          id_match <- regmatches(href, regexpr("[a-f0-9]{8}", href))
          if (length(id_match) > 0) return(id_match)
        }
      }
    }
    NA_character_
  }

  # Extract manager and captain for each team
  home_team_node <- rvest::html_node(page, "#sb_team_0")
  away_team_node <- rvest::html_node(page, "#sb_team_1")

  home_manager <- extract_datapoint(home_team_node, "Manager")
  away_manager <- extract_datapoint(away_team_node, "Manager")
  home_captain <- extract_datapoint(home_team_node, "Captain")
  away_captain <- extract_datapoint(away_team_node, "Captain")
  home_captain_id <- extract_captain_id(home_team_node)
  away_captain_id <- extract_captain_id(away_team_node)

  # Extract venue, attendance, officials from scorebox_meta
  scorebox_meta <- rvest::html_node(page, ".scorebox_meta")
  venue <- NA_character_
  attendance <- NA_integer_
  referee <- NA_character_
  ar1 <- NA_character_
  ar2 <- NA_character_
  fourth_official <- NA_character_
  var_official <- NA_character_
  kickoff_time <- NA_character_
  kickoff_epoch <- NA_integer_

  if (!is.na(scorebox_meta)) {
    meta_divs <- rvest::html_nodes(scorebox_meta, "div")
    for (div in meta_divs) {
      text <- rvest::html_text(div)

      # Venue
      if (grepl("Venue:", text, ignore.case = TRUE)) {
        venue <- sub(".*Venue:\\s*", "", text, ignore.case = TRUE)
        venue <- trimws(venue)
      }

      # Attendance
      if (grepl("Attendance:", text, ignore.case = TRUE)) {
        att_match <- regmatches(text, regexpr("[0-9,]+", text))
        if (length(att_match) > 0) {
          attendance <- as.integer(gsub(",", "", att_match))
        }
      }

      # Officials
      if (grepl("Officials:", text, ignore.case = TRUE)) {
        officials_text <- sub(".*Officials:\\s*", "", text, ignore.case = TRUE)
        # Split by middot (Unicode U+00B7) or comma
        officials_list <- strsplit(officials_text, "\\s*[\u00b7,]\\s*")[[1]]
        officials_list <- trimws(officials_list)

        for (official in officials_list) {
          if (grepl("\\(Referee\\)", official, ignore.case = TRUE)) {
            referee <- sub("\\s*\\(Referee\\).*", "", official, ignore.case = TRUE)
          } else if (grepl("\\(AR1\\)", official, ignore.case = TRUE)) {
            ar1 <- sub("\\s*\\(AR1\\).*", "", official, ignore.case = TRUE)
          } else if (grepl("\\(AR2\\)", official, ignore.case = TRUE)) {
            ar2 <- sub("\\s*\\(AR2\\).*", "", official, ignore.case = TRUE)
          } else if (grepl("\\(4th\\)", official, ignore.case = TRUE)) {
            fourth_official <- sub("\\s*\\(4th\\).*", "", official, ignore.case = TRUE)
          } else if (grepl("\\(VAR\\)", official, ignore.case = TRUE)) {
            var_official <- sub("\\s*\\(VAR\\).*", "", official, ignore.case = TRUE)
          }
        }
      }
    }

    # Extract kickoff time from venuetime span
    venuetime <- rvest::html_node(scorebox_meta, ".venuetime")
    if (!is.na(venuetime)) {
      kickoff_time <- rvest::html_attr(venuetime, "data-venue-time")
      epoch_str <- rvest::html_attr(venuetime, "data-venue-epoch")
      if (!is.na(epoch_str)) {
        kickoff_epoch <- as.integer(epoch_str)
      }
    }
  }

  # Extract formations from lineup divs
  home_formation <- NA_character_
  away_formation <- NA_character_

  home_lineup <- rvest::html_node(page, ".lineup#a")
  away_lineup <- rvest::html_node(page, ".lineup#b")

  extract_formation <- function(lineup_node) {
    if (is.na(lineup_node)) return(NA_character_)
    header <- rvest::html_node(lineup_node, "th")
    if (is.na(header)) return(NA_character_)
    text <- rvest::html_text(header)
    # Extract formation pattern like (4-3-3) or (4-1-4-1)
    formation_match <- regmatches(text, regexpr("\\([0-9]+-[0-9]+-[0-9]+(-[0-9]+)?\\)", text))
    if (length(formation_match) > 0) {
      # Remove parentheses
      return(gsub("[()]", "", formation_match))
    }
    NA_character_
  }

  home_formation <- extract_formation(home_lineup)
  away_formation <- extract_formation(away_lineup)

  data.frame(
    fbref_id = if (length(fbref_id) > 0) fbref_id else NA_character_,
    match_url = match_url,
    match_date = match_date,
    home_team = if (length(teams) >= 1) teams[1] else NA_character_,
    away_team = if (length(teams) >= 2) teams[2] else NA_character_,
    home_score = if (length(scores) >= 1) scores[1] else NA_integer_,
    away_score = if (length(scores) >= 2) scores[2] else NA_integer_,
    home_team_id = if (length(team_ids) >= 1) team_ids[1] else NA_character_,
    away_team_id = if (length(team_ids) >= 2) team_ids[2] else NA_character_,
    home_manager = home_manager,
    away_manager = away_manager,
    home_captain = home_captain,
    away_captain = away_captain,
    home_captain_id = home_captain_id,
    away_captain_id = away_captain_id,
    venue = venue,
    attendance = attendance,
    kickoff_time = kickoff_time,
    kickoff_epoch = kickoff_epoch,
    referee = referee,
    ar1 = ar1,
    ar2 = ar2,
    fourth_official = fourth_official,
    var_official = var_official,
    home_formation = home_formation,
    away_formation = away_formation,
    stringsAsFactors = FALSE
  )
}


#' Scrape match events timeline from FBref page
#'
#' Parses the events wrap section to extract goals, cards, and substitutions
#' with precise minute information.
#'
#' @param page Parsed HTML document
#' @param match_url Original match URL for ID extraction
#'
#' @return Data frame with event timeline, or NULL if no events found
#' @keywords internal
scrape_match_events <- function(page, match_url) {
  # Extract match ID from URL
  fbref_id <- regmatches(match_url, regexpr("[a-f0-9]{8}", match_url))
  if (length(fbref_id) == 0) fbref_id <- NA_character_

  # Find events container
  events_wrap <- rvest::html_node(page, "#events_wrap")
  if (is.na(events_wrap)) {
    return(NULL)
  }

  # Get all event divs (class "event a" for home, "event b" for away)
  event_nodes <- rvest::html_nodes(events_wrap, ".event")

  if (length(event_nodes) == 0) {
    return(NULL)
  }

  # Parse each event
  events_list <- lapply(event_nodes, function(event_node) {
    # Determine team from class (a = home, b = away)
    event_class <- rvest::html_attr(event_node, "class")
    is_home <- grepl("\\ba\\b", event_class)

    # Get event icon class to determine event type
    icon_node <- rvest::html_node(event_node, ".event_icon")
    event_type <- NA_character_
    if (!is.na(icon_node)) {
      icon_class <- rvest::html_attr(icon_node, "class")
      # Map icon classes to event types
      if (grepl("penalty_goal", icon_class)) {
        event_type <- "penalty_goal"
      } else if (grepl("own_goal", icon_class)) {
        event_type <- "own_goal"
      } else if (grepl("\\bgoal\\b", icon_class)) {
        event_type <- "goal"
      } else if (grepl("yellow_red_card", icon_class)) {
        event_type <- "yellow_red_card"
      } else if (grepl("red_card", icon_class)) {
        event_type <- "red_card"
      } else if (grepl("yellow_card", icon_class)) {
        event_type <- "yellow_card"
      } else if (grepl("substitute_in", icon_class)) {
        event_type <- "sub_on"
      }
    }

    # Skip if no recognizable event type
    if (is.na(event_type)) {
      return(NULL)
    }

    # Extract minute from first div text (e.g., "19'" or "45+1'")
    first_div <- rvest::html_node(event_node, "div")
    minute_text <- if (!is.na(first_div)) rvest::html_text(first_div) else ""

    # Parse minute and added time
    minute <- NA_integer_
    added_time <- NA_integer_

    # Match patterns like "19'" or "45+1'" or "90+7'"
    # FBref uses &rsquor; HTML entity (or actual quote chars) after the minute
    # Match: digits, optional +digits, then any quote-like char or &rsquor;
    minute_match <- regmatches(minute_text, regexpr("([0-9]+)\\+?([0-9]*)(&rsquor;|['\\u2019])", minute_text))
    if (length(minute_match) > 0) {
      # Extract base minute
      base_min <- regmatches(minute_match, regexpr("^[0-9]+", minute_match))
      if (length(base_min) > 0) {
        minute <- as.integer(base_min)
      }
      # Extract added time if present (after +)
      if (grepl("\\+", minute_match)) {
        added_match <- regmatches(minute_match, regexpr("\\+([0-9]+)", minute_match))
        if (length(added_match) > 0) {
          added_time <- as.integer(gsub("\\+", "", added_match))
        }
      }
    }

    # Extract score at time of event (from small span, e.g., "1:0")
    score_span <- rvest::html_node(first_div, "small span")
    score_home <- NA_integer_
    score_away <- NA_integer_
    if (!is.na(score_span)) {
      score_text <- rvest::html_text(score_span)
      score_parts <- strsplit(score_text, "[:-]")[[1]]
      if (length(score_parts) == 2) {
        score_home <- suppressWarnings(as.integer(trimws(score_parts[1])))
        score_away <- suppressWarnings(as.integer(trimws(score_parts[2])))
      }
    }

    # Extract primary player (first link in event)
    player_links <- rvest::html_nodes(event_node, "a")
    player <- NA_character_
    player_id <- NA_character_
    if (length(player_links) > 0) {
      player <- rvest::html_text(player_links[[1]])
      href <- rvest::html_attr(player_links[[1]], "href")
      if (!is.na(href)) {
        id_match <- regmatches(href, regexpr("[a-f0-9]{8}", href))
        if (length(id_match) > 0) {
          player_id <- id_match
        }
      }
    }

    # Extract secondary player (assist for goals, sub off for subs)
    secondary_player <- NA_character_
    secondary_player_id <- NA_character_

    # Look for assist or substitution info in small tags
    small_nodes <- rvest::html_nodes(event_node, "small")
    for (small_node in small_nodes) {
      small_text <- rvest::html_text(small_node)

      # Assist pattern: "Assist: PlayerName"
      if (grepl("Assist:", small_text, ignore.case = TRUE)) {
        assist_link <- rvest::html_node(small_node, "a")
        if (!is.na(assist_link)) {
          secondary_player <- rvest::html_text(assist_link)
          href <- rvest::html_attr(assist_link, "href")
          if (!is.na(href)) {
            id_match <- regmatches(href, regexpr("[a-f0-9]{8}", href))
            if (length(id_match) > 0) {
              secondary_player_id <- id_match
            }
          }
        }
        break
      }

      # Substitution pattern: "for PlayerName"
      if (grepl("^\\s*for\\s+", small_text, ignore.case = TRUE)) {
        sub_link <- rvest::html_node(small_node, "a")
        if (!is.na(sub_link)) {
          secondary_player <- rvest::html_text(sub_link)
          href <- rvest::html_attr(sub_link, "href")
          if (!is.na(href)) {
            id_match <- regmatches(href, regexpr("[a-f0-9]{8}", href))
            if (length(id_match) > 0) {
              secondary_player_id <- id_match
            }
          }
        }
        break
      }
    }

    data.frame(
      fbref_id = fbref_id,
      minute = minute,
      added_time = added_time,
      event_type = event_type,
      is_home = is_home,
      player = player,
      player_id = player_id,
      secondary_player = secondary_player,
      secondary_player_id = secondary_player_id,
      score_home = score_home,
      score_away = score_away,
      stringsAsFactors = FALSE
    )
  })

  # Remove NULL entries and combine
  events_list <- events_list[!sapply(events_list, is.null)]

  if (length(events_list) == 0) {
    return(NULL)
  }

  result <- do.call(rbind, events_list)

  # Sort by minute, then added_time (treat NA as 0 so 90' comes before 90+1')
  sort_added_time <- ifelse(is.na(result$added_time), 0L, result$added_time)
  result <- result[order(result$minute, sort_added_time, na.last = TRUE), ]
  rownames(result) <- NULL

  result
}


#' Parse FBref stats table with multi-row headers
#'
#' Handles the complex header structure of FBref stats tables.
#' Uses multiple parsing strategies to handle different table structures
#' (some lower-league cup matches have different HTML structure).
#'
#' @param page Parsed HTML document
#' @param table_id ID of the table to parse
#'
#' @return Data frame with parsed stats, or NULL if table not found
#' @keywords internal
parse_stats_table <- function(page, table_id) {
  table_node <- rvest::html_node(page, paste0("#", table_id))

  if (length(table_node) == 0 || is.na(table_node)) {
    return(NULL)
  }

  # Get header rows (FBref uses 2-row headers)
  header_rows <- rvest::html_nodes(table_node, "thead tr")

  if (length(header_rows) < 2) {
    # Single header row - use standard parsing
    df <- tryCatch(
      rvest::html_table(table_node, fill = TRUE),
      error = function(e) NULL
    )
    if (!is.null(df)) {
      return(clean_column_names(df))
    }
    return(NULL)
  }

  # Get column names from second header row (more specific names)
  col_names <- rvest::html_text(rvest::html_nodes(header_rows[[2]], "th"))

  # Get data rows from tbody
  data_rows <- rvest::html_nodes(table_node, "tbody tr")

  # Filter out spacer/header rows within tbody
  row_classes <- rvest::html_attr(data_rows, "class")
  row_classes[is.na(row_classes)] <- ""
  data_rows <- data_rows[!grepl("thead|spacer|partial_table", row_classes)]

  # If no tbody rows found, try fallback to html_table (some cup matches have different structure)
  if (length(data_rows) == 0) {
    df <- tryCatch({
      result <- rvest::html_table(table_node, fill = TRUE)
      # html_table may include header rows as data - filter them out
      if (nrow(result) > 0) {
        # Remove rows where first column matches a header value
        first_col <- result[[1]]
        result <- result[!first_col %in% c("", "Player", col_names[1]), ]
      }
      result
    }, error = function(e) NULL)

    if (!is.null(df) && nrow(df) > 0) {
      return(clean_column_names(df))
    }
    return(NULL)
  }

  # Parse each row
  parse_row <- function(row) {
    cells <- rvest::html_nodes(row, "th, td")
    rvest::html_text(cells)
  }

  row_data <- lapply(data_rows, parse_row)

  # Filter rows with correct number of columns
  expected_cols <- length(col_names)
  row_data <- row_data[sapply(row_data, length) == expected_cols]

  if (length(row_data) == 0) {
    return(NULL)
  }

  # Build data frame
  df <- as.data.frame(
    do.call(rbind, row_data),
    stringsAsFactors = FALSE
  )
  names(df) <- col_names

  # Clean column names to snake_case
  clean_column_names(df)
}


#' Combine home and away team tables
#'
#' Merges home and away data for a table type into single data frame.
#'
#' @param parsed_data List containing parsed tables
#' @param table_type Type of table (e.g., "summary", "passing")
#'
#' @return Combined data frame with both teams, or NULL if no data
#' @keywords internal
combine_team_tables <- function(parsed_data, table_type) {
  home_key <- paste0(table_type, "_home")
  away_key <- paste0(table_type, "_away")

  home_df <- parsed_data[[home_key]]
  away_df <- parsed_data[[away_key]]

  if (is.null(home_df) && is.null(away_df)) {
    return(NULL)
  }

  dplyr::bind_rows(home_df, away_df)
}


#' Parse all tables from FBref match page
#'
#' Extracts all available stat tables from a match page.
#'
#' @param page Parsed HTML document
#' @param match_url Original match URL
#'
#' @return List containing metadata and all parsed tables
#' @export
parse_match_page <- function(page, match_url) {
  # Extract metadata first
  metadata <- extract_match_metadata(page, match_url)

  result <- list(metadata = metadata)

  # Define table types and their ID patterns
  table_types <- c("summary", "passing", "passing_types",
                   "defense", "possession", "misc")

  home_team_id <- metadata$home_team_id
  away_team_id <- metadata$away_team_id

  # Parse tables for each team
  for (team_id in c(home_team_id, away_team_id)) {
    if (is.na(team_id)) next

    is_home <- team_id == home_team_id
    team_label <- if (is_home) "home" else "away"
    team_name <- if (is_home) metadata$home_team else metadata$away_team

    for (table_type in table_types) {
      table_id <- paste0("stats_", team_id, "_", table_type)
      df <- parse_stats_table(page, table_id)

      if (!is.null(df) && nrow(df) > 0) {
        # Add team and home/away context
        df$team <- team_name
        df$is_home <- is_home
        df$match_url <- match_url

        # Store in results
        key <- paste0(table_type, "_", team_label)
        result[[key]] <- df
      }
    }

    # Keeper stats (different ID pattern)
    keeper_id <- paste0("keeper_stats_", team_id)
    keeper_df <- parse_stats_table(page, keeper_id)
    if (!is.null(keeper_df) && nrow(keeper_df) > 0) {
      keeper_df$team <- team_name
      keeper_df$is_home <- is_home
      keeper_df$match_url <- match_url
      result[[paste0("keeper_", team_label)]] <- keeper_df
    }
  }

  # Shots table (combined for all shots)
  shots_df <- parse_stats_table(page, "shots_all")
  if (!is.null(shots_df) && nrow(shots_df) > 0) {
    shots_df$match_url <- match_url
    result$shots <- shots_df
  }

  # Events timeline (goals, cards, substitutions)
  events_df <- scrape_match_events(page, match_url)
  if (!is.null(events_df) && nrow(events_df) > 0) {
    events_df$match_url <- match_url
    result$events <- events_df
  }

  result
}


# ============================================================================
# Cache Management
# ============================================================================

# Environment for storing pannadata path
.panna_env <- new.env(parent = emptyenv())

#' Get or set pannadata directory
#'
#' Gets or sets the base directory for data storage. By default uses
#' the pannadata directory in the pannaverse folder structure.
#'
#' @param path Optional new path to set. If NULL, returns current path.
#'
#' @return Current pannadata directory path
#' @export
#'
#' @examples
#' # Get current path
#' pannadata_dir()
#'
#' # Set custom path
#' pannadata_dir("C:/my/custom/path")
pannadata_dir <- function(path = NULL) {
  if (!is.null(path)) {
    .panna_env$pannadata_dir <- path
    return(invisible(path))
  }

  # Return cached value if set
 if (exists("pannadata_dir", envir = .panna_env)) {
    return(.panna_env$pannadata_dir)
  }

  # Check environment variable
  env_path <- Sys.getenv("PANNADATA_DIR", "")
  if (env_path != "") {
    return(env_path)
  }

  # Default: pannadata sibling directory
  # Try to find pannaverse/pannadata relative to current panna location
  default_path <- file.path(dirname(getwd()), "pannadata", "data")

  # If that doesn't exist, fall back to local data folder
  if (!dir.exists(dirname(default_path))) {
    default_path <- file.path("data", "fbref_matches")
  }

  default_path
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
#' @export
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
#' @export
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
scrape_fixtures <- function(league, season, completed_only = TRUE) {
  url <- get_fbref_schedule_url(league, season)

  progress_msg(sprintf("Fetching fixtures: %s %s", league, season))

  # Fetch page with session cookies
  response <- httr::GET(
    url,
    httr::add_headers(.headers = get_fbref_headers()),
    httr::timeout(30),
    handle = get_fbref_session()
  )

  status <- httr::status_code(response)
  if (status == 429 || status == 403) {
    stop(sprintf("Rate limited or blocked (HTTP %d). Stopping scraper.", status))
  }
  if (status != 200) {
    warning("Failed to fetch fixtures for ", league, " ", season,
            " - Status: ", status)
    return(NULL)
  }

  html_content <- httr::content(response, "text", encoding = "UTF-8")
  page <- rvest::read_html(html_content)

  # Find the fixtures table - it has class "stats_table" and id containing "sched"
  fixtures_table <- rvest::html_node(page, "table.stats_table")

  if (is.na(fixtures_table)) {
    warning("Could not find fixtures table for ", league, " ", season)
    return(NULL)
  }

  # Parse table
  df <- tryCatch({
    rvest::html_table(fixtures_table, fill = TRUE)
  }, error = function(e) {
    warning("Error parsing fixtures table: ", e$message)
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
  # Validate inputs
  if (length(match_urls) == 0) {
    stop("No match URLs provided")
  }

  if (missing(league) || missing(season)) {
    stop("league and season are required for file naming")
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
      warning("Could not extract match ID from URL: ", url)
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
      warning("Error fetching ", url, ": ", e$message)
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
        dplyr::bind_rows(x)
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
      final[[tt]] <- dplyr::bind_rows(results[[tt]])
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


#' Aggregate cached match data
#'
#' Combines all cached data for a table type into a single data frame.
#' Uses parquet files if available (fast), falls back to RDS files (slower).
#'
#' @param table_type Type of table to aggregate (e.g., "summary", "shots")
#' @param league Optional league filter (e.g., "ENG")
#' @param season Optional season filter (e.g., "2024-2025")
#' @param prefer_parquet If TRUE (default), use parquet when available
#'
#' @return Combined data frame, or NULL if no cached data
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all cached summary stats (uses parquet if available)
#' all_summary <- aggregate_cached_matches("summary")
#'
#' # Load only Premier League 2024-2025
#' pl_summary <- aggregate_cached_matches("summary", league = "ENG",
#'                                         season = "2024-2025")
#'
#' # Force RDS loading (skip parquet)
#' pl_summary <- aggregate_cached_matches("summary", league = "ENG",
#'                                         season = "2024-2025",
#'                                         prefer_parquet = FALSE)
#' }
aggregate_cached_matches <- function(table_type, league = NULL, season = NULL,
                                     prefer_parquet = TRUE) {

  # Fast path: if league AND season specified, try parquet first
  if (prefer_parquet && !is.null(league) && !is.null(season)) {
    parquet_path <- get_parquet_path(table_type, league, season)

    if (file.exists(parquet_path)) {
      return(arrow::read_parquet(parquet_path))
    }
  }

  # Fast path: if only league specified, combine all parquet files for that league
  if (prefer_parquet && !is.null(league) && is.null(season)) {
    parquet_dir <- file.path(pannadata_dir(), table_type, league)
    if (dir.exists(parquet_dir)) {
      parquet_files <- list.files(parquet_dir, pattern = "\\.parquet$",
                                  full.names = TRUE)
      if (length(parquet_files) > 0) {
        all_data <- lapply(parquet_files, arrow::read_parquet)
        return(dplyr::bind_rows(all_data))
      }
    }
  }

  # Fallback: read from RDS files (original behavior)
  cache_dir <- get_fbref_match_cache_dir(table_type, create = FALSE)

  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  # List matching files
  cached <- list_cached_matches(table_type, league, season)

  if (nrow(cached) == 0) {
    return(NULL)
  }

  # Load and combine
  all_data <- lapply(seq_len(nrow(cached)), function(i) {
    load_match_table(
      cached$league[i],
      cached$season[i],
      cached$fbref_id[i],
      table_type
    )
  })

  all_data <- all_data[!sapply(all_data, is.null)]

  if (length(all_data) == 0) {
    return(NULL)
  }

  dplyr::bind_rows(all_data)
}


# Parquet functions ----

#' Get parquet file path for a league-season
#'
#' Returns the path where parquet files are stored:
#' \code{{pannadata_dir}/{table_type}/{league}/{season}.parquet}
#'
#' @param table_type Table type (e.g., "summary", "events")
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2024-2025")
#' @param create If TRUE, create parent directory if missing
#'
#' @return Path to parquet file
#' @keywords internal
get_parquet_path <- function(table_type, league, season, create = FALSE) {
  base_dir <- pannadata_dir()
  parquet_dir <- file.path(base_dir, table_type, league)

  if (create && !dir.exists(parquet_dir)) {
    dir.create(parquet_dir, recursive = TRUE)
  }

  file.path(parquet_dir, paste0(season, ".parquet"))
}


#' Build parquet file from RDS files for a league-season
#'
#' Reads all RDS files for a table_type/league/season and combines them
#' into a single parquet file. If a parquet file already exists, new RDS
#' data is merged with existing parquet data (for incremental updates).
#'
#' @param table_type Table type (e.g., "summary", "events")
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2024-2025")
#' @param verbose Print progress messages
#'
#' @return Path to created parquet file, or NULL if no data
#' @export
build_parquet <- function(table_type, league, season, verbose = TRUE) {
  if (verbose) message(sprintf("  %s/%s:", league, season))

  parquet_path <- get_parquet_path(table_type, league, season, create = TRUE)

  # Load existing parquet data if it exists
  existing_data <- NULL
  existing_ids <- character(0)
  if (file.exists(parquet_path)) {
    existing_data <- tryCatch({
      arrow::read_parquet(parquet_path)
    }, error = function(e) NULL)

    if (!is.null(existing_data) && "fbref_id" %in% names(existing_data)) {
      existing_ids <- unique(existing_data$fbref_id)
      if (verbose) message(sprintf("    Existing parquet: %d matches", length(existing_ids)))
    }
  }

  # Get all RDS files for this combination
  cached <- list_cached_matches(table_type, league, season)

  # Load new RDS data (only files not already in parquet)
  new_data <- NULL
  if (nrow(cached) > 0) {
    new_ids <- setdiff(cached$fbref_id, existing_ids)

    if (length(new_ids) > 0) {
      if (verbose) message(sprintf("    Loading %d new from RDS", length(new_ids)))

      new_cached <- cached[cached$fbref_id %in% new_ids, ]
      new_data_list <- lapply(seq_len(nrow(new_cached)), function(i) {
        load_match_table(new_cached$league[i], new_cached$season[i],
                         new_cached$fbref_id[i], table_type)
      })
      new_data_list <- new_data_list[!sapply(new_data_list, is.null)]

      if (length(new_data_list) > 0) {
        new_data <- dplyr::bind_rows(new_data_list)
      }
    }
  }

  # Combine existing and new data
  if (is.null(existing_data) && is.null(new_data)) {
    if (verbose) message(sprintf("No data for %s/%s/%s", table_type, league, season))
    return(NULL)
  }

  combined <- dplyr::bind_rows(existing_data, new_data)

  if (nrow(combined) == 0) {
    if (verbose) message("No valid data found")
    return(NULL)
  }

  # Write parquet
  arrow::write_parquet(combined, parquet_path)

  if (verbose) {
    size_mb <- file.size(parquet_path) / (1024 * 1024)
    n_matches <- length(unique(combined$fbref_id))
    message(sprintf("    -> %d matches, %.2f MB", n_matches, size_mb))
  }

  invisible(parquet_path)
}


#' Build all parquet files
#'
#' Iterates through all table_type/league/season combinations and creates
#' parquet files. Discovers combinations from both existing RDS files and
#' existing parquet files (for incremental updates on CI).
#'
#' @param table_types Character vector of table types to process.
#'   Default: all standard table types.
#' @param leagues Optional character vector of leagues to process.
#'   If NULL, processes all leagues found.
#' @param seasons Optional character vector of seasons to process.
#'   If NULL, processes all seasons found.
#' @param verbose Print progress messages
#'
#' @return Data frame with table_type, league, season, n_matches, size_mb columns
#' @export
build_all_parquet <- function(table_types = NULL, leagues = NULL,
                              seasons = NULL, verbose = TRUE) {
  # Default table types
  if (is.null(table_types)) {
    table_types <- c("summary", "passing", "passing_types", "defense",
                     "possession", "misc", "keeper", "shots", "events", "metadata")
  }

  results <- list()
  base_dir <- pannadata_dir()

  for (tt in table_types) {
    if (verbose) message(sprintf("\nProcessing table type: %s", tt))

    tt_dir <- file.path(base_dir, tt)
    if (!dir.exists(tt_dir)) next

    # Get leagues from both RDS subdirs and parquet files
    available_leagues <- if (!is.null(leagues)) {
      leagues
    } else {
      unique(list.dirs(tt_dir, recursive = FALSE, full.names = FALSE))
    }

    for (lg in available_leagues) {
      league_dir <- file.path(tt_dir, lg)
      if (!dir.exists(league_dir)) next

      # Get seasons from both RDS subdirs and parquet files
      rds_seasons <- list.dirs(league_dir, recursive = FALSE, full.names = FALSE)
      parquet_files <- list.files(league_dir, pattern = "\\.parquet$")
      parquet_seasons <- gsub("\\.parquet$", "", parquet_files)

      available_seasons <- if (!is.null(seasons)) {
        seasons
      } else {
        unique(c(rds_seasons, parquet_seasons))
      }

      for (sn in available_seasons) {
        # Check if this season has RDS files OR existing parquet
        season_dir <- file.path(league_dir, sn)
        has_rds <- dir.exists(season_dir) &&
                   length(list.files(season_dir, pattern = "\\.rds$")) > 0
        has_parquet <- file.exists(get_parquet_path(tt, lg, sn))

        if (!has_rds && !has_parquet) next

        parquet_path <- tryCatch({
          build_parquet(tt, lg, sn, verbose = verbose)
        }, error = function(e) {
          if (verbose) warning(sprintf("Error building %s/%s/%s: %s",
                                       tt, lg, sn, e$message))
          NULL
        })

        if (!is.null(parquet_path) && file.exists(parquet_path)) {
          # Count matches from parquet
          n_matches <- tryCatch({
            df <- arrow::read_parquet(parquet_path, col_select = "fbref_id")
            length(unique(df$fbref_id))
          }, error = function(e) 0)

          results[[length(results) + 1]] <- data.frame(
            table_type = tt,
            league = lg,
            season = sn,
            n_matches = n_matches,
            size_mb = round(file.size(parquet_path) / (1024 * 1024), 2),
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      table_type = character(0),
      league = character(0),
      season = character(0),
      n_matches = integer(0),
      size_mb = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  result_df <- do.call(rbind, results)

  if (verbose) {
    total_mb <- sum(result_df$size_mb)
    message(sprintf("\nBuilt %d parquet files (%.1f MB total)",
                    nrow(result_df), total_mb))
  }

  result_df
}


# Convenience loaders ----

#' Load summary data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of player summary stats or NULL
#' @export
load_summary <- function(league, season) {
  aggregate_cached_matches("summary", league = league, season = season)
}

#' Load events data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of match events or NULL
#' @export
load_events <- function(league, season) {
  aggregate_cached_matches("events", league = league, season = season)
}

#' Load shooting data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of shots or NULL
#' @export
load_shots <- function(league, season) {
  aggregate_cached_matches("shots", league = league, season = season)
}

#' Load metadata from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of match metadata or NULL
#' @export
load_metadata <- function(league, season) {
  aggregate_cached_matches("metadata", league = league, season = season)
}

#' Load passing data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of passing stats or NULL
#' @export
load_passing <- function(league, season) {
  aggregate_cached_matches("passing", league = league, season = season)
}

#' Load defense data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of defensive stats or NULL
#' @export
load_defense <- function(league, season) {
  aggregate_cached_matches("defense", league = league, season = season)
}

#' Load possession data from pannadata
#'
#' @param league League code (e.g., "ENG")
#' @param season Season string (e.g., "2023-2024")
#' @return Data frame of possession stats or NULL
#' @export
load_possession <- function(league, season) {
  aggregate_cached_matches("possession", league = league, season = season)
}


#' Migrate old metadata files to include tables_available field
#'
#' Updates metadata files that were created before the tables_available
#' field was added. Checks which table types actually exist in the cache
#' and updates the metadata accordingly.
#'
#' Uses hierarchical path: \code{\{table_type\}/\{league\}/\{season\}/\{id\}.rds}
#'
#' @param league Optional league filter
#' @param season Optional season filter
#' @param verbose Print progress (default TRUE)
#'
#' @return Number of metadata files updated
#' @export
#'
#' @examples
#' \dontrun{
#' # Migrate all old metadata
#' migrate_metadata_tables_available()
#'
#' # Migrate only ENG 2017-2018
#' migrate_metadata_tables_available(league = "ENG", season = "2017-2018")
#' }
migrate_metadata_tables_available <- function(league = NULL, season = NULL,
                                               verbose = TRUE) {
  # All possible table types (excluding metadata itself)
  all_table_types <- c("summary", "passing", "passing_types", "defense",
                       "possession", "misc", "keeper", "shots", "events")

  # List all metadata files
  meta_cached <- list_cached_matches("metadata", league, season)

  if (nrow(meta_cached) == 0) {
    if (verbose) message("No metadata files found to migrate")
    return(0)
  }

  n_updated <- 0
  n_skipped <- 0

  for (i in seq_len(nrow(meta_cached))) {
    lg <- meta_cached$league[i]
    sn <- meta_cached$season[i]
    fbref_id <- meta_cached$fbref_id[i]

    # Load metadata
    meta <- load_match_table(lg, sn, fbref_id, "metadata")

    if (is.null(meta)) next

    # Check if already has tables_available
    if ("tables_available" %in% names(meta) && !is.na(meta$tables_available[1])) {
      n_skipped <- n_skipped + 1
      next
    }

    # Check which tables exist for this match (hierarchical path)
    available <- character(0)
    filename <- make_match_filename(fbref_id)

    for (tt in all_table_types) {
      tt_dir <- get_fbref_match_cache_dir(tt, lg, sn, create = FALSE)
      if (dir.exists(tt_dir) && file.exists(file.path(tt_dir, filename))) {
        available <- c(available, tt)
      }
    }

    # Add metadata itself to the list
    available <- c("metadata", available)

    # Update and save
    meta$tables_available <- paste(available, collapse = ",")
    save_match_table(meta, lg, sn, fbref_id, "metadata")
    n_updated <- n_updated + 1

    if (verbose && (n_updated %% 100 == 0)) {
      progress_msg(sprintf("Migrated %d metadata files...", n_updated))
    }
  }

  if (verbose) {
    progress_msg(sprintf("Migration complete: %d updated, %d already current",
                         n_updated, n_skipped))
  }

  n_updated
}


# Batch Scraping Helpers ----

#' Get cached match URLs from metadata
#'
#' Reads all cached metadata files for a league-season and extracts
#' the match URLs. Useful for re-scraping or updating cached matches.
#'
#' @param league League code (e.g., "ENG", "ESP")
#' @param season Season string (e.g., "2023-2024")
#'
#' @return Character vector of match URLs
#' @export
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
