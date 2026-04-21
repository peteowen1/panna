# FBref HTML parsing functions
#
# Pure functions for extracting data from FBref match pages.
# These functions take parsed HTML documents and return data frames.
# No state or HTTP dependencies.


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
    away_formation = away_formation
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
      score_away = score_away
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
    do.call(rbind, row_data)
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

  rbindlist(list(home_df, away_df), use.names = TRUE, fill = TRUE)
}


#' Parse all tables from FBref match page
#'
#' Extracts all available stat tables from a match page.
#'
#' @param page Parsed HTML document
#' @param match_url Original match URL
#'
#' @return List containing metadata and all parsed tables
#' @keywords internal
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
