# Splint creation functions for panna package
#
# A "splint" is a time segment between substitutions or goals in a match.
# This is the unit of analysis for RAPM models, as the lineup remains
# constant within each splint.


#' Parse shot minute strings
#'
#' Converts shot minute strings like "90+4" into minute and added_time components.
#'
#' @param minute_str Character vector of minute strings (e.g., "45", "90+4", "45+1")
#'
#' @return Data frame with columns: minute (numeric), added_time (numeric)
#' @keywords internal
parse_shot_minute <- function(minute_str) {
  if (is.null(minute_str) || length(minute_str) == 0) {
    return(data.frame(minute = numeric(0), added_time = numeric(0)))
  }

  # Handle already numeric input
  if (is.numeric(minute_str)) {
    return(data.frame(minute = minute_str, added_time = rep(0, length(minute_str))))
  }

  # Vectorized parsing of strings like "90+4" into minute=90, added_time=4
  has_plus <- grepl("\\+", minute_str) & !is.na(minute_str)

  # Extract base minute (everything before +)
  base_minute <- sub("\\+.*", "", minute_str)
  minute <- as.numeric(base_minute)

 # Extract added time (everything after +), default to 0
  added_str <- ifelse(has_plus, sub(".*\\+", "", minute_str), "0")
  added_time <- as.numeric(added_str)
  added_time[is.na(added_time)] <- 0

  data.frame(minute = minute, added_time = added_time)
}


#' Get first-half stoppage from shots data
#'
#' Parses shot minute strings to find max added_time at minute 45.
#'
#' @param shots Data frame with minute column (character, e.g., "45+3")
#'
#' @return Integer, first-half stoppage in minutes (0 if none)
#' @keywords internal
get_first_half_stoppage_from_shots <- function(shots) {
  if (is.null(shots) || nrow(shots) == 0 || !"minute" %in% names(shots)) {
    return(0L)
  }

  parsed <- parse_shot_minute(shots$minute)

  # Find first-half stoppage (minute = 45 with added_time > 0)
  is_first_half_stoppage <- !is.na(parsed$minute) & parsed$minute == 45 &
    !is.na(parsed$added_time) & parsed$added_time > 0

  if (any(is_first_half_stoppage)) {
    return(as.integer(max(parsed$added_time[is_first_half_stoppage], na.rm = TRUE)))
  }

  0L
}


#' Get match end from shots data
#'
#' Uses the last shot to determine match end time.
#'
#' @param shots Data frame with minute column (character, e.g., "90+4")
#' @param first_half_stoppage First half stoppage to offset second half
#'
#' @return Numeric match end minute (effective time)
#' @keywords internal
get_match_end_from_shots <- function(shots, first_half_stoppage = 0L) {
  if (is.null(shots) || nrow(shots) == 0 || !"minute" %in% names(shots)) {
    return(NA_real_)
  }

  parsed <- parse_shot_minute(shots$minute)

  # Calculate effective minutes
  effective <- calculate_effective_minute(parsed$minute, parsed$added_time, first_half_stoppage)

  if (all(is.na(effective))) {
    return(NA_real_)
  }

  max(effective, na.rm = TRUE) + 0.5
}


#' Calculate effective minute including stoppage time
#'
#' Converts minute + added_time into a single continuous time value.
#' Second-half events are offset by first-half stoppage to create continuous time.
#'
#' Examples with 3 mins first-half stoppage:
#' - First half minute 30 -> 30
#' - First half 45+3 -> 48
#' - Second half minute 46 -> 49 (offset by 3)
#' - Second half 90+11 -> 104 (90 + 11 + 3)
#'
#' @param minute Integer vector of base minutes
#' @param added_time Integer vector of added time (can be NA)
#' @param first_half_stoppage Integer, minutes of first-half stoppage (default 0)
#'
#' @return Numeric vector of effective minutes
#' @keywords internal
calculate_effective_minute <- function(minute, added_time, first_half_stoppage = 0L) {
  # Handle NULL/empty inputs
  if (is.null(minute) || length(minute) == 0) {
    return(numeric(0))
  }

  # Coerce added_time NA to 0
  added <- ifelse(is.na(added_time), 0L, as.integer(added_time))

  # Calculate base effective minute
  effective <- minute + added

  # Offset second-half events (minute >= 46) by first-half stoppage
  is_second_half <- minute >= 46
  effective[is_second_half] <- effective[is_second_half] + first_half_stoppage

 effective
}


#' Get first-half stoppage time from events
#'
#' Finds the maximum added_time for events at minute 45.
#'
#' @param events Data frame with minute and optionally added_time columns
#'
#' @return Integer, first-half stoppage in minutes (0 if none)
#' @keywords internal
get_first_half_stoppage <- function(events) {
  if (is.null(events) || nrow(events) == 0) {
    return(0L)
  }

  added_time <- if ("added_time" %in% names(events)) events$added_time else rep(0L, nrow(events))

  # Find first-half stoppage events (minute = 45 with added_time > 0)
  first_half_stoppage <- events$minute == 45 & !is.na(added_time) & added_time > 0

  if (any(first_half_stoppage)) {
    return(as.integer(max(added_time[first_half_stoppage], na.rm = TRUE)))
  }

  0L
}


#' Calculate match end minute from events and/or shots
#'
#' Determines the actual match end time based on the latest event or shot observed.
#' Uses continuous time (second half offset by first-half stoppage).
#' When both events and shots are provided, uses the maximum of both.
#'
#' @param events Data frame with minute and optionally added_time columns
#' @param shots Data frame with minute column (character, e.g., "90+4")
#' @param default_end Default match end if no data (default 91)
#'
#' @return Numeric match end minute
#' @keywords internal
calculate_match_end <- function(events, shots = NULL, default_end = 91) {
  # Get first-half stoppage from both sources, use max
  events_stoppage <- get_first_half_stoppage(events)
  shots_stoppage <- get_first_half_stoppage_from_shots(shots)
  first_half_stoppage <- max(events_stoppage, shots_stoppage)

  # Calculate match end from events
  events_end <- NA_real_
  if (!is.null(events) && nrow(events) > 0) {
    added_time <- if ("added_time" %in% names(events)) events$added_time else rep(0L, nrow(events))
    effective_mins <- calculate_effective_minute(events$minute, added_time, first_half_stoppage)
    if (!all(is.na(effective_mins))) {
      events_end <- max(effective_mins, na.rm = TRUE) + 0.5
    }
  }

  # Calculate match end from shots
  shots_end <- get_match_end_from_shots(shots, first_half_stoppage)

  # Use max of events and shots, or default
  min_end <- 91 + first_half_stoppage
  candidates <- c(events_end, shots_end)
  candidates <- candidates[!is.na(candidates)]

  if (length(candidates) == 0) {
    return(default_end)
  }

  max(min_end, max(candidates))
}


#' Calculate first-half end minute from events and/or shots
#'
#' Determines when first half actually ended based on stoppage time events or shots.
#' Uses full minutes: 45+3 -> first half ends at 48.5
#' Falls back to events only if shots is NULL.
#'
#' @param events Data frame with minute and optionally added_time columns
#' @param shots Data frame with minute column (character, e.g., "45+3")
#' @param default_end Default first-half end if no stoppage data (default 46)
#'
#' @return Numeric first-half end minute
#' @keywords internal
calculate_first_half_end <- function(events, shots = NULL, default_end = 46) {
  # Get first-half stoppage from both sources
  events_stoppage <- get_first_half_stoppage(events)
  shots_stoppage <- get_first_half_stoppage_from_shots(shots)

  # Use max of both (handles NULL/0 gracefully)
  max_stoppage <- max(events_stoppage, shots_stoppage, na.rm = TRUE)

  if (max_stoppage > 0) {
    # Return 45 + stoppage + small buffer (so 45+3 ends at 48.5)
    return(45 + max_stoppage + 0.5)
  }

  default_end
}


#' Extract match events with timing
#'
#' Parses goals, substitutions, and red cards from event data with their minutes.
#'
#' @param events Processed events data frame for a single match
#' @param match_id Match identifier to filter for
#'
#' @return Data frame of events sorted by minute
#' @export
extract_match_events <- function(events, match_id) {
  # Handle NULL or empty events
 if (is.null(events) || nrow(events) == 0) {
    return(data.frame(
      match_id = character(0),
      minute = numeric(0),
      event_type = character(0),
      is_goal = logical(0),
      is_sub = logical(0),
      is_red_card = logical(0),
      is_home = logical(0)
    ))
  }

  match_events <- events %>%
    dplyr::filter(.data$match_id == !!match_id) %>%
    dplyr::arrange(.data$minute) %>%
    dplyr::mutate(
      # Handle multiple event type formats (FBref scraper uses lowercase with underscores)
      is_goal = .data$event_type %in% c("goal", "Goal", "penalty_goal", "own_goal"),
      is_sub = .data$event_type %in% c("sub_on", "substitution", "Substitution"),
      is_red_card = if ("is_red_card" %in% names(.)) .data$is_red_card else
        .data$event_type %in% c("red_card", "Red Card", "yellow_red_card")
    )

  match_events
}


#' Extract substitution times from lineups
#'
#' Uses pre-calculated on_minute from leading space detection in player names.
#' FBref encodes substitutes with leading spaces - subs come on for the player
#' directly above them in the same team.
#'
#' @param lineups Lineups data with on_minute column
#'
#' @return Numeric vector of substitution minutes
#' @keywords internal
extract_sub_events <- function(lineups) {
  if (is.null(lineups) || nrow(lineups) == 0) return(NULL)

  # Check if we have the pre-calculated on_minute column
  if ("on_minute" %in% names(lineups)) {
    # Use pre-calculated times from leading space detection
    # Subs are players with is_starter == FALSE (if available) or on_minute > 0
    if ("is_starter" %in% names(lineups)) {
      subs <- lineups[lineups$is_starter == FALSE, , drop = FALSE]
    } else {
      subs <- lineups[lineups$on_minute > 0, , drop = FALSE]
    }

    if (nrow(subs) == 0) return(NULL)

    sub_times <- unique(subs$on_minute[!is.na(subs$on_minute) & subs$on_minute > 0])
    return(sub_times)
  }

  # Fallback: old method (90 - minutes)
  if ("is_starter" %in% names(lineups) && "minutes" %in% names(lineups)) {
    subs <- lineups[lineups$is_starter == FALSE & lineups$minutes > 0, , drop = FALSE]
    if (nrow(subs) > 0) {
      sub_times <- pmax(0, 90 - as.numeric(subs$minutes))
      return(unique(sub_times[!is.na(sub_times)]))
    }
  }

  NULL
}


#' Create splint boundaries
#'
#' Defines start and end times for each splint based on events and optionally shots.
#' Splints are created at:
#' - Start of match (minute 0)
#' - Each goal
#' - Each substitution
#' - Each red card
#' - Half time (minute 45)
#' - End of match (dynamically calculated from stoppage time in events/shots)
#'
#' Uses continuous time where second-half events are offset by first-half stoppage.
#' Example with 3 mins first-half stoppage and 11 mins second-half stoppage:
#' - First half: 0 to 48 (45 + 3)
#' - Second half: 48 to 104 (offset by 3, plus 90+11)
#'
#' Also tracks game state (cumulative goals, red cards, player counts) at each splint boundary.
#'
#' @param events Data frame of match events (with minute and optionally added_time columns)
#' @param shots Data frame of shots (optional, with minute column for stoppage time)
#' @param include_goals Logical, whether to create new splints at goals (default TRUE)
#' @param include_halftime Logical, whether to create splint at halftime (default TRUE)
#'
#' @return Data frame of splint boundaries with game state
#' @export
create_splint_boundaries <- function(events, shots = NULL, include_goals = TRUE, include_halftime = TRUE) {
  # Get first-half stoppage from both events and shots
  events_stoppage <- get_first_half_stoppage(events)
  shots_stoppage <- get_first_half_stoppage_from_shots(shots)
  first_half_stoppage <- max(events_stoppage, shots_stoppage, na.rm = TRUE)

  # Calculate dynamic match boundaries using both events and shots
  match_end <- calculate_match_end(events, shots = shots, default_end = 91)
  first_half_end <- calculate_first_half_end(events, shots = shots, default_end = 46)

  # Start with match boundaries
  boundaries <- c(0, match_end)

  # Add halftime boundary (end of first half including stoppage)
  if (include_halftime) {
    boundaries <- c(boundaries, first_half_end)
  }

  # Track goals and red cards for game state
  goal_events <- NULL
  red_card_events <- NULL

  # Helper to get effective minute from events (with first-half offset for second half)
  get_effective_minutes <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(numeric(0))
    added <- if ("added_time" %in% names(df)) df$added_time else rep(0L, nrow(df))
    calculate_effective_minute(df$minute, added, first_half_stoppage)
  }

  # Only process events if we have data
  if (!is.null(events) && nrow(events) > 0) {
    # Add substitution times (using effective minutes)
    if ("is_sub" %in% names(events)) {
      sub_events <- events[events$is_sub, , drop = FALSE]
      sub_times <- get_effective_minutes(sub_events)
      boundaries <- c(boundaries, sub_times)
    }

    # Add goal times and track who scored
    if (include_goals && "is_goal" %in% names(events)) {
      goal_events <- events[events$is_goal, , drop = FALSE]
      if (nrow(goal_events) > 0) {
        goal_events$effective_minute <- get_effective_minutes(goal_events)
        goal_events <- goal_events[order(goal_events$effective_minute), , drop = FALSE]
        boundaries <- c(boundaries, goal_events$effective_minute)
      }
    }

    # Add red card times and track which team
    if ("is_red_card" %in% names(events)) {
      red_card_events <- events[events$is_red_card, , drop = FALSE]
      if (nrow(red_card_events) > 0) {
        red_card_events$effective_minute <- get_effective_minutes(red_card_events)
        red_card_events <- red_card_events[order(red_card_events$effective_minute), , drop = FALSE]
        boundaries <- c(boundaries, red_card_events$effective_minute)
      }
    }
  }

  # Clean up
  boundaries <- sort(unique(boundaries))
  boundaries <- boundaries[!is.na(boundaries)]

  # Create boundary data frame
  n_splints <- length(boundaries) - 1

  if (n_splints < 1) {
    # No events, single splint for whole match
    return(data.frame(
      splint_num = 1,
      start_minute = 0,
      end_minute = match_end,
      duration = match_end,
      avg_min = match_end / 2,
      gf_home = 0,
      ga_home = 0,
      goals_home = 0,
      goals_away = 0,
      red_home = 0,
      red_away = 0,
      n_players_home = 11,
      n_players_away = 11
    ))
  }

  # Calculate game state at each boundary (cumulative at start of each splint)
  goal_counts <- count_events_before(goal_events, boundaries)
  gf_home <- goal_counts$home
  ga_home <- goal_counts$away

  red_counts <- count_events_before(red_card_events, boundaries)
  red_home <- red_counts$home
  red_away <- red_counts$away

  # Calculate per-splint goals (goals scored IN each splint)
  goals_in_splint <- count_events_in_splint(goal_events, boundaries)
  goals_home <- goals_in_splint$home
  goals_away <- goals_in_splint$away

  # Calculate player counts (11 minus red cards)
  n_players_home <- 11 - red_home
  n_players_away <- 11 - red_away

  data.frame(
    splint_num = seq_len(n_splints),
    start_minute = boundaries[-length(boundaries)],
    end_minute = boundaries[-1],
    duration = diff(boundaries),
    avg_min = (boundaries[-length(boundaries)] + boundaries[-1]) / 2,
    gf_home = gf_home,
    ga_home = ga_home,
    goals_home = goals_home,
    goals_away = goals_away,
    red_home = red_home,
    red_away = red_away,
    n_players_home = n_players_home,
    n_players_away = n_players_away
  )
}


#' Assign players to splints
#'
#' Determines which players were on the pitch for each splint.
#' Uses lineup minutes and substitution events to track when players
#' were actually on the pitch.
#'
#' @param boundaries Data frame of splint boundaries
#' @param lineups Data frame of match lineups
#' @param events Data frame of match events (for substitutions)
#' @param match_id Match identifier
#'
#' @return Data frame with player assignments per splint
#' @export
assign_players_to_splints <- function(boundaries, lineups, events, match_id) {
  # Handle NULL or empty lineups
  if (is.null(lineups) || nrow(lineups) == 0) {
    return(data.frame(
      match_id = character(0),
      team = character(0),
      is_home = logical(0),
      player_name = character(0),
      player_id = character(0),
      splint_num = integer(0),
      start_minute = numeric(0),
      end_minute = numeric(0)
    ))
  }

  # Get match lineups - only players who actually played
  match_lineups <- lineups %>%
    dplyr::filter(.data$match_id == !!match_id, .data$minutes > 0)

  if (nrow(match_lineups) == 0) {
    return(data.frame(
      match_id = character(0),
      team = character(0),
      is_home = logical(0),
      player_name = character(0),
      player_id = character(0),
      splint_num = integer(0),
      start_minute = numeric(0),
      end_minute = numeric(0)
    ))
  }

  # Calculate on/off minutes for each player
  # Starters: on at minute 0
  # Subs: on at (90 - minutes) approximately
  # This is an approximation since we don't have exact sub timing
  player_timing <- match_lineups %>%
    dplyr::mutate(
      # Starters come on at minute 0, subs come on at (90 - minutes_played)
      on_minute = dplyr::if_else(.data$is_starter, 0, pmax(0, 90 - .data$minutes)),
      # Everyone who isn't subbed off stays until 90
      # Players who are subbed off: their off_minute = on_minute + minutes
      off_minute = pmin(90, .data$on_minute + .data$minutes)
    ) %>%
    dplyr::select(match_id, team, is_home, player_name, player_id, on_minute, off_minute)

  # For each splint, determine players on pitch
  splint_players <- purrr::map_dfr(seq_len(nrow(boundaries)), function(i) {
    start_min <- boundaries$start_minute[i]
    end_min <- boundaries$end_minute[i]

    # A player is on pitch if they:
    # - Came on before or at the splint start (on_minute <= start_min)
    # - Left after the splint start (off_minute > start_min)
    on_pitch <- player_timing %>%
      dplyr::filter(
        .data$on_minute <= start_min,
        .data$off_minute > start_min
      ) %>%
      dplyr::select(match_id, team, is_home, player_name, player_id)

    if (nrow(on_pitch) == 0) {
      return(NULL)
    }

    on_pitch$splint_num <- i
    on_pitch$start_minute <- start_min
    on_pitch$end_minute <- end_min

    on_pitch
  })

  splint_players
}


#' Calculate splint-level xG differential
#'
#' Calculates the non-penalty xG differential for each splint.
#'
#' @param boundaries Data frame of splint boundaries
#' @param shooting Processed shooting data
#' @param match_id Match identifier
#' @param home_team Name of the home team
#' @param away_team Name of the away team
#'
#' @return Data frame with npxGD for each splint
#' @export
calculate_splint_npxgd <- function(boundaries, shooting, match_id,
                                    home_team = NULL, away_team = NULL) {
  # Handle NULL or empty shooting data
  if (is.null(shooting) || nrow(shooting) == 0) {
    return(data.frame(
      splint_num = seq_len(nrow(boundaries)),
      start_minute = boundaries$start_minute,
      end_minute = boundaries$end_minute,
      duration = boundaries$duration,
      npxg_home = 0,
      npxg_away = 0,
      npxgd = 0,
      npxgd_per_90 = 0
    ))
  }

  match_shots <- shooting %>%
    dplyr::filter(.data$match_id == !!match_id,
                  !.data$is_penalty)

  # Handle case with no shots
  if (nrow(match_shots) == 0) {
    return(data.frame(
      splint_num = seq_len(nrow(boundaries)),
      start_minute = boundaries$start_minute,
      end_minute = boundaries$end_minute,
      duration = boundaries$duration,
      npxg_home = 0,
      npxg_away = 0,
      npxgd = 0,
      npxgd_per_90 = 0
    ))
  }

  # Assign shots to splints based on minute
  splint_xg <- purrr::map_dfr(seq_len(nrow(boundaries)), function(i) {
    start_min <- boundaries$start_minute[i]
    end_min <- boundaries$end_minute[i]

    splint_shots <- match_shots %>%
      dplyr::filter(.data$minute >= start_min,
                    .data$minute < end_min)

    # Calculate xG by team - properly identify home vs away
    if (nrow(splint_shots) == 0) {
      npxg_home <- 0
      npxg_away <- 0
    } else {
      team_xg <- splint_shots %>%
        dplyr::group_by(.data$team) %>%
        dplyr::summarise(xg = sum(.data$xg, na.rm = TRUE), .groups = "drop")

      # Match by team name if provided, otherwise use first/second as fallback
      if (!is.null(home_team)) {
        npxg_home <- sum(team_xg$xg[team_xg$team == home_team], na.rm = TRUE)
        npxg_away <- sum(team_xg$xg[team_xg$team == away_team], na.rm = TRUE)
      } else {
        # Fallback to old behavior (less reliable)
        npxg_home <- if (nrow(team_xg) >= 1) team_xg$xg[1] else 0
        npxg_away <- if (nrow(team_xg) >= 2) team_xg$xg[2] else 0
      }
    }

    data.frame(
      splint_num = i,
      start_minute = start_min,
      end_minute = end_min,
      duration = end_min - start_min,
      npxg_home = npxg_home,
      npxg_away = npxg_away,
      stringsAsFactors = FALSE
    )
  })

  splint_xg %>%
    dplyr::mutate(
      npxgd = .data$npxg_home - .data$npxg_away,
      npxgd_per_90 = safe_divide(.data$npxgd * 90, .data$duration)
    )
}


#' Create splints for a single match
#'
#' Master function to create all splint data for one match.
#'
#' @param match_id Match identifier
#' @param events Processed events data
#' @param lineups Processed lineups data
#' @param shooting Processed shooting data
#' @param results Processed match results
#' @param include_goals Whether to create splints at goal times
#'
#' @return List with splint data for the match
#' @export
create_match_splints <- function(match_id, events, lineups, shooting, results,
                                  include_goals = TRUE) {
  # Store match_id as local variable to avoid scoping issues
  current_match_id <- match_id

  # Get match info first (needed for home/away team identification)
  # Include league and season_end_year if available (for multi-league)
  info_cols <- c("match_id", "season", "home_team", "away_team")
  if ("league" %in% names(results)) info_cols <- c(info_cols, "league")
  if ("season_end_year" %in% names(results)) info_cols <- c(info_cols, "season_end_year")
  if ("country" %in% names(results)) info_cols <- c(info_cols, "country")

  match_info <- results %>%
    dplyr::filter(.data$match_id == !!current_match_id) %>%
    dplyr::select(dplyr::any_of(info_cols))

  home_team <- if (nrow(match_info) > 0) match_info$home_team[1] else NULL
  away_team <- if (nrow(match_info) > 0) match_info$away_team[1] else NULL

  # Extract events for this match
  match_events <- extract_match_events(events, current_match_id)

  # Create splint boundaries
  boundaries <- create_splint_boundaries(match_events, include_goals = include_goals)

  # Add match_id to boundaries early
  boundaries$match_id <- current_match_id

  # Add league/season to boundaries if available
  if ("league" %in% names(match_info) && nrow(match_info) > 0) {
    boundaries$league <- match_info$league[1]
  }
  if ("season_end_year" %in% names(match_info) && nrow(match_info) > 0) {
    boundaries$season_end_year <- match_info$season_end_year[1]
  }
  if ("country" %in% names(match_info) && nrow(match_info) > 0) {
    boundaries$country <- match_info$country[1]
  }

  # Assign players to splints
  players <- assign_players_to_splints(boundaries, lineups, events, current_match_id)

  # Calculate xG for each splint - pass home/away teams for proper identification
  xg_data <- calculate_splint_npxgd(boundaries, shooting, current_match_id,
                                     home_team = home_team, away_team = away_team)

  # Combine - match_id is already in boundaries
  splints <- boundaries %>%
    dplyr::left_join(xg_data %>%
                       dplyr::select(splint_num, npxg_home, npxg_away, npxgd, npxgd_per_90),
                     by = "splint_num")

  list(
    match_id = current_match_id,
    match_info = match_info,
    splints = splints,
    players = players
  )
}


#' Create splints for all matches
#'
#' Generates splint data for an entire dataset.
#' Uses data.table for fast pre-splitting by match_id.
#'
#' @param processed_data List of processed data from process_all_data
#' @param include_goals Whether to create splints at goal times
#' @param verbose Print progress messages
#'
#' @return List with combined splint data
#' @export
create_all_splints <- function(processed_data, include_goals = TRUE, verbose = TRUE) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required. Install with: install.packages('data.table')")
  }

  match_ids <- unique(processed_data$results$match_id)
  n_matches <- length(match_ids)

  if (verbose) {
    message(sprintf("Creating splints for %d matches", n_matches))
    message("  Pre-splitting data by match_id...")
  }

  # Pre-split data by match_id for O(1) lookup (HUGE speedup)
  # Convert to data.table and split
  dt_lineups <- data.table::as.data.table(processed_data$lineups)
  dt_shooting <- data.table::as.data.table(processed_data$shooting)
  dt_results <- data.table::as.data.table(processed_data$results)

  # Create named lists for O(1) lookup by match_id
  # Use base R split with the match_id column as factor
  lineups_by_match <- split(dt_lineups, dt_lineups$match_id)
  shooting_by_match <- split(dt_shooting, dt_shooting$match_id)
  results_by_match <- split(dt_results, dt_results$match_id)

  # Events may be NULL
  events_by_match <- NULL
  if (!is.null(processed_data$events) && nrow(processed_data$events) > 0) {
    dt_events <- data.table::as.data.table(processed_data$events)
    events_by_match <- split(dt_events, dt_events$match_id)
  }

  # Stats summary for red cards (crd_r column)
  stats_by_match <- NULL
  if (!is.null(processed_data$stats_summary) && nrow(processed_data$stats_summary) > 0) {
    dt_stats <- data.table::as.data.table(processed_data$stats_summary)

    # Add match_id if not present (join via match_url)
    if (!"match_id" %in% names(dt_stats) && "match_url" %in% names(dt_stats)) {
      match_id_lookup <- unique(dt_results[, c("match_url", "match_id"), with = FALSE])
      dt_stats <- merge(dt_stats, match_id_lookup, by = "match_url", all.x = TRUE)
    }

    # Only keep rows with red cards to save memory
    if ("crd_r" %in% names(dt_stats) && "match_id" %in% names(dt_stats)) {
      # Use explicit column reference and convert to numeric (may be character from FBref)
      crd_r_vals <- as.numeric(dt_stats[["crd_r"]])
      red_card_rows <- which(!is.na(crd_r_vals) & crd_r_vals > 0)
      if (length(red_card_rows) > 0) {
        # Use [i, ] with comma to ensure row selection (not column selection)
        dt_stats <- dt_stats[red_card_rows, ]
        stats_by_match <- split(dt_stats, dt_stats$match_id)
      }
    }
  }

  if (verbose) {
    message("  Processing matches...")
  }

  # Pre-allocate lists for results
  all_splints <- vector("list", n_matches)
  all_players <- vector("list", n_matches)
  all_match_info <- vector("list", n_matches)

  # Progress tracking
  last_pct <- 0

  for (i in seq_along(match_ids)) {
    mid <- match_ids[i]

    # Progress update every 10%
    if (verbose) {
      pct <- floor(i / n_matches * 10) * 10
      if (pct > last_pct) {
        message(sprintf("  %d%% (%d/%d matches)", pct, i, n_matches))
        last_pct <- pct
      }
    }

    tryCatch({
      # O(1) lookup instead of filtering full tables
      match_lineups <- lineups_by_match[[mid]]
      match_shooting <- shooting_by_match[[mid]]
      match_results <- results_by_match[[mid]]
      match_events <- if (!is.null(events_by_match)) events_by_match[[mid]] else NULL
      match_stats <- if (!is.null(stats_by_match)) stats_by_match[[mid]] else NULL

      # Convert back to data.frame for existing functions
      if (!is.null(match_lineups)) match_lineups <- as.data.frame(match_lineups)
      if (!is.null(match_shooting)) match_shooting <- as.data.frame(match_shooting)
      if (!is.null(match_results)) match_results <- as.data.frame(match_results)
      if (!is.null(match_events)) match_events <- as.data.frame(match_events)
      if (!is.null(match_stats)) match_stats <- as.data.frame(match_stats)

      result <- create_match_splints_fast(
        match_id = mid,
        events = match_events,
        lineups = match_lineups,
        shooting = match_shooting,
        results = match_results,
        stats = match_stats,
        include_goals = include_goals
      )

      all_splints[[i]] <- result$splints
      all_players[[i]] <- result$players
      all_match_info[[i]] <- result$match_info

    }, error = function(e) {
      warning(paste("Failed to create splints for match:", mid, "-", e$message))
    })
  }

  if (verbose) {
    message("  Combining results...")
  }

  # Fast row-binding with data.table::rbindlist
  combined <- list(
    splints = data.table::rbindlist(all_splints[!sapply(all_splints, is.null)], fill = TRUE),
    players = data.table::rbindlist(all_players[!sapply(all_players, is.null)], fill = TRUE),
    match_info = data.table::rbindlist(all_match_info[!sapply(all_match_info, is.null)], fill = TRUE)
  )

  # Convert back to data.frame and add splint_id
  combined$splints <- as.data.frame(combined$splints)
  combined$players <- as.data.frame(combined$players)
  combined$match_info <- as.data.frame(combined$match_info)

  # Add splint_id (vectorized)
  if (nrow(combined$splints) > 0) {
    combined$splints$splint_id <- paste(combined$splints$match_id,
                                         combined$splints$splint_num, sep = "_")
  }
  if (nrow(combined$players) > 0) {
    combined$players$splint_id <- paste(combined$players$match_id,
                                         combined$players$splint_num, sep = "_")
  }

  if (verbose) {
    message(sprintf("Created %d splints from %d matches",
                    nrow(combined$splints), n_matches))
  }

  combined
}


#' Create splints for a single match (fast version)
#'
#' Optimized version that takes pre-filtered data for a single match.
#' Derives goal times from shooting data if events are not available.
#' Derives red card times from stats_summary if available.
#'
#' @param match_id Match identifier
#' @param events Pre-filtered events for this match
#' @param lineups Pre-filtered lineups for this match
#' @param shooting Pre-filtered shooting for this match
#' @param results Pre-filtered results for this match
#' @param stats Pre-filtered stats_summary for this match (for red cards)
#' @param include_goals Whether to create splints at goal times
#'
#' @return List with splint data for the match
#' @keywords internal
create_match_splints_fast <- function(match_id, events, lineups, shooting, results,
                                       stats = NULL, include_goals = TRUE) {
  current_match_id <- match_id

  # Get match info
  info_cols <- c("match_id", "season", "home_team", "away_team")
  if (!is.null(results) && "league" %in% names(results)) info_cols <- c(info_cols, "league")
  if (!is.null(results) && "season_end_year" %in% names(results)) info_cols <- c(info_cols, "season_end_year")
  if (!is.null(results) && "country" %in% names(results)) info_cols <- c(info_cols, "country")

  match_info <- if (!is.null(results) && nrow(results) > 0) {
    results[1, intersect(info_cols, names(results)), drop = FALSE]
  } else {
    data.frame(match_id = current_match_id)
  }

  home_team <- if (!is.null(results) && nrow(results) > 0) results$home_team[1] else NULL
  away_team <- if (!is.null(results) && nrow(results) > 0) results$away_team[1] else NULL

  # Derive goal times from shooting data if events are NULL/empty
  goal_times <- NULL
  goal_is_home <- NULL
  if (include_goals && !is.null(shooting) && nrow(shooting) > 0) {
    goals <- shooting[shooting$is_goal == TRUE & !is.na(shooting$is_goal), , drop = FALSE]
    if (nrow(goals) > 0) {
      goal_times <- goals$minute[!is.na(goals$minute)]
      # Determine if home team scored
      if (!is.null(home_team) && "team" %in% names(goals)) {
        goal_is_home <- goals$team[!is.na(goals$minute)] == home_team
      }
    }
  }

  # Get sub times from lineups using pre-calculated on_minute
  # (from leading space detection in 01_load_pannadata.R)
  sub_times <- extract_sub_events(lineups)

  # Derive red card times - use off_minute from lineups if available (more accurate for subs)
  # For a sub who came on at 45 and got red card after 25 mins, off_minute = 70 (correct)
  # Using raw minutes would incorrectly give 25
  red_card_times <- NULL
  red_card_is_home <- NULL

  # Prefer lineups with pre-calculated off_minute (handles subs correctly)
  if (!is.null(lineups) && nrow(lineups) > 0 && "crd_r" %in% names(lineups)) {
    # Convert crd_r to numeric (may be character from FBref)
    crd_r_num <- as.numeric(lineups$crd_r)
    red_card_idx <- which(!is.na(crd_r_num) & crd_r_num > 0)
    if (length(red_card_idx) > 0) {
      red_cards <- lineups[red_card_idx, , drop = FALSE]
      # Use off_minute if available (accurate for subs), else fall back to minutes
      if ("off_minute" %in% names(red_cards)) {
        red_card_times <- red_cards$off_minute
      } else if ("minutes" %in% names(red_cards)) {
        red_card_times <- as.numeric(red_cards$minutes)
      }
      red_card_times <- red_card_times[!is.na(red_card_times) & red_card_times > 0]
      # Determine if home team got red card
      if (!is.null(home_team) && "team" %in% names(red_cards) && length(red_card_times) > 0) {
        red_card_is_home <- red_cards$team == home_team
        if ("off_minute" %in% names(red_cards)) {
          red_card_is_home <- red_card_is_home[!is.na(red_cards$off_minute) & red_cards$off_minute > 0]
        }
      }
    }
  } else if (!is.null(stats) && nrow(stats) > 0 && "crd_r" %in% names(stats)) {
    # Fallback to stats if lineups doesn't have crd_r
    crd_r_num <- as.numeric(stats$crd_r)
    red_card_idx <- which(!is.na(crd_r_num) & crd_r_num > 0)
    if (length(red_card_idx) > 0 && "min" %in% names(stats)) {
      red_cards <- stats[red_card_idx, , drop = FALSE]
      red_card_times <- as.numeric(red_cards$min)
      red_card_times <- red_card_times[!is.na(red_card_times) & red_card_times > 0]
      if (!is.null(home_team) && "team" %in% names(red_cards) && length(red_card_times) > 0) {
        red_card_is_home <- red_cards$team == home_team
      }
    }
  }

  # Create splint boundaries with derived goal/sub/red card times
  # Pass shooting data for more accurate match end timing
  boundaries <- create_splint_boundaries_fast(
    events = events,
    shots = shooting,
    include_goals = include_goals,
    goal_times = goal_times,
    goal_is_home = goal_is_home,
    sub_times = sub_times,
    red_card_times = red_card_times,
    red_card_is_home = red_card_is_home
  )
  boundaries$match_id <- current_match_id

  # Add league/season to boundaries
  if (!is.null(results) && nrow(results) > 0) {
    if ("league" %in% names(results)) boundaries$league <- results$league[1]
    if ("season_end_year" %in% names(results)) boundaries$season_end_year <- results$season_end_year[1]
    if ("country" %in% names(results)) boundaries$country <- results$country[1]
  }

  # Assign players (lineups already filtered)
  players <- assign_players_to_splints_fast(boundaries, lineups, current_match_id)

  # Calculate xG (shooting already filtered)
  xg_data <- calculate_splint_npxgd_fast(boundaries, shooting, home_team, away_team)

  # Combine
  splints <- merge(boundaries, xg_data[, c("splint_num", "npxg_home", "npxg_away", "npxgd", "npxgd_per_90")],
                   by = "splint_num", all.x = TRUE)

  list(
    match_id = current_match_id,
    match_info = match_info,
    splints = splints,
    players = players
  )
}


#' Create splint boundaries (fast version)
#'
#' @param events Event data (may be NULL)
#' @param shots Shots data with minute column (may be NULL)
#' @param include_goals Whether to include goal boundaries
#' @param include_halftime Whether to include halftime boundary
#' @param goal_times Numeric vector of goal minutes (derived from shooting)
#' @param goal_is_home Logical vector indicating if each goal was by home team
#' @param sub_times Numeric vector of substitution minutes (derived from lineups)
#' @param red_card_times Numeric vector of red card minutes (derived from stats)
#' @param red_card_is_home Logical vector indicating if each red card was for home team
#' @keywords internal
create_splint_boundaries_fast <- function(events, shots = NULL, include_goals = TRUE, include_halftime = TRUE,
                                           goal_times = NULL, goal_is_home = NULL, sub_times = NULL,
                                           red_card_times = NULL, red_card_is_home = NULL) {
  # Calculate match end using both events and shots (falls back gracefully)
  match_end <- calculate_match_end(events, shots = shots, default_end = 91)
  first_half_end <- calculate_first_half_end(events, shots = shots, default_end = 46)

  boundaries <- c(0, match_end)
  if (include_halftime) boundaries <- c(boundaries, first_half_end)

  goal_events <- NULL
  red_card_events <- NULL

  # First try to get events from the events data frame
  if (!is.null(events) && nrow(events) > 0) {
    # Substitutions from events (handle multiple event type formats)
    if ("is_sub" %in% names(events) || "event_type" %in% names(events)) {
      is_sub <- if ("is_sub" %in% names(events)) events$is_sub else
        events$event_type %in% c("sub_on", "substitution", "Substitution")
      event_sub_times <- events$minute[is_sub & !is.na(is_sub)]
      boundaries <- c(boundaries, event_sub_times)
    }

    # Goals from events (handle multiple event type formats)
    if (include_goals) {
      is_goal <- if ("is_goal" %in% names(events)) events$is_goal else
        events$event_type %in% c("goal", "Goal", "penalty_goal", "own_goal")
      goal_mask <- is_goal & !is.na(is_goal)
      if (any(goal_mask)) {
        goal_events <- events[goal_mask, c("minute", "is_home"), drop = FALSE]
        boundaries <- c(boundaries, goal_events$minute)
      }
    }

    # Red cards from events (handle multiple event type formats)
    is_red <- if ("is_red_card" %in% names(events)) events$is_red_card else
      if ("event_type" %in% names(events))
        events$event_type %in% c("red_card", "Red Card", "yellow_red_card") else
        rep(FALSE, nrow(events))
    red_mask <- is_red & !is.na(is_red)
    if (any(red_mask)) {
      red_card_events <- events[red_mask, c("minute", "is_home"), drop = FALSE]
      boundaries <- c(boundaries, red_card_events$minute)
    }
  }

  # Use derived goal times if events didn't provide them
  if (include_goals && is.null(goal_events) && !is.null(goal_times) && length(goal_times) > 0) {
    boundaries <- c(boundaries, goal_times)
    # Create goal_events structure for counting
    goal_events <- data.frame(
      minute = goal_times,
      is_home = if (!is.null(goal_is_home)) goal_is_home else rep(NA, length(goal_times))
    )
  }

  # Use derived sub times if events didn't provide them
  if (!is.null(sub_times) && length(sub_times) > 0) {
    boundaries <- c(boundaries, sub_times)
  }

  # Use derived red card times if events didn't provide them
  if (is.null(red_card_events) && !is.null(red_card_times) && length(red_card_times) > 0) {
    boundaries <- c(boundaries, red_card_times)
    # Create red_card_events structure for counting
    red_card_events <- data.frame(
      minute = red_card_times,
      is_home = if (!is.null(red_card_is_home)) red_card_is_home else rep(NA, length(red_card_times))
    )
  }

  boundaries <- sort(unique(boundaries[!is.na(boundaries)]))
  n_splints <- length(boundaries) - 1

  if (n_splints < 1) {
    return(data.frame(
      splint_num = 1, start_minute = 0, end_minute = 90, duration = 90, avg_min = 45,
      gf_home = 0, ga_home = 0, goals_home = 0, goals_away = 0,
      red_home = 0, red_away = 0, n_players_home = 11, n_players_away = 11
    ))
  }

  goal_counts <- count_events_before(goal_events, boundaries)
  red_counts <- count_events_before(red_card_events, boundaries)

  # Calculate per-splint goals (goals scored IN each splint)
  goals_in_splint <- count_events_in_splint(goal_events, boundaries)

  data.frame(
    splint_num = seq_len(n_splints),
    start_minute = boundaries[-length(boundaries)],
    end_minute = boundaries[-1],
    duration = diff(boundaries),
    avg_min = (boundaries[-length(boundaries)] + boundaries[-1]) / 2,
    gf_home = goal_counts$home,
    ga_home = goal_counts$away,
    goals_home = goals_in_splint$home,
    goals_away = goals_in_splint$away,
    red_home = red_counts$home,
    red_away = red_counts$away,
    n_players_home = 11 - red_counts$home,
    n_players_away = 11 - red_counts$away
  )
}


#' Assign players to splints (fast version)
#' @keywords internal
assign_players_to_splints_fast <- function(boundaries, lineups, match_id) {
  if (is.null(lineups) || nrow(lineups) == 0) {
    return(data.frame(
      match_id = character(0), team = character(0), is_home = logical(0),
      player_name = character(0), player_id = character(0),
      splint_num = integer(0), start_minute = numeric(0), end_minute = numeric(0)
    ))
  }

  # Filter to players who played
  lineups <- lineups[lineups$minutes > 0, , drop = FALSE]
  if (nrow(lineups) == 0) {
    return(data.frame(
      match_id = character(0), team = character(0), is_home = logical(0),
      player_name = character(0), player_id = character(0),
      splint_num = integer(0), start_minute = numeric(0), end_minute = numeric(0)
    ))
  }

  # Use pre-calculated on/off times from leading space detection if available
  # Otherwise fall back to the old approximation method
  if ("on_minute" %in% names(lineups) && "off_minute" %in% names(lineups)) {
    on_minute <- lineups$on_minute
    off_minute <- lineups$off_minute
  } else {
    # Fallback: approximate from is_starter and minutes
    is_starter <- if ("is_starter" %in% names(lineups)) lineups$is_starter else TRUE
    on_minute <- ifelse(is_starter, 0, pmax(0, 90 - lineups$minutes))
    off_minute <- pmin(90, on_minute + lineups$minutes)
  }

  # Vectorized assignment: for each player, find which splints they were on for
  n_splints <- nrow(boundaries)
  n_players <- nrow(lineups)

  # Create result list
  result_list <- vector("list", n_splints)

  for (s in seq_len(n_splints)) {
    start_min <- boundaries$start_minute[s]

    # Player is on pitch if: on_minute <= start_min AND off_minute > start_min
    on_pitch <- (on_minute <= start_min) & (off_minute > start_min)

    if (any(on_pitch)) {
      result_list[[s]] <- data.frame(
        match_id = match_id,
        team = lineups$team[on_pitch],
        is_home = lineups$is_home[on_pitch],
        player_name = lineups$player_name[on_pitch],
        player_id = lineups$player_id[on_pitch],
        splint_num = s,
        start_minute = start_min,
        end_minute = boundaries$end_minute[s],
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, result_list[!sapply(result_list, is.null)])
}


#' Calculate splint xG (fast version)
#' @keywords internal
calculate_splint_npxgd_fast <- function(boundaries, shooting, home_team, away_team) {
  n_splints <- nrow(boundaries)

  if (is.null(shooting) || nrow(shooting) == 0) {
    return(data.frame(
      splint_num = seq_len(n_splints),
      npxg_home = rep(0, n_splints),
      npxg_away = rep(0, n_splints),
      npxgd = rep(0, n_splints),
      npxgd_per_90 = rep(0, n_splints)
    ))
  }

  # Filter non-penalty shots once
  np_shots <- shooting[!shooting$is_penalty, , drop = FALSE]

  if (nrow(np_shots) == 0) {
    return(data.frame(
      splint_num = seq_len(n_splints),
      npxg_home = rep(0, n_splints),
      npxg_away = rep(0, n_splints),
      npxgd = rep(0, n_splints),
      npxgd_per_90 = rep(0, n_splints)
    ))
  }

  # Vectorized splint assignment using findInterval
  shot_minutes <- np_shots$minute
  # findInterval returns which interval (splint) each shot falls into
  shot_splint <- findInterval(shot_minutes, boundaries$start_minute)
  # Adjust for shots exactly at end_minute (should go to that splint, not next)
  shot_splint[shot_splint > n_splints] <- n_splints

  # Identify home/away shots
  is_home_shot <- np_shots$team == home_team

  # Aggregate xG by splint and team
  npxg_home <- numeric(n_splints)
  npxg_away <- numeric(n_splints)

  for (s in seq_len(n_splints)) {
    in_splint <- shot_splint == s
    if (any(in_splint)) {
      npxg_home[s] <- sum(np_shots$xg[in_splint & is_home_shot], na.rm = TRUE)
      npxg_away[s] <- sum(np_shots$xg[in_splint & !is_home_shot], na.rm = TRUE)
    }
  }

  npxgd <- npxg_home - npxg_away
  duration <- boundaries$duration
  npxgd_per_90 <- ifelse(duration > 0, npxgd * 90 / duration, 0)

  data.frame(
    splint_num = seq_len(n_splints),
    npxg_home = npxg_home,
    npxg_away = npxg_away,
    npxgd = npxgd,
    npxgd_per_90 = npxgd_per_90
  )
}


#' Get players on pitch for a splint
#'
#' Returns the player IDs for home and away teams in a splint.
#'
#' @param splint_data Combined splint data from create_all_splints
#' @param splint_id Splint identifier
#'
#' @return List with home_players and away_players character vectors
#' @export
get_splint_players <- function(splint_data, splint_id) {
  players <- splint_data$players %>%
    dplyr::filter(.data$splint_id == !!splint_id)

  list(
    home_players = players$player_id[players$is_home],
    away_players = players$player_id[!players$is_home]
  )
}


# Opta Data Adapters for Splint Creation ----
# These functions convert Opta data formats to the format expected by
# the existing splint creation functions.

#' Prepare Opta Events for Splint Creation
#'
#' Converts Opta event data to the format expected by splint creation functions.
#' Opta events have: event_type (goal, substitution, red_card, yellow_card),
#' minute, second, team_id, player_id, etc.
#'
#' @param opta_events Data frame from load_opta_events()
#' @param match_results Data frame with home_team, away_team, team_id mapping
#'
#' @return Data frame with columns: match_id, minute, added_time, event_type,
#'   is_goal, is_sub, is_red_card, is_home, player_id, player_name
#'
#' @keywords internal
prepare_opta_events_for_splints <- function(opta_events, match_results = NULL) {
  if (is.null(opta_events) || nrow(opta_events) == 0) {
    return(data.frame(
      match_id = character(0),
      minute = numeric(0),
      added_time = numeric(0),
      event_type = character(0),
      is_goal = logical(0),
      is_sub = logical(0),
      is_red_card = logical(0),
      is_home = logical(0),
      player_id = character(0),
      player_name = character(0)
    ))
  }

  # Convert event_type to boolean flags
  events <- opta_events %>%
    dplyr::mutate(
      minute = as.numeric(.data$minute),
      added_time = if ("second" %in% names(.)) as.numeric(.data$second) / 60 else 0,
      is_goal = .data$event_type %in% c("goal"),
      is_sub = .data$event_type %in% c("substitution"),
      is_red_card = .data$event_type %in% c("red_card", "second_yellow")
    )

  # Determine is_home based on team_position if available, else use match_results
  if ("team_position" %in% names(events)) {
    events$is_home <- tolower(events$team_position) == "home"
  } else if (!is.null(match_results) && "home_team_id" %in% names(match_results)) {
    # Join with match_results to get home/away
    events <- events %>%
      dplyr::left_join(
        match_results %>% dplyr::select(match_id, home_team_id),
        by = "match_id"
      ) %>%
      dplyr::mutate(is_home = .data$team_id == .data$home_team_id) %>%
      dplyr::select(-home_team_id)
  } else {
    # Fallback: set is_home to NA (will be inferred later)
    events$is_home <- NA
  }

  # Select and rename columns for compatibility
  events %>%
    dplyr::select(
      match_id, minute, added_time, event_type,
      is_goal, is_sub, is_red_card, is_home,
      player_id, player_name
    )
}


#' Prepare Opta Lineups for Splint Creation
#'
#' Converts Opta lineup data to the format expected by splint creation functions.
#' Opta lineups have: is_starter, minutes_played, sub_on_minute, sub_off_minute.
#'
#' @param opta_lineups Data frame from load_opta_lineups()
#'
#' @return Data frame with columns: match_id, player_id, player_name, team,
#'   is_home, is_starter, minutes, on_minute, off_minute
#'
#' @keywords internal
prepare_opta_lineups_for_splints <- function(opta_lineups) {
  if (is.null(opta_lineups) || nrow(opta_lineups) == 0) {
    return(data.frame(
      match_id = character(0),
      player_id = character(0),
      player_name = character(0),
      team = character(0),
      is_home = logical(0),
      is_starter = logical(0),
      minutes = numeric(0),
      on_minute = numeric(0),
      off_minute = numeric(0)
    ))
  }

  lineups <- opta_lineups %>%
    dplyr::mutate(
      # Map Opta column names to expected names
      team = if ("team_name" %in% names(.)) .data$team_name else .data$team_id,
      minutes = as.numeric(if ("minutes_played" %in% names(.)) .data$minutes_played else 0),
      # Opta provides explicit on/off times
      on_minute = if ("sub_on_minute" %in% names(.)) {
        dplyr::if_else(.data$is_starter, 0, as.numeric(.data$sub_on_minute))
      } else {
        dplyr::if_else(.data$is_starter, 0, NA_real_)
      },
      off_minute = if ("sub_off_minute" %in% names(.)) {
        # If sub_off_minute is 0 or NA, player played to end (90+ min)
        sub_off <- as.numeric(.data$sub_off_minute)
        dplyr::if_else(is.na(sub_off) | sub_off == 0, 90, sub_off)
      } else {
        90  # Default to full match
      },
      # Determine is_home from team_position
      is_home = if ("team_position" %in% names(.)) {
        tolower(.data$team_position) == "home"
      } else {
        NA
      }
    )

  # Ensure off_minute is at least on_minute + minutes
  lineups$off_minute <- pmax(lineups$off_minute, lineups$on_minute + lineups$minutes)

  lineups %>%
    dplyr::select(
      match_id, player_id, player_name, team, is_home,
      is_starter, minutes, on_minute, off_minute
    )
}


#' Prepare Opta Shot Events for Splint xG Calculation
#'
#' Converts Opta shot event data to the format expected by splint xG calculation.
#' Note: Opta API does not provide xG values directly - they must be calculated
#' from x/y coordinates using an xG model, or goals can be used as a proxy.
#'
#' @param opta_shot_events Data frame from load_opta_shot_events()
#' @param use_goals_as_xg Logical. If TRUE, use is_goal as xG (1 for goal, 0 otherwise).
#'   This is a fallback when no xG model is available. Default FALSE.
#' @param match_results Optional data frame with home_team, team_id mapping
#'
#' @return Data frame with columns: match_id, minute, team, player_id, player_name,
#'   xg, is_goal, is_penalty
#'
#' @keywords internal
prepare_opta_shots_for_splints <- function(opta_shot_events, use_goals_as_xg = FALSE,
                                            match_results = NULL) {
  if (is.null(opta_shot_events) || nrow(opta_shot_events) == 0) {
    return(data.frame(
      match_id = character(0),
      minute = numeric(0),
      team = character(0),
      player_id = character(0),
      player_name = character(0),
      xg = numeric(0),
      is_goal = logical(0),
      is_penalty = logical(0)
    ))
  }

  shots <- opta_shot_events %>%
    dplyr::mutate(
      minute = as.numeric(.data$minute),
      # Determine team name from team_id
      team = if ("team_name" %in% names(.)) .data$team_name else .data$team_id,
      # xG handling - Opta API doesn't provide xG, need to calculate or use goals
      xg = if ("xg" %in% names(.)) {
        as.numeric(.data$xg)
      } else if (use_goals_as_xg) {
        dplyr::if_else(.data$is_goal, 1, 0)
      } else {
        NA_real_  # Will need xG model to fill this
      },
      # Penalty detection from situation field
      is_penalty = if ("situation" %in% names(.)) {
        tolower(.data$situation) == "penalty"
      } else {
        FALSE
      }
    )

  shots %>%
    dplyr::select(
      match_id, minute, team, player_id, player_name,
      xg, is_goal, is_penalty
    )
}


#' Create Processed Data Structure from Opta Data
#'
#' Creates the processed_data list structure expected by create_all_splints()
#' from Opta data sources. This is the main entry point for using Opta data
#' with the splint creation pipeline.
#'
#' @param opta_lineups Data frame from load_opta_lineups()
#' @param opta_events Data frame from load_opta_events()
#' @param opta_shot_events Data frame from load_opta_shot_events()
#' @param opta_stats Optional data frame from load_opta_stats() (for match metadata)
#' @param use_goals_as_xg Logical. Use goals as xG proxy if TRUE. Default FALSE.
#'
#' @return List with components: lineups, events, shooting, results, stats_summary
#'
#' @export
#' @examples
#' \dontrun{
#' # Load Opta data
#' lineups <- load_opta_lineups("ENG", "2024-2025")
#' events <- load_opta_events("ENG", "2024-2025")
#' shots <- load_opta_shot_events("ENG", "2024-2025")
#'
#' # Create processed data structure
#' processed <- create_opta_processed_data(lineups, events, shots)
#'
#' # Create splints
#' splints <- create_all_splints(processed)
#' }
create_opta_processed_data <- function(opta_lineups, opta_events = NULL,
                                        opta_shot_events = NULL, opta_stats = NULL,
                                        use_goals_as_xg = FALSE) {
  # Create match results from lineups (get unique matches with home/away teams)
  results <- NULL
  if (!is.null(opta_lineups) && nrow(opta_lineups) > 0) {
    results <- opta_lineups %>%
      dplyr::filter(.data$is_starter) %>%
      dplyr::group_by(.data$match_id) %>%
      dplyr::summarise(
        home_team = dplyr::first(.data$team_name[tolower(.data$team_position) == "home"]),
        away_team = dplyr::first(.data$team_name[tolower(.data$team_position) == "away"]),
        home_team_id = dplyr::first(.data$team_id[tolower(.data$team_position) == "home"]),
        away_team_id = dplyr::first(.data$team_id[tolower(.data$team_position) == "away"]),
        match_date = dplyr::first(.data$match_date),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        season = extract_season_from_date(.data$match_date)
      )
  }

  # Prepare lineups
  lineups <- prepare_opta_lineups_for_splints(opta_lineups)

  # Prepare events (if provided)
  events <- if (!is.null(opta_events)) {
    prepare_opta_events_for_splints(opta_events, results)
  } else {
    NULL
  }

  # Prepare shooting (if provided)
  shooting <- if (!is.null(opta_shot_events)) {
    prepare_opta_shots_for_splints(opta_shot_events, use_goals_as_xg, results)
  } else {
    NULL
  }

  # Return in expected format
  list(
    lineups = lineups,
    events = events,
    shooting = shooting,
    results = results,
    stats_summary = opta_stats
  )
}


#' Extract Season from Match Date
#'
#' Determines the season string (e.g., "2024-2025") from a match date.
#' Assumes seasons run August to May: Aug-Dec = first year, Jan-May = second year.
#'
#' @param date Date or character date string (YYYY-MM-DD format)
#'
#' @return Character season string (e.g., "2024-2025")
#' @keywords internal
extract_season_from_date <- function(date) {
  if (is.null(date) || length(date) == 0) return(NA_character_)

  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))

  # If month is Jan-July, season started previous year
  start_year <- ifelse(month <= 7, year - 1, year)
  paste0(start_year, "-", start_year + 1)
}


# Note: count_events_before and count_events_in_splint are defined in utils.R
