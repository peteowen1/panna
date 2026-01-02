# Splint creation functions for panna package
#
# A "splint" is a time segment between substitutions or goals in a match.
# This is the unit of analysis for RAPM models, as the lineup remains
# constant within each splint.

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
      is_goal = .data$event_type == "goal",
      is_sub = .data$event_type == "substitution",
      is_red_card = if ("is_red_card" %in% names(.)) .data$is_red_card else .data$event_type == "red_card"
    )

  match_events
}


#' Create splint boundaries
#'
#' Defines start and end times for each splint based on events.
#' Splints are created at:
#' - Start of match (minute 0)
#' - Each goal
#' - Each substitution
#' - Each red card
#' - Half time (minute 45)
#' - End of match (minute 90)
#'
#' Also tracks game state (cumulative goals, red cards, player counts) at each splint boundary.
#'
#' @param events Data frame of match events
#' @param include_goals Logical, whether to create new splints at goals (default TRUE)
#' @param include_halftime Logical, whether to create splint at halftime (default TRUE)
#'
#' @return Data frame of splint boundaries with game state
#' @export
create_splint_boundaries <- function(events, include_goals = TRUE, include_halftime = TRUE) {
  # Start with match boundaries
  boundaries <- c(0, 90)

  # Add halftime
  if (include_halftime) {
    boundaries <- c(boundaries, 45)
  }

  # Track goals and red cards for game state
  goal_events <- NULL
  red_card_events <- NULL

  # Only process events if we have data
  if (!is.null(events) && nrow(events) > 0) {
    # Add substitution times
    if ("is_sub" %in% names(events)) {
      sub_times <- events$minute[events$is_sub]
      boundaries <- c(boundaries, sub_times)
    }

    # Add goal times and track who scored
    if (include_goals && "is_goal" %in% names(events)) {
      goal_events <- events %>%
        dplyr::filter(.data$is_goal) %>%
        dplyr::select(minute, is_home) %>%
        dplyr::arrange(minute)
      goal_times <- goal_events$minute
      boundaries <- c(boundaries, goal_times)
    }

    # Add red card times and track which team
    if ("is_red_card" %in% names(events)) {
      red_card_events <- events %>%
        dplyr::filter(.data$is_red_card) %>%
        dplyr::select(minute, is_home) %>%
        dplyr::arrange(minute)
      if (nrow(red_card_events) > 0) {
        red_card_times <- red_card_events$minute
        boundaries <- c(boundaries, red_card_times)
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
      end_minute = 90,
      duration = 90,
      avg_min = 45,
      gf_home = 0,
      ga_home = 0,
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
#'
#' @param processed_data List of processed data from process_all_data
#' @param include_goals Whether to create splints at goal times
#' @param verbose Print progress messages
#'
#' @return List with combined splint data
#' @export
create_all_splints <- function(processed_data, include_goals = TRUE, verbose = TRUE) {
  match_ids <- unique(processed_data$results$match_id)

  if (verbose) {
    progress_msg(paste("Creating splints for", length(match_ids), "matches"))
  }

  all_splints <- purrr::map(seq_along(match_ids), function(i) {
    if (verbose && i %% 100 == 0) {
      progress_msg(paste("  Processing match", i, "of", length(match_ids)))
    }

    tryCatch({
      create_match_splints(
        match_id = match_ids[i],
        events = processed_data$events,
        lineups = processed_data$lineups,
        shooting = processed_data$shooting,
        results = processed_data$results,
        include_goals = include_goals
      )
    }, error = function(e) {
      warning(paste("Failed to create splints for match:", match_ids[i]))
      NULL
    })
  })

  # Remove failed matches
  all_splints <- all_splints[!sapply(all_splints, is.null)]

  # Combine into single data frames
  combined <- list(
    splints = purrr::map_dfr(all_splints, ~ .x$splints),
    players = purrr::map_dfr(all_splints, ~ .x$players),
    match_info = purrr::map_dfr(all_splints, ~ .x$match_info)
  )

  # Add splint_id
  combined$splints <- combined$splints %>%
    dplyr::mutate(splint_id = paste(.data$match_id, .data$splint_num, sep = "_"))

  combined$players <- combined$players %>%
    dplyr::mutate(splint_id = paste(.data$match_id, .data$splint_num, sep = "_"))

  if (verbose) {
    progress_msg(paste("Created", nrow(combined$splints), "splints"))
  }

  combined
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
