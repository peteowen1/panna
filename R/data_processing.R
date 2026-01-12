# Data processing and cleaning functions for panna package

#' Process and clean match results
#'
#' Cleans match results data and adds derived columns.
#'
#' @param results Data frame of match results
#'
#' @return Cleaned data frame with standardized columns
#' @export
process_match_results <- function(results) {
  # Column names are already snake_case from clean_column_names()
  # Preserve league columns if present (for multi-league data)
  has_league <- "league" %in% names(results)
  has_country <- "country" %in% names(results)
  has_season_year <- "season_end_year" %in% names(results)

  # Handle both home_xg and home_x_g column naming
  if ("home_x_g" %in% names(results) && !"home_xg" %in% names(results)) {
    results <- results %>%
      dplyr::rename(home_xg = home_x_g, away_xg = away_x_g)
  }

  results <- results %>%
    dplyr::mutate(
      home_team = standardize_team_names(.data$home),
      away_team = standardize_team_names(.data$away),
      home_goals = as.numeric(.data$home_goals),
      away_goals = as.numeric(.data$away_goals),
      home_xg = as.numeric(.data$home_xg),
      away_xg = as.numeric(.data$away_xg),
      date = as.Date(.data$date),
      season = .data$season_end_year
    ) %>%
    dplyr::mutate(
      match_id = create_match_id(
        paste0(as.numeric(.data$season) - 1, "-", .data$season),
        .data$date,
        .data$home_team,
        .data$away_team
      ),
      goal_diff = .data$home_goals - .data$away_goals,
      xg_diff = .data$home_xg - .data$away_xg
    )

  # Select core columns plus optional league columns
  core_cols <- c(
    "match_id", "season", "date", "match_url",
    "home_team", "away_team",
    "home_goals", "away_goals", "goal_diff",
    "home_xg", "away_xg", "xg_diff"
  )
  optional_cols <- c()
  if (has_league) optional_cols <- c(optional_cols, "league")
  if (has_country) optional_cols <- c(optional_cols, "country")
  if (has_season_year) optional_cols <- c(optional_cols, "season_end_year")

  results <- results %>%
    dplyr::select(dplyr::all_of(c(core_cols, optional_cols)))

  results
}


#' Process match lineups
#'
#' Cleans lineup data and extracts player information.
#' Uses data.table for speed with large datasets.
#'
#' @param lineups Data frame of match lineups
#' @param results Processed match results for match IDs
#'
#' @return Cleaned data frame with player lineup info
#' @export
process_match_lineups <- function(lineups, results) {
  if (is.null(lineups) || nrow(lineups) == 0) {
    warning("No lineup data to process")
    return(NULL)
  }

  # Require data.table for speed with large datasets
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required for processing large datasets. Install with: install.packages('data.table')")
  }

  progress_msg(sprintf("  [data.table] Processing %d lineup rows...", nrow(lineups)))

  dt_lineups <- data.table::as.data.table(lineups)
  dt_results <- data.table::as.data.table(results)
  dt_results <- unique(dt_results[, c("match_url", "match_id"), with = FALSE])

  # Merge
  dt_lineups <- merge(dt_lineups, dt_results, by = "match_url", all.x = TRUE)

  # Standardize column names before processing
  # team: could be 'team' or 'squad'
  if (!"team" %in% names(dt_lineups) && "squad" %in% names(dt_lineups)) {
    data.table::setnames(dt_lineups, "squad", "team")
  }

  # player_name: could be 'player_name' or 'player'
  if (!"player_name" %in% names(dt_lineups) && "player" %in% names(dt_lineups)) {
    data.table::setnames(dt_lineups, "player", "player_name")
  }

  # is_home: could be 'is_home', 'home_away' (convert "Home"/"Away" to TRUE/FALSE)
  if (!"is_home" %in% names(dt_lineups)) {
    if ("home_away" %in% names(dt_lineups)) {
      dt_lineups[, is_home := tolower(home_away) == "home"]
    } else {
      # Default to NA if we can't determine
      dt_lineups[, is_home := NA]
    }
  }

  # minutes: could be 'minutes' or 'min'
  if (!"minutes" %in% names(dt_lineups) && "min" %in% names(dt_lineups)) {
    data.table::setnames(dt_lineups, "min", "minutes")
  }

  # Process columns (vectorized) - use set() for speed
  data.table::set(dt_lineups, j = "player_name", value = standardize_player_names(dt_lineups$player_name))
  data.table::set(dt_lineups, j = "team", value = standardize_team_names(dt_lineups$team))
  data.table::set(dt_lineups, j = "minutes", value = as.numeric(dt_lineups$minutes))

  # Handle column name variations
  # Position: could be 'pos' or 'position'
  pos_col <- if ("pos" %in% names(dt_lineups)) dt_lineups$pos else dt_lineups$position

  # Player ID: prefer FBref ID from href, fall back to cleaned player name
  player_id_col <- if ("player_href" %in% names(dt_lineups)) {
    # Extract FBref player ID from href (e.g., /players/d080ed5e/Name -> d080ed5e)
    ids <- extract_fbref_player_id(dt_lineups$player_href)
    # Fall back to clean name where extraction failed
    ids[is.na(ids)] <- clean_player_name(dt_lineups$player_name[is.na(ids)])
    ids
  } else if ("player_url" %in% names(dt_lineups)) {
    ids <- extract_fbref_player_id(dt_lineups$player_url)
    ids[is.na(ids)] <- clean_player_name(dt_lineups$player_name[is.na(ids)])
    ids
  } else if ("player_id" %in% names(dt_lineups)) {
    dt_lineups$player_id
  } else {
    # Use cleaned player_name as ID - lowercase, no whitespace for consistent matching
    clean_player_name(dt_lineups$player_name)
  }

  # is_starter: may not exist
  is_starter_col <- if ("is_starter" %in% names(dt_lineups)) {
    dt_lineups$is_starter
  } else {
    rep(TRUE, nrow(dt_lineups))
  }

  # on_minute and off_minute: used for substitution timing
  on_minute_col <- if ("on_minute" %in% names(dt_lineups)) {
    as.numeric(dt_lineups$on_minute)
  } else {
    rep(NA_real_, nrow(dt_lineups))
  }

  off_minute_col <- if ("off_minute" %in% names(dt_lineups)) {
    as.numeric(dt_lineups$off_minute)
  } else {
    rep(NA_real_, nrow(dt_lineups))
  }

  # Select and rename columns
  result <- data.frame(
    match_id = dt_lineups$match_id,
    team = dt_lineups$team,
    is_home = dt_lineups$is_home,
    player_name = dt_lineups$player_name,
    player_id = player_id_col,
    is_starter = is_starter_col,
    position = pos_col,
    minutes = dt_lineups$minutes,
    on_minute = on_minute_col,
    off_minute = off_minute_col,
    stringsAsFactors = FALSE
  )

  result
}


#' Process match events
#'
#' Cleans event data (goals, substitutions) with timing.
#'
#' @param events Data frame of match events
#' @param results Processed match results for match IDs
#'
#' @return Cleaned data frame with match events
#' @export
process_match_events <- function(events, results) {
  if (is.null(events) || nrow(events) == 0) {
    warning("No event data to process")
    return(NULL)
  }

  # Create match_id lookup with home team for is_home determination
  match_lookup <- results %>%
    dplyr::select(match_url, match_id, home_team, away_team) %>%
    dplyr::distinct()

  # Find the team and player columns

  team_col <- find_column(events, c("team", "squad", "home_team"))
  player_col <- find_column(events, c("player", "player_name"))

  # Ensure event indicator columns exist
  events <- ensure_column(events, "is_goal", source_col = "event_type", pattern = "Goal")
  events <- ensure_column(events, "is_sub", source_col = "event_type", pattern = "Sub")
  events <- ensure_column(events, "is_penalty", source_col = player_col, pattern = "pen")
  events <- ensure_column(events, "is_own_goal", default = FALSE)
  events <- ensure_column(events, "is_red_card", source_col = "event_type",
                          pattern = "Red Card|Sent Off|Second Yellow")

  # Standardize team column name
  if (!is.null(team_col) && team_col != "team") {
    events$team <- events[[team_col]]
  }

  # Standardize player column name
  if (!is.null(player_col) && player_col != "player") {
    events$player <- events[[player_col]]
  }

  # Join with match lookup to get match_id and home/away teams
  events <- dplyr::left_join(events, match_lookup, by = "match_url")

  # Handle case where join failed (missing match_url matches)
  if (!"home_team" %in% names(events) || all(is.na(events$home_team))) {
    warning("Could not match events to results. Events may be missing match_url.")
    # Try to determine is_home from existing data if available
    if ("is_home" %in% names(events)) {
      # Already has is_home
    } else {
      events$is_home <- NA
    }
  }

  # Filter to rows that successfully joined
 events <- events %>%
    dplyr::filter(!is.na(.data$match_id))

  if (nrow(events) == 0) {
    warning("No events matched to results after join")
    return(NULL)
  }

  # Process events
  events <- events %>%
    dplyr::mutate(
      team_std = if ("team" %in% names(.)) standardize_team_names(.data$team) else NA_character_,
      # Determine is_home by comparing team to home_team (if both available)
      is_home = if ("home_team" %in% names(.) && "team_std" %in% names(.)) {
        .data$team_std == .data$home_team
      } else if ("is_home" %in% names(.)) {
        .data$is_home
      } else {
        NA
      },
      # Normalize event_type based on existing flags
      event_type = dplyr::case_when(
        .data$is_goal ~ "goal",
        .data$is_sub ~ "substitution",
        .data$is_red_card ~ "red_card",
        TRUE ~ "other"
      ),
      minute = as.numeric(.data$minute)
    ) %>%
    dplyr::filter(.data$event_type %in% c("goal", "substitution", "red_card"))

  # Select final columns (handling missing columns gracefully)
  result <- events %>%
    dplyr::transmute(
      match_id = .data$match_id,
      team = if ("team_std" %in% names(.)) .data$team_std else .data$team,
      is_home = .data$is_home,
      event_type = .data$event_type,
      minute = .data$minute,
      player_name = if ("player" %in% names(.)) .data$player else NA_character_,
      is_penalty = .data$is_penalty,
      is_own_goal = .data$is_own_goal,
      is_red_card = .data$is_red_card
    )

  result
}


#' Parse event minute from time string
#'
#' Handles formats like "45+2" (45 plus 2 minutes stoppage time).
#'
#' @param time_str Character vector of event times
#'
#' @return Numeric vector of minutes
#' @keywords internal
parse_event_minute <- function(time_str) {
  sapply(time_str, function(t) {
    if (is.na(t) || t == "") return(NA_real_)

    # Handle stoppage time format: "45+2"
    if (grepl("\\+", t)) {
      parts <- strsplit(t, "\\+")[[1]]
      base <- as.numeric(gsub("[^0-9]", "", parts[1]))
      added <- as.numeric(gsub("[^0-9]", "", parts[2]))
      return(base + added / 10)  # Encode stoppage as decimal
    }

    as.numeric(gsub("[^0-9]", "", t))
  }, USE.NAMES = FALSE)
}


#' Process shooting data
#'
#' Cleans shot-level data with xG values.
#'
#' @param shooting Data frame of shot data
#' @param results Processed match results for match IDs
#'
#' @return Cleaned data frame with shot data
#' @export
process_shooting_data <- function(shooting, results) {
  if (is.null(shooting) || nrow(shooting) == 0) {
    warning("No shooting data to process")
    return(NULL)
  }

  # Require data.table for speed with large datasets
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required for processing large datasets. Install with: install.packages('data.table')")
  }

  progress_msg(sprintf("  [data.table] Processing %d shot rows...", nrow(shooting)))

  dt_shooting <- data.table::as.data.table(shooting)
  dt_results <- data.table::as.data.table(results)
  dt_results <- unique(dt_results[, c("match_url", "match_id"), with = FALSE])

  # Merge
  dt_shooting <- merge(dt_shooting, dt_results, by = "match_url", all.x = TRUE)

  # Process columns (vectorized) - use set() for speed
  data.table::set(dt_shooting, j = "player_name", value = standardize_player_names(dt_shooting$player))
  data.table::set(dt_shooting, j = "team", value = standardize_team_names(dt_shooting$squad))

  # Parse minute - handles stoppage time like "90+4"
  parsed_minutes <- parse_shot_minute(dt_shooting$minute)
  # Combine base minute + added time for effective minute
  data.table::set(dt_shooting, j = "minute", value = parsed_minutes$minute)
  data.table::set(dt_shooting, j = "added_time", value = parsed_minutes$added_time)

  data.table::set(dt_shooting, j = "xg", value = as.numeric(dt_shooting$x_g))
  data.table::set(dt_shooting, j = "is_goal", value = (dt_shooting$outcome == "Goal"))
  data.table::set(dt_shooting, j = "is_penalty", value = grepl("Penalty", dt_shooting$notes, ignore.case = TRUE))

  # Select columns
  result <- data.frame(
    match_id = dt_shooting$match_id,
    team = dt_shooting$team,
    player_name = dt_shooting$player_name,
    minute = dt_shooting$minute,
    added_time = dt_shooting$added_time,
    xg = dt_shooting$xg,
    is_goal = dt_shooting$is_goal,
    is_penalty = dt_shooting$is_penalty,
    body_part = dt_shooting$body_part,
    shot_type = dt_shooting$notes,
    stringsAsFactors = FALSE
  )

  result
}


#' Process advanced match stats
#'
#' Cleans advanced player stats data.
#' Uses data.table for speed with large datasets.
#'
#' @param stats Data frame of advanced match stats
#' @param results Processed match results for match IDs
#' @param stat_type The type of stats being processed
#'
#' @return Cleaned data frame with advanced stats
#' @export
process_advanced_stats <- function(stats, results, stat_type = "summary") {
  if (is.null(stats) || nrow(stats) == 0) {
    warning(paste("No", stat_type, "data to process"))
    return(NULL)
  }

  # Require data.table for speed with large datasets
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("data.table package required for processing large datasets. Install with: install.packages('data.table')")
  }

  # Row count shown for debugging if needed
  # progress_msg(sprintf("  [data.table] Processing %s: %d rows...", stat_type, nrow(stats)))

  # Convert to data.table

  dt_stats <- data.table::as.data.table(stats)
  dt_results <- data.table::as.data.table(results)
  dt_results <- unique(dt_results[, c("match_url", "match_id"), with = FALSE])

  # Merge to get match_id
  dt_stats <- merge(dt_stats, dt_results, by = "match_url", all.x = TRUE)

  # Handle column name variations
  # player: could be 'player' or 'player_name'
  if (!"player_name" %in% names(dt_stats) && "player" %in% names(dt_stats)) {
    data.table::setnames(dt_stats, "player", "player_name")
  }

  # team: could be 'team' or 'squad'
  if (!"team" %in% names(dt_stats) && "squad" %in% names(dt_stats)) {
    data.table::setnames(dt_stats, "squad", "team")
  }

  # Handle is_home: could be boolean 'is_home' or string 'home_away'
  if ("is_home" %in% names(dt_stats)) {
    data.table::set(dt_stats, j = "is_home", value = as.logical(dt_stats$is_home))
  } else if ("home_away" %in% names(dt_stats)) {
    data.table::set(dt_stats, j = "is_home", value = (dt_stats$home_away == "Home"))
  } else {
    data.table::set(dt_stats, j = "is_home", value = NA)
  }

  # Standardize names (vectorized)
  data.table::set(dt_stats, j = "player_name", value = standardize_player_names(dt_stats$player_name))
  data.table::set(dt_stats, j = "team", value = standardize_team_names(dt_stats$team))

  # Select relevant columns based on stat_type
  base_cols <- c("match_id", "team", "is_home", "player_name")
  stat_cols <- get_stat_columns(stat_type)
  keep_cols <- intersect(c(base_cols, stat_cols), names(dt_stats))

  dt_stats <- dt_stats[, keep_cols, with = FALSE]

  # Convert stat columns to numeric (they come in as character)
  numeric_cols <- intersect(stat_cols, names(dt_stats))
  for (col in numeric_cols) {
    data.table::set(dt_stats, j = col, value = as.numeric(dt_stats[[col]]))
  }

  # Return as data.frame for consistency
  as.data.frame(dt_stats)
}


#' Get column names for stat type
#'
#' Returns expected column names from pannadata/FBref data.
#'
#' @param stat_type Type of statistics
#'
#' @return Character vector of column names
#' @keywords internal
get_stat_columns <- function(stat_type) {
 # Column names from pannadata (FBref data with janitor::clean_names())
 switch(stat_type,
   "summary" = c(
     # Basic stats
     "min", "gls", "ast", "pk", "p_katt", "sh", "so_t", "crd_y", "crd_r",
     "touches", "tkl", "int", "blocks",
     # Expected stats
     "x_g", "npx_g", "x_ag",
     # Shot-creating actions
     "sca", "gca",
     # Passing
     "cmp", "att", "cmp_percent", "prg_p",
     # Carries
     "carries", "prg_c",
     # Take-ons (att_2 is take-on attempts, succ is successful)
     "att_2", "succ",
     # Position
     "pos"
   ),
   "passing" = c(
     # Totals
     "cmp", "att", "cmp_percent", "tot_dist", "prg_dist",
     # Short passes
     "cmp_2", "att_2", "cmp_percent_2",
     # Medium passes
     "cmp_3", "att_3", "cmp_percent_3",
     # Long passes
     "cmp_4", "att_4", "cmp_percent_4",
     # Assists and key passes
     "ast", "x_ag", "x_a", "kp", "x1_3", "ppa", "crs_pa", "prg_p"
   ),
   "defense" = c(
     # Tackles
     "tkl", "tkl_w", "def_3rd", "mid_3rd", "att_3rd",
     # Challenges (tkl_2 is dribblers tackled, att is challenges)
     "tkl_2", "att", "tkl_percent", "lost",
     # Blocks
     "blocks", "sh", "pass",
     # Other
     "int", "tkl_int", "clr", "err"
   ),
   "possession" = c(
     # Touches
     "touches", "def_pen", "def_3rd", "mid_3rd", "att_3rd", "att_pen", "live",
     # Take-ons
     "att", "succ", "succ_percent", "tkld", "tkld_percent",
     # Carries
     "carries", "tot_dist", "prg_dist", "prg_c", "x1_3", "cpa", "mis", "dis",
     # Receiving
     "rec", "prg_r"
   ),
   character(0)
 )
}


#' Calculate non-penalty xG and goals
#'
#' Calculates non-penalty expected goals from shot data.
#'
#' @param shooting Processed shooting data
#'
#' @return Data frame with npxG by player and match
#' @export
calculate_npxg <- function(shooting) {
  shooting %>%
    dplyr::filter(!.data$is_penalty) %>%
    dplyr::group_by(.data$match_id, .data$team, .data$player_name) %>%
    dplyr::summarise(
      npxg = sum(.data$xg, na.rm = TRUE),
      npg = sum(.data$is_goal, na.rm = TRUE),
      np_shots = dplyr::n(),
      .groups = "drop"
    )
}


#' Merge all processed data
#'
#' Combines all processed data sources into unified datasets.
#'
#' @param processed_data List of processed data frames
#'
#' @return List with merged data frames
#' @export
merge_processed_data <- function(processed_data) {
  list(
    matches = processed_data$results,
    lineups = processed_data$lineups,
    events = processed_data$events,
    shooting = processed_data$shooting,
    player_stats = processed_data$stats_summary
  )
}


#' Process all raw data
#'
#' Master function to process all collected data.
#' Supports per-component caching to avoid reprocessing on crashes.
#'
#' @param raw_data List of raw data from pannadata
#' @param show_progress Logical, whether to show progress bar (default TRUE)
#' @param cache_dir Optional directory for per-component caching. If provided,
#'   each component is cached separately and only reprocessed if raw data changed.
#' @param raw_data_mtime Optional modification time of raw data file for cache invalidation.
#'   If not provided and cache_dir is set, all components will be reprocessed.
#'
#' @return List of processed data frames
#' @export
process_all_data <- function(raw_data, show_progress = TRUE, cache_dir = NULL, raw_data_mtime = NULL) {
  # Define processing steps and their cache keys
  components <- c(
    "results", "lineups", "events", "shooting",
    "stats_summary", "stats_passing", "stats_defense", "stats_possession",
    "stats_misc", "stats_passing_types", "stats_keeper"
  )
  step_names <- c(
    "Match results", "Lineups", "Events", "Shooting",
    "Stats: summary", "Stats: passing", "Stats: defense", "Stats: possession",
    "Stats: misc", "Stats: passing_types", "Stats: keeper"
  )
  n_steps <- length(components)

  # Helper to check if cache is valid for a component
  cache_valid <- function(component) {
    if (is.null(cache_dir)) return(FALSE)
    cache_file <- file.path(cache_dir, paste0("02_", component, ".rds"))
    if (!file.exists(cache_file)) return(FALSE)
    if (is.null(raw_data_mtime)) return(FALSE)
    file.mtime(cache_file) > raw_data_mtime
  }

  # Helper to load from cache
  load_cache <- function(component) {
    cache_file <- file.path(cache_dir, paste0("02_", component, ".rds"))
    readRDS(cache_file)
  }

  # Helper to save to cache
  save_cache <- function(component, data) {
    if (!is.null(cache_dir)) {
      cache_file <- file.path(cache_dir, paste0("02_", component, ".rds"))
      saveRDS(data, cache_file)
    }
  }

  # Progress tracking (simple message-based to avoid cli environment issues)
  update_progress <- function(step_num, step_name, cached = FALSE) {
    status <- if (cached) " (cached)" else ""
    if (show_progress) {
      message(sprintf("[%d/%d] %s%s", step_num, n_steps, step_name, status))
    }
  }

  # Step 1: Results (required for all other steps)
  if (cache_valid("results")) {
    update_progress(1, "Match results", cached = TRUE)
    results <- load_cache("results")
  } else {
    update_progress(1, "Match results")
    results <- process_match_results(raw_data$results)
    save_cache("results", results)
  }

  # Step 2: Lineups
  if (cache_valid("lineups")) {
    update_progress(2, "Lineups", cached = TRUE)
    lineups <- load_cache("lineups")
  } else {
    update_progress(2, "Lineups")
    lineups <- process_match_lineups(raw_data$lineups, results)
    save_cache("lineups", lineups)
  }

  # Step 3: Events
  if (cache_valid("events")) {
    update_progress(3, "Events", cached = TRUE)
    events <- load_cache("events")
  } else {
    update_progress(3, "Events")
    events <- process_match_events(raw_data$events, results)
    save_cache("events", events)
  }

  # Step 4: Shooting
  if (cache_valid("shooting")) {
    update_progress(4, "Shooting", cached = TRUE)
    shooting <- load_cache("shooting")
  } else {
    update_progress(4, "Shooting")
    shooting <- process_shooting_data(raw_data$shooting, results)
    save_cache("shooting", shooting)
  }

  # Steps 5-11: Advanced stats
  process_or_cache_stats <- function(step_num, component, stat_type, step_name) {
    if (cache_valid(component)) {
      update_progress(step_num, step_name, cached = TRUE)
      load_cache(component)
    } else {
      update_progress(step_num, step_name)
      data <- process_advanced_stats(raw_data[[component]], results, stat_type)
      save_cache(component, data)
      data
    }
  }

  stats_summary <- process_or_cache_stats(5, "stats_summary", "summary", "Stats: summary")
  stats_passing <- process_or_cache_stats(6, "stats_passing", "passing", "Stats: passing")
  stats_defense <- process_or_cache_stats(7, "stats_defense", "defense", "Stats: defense")
  stats_possession <- process_or_cache_stats(8, "stats_possession", "possession", "Stats: possession")
  stats_misc <- process_or_cache_stats(9, "stats_misc", "misc", "Stats: misc")
  stats_passing_types <- process_or_cache_stats(10, "stats_passing_types", "passing_types", "Stats: passing_types")
  stats_keeper <- process_or_cache_stats(11, "stats_keeper", "keeper", "Stats: keeper")

  if (show_progress) {
    message("Data processing complete!")
  }

  list(
    results = results,
    lineups = lineups,
    events = events,
    shooting = shooting,
    stats_summary = stats_summary,
    stats_passing = stats_passing,
    stats_defense = stats_defense,
    stats_possession = stats_possession,
    stats_misc = stats_misc,
    stats_passing_types = stats_passing_types,
    stats_keeper = stats_keeper
  )
}


#' Detect and filter bad xG data from splints
#'
#' Identifies league-seasons with high rates of missing/zero xG data
#' and filters them out. Reports what was filtered.
#'
#' @param splint_data Splint data list from create_all_splints
#' @param zero_xg_threshold Percentage threshold for flagging bad data (default 20%)
#' @param verbose Whether to print filtering report
#'
#' @return List with filtered splint_data and filtering report
#' @export
filter_bad_xg_data <- function(splint_data, zero_xg_threshold = 20, verbose = TRUE) {
  splints <- splint_data$splints

  # Check if we have league/season columns
 has_league <- "league" %in% names(splints)
  has_season <- "season_end_year" %in% names(splints)

  if (!has_league && !has_season) {
    if (verbose) progress_msg("No league/season columns - skipping xG quality filter")
    return(list(
      splint_data = splint_data,
      report = NULL,
      filtered_groups = NULL
    ))
  }

  # Calculate zero xG percentage by league-season (or just season if no league)
  group_cols <- c()
  if (has_league) group_cols <- c(group_cols, "league")
  if (has_season) group_cols <- c(group_cols, "season_end_year")

  xg_quality <- splints %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n_splints = dplyr::n(),
      n_matches = dplyr::n_distinct(.data$match_id),
      zero_xg_count = sum(.data$npxg_home == 0 & .data$npxg_away == 0, na.rm = TRUE),
      zero_xg_pct = .data$zero_xg_count / .data$n_splints * 100,
      avg_npxg = mean(.data$npxg_home + .data$npxg_away, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      is_bad = .data$zero_xg_pct >= zero_xg_threshold
    )

  # Identify bad groups
  bad_groups <- xg_quality %>%
    dplyr::filter(.data$is_bad)

  good_groups <- xg_quality %>%
    dplyr::filter(!.data$is_bad)

  if (verbose) {
    cat("\n=== xG Data Quality Check ===\n")
    cat(sprintf("Threshold: %.0f%% zero xG splints\n\n", zero_xg_threshold))

    if (nrow(bad_groups) > 0) {
      cat("FILTERED OUT (bad data):\n")
      for (i in seq_len(nrow(bad_groups))) {
        row <- bad_groups[i, ]
        label <- if (has_league && has_season) {
          sprintf("  %s %d", row$league, row$season_end_year)
        } else if (has_league) {
          sprintf("  %s", row$league)
        } else {
          sprintf("  Season %d", row$season_end_year)
        }
        cat(sprintf("%s: %.1f%% zero xG (%d splints, %d matches)\n",
                    label, row$zero_xg_pct, row$n_splints, row$n_matches))
      }
      cat("\n")
    }

    cat("KEPT (good data):\n")
    for (i in seq_len(nrow(good_groups))) {
      row <- good_groups[i, ]
      label <- if (has_league && has_season) {
        sprintf("  %s %d", row$league, row$season_end_year)
      } else if (has_league) {
        sprintf("  %s", row$league)
      } else {
        sprintf("  Season %d", row$season_end_year)
      }
      cat(sprintf("%s: %.1f%% zero xG (%d splints, avg xG=%.2f)\n",
                  label, row$zero_xg_pct, row$n_splints, row$avg_npxg))
    }
  }

  # Filter splints
  if (nrow(bad_groups) > 0) {
    # Build filter condition dynamically
    if (has_league && has_season) {
      bad_keys <- paste(bad_groups$league, bad_groups$season_end_year, sep = "_")
      splint_keys <- paste(splints$league, splints$season_end_year, sep = "_")
      keep_mask <- !(splint_keys %in% bad_keys)
    } else if (has_league) {
      keep_mask <- !(splints$league %in% bad_groups$league)
    } else {
      keep_mask <- !(splints$season_end_year %in% bad_groups$season_end_year)
    }

    valid_splints <- splints[keep_mask, ]
    valid_splint_ids <- valid_splints$splint_id

    splint_data$splints <- valid_splints
    splint_data$players <- splint_data$players %>%
      dplyr::filter(.data$splint_id %in% valid_splint_ids)
    splint_data$match_info <- splint_data$match_info %>%
      dplyr::filter(.data$match_id %in% unique(valid_splints$match_id))

    if (verbose) {
      cat(sprintf("\nFiltered: %d -> %d splints (removed %d)\n",
                  nrow(splints), nrow(valid_splints),
                  nrow(splints) - nrow(valid_splints)))
    }
  } else {
    if (verbose) cat("\nNo bad data detected - keeping all splints\n")
  }

  list(
    splint_data = splint_data,
    report = xg_quality,
    filtered_groups = bad_groups
  )
}
