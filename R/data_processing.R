# Data processing and cleaning functions for panna package

#' Process and clean match results
#'
#' Cleans match results data and adds derived columns.
#'
#' @param results Data frame from scrape_pl_match_results
#'
#' @return Cleaned data frame with standardized columns
#' @export
process_match_results <- function(results) {
  # Column names are already snake_case from clean_column_names()
  # Preserve league columns if present (for multi-league data)
  has_league <- "league" %in% names(results)
  has_country <- "country" %in% names(results)
  has_season_year <- "season_end_year" %in% names(results)

  results <- results %>%
    dplyr::mutate(
      home_team = standardize_team_names(.data$home),
      away_team = standardize_team_names(.data$away),
      home_goals = as.numeric(.data$home_goals),
      away_goals = as.numeric(.data$away_goals),
      home_xg = as.numeric(.data$home_x_g),
      away_xg = as.numeric(.data$away_x_g),
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
#' @param lineups Data frame from scrape_pl_match_lineups
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

  # Process columns (vectorized) - use set() for speed
  data.table::set(dt_lineups, j = "player_name", value = standardize_player_names(dt_lineups$player_name))
  data.table::set(dt_lineups, j = "team", value = standardize_team_names(dt_lineups$team))
  data.table::set(dt_lineups, j = "minutes", value = as.numeric(dt_lineups$minutes))

  # Select and rename columns
  result <- data.frame(
    match_id = dt_lineups$match_id,
    team = dt_lineups$team,
    is_home = dt_lineups$is_home,
    player_name = dt_lineups$player_name,
    player_id = dt_lineups$player_href,
    is_starter = dt_lineups$is_starter,
    position = dt_lineups$pos,
    minutes = dt_lineups$minutes,
    stringsAsFactors = FALSE
  )

  result
}


#' Process match events
#'
#' Cleans event data (goals, substitutions) with timing.
#'
#' @param events Data frame from scrape_pl_match_summary
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
#' @param shooting Data frame from scrape_pl_match_shooting
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
  data.table::set(dt_shooting, j = "minute", value = as.numeric(dt_shooting$minute))
  data.table::set(dt_shooting, j = "xg", value = as.numeric(dt_shooting$x_g))
  data.table::set(dt_shooting, j = "is_goal", value = (dt_shooting$outcome == "Goal"))
  data.table::set(dt_shooting, j = "is_penalty", value = grepl("Penalty", dt_shooting$notes, ignore.case = TRUE))

  # Select columns
  result <- data.frame(
    match_id = dt_shooting$match_id,
    team = dt_shooting$team,
    player_name = dt_shooting$player_name,
    minute = dt_shooting$minute,
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
#'
#' @param stats Data frame from scrape_pl_advanced_match_stats
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

  # Create match_id lookup
  match_lookup <- results %>%
    dplyr::select(match_url, match_id) %>%
    dplyr::distinct()

  # Column names are already snake_case from clean_column_names()
  stats <- stats %>%
    dplyr::left_join(match_lookup, by = "match_url") %>%
    dplyr::mutate(
      player_name = standardize_player_names(.data$player),
      team = standardize_team_names(.data$team),
      is_home = .data$home_away == "Home"
    )

  # Select relevant columns based on stat_type
  base_cols <- c("match_id", "team", "is_home", "player_name")
  stat_cols <- get_stat_columns(stat_type)

  stats <- stats %>%
    dplyr::select(dplyr::any_of(c(base_cols, stat_cols)))

  # Convert stat columns to numeric (they come in as character)
  numeric_cols <- intersect(stat_cols, names(stats))
  for (col in numeric_cols) {
    stats[[col]] <- as.numeric(stats[[col]])
  }

  stats
}


#' Get column names for stat type
#'
#' Returns expected column names from worldfootballR data.
#' Note: worldfootballR uses suffixes like _expected, _tackles, _touches, etc.
#'
#' @param stat_type Type of statistics
#'
#' @return Character vector of column names
#' @keywords internal
get_stat_columns <- function(stat_type) {
 # Column names match worldfootballR output (after janitor::clean_names())
 # Many columns have category suffixes like _expected, _tackles, _touches, _carries
 switch(stat_type,
   "summary" = c(
     # Basic stats
     "min", "gls", "ast", "pk", "p_katt", "sh", "so_t", "crd_y", "crd_r",
     "touches", "tkl", "int", "blocks",
     # Expected stats (have _expected suffix)
     "x_g_expected", "npx_g_expected", "x_ag_expected",
     # Shot-creating actions (have _sca suffix)
     "sca_sca", "gca_sca",
     # Passing totals (have _passes suffix)
     "cmp_passes", "att_passes", "cmp_percent_passes", "prg_p_passes",
     # Carries (have _carries suffix)
     "carries_carries", "prg_c_carries",
     # Take-ons (have _take_ons suffix)
     "att_take_ons", "succ_take_ons"
   ),
   "passing" = c(
     # Totals (have _total suffix)
     "cmp_total", "att_total", "cmp_percent_total", "tot_dist_total", "prg_dist_total",
     # By distance
     "cmp_short", "att_short", "cmp_percent_short",
     "cmp_medium", "att_medium", "cmp_percent_medium",
     "cmp_long", "att_long", "cmp_percent_long",
     # Assists and key passes
     "ast", "x_ag", "x_a", "kp", "final_third", "ppa", "crs_pa", "prg_p"
   ),
   "defense" = c(
     # Tackles (have _tackles suffix)
     "tkl_tackles", "tkl_w_tackles", "def_3rd_tackles", "mid_3rd_tackles", "att_3rd_tackles",
     # Challenges (have _challenges suffix)
     "tkl_challenges", "att_challenges", "tkl_percent_challenges", "lost_challenges",
     # Blocks (have _blocks suffix)
     "blocks_blocks", "sh_blocks", "pass_blocks",
     # Other
     "int", "tkl_int", "clr", "err"
   ),
   "possession" = c(
     # Touches (have _touches suffix)
     "touches_touches", "def_pen_touches", "def_3rd_touches", "mid_3rd_touches",
     "att_3rd_touches", "att_pen_touches", "live_touches",
     # Take-ons (have _take_ons suffix)
     "att_take_ons", "succ_take_ons", "succ_percent_take_ons",
     "tkld_take_ons", "tkld_percent_take_ons",
     # Carries (have _carries suffix)
     "carries_carries", "tot_dist_carries", "prg_dist_carries", "prg_c_carries",
     "final_third_carries", "cpa_carries", "mis_carries", "dis_carries",
     # Receiving (have _receiving suffix)
     "rec_receiving", "prg_r_receiving"
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
#'
#' @param raw_data List of raw data from scrape_pl_comprehensive
#'
#' @return List of processed data frames
#' @export
process_all_data <- function(raw_data) {
  progress_msg("Processing match results...")
  results <- process_match_results(raw_data$results)

  progress_msg("Processing lineups...")
  lineups <- process_match_lineups(raw_data$lineups, results)

  progress_msg("Processing events...")
  events <- process_match_events(raw_data$events, results)

  progress_msg("Processing shooting data...")
  shooting <- process_shooting_data(raw_data$shooting, results)

  progress_msg("Processing advanced stats...")
  stats_summary <- process_advanced_stats(raw_data$stats_summary, results, "summary")
  stats_passing <- process_advanced_stats(raw_data$stats_passing, results, "passing")
  stats_defense <- process_advanced_stats(raw_data$stats_defense, results, "defense")
  stats_possession <- process_advanced_stats(raw_data$stats_possession, results, "possession")

  list(
    results = results,
    lineups = lineups,
    events = events,
    shooting = shooting,
    stats_summary = stats_summary,
    stats_passing = stats_passing,
    stats_defense = stats_defense,
    stats_possession = stats_possession
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
