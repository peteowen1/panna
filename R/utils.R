# Utility functions for panna package


#' Check that a suggested package is installed
#'
#' Throws an informative error if a package listed in Suggests is missing.
#'
#' @param pkg Package name (character)
#' @param reason Brief reason why the package is needed
#'
#' @return TRUE invisibly if package is available
#' @keywords internal
.check_suggests <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("Package '%s' is required but not installed.", pkg)
    if (!is.null(reason)) {
      msg <- paste(msg, reason)
    }
    cli::cli_abort(c(
      msg,
      "i" = 'Install it with: install.packages("{pkg}")'
    ))
  }
  invisible(TRUE)
}


#' Clean column names to snake_case
#'
#' Applies janitor::clean_names() to standardize all column names.
#'
#' @param data Data frame
#'
#' @return Data frame with snake_case column names
#' @export
clean_column_names <- function(data) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }
  .check_suggests("janitor", "clean_column_names() requires janitor.")
  janitor::clean_names(data)
}


#' Safe division handling division by zero
#'
#' @param x Numerator
#' @param y Denominator
#' @param default Value to return when denominator is zero (default: 0)
#'
#' @return x / y, or default if y is zero
#' @export
#'
#' @examples
#' safe_divide(10, 2)
#' safe_divide(10, 0)
#' safe_divide(10, 0, default = NA)
safe_divide <- function(x, y, default = 0) {
  # Preserve genuine NAs from inputs; only replace Inf/NaN from division-by-zero
  input_na <- is.na(x) | is.na(y)
  result <- x / y
  bad <- (is.infinite(result) | is.nan(result)) & !input_na
  result[bad] <- default
  result
}


#' Validate season input
#'
#' Checks that seasons are in valid "YYYY-YYYY" format and within the
#' available data range. The minimum year depends on the data source:
#' FBref starts at 2017, Opta at 2013.
#'
#' @param seasons Character vector of seasons in format "YYYY-YYYY" (e.g., "2023-2024")
#' @param min_year Minimum start year allowed (default 2017 for FBref)
#' @param source_name Name of the data source for error messages (default "FBref")
#'
#' @return TRUE if valid, otherwise throws an error
#' @export
#'
#' @examples
#' validate_seasons("2023-2024")
#' validate_seasons(c("2022-2023", "2023-2024"))
#' validate_seasons("2013-2014", min_year = 2013, source_name = "Opta")
validate_seasons <- function(seasons, min_year = 2017, source_name = "FBref") {
  # Check format
  if (!all(grepl("^\\d{4}-\\d{4}$", seasons))) {
    bad_seasons <- seasons[!grepl("^\\d{4}-\\d{4}$", seasons)]
    cli::cli_abort(c(
      "Seasons must be in format 'YYYY-YYYY'.",
      "x" = "Invalid: {.val {head(bad_seasons, 3)}}",
      "i" = "Example: {.val 2023-2024}"
    ))
  }

  # Extract start years
  start_years <- as.numeric(substr(seasons, 1, 4))

  # Check minimum year
  if (any(start_years < min_year)) {
    bad_seasons <- seasons[start_years < min_year]
    min_season <- paste0(min_year, "-", min_year + 1L)
    cli::cli_abort(c(
      "{source_name} data is only available from {min_season} season onwards.",
      "x" = "Invalid: {.val {head(bad_seasons, 3)}}"
    ))
  }

  # Check end year is start + 1
  end_years <- as.numeric(substr(seasons, 6, 9))
  if (!all(end_years == start_years + 1)) {
    bad_seasons <- seasons[end_years != start_years + 1]
    cli::cli_abort(c(
      "Season end year must be start year + 1.",
      "x" = "Invalid: {.val {head(bad_seasons, 3)}}",
      "i" = "Example: {.val 2023-2024}"
    ))
  }

  TRUE
}


#' Validate min_minutes parameter
#'
#' Checks that min_minutes is a single non-negative number.
#'
#' @param min_minutes Value to validate
#' @keywords internal
validate_min_minutes <- function(min_minutes) {
  if (!is.numeric(min_minutes) || length(min_minutes) != 1 ||
      is.na(min_minutes) || min_minutes < 0) {
    cli::cli_abort(c(
      "{.arg min_minutes} must be a single non-negative number.",
      "x" = "Got {.val {min_minutes}}."
    ))
  }
  invisible(TRUE)
}


#' Standardize player names
#'
#' Cleans and standardizes player names for consistent matching across datasets.
#' Uses memoization to cache unique names for O(1) lookup on repeated values.
#'
#' @param names Character vector of player names
#'
#' @return Character vector of standardized names
#' @keywords internal
standardize_player_names <- function(names) {
  # Memoization: process unique names once, then lookup
  unique_names <- unique(names)

  # Fast vectorized implementation (avoids slow tools::toTitleCase)
  cleaned <- trimws(unique_names)
  # Remove extra whitespace first
  cleaned <- gsub("\\s+", " ", cleaned)
  # Remove common suffixes
  cleaned <- gsub("\\s+(Jr\\.|Sr\\.|II|III|IV)$", "", cleaned)
  # Fast title case: lowercase then capitalize first letter of each word
  cleaned <- tolower(cleaned)
  # Use gsub with perl regex for fast word boundary capitalization
  cleaned <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", cleaned, perl = TRUE)

  # Build lookup and return via O(1) match
  lookup <- setNames(cleaned, unique_names)
  unname(lookup[names])
}


#' Clean player name for matching
#'
#' Creates a minimal normalized version of player name for fuzzy matching.
#' Unlike standardize_player_names() which preserves readable format,
#' this creates a key: lowercase with all whitespace removed.
#' Uses memoization to cache unique names for O(1) lookup on repeated values.
#'
#' @param names Character vector of player names
#'
#' @return Character vector of cleaned names (lowercase, no whitespace)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' clean_player_name(c("Kylian Mbappé", "kylian mbappé", "KYLIAN MBAPPÉ"))
#' # All return "kylianmbappé"
#' }
clean_player_name <- function(names) {
  # Memoization: process unique names once, then lookup
  unique_names <- unique(names)

  # First replace non-breaking spaces (U+00A0) with regular spaces
  # These come from HTML scraping and trimws() doesn't handle them
  cleaned <- gsub("\u00A0", " ", unique_names)
  # Trim leading/trailing whitespace
  cleaned <- trimws(cleaned)
  # Convert to lowercase
  cleaned <- tolower(cleaned)
  # Remove all internal whitespace (spaces, tabs, etc.)
  cleaned <- gsub("\\s+", "", cleaned)

  # Build lookup and return via O(1) match
  lookup <- setNames(cleaned, unique_names)
  unname(lookup[names])
}


#' Extract FBref player ID from href
#'
#' Extracts the 8-character hex ID from an FBref player URL.
#' Example: "/players/d080ed5e/Kylian-Mbappe" -> "d080ed5e"
#'
#' Note: player_href is extracted during FBref scraping and used in
#' data processing to derive player IDs.
#'
#' @param hrefs Character vector of FBref player hrefs
#'
#' @return Character vector of 8-char hex IDs (NA if not found)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' extract_fbref_player_id("/players/d080ed5e/Kylian-Mbappe")
#' # Returns "d080ed5e"
#' }
extract_fbref_player_id <- function(hrefs) {
  # Extract 8-char hex ID from /players/xxxxxxxx/... pattern
  ids <- gsub(".*/players/([a-f0-9]{8})/.*", "\\1", hrefs)
  # Return NA for hrefs that don't match the pattern
  ids[!grepl("^[a-f0-9]{8}$", ids)] <- NA_character_
  ids
}


#' Standardize team names
#'
#' Cleans and standardizes team names for consistent matching.
#' Uses vectorized lookup for speed on large datasets.
#'
#' @param names Character vector of team names
#'
#' @return Character vector of standardized names
#' @keywords internal
standardize_team_names <- function(names) {
 names <- trimws(names)

 # Common team name standardizations (lookup table)
 # Keys are variants, values are canonical names
 lookup_from <- c(
   "Manchester Utd", "Man Utd", "Man United",
   "Man City",
   "Tottenham", "Spurs",
   "Wolves", "Wolverhampton",
   "Brighton", "Brighton and Hove Albion",
   "West Ham",
   "Newcastle",
   "Nott'ham Forest", "Nottingham",
   "Sheffield Utd",
   "Leeds",
   "Leicester"
 )
 lookup_to <- c(
   "Manchester United", "Manchester United", "Manchester United",
   "Manchester City",
   "Tottenham Hotspur", "Tottenham Hotspur",
   "Wolverhampton Wanderers", "Wolverhampton Wanderers",
   "Brighton & Hove Albion", "Brighton & Hove Albion",
   "West Ham United",
   "Newcastle United",
   "Nottingham Forest", "Nottingham Forest",
   "Sheffield United",
   "Leeds United",
   "Leicester City"
 )

 # Vectorized lookup using match() - O(n) instead of O(n*m)
 idx <- match(names, lookup_from)
 needs_replace <- !is.na(idx)
 names[needs_replace] <- lookup_to[idx[needs_replace]]

 names
}


#' Create unique match ID
#'
#' Generates a unique identifier for a match based on season, date, and teams.
#'
#' @param season Season string (e.g., "2023-2024")
#' @param date Date of match
#' @param home_team Home team name
#' @param away_team Away team name
#'
#' @return Character vector of match IDs
#' @keywords internal
create_match_id <- function(season, date, home_team, away_team) {
  paste(season, format(as.Date(date), "%Y%m%d"), home_team, away_team, sep = "_")
}


#' Create unique player ID
#'
#' Generates a unique identifier for a player based on name and team.
#' Note: This is a simple implementation; FBref player URLs are more reliable.
#'
#' @param player_name Player name
#' @param fbref_id Optional FBref player ID (preferred if available
#'
#' @return Character vector of player IDs
#' @keywords internal
create_player_id <- function(player_name, fbref_id = NULL) {
  # Fallback: clean name
  id <- tolower(player_name)
  id <- gsub("[^a-z0-9]", "_", id)
  id <- gsub("_+", "_", id)
  id <- gsub("^_|_$", "", id)

  # Use fbref_id where available (element-wise)
  if (!is.null(fbref_id)) {
    has_id <- !is.na(fbref_id) & nzchar(fbref_id)
    id[has_id] <- fbref_id[has_id]
  }
  id
}


#' Validate data frame input
#'
#' Validates that input is a non-empty data frame with required columns.
#' Throws informative errors using cli package when validation fails.
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param arg_name Name of the argument (for error messages)
#' @param min_rows Minimum number of rows required (default 1)
#'
#' @return TRUE invisibly if valid, otherwise throws an error
#' @export
#'
#' @examples
#' df <- data.frame(player_name = "Messi", minutes = 90)
#' validate_dataframe(df, c("player_name", "minutes"))
#' # Returns TRUE
#'
#' \dontrun{
#' # These will throw errors:
#' validate_dataframe(NULL, c("player_name"))
#' validate_dataframe(data.frame(), c("player_name"))
#' validate_dataframe(df, c("nonexistent_col"))
#' }
validate_dataframe <- function(data, required_cols = NULL, arg_name = "data", min_rows = 1) {
  # Check NULL
	if (is.null(data)) {
    cli::cli_abort(c(
      "{.arg {arg_name}} cannot be NULL.",
      "i" = "Please provide a valid data frame."
    ))
  }

  # Check is data frame
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.arg {arg_name}} must be a data frame.",
      "x" = "Got {.cls {class(data)}} instead."
    ))
  }

  # Check minimum rows
  if (nrow(data) < min_rows) {
    cli::cli_abort(c(
      "{.arg {arg_name}} must have at least {min_rows} row{?s}.",
      "x" = "Got {nrow(data)} row{?s}."
    ))
  }

  # Check required columns
  if (!is.null(required_cols) && length(required_cols) > 0) {
    missing <- setdiff(required_cols, names(data))
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "{.arg {arg_name}} is missing required column{?s}.",
        "x" = "Missing: {.field {missing}}",
        "i" = "Available: {.field {head(names(data), 10)}}{if(ncol(data) > 10) '...' else ''}"
      ))
    }
  }

  invisible(TRUE)
}


#' Check and report data completeness
#'
#' Validates data quality and reports missing values.
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param warn Logical, whether to print warnings for missing data
#'
#' @return List with validation results
#' @keywords internal
validate_data_completeness <- function(data, required_cols = NULL, warn = TRUE) {
  result <- list(
    n_rows = nrow(data),
    n_cols = ncol(data),
    missing_cols = character(0),
    na_summary = list()
  )

  # Check required columns
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(data))
    if (length(missing) > 0) {
      result$missing_cols <- missing
      if (warn) {
        cli::cli_warn("Missing required columns: {paste(missing, collapse = ', ')}")
      }
    }
  }

  # Check NA values
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  na_pcts <- if (nrow(data) > 0) round(na_counts / nrow(data) * 100, 1) else rep(0, length(na_counts))
  result$na_summary <- data.frame(
    column = names(na_counts),
    na_count = na_counts,
    na_pct = na_pcts,
    row.names = NULL
  )

  if (warn && any(na_pcts > 20)) {
    high_na <- names(na_pcts)[na_pcts > 20]
    cli::cli_warn("Columns with >20% missing: {paste(high_na, collapse = ', ')}")
  }

  result
}


#' Calculate minutes per 90
#'
#' @param stat Statistic value (counting stat)
#' @param minutes Minutes played
#'
#' @return Statistic per 90 minutes
#' @export
per_90 <- function(stat, minutes) {
  safe_divide(stat * 90, minutes)
}


#' Print progress message
#'
#' @param msg Message to print
#' @param verbose Whether to print
#'
#' @return NULL (invisibly)
#' @keywords internal
progress_msg <- function(msg, verbose = TRUE) {
  if (verbose) {
    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
  }
  invisible(NULL)
}


#' Find first matching column from candidates
#'
#' Returns the first column name that exists in the data frame.
#'
#' @param data Data frame to search
#' @param candidates Character vector of column names to try (in priority order)
#'
#' @return First matching column name, or NULL if none found
#' @keywords internal
find_column <- function(data, candidates) {
  matches <- intersect(candidates, names(data))
  if (length(matches) == 0) NULL else matches[1]
}


#' Count events by splint boundary
#'
#' Core logic for counting home/away events relative to splint boundaries.
#'
#' @param events Data frame with 'minute' (or 'effective_minute') and 'is_home' columns
#' @param boundaries Numeric vector of splint boundary minutes
#' @param type Either "before" (cumulative before each boundary start) or
#'   "within" (events within each splint interval)
#'
#' @return List with 'home' and 'away' counts (vectors of length n_splints)
#' @keywords internal
.count_events <- function(events, boundaries, type = c("before", "within")) {
  type <- match.arg(type)
  n_splints <- length(boundaries) - 1
  home_count <- rep(0L, n_splints)
  away_count <- rep(0L, n_splints)

  if (!is.null(events) && nrow(events) > 0) {
    event_mins <- if ("effective_minute" %in% names(events)) {
      events$effective_minute
    } else {
      events$minute
    }
    is_home <- events$is_home

    if (type == "within") {
      # findInterval: bin each event into its splint (1-based)
      bins <- findInterval(event_mins, boundaries, rightmost.closed = FALSE, left.open = FALSE)
      # Clamp to valid range (events outside boundaries get 0 or n_splints+1)
      valid <- bins >= 1 & bins <= n_splints
      if (any(valid)) {
        home_count <- tabulate(bins[valid & is_home], nbins = n_splints)
        away_count <- tabulate(bins[valid & !is_home], nbins = n_splints)
      }
    } else {
      # "before": count events before each splint's start boundary
      # For splint i, count events with minute < boundaries[i]
      # Use cumulative counts on sorted events
      home_mins <- sort(event_mins[is_home])
      away_mins <- sort(event_mins[!is_home])
      home_count <- findInterval(boundaries[seq_len(n_splints)] - .Machine$double.eps,
                                 home_mins)
      away_count <- findInterval(boundaries[seq_len(n_splints)] - .Machine$double.eps,
                                 away_mins)
    }
  }

  list(home = as.integer(home_count), away = as.integer(away_count))
}

#' Count events before each boundary
#'
#' Counts home/away events occurring before each splint boundary.
#' Used for tracking cumulative goals, red cards, etc.
#'
#' @param events Data frame with 'minute' (or 'effective_minute') and 'is_home' columns
#' @param boundaries Numeric vector of splint boundary minutes
#'
#' @return List with 'home' and 'away' counts (vectors same length as boundaries minus 1)
#' @keywords internal
count_events_before <- function(events, boundaries) {
  .count_events(events, boundaries, type = "before")
}

#' Count events within each splint
#'
#' Counts events that occur IN each splint (between start and end boundaries).
#' Unlike count_events_before which gives cumulative counts, this gives per-splint counts.
#'
#' @param events Data frame with 'minute' (or 'effective_minute') and 'is_home' columns
#' @param boundaries Numeric vector of splint boundary minutes
#'
#' @return List with 'home' and 'away' counts (vectors of length n_splints)
#' @keywords internal
count_events_in_splint <- function(events, boundaries) {
  .count_events(events, boundaries, type = "within")
}


#' Extract season from match_id
#'
#' Extracts the season string from a match_id.
#' Match IDs have format "2017-2018_20170915_TeamA_TeamB".
#'
#' @param match_id Character vector of match IDs
#'
#' @return Character vector of season strings (e.g., "2017-2018")
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' extract_season_from_match_id("2017-2018_20170915_Bournemouth_Brighton")
#' # Returns "2017-2018"
#' }
extract_season_from_match_id <- function(match_id) {
  sub("^([0-9]{4}-[0-9]{4})_.*", "\\1", match_id)
}


#' Extract season end year from match_id
#'
#' Extracts the season end year as a numeric value from a match_id.
#' Match IDs have format "2017-2018_20170915_TeamA_TeamB".
#'
#' @param match_id Character vector of match IDs
#'
#' @return Numeric vector of season end years (e.g., 2018)
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' extract_season_end_year_from_match_id("2017-2018_20170915_Bournemouth_Brighton")
#' # Returns 2018
#' }
extract_season_end_year_from_match_id <- function(match_id) {
  season <- extract_season_from_match_id(match_id)
  as.numeric(substr(season, 6, 9))
}


#' Extract season end year from a season string
#'
#' Handles both "YYYY-YYYY" format (returns second year) and
#' tournament "YYYY Country" format (returns the year).
#'
#' @param season Season string (e.g., "2023-2024" or "2018 Russia")
#' @return Numeric end year, or NA_real_ if unparseable
#' @keywords internal
extract_season_end_year <- function(season) {
  if (grepl("^\\d{4}-\\d{4}$", season)) return(as.numeric(substr(season, 6, 9)))
  year <- as.numeric(sub("^(\\d{4}).*", "\\1", season))
  if (!is.na(year)) return(year)
  NA_real_
}


#' Format a duration in seconds as a human-readable string
#'
#' @param secs Duration in seconds
#' @return Formatted string (e.g., "3.2 seconds", "1.5 minutes", "2.1 hours")
#' @keywords internal
format_duration <- function(secs) {
  if (secs < 60) return(sprintf("%.1f seconds", secs))
  if (secs < 3600) return(sprintf("%.1f minutes", secs / 60))
  sprintf("%.1f hours", secs / 3600)
}


#' Extract column from data frame with zero fallback
#'
#' Returns the column as numeric if it exists, otherwise a vector of zeros.
#' Used internally by player stat aggregation functions.
#'
#' @param df Data frame
#' @param col Column name
#' @return Numeric vector
#' @keywords internal
.get_col <- function(df, col) {
  if (col %in% names(df)) as.numeric(df[[col]]) else rep(0, nrow(df))
}


#' Ensure column exists with default
#'
#' Creates a column if it doesn't exist, optionally deriving from another column.
#'
#' @param data Data frame
#' @param col_name Name of column to ensure
#' @param default Default value or function to derive value
#' @param source_col Optional source column for pattern matching
#' @param pattern Regex pattern to match in source column
#'
#' @return Data frame with column ensured
#' @keywords internal
ensure_column <- function(data, col_name, default = FALSE, source_col = NULL, pattern = NULL) {
  if (col_name %in% names(data)) {
    return(data)
  }

  if (!is.null(source_col) && !is.null(pattern) && source_col %in% names(data)) {
    data[[col_name]] <- grepl(pattern, data[[source_col]], ignore.case = TRUE)
  } else {
    data[[col_name]] <- default
  }

  data
}


#' Rename columns using a mapping
#'
#' Renames columns in a data frame based on a named vector mapping.
#' The mapping format is: c(new_name1 = "old_name1", new_name2 = "old_name2")
#'
#' @param data Data frame
#' @param mapping Named character vector where names are new column names
#'   and values are existing column names to rename
#'
#' @return Data frame with renamed columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' df <- data.frame(a = 1, b = 2)
#' rename_columns(df, c(x = "a", y = "b"))
#' }
rename_columns <- function(data, mapping) {
  old_names <- unname(mapping)
  new_names <- names(mapping)
  # Only rename columns that exist in data
  present <- old_names %in% names(data)
  if (any(present)) {
    idx <- match(old_names[present], names(data))
    names(data)[idx] <- new_names[present]
  }
  data
}


#' Aggregate player statistics with optional team grouping
#'
#' Common aggregation pattern for player stats functions. Aggregates columns
#' by player (and optionally team), adding most frequent team when not grouping by team.
#'
#' @param data Data frame with player-level data
#' @param agg_cols Named list where names are output column names and values are
#'   expressions to aggregate (as strings or column names)
#' @param by_team Logical. If TRUE, group by player and team. If FALSE, group by
#'   player only and add most frequent team.
#' @param player_col Name of player column (default "player")
#' @param team_col Name of team column (default "team")
#'
#' @return Aggregated data frame with player, team, and aggregated columns
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Aggregate goals and assists
#' aggregate_player_data(
#'   data = match_data,
#'   agg_cols = list(matches = "1", goals = "gls", assists = "ast"),
#'   by_team = FALSE
#' )
#' }
aggregate_player_data <- function(data, agg_cols, by_team = FALSE,
                                   player_col = "player", team_col = "team") {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }

  # Build formula for aggregate
  agg_formula <- if (by_team) {
    stats::as.formula(paste(". ~", player_col, "+", team_col))
  } else {
    stats::as.formula(paste(". ~", player_col))
  }

  # Build data frame of columns to aggregate
  agg_data <- data.frame(row.names = seq_len(nrow(data)))
  for (col_name in names(agg_cols)) {
    col_expr <- agg_cols[[col_name]]
    if (col_expr == "1") {
      # Count rows
      agg_data[[col_name]] <- 1
    } else if (col_expr %in% names(data)) {
      agg_data[[col_name]] <- as.numeric(data[[col_expr]])
    } else {
      # Unknown column - warn and fill with zeros
      cli::cli_warn(c(
        "Column {.val {col_expr}} not found in data.",
        "i" = "Pre-compute derived columns before passing to {.fn aggregate_player_data}."
      ))
      agg_data[[col_name]] <- rep(0, nrow(data))
    }
  }

  # Add grouping columns
  agg_data[[player_col]] <- data[[player_col]]
  if (by_team && team_col %in% names(data)) {
    agg_data[[team_col]] <- data[[team_col]]
  }

  # Aggregate
  result <- stats::aggregate(
    agg_formula,
    data = agg_data,
    FUN = function(x) sum(x, na.rm = TRUE),
    na.action = stats::na.pass
  )

  # Add most frequent team if not grouping by team
  if (!by_team && team_col %in% names(data)) {
    team_mode <- stats::aggregate(
      stats::as.formula(paste(team_col, "~", player_col)),
      data = data,
      FUN = function(x) names(which.max(table(x)))
    )
    result <- data.table::as.data.table(team_mode)[data.table::as.data.table(result), on = player_col]
    data.table::setDF(result)
  }

  result
}


#' HTTP GET with exponential backoff retry
#'
#' Wraps httr::GET with automatic retry on transient failures (5xx errors,
#' connection timeouts). Does NOT retry on rate limiting (429) or blocking (403)
#' as those require different handling.
#'
#' @param url URL to fetch
#' @param max_retries Maximum number of retry attempts (default 3)
#' @param base_delay Initial delay in seconds before first retry (default 1)
#' @param max_delay Maximum delay between retries in seconds (default 30)
#' @param ... Additional arguments passed to httr::GET (headers, timeout, handle)
#'
#' @return httr response object, or NULL with attributes on permanent failure
#' @keywords internal
fetch_with_retry <- function(url, max_retries = 3, base_delay = 1, max_delay = 30, ...) {
  .check_suggests("httr", "HTTP requests require httr.")
  attempt <- 0

  while (attempt <= max_retries) {
    # Try the request
    response <- tryCatch(
      httr::GET(url, ...),
      error = function(e) {
        list(error = TRUE, message = conditionMessage(e))
      }
    )

    # Handle connection errors
    if (is.list(response) && isTRUE(response$error)) {
      attempt <- attempt + 1
      if (attempt <= max_retries) {
        delay <- min(base_delay * (2^(attempt - 1)), max_delay)
        cli::cli_alert_warning(
          "Connection error: {response$message}. Retrying in {delay}s ({attempt}/{max_retries})"
        )
        Sys.sleep(delay)
        next
      } else {
        cli::cli_alert_danger("Connection failed after {max_retries} retries")
        return(structure(list(), class = "fetch_error",
                         connection_error = TRUE, error_message = response$message))
      }
    }

    status <- httr::status_code(response)

    # Permanent failures - don't retry
    # Use structure() to create NULL with attributes (attr() on NULL fails)
    if (status == 429) {
      return(structure(list(), class = "fetch_error", rate_limited = TRUE))
    }

    if (status == 403) {
      return(structure(list(), class = "fetch_error", blocked = TRUE))
    }

    if (status == 404) {
      return(structure(list(), class = "fetch_error", not_found = TRUE))
    }

    # Success
    if (status >= 200 && status < 300) {
      return(response)
    }

    # Transient failures (5xx) - retry with backoff
    if (status >= 500 && status < 600) {
      attempt <- attempt + 1
      if (attempt <= max_retries) {
        delay <- min(base_delay * (2^(attempt - 1)), max_delay)
        cli::cli_alert_warning(
          "Server error {status}. Retrying in {delay}s ({attempt}/{max_retries})"
        )
        Sys.sleep(delay)
        next
      }
    }

    # Other errors - don't retry
    return(structure(list(), class = "fetch_error", http_error = TRUE, status_code = status))
  }

  # Should not reach here, but handle anyway
  return(structure(list(), class = "fetch_error", max_retries_exceeded = TRUE))
}


#' Build SQL WHERE clause from filters
#'
#' Constructs a SQL WHERE clause from a named list of filter conditions.
#' Values are properly quoted for SQL safety. NULL values are skipped.
#'
#' @param filters Named list where names are column names and values are
#'   filter values. Values can be character (quoted), numeric (unquoted),
#'   or NULL (skipped).
#' @param prefix Whether to include "WHERE " prefix. If FALSE, returns just
#'   the conditions joined by AND.
#'
#' @return Character string with SQL WHERE clause, or empty string if no filters.
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' build_where_clause(list(league = "ENG", season = "2023-2024"))
#' # Returns: "WHERE league = 'ENG' AND season = '2023-2024'"
#'
#' build_where_clause(list(league = "ENG", min_goals = 5))
#' # Returns: "WHERE league = 'ENG' AND min_goals = 5"
#'
#' build_where_clause(list(league = NULL, season = "2023-2024"))
#' # Returns: "WHERE season = '2023-2024'"
#'
#' build_where_clause(list())
#' # Returns: ""
#' }
build_where_clause <- function(filters, prefix = TRUE) {
  if (is.null(filters) || length(filters) == 0) {
    return("")
  }

  # Remove NULL values
  filters <- Filter(Negate(is.null), filters)

  if (length(filters) == 0) {
    return("")
  }

  # Validate column names to prevent SQL injection
  validate_sql_columns(names(filters))

  # Build conditions
  conditions <- vapply(names(filters), function(col_name) {
    value <- filters[[col_name]]
    if (length(value) > 1) {
      # Multiple values: use IN clause
      if (is.character(value)) {
        escaped <- gsub("'", "''", value)
        vals <- paste0("'", escaped, "'", collapse = ", ")
      } else if (is.numeric(value)) {
        vals <- paste(value, collapse = ", ")
      } else {
        escaped <- gsub("'", "''", as.character(value))
        vals <- paste0("'", escaped, "'", collapse = ", ")
      }
      sprintf("%s IN (%s)", col_name, vals)
    } else if (is.character(value)) {
      escaped <- gsub("'", "''", value)
      sprintf("%s = '%s'", col_name, escaped)
    } else if (is.numeric(value)) {
      sprintf("%s = %s", col_name, value)
    } else {
      escaped <- gsub("'", "''", as.character(value))
      sprintf("%s = '%s'", col_name, escaped)
    }
  }, character(1))

  where_sql <- paste(conditions, collapse = " AND ")

  if (prefix && nzchar(where_sql)) {
    paste("WHERE", where_sql)
  } else {
    where_sql
  }
}


#' Standardize data frame column names using a mapping
#'
#' Renames columns in a data frame or data.table if alternative names exist.
#' This handles common variations in column naming across different data sources.
#' For data.table objects, uses setnames() for efficient in-place renaming.
#'
#' @param data Data frame or data.table to standardize
#' @param col_map Named list where names are canonical column names and
#'   values are character vectors of alternative names to look for.
#'
#' @return Data frame/data.table with standardized column names
#' @keywords internal
standardize_data_columns <- function(data, col_map) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }

  is_dt <- inherits(data, "data.table")
  current_names <- names(data)

  for (canonical_name in names(col_map)) {
    # Skip if canonical name already exists
    if (canonical_name %in% current_names) {
      next
    }

    # Look for alternatives
    alternatives <- col_map[[canonical_name]]
    for (alt_name in alternatives) {
      if (alt_name %in% current_names) {
        # Found an alternative, rename it
        if (is_dt) {
          data.table::setnames(data, alt_name, canonical_name)
        } else {
          names(data)[names(data) == alt_name] <- canonical_name
        }
        current_names <- names(data)  # Update for next iteration
        break
      }
    }
  }

  data
}


#' Default column mapping for FBref/Opta data
#'
#' Standard column name variations encountered across different data sources.
#'
#' @return Named list of canonical column names to alternatives
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' col_map <- default_column_map()
#' names(col_map)
#' }
default_column_map <- function() {
  list(
    team = c("squad", "team_name"),
    player_name = c("player"),
    minutes = c("min", "mins_played", "minsPlayed"),
    position = c("pos"),
    player_id = c("playerId"),
    team_id = c("teamId"),
    match_id = c("matchId")
  )
}
