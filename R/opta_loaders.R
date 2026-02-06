# Opta Data Loader Functions
#
# Functions for loading Opta (TheAnalyst) data from local parquet files.
# Data is scraped from TheAnalyst API and stored in pannadata/data/opta/.
#
# League codes: EPL, La_Liga, Bundesliga, Serie_A, Ligue_1
# Seasons: 2010-2011 to 2025-2026

#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
NULL


#' Validate SQL column names
#'
#' Checks that column names contain only safe characters to prevent SQL injection.
#'
#' @param columns Character vector of column names
#'
#' @return The validated column names (unchanged if valid)
#' @keywords internal
validate_sql_columns <- function(columns) {
  if (is.null(columns)) return(NULL)
  invalid_cols <- columns[!grepl("^[a-zA-Z_][a-zA-Z0-9_]*$", columns)]
  if (length(invalid_cols) > 0) {
    cli::cli_abort(c(
      "Invalid column names detected.",
      "i" = "Column names must contain only letters, numbers, and underscores.",
      "x" = "Invalid: {paste(invalid_cols, collapse = ', ')}"
    ))
  }
  columns
}


# Opta league code mapping
OPTA_LEAGUES <- c(
  ENG = "EPL",
  ESP = "La_Liga",
  GER = "Bundesliga",
  ITA = "Serie_A",
  FRA = "Ligue_1"
)

# Environment to store opta data path
.opta_env <- new.env(parent = emptyenv())

#' Get or Set Opta Data Directory
#'
#' Returns the path to the Opta data directory within pannadata.
#' Uses pannadata/data/opta/ as the base path.
#'
#' @param path Optional path to set as the Opta data directory.
#'
#' @return Character path to Opta data directory.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get current path (auto-detected from pannadata)
#' opta_data_dir()
#'
#' # Set explicit path
#' opta_data_dir("C:/path/to/pannadata/data/opta")
#' }
opta_data_dir <- function(path = NULL) {
  # If path provided, set it
  if (!is.null(path) && length(path) > 0 && nzchar(path)) {
    .opta_env$opta_dir <- normalizePath(path, mustWork = FALSE)
    return(invisible(.opta_env$opta_dir))
  }

  # Return cached path if set
  if (exists("opta_dir", envir = .opta_env)) {
    return(.opta_env$opta_dir)
  }

  # Use pannadata_dir() as the base and add opta/
  base_dir <- tryCatch({
    pannadata_dir()
  }, error = function(e) NULL)

  if (!is.null(base_dir)) {
    opta_dir <- file.path(base_dir, "opta")
    if (dir.exists(opta_dir)) {
      .opta_env$opta_dir <- normalizePath(opta_dir)
      return(.opta_env$opta_dir)
    }
    # Directory doesn't exist yet, but return the path anyway
    .opta_env$opta_dir <- normalizePath(opta_dir, mustWork = FALSE)
    return(.opta_env$opta_dir)
  }

  cli::cli_abort(c(
    "Opta data directory not found.",
    "i" = "Set it explicitly with: {.code opta_data_dir('path/to/pannadata/data/opta')}",
    "i" = "Or set pannadata_dir() first"
  ))
}


#' Convert League Code to Opta Format
#'
#' Converts panna league codes (ENG, ESP, etc.) to Opta format (EPL, La_Liga, etc.).
#'
#' @param league Panna league code or Opta league code.
#' @return Opta league code.
#' @keywords internal
to_opta_league <- function(league) {
  if (league %in% OPTA_LEAGUES) {
    return(league)
  }
  if (league %in% names(OPTA_LEAGUES)) {
    return(OPTA_LEAGUES[[league]])
  }
  cli::cli_abort("Unknown league: {league}. Use one of: {paste(names(OPTA_LEAGUES), collapse=', ')} or {paste(OPTA_LEAGUES, collapse=', ')}")
}


#' List Available Opta Seasons
#'
#' Returns available seasons for a given league in the Opta data.
#'
#' @param league League code (e.g., "ENG", "EPL", "ESP", "La_Liga").
#'
#' @return Character vector of available seasons.
#'
#' @export
#' @examples
#' \dontrun{
#' list_opta_seasons("ENG")
#' list_opta_seasons("EPL")
#' }
list_opta_seasons <- function(league) {
  opta_league <- to_opta_league(league)
  # Look in opta/player_stats/{league}/ for season files
  league_dir <- file.path(opta_data_dir(), "player_stats", opta_league)

  if (!dir.exists(league_dir)) {
    cli::cli_abort("No data found for league: {league}")
  }

  # List parquet files and extract season names
  files <- list.files(league_dir, pattern = "\\.parquet$", full.names = FALSE)
  seasons <- tools::file_path_sans_ext(files)
  sort(seasons, decreasing = TRUE)
}


#' Load Opta Player Stats
#'
#' Loads player-level match statistics from Opta/TheAnalyst data.
#' Contains 271 columns including Opta-exclusive stats like progressiveCarries,
#' possWonDef3rd, touchesInOppBox, bigChanceCreated, etc.
#'
#' @param league League code. Accepts panna format (ENG, ESP, GER, ITA, FRA)
#'   or Opta format (EPL, La_Liga, Bundesliga, Serie_A, Ligue_1).
#' @param season Optional season filter (e.g., "2021-2022"). If NULL, loads all seasons.
#' @param columns Optional character vector of columns to select. If NULL, selects all.
#' @param source Data source: "local" (default) to load from local files,
#'   "remote" to download from GitHub releases.
#'
#' @return Data frame of player statistics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all EPL data from local files
#' epl <- load_opta_stats("ENG")
#'
#' # Load from GitHub releases (downloads opta-parquet.tar.gz)
#' epl_remote <- load_opta_stats("ENG", season = "2021-2022", source = "remote")
#'
#' # Load specific columns only
#' epl_basic <- load_opta_stats("ENG", columns = c(
#'   "match_id", "player_name", "team_name", "minsPlayed",
#'   "goals", "totalScoringAtt", "progressiveCarries"
#' ))
#'
#' # Load all Big 5 leagues
#' big5 <- purrr::map_dfr(c("ENG", "ESP", "GER", "ITA", "FRA"), load_opta_stats)
#' }
load_opta_stats <- function(league, season = NULL, columns = NULL,
                            source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("player_stats", league, season, columns, source)
}


#' Load Opta Shot Data
#'
#' Loads aggregated shot statistics per player per match from Opta data.
#' Includes shot locations, body parts, and big chance metrics.
#'
#' @inheritParams load_opta_stats
#'
#' @return Data frame of shot statistics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all EPL shot data
#' epl_shots <- load_opta_shots("ENG")
#'
#' # Load specific season from remote
#' shots_2122 <- load_opta_shots("ENG", season = "2021-2022", source = "remote")
#' }
load_opta_shots <- function(league, season = NULL, columns = NULL,
                            source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("shots", league, season, columns, source)
}


#' Load Opta Shot Events (Individual Shots with Coordinates)
#'
#' Loads individual shot events with x/y coordinates from Opta/TheAnalyst data.
#' Each row is a single shot with location, outcome, body part, and situation.
#' Useful for xG modeling as it includes shot coordinates.
#'
#' @inheritParams load_opta_stats
#'
#' @return Data frame of shot events with columns:
#'   \itemize{
#'     \item match_id: Match identifier
#'     \item event_id: Unique event identifier
#'     \item player_id, player_name: Shooter info
#'     \item team_id: Team that took the shot
#'     \item minute, second: Time of shot
#'     \item x, y: Shot coordinates (0-100 scale)
#'     \item outcome: 1=on target, 0=off target
#'     \item is_goal: Whether shot resulted in goal
#'     \item type_id: 13=saved, 14=post, 15=miss, 16=goal
#'     \item body_part: Head, LeftFoot, RightFoot
#'     \item situation: OpenPlay, SetPiece, Corner, Penalty
#'     \item big_chance: TRUE if big chance
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Load EPL shot events with coordinates
#' epl_shots <- load_opta_shot_events("ENG", season = "2024-2025")
#'
#' # Analyze shots by location
#' library(ggplot2)
#' ggplot(epl_shots, aes(x = x, y = y, color = is_goal)) +
#'   geom_point(alpha = 0.5)
#' }
load_opta_shot_events <- function(league, season = NULL, columns = NULL,
                                   source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("shot_events", league, season, columns, source)
}


#' Load Opta Match Events (Goals, Cards, Substitutions)
#'
#' Loads match events including goals, cards, and substitutions with timing.
#' Useful for creating splint boundaries in RAPM calculations.
#'
#' @inheritParams load_opta_stats
#'
#' @return Data frame of match events with columns:
#'   \itemize{
#'     \item match_id: Match identifier
#'     \item event_type: goal, yellow_card, red_card, second_yellow, substitution
#'     \item minute, second: Time of event
#'     \item team_id: Team involved
#'     \item player_id, player_name: Player involved
#'     \item player_on_id, player_on_name: Substitute coming on (for substitutions)
#'     \item player_off_id, player_off_name: Player leaving (for substitutions)
#'     \item assist_player_id, assist_player_name: Assister (for goals)
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Load EPL match events
#' epl_events <- load_opta_events("ENG", season = "2024-2025")
#'
#' # Filter to just red cards
#' red_cards <- epl_events |>
#'   dplyr::filter(event_type == "red_card")
#' }
load_opta_events <- function(league, season = NULL, columns = NULL,
                              source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("events", league, season, columns, source)
}


#' Load Opta All Match Events (All Events with X/Y Coordinates)
#'
#' Loads ALL match events with x/y coordinates from Opta/TheAnalyst data.
#' Each match typically has ~2000 events including passes, tackles, aerials,
#' dribbles, shots, and more. This is the most comprehensive event data available.
#'
#' @inheritParams load_opta_stats
#'
#' @return Data frame of all match events with columns:
#'   \itemize{
#'     \item match_id: Match identifier
#'     \item event_id: Unique event identifier
#'     \item type_id: Opta event type (1=pass, 3=dribble, 7=tackle, 13-16=shots, 44=aerial, etc.)
#'     \item player_id, player_name: Player involved
#'     \item team_id: Team that performed the action
#'     \item minute, second: Time of event
#'     \item x, y: Start coordinates (0-100 scale)
#'     \item end_x, end_y: End coordinates for passes/carries (0-100 scale)
#'     \item outcome: 1=successful, 0=unsuccessful
#'     \item period_id: 1=first half, 2=second half
#'     \item qualifier_json: Full qualifiers as JSON string for advanced analysis
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all EPL match events
#' epl_events <- load_opta_match_events("ENG", season = "2024-2025")
#'
#' # Filter to just passes (type_id = 1)
#' passes <- epl_events |>
#'   dplyr::filter(type_id == 1)
#'
#' # Build passing networks
#' pass_counts <- passes |>
#'   dplyr::filter(outcome == 1) |>
#'   dplyr::count(match_id, player_id)
#'
#' # Filter to tackles (type_id = 7)
#' tackles <- epl_events |>
#'   dplyr::filter(type_id == 7)
#' }
load_opta_match_events <- function(league, season = NULL, columns = NULL,
                                    source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("match_events", league, season, columns, source)
}


#' Load Opta Lineup Data
#'
#' Loads lineup information including starting XI, positions, and minutes played.
#' Useful for assigning players to time periods in RAPM calculations.
#'
#' @inheritParams load_opta_stats
#'
#' @return Data frame of lineup data with columns:
#'   \itemize{
#'     \item match_id, match_date: Match identifiers
#'     \item player_id, player_name: Player info
#'     \item team_id, team_name: Team info
#'     \item team_position: home or away
#'     \item position: Goalkeeper, Defender, Midfielder, etc.
#'     \item position_side: Left, Right, Centre
#'     \item formation_place: 1-11 for starters
#'     \item shirt_number: Jersey number
#'     \item is_starter: TRUE if player started
#'     \item minutes_played: Total minutes played
#'     \item sub_on_minute: Minute substituted on (0 if started)
#'     \item sub_off_minute: Minute substituted off (0 if played full match)
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Load EPL lineups
#' epl_lineups <- load_opta_lineups("ENG", season = "2024-2025")
#'
#' # Find starters with most minutes
#' starters <- epl_lineups |>
#'   dplyr::filter(is_starter) |>
#'   dplyr::group_by(player_name, team_name) |>
#'   dplyr::summarise(total_mins = sum(minutes_played))
#' }
load_opta_lineups <- function(league, season = NULL, columns = NULL,
                               source = c("local", "remote")) {
  source <- match.arg(source)
  load_opta_table("lineups", league, season, columns, source)
}


#' Load All Opta Data for Big 5 Leagues
#'
#' Convenience function to load Opta stats for all Big 5 European leagues.
#'
#' @param season Optional season filter. If NULL, loads all available seasons.
#' @param columns Optional character vector of columns to select.
#'
#' @return Data frame with league column added.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all Big 5 data (warning: large!)
#' big5 <- load_opta_big5()
#'
#' # Load specific season across all leagues
#' big5_2122 <- load_opta_big5(season = "2021-2022")
#' }
load_opta_big5 <- function(season = NULL, columns = NULL) {
  leagues <- names(OPTA_LEAGUES)

  results <- lapply(leagues, function(lg) {
    tryCatch({
      df <- load_opta_stats(lg, season, columns)
      df$league <- lg
      df
    }, error = function(e) {
      cli::cli_warn("Failed to load {lg}: {e$message}")
      NULL
    })
  })

  dplyr::bind_rows(Filter(Negate(is.null), results))
}


#' Internal function to load Opta table data
#'
#' @param table_type "player_stats" or "shots"
#' @param league League code
#' @param season Optional season filter
#' @param columns Optional columns to select
#' @param source "local" or "remote"
#'
#' @return Data frame
#' @keywords internal
load_opta_table <- function(table_type, league, season, columns,
                             source = "local") {
  opta_league <- to_opta_league(league)

  # Handle remote source
  if (source == "remote") {
    return(query_remote_opta_parquet(table_type, opta_league, season, columns))
  }

  # Local source - check for consolidated file first
  base_dir <- opta_data_dir()
  consolidated_file <- file.path(base_dir, paste0("opta_", table_type, ".parquet"))

  if (file.exists(consolidated_file)) {
    # Use consolidated file with WHERE clause
    parquet_path <- normalizePath(consolidated_file, winslash = "/", mustWork = TRUE)

    # Build column selection (validate to prevent SQL injection)
    col_sql <- if (!is.null(columns)) {
      paste(validate_sql_columns(columns), collapse = ", ")
    } else {
      "*"
    }

    # Build WHERE clause using helper
    where_sql <- build_where_clause(
      list(competition = opta_league, season = season),
      prefix = FALSE
    )

    sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_path, where_sql)
  } else {
    # Fall back to hierarchical structure
    if (!is.null(season)) {
      parquet_path <- file.path(base_dir, table_type, opta_league, paste0(season, ".parquet"))
      if (!file.exists(parquet_path)) {
        cli::cli_abort(c(
          "Opta data not found.",
          "i" = "Run {.code pb_download_opta()} to download the latest data."
        ))
      }
      parquet_pattern <- sprintf("'%s'", normalizePath(parquet_path, winslash = "/", mustWork = TRUE))
    } else {
      league_dir <- file.path(base_dir, table_type, opta_league)
      if (!dir.exists(league_dir)) {
        cli::cli_abort(c(
          "Opta data not found.",
          "i" = "Run {.code pb_download_opta()} to download the latest data."
        ))
      }
      parquet_pattern <- sprintf("'%s/*.parquet'", normalizePath(league_dir, winslash = "/", mustWork = TRUE))
    }

    # Build column selection (validate to prevent SQL injection)
    col_sql <- if (!is.null(columns)) {
      paste(validate_sql_columns(columns), collapse = ", ")
    } else {
      "*"
    }
    sql <- sprintf("SELECT %s FROM read_parquet(%s, union_by_name=true)", col_sql, parquet_pattern)
  }

  # Execute query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  cli::cli_alert_info("Loading Opta {table_type} for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


#' Get Opta Column Names
#'
#' Returns column names available in Opta data without loading the full dataset.
#'
#' @param table_type One of "player_stats", "shots", "shot_events", "events", or "lineups".
#'
#' @return Character vector of column names.
#'
#' @export
#' @examples
#' \dontrun{
#' # See all player stats columns
#' get_opta_columns("player_stats")
#'
#' # See shot event columns (individual shots with x/y)
#' get_opta_columns("shot_events")
#'
#' # See match event columns (ALL events with x/y)
#' get_opta_columns("match_events")
#'
#' # See event columns (goals, cards, subs)
#' get_opta_columns("events")
#'
#' # See lineup columns
#' get_opta_columns("lineups")
#' }
get_opta_columns <- function(table_type = c("player_stats", "shots", "shot_events", "match_events", "events", "lineups")) {
  table_type <- match.arg(table_type)

  base_dir <- opta_data_dir()

  # Find any parquet file of this type
  # Path structure: opta/{table_type}/{league}/{season}.parquet
  pattern <- file.path(base_dir, table_type, "*", "*.parquet")
  files <- Sys.glob(pattern)

  if (length(files) == 0) {
    cli::cli_abort("No {table_type} parquet files found")
  }

  # Query schema from first file
  parquet_path <- normalizePath(files[1], winslash = "/", mustWork = TRUE)

  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  sql <- sprintf("SELECT * FROM '%s' LIMIT 0", parquet_path)
  result <- DBI::dbGetQuery(conn, sql)

  names(result)
}


# Remote Opta Data Loading ----

# Environment to cache remote Opta data path
.opta_remote_env <- new.env(parent = emptyenv())

#' Download Opta data from GitHub Releases
#'
#' Downloads opta-parquet.tar.gz from the opta-latest release and extracts
#' to a temporary directory. Caches the extracted path for the session.
#'
#' @param repo GitHub repository in "owner/repo" format
#' @param tag Release tag (default: "opta-latest")
#' @param force Force re-download even if cached (default: FALSE)
#'
#' @return Path to extracted Opta data directory
#' @keywords internal
download_remote_opta <- function(repo = "peteowen1/pannadata",
                                  tag = "opta-latest",
                                  force = FALSE) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required for remote Opta loading.")
  }

  # Check if we have a valid cached path
  cache_key <- paste0(repo, "_", tag)
  if (!force && exists(cache_key, envir = .opta_remote_env)) {
    cached_path <- get(cache_key, envir = .opta_remote_env)
    if (dir.exists(cached_path)) {
      return(cached_path)
    }
  }

  cli::cli_alert_info("Downloading Opta data from {repo} ({tag})...")

  # Download to temp directory
  temp_dir <- tempdir()
  archive_file <- file.path(temp_dir, "opta-parquet.tar.gz")

  tryCatch({
    piggyback::pb_download(
      file = "opta-parquet.tar.gz",
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download Opta data from {repo} ({tag})",
      "i" = "Make sure opta-parquet.tar.gz exists in the release.",
      "x" = e$message
    ))
  })

  if (!file.exists(archive_file)) {
    cli::cli_abort("Download failed - opta-parquet.tar.gz not found")
  }

  # Extract to session-specific temp directory
  extract_dir <- file.path(temp_dir, paste0("opta_", tag))
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE)
  }
  dir.create(extract_dir, recursive = TRUE)

  cli::cli_alert_info("Extracting Opta data...")
  utils::untar(archive_file, exdir = extract_dir)

  # The tar.gz contains opta/{table_type}/{league}/{season}.parquet structure
  data_dir <- file.path(extract_dir, "opta")
  if (!dir.exists(data_dir)) {
    # Maybe it's directly in extract_dir
    data_dir <- extract_dir
  }

  # Count files
  n_parquet <- length(list.files(data_dir, pattern = "\\.parquet$", recursive = TRUE))
  cli::cli_alert_success("Extracted {n_parquet} parquet files")

  # Cache the path
  assign(cache_key, data_dir, envir = .opta_remote_env)

  # Cleanup archive
  file.remove(archive_file)

  data_dir
}


#' Query remote Opta parquet data
#'
#' Downloads Opta data from GitHub releases and queries using DuckDB.
#' Data is cached for the session to avoid repeated downloads.
#'
#' @param table_type "player_stats" or "shots"
#' @param opta_league League code in Opta format (EPL, La_Liga, etc.)
#' @param season Optional season filter (e.g., "2021-2022")
#' @param columns Optional columns to select
#' @param repo GitHub repository (default: "peteowen1/pannadata")
#' @param tag Release tag (default: "opta-latest")
#'
#' @return Data frame with query results
#' @keywords internal
query_remote_opta_parquet <- function(table_type, opta_league, season = NULL,
                                       columns = NULL,
                                       repo = "peteowen1/pannadata",
                                       tag = "opta-latest") {
  # Download/get cached data
  base_dir <- download_remote_opta(repo = repo, tag = tag)

  # Build file pattern
  # Path structure: {base_dir}/{table_type}/{league}/{season}.parquet
  if (!is.null(season)) {
    # Specific season: {table_type}/{league}/{season}.parquet
    parquet_path <- file.path(base_dir, table_type, opta_league, paste0(season, ".parquet"))
    if (!file.exists(parquet_path)) {
      cli::cli_abort(c(
        "Remote parquet file not found",
        "i" = "Looking for: {table_type}/{opta_league}/{season}.parquet",
        "i" = "Available table types: {paste(list.dirs(base_dir, recursive = FALSE, full.names = FALSE), collapse = ', ')}"
      ))
    }
    parquet_pattern <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s'", parquet_pattern)
  } else {
    # All seasons for league: {table_type}/{league}/*.parquet
    league_dir <- file.path(base_dir, table_type, opta_league)
    if (!dir.exists(league_dir)) {
      cli::cli_abort(c(
        "Remote league directory not found: {table_type}/{opta_league}",
        "i" = "Available table types: {paste(list.dirs(base_dir, recursive = FALSE, full.names = FALSE), collapse = ', ')}"
      ))
    }
    parquet_pattern <- normalizePath(league_dir, winslash = "/", mustWork = TRUE)
    parquet_pattern <- sprintf("'%s/*.parquet'", parquet_pattern)
  }

  # Build column selection (validate to prevent SQL injection)
  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  sql <- sprintf("SELECT %s FROM read_parquet(%s, union_by_name=true)", col_sql, parquet_pattern)

  # Execute query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  cli::cli_alert_info("Querying remote Opta {table_type} for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


#' Clear remote Opta data cache
#'
#' Removes cached remote Opta data, forcing a fresh download on next access.
#'
#' @export
#' @examples
#' \dontrun{
#' # Force fresh download on next remote load
#' clear_remote_opta_cache()
#' }
#' Load Opta xG/xA/xPass Player Metrics
#'
#' Loads pre-computed player-level xG, xA, and xPass metrics from parquet files.
#' These are generated by the 03_calculate_player_xmetrics.R pipeline script
#' which applies trained xG/xPass models to all Opta event data.
#'
#' @param league League code (e.g., "ENG", "EPL").
#' @param season Optional season filter (e.g., "2024-2025").
#' @param columns Optional character vector of columns to select.
#' @param source Data source: "local" (default) or "remote".
#'
#' @return Data frame with player xmetrics including xg, npxg, xa, xpass stats.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load EPL xmetrics
#' epl_xm <- load_opta_xmetrics("ENG", season = "2024-2025")
#'
#' # Top xG players
#' head(epl_xm[order(-epl_xm$xg), c("player_name", "team_name", "xg", "goals")])
#' }
load_opta_xmetrics <- function(league, season = NULL, columns = NULL,
                                source = c("local", "remote")) {
  source <- match.arg(source)
  opta_league <- to_opta_league(league)

  # xmetrics live under opta_data_dir()/xmetrics/ (alongside player_stats, shots, etc.)
  xmetrics_dir <- file.path(opta_data_dir(), "xmetrics", opta_league)

  if (!is.null(season)) {
    parquet_path <- file.path(xmetrics_dir, paste0(season, ".parquet"))
    if (!file.exists(parquet_path)) {
      cli::cli_abort(c(
        "Opta xmetrics not found for {opta_league} {season}.",
        "i" = "Run the 03_calculate_player_xmetrics.R pipeline first."
      ))
    }
    parquet_pattern <- sprintf("'%s'", normalizePath(parquet_path, winslash = "/", mustWork = TRUE))
  } else {
    if (!dir.exists(xmetrics_dir)) {
      cli::cli_abort(c(
        "Opta xmetrics directory not found for {opta_league}.",
        "i" = "Run the 03_calculate_player_xmetrics.R pipeline first."
      ))
    }
    parquet_pattern <- sprintf("'%s/*.parquet'", normalizePath(xmetrics_dir, winslash = "/", mustWork = TRUE))
  }

  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }
  sql <- sprintf("SELECT %s FROM read_parquet(%s, union_by_name=true)", col_sql, parquet_pattern)

  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE))

  cli::cli_alert_info("Loading Opta xmetrics for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


clear_remote_opta_cache <- function() {

  rm(list = ls(envir = .opta_remote_env), envir = .opta_remote_env)
  cli::cli_alert_success("Remote Opta cache cleared")
}


#' Download Opta Data to Local Directory
#'
#' Downloads consolidated Opta parquet files from GitHub releases to the local
#' pannadata/data/opta/ directory. Downloads two files:
#' - opta_player_stats.parquet (~50MB, 1M+ rows)
#' - opta_shots.parquet (~4MB, 270K rows)
#'
#' @param repo GitHub repository in "owner/repo" format.
#' @param tag Release tag (default: "opta-latest").
#' @param dest Destination directory. If NULL, uses pannadata_dir()/opta.
#'
#' @return Invisibly returns the path to the installed data.
#'
#' @export
#' @examples
#' \dontrun{
#' # Download latest Opta data to pannadata/data/opta/
#' pb_download_opta()
#'
#' # Then load with:
#' load_opta_stats("ENG", season = "2024-2025")
#' }
pb_download_opta <- function(repo = "peteowen1/pannadata",
                              tag = "opta-latest",
                              dest = NULL) {
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  # Determine destination
  if (is.null(dest)) {
    dest <- tryCatch({
      pannadata_dir()
    }, error = function(e) {
      cli::cli_abort(c(
        "Could not determine pannadata directory.",
        "i" = "Set dest explicitly or configure pannadata_dir() first."
      ))
    })
  }

  opta_dir <- file.path(dest, "opta")
  dir.create(opta_dir, showWarnings = FALSE, recursive = TRUE)

  cli::cli_alert_info("Downloading Opta data from {repo} ({tag})...")

  # Download consolidated parquet files
  files_to_download <- c(
    "opta_player_stats.parquet",
    "opta_shots.parquet",
    "opta_shot_events.parquet",
    "opta_match_events.parquet",
    "opta_events.parquet",
    "opta_lineups.parquet"
  )

  for (f in files_to_download) {
    cli::cli_alert_info("Downloading {f}...")
    tryCatch({
      piggyback::pb_download(
        file = f,
        repo = repo,
        tag = tag,
        dest = opta_dir,
        overwrite = TRUE
      )
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to download {f} from {repo} ({tag})",
        "x" = e$message
      ))
    })
  }

  # Report sizes
  for (f in files_to_download) {
    fpath <- file.path(opta_dir, f)
    if (file.exists(fpath)) {
      size_mb <- round(file.info(fpath)$size / (1024 * 1024), 1)
      cli::cli_alert_success("{f}: {size_mb} MB")
    }
  }

  # Update cached path
  .opta_env$opta_dir <- normalizePath(opta_dir)

  cli::cli_alert_success("Opta data installed to {opta_dir}")
  invisible(opta_dir)
}
