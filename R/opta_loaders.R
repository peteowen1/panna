# Opta Data Loader Functions
#
# Functions for loading Opta (TheAnalyst) data from local and remote parquet
# files, with catalog-based discovery of available leagues and seasons.
# Data is scraped from TheAnalyst API and stored in pannadata/data/opta/.
#
# League codes: EPL, La_Liga, Bundesliga, Serie_A, Ligue_1, Eredivisie,
#   Primeira_Liga, Super_Lig, Championship, Scottish_Premiership,
#   UCL, UEL, Conference_League, World_Cup, UEFA_Euros
# Seasons: 2013-2014 to 2025-2026

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


#' Validate a parquet file by checking magic bytes
#'
#' Parquet files must start and end with the 4-byte magic number "PAR1".
#' Interrupted downloads produce truncated files missing the footer.
#'
#' @param path Path to the parquet file.
#' @return TRUE if valid parquet file, FALSE if corrupt, NA if validation
#'   could not be performed (e.g., permission denied).
#' @keywords internal
validate_parquet_file <- function(path) {
  if (!file.exists(path)) return(FALSE)
  fsize <- file.info(path)$size
  if (is.na(fsize) || fsize < 12) return(FALSE)
  tryCatch({
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    header <- readBin(con, "raw", n = 4)
    seek(con, fsize - 4)
    footer <- readBin(con, "raw", n = 4)
    magic <- charToRaw("PAR1")
    identical(header, magic) && identical(footer, magic)
  }, error = function(e) {
    cli::cli_warn("Could not validate parquet file {.path {path}}: {e$message}")
    NA  # Indeterminate — callers should not delete
  })
}


# Opta league code mapping
OPTA_LEAGUES <- c(
  # Big 5
  ENG = "EPL",
  ESP = "La_Liga",
  GER = "Bundesliga",
  ITA = "Serie_A",
  FRA = "Ligue_1",
  # Extended domestic
  NED = "Eredivisie",
  POR = "Primeira_Liga",
  TUR = "Super_Lig",
  ENG2 = "Championship",
  SCO = "Scottish_Premiership",
  # European comps
  UCL = "UCL",
  UEL = "UEL",
  UECL = "Conference_League",
  # International
  WC = "World_Cup",
  EURO = "UEFA_Euros"
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
#' Matching is case-insensitive for convenience (e.g., "epl", "eng", "Eng" all work).
#' Falls back to catalog lookup, then pass-through for valid-looking codes.
#'
#' @param league Panna league code or Opta league code (case-insensitive).
#' @return Opta league code.
#' @keywords internal
to_opta_league <- function(league) {
  league_upper <- toupper(league)

  # 1. Panna alias, case-insensitive (e.g., "ENG"/"eng" -> "EPL")
  if (league_upper %in% names(OPTA_LEAGUES)) {
    return(OPTA_LEAGUES[[league_upper]])
  }
  # 2. Direct Opta code, case-insensitive (e.g., "EPL"/"epl" -> "EPL")
  opta_codes_upper <- toupper(OPTA_LEAGUES)
  match_idx <- match(league_upper, opta_codes_upper)
  if (!is.na(match_idx)) {
    return(OPTA_LEAGUES[[match_idx]])
  }
  # 3. Catalog lookup (session-cached)
  catalog <- tryCatch(download_opta_catalog(), error = function(e) {
    cli::cli_warn("Could not load Opta catalog: {e$message}")
    NULL
  })
  if (!is.null(catalog)) {
    catalog_names_upper <- toupper(names(catalog$competitions))
    if (league_upper %in% catalog_names_upper) {
      return(names(catalog$competitions)[match(league_upper, catalog_names_upper)])
    }
    aliases <- catalog$panna_aliases
    aliases_names_upper <- toupper(names(aliases))
    if (league_upper %in% aliases_names_upper) {
      return(aliases[[match(league_upper, aliases_names_upper)]])
    }
  }
  # 4. Pass-through with warning only when catalog was unavailable (offline scenario)
  if (is.null(catalog) && grepl("^[A-Za-z][A-Za-z0-9_]+$", league)) {
    cli::cli_warn(c(
      "League {.val {league}} not in hardcoded mappings (catalog unavailable).",
      "i" = "Passing through as-is. Use {.fn list_opta_leagues} to see available competitions."
    ))
    return(league)
  }
  cli::cli_abort(c(
    "Unknown league code: {.val {league}}.",
    "i" = "Use {.fn list_opta_leagues} to see available competitions."
  ))
}


#' List Available Opta Seasons
#'
#' Returns available seasons for a given league in the Opta data.
#'
#' @param league League code (e.g., "ENG", "EPL", "ESP", "La_Liga").
#' @param source Data source: "catalog" (default) reads from downloaded catalog,
#'   "remote" is an alias for "catalog", "local" scans the local filesystem.
#'   Falls back to catalog if local directory doesn't exist.
#'
#' @return Character vector of available seasons.
#'
#' @export
#' @examples
#' \dontrun{
#' list_opta_seasons("ENG")
#' list_opta_seasons("EPL", source = "local")
#' }
list_opta_seasons <- function(league, source = c("catalog", "remote", "local")) {
  source <- match.arg(source)
  if (source == "remote") source <- "catalog"
  opta_league <- to_opta_league(league)

  if (source == "local") {
    league_dir <- file.path(opta_data_dir(), "player_stats", opta_league)
    if (dir.exists(league_dir)) {
      files <- list.files(league_dir, pattern = "\\.parquet$", full.names = FALSE)
      seasons <- tools::file_path_sans_ext(files)
      return(sort(seasons, decreasing = TRUE))
    }
    # Fall through to catalog if local dir doesn't exist
    cli::cli_alert_info("No local data for {opta_league}, checking catalog...")
  }

  # Catalog source
  catalog <- tryCatch(download_opta_catalog(), error = function(e) {
    cli::cli_warn("Could not load Opta catalog: {e$message}")
    NULL
  })
  if (!is.null(catalog) && opta_league %in% names(catalog$competitions)) {
    seasons <- catalog$competitions[[opta_league]]$seasons
    return(sort(unlist(seasons), decreasing = TRUE))
  }

  cli::cli_abort(c(
    "No data found for league: {.val {league}}",
    "i" = "If the catalog failed to load, check your internet connection.",
    "i" = "Use {.fn list_opta_leagues} to see available competitions."
  ))
}


#' Load Opta Player Stats
#'
#' Loads player-level match statistics from Opta/TheAnalyst data.
#' Contains 263 columns including Opta-exclusive stats like progressiveCarries,
#' possWonDef3rd, touchesInOppBox, bigChanceCreated, etc.
#'
#' @param league League code. Accepts panna format (ENG, ESP, GER, ITA, FRA)
#'   or Opta format (EPL, La_Liga, Bundesliga, Serie_A, Ligue_1).
#' @param season Optional season filter (e.g., "2021-2022"). If NULL, loads all seasons.
#' @param columns Optional character vector of columns to select. If NULL, selects all.
#' @param source Data source: "remote" (default) downloads from GitHub releases,
#'   "local" loads from local files (requires prior \code{pb_download_opta()}).
#'
#' @return Data frame of player statistics.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load EPL data (downloads automatically from GitHub)
#' epl <- load_opta_stats("ENG")
#'
#' # Load from local files (requires pb_download_opta() first)
#' epl_local <- load_opta_stats("ENG", season = "2021-2022", source = "local")
#'
#' # Load specific columns only
#' epl_basic <- load_opta_stats("ENG", columns = c(
#'   "match_id", "player_name", "team_name", "minsPlayed",
#'   "goals", "totalScoringAtt", "progressiveCarries"
#' ))
#'
#' # Load all Big 5 leagues
#' big5 <- data.table::rbindlist(lapply(c("ENG", "ESP", "GER", "ITA", "FRA"), load_opta_stats))
#' }
load_opta_stats <- function(league, season = NULL, columns = NULL,
                            source = c("remote", "local")) {
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
#' # Load specific season
#' shots_2122 <- load_opta_shots("ENG", season = "2021-2022")
#' }
load_opta_shots <- function(league, season = NULL, columns = NULL,
                            source = c("remote", "local")) {
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
                                   source = c("remote", "local")) {
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
                              source = c("remote", "local")) {
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
                                    source = c("remote", "local")) {
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
                               source = c("remote", "local")) {
  source <- match.arg(source)
  load_opta_table("lineups", league, season, columns, source)
}


#' Load Opta Fixture Data
#'
#' Loads fixture data including both played and upcoming matches from Opta.
#' Fixtures are saved by the scraper alongside match data and include match
#' status (Played, Fixture, Postponed).
#'
#' @inheritParams load_opta_stats
#' @param status Optional match status filter (e.g., "Fixture", "Played", "Postponed").
#'   If NULL (default), returns all statuses.
#'
#' @return Data frame of fixtures with columns:
#'   \itemize{
#'     \item match_id: Match identifier
#'     \item match_date: Scheduled/played date
#'     \item match_status: Fixture, Played, or Postponed
#'     \item home_team, away_team: Team names
#'     \item home_team_id, away_team_id: Team IDs
#'     \item competition: Opta league code
#'     \item season: Season identifier
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all EPL fixtures
#' epl_fix <- load_opta_fixtures("ENG", season = "2024-2025")
#'
#' # Load only upcoming matches
#' upcoming <- load_opta_fixtures("ENG", season = "2024-2025", status = "Fixture")
#' }
load_opta_fixtures <- function(league, season = NULL, columns = NULL,
                                status = NULL,
                                source = c("remote", "local")) {
  source <- match.arg(source)
  result <- load_opta_table("fixtures", league, season, columns, source)

  if (!is.null(status) && "match_status" %in% names(result)) {
    result <- result[result$match_status %in% status, , drop = FALSE]
  }

  result
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
  leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")

  error_msgs <- character(0)
  results <- lapply(leagues, function(lg) {
    tryCatch({
      df <- load_opta_stats(lg, season, columns)
      df$league <- lg
      df
    }, error = function(e) {
      error_msgs[[lg]] <<- e$message
      cli::cli_warn("Failed to load {lg}: {e$message}")
      NULL
    })
  })

  valid_results <- Filter(Negate(is.null), results)

  if (length(valid_results) == 0) {
    unique_errs <- unique(error_msgs)
    if (length(unique_errs) == 1) {
      cli::cli_abort(c(
        "Failed to load data for any Big 5 league.",
        "x" = "All 5 leagues failed with: {unique_errs}"
      ))
    } else {
      cli::cli_abort("Failed to load data for any Big 5 league.")
    }
  }
  if (length(valid_results) < length(leagues)) {
    loaded <- vapply(valid_results, function(x) x$league[1], character(1))
    failed <- setdiff(leagues, loaded)
    cli::cli_warn(c(
      "Only {length(valid_results)}/{length(leagues)} Big 5 leagues loaded.",
      "i" = "Failed: {paste(failed, collapse = ', ')}",
      "i" = "Results may be incomplete."
    ))
  }

  rbindlist(valid_results, use.names = TRUE, fill = TRUE)
}


#' Suggest Available Seasons for an Opta League
#'
#' Returns available seasons for a league, checking local data then catalog.
#' Useful for discovering what season format a competition uses (e.g.,
#' "2024-2025" for leagues vs "2018 Russia" for World Cup).
#'
#' @param league League code (e.g., "World_Cup", "EPL", "AFCON").
#'   Accepts both panna aliases (e.g., "ENG") and Opta codes.
#' @param table_type Table type to check (default: "match_events").
#' @param base_dir Opta data directory. If NULL, auto-detected.
#'
#' @return Character vector of available seasons (most recent first), or empty.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' suggest_opta_seasons("World_Cup")
#' # [1] "2022 Qatar" "2018 Russia" "2014 Brazil" ...
#'
#' suggest_opta_seasons("EPL")
#' # [1] "2025-2026" "2024-2025" "2023-2024" ...
#' }
suggest_opta_seasons <- function(league, table_type = "match_events",
                                  base_dir = NULL) {
  opta_league <- to_opta_league(league)
  if (is.null(base_dir)) {
    base_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  }

  seasons <- character(0)

  # Try local filesystem (hierarchical)
  if (!is.null(base_dir)) {
    league_dir <- file.path(base_dir, table_type, opta_league)
    if (dir.exists(league_dir)) {
      files <- list.files(league_dir, pattern = "\\.parquet$", full.names = FALSE)
      seasons <- sort(tools::file_path_sans_ext(files), decreasing = TRUE)
    }
  }

  # Try consolidated parquet (distinct seasons via DuckDB)
  if (length(seasons) == 0 && !is.null(base_dir)) {
    consolidated <- file.path(base_dir, paste0("opta_", table_type, ".parquet"))
    if (file.exists(consolidated)) {
      tryCatch({
        conn <- DBI::dbConnect(duckdb::duckdb())
        on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
        pq <- normalizePath(consolidated, winslash = "/", mustWork = TRUE)
        where_sql <- build_where_clause(list(competition = opta_league), prefix = FALSE)
        sql <- sprintf(
          "SELECT DISTINCT season FROM '%s' WHERE %s ORDER BY season DESC",
          pq, where_sql
        )
        res <- DBI::dbGetQuery(conn, sql)
        seasons <- res$season
      }, error = function(e) {
        cli::cli_warn("Could not query consolidated parquet: {e$message}")
        NULL
      })
    }
  }

  # Fall back to catalog
  if (length(seasons) == 0) {
    catalog <- tryCatch(download_opta_catalog(), error = function(e) {
      cli::cli_warn("Could not load Opta catalog: {e$message}")
      NULL
    })
    if (!is.null(catalog) && opta_league %in% names(catalog$competitions)) {
      seasons <- sort(unlist(catalog$competitions[[opta_league]]$seasons),
                      decreasing = TRUE)
    }
  }

  seasons
}


#' Internal function to load Opta table data
#'
#' @param table_type Table type: "player_stats", "shots", "shot_events",
#'   "events", "match_events", "lineups", or "fixtures".
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
        avail <- suggest_opta_seasons(opta_league, table_type, base_dir)
        if (length(avail) > 0) {
          cli::cli_abort(c(
            "No data found for {opta_league} season {.val {season}}.",
            "i" = "Available seasons: {paste(avail, collapse = ', ')}",
            "i" = "Note: leagues use {.val 2024-2025} format, tournaments use {.val {c('2024', '2024 Germany')}} format."
          ))
        } else {
          cli::cli_abort(c(
            "Opta data not found for {opta_league}.",
            "i" = "Run {.code pb_download_opta()} to download the latest data.",
            "i" = "Or use {.code source = 'remote'} to load directly from GitHub."
          ))
        }
      }
      parquet_pattern <- sprintf("'%s'", normalizePath(parquet_path, winslash = "/", mustWork = TRUE))
    } else {
      league_dir <- file.path(base_dir, table_type, opta_league)
      if (!dir.exists(league_dir)) {
        cli::cli_abort(c(
          "Opta data not found.",
          "i" = "Run {.code pb_download_opta()} to download the latest data.",
          "i" = "Or use {.code source = 'remote'} to load directly from GitHub."
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
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cli::cli_alert_info("Loading Opta {table_type} for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "Parquet file is corrupt for {opta_league} {table_type}.",
        "i" = "Try {.code source = 'remote'} or re-download with {.fn pb_download_opta}."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  # If season was requested but got 0 rows, show available seasons
  if (nrow(result) == 0 && !is.null(season)) {
    avail <- suggest_opta_seasons(opta_league, table_type, base_dir)
    msg <- "No data found for {opta_league} season {.val {season}}."
    hints <- character(0)
    if (length(avail) > 0) {
      hints <- c(hints, "i" = "Available seasons: {paste(avail, collapse = ', ')}")
    }
    hints <- c(hints, "i" = "Note: leagues use {.val 2024-2025} format, tournaments use {.val {c('2024', '2024 Germany')}} format.")
    cli::cli_abort(c(msg, hints))
  }

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
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  sql <- sprintf("SELECT * FROM '%s' LIMIT 0", parquet_path)
  result <- DBI::dbGetQuery(conn, sql)

  names(result)
}


# Remote Opta Data Loading ----

# Environment to cache remote Opta data paths
.opta_remote_env <- new.env(parent = emptyenv())


#' Download and Cache Opta Data Catalog
#'
#' Loads the opta-catalog.json file, checking session cache first,
#' then local file, then downloading from GitHub releases.
#'
#' @param repo GitHub repository (default: "peteowen1/pannadata").
#' @param tag Release tag (default: "opta-latest").
#'
#' @return List with catalog data (competitions, panna_aliases).
#' @keywords internal
download_opta_catalog <- function(repo = "peteowen1/pannadata",
                                   tag = "opta-latest") {
  # 1. Session cache
  if (exists("opta_catalog", envir = .opta_remote_env)) {
    return(get("opta_catalog", envir = .opta_remote_env))
  }

  # 2. Local file
  local_path <- tryCatch(
    file.path(opta_data_dir(), "opta-catalog.json"),
    error = function(e) NULL
  )
  if (!is.null(local_path) && file.exists(local_path)) {
    catalog <- tryCatch(
      jsonlite::fromJSON(local_path, simplifyVector = FALSE),
      error = function(e) {
        cli::cli_alert_warning(
          "Local catalog at {.path {local_path}} is corrupt: {e$message}. Downloading fresh."
        )
        NULL
      }
    )
    if (!is.null(catalog) && !is.null(catalog$competitions)) {
      assign("opta_catalog", catalog, envir = .opta_remote_env)
      return(catalog)
    }
    # Fall through to download if invalid structure
  }

  # 3. Download from release
  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg piggyback} is required to download the catalog.")
  }

  temp_dir <- file.path(tempdir(), "opta_catalog")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  tryCatch({
    piggyback::pb_download(
      file = "opta-catalog.json",
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to download opta-catalog.json from {repo} ({tag})",
      "x" = e$message
    ))
  })

  catalog_path <- file.path(temp_dir, "opta-catalog.json")
  if (!file.exists(catalog_path)) {
    cli::cli_abort("Download failed - opta-catalog.json not found")
  }

  catalog <- tryCatch(
    jsonlite::fromJSON(catalog_path, simplifyVector = FALSE),
    error = function(e) {
      cli::cli_abort(c(
        "Downloaded opta-catalog.json is corrupt.",
        "x" = e$message,
        "i" = "Please try again or check your network connection."
      ))
    }
  )
  assign("opta_catalog", catalog, envir = .opta_remote_env)
  catalog
}


#' List Available Opta Leagues
#'
#' Returns a data frame of all competitions available in the Opta data,
#' with metadata including name, country, type, and tier.
#'
#' @param type Optional filter: "league", "cup", "domestic_cup", "international".
#' @param tier Optional numeric filter for competition tier (1 = top tier).
#' @param source Data source: "catalog" (default) downloads the catalog,
#'   "remote" is an alias for "catalog", "local" scans the local filesystem.
#'
#' @return Data frame with columns: code, name, country, type, tier,
#'   n_seasons, n_matches, panna_alias.
#'
#' @export
#' @examples
#' \dontrun{
#' # All competitions
#' list_opta_leagues()
#'
#' # Top-tier leagues only
#' list_opta_leagues(tier = 1)
#'
#' # Just cups
#' list_opta_leagues(type = "cup")
#'
#' # Scan local filesystem (no download)
#' list_opta_leagues(source = "local")
#' }
list_opta_leagues <- function(type = NULL, tier = NULL,
                               source = c("catalog", "remote", "local")) {
  source <- match.arg(source)
  if (source == "remote") source <- "catalog"

  if (source == "catalog") {
    catalog <- download_opta_catalog()
    comps <- catalog$competitions
    aliases <- catalog$panna_aliases

    # Build reverse alias lookup (Opta code -> panna code)
    reverse_aliases <- stats::setNames(names(aliases), unlist(aliases))

    rows <- lapply(names(comps), function(code) {
      c <- comps[[code]]
      data.frame(
        code = code,
        name = c$name %||% code,
        country = c$country %||% "Unknown",
        type = c$type %||% "unknown",
        tier = as.integer(c$tier %||% 99L),
        n_seasons = length(c$seasons),
        n_matches = as.integer(c$n_matches %||% 0L),
        panna_alias = if (code %in% names(reverse_aliases)) reverse_aliases[[code]] else NA_character_,
        stringsAsFactors = FALSE
      )
    })
    result <- if (length(rows) > 0) do.call(rbind, rows) else {
      data.frame(code = character(0), name = character(0), country = character(0),
                 type = character(0), tier = integer(0), n_seasons = integer(0),
                 n_matches = integer(0), panna_alias = character(0),
                 stringsAsFactors = FALSE)
    }
  } else {
    # Local filesystem scan
    base_dir <- opta_data_dir()
    ps_dir <- file.path(base_dir, "player_stats")
    if (!dir.exists(ps_dir)) {
      cli::cli_abort("No local Opta data found at {.path {ps_dir}}")
    }
    leagues <- list.dirs(ps_dir, full.names = FALSE, recursive = FALSE)
    reverse_opta <- stats::setNames(names(OPTA_LEAGUES), OPTA_LEAGUES)

    rows <- lapply(leagues, function(lg) {
      season_files <- list.files(file.path(ps_dir, lg), pattern = "\\.parquet$")
      data.frame(
        code = lg,
        name = lg,
        country = NA_character_,
        type = NA_character_,
        tier = NA_integer_,
        n_seasons = length(season_files),
        n_matches = NA_integer_,
        panna_alias = if (lg %in% names(reverse_opta)) reverse_opta[[lg]] else NA_character_,
        stringsAsFactors = FALSE
      )
    })
    result <- if (length(rows) > 0) do.call(rbind, rows) else {
      data.frame(code = character(0), name = character(0), country = character(0),
                 type = character(0), tier = integer(0), n_seasons = integer(0),
                 n_matches = integer(0), panna_alias = character(0),
                 stringsAsFactors = FALSE)
    }
  }

  # Apply filters
  if (!is.null(type)) {
    result <- result[result$type %in% type, , drop = FALSE]
  }
  if (!is.null(tier)) {
    result <- result[!is.na(result$tier) & result$tier <= tier, , drop = FALSE]
  }

  result[order(result$tier, result$name), , drop = FALSE]
}


#' Query remote Opta parquet data
#'
#' Downloads individual consolidated Opta files from GitHub releases and
#' queries using DuckDB. Each table type is a single consolidated file
#' (e.g., opta_player_stats.parquet) that is cached for the session.
#'
#' @param table_type Table type (player_stats, shots, shot_events, events, lineups, fixtures)
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

  # match_events are stored as per-league files (too large for single consolidated file)
  if (table_type == "match_events") {
    return(query_remote_opta_match_events(opta_league, season, columns,
                                           repo = repo, tag = tag))
  }

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required for remote Opta loading.")
  }

  # Download consolidated file for this table type (cached per session)
  file_name <- paste0("opta_", table_type, ".parquet")
  cache_key <- paste0(table_type, "_", repo, "_", tag)

  # Check cache (validate parquet is not corrupt)
  parquet_path <- NULL
  if (exists(cache_key, envir = .opta_remote_env)) {
    cached_path <- get(cache_key, envir = .opta_remote_env)
    if (file.exists(cached_path) && validate_parquet_file(cached_path)) {
      parquet_path <- cached_path
    } else if (file.exists(cached_path)) {
      cli::cli_alert_warning("Cached {file_name} is corrupt (incomplete download?). Re-downloading...")
      unlink(cached_path)
      rm(list = cache_key, envir = .opta_remote_env)
    }
  }

  # Download if not cached
  if (is.null(parquet_path)) {
    temp_dir <- file.path(tempdir(), "opta_consolidated")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

    cli::cli_alert_info("Downloading {file_name} from {repo} ({tag})...")

    tryCatch({
      piggyback::pb_download(
        file = file_name,
        repo = repo,
        tag = tag,
        dest = temp_dir,
        overwrite = TRUE
      )
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to download {file_name} from {repo} ({tag})",
        "i" = "Run {.code pb_download_opta()} to download all Opta data.",
        "x" = e$message
      ))
    })

    parquet_path <- file.path(temp_dir, file_name)
    if (!file.exists(parquet_path)) {
      cli::cli_abort("Download failed - {file_name} not found after download")
    }

    # Validate downloaded file (isFALSE: skip deletion if validation is indeterminate/NA)
    if (isFALSE(validate_parquet_file(parquet_path))) {
      unlink(parquet_path)
      cli::cli_abort(c(
        "Downloaded {file_name} is corrupt (incomplete download).",
        "i" = "Please try again. If the problem persists, check your network connection."
      ))
    }

    assign(cache_key, parquet_path, envir = .opta_remote_env)
  }

  # Build column selection (validate to prevent SQL injection)
  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  # Build WHERE clause
  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
  where_sql <- build_where_clause(
    list(competition = opta_league, season = season),
    prefix = FALSE
  )
  sql <- if (nchar(where_sql) > 0) {
    sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)
  } else {
    sprintf("SELECT %s FROM '%s'", col_sql, parquet_norm)
  }

  # Execute query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cli::cli_alert_info("Querying remote Opta {table_type} for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      # Invalidate corrupt cached file
      if (exists(cache_key, envir = .opta_remote_env)) {
        cached <- get(cache_key, envir = .opta_remote_env)
        if (file.exists(cached)) unlink(cached)
        rm(list = cache_key, envir = .opta_remote_env)
      }
      cli::cli_abort(c(
        "Cached parquet file is corrupt (no magic bytes).",
        "i" = "The corrupt file has been removed. Please re-run your command to re-download."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


#' Download and query remote Opta match events (per-league files)
#'
#' Match events are too large for a single consolidated file, so they are stored
#' as per-league files (\code{events_\{league\}.parquet}) in the release. This function
#' downloads the per-league file and queries it with DuckDB.
#'
#' @param opta_league League code in Opta format (EPL, La_Liga, etc.)
#' @param season Optional season filter (e.g., "2021-2022")
#' @param columns Optional columns to select
#' @param repo GitHub repository
#' @param tag Release tag
#'
#' @return Data frame with query results
#' @keywords internal
query_remote_opta_match_events <- function(opta_league, season = NULL,
                                            columns = NULL,
                                            repo = "peteowen1/pannadata",
                                            tag = "opta-latest") {
  # Cache key for this league's events file
  cache_key <- paste0("match_events_", opta_league, "_", repo, "_", tag)

  # Check cache first (validate parquet is not corrupt)
  if (exists(cache_key, envir = .opta_remote_env)) {
    cached_path <- get(cache_key, envir = .opta_remote_env)
    if (file.exists(cached_path) && validate_parquet_file(cached_path)) {
      parquet_path <- cached_path
    } else {
      if (file.exists(cached_path)) {
        cli::cli_alert_warning("Cached events for {opta_league} is corrupt. Re-downloading...")
        unlink(cached_path)
      }
      rm(list = cache_key, envir = .opta_remote_env)
      parquet_path <- NULL
    }
  } else {
    parquet_path <- NULL
  }

  # Download if not cached
  if (is.null(parquet_path)) {
    file_name <- paste0("events_", opta_league, ".parquet")
    temp_dir <- file.path(tempdir(), "opta_match_events")
    dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

    cli::cli_alert_info("Downloading {file_name} from {repo} ({tag})...")

    tryCatch({
      piggyback::pb_download(
        file = file_name,
        repo = repo,
        tag = tag,
        dest = temp_dir,
        overwrite = TRUE
      )
    }, error = function(e) {
      cli::cli_abort(c(
        "Failed to download match events for {opta_league}",
        "i" = "Expected file: {file_name} in release {tag}",
        "x" = e$message
      ))
    })

    parquet_path <- file.path(temp_dir, file_name)
    if (!file.exists(parquet_path)) {
      cli::cli_abort("Download failed - {file_name} not found after download")
    }

    # Validate downloaded file (isFALSE: skip deletion if validation is indeterminate/NA)
    if (isFALSE(validate_parquet_file(parquet_path))) {
      unlink(parquet_path)
      cli::cli_abort(c(
        "Downloaded events for {opta_league} is corrupt (incomplete download).",
        "i" = "Please try again. If the problem persists, check your network connection."
      ))
    }

    # Cache the path
    assign(cache_key, parquet_path, envir = .opta_remote_env)
  }

  # Build column selection
  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  # Build WHERE clause for season filter (using build_where_clause to prevent SQL injection)
  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
  where_sql <- build_where_clause(list(season = season), prefix = FALSE)
  if (nchar(where_sql) > 0) {
    sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)
  } else {
    sql <- sprintf("SELECT %s FROM '%s'", col_sql, parquet_norm)
  }

  # Execute query with DuckDB
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cli::cli_alert_info("Querying match events for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      if (exists(cache_key, envir = .opta_remote_env)) {
        cached <- get(cache_key, envir = .opta_remote_env)
        if (file.exists(cached)) unlink(cached)
        rm(list = cache_key, envir = .opta_remote_env)
      }
      cli::cli_abort(c(
        "Cached parquet file is corrupt (no magic bytes).",
        "i" = "The corrupt file has been removed. Please re-run your command to re-download."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


#' Load Opta xG/xA/xPass Player Metrics
#'
#' Loads pre-computed player-level xG, xA, and xPass metrics from parquet files.
#' These are generated by the 03_calculate_player_xmetrics.R pipeline script
#' which applies trained xG/xPass models to all Opta event data.
#'
#' @param league League code (e.g., "ENG", "EPL").
#' @param season Optional season filter (e.g., "2024-2025").
#' @param columns Optional character vector of columns to select.
#' @param source Data source: only "local" is supported (xmetrics are
#'   pipeline-generated, not available in GitHub releases).
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
                                source = c("local")) {
  source <- match.arg(source)
  opta_league <- to_opta_league(league)

  # xmetrics are pipeline-generated per-league/season files, not in the GitHub release
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
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cli::cli_alert_info("Loading Opta xmetrics for {opta_league}...")

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      cli::cli_abort(c(
        "Parquet file is corrupt for {opta_league} xmetrics.",
        "i" = "Re-run the 03_calculate_player_xmetrics.R pipeline to regenerate."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
  result
}


#' Load pre-computed Opta skill estimates
#'
#' Downloads and queries \code{opta_skills.parquet} from the \code{opta-latest}
#' GitHub release. This file contains Bayesian decay-weighted skill estimates
#' produced by the estimated skills pipeline (~15K player-seasons, ~2-3 MB).
#'
#' @param season Optional season filter as end year (e.g., \code{2025} for
#'   2024-2025 season).
#' @param columns Optional character vector of columns to select.
#' @param source Data source: \code{"remote"} (default, downloads from GitHub)
#'   or \code{"local"} (reads from \code{opta_data_dir()}).
#' @param repo GitHub repository (default: "peteowen1/pannadata").
#' @param tag Release tag (default: "opta-latest").
#'
#' @return Data frame with one row per player-season containing skill estimates,
#'   player metadata (\code{player_id}, \code{player_name},
#'   \code{primary_position}), and context columns (\code{season_end_year},
#'   \code{weighted_90s}, \code{total_minutes}).
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all skills
#' skills <- load_opta_skills()
#'
#' # Load specific season
#' skills_2025 <- load_opta_skills(season = 2025)
#'
#' # Use with player_skill_profile
#' player_skill_profile("H. Kane", skills = skills)
#' }
load_opta_skills <- function(season = NULL, columns = NULL,
                              source = c("remote", "local"),
                              repo = "peteowen1/pannadata",
                              tag = "opta-latest") {
  source <- match.arg(source)
  file_name <- "opta_skills.parquet"
  cache_key <- paste0(file_name, "_", repo, "_", tag)

  parquet_path <- download_opta_release_file(
    file_name, source = source, repo = repo, tag = tag
  )

  # Build SQL query
  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
  where_sql <- build_where_clause(
    list(season_end_year = season),
    prefix = FALSE
  )

  sql <- if (nchar(where_sql) > 0) {
    sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)
  } else {
    sprintf("SELECT %s FROM '%s'", col_sql, parquet_norm)
  }

  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      if (source == "remote" && exists(cache_key, envir = .opta_remote_env)) {
        cached <- get(cache_key, envir = .opta_remote_env)
        if (file.exists(cached)) unlink(cached)
        rm(list = cache_key, envir = .opta_remote_env)
      }
      cli::cli_abort(c(
        "Parquet file is corrupt.",
        "i" = "The corrupt file has been removed. Please re-run your command."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success(
    "Loaded {format(nrow(result), big.mark=',')} skill estimates ({ncol(result)} columns)"
  )
  result
}


#' Load pre-computed Opta match-level stats
#'
#' Downloads and queries \code{opta_match_stats.parquet} from the
#' \code{opta-latest} GitHub release. This file contains processed match-level
#' player stats with \code{_p90} columns, produced by the estimated skills
#' pipeline step 01 (~15 MB, ~100K rows).
#'
#' @param season Optional season filter as end year (e.g., \code{2025}).
#' @param columns Optional character vector of columns to select.
#' @param source Data source: \code{"remote"} (default) or \code{"local"}.
#' @param repo GitHub repository (default: "peteowen1/pannadata").
#' @param tag Release tag (default: "opta-latest").
#'
#' @return Data frame with one row per player-match containing processed stats
#'   with \code{_p90} suffixes, \code{player_id}, \code{player_name},
#'   \code{match_date}, \code{total_minutes}, etc.
#'
#' @export
#' @examples
#' \dontrun{
#' # Load all match stats
#' ms <- load_opta_match_stats()
#'
#' # Use with player_skill_profile for full diagnostic output
#' player_skill_profile("H. Kane", match_stats = ms)
#' }
load_opta_match_stats <- function(season = NULL, columns = NULL,
                                   source = c("remote", "local"),
                                   repo = "peteowen1/pannadata",
                                   tag = "opta-latest") {
  source <- match.arg(source)
  file_name <- "opta_match_stats.parquet"

  parquet_path <- download_opta_release_file(
    file_name, source = source, repo = repo, tag = tag
  )

  # Build SQL query
  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)

  # Season filter: match_stats uses season_end_year (if present) or derive from match_date
  where_sql <- build_where_clause(
    list(season_end_year = season),
    prefix = FALSE
  )

  sql <- if (nchar(where_sql) > 0) {
    sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)
  } else {
    sprintf("SELECT %s FROM '%s'", col_sql, parquet_norm)
  }

  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  cache_key <- paste0(file_name, "_", repo, "_", tag)

  result <- tryCatch({
    DBI::dbGetQuery(conn, sql)
  }, error = function(e) {
    if (grepl("magic bytes|No magic bytes", e$message, ignore.case = TRUE)) {
      if (source == "remote" && exists(cache_key, envir = .opta_remote_env)) {
        cached <- get(cache_key, envir = .opta_remote_env)
        if (file.exists(cached)) unlink(cached)
        rm(list = cache_key, envir = .opta_remote_env)
      }
      cli::cli_abort(c(
        "Parquet file is corrupt.",
        "i" = "The corrupt file has been removed. Please re-run your command."
      ))
    }
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  cli::cli_alert_success(
    "Loaded {format(nrow(result), big.mark=',')} match stats ({ncol(result)} columns)"
  )
  result
}


#' Download a file from an Opta GitHub release with fallback
#'
#' Handles session caching, parquet validation, and falls back to a direct
#' GitHub URL if piggyback's memoised asset list is stale.
#'
#' @param file_name Name of the file to download.
#' @param source "remote" or "local".
#' @param repo GitHub repository.
#' @param tag Release tag.
#'
#' @return Path to the downloaded file.
#' @keywords internal
download_opta_release_file <- function(file_name,
                                        source = c("remote", "local"),
                                        repo = "peteowen1/pannadata",
                                        tag = "opta-latest") {
  source <- match.arg(source)

  if (source == "local") {
    parquet_path <- file.path(opta_data_dir(), file_name)
    if (!file.exists(parquet_path)) {
      cli::cli_abort(c(
        "Local {file_name} not found at {.path {parquet_path}}.",
        "i" = "Run {.code pb_download_opta()} or use {.code source = 'remote'}."
      ))
    }
    return(parquet_path)
  }

  # Remote: download + cache per session
  cache_key <- paste0(file_name, "_", repo, "_", tag)

  if (exists(cache_key, envir = .opta_remote_env)) {
    cached_path <- get(cache_key, envir = .opta_remote_env)
    if (file.exists(cached_path) && validate_parquet_file(cached_path)) {
      return(cached_path)
    } else if (file.exists(cached_path)) {
      cli::cli_alert_warning("Cached {file_name} is corrupt. Re-downloading...")
      unlink(cached_path)
      rm(list = cache_key, envir = .opta_remote_env)
    }
  }

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required for remote loading.")
  }

  temp_dir <- file.path(tempdir(), "opta_consolidated")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  cli::cli_alert_info("Downloading {file_name} from {repo} ({tag})...")

  # Remove any stale cached file before downloading to prevent using outdated data
  parquet_path <- file.path(temp_dir, file_name)
  if (file.exists(parquet_path)) unlink(parquet_path)

  # Try piggyback first; fall back to direct URL if stale cache
  pb_error_msg <- NULL
  tryCatch({
    piggyback::pb_download(
      file = file_name,
      repo = repo,
      tag = tag,
      dest = temp_dir,
      overwrite = TRUE
    )
  }, error = function(e) {
    pb_error_msg <<- e$message
    cli::cli_alert_warning("piggyback failed: {e$message}, trying direct URL...")
    NULL
  })

  if (!file.exists(parquet_path)) {
    direct_url <- sprintf(
      "https://github.com/%s/releases/download/%s/%s",
      repo, tag, file_name
    )
    cli::cli_alert_info("Retrying via direct download...")
    tryCatch({
      utils::download.file(
        direct_url,
        destfile = parquet_path,
        mode = "wb",
        quiet = TRUE
      )
    }, error = function(e) {
      bullets <- c(
        "Failed to download {file_name} from {repo} ({tag})",
        "i" = "Run {.code pb_download_opta()} to download all Opta data."
      )
      if (!is.null(pb_error_msg)) {
        bullets <- c(bullets, "x" = "piggyback: {pb_error_msg}")
      }
      bullets <- c(bullets, "x" = "Direct URL: {e$message}")
      cli::cli_abort(bullets)
    })
  }

  if (!file.exists(parquet_path)) {
    cli::cli_abort("Download failed - {file_name} not found after download")
  }

  if (isFALSE(validate_parquet_file(parquet_path))) {
    unlink(parquet_path)
    cli::cli_abort(c(
      "Downloaded {file_name} is corrupt (incomplete download).",
      "i" = "Please try again."
    ))
  }

  assign(cache_key, parquet_path, envir = .opta_remote_env)
  parquet_path
}


#' Clear remote Opta data cache
#'
#' Removes cached remote Opta data, forcing a fresh download on next access.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' clear_remote_opta_cache()
#' }
clear_remote_opta_cache <- function() {
  rm(list = ls(envir = .opta_remote_env), envir = .opta_remote_env)
  cli::cli_alert_success("Remote Opta cache cleared")
}


#' Download Opta Data to Local Directory
#'
#' Downloads consolidated Opta parquet files from GitHub releases to the local
#' pannadata/data/opta/ directory. Match events are excluded (too large for a
#' single file) - use `load_opta_match_events(source = "remote")` to load them
#' on-demand from per-league release files.
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
  # Note: match_events are per-league (too large for single file), downloaded separately
  files_to_download <- c(
    "opta_player_stats.parquet",
    "opta_shots.parquet",
    "opta_shot_events.parquet",
    "opta_events.parquet",
    "opta_lineups.parquet",
    "opta_fixtures.parquet",
    "opta_skills.parquet",
    "opta_match_stats.parquet",
    "opta-catalog.json"
  )

  download_success <- character(0)
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
      download_success <- c(download_success, f)
    }, error = function(e) {
      cli::cli_warn(c(
        "Failed to download {f} from {repo} ({tag})",
        "x" = e$message
      ))
    })
  }

  # Check for files that failed to download
  failed <- setdiff(files_to_download, download_success)
  if (length(failed) > 0) {
    stale <- failed[file.exists(file.path(opta_dir, failed))]
    missing <- setdiff(failed, stale)
    if (length(stale) > 0) {
      cli::cli_warn(c(
        "Using stale cached versions of {length(stale)} file(s) (download failed)",
        "i" = paste(stale, collapse = ", ")
      ))
    }
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "Failed to download {length(missing)} Opta file(s)",
        "x" = paste(missing, collapse = ", ")
      ))
    }
  }

  # Validate parquet files and error if any are corrupt
  parquet_files <- grep("\\.parquet$", download_success, value = TRUE)
  corrupt_files <- character(0)
  for (f in parquet_files) {
    fpath <- file.path(opta_dir, f)
    if (file.exists(fpath) && isFALSE(validate_parquet_file(fpath))) {
      cli::cli_warn("Downloaded {f} is corrupt (invalid parquet). Deleting.")
      unlink(fpath)
      corrupt_files <- c(corrupt_files, f)
    }
  }
  if (length(corrupt_files) > 0) {
    cli::cli_abort(c(
      "{length(corrupt_files)} downloaded file(s) were corrupt and deleted.",
      "x" = paste(corrupt_files, collapse = ", "),
      "i" = "Please re-run {.fn pb_download_opta} or check your network connection."
    ))
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
