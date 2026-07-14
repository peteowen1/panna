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
    NA  # Indeterminate -- callers should not delete
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
  BEL = "Belgian_First_Division",
  BRA = "Brazilian_Serie_A",
  AUS = "A_League",
  TUN = "Tunisian_Ligue_1",
  # Americas / Asia domestic
  MLS = "MLS",
  MEX = "Liga_MX",
  ARG = "Argentine_Liga_Profesional",
  SAU = "Saudi_League",
  # African club
  CAFCL = "CAF_CL",
  # European comps
  UCL = "UCL",
  UEL = "UEL",
  UECL = "Conference_League",
  # Americas / Asia club bridges (cross-league connectivity)
  LIB = "CONMEBOL_Libertadores",
  SUD = "CONMEBOL_Sudamericana",
  CCC = "Concacaf_Champions_Cup",
  LGC = "Leagues_Cup",
  ACLE = "AFC_Champions_League_Elite",
  CWC = "Club_World_Cup",
  # International -- tournaments
  WC = "World_Cup",
  EURO = "UEFA_Euros",
  AFCON = "AFCON",
  COPA = "Copa_America",
  GOLD = "CONCACAF_Gold_Cup",
  ACUP = "AFC_Asian_Cup",
  GULF = "Gulf_Cup_of_Nations",
  # International -- qualifiers + standing competitions
  # Without these, national-team Elo iteration only saw WC + Euros,
  # leaving teams like Norway (who topped UEFA WC Qualifying Group I)
  # stuck near the 1500 initial. Adding the qualifier comps lets Elo
  # accumulate proper national-team form across an entire qualifying
  # cycle (~30 matches per UEFA team in a typical cycle).
  WCQ_UEFA     = "UEFA_WC_Qualifiers",
  WCQ_CONMEBOL = "CONMEBOL_WC_Qualifiers",
  WCQ_CAF      = "CAF_WC_Qualifiers",
  WCQ_AFC      = "AFC_WC_Qualifiers",
  EUROQ        = "UEFA_Euro_Qualifiers",
  AFCONQ       = "AFCON_Qualifiers",
  ACUPQ        = "Asian_Cup_Qualifiers",
  NL           = "UEFA_Nations_League",
  INTL_FR      = "Intl_Friendlies"
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
#' @family cache management
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
#' @family competition metadata
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
    # Union seasons across every per-season data-type dir AND every top-level
    # consolidated parquet. Previously this only checked player_stats/ and
    # then fell through one dir at a time, returning on the first hit. That
    # was wrong twice over:
    #   * Newly-added intl comps (UEFA_WC_Qualifiers, NL, Intl_Friendlies)
    #     have lineups+fixtures+events on disk but no player_stats files --
    #     so a single-dir-first walk missed them.
    #   * After a release sync, the consolidated opta_*.parquet contain
    #     comp+season pairs that may NOT yet exist as per-season files
    #     locally (the release only ships consolidated). The single-dir
    #     walk never consulted the consolidated parquets, so freshly-synced
    #     tournaments (e.g. WC 2022 Qatar arriving in opta_lineups.parquet)
    #     were invisible to list_opta_seasons until manually materialized.
    # Union all sources so any single one suffices.
    base <- opta_data_dir()
    seasons <- character(0)

    candidate_dirs <- c("player_stats", "lineups", "fixtures", "events")
    for (sub in candidate_dirs) {
      league_dir <- file.path(base, sub, opta_league)
      if (dir.exists(league_dir)) {
        files <- list.files(league_dir, pattern = "\\.parquet$", full.names = FALSE)
        if (length(files) > 0) {
          seasons <- union(seasons, tools::file_path_sans_ext(files))
        }
      }
    }

    # Also query the consolidated parquets for season values not yet
    # materialized as per-season files (typical after `pb_download_opta()`
    # of just the consolidated parquet).
    for (tbl in c("lineups", "fixtures", "player_stats", "events")) {
      consolidated <- file.path(base, sprintf("opta_%s.parquet", tbl))
      if (file.exists(consolidated)) {
        # Explicit connect+disconnect inside the tryCatch -- no on.exit, since
        # on.exit registered inside tryCatch executes at the ENCLOSING function
        # scope, not at the tryCatch scope, so multiple loop iterations would
        # stack up disconnect calls against stale handles ("already closed"
        # warnings on function return).
        cons_seasons <- tryCatch({
          conn_ls <- DBI::dbConnect(duckdb::duckdb())
          path_q <- normalizePath(consolidated, winslash = "/", mustWork = TRUE)
          sql <- sprintf(
            "SELECT DISTINCT season FROM '%s' WHERE competition = '%s' AND season IS NOT NULL",
            path_q, opta_league
          )
          rs <- DBI::dbGetQuery(conn_ls, sql)
          DBI::dbDisconnect(conn_ls, shutdown = TRUE)
          as.character(rs$season)
        }, error = function(e) character(0))
        if (length(cons_seasons) > 0) {
          seasons <- union(seasons, cons_seasons)
        }
      }
    }

    if (length(seasons) > 0) {
      return(sort(seasons, decreasing = TRUE))
    }

    # Fall through to catalog if NO local source -- per-season or consolidated --
    # exposed any season for this league.
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
  ), class = "vb_error_absent")
}


#' Resolve a League-Season String for Blog-Style Pipelines
#'
#' Given a league and the "domestic" season a pipeline is iterating over,
#' returns the Opta season string to pass to \code{load_opta_match_events()}.
#' Continental club comps (UCL/UEL/UECL) share the "YYYY-YYYY" format with
#' domestic leagues and are returned as-is. International tournaments (WC,
#' EURO) use "YYYY Country" (or bare "YYYY" for pan-European EURO 2020);
#' this helper maps a tournament played in summer YYYY onto the domestic
#' season ending in YYYY (e.g. WC 2014 Brazil -> "2013-2014").
#'
#' Returns \code{NULL} when there is no tournament in the given year so
#' callers can skip gracefully.
#'
#' @param league League code (e.g. "ENG", "UCL", "WC", "EURO").
#' @param domestic_season Domestic season string, e.g. "2013-2014".
#' @param tournament_leagues Character vector of league codes that use
#'   "YYYY Country"-style season strings. Defaults to \code{c("WC", "EURO")}.
#'
#' @return Season string to pass to \code{load_opta_*()}, or \code{NULL} if
#'   no matching tournament exists for the given year.
#' @family competition metadata
#' @export
#' @examples
#' \dontrun{
#' resolve_league_season("ENG",  "2013-2014")  # -> "2013-2014"
#' resolve_league_season("UCL",  "2013-2014")  # -> "2013-2014"
#' resolve_league_season("WC",   "2013-2014")  # -> "2014 Brazil"
#' resolve_league_season("EURO", "2019-2020")  # -> "2020" (pan-European)
#' resolve_league_season("WC",   "2018-2019")  # -> NULL (no WC that year)
#' }
resolve_league_season <- function(league, domestic_season,
                                   tournament_leagues = c("WC", "EURO")) {
  if (!league %in% tournament_leagues) return(domestic_season)

  # Extract ending year from "YYYY-YYYY" format
  t_year <- suppressWarnings(as.integer(sub(".*-", "", domestic_season)))
  if (is.na(t_year)) return(NULL)

  avail <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
  # Year-prefix match FIRST — the calendar/main season for the year: "2021"
  # (calendar leagues), "2014 Brazil" / "2020" (tournaments). This is the season
  # that should own the slot.
  matching <- avail[grepl(paste0("^", t_year, "( |$)"), avail)]
  if (length(matching) > 0) return(matching[1])
  # Exact split-season label as FALLBACK — recovers "YYYY-YYYY" labels that have
  # no calendar equivalent (e.g. Argentine Superliga 2016-2017..2019-2020), WITHOUT
  # stealing the year-2021 slot from the calendar "2021" Primera (vs Superliga
  # "2020-2021"). Year-prefix matching alone misses these split labels entirely.
  if (domestic_season %in% avail) return(domestic_season)
  NULL
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
#' @family opta loaders
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
#' @family opta loaders
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
#'     \item type_id: 13=miss, 14=post, 15=saved, 16=goal
#'     \item body_part: Head, LeftFoot, RightFoot
#'     \item situation: OpenPlay, SetPiece, Corner, Penalty
#'     \item big_chance: TRUE if big chance
#'   }
#'
#' @family opta loaders
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
#' @family opta loaders
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
#' @family opta loaders
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
#' @family opta loaders
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
#' @family opta loaders
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


#' Load the event-less match_id registry
#'
#' Returns match_ids that Opta has player_stats for but provides NO event feed
#' for (e.g. cup qualifier rounds), as recorded by pannadata's
#' \code{rebuild_events.py} into \code{event_less_match_ids.parquet} on the
#' \code{opta-latest} release. \code{check_events_coverage()} subtracts these
#' from the expected-events denominator so genuinely event-less matches don't
#' register as a coverage shortfall (an unsatisfiable gate for the continental
#' cups). Degrades gracefully: if the registry asset/file is absent (it won't
#' exist until the first rebuild has run), returns \code{character(0)} and the
#' coverage check falls back to its stricter all-player_stats denominator.
#'
#' @param league panna league code (filtered to its Opta competition).
#' @param season Optional season label filter.
#' @param source "remote" (download from opta-latest) or "local".
#' @return Character vector of event-less match_ids (possibly empty).
#' @keywords internal
load_opta_eventless_ids <- function(league, season = NULL,
                                     source = c("remote", "local")) {
  source <- match.arg(source)
  if (!requireNamespace("arrow", quietly = TRUE)) return(character(0))
  opta_league <- to_opta_league(league)
  file_name   <- "event_less_match_ids.parquet"

  path <- NULL
  if (source == "local") {
    cand <- file.path(opta_data_dir(), file_name)
    if (file.exists(cand)) path <- cand
  } else {
    if (!requireNamespace("piggyback", quietly = TRUE)) return(character(0))
    cache_key <- "eventless_peteowen1/pannadata_opta-latest"
    if (exists(cache_key, envir = .opta_remote_env)) {
      cached <- get(cache_key, envir = .opta_remote_env)
      if (file.exists(cached)) path <- cached
    }
    if (is.null(path)) {
      temp_dir <- file.path(tempdir(), "opta_eventless")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      ok <- tryCatch({
        piggyback::pb_download(file = file_name, repo = "peteowen1/pannadata",
                               tag = "opta-latest", dest = temp_dir,
                               overwrite = TRUE)
        TRUE
      }, error = function(e) FALSE)
      cand <- file.path(temp_dir, file_name)
      if (isTRUE(ok) && file.exists(cand)) {
        path <- cand
        assign(cache_key, cand, envir = .opta_remote_env)
      }
    }
  }
  if (is.null(path)) return(character(0))  # registry not available yet

  reg <- tryCatch(as.data.frame(arrow::read_parquet(path)),
                  error = function(e) NULL)
  if (is.null(reg) || nrow(reg) == 0L || !"match_id" %in% names(reg)) {
    return(character(0))
  }
  # NA-safe filtering: `TRUE & NA` is NA, and `match_id[NA]` injects an NA into
  # the result — which would silently UNDER-subtract the event-less set and turn
  # a genuinely-excluded match back into a false coverage gap. Force NA -> FALSE.
  keep <- rep(TRUE, nrow(reg))
  if ("competition" %in% names(reg)) {
    keep <- keep & !is.na(reg$competition) & reg$competition == opta_league
  }
  if (!is.null(season) && "season" %in% names(reg)) {
    keep <- keep & !is.na(reg$season) & reg$season == season
  }
  unique(as.character(reg$match_id[keep]))
}


#' Check events_consolidated Coverage vs Played Fixtures
#'
#' Counts unique match_ids in \code{events_consolidated/events_<comp>.parquet}
#' (what the EPV pipeline reads) and compares to the number of played
#' fixtures from \code{opta_fixtures.parquet} (the canonical source of
#' truth for which matches actually occurred) for a given league-season.
#' Surfaces the gap as data so callers (step 10b in the predictions
#' pipeline) can refuse to silently ship game_logs that miss matches.
#'
#' Background: the events_consolidated build step in pannadata's daily
#' scraper occasionally produces a per-comp parquet that's short of the
#' actual match count — observed during the 2026-05-29 audit where
#' \code{events_Championship.parquet} on \code{opta-latest} had only
#' 265 of 557 played Championship 2025-2026 matches, causing the blog
#' Value tab to cap at GP=24 instead of 46. Without an explicit check,
#' step 10b silently produced game_logs covering only the events it
#' could see.
#'
#' @param league panna league code (e.g. "EPL", "ENG2", "TUR")
#' @param season Season string (e.g. "2025-2026")
#' @param source One of "remote" (default) or "local" — where to read from.
#'
#' The gap is measured against the EXPECTED-events universe, not raw played
#' fixtures: matches Opta actually covers (player_stats) minus those confirmed
#' event-less in the registry (\code{\link{load_opta_eventless_ids}}). This
#' stops the continental cups (whose played fixtures include qualifier rounds
#' Opta provides no event feed for) from tripping an unsatisfiable gate, while
#' still catching a genuine shortfall like the Championship case above.
#'
#' @param league panna league code (e.g. "EPL", "ENG2", "TUR")
#' @param season Season string (e.g. "2025-2026")
#' @param source One of "remote" (default) or "local" — where to read from.
#'
#' @return Invisibly: list with
#'   \itemize{
#'     \item \code{league}, \code{season}: identifiers
#'     \item \code{n_played}: distinct played fixtures (context)
#'     \item \code{n_player_stats}: distinct matches Opta covers (the universe)
#'     \item \code{n_eventless}: registry matches excluded (no Opta event feed)
#'     \item \code{n_expected}: \code{n_player_stats - n_eventless} — matches
#'       that should have events
#'     \item \code{n_events}: distinct match_ids in events_consolidated
#'     \item \code{gap}: expected matches missing from events
#'     \item \code{missing_match_ids}: vector of expected match_ids not in
#'       events (length == gap)
#'   }
#'
#' @family validation
#' @export
check_events_coverage <- function(league, season,
                                    source = c("remote", "local")) {
  source <- match.arg(source)

  # Narrow error catching: only swallow file-not-found / season-not-in-
  # catalog conditions (= legitimate "source not local yet"). Re-raise
  # corruption / DuckDB / network errors so the caller can distinguish
  # silent "data not here" from "data here but broken". Pre-fix, any
  # tryCatch error collapsed to n_events=0 which got reclassified by
  # assert_events_coverage() as source_missing and skipped — masking
  # corrupt-parquet failures as "no problem, lazy-load handles it."
  is_missing_source_err <- function(e) {
    # Prefer the typed condition class the loaders themselves now signal
    # (load_opta_table()'s "No data found for .../Opta data not found ..."
    # sites raise class vb_error_absent -- see R/versebus.R's error
    # taxonomy). Fall back to an ANCHORED message check only for older/
    # untyped callers -- panna H-GATE (2026-07-08 review): the previous
    # unanchored pattern ("not found|does not exist|...") also matched
    # DuckDB binder-error text (`column "x" does not exist`) and corrupt-
    # file IO errors, silently reclassifying REAL load failures as
    # source_missing and swallowing them into an empty data.frame.
    if (inherits(e, "vb_error_absent")) return(TRUE)
    if (any(class(e) %in% c("vb_error_transient", "vb_error_integrity", "vb_error_stale"))) {
      return(FALSE)
    }
    msg <- conditionMessage(e)
    grepl("^No data found for|^Opta data not found|not found in repo",
          msg, ignore.case = TRUE)
  }

  load_or_rethrow <- function(loader) {
    tryCatch(loader(), error = function(e) {
      if (is_missing_source_err(e)) {
        return(data.frame(match_id = character(0)))
      }
      # Re-raise so the caller knows this is a REAL load failure, not
      # just "file not local yet". assert_events_coverage() can classify
      # it as load_error vs the legitimate source_missing pattern.
      stop(e)
    })
  }

  fx <- load_or_rethrow(function()
    load_opta_fixtures(league, season = season, status = "Played",
                        source = source, columns = c("match_id")))
  ps <- load_or_rethrow(function()
    load_opta_stats(league, season = season,
                     source = source, columns = c("match_id")))
  ev <- load_or_rethrow(function()
    load_opta_match_events(league, season = season,
                            source = source, columns = c("match_id")))

  played_ids    <- unique(fx$match_id)
  ps_ids        <- unique(ps$match_id)
  event_ids     <- unique(ev$match_id)
  eventless_ids <- load_opta_eventless_ids(league, season = season, source = source)

  # The "should have events" universe is the matches Opta actually covers
  # (player_stats), minus those confirmed event-less (no Opta event feed —
  # e.g. cup qualifiers). This drops two classes of unsatisfiable matches that
  # a naive played-fixtures denominator wrongly counted as gaps: (a) played
  # fixtures Opta has no data for at all (absent from player_stats), and (b)
  # matches Opta has stats but no events for (the registry). Falls back to
  # played fixtures when player_stats isn't available (source not local yet).
  universe_ids <- if (length(ps_ids) > 0L) ps_ids else played_ids
  expected_ids <- setdiff(universe_ids, eventless_ids)
  missing      <- setdiff(expected_ids, event_ids)

  invisible(list(
    league         = league,
    season         = season,
    n_played       = length(played_ids),
    n_player_stats = length(ps_ids),
    n_eventless    = length(eventless_ids),
    n_expected     = length(expected_ids),
    n_events       = length(event_ids),
    gap            = length(missing),
    missing_match_ids = missing
  ))
}


#' Assert Events Coverage Across Multiple Leagues
#'
#' Runs \code{check_events_coverage()} for each (league, season) pair and
#' decides whether to proceed. Emits a per-league summary; aborts loudly
#' if any league's gap exceeds \code{abort_threshold}, otherwise emits
#' warnings for gaps above \code{warn_threshold}.
#'
#' Intended as a guard at the top of pipeline steps that consume events
#' (step 10b export_game_logs, step 10c export_equity). Catches the
#' "events_consolidated is short" pattern BEFORE producing incomplete
#' game_logs that get silently shipped to blog-latest.
#'
#' @param league_seasons Either a character vector of league codes (all
#'   checked against the same \code{season} argument) OR a list of
#'   \code{list(league=..., season=...)} pairs.
#' @param season Default season if \code{league_seasons} is a vector.
#' @param warn_threshold Per-league gap above which to warn. Default 5.
#' @param abort_threshold Per-league gap above which to abort. Default
#'   \code{Inf} (warn-only). Set to a numeric (e.g. 20) to make the
#'   pipeline refuse to continue.
#' @param source One of "remote" or "local".
#'
#' @return Invisibly: list with per-league reports + summary stats.
#' @family validation
#' @export
assert_events_coverage <- function(league_seasons, season = NULL,
                                     warn_threshold = 5L,
                                     abort_threshold = Inf,
                                     source = c("remote", "local")) {
  source <- match.arg(source)

  # Normalize input to list(list(league, season), ...)
  if (is.character(league_seasons)) {
    if (is.null(season)) {
      stop("`season` must be supplied when `league_seasons` is a character vector.")
    }
    ls_list <- lapply(league_seasons, function(lg) list(league = lg, season = season))
  } else {
    ls_list <- league_seasons
  }

  cli::cli_h2("Events coverage check ({length(ls_list)} league-seasons)")

  reports <- lapply(ls_list, function(p) {
    r <- check_events_coverage(p$league, p$season, source = source)
    # Classify each report:
    #   source_missing -- n_events == 0 AND the universe is non-empty: per-comp
    #     events file isn't local yet (typical on a fresh GHA runner). The
    #     downstream load_opta_match_events() will lazy-download via piggyback,
    #     so this is NOT a coverage shortfall — skip the abort check.
    #   partial_gap   -- gap > warn_threshold against the EXPECTED-events
    #     universe (player_stats minus the event-less registry). Catches a real
    #     shortfall (the 2026-05-29 Championship 265/557 case) without flagging
    #     the cup qualifiers Opta provides no event feed for.
    #   ok            -- gap <= warn_threshold.
    has_universe <- r$n_played > 0L || r$n_player_stats > 0L
    r$status <- if (r$n_events == 0L && has_universe) "source_missing"
                else if (r$gap > warn_threshold) "partial_gap"
                else "ok"
    elx <- if (r$n_eventless > 0L) sprintf("; %d event-less excluded", r$n_eventless) else ""
    if (r$status == "source_missing") {
      cli::cli_alert_info(
        "{r$league} {r$season}: source not local yet (lazy-loaded downstream)"
      )
    } else if (r$status == "partial_gap") {
      cli::cli_alert_warning(
        "{r$league} {r$season}: events cover {r$n_events} / {r$n_expected} expected matches (gap={r$gap}{elx})"
      )
    } else {
      cli::cli_alert_success(
        "{r$league} {r$season}: {r$n_events} / {r$n_expected} expected ({r$gap} gap{elx})"
      )
    }
    r
  })

  partial_gaps <- vapply(reports, function(r) {
    if (identical(r$status, "partial_gap")) r$gap else 0L
  }, integer(1))
  total_gap        <- sum(partial_gaps)
  max_gap          <- if (length(partial_gaps) > 0L) max(partial_gaps) else 0L
  n_source_missing <- sum(vapply(reports,
    function(r) identical(r$status, "source_missing"), logical(1)))

  cli::cli_text(
    "Partial gap across {length(ls_list) - n_source_missing} downloadable league(s): {total_gap} matches; worst single: {max_gap}{if (n_source_missing > 0L) sprintf(' (+ %d source-missing skipped)', n_source_missing) else ''}"
  )

  bad <- reports[partial_gaps > abort_threshold]
  if (length(bad) > 0L) {
    msgs <- vapply(bad, function(r) {
      sprintf("  %s %s: missing %d of %d expected (events %d; %d event-less excluded; e.g. %s)",
              r$league, r$season, r$gap, r$n_expected, r$n_events,
              r$n_eventless, paste(head(r$missing_match_ids, 3), collapse = ", "))
    }, character(1))
    cli::cli_abort(c(
      "Refusing to proceed: {length(bad)} league(s) exceed events-coverage abort threshold ({abort_threshold}):",
      stats::setNames(msgs, rep(" ", length(msgs))),
      "i" = "Backfill the affected comps with pannadata's rebuild-events.yml (records event-less matches to the registry); do NOT rely on force_rescrape."
    ))
  }

  invisible(list(reports = reports, total_gap = total_gap, max_gap = max_gap,
                  n_source_missing = n_source_missing))
}


#' Load All Opta Data for Big 5 Leagues
#'
#' Convenience function to load Opta stats for all Big 5 European leagues.
#'
#' @param season Optional season filter. If NULL, loads all available seasons.
#' @param columns Optional character vector of columns to select.
#' @param source Character. "remote" (default) or "local".
#'
#' @return Data frame with league column added.
#'
#' @family opta loaders
#' @export
#' @examples
#' \dontrun{
#' # Load all Big 5 data (warning: large!)
#' big5 <- load_opta_big5()
#'
#' # Load specific season across all leagues
#' big5_2122 <- load_opta_big5(season = "2021-2022")
#' }
load_opta_big5 <- function(season = NULL, columns = NULL,
                            source = c("remote", "local")) {
  source <- match.arg(source)
  leagues <- c("ENG", "ESP", "GER", "ITA", "FRA")

  error_msgs <- list()
  results <- lapply(leagues, function(lg) {
    tryCatch({
      df <- load_opta_stats(lg, season, columns, source = source)
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
    unique_errs <- unique(unlist(error_msgs))
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

  # Per-league fallback (events_consolidated/ layout -- see load_opta_table)
  if (length(seasons) == 0 && !is.null(base_dir) && table_type == "match_events") {
    per_league_file <- file.path(base_dir, "events_consolidated",
                                  paste0("events_", opta_league, ".parquet"))
    if (file.exists(per_league_file)) {
      tryCatch({
        conn <- DBI::dbConnect(duckdb::duckdb())
        on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
        pq <- normalizePath(per_league_file, winslash = "/", mustWork = TRUE)
        sql <- sprintf("SELECT DISTINCT season FROM '%s' ORDER BY season DESC", pq)
        res <- DBI::dbGetQuery(conn, sql)
        seasons <- res$season
      }, error = function(e) {
        cli::cli_warn("Could not query per-league parquet: {e$message}")
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

  # Pannadata's match_events are too large to consolidate into a single file --
  # they ship as per-league parquets at events_consolidated/events_<league>.parquet.
  # Try that path before falling back to the hierarchical layout.
  per_league_file <- if (table_type == "match_events") {
    file.path(base_dir, "events_consolidated",
              paste0("events_", opta_league, ".parquet"))
  } else {
    NULL
  }

  # Track which branch produced the SQL -- needed so that when the
  # consolidated parquet returns 0 rows for a (comp, season) pair, we can
  # transparently fall through to the per-season file if one exists. This
  # handles the freshness skew you get when only the consolidated parquet
  # was re-synced (or only per-season files were regenerated locally) and
  # the two sources disagree about which (comp, season) pairs are present.
  used_consolidated <- FALSE

  if (file.exists(consolidated_file)) {
    used_consolidated <- TRUE
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

    if (nzchar(where_sql)) {
      sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_path, where_sql)
    } else {
      sql <- sprintf("SELECT %s FROM '%s'", col_sql, parquet_path)
    }
  } else if (!is.null(per_league_file) && file.exists(per_league_file)) {
    # File is already per-league, so apply only the season filter (no competition).
    parquet_path <- normalizePath(per_league_file, winslash = "/", mustWork = TRUE)

    col_sql <- if (!is.null(columns)) {
      paste(validate_sql_columns(columns), collapse = ", ")
    } else {
      "*"
    }

    where_sql <- build_where_clause(list(season = season), prefix = FALSE)

    if (nzchar(where_sql)) {
      sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_path, where_sql)
    } else {
      sql <- sprintf("SELECT %s FROM '%s'", col_sql, parquet_path)
    }
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
          ), class = "vb_error_absent")
        } else {
          cli::cli_abort(c(
            "Opta data not found for {opta_league}.",
            "i" = "Run {.code pb_download_opta()} to download the latest data.",
            "i" = "Or use {.code source = 'remote'} to load directly from GitHub."
          ), class = "vb_error_absent")
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
        ), class = "vb_error_absent")
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
      ), class = "vb_error_integrity")
    }
    # Binder errors (e.g. `column "x" does not exist`) and other DuckDB
    # failures are REAL load failures, not "data not here yet" -- left
    # unclassed (not vb_error_absent) so they propagate as errors instead of
    # being reclassified as source_missing by callers like
    # check_events_coverage()'s is_missing_source_err() (panna H-GATE,
    # 2026-07-08 review).
    cli::cli_abort("DuckDB query failed: {e$message}")
  })

  # If season was requested but got 0 rows AND we read from the consolidated
  # parquet, try the per-season file before erroring. Consolidated parquets
  # can be stale relative to per-season files (e.g. after a partial sync) --
  # falling through lets recently-materialized per-season data work even when
  # the consolidated file hasn't been re-uploaded.
  if (nrow(result) == 0 && !is.null(season) && used_consolidated) {
    per_season_path <- file.path(base_dir, table_type, opta_league,
                                  paste0(season, ".parquet"))
    if (file.exists(per_season_path)) {
      cli::cli_alert_info(
        "Consolidated {table_type} has no rows for {opta_league} {.val {season}} -- falling through to per-season file."
      )
      per_path_q <- normalizePath(per_season_path, winslash = "/", mustWork = TRUE)
      col_sql <- if (!is.null(columns)) {
        paste(validate_sql_columns(columns), collapse = ", ")
      } else {
        "*"
      }
      sql2 <- sprintf("SELECT %s FROM read_parquet('%s', union_by_name=true)",
                       col_sql, per_path_q)
      result <- tryCatch(DBI::dbGetQuery(conn, sql2),
                          error = function(e) result)
    }
  }

  # If season was requested but got 0 rows (and either no per-season fallback
  # existed, or it also returned nothing), show available seasons.
  if (nrow(result) == 0 && !is.null(season)) {
    avail <- suggest_opta_seasons(opta_league, table_type, base_dir)
    msg <- "No data found for {opta_league} season {.val {season}}."
    hints <- character(0)
    if (length(avail) > 0) {
      hints <- c(hints, "i" = "Available seasons: {paste(avail, collapse = ', ')}")
    }
    hints <- c(hints, "i" = "Note: leagues use {.val 2024-2025} format, tournaments use {.val {c('2024', '2024 Germany')}} format.")
    cli::cli_abort(c(msg, hints), class = "vb_error_absent")
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
#' @keywords internal
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
#' then local file (with TTL freshness check), then downloading from
#' GitHub releases.
#'
#' @param repo GitHub repository (default: "peteowen1/pannadata").
#' @param tag Release tag (default: "opta-latest").
#' @param max_age_hours Freshness window for the local catalog cache.
#'   If the local file is older than this (mtime-based), it's treated as
#'   stale and re-downloaded. Default 6 hours, override globally via
#'   \code{options(panna.opta_catalog_ttl_hours = N)}. Set \code{Inf} to
#'   disable the TTL (legacy behavior -- trust local forever).
#'
#' @return List with catalog data (competitions, panna_aliases).
#' @keywords internal
download_opta_catalog <- function(repo = "peteowen1/pannadata",
                                   tag = "opta-latest",
                                   max_age_hours = getOption(
                                     "panna.opta_catalog_ttl_hours", 6
                                   )) {
  # 1. Session cache
  if (exists("opta_catalog", envir = .opta_remote_env)) {
    return(get("opta_catalog", envir = .opta_remote_env))
  }

  # 2. Local file (with TTL freshness check)
  local_path <- tryCatch(
    file.path(opta_data_dir(), "opta-catalog.json"),
    error = function(e) NULL
  )
  if (!is.null(local_path) && file.exists(local_path)) {
    # Freshness: skip local cache if older than max_age_hours. Prevents the
    # classic "daily scrape refreshed the remote catalog but local is from
    # two weeks ago" foot-gun -- pipelines silently miss newly-scraped seasons
    # (e.g. EURO 2020, WC 2022 Qatar) because they never re-downloaded.
    local_age_hours <- as.numeric(
      difftime(Sys.time(), file.info(local_path)$mtime, units = "hours")
    )
    if (is.finite(max_age_hours) && local_age_hours > max_age_hours) {
      cli::cli_alert_info(
        "Local Opta catalog is {round(local_age_hours, 1)}h old (TTL {max_age_hours}h). Refreshing from {repo}."
      )
      # Fall through to download.
    } else {
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

  # Persist the fresh catalog to opta_data_dir so subsequent R sessions pick
  # up the refreshed file (with updated mtime for TTL) instead of re-downloading.
  if (!is.null(local_path)) {
    tryCatch(
      file.copy(catalog_path, local_path, overwrite = TRUE),
      error = function(e) {
        cli::cli_alert_info(
          "Could not persist catalog to {.path {local_path}}: {e$message}"
        )
      }
    )
  }

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
#' @family competition metadata
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
      comp <- comps[[code]]
      data.frame(
        code = code,
        name = comp$name %||% code,
        country = comp$country %||% "Unknown",
        type = comp$type %||% "unknown",
        tier = as.integer(comp$tier %||% 99L),
        n_seasons = length(comp$seasons),
        n_matches = as.integer(comp$n_matches %||% 0L),
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
#' Remote mode downloads a consolidated file from GitHub Releases (opta-latest).
#' Local mode reads pipeline-generated per-league/season files from disk.
#'
#' @param league League code (e.g., "ENG", "EPL").
#' @param season Optional season filter (e.g., "2024-2025").
#' @param columns Optional character vector of columns to select.
#' @param source Data source: "remote" (default, from GitHub Releases) or
#'   "local" (pipeline-generated files).
#' @param by_match Logical. If \code{TRUE}, load the per-player-match artifact
#'   (\code{xmetrics_bymatch/}, one row per player-match keyed by
#'   \code{match_id}) instead of the season-level aggregate. Default
#'   \code{FALSE}.
#'
#' @return Data frame with player xmetrics including xg, npxg, xa, xpass stats.
#'
#' @family opta loaders
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
                                source = c("remote", "local"),
                                by_match = FALSE) {
  source <- match.arg(source)
  opta_league <- to_opta_league(league)

  # by_match selects the per-player-match artifact (one row per player-match,
  # keyed by match_id) instead of the season-level aggregate. Used by the skills
  # pipeline's xG join; produced alongside the season file by 03.
  subdir <- if (by_match) "xmetrics_bymatch" else "xmetrics"

  # Remote: use consolidated parquet from GitHub Releases
  if (source == "remote") {
    return(query_remote_opta_parquet(subdir, opta_league, season,
                                      columns = columns))
  }

  # Local: pipeline-generated per-league/season files
  xmetrics_dir <- file.path(opta_data_dir(), subdir, opta_league)

  if (!is.null(season)) {
    parquet_path <- file.path(xmetrics_dir, paste0(season, ".parquet"))
    if (!file.exists(parquet_path)) {
      cli::cli_abort(c(
        "Opta xmetrics not found for {opta_league} {season}.",
        "i" = "Try {.code source = 'remote'} or run the 03_calculate_player_xmetrics.R pipeline."
      ))
    }
    parquet_pattern <- sprintf("'%s'", normalizePath(parquet_path, winslash = "/", mustWork = TRUE))
  } else {
    if (!dir.exists(xmetrics_dir)) {
      cli::cli_abort(c(
        "Opta xmetrics directory not found for {opta_league}.",
        "i" = "Try {.code source = 'remote'} or run the 03_calculate_player_xmetrics.R pipeline."
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


#' Enrich per-match stats with per-match xMetrics (xG + finishing/keeper value)
#'
#' Left-joins per-player-match xG and the redesign's over-performance features
#' (npg/ibox/obox \code{g_minus_xg}, \code{placement_added}, keeper \code{gsaa},
#' plus \code{xg_per90}/\code{npxg_per90}/xA/xPass) onto a box-score
#' \code{match_stats} table by \code{(player_id, match_id)}, sourcing the
#' per-match \code{xmetrics_bymatch/} artifact via
#' \code{\link{load_opta_xmetrics}(by_match = TRUE)}. Shared by the skills
#' estimation (step 2) and the PSR/PSV coefficient training (step 7) so both
#' see the identical feature set (avoids the train/serve drift of a duplicated
#' inline join).
#'
#' @param match_stats data.table/data.frame with \code{league}, \code{season},
#'   \code{match_id}, \code{player_id}.
#' @param verbose Print join diagnostics. Default \code{TRUE}.
#' @param fail_if_missing_frac Numeric in \code{[0, 1]}. If the fraction of
#'   league-seasons whose \code{xmetrics_bymatch/} file fails to load exceeds
#'   this, \code{stop()} instead of silently training on a partly-xG-blind
#'   dataset. Default \code{Inf} (library-safe: never fails). Pipeline callers
#'   that require the features (skills step 2, PSR step 7) should pass a finite
#'   value (e.g. \code{0.5}). A total miss (no files at all) always errors when
#'   this is finite, regardless of the fraction.
#' @param source Where to load \code{xmetrics_bymatch} from: \code{"local"}
#'   (default, pipeline-generated per-league/season files under
#'   \code{opta_data_dir()}) or \code{"remote"} (the consolidated
#'   \code{opta_xmetrics_bymatch.parquet} on the \code{opta-latest} release —
#'   the only option that works on a GHA runner, which never has the local
#'   per-league/season tree; see \code{\link{load_opta_xmetrics}}).
#'
#' @return \code{match_stats} (as data.table) with the xMetrics columns added
#'   (NA-filled to 0 for player-matches with no shots). Returns input unchanged
#'   (with a warning) if key columns are missing or no bymatch files are found
#'   and \code{fail_if_missing_frac} is \code{Inf}.
#' @family epv
#' @export
enrich_match_stats_with_xmetrics <- function(match_stats, verbose = TRUE,
                                              fail_if_missing_frac = Inf,
                                              source = c("local", "remote")) {
  source <- match.arg(source)
  match_stats <- data.table::as.data.table(match_stats)

  # source xmetrics column -> match_stats column (suffix where names collide)
  xm_map <- c(
    xg_per90 = "xg_per90", npxg_per90 = "npxg_per90",
    xa_per90 = "xa_per90_xmetrics",
    xpass_overperformance_per90 = "xpass_overperformance_per90_xmetrics",
    npg_minus_npxg_per90 = "npg_minus_npxg_per90",
    ibox_g_minus_xg_per90 = "ibox_g_minus_xg_per90",
    obox_g_minus_xg_per90 = "obox_g_minus_xg_per90",
    placement_added_per90 = "placement_added_per90",
    gsaa_per90 = "gsaa_per90",
    # Above-expected physical-duel counts (xDuel, 5 contests) — replace *_success ratios
    aerial_woe_per90 = "aerial_woe_per90",
    aerial_poss_woe_per90 = "aerial_poss_woe_per90",
    takeon_woe_per90 = "takeon_woe_per90",
    tackle_poss_woe_per90 = "tackle_poss_woe_per90",
    containment_woe_per90 = "containment_woe_per90"
  )

  if (!all(c("league", "season", "match_id", "player_id") %in% names(match_stats))) {
    warning("match_stats missing league/season/match_id/player_id — skipping xMetrics join",
            call. = FALSE)
    return(match_stats)
  }

  ls_pairs <- unique(match_stats[, .(league, season)])

  if (source == "remote") {
    # opta_xmetrics_bymatch.parquet is ONE consolidated file on opta-latest —
    # querying it once per (league, season) pair (the local-mode loop below)
    # means N rounds of DuckDB connect/query/disconnect against the SAME
    # cached file. On a real weekly match_stats table (~300 league-season
    # pairs) this left several GB of RSS the GHA runner never reclaimed —
    # gc()-invisible memory that OOM-killed the run well before its own
    # heavy lifting even started (panna#128). One query instead.
    if (verbose) cat("  Fetching consolidated xMetrics (remote, one query)...\n")
    xm <- tryCatch({
      x <- data.table::as.data.table(
        query_remote_opta_parquet("xmetrics_bymatch", opta_league = NULL, season = NULL))
      keep <- intersect(c("player_id", "match_id", names(xm_map)), names(x))
      x[, ..keep]
    }, error = function(e) NULL)
    if (is.null(xm)) {
      xm <- data.table::data.table()
      n_missing <- nrow(ls_pairs)
    } else {
      n_missing <- 0L
    }
  } else {
    if (verbose) cat(sprintf("  Joining per-match xMetrics over %d league-seasons...\n",
                             nrow(ls_pairs)))
    xm_list <- vector("list", nrow(ls_pairs))
    n_missing <- 0L
    for (i in seq_len(nrow(ls_pairs))) {
      lg <- ls_pairs$league[i]; sn <- ls_pairs$season[i]
      xm_list[[i]] <- tryCatch({
        x <- data.table::as.data.table(
          load_opta_xmetrics(lg, season = sn, source = source, by_match = TRUE))
        keep <- intersect(c("player_id", "match_id", names(xm_map)), names(x))
        x[, ..keep]
      }, error = function(e) { n_missing <<- n_missing + 1L; NULL })
    }
    xm <- data.table::rbindlist(Filter(Negate(is.null), xm_list), fill = TRUE)
  }

  miss_frac <- n_missing / nrow(ls_pairs)

  if (nrow(xm) == 0) {
    msg <- if (source == "remote") {
      sprintf(paste0(
        "No per-match xMetrics found (opta_xmetrics_bymatch.parquet missing/empty for all ",
        "%d league-seasons on opta-latest). Re-run data-raw/epv/04b_export_xmetrics_bymatch.R."),
        nrow(ls_pairs))
    } else {
      sprintf(paste0(
        "No per-match xMetrics found (xmetrics_bymatch/ absent for all %d league-seasons). ",
        "Re-run data-raw/epv/03_calculate_player_xmetrics.R."), nrow(ls_pairs))
    }
    # A total miss is fatal whenever the caller demanded the features (finite
    # fail_if_missing_frac) — training xG-blind silently is the bug this exists
    # to prevent. Library-default (Inf) warns and proceeds.
    if (is.finite(fail_if_missing_frac)) {
      stop(msg, " Refusing to proceed (fail_if_missing_frac is set).", call. = FALSE)
    }
    warning(msg, " Proceeding WITHOUT xG features.", call. = FALSE)
    return(match_stats)
  }

  # Partial gap: a `cat` would vanish under verbose=FALSE / in pipeline logs, and
  # NA->0-filling unmatched rows makes "file missing" indistinguishable from
  # "player took no shots". Surface it as a warning, and fail when it's too wide.
  if (n_missing > 0L) {
    gapmsg <- sprintf("xMetrics bymatch missing for %d/%d league-seasons (%.0f%%).",
                      n_missing, nrow(ls_pairs), 100 * miss_frac)
    if (miss_frac > fail_if_missing_frac) {
      stop(gapmsg, " Exceeds fail_if_missing_frac=", fail_if_missing_frac,
           "; re-run 03_calculate_player_xmetrics.R.", call. = FALSE)
    }
    warning(gapmsg, " Affected player-matches get xMetrics = 0.", call. = FALSE)
  }

  old <- intersect(names(xm_map), names(xm))
  data.table::setnames(xm, old, unname(xm_map[old]))
  xm <- unique(xm, by = c("player_id", "match_id"))
  match_stats <- merge(match_stats, xm, by = c("player_id", "match_id"), all.x = TRUE)
  added <- intersect(unname(xm_map), names(match_stats))
  for (col in added) {
    data.table::set(match_stats, which(is.na(match_stats[[col]])), col, 0)
  }
  if (verbose) cat(sprintf("  xMetrics joined: %d cols (%s); %d/%d league-seasons missing bymatch\n",
                           length(added), paste(added, collapse = ", "), n_missing, nrow(ls_pairs)))
  match_stats
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
#' @family opta loaders
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
#' @family opta loaders
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


#' Load pre-computed weekly PSR snapshots
#'
#' Downloads and queries \code{opta_psr_weekly.parquet} from the
#' \code{opta-latest} GitHub release. Contains PSR/OSR/DSR for every player
#' at weekly (last 2 years) or monthly (older) snapshot dates.
#'
#' @param date Optional date filter. If provided, returns the snapshot for the
#'   nearest weekly date at or before this date. Accepts a \code{Date} or a
#'   character string parseable by \code{as.Date()}.
#' @param columns Optional character vector of columns to select.
#' @param source Data source: \code{"remote"} (default) or \code{"local"}.
#' @param repo GitHub repository (default: "peteowen1/pannadata").
#' @param tag Release tag (default: "opta-latest").
#'
#' @return Data frame with columns: \code{snapshot_date}, \code{player_id},
#'   \code{player_name}, \code{primary_position}, \code{psr}, \code{osr},
#'   \code{dsr}, \code{weighted_90s}.
#'
#' @family psr
#' @export
#' @examples
#' \dontrun{
#' # Latest snapshot
#' psr <- load_opta_psr_weekly()
#'
#' # Snapshot nearest to a specific date
#' psr <- load_opta_psr_weekly(date = "2026-03-18")
#' }
load_opta_psr_weekly <- function(date = NULL, columns = NULL,
                                  source = c("remote", "local"),
                                  repo = "peteowen1/pannadata",
                                  tag = "opta-latest") {
  source <- match.arg(source)
  file_name <- "opta_psr_weekly.parquet"

  parquet_path <- download_opta_release_file(
    file_name, source = source, repo = repo, tag = tag
  )

  col_sql <- if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }

  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)

  # Build date filter: latest snapshot_date <= requested date
  where_sql <- if (!is.null(date)) {
    d <- as.Date(date)
    # Find the max snapshot_date that is <= d, then return all rows for that date
    sprintf("snapshot_date = (SELECT MAX(snapshot_date) FROM '%s' WHERE snapshot_date <= DATE '%s')",
            parquet_norm, format(d, "%Y-%m-%d"))
  } else {
    # Default: latest snapshot date
    sprintf("snapshot_date = (SELECT MAX(snapshot_date) FROM '%s')", parquet_norm)
  }

  sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)

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

  snap_date <- if (nrow(result) > 0) as.character(result$snapshot_date[1]) else "none"
  cli::cli_alert_success(
    "Loaded {format(nrow(result), big.mark=',')} PSR ratings (snapshot: {snap_date})"
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
