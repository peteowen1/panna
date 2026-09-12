# Opta Data Loader Functions
#
# The load_opta_*() family: reads a table for a league/season from local
# parquet or a GitHub release. Supporting concerns live alongside:
#   opta_paths.R    - directory/league/season resolution, SQL helpers
#   opta_coverage.R - event-less registry + coverage gate
#   opta_remote.R   - catalog, league listing, remote query engine
#
# League codes: EPL, La_Liga, Bundesliga, Serie_A, Ligue_1, Eredivisie,
#   Primeira_Liga, Super_Lig, Championship, Scottish_Premiership,
#   UCL, UEL, Conference_League, World_Cup, UEFA_Euros
# Seasons: 2013-2014 to 2025-2026

#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom duckdb duckdb
#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
NULL


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
#' @section The `xg` column is OPTA's, not ours:
#' `opta_shot_events.parquet` ships an `xg` column supplied by Opta. It is NOT
#' panna's model output and must never be used as such -- production always uses
#' our own xG (via SPADL and `predict_xg()`); Opta's is a benchmark only, for
#' confirming ours is better on the shots they scored.
#'
#' They are trivial to tell apart: Opta's is quantised to 3 decimal places (956
#' distinct values across 3.3M shots, penalties exactly 0.800), ours is
#' float-continuous with ~84% of values unique. Neither of our models reproduces
#' the stored column (correlations 0.964 / 0.967) because it was never ours.
#'
#' Measured head-to-head on 1,839,859 identical shots (2026-09-03, penalty
#' override applied): ours wins on logloss (0.2519 vs 0.2549), Opta marginally
#' better on bias (1.028 vs 1.033).
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
        seasons <- .with_duckdb(function(conn) {
          pq <- normalizePath(consolidated, winslash = "/", mustWork = TRUE)
          where_sql <- build_where_clause(list(competition = opta_league), prefix = FALSE)
          sql <- sprintf(
            "SELECT DISTINCT season FROM '%s' WHERE %s ORDER BY season DESC",
            pq, where_sql
          )
          res <- DBI::dbGetQuery(conn, sql)
          res$season
        })
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
        seasons <- .with_duckdb(function(conn) {
          pq <- normalizePath(per_league_file, winslash = "/", mustWork = TRUE)
          sql <- sprintf("SELECT DISTINCT season FROM '%s' ORDER BY season DESC", pq)
          res <- DBI::dbGetQuery(conn, sql)
          res$season
        })
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
    col_sql <- .col_sql(columns)

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

    col_sql <- .col_sql(columns)

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
    col_sql <- .col_sql(columns)
    sql <- sprintf("SELECT %s FROM read_parquet(%s, union_by_name=true)", col_sql, parquet_pattern)
  }

  # Execute query with DuckDB. The whole block is wrapped in one .with_duckdb()
  # call (rather than one per query) because the per-season fallback below
  # reuses the SAME connection for a second query.
  cli::cli_alert_info("Loading Opta {table_type} for {opta_league}...")

  result <- .with_duckdb(function(conn) {
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
        col_sql <- .col_sql(columns)
        sql2 <- sprintf("SELECT %s FROM read_parquet('%s', union_by_name=true)",
                         col_sql, per_path_q)
        result <- tryCatch(DBI::dbGetQuery(conn, sql2),
                            error = function(e) result)
      }
    }

    result
  })

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

  sql <- sprintf("SELECT * FROM '%s' LIMIT 0", parquet_path)
  result <- .with_duckdb(function(conn) DBI::dbGetQuery(conn, sql))

  names(result)
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

  col_sql <- .col_sql(columns)
  sql <- sprintf("SELECT %s FROM read_parquet(%s, union_by_name=true)", col_sql, parquet_pattern)

  cli::cli_alert_info("Loading Opta xmetrics for {opta_league}...")

  result <- .with_duckdb(function(conn) {
    tryCatch({
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
#'   and \code{fail_if_missing_frac} is \code{Inf}. NOTE: a data.table input
#'   is enriched \emph{by reference} (no defensive copy — the copy alone was
#'   ~6GB on 08b's table); always use the return value, and \code{copy()}
#'   first if you need the un-enriched original.
#' @family epv
#' @export
enrich_match_stats_with_xmetrics <- function(match_stats, verbose = TRUE,
                                              fail_if_missing_frac = Inf,
                                              source = c("local", "remote")) {
  source <- match.arg(source)
  # as.data.table() on an already-valid data.table is a FULL deep copy (the
  # panna#128 anti-pattern) — on 08b's ~6GB match_stats that copy alone was a
  # third of the GHA runner. Guard it; a data.table input is enriched BY
  # REFERENCE below (callers all reassign the return value, so this only
  # drops the wasted copy).
  if (!data.table::is.data.table(match_stats)) {
    match_stats <- data.table::as.data.table(match_stats)
  }

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
    # Select ONLY the join keys + mapped columns at the duckdb layer: the
    # consolidated bymatch parquet is ~3.3M rows x 70 columns, and the
    # full-width load (plus the as.data.table() deep copy that followed it)
    # was the transient that OOM-killed psr-weekly-snapshot on GHA on
    # 2026-07-08/15 -- right on top of the ~6GB match_stats already resident.
    # A narrow SELECT hard-fails on a vintage missing any named column
    # (duckdb binder error), so fall back to the old full-width load there.
    want <- c("player_id", "match_id", names(xm_map))
    xm <- tryCatch({
      x <- tryCatch(
        query_remote_opta_parquet("xmetrics_bymatch", opta_league = NULL,
                                  season = NULL, columns = want),
        error = function(e) {
          # Loud, not silent: if this fires every run (e.g. a renamed column
          # made the narrow select permanently fail), the OOM-safety
          # optimization has stopped working and logs must show it.
          cli::cli_warn("Narrow xMetrics select failed ({conditionMessage(e)}); falling back to the full-width load (memory-heavy legacy path).")
          query_remote_opta_parquet("xmetrics_bymatch", opta_league = NULL,
                                    season = NULL)
        })
      data.table::setDT(x)
      keep <- intersect(want, names(x))
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
  # Bounded-memory join instead of merge()/join-assign: merge() allocates a
  # full copy of match_stats (+6GB on 08b's table), and even a data.table
  # `X[i, (cols) := mget(...)]` join-assign transiently peaked at ~28.5GB on
  # these key cardinalities (measured 2026-07-18, unkeyed character bmerge
  # over 1.9M x 3.3M rows) — both OOM a 16GB GHA runner. A hash match() on
  # composite keys + per-column set() peaks at ~7GB for the same result;
  # unmatched rows get NA here and 0 in the fill loop below. \r cannot occur
  # in Opta alphanumeric ids, so the pasted key is collision-free.
  added_cols <- setdiff(names(xm), c("player_id", "match_id"))
  idx <- match(paste(match_stats$player_id, match_stats$match_id, sep = "\r"),
               paste(xm$player_id, xm$match_id, sep = "\r"))
  # Join-coverage tripwire (review catch): unlike merge(), match() cannot
  # hard-fail on key type/format drift — a structural divergence (e.g. after
  # a bymatch regen changes id types) yields all-NA idx, and the NA->0 fill
  # below would silently ship an xG-blind feature set that
  # fail_if_missing_frac (which only tracks missing FILES) cannot see.
  # Healthy coverage is ~98% (measured 2026-07-18).
  matched_frac <- if (length(idx)) mean(!is.na(idx)) else 0
  if (nrow(xm) > 0 && matched_frac == 0) {
    stop("xMetrics join matched 0 of ", length(idx), " match_stats rows despite ",
         nrow(xm), " xmetrics rows - player_id/match_id key mismatch ",
         "(type or format drift after a bymatch regen?)", call. = FALSE)
  }
  if (nrow(xm) > 0 && matched_frac < 0.5) {
    warning(sprintf(paste0(
      "xMetrics join matched only %.1f%% of match_stats rows (healthy is ~98%%) - ",
      "investigate key drift; unmatched rows get xMetrics = 0."),
      100 * matched_frac), call. = FALSE)
  }
  for (col in added_cols) {
    data.table::set(match_stats, j = col, value = xm[[col]][idx])
  }
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
  col_sql <- .col_sql(columns)

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

  result <- .with_duckdb(function(conn) {
    tryCatch({
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
  col_sql <- .col_sql(columns)

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

  cache_key <- paste0(file_name, "_", repo, "_", tag)

  result <- .with_duckdb(function(conn) {
    tryCatch({
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

  col_sql <- .col_sql(columns)

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

  cache_key <- paste0(file_name, "_", repo, "_", tag)

  result <- .with_duckdb(function(conn) {
    tryCatch({
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
  })

  snap_date <- if (nrow(result) > 0) as.character(result$snapshot_date[1]) else "none"
  cli::cli_alert_success(
    "Loaded {format(nrow(result), big.mark=',')} PSR ratings (snapshot: {snap_date})"
  )
  result
}

