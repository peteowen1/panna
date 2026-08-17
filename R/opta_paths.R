# Opta Data Location & Naming
#
# Resolving WHERE Opta data lives and WHAT a league/season is called:
# the data directory, league-code translation, season discovery, and the
# SQL/parquet safety helpers the loaders share. No table is read here --
# see opta_loaders.R for that.
#
# Split out of opta_loaders.R (2026-08-17), which had grown to 2,406 lines
# spanning four unrelated responsibilities. Pure move: no function bodies
# changed.


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


#' Build a SQL column-selection list
#'
#' Shared by every Opta loader: validated, comma-separated column list, or
#' "*" when no column subset was requested.
#'
#' @param columns Optional character vector of column names, or NULL.
#' @return SQL fragment (character scalar).
#' @keywords internal
#' @noRd
.col_sql <- function(columns) {
  if (!is.null(columns)) {
    paste(validate_sql_columns(columns), collapse = ", ")
  } else {
    "*"
  }
}


#' Run a function with a scratch DuckDB connection
#'
#' Opens an in-memory DuckDB connection, guarantees disconnect via
#' `on.exit()` scoped to THIS call (safe to invoke repeatedly inside a loop --
#' each call disconnects its own connection when it returns, rather than
#' deferring to the caller's enclosing function), and calls `fn(conn)`.
#'
#' @param fn Function taking one argument, the DuckDB connection.
#' @return The return value of `fn(conn)`.
#' @keywords internal
#' @noRd
.with_duckdb <- function(fn) {
  conn <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  fn(conn)
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
        # .with_duckdb()'s on.exit is scoped to its OWN call frame, so it
        # disconnects when this call returns -- not at this enclosing
        # function's return -- and is therefore safe to call once per loop
        # iteration without stacking up stale handles ("already closed"
        # warnings on function return, which a bare on.exit() here would hit).
        cons_seasons <- tryCatch({
          .with_duckdb(function(conn_ls) {
            path_q <- normalizePath(consolidated, winslash = "/", mustWork = TRUE)
            # build_where_clause() escapes the value (a competition label with
            # an apostrophe otherwise breaks the query); interpolating it
            # directly bypassed that.
            sql <- sprintf(
              "SELECT DISTINCT season FROM '%s' %s AND season IS NOT NULL",
              path_q,
              build_where_clause(list(competition = opta_league))
            )
            rs <- DBI::dbGetQuery(conn_ls, sql)
            as.character(rs$season)
          })
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

