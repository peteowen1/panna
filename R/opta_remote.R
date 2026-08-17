# Opta Remote Access
#
# The GitHub-release side: catalog download + TTL cache, league listing,
# and the remote parquet query engine (including its corrupt-download
# retry). Everything here talks to the network or the session cache;
# opta_loaders.R calls into it but does not implement it.
#
# Split out of opta_loaders.R (2026-08-17). Pure move.


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
                                       tag = "opta-latest",
                                       max_retries = 2L,
                                       retry_backoff_sec = 30) {

  # match_events are stored as per-league files (too large for single consolidated file)
  if (table_type == "match_events") {
    return(query_remote_opta_match_events(opta_league, season, columns,
                                           repo = repo, tag = tag))
  }

  # panna#157: a same-second daily dispatch can have another producer
  # mid-overwrite (plain piggyback delete-then-upload, non-atomic) on this
  # exact asset while we're downloading/querying it -- the corrupt-file
  # detection below already exists, but previously just aborted instead of
  # retrying. Bounded retry turns a same-run transient race into a ~30-90s
  # delay instead of a failed/degraded pipeline step; a non-corruption error
  # (missing file, bad SQL, network outage) still fails immediately.
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    out <- tryCatch(
      .query_remote_opta_parquet_once(table_type, opta_league, season, columns, repo, tag),
      error = function(e) e
    )
    if (!inherits(out, "error")) return(out)
    is_corruption <- grepl("corrupt|magic bytes", conditionMessage(out), ignore.case = TRUE)
    if (!is_corruption || attempt > max_retries) stop(out)
    cli::cli_alert_warning(paste0(
      "Retry {attempt}/{max_retries} for {table_type} after a corrupt/incomplete read ",
      "(likely a concurrent overwrite on {repo}@{tag}) -- waiting {retry_backoff_sec}s."
    ))
    Sys.sleep(retry_backoff_sec)
  }
}

#' @keywords internal
.query_remote_opta_parquet_once <- function(table_type, opta_league, season,
                                             columns, repo, tag) {
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
    # isTRUE/isFALSE, never a bare condition: validate_parquet_file() returns
    # NA when validation could not be PERFORMED (locked file, permission
    # denied), and `TRUE && NA` is NA -- a bare `if (NA)` aborts with "missing
    # value where TRUE/FALSE needed" instead of falling through to the
    # re-download this branch exists to provide. NA must also not delete the
    # file (see validate_parquet_file()'s contract), so only isFALSE unlinks.
    cache_valid <- validate_parquet_file(cached_path)
    if (isTRUE(cache_valid)) {
      parquet_path <- cached_path
    } else if (isFALSE(cache_valid) && file.exists(cached_path)) {
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
  col_sql <- .col_sql(columns)

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
  cli::cli_alert_info("Querying remote Opta {table_type} for {opta_league}...")

  result <- .with_duckdb(function(conn) {
    tryCatch({
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
    # isTRUE/isFALSE, never a bare condition -- see the note at the
    # consolidated-parquet cache check above (NA = "could not validate",
    # which must refetch without deleting, not abort).
    cache_valid <- validate_parquet_file(cached_path)
    if (isTRUE(cache_valid)) {
      parquet_path <- cached_path
    } else {
      if (isFALSE(cache_valid) && file.exists(cached_path)) {
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
  col_sql <- .col_sql(columns)

  # Build WHERE clause for season filter (using build_where_clause to prevent SQL injection)
  parquet_norm <- normalizePath(parquet_path, winslash = "/", mustWork = TRUE)
  where_sql <- build_where_clause(list(season = season), prefix = FALSE)
  if (nchar(where_sql) > 0) {
    sql <- sprintf("SELECT %s FROM '%s' WHERE %s", col_sql, parquet_norm, where_sql)
  } else {
    sql <- sprintf("SELECT %s FROM '%s'", col_sql, parquet_norm)
  }

  # Execute query with DuckDB
  cli::cli_alert_info("Querying match events for {opta_league}...")

  result <- .with_duckdb(function(conn) {
    tryCatch({
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
  })

  cli::cli_alert_success("Loaded {format(nrow(result), big.mark=',')} rows ({ncol(result)} columns)")
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
    # isTRUE/isFALSE, never a bare condition -- see the note at the
    # consolidated-parquet cache check above (NA = "could not validate",
    # which must refetch without deleting, not abort).
    cache_valid <- validate_parquet_file(cached_path)
    if (isTRUE(cache_valid)) {
      return(cached_path)
    } else if (isFALSE(cache_valid) && file.exists(cached_path)) {
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

