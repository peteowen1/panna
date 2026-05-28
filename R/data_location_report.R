#' Report Where Local Opta Data Is Coming From
#'
#' Prints a diagnostic summary of how panna will resolve local data lookups:
#' which directory \code{pannadata_dir()} and \code{opta_data_dir()} point at,
#' which consolidated parquet files exist, how many distinct \code{(competition,
#' season)} pairs each contains, which per-season directories exist, and where
#' the consolidated parquets disagree with the per-season files.
#'
#' Use this whenever \code{load_opta_*()} returns nothing for data you believe
#' is on disk, or to confirm that a sync landed in the place panna actually
#' looks. The freshness skew between the consolidated parquet and per-season
#' files is the most common silent failure -- this surfaces it.
#'
#' @param leagues Optional character vector of Opta league codes (e.g.
#'   \code{c("World_Cup", "UEFA_Euros")}) to spot-check. Default \code{NULL}
#'   summarises all leagues found.
#'
#' @return Invisibly returns a list of the gathered facts so the report can
#'   also be consumed programmatically. The function's main value is the
#'   printed output.
#' @export
data_location_report <- function(leagues = NULL) {
  cli::cli_h1("panna data location report")

  # --- Path resolution -----------------------------------------------------
  cli::cli_h2("Directory resolution")
  pd <- tryCatch(pannadata_dir(), error = function(e) paste("ERR:", e$message))
  od <- tryCatch(opta_data_dir(), error = function(e) paste("ERR:", e$message))
  cli::cli_alert_info("getwd():       {getwd()}")
  cli::cli_alert_info("PANNADATA_DIR env: {Sys.getenv('PANNADATA_DIR', '<unset>')}")
  cli::cli_alert_info("pannadata_dir(): {pd}")
  cli::cli_alert_info("opta_data_dir(): {od}")

  if (startsWith(pd, tools::R_user_dir("panna", "data"))) {
    cli::cli_alert_warning(c(
      "pannadata_dir() fell through to R_user_dir() -- no `pannadata/data` ",
      "near getwd(). Set PANNADATA_DIR env var or pannadata_dir('path') ",
      "explicitly if this isn't intentional."
    ))
  }

  # --- Consolidated parquets ----------------------------------------------
  cli::cli_h2("Consolidated parquets in opta_data_dir()")
  cons_tables <- c("lineups", "fixtures", "player_stats", "events",
                    "shots", "shot_events", "match_xg", "match_stats")
  consolidated_summary <- list()
  for (tbl in cons_tables) {
    p <- file.path(od, sprintf("opta_%s.parquet", tbl))
    if (!file.exists(p)) {
      cli::cli_text("  {.val opta_{tbl}.parquet}: {.emph not present}")
      next
    }
    size_mb <- file.info(p)$size / 1024^2
    info <- tryCatch({
      conn <- DBI::dbConnect(duckdb::duckdb())
      path_q <- normalizePath(p, winslash = "/", mustWork = TRUE)
      pairs <- DBI::dbGetQuery(conn, sprintf(
        "SELECT competition, COUNT(DISTINCT season) AS n_seasons FROM '%s' GROUP BY competition ORDER BY competition",
        path_q
      ))
      DBI::dbDisconnect(conn, shutdown = TRUE)
      list(n_pairs = sum(pairs$n_seasons), pairs = pairs)
    }, error = function(e) list(n_pairs = NA, pairs = NULL,
                                  err = conditionMessage(e)))
    consolidated_summary[[tbl]] <- info
    cli::cli_text("  {.val opta_{tbl}.parquet}: {round(size_mb,1)} MB, {info$n_pairs %||% '?'} (comp,season) pairs")
  }

  # --- Per-season dir summary --------------------------------------------
  cli::cli_h2("Per-season directories in opta_data_dir()")
  per_dir_summary <- list()
  for (sub in c("player_stats", "lineups", "fixtures", "events")) {
    sub_path <- file.path(od, sub)
    if (!dir.exists(sub_path)) {
      cli::cli_text("  {.path {sub}/}: {.emph not present}")
      next
    }
    comps <- list.dirs(sub_path, recursive = FALSE, full.names = FALSE)
    files_per_comp <- vapply(comps, function(cmp) {
      length(list.files(file.path(sub_path, cmp), pattern = "\\.parquet$"))
    }, integer(1))
    per_dir_summary[[sub]] <- setNames(files_per_comp, comps)
    cli::cli_text("  {.path {sub}/}: {length(comps)} competitions, {sum(files_per_comp)} per-season files")
  }

  # --- Per-league spot check ---------------------------------------------
  if (is.null(leagues)) {
    # Default: all comps that appear in either source
    all_comps <- unique(c(
      unlist(lapply(per_dir_summary, names)),
      unlist(lapply(consolidated_summary, function(x) {
        if (!is.null(x$pairs)) x$pairs$competition else character(0)
      }))
    ))
    leagues <- sort(all_comps)
  }

  if (length(leagues) > 0L) {
    cli::cli_h2("Per-league seasons (consolidated vs per-season)")
    incons <- character(0)
    for (lg in leagues) {
      lines <- character(0)
      # Per-season seasons by table
      ps_seasons <- list()
      for (sub in names(per_dir_summary)) {
        league_dir <- file.path(od, sub, lg)
        if (dir.exists(league_dir)) {
          files <- list.files(league_dir, pattern = "\\.parquet$")
          ps_seasons[[sub]] <- tools::file_path_sans_ext(files)
        }
      }
      # Consolidated seasons (use lineups as canonical)
      cons_seasons <- if (file.exists(file.path(od, "opta_lineups.parquet"))) {
        tryCatch({
          conn <- DBI::dbConnect(duckdb::duckdb())
          rs <- DBI::dbGetQuery(conn, sprintf(
            "SELECT DISTINCT season FROM '%s' WHERE competition = '%s'",
            normalizePath(file.path(od, "opta_lineups.parquet"),
                           winslash = "/", mustWork = TRUE),
            lg
          ))
          DBI::dbDisconnect(conn, shutdown = TRUE)
          sort(as.character(rs$season))
        }, error = function(e) character(0))
      } else character(0)
      # Per-season union (canonical: lineups)
      ps_canonical <- sort(ps_seasons$lineups %||% character(0))

      only_cons <- setdiff(cons_seasons, ps_canonical)
      only_ps   <- setdiff(ps_canonical, cons_seasons)
      if (length(only_cons) > 0L || length(only_ps) > 0L) {
        incons <- c(incons, lg)
        cli::cli_text("  {.strong {lg}}:")
        if (length(only_cons) > 0L) {
          cli::cli_text("    only in consolidated lineups: {paste(only_cons, collapse=', ')}")
        }
        if (length(only_ps) > 0L) {
          cli::cli_text("    only in per-season lineups: {paste(only_ps, collapse=', ')}")
        }
      }
    }
    if (length(incons) == 0L) {
      cli::cli_alert_success("Consolidated and per-season agree for all {length(leagues)} leagues checked.")
    } else {
      cli::cli_alert_warning(
        "Consistency mismatch in {length(incons)} league{?s}: {paste(incons, collapse=', ')}. With the load_opta_table() fall-through, reads will still succeed for either source. Materialize per-season files with the consolidated parquet to fully sync."
      )
    }
  }

  invisible(list(
    pannadata_dir = pd,
    opta_data_dir = od,
    consolidated = consolidated_summary,
    per_season = per_dir_summary
  ))
}

# `%||%` is imported from rlang via panna-package.R (single source).
