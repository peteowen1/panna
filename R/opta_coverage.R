# Opta Event Coverage
#
# The event-less match registry and the coverage gate built on it.
# `check_events_coverage()` measures the gap between matches Opta has
# stats for and matches it has EVENTS for; the event-less registry is the
# set of matches that can never have events (Opta box-scores them only),
# and is subtracted from the denominator so a permanent gap isn't
# reported as a scrape failure.
#
# Split out of opta_loaders.R (2026-08-17). Pure move.


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

  # "Not built yet" and "built but broken" are different answers and must not
  # collapse into the same silent character(0). This registry is SUBTRACTED
  # from the coverage denominator, so an empty return means "no match is
  # event-less" -- which turns every genuinely-excluded match back into an
  # apparent coverage gap. Absence is normal (pre-first-rebuild) and stays
  # quiet; a file that exists and won't parse, or parses without `match_id`,
  # is a corrupt/truncated download or a schema change, and says so.
  reg <- tryCatch(
    as.data.frame(arrow::read_parquet(path)),
    error = function(e) {
      cli::cli_warn(c(
        "Event-less registry at {.path {path}} exists but could not be read:
         {conditionMessage(e)}",
        "!" = "Treating it as EMPTY -- event-less matches will be counted as
               coverage gaps until this is resolved.",
        "i" = "Usually a truncated download; delete the file and re-fetch."
      ))
      NULL
    }
  )
  if (is.null(reg)) return(character(0))
  if (!"match_id" %in% names(reg)) {
    cli::cli_warn(c(
      "Event-less registry at {.path {path}} has no {.field match_id} column
       (columns: {.val {names(reg)}}) -- schema change upstream?",
      "!" = "Treating it as EMPTY -- event-less matches will be counted as
             coverage gaps."
    ))
    return(character(0))
  }
  if (nrow(reg) == 0L) return(character(0))  # genuinely empty registry
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

