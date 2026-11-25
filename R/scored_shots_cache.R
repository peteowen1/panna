# Shared event -> scored-shots path for the EPV xMetrics job and Opta RAPM step 1.
#
# Both jobs used to convert every league-season's events to SPADL and score the
# shots on their own, every day (issue #505). They now share:
#   * score_spadl_shots()        - the ONE place penalties are flagged and shots
#                                  are scored (SPADL -> is_penalty -> xG).
#   * scored_shots_key()         - a fingerprint of everything the scored shots
#                                  depend on: the events, the shot lookup that
#                                  supplies body part / situation, the xG model
#                                  and the scoring-code version.
#   * write/read_scored_shots()  - a per-league-season table of scored shots,
#                                  written by the EPV job (which has to build
#                                  full SPADL anyway) and read by RAPM step 1.
#
# A reader NEVER trusts an entry on config alone (the trap behind review
# R060/R068/R076, see 01_load_opta_data.R): an entry is used only when its key
# equals the key recomputed from the events the reader has loaded right now.
# Anything else - missing file, missing league-season, changed events, changed
# xG model - is a miss, and the caller converts the events itself.

#' Version of the shot-scoring logic
#'
#' Bump when \code{score_spadl_shots()} (or the SPADL shot columns it keeps)
#' changes in a way that alters scored shots, so every cached entry misses.
#' @noRd
SCORED_SHOTS_VERSION <- 1L

# SPADL columns a scored-shots row keeps: everything extract_shots_from_spadl()
# reads, plus the ids needed to audit a row against the events.
.SCORED_SHOTS_COLS <- c("match_id", "original_event_id", "period_id",
                        "time_seconds", "team_id", "player_id", "player_name",
                        "action_type", "result", "xg", "is_penalty")

# Event columns that feed SPADL conversion and xG scoring. qualifier_json is
# included on purpose: it carries penalty (9) and direct-corner (263) tags.
.EVENT_FINGERPRINT_COLS <- c("match_id", "event_id", "type_id", "team_id",
                             "player_id", "player_name", "minute", "second",
                             "x", "y", "end_x", "end_y", "outcome", "period_id",
                             "qualifier_json")

# Reduce a data frame to plain, canonically ordered vectors, so the hash does
# not depend on the container (data.frame / tibble / data.table), the row
# order, row names, factor vs character or integer64 vs numeric id columns.
.canonical_columns <- function(df, cols) {
  df <- as.data.frame(df)
  cols <- intersect(cols, names(df))
  out <- lapply(cols, function(cc) {
    v <- df[[cc]]
    if (inherits(v, "integer64") || is.factor(v)) v <- as.character(v)
    v <- as.vector(v)
    # integer vs double must not change the hash (the same parquet column can
    # arrive as either through different loaders).
    if (!is.character(v)) v <- as.numeric(v)
    v
  })
  names(out) <- cols
  if (length(out) > 0 && length(out[[1]]) > 1) {
    ord <- do.call(order, c(unname(out), list(method = "radix", na.last = TRUE)))
    out <- lapply(out, function(v) v[ord])
  }
  out
}

#' Fingerprint an Opta events table
#'
#' Hashes the event columns that feed SPADL conversion and shot scoring. Same
#' events in any row order or container give the same value; a changed, added
#' or removed event gives a different one.
#'
#' @param events Data frame from \code{load_opta_match_events()} (one
#'   league-season).
#' @return A single hash string.
#' @keywords internal
#' @noRd
opta_events_fingerprint <- function(events) {
  rlang::hash(.canonical_columns(events, .EVENT_FINGERPRINT_COLS))
}

#' Fingerprint an xG model
#'
#' Hashes the fitted booster bytes and the feature metadata, so retraining or
#' republishing a model with different weights or features changes the value.
#'
#' @param xg_model Model from \code{load_xg_model()}.
#' @return A single hash string.
#' @keywords internal
#' @noRd
xg_model_fingerprint <- function(xg_model) {
  booster <- xg_model$model
  bytes <- if (!is.null(booster) && requireNamespace("xgboost", quietly = TRUE) &&
               inherits(booster, "xgb.Booster")) {
    tryCatch(xgboost::xgb.save.raw(booster), error = function(e) NULL)
  } else NULL
  # No saveable booster (a stub model in a test, an older object): hash the
  # whole object rather than silently returning a constant.
  rlang::hash(list(bytes = if (is.null(bytes)) xg_model else bytes,
                   meta = xg_model$panna_metadata))
}

#' Cache key for one league-season's scored shots
#'
#' @param events Match events for the league-season.
#' @param xg_model xG model object.
#' @param shot_lookup The \code{shot_lookup} given to \code{add_xg_to_spadl()}
#'   (body part and situation by event), or NULL.
#' @return A single hash string covering events, shot lookup, xG model and
#'   \code{SCORED_SHOTS_VERSION}.
#' @keywords internal
#' @noRd
scored_shots_key <- function(events, xg_model, shot_lookup = NULL) {
  lk <- if (is.null(shot_lookup)) NULL else
    .canonical_columns(shot_lookup, c("match_id", "event_id", "body_part", "situation"))
  rlang::hash(list(events = opta_events_fingerprint(events),
                   shot_lookup = lk,
                   xg_model = xg_model_fingerprint(xg_model),
                   version = SCORED_SHOTS_VERSION))
}

#' Flag penalties on SPADL and score its shots with xG
#'
#' The single scoring path shared by the EPV and RAPM jobs. Penalties come from
#' SPADL's own \code{is_penalty} (qualifier 9, anchored on the JSON key) and get
#' the fixed \code{PENALTY_XG}; every other shot gets the model's xG with body
#' part and situation joined from \code{shot_lookup}.
#'
#' @param spadl SPADL from \code{convert_opta_to_spadl()} / \code{get_or_build_spadl()}.
#' @param events The raw events the SPADL was built from.
#' @param xg_model xG model.
#' @param season Season label (some models are season-aware).
#' @param shot_lookup Body part / situation lookup, or NULL.
#' @return \code{spadl} with integer \code{is_penalty} and numeric \code{xg}.
#' @keywords internal
#' @noRd
score_spadl_shots <- function(spadl, events, xg_model, season, shot_lookup = NULL) {
  if ("is_penalty" %in% names(spadl)) {
    # %in% maps NA (and anything but 1/TRUE) to 0, so step 1's penalty sum and its
    # zero-penalty guard (panna#521) never see NA.
    spadl$is_penalty <- as.integer(spadl$is_penalty %in% c(1L, TRUE))
  } else {
    # A SPADL cache predating the is_penalty column: re-derive from the events
    # with the same anchored pattern (the unanchored one matched qualifier
    # VALUES such as "55":"9").
    cli::cli_alert_warning("spadl$is_penalty missing (stale cache?) - deriving from raw events")
    pen_evt <- events$type_id %in% c(13L, 14L, 15L, 16L) &
      grepl('[{,]"9":', events$qualifier_json)
    pen_keys <- paste(events$match_id[pen_evt], events$player_id[pen_evt],
                      events$minute[pen_evt], sep = "_")
    spadl$is_penalty <- 0L
    shot_idx <- which(spadl$action_type == "shot")
    if (length(pen_keys) > 0 && length(shot_idx) > 0) {
      spadl_keys <- paste(spadl$match_id[shot_idx], spadl$player_id[shot_idx],
                          floor(spadl$time_seconds[shot_idx] / 60), sep = "_")
      spadl$is_penalty[shot_idx[spadl_keys %in% pen_keys]] <- 1L
    }
  }

  spadl <- add_xg_to_spadl(spadl, xg_model, season = season,
                           shot_lookup = shot_lookup, events = events)

  pen_idx <- spadl$action_type == "shot" & spadl$is_penalty == 1L
  if (sum(pen_idx) > 0) {
    spadl$xg[pen_idx] <- PENALTY_XG
    cli::cli_alert_info("Set {sum(pen_idx)} penalties to xG={PENALTY_XG}")
  }
  spadl
}

#' Scored-shot rows for the shared cache
#'
#' @param spadl SPADL already passed through \code{score_spadl_shots()}.
#' @param league,season League-season the rows belong to.
#' @param key Value of \code{scored_shots_key()} for the events behind \code{spadl}.
#' @return Data frame of the shot rows (zero rows when there are no shots).
#' @keywords internal
#' @noRd
scored_shots_rows <- function(spadl, league, season, key) {
  shots <- spadl[spadl$action_type == "shot", intersect(.SCORED_SHOTS_COLS, names(spadl)),
                 drop = FALSE]
  shots <- as.data.frame(shots)
  n <- nrow(shots)
  shots$league <- rep(league, n)
  shots$season <- rep(season, n)
  shots$cache_key <- rep(key, n)
  shots
}

#' Write the scored-shots table
#'
#' @param rows Data frame, or list of data frames, from \code{scored_shots_rows()}.
#' @param path Parquet file to write.
#' @return \code{path}, invisibly.
#' @keywords internal
#' @noRd
write_scored_shots <- function(rows, path) {
  if (!is.data.frame(rows)) {
    rows <- data.table::rbindlist(Filter(function(x) !is.null(x) && nrow(x) > 0, rows),
                                  fill = TRUE, use.names = TRUE)
  }
  rows <- as.data.frame(rows)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(rows, path)
  invisible(path)
}

#' Read the scored-shots table
#'
#' @param path Parquet written by \code{write_scored_shots()}.
#' @return Data frame, or NULL when the file is absent or unreadable (a miss
#'   for every league-season, never an error).
#' @keywords internal
#' @noRd
read_scored_shots <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(as.data.frame(arrow::read_parquet(path)), error = function(e) {
    cli::cli_alert_warning("Scored-shots cache {.file {path}} unreadable ({conditionMessage(e)}); ignoring it")
    NULL
  })
}

#' Look up one league-season in the scored-shots table
#'
#' A hit requires the stored key to equal \code{key}, recomputed by the caller
#' from the events it has loaded now. Rows under any other key (stale events,
#' a different xG model, an older scoring version) are a miss.
#'
#' @param table Result of \code{read_scored_shots()}.
#' @param league,season League-season to look up.
#' @param key Value of \code{scored_shots_key()} for the caller's events.
#' @return Shot rows (SPADL-shaped, ready for \code{extract_shots_from_spadl()}),
#'   or NULL on a miss.
#' @keywords internal
#' @noRd
lookup_scored_shots <- function(table, league, season, key) {
  if (is.null(table) || nrow(table) == 0) return(NULL)
  sel <- table$league == league & table$season == season
  if (!any(sel)) return(NULL)
  hit <- table[sel, , drop = FALSE]
  if (!all(hit$cache_key == key)) return(NULL)
  hit[, setdiff(names(hit), c("league", "season", "cache_key")), drop = FALSE]
}
