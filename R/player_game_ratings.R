# Unified Player Game Ratings
# ============================
# One row per player per match with all value metrics: EPV, WPA, PSV/OSV/DSV,
# and a combined "panna value" rating.
#
# Mirrors torpverse R/player_game_ratings.R but adapted for soccer's metric set.


#' Build unified player game ratings
#'
#' Merges per-game EPV, WPA, and PSV into a single data.table with one row
#' per player per match. Computes a combined \code{piero_value} blending
#' EPV and PSV contributions.
#'
#' @param player_game_epv Per-game EPV from \code{\link{aggregate_player_game_epv}}.
#' @param player_game_wpa Per-game WPA from \code{\link{aggregate_player_game_wpa}}.
#'   Optional; WPA columns are NA if not provided.
#' @param player_game_psv Per-game PSV from \code{\link{calculate_psv_components}}.
#'   Optional; PSV columns are NA if not provided.
#' @param epv_weight Weight for EPV in combined piero_value (default
#'   \code{PANNA_EPR_WEIGHT}).
#' @param psv_weight Weight for PSV in combined piero_value (default
#'   \code{PANNA_PSR_WEIGHT}).
#'
#' @return A data.table with one row per player per match:
#'   \describe{
#'     \item{player_id, player_name, team_id, match_id}{Identifiers}
#'     \item{minutes_played, position}{From lineups (if available)}
#'     \item{epv_total, epv_offensive, epv_defensive}{EPV components (raw)}
#'     \item{epv_total_adj, epv_offensive_adj, epv_defensive_adj}{EPV
#'       components after position centering. \code{epv_total_adj} also
#'       includes the opponent-strength adjustment.}
#'     \item{opp_adj}{Minutes-weighted opponent-strength adjustment (additive).}
#'     \item{epv_p90}{EPV per 90 minutes}
#'     \item{wpa_total, wpa_as_actor, wpa_as_receiver}{WPA components}
#'     \item{wpa_p90}{WPA per 90 minutes}
#'     \item{psv, osv, dsv}{Player Stat Value with O/D decomposition}
#'     \item{piero_value}{Combined: \code{epv_weight * epv_total_adj +
#'       psv_weight * psv} (falls back to \code{epv_total} if no adj columns).}
#'     \item{piero_value_p90}{Combined per 90 minutes}
#'   }
#'
#' @export
build_player_game_ratings <- function(player_game_epv,
                                       player_game_wpa = NULL,
                                       player_game_psv = NULL,
                                       epv_weight = PANNA_EPR_WEIGHT,
                                       psv_weight = PANNA_PSR_WEIGHT) {
  result <- data.table::as.data.table(player_game_epv)

  # Select core EPV columns. Adjusted variants (position + opponent) are
  # carried through when present so piero_value can prefer them over raw EPV.
  epv_cols <- intersect(
    c("player_id", "player_name", "team_id", "match_id",
      "minutes_played", "position", "n_actions",
      "epv_total", "epv_offensive", "epv_defensive",
      "epv_total_adj", "epv_offensive_adj", "epv_defensive_adj", "opp_adj",
      "epv_passing", "epv_shooting", "epv_dribbling", "epv_aerial",
      "epv_keeping", "epv_defending",
      "epv_as_actor", "epv_as_receiver", "epv_duel_blame",
      "epv_total_p90", "epv_offensive_p90", "epv_defensive_p90",
      "epv_adj"),
    names(result)
  )
  result <- result[, ..epv_cols]

  # Rename epv_total_p90 to epv_p90 for brevity
  if ("epv_total_p90" %in% names(result)) {
    data.table::setnames(result, "epv_total_p90", "epv_p90")
  }

  # --- Merge WPA ---
  if (!is.null(player_game_wpa)) {
    wpa <- data.table::as.data.table(player_game_wpa)
    wpa_cols <- intersect(
      c("player_id", "match_id", "wpa_total", "wpa_as_actor",
        "wpa_as_receiver", "max_wpa",
        "wpa_total_p90", "wpa_as_actor_p90", "wpa_as_receiver_p90",
        "wpa_adj"),
      names(wpa)
    )
    wpa <- wpa[, ..wpa_cols]
    if ("wpa_total_p90" %in% names(wpa)) {
      data.table::setnames(wpa, "wpa_total_p90", "wpa_p90")
    }
    result <- merge(result, wpa, by = c("player_id", "match_id"),
                     all.x = TRUE)
  }

  # --- Merge PSV ---
  if (!is.null(player_game_psv)) {
    psv <- data.table::as.data.table(player_game_psv)
    psv_cols <- intersect(
      c("player_id", "match_id", "psv", "psv_raw", "osv", "dsv"),
      names(psv)
    )
    psv <- psv[, ..psv_cols]
    result <- merge(result, psv, by = c("player_id", "match_id"),
                     all.x = TRUE)
  }

  # --- Combined piero_value ---
  # Prefer fully-adjusted EPV (position-centered + opponent-adjusted) when
  # available. Falls back to raw epv_total if adjustments weren't applied
  # upstream (e.g. missing position or match_date).
  epv_val <- if ("epv_total_adj" %in% names(result)) {
    data.table::fifelse(is.na(result$epv_total_adj), 0, result$epv_total_adj)
  } else if ("epv_total" %in% names(result)) {
    result$epv_total
  } else 0
  psv_val <- if ("psv" %in% names(result)) {
    data.table::fifelse(is.na(result$psv), 0, result$psv)
  } else 0

  result[, piero_value := epv_weight * epv_val + psv_weight * psv_val]

  # Per-90 piero_value
  if ("minutes_played" %in% names(result)) {
    mins_safe <- pmax(result$minutes_played, 1, na.rm = TRUE)
    result[, piero_value_p90 := piero_value / (mins_safe / 90)]
  }

  # Track which metrics were computed (before NA fill masks the difference)
  result[, has_wpa := !is.null(player_game_wpa) && "wpa_total" %in% names(result)]
  result[, has_psv := !is.null(player_game_psv) && "psv" %in% names(result)]

  # Fill NAs in numeric columns
  num_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  for (col in num_cols) {
    data.table::set(result, which(is.na(result[[col]])), col, 0)
  }

  data.table::setorder(result, match_id, -piero_value)
  result[]
}


#' Aggregate player game ratings to season level
#'
#' Summarizes per-game ratings to one row per player per season, with
#' total and per-90 averages for all value metrics.
#'
#' @param game_ratings Output of \code{\link{build_player_game_ratings}}.
#' @param season_col Column name containing season identifier (default
#'   \code{"season"}). If not present, all data is treated as one season.
#'
#' @return A data.table with one row per player (per season), containing
#'   summed totals and minutes-weighted per-90 averages.
#'
#' @export
aggregate_season_ratings <- function(game_ratings, season_col = "season") {

  dt <- data.table::as.data.table(game_ratings)

  group_cols <- "player_id"
  if (season_col %in% names(dt)) group_cols <- c(group_cols, season_col)

  # Value metric columns to aggregate
  value_cols <- intersect(
    c("epv_total", "epv_offensive", "epv_defensive",
      "wpa_total", "psv", "osv", "dsv", "piero_value"),
    names(dt)
  )

  # Aggregate
  result <- dt[, c(
    list(
      player_name = player_name[1],
      n_games = .N,
      total_minutes = sum(minutes_played, na.rm = TRUE)
    ),
    lapply(.SD, sum, na.rm = TRUE)
  ), by = group_cols, .SDcols = value_cols]

  # Per-90 rates
  mins_safe <- pmax(result$total_minutes, 1)
  for (col in value_cols) {
    p90_col <- paste0(col, "_p90")
    data.table::set(result, j = p90_col,
                     value = result[[col]] / (mins_safe / 90))
  }

  data.table::setorder(result, -piero_value)
  result[]
}


# ============================================================================
# Player Value Profile
# ============================================================================

#' Get a player's value metrics profile
#'
#' Returns a summary of all value metrics for a player: EPR (from per-game
#' EPV), PSR (from estimated skills), per-game EPV/WPA/PSV averages, and
#' the combined panna value. Loads from cached pipeline output.
#'
#' @param player Character string -- player name (partial match,
#'   case-insensitive). E.g., \code{"Salah"}, \code{"H. Kane"}.
#' @param season Season filter (e.g., \code{"2024-2025"}). If NULL, uses
#'   the most recent available season.
#' @param source Data source: \code{"local"} (default, pipeline caches) or
#'   \code{"remote"} (GitHub Releases).
#'
#' @return A list with:
#'   \describe{
#'     \item{player_name}{Matched player name}
#'     \item{summary}{One-row data.table with season totals and per-90 rates}
#'     \item{game_log}{Per-game data.table (if available) with EPV/WPA/PSV
#'       per match}
#'     \item{ratings}{Named list: epr, psr, piero_value}
#'   }
#'
#' @export
player_value <- function(player = NULL, season = NULL,
                          source = c("local", "remote")) {
  source <- match.arg(source)
  target_player <- player

  if (is.null(target_player)) {
    cli::cli_abort("Provide a player name, e.g. {.code player_value(\"Salah\")}")
  }

  # --- Load per-game EPV ---
  epv_files <- list.files("data-raw/cache/epv/players",
                           pattern = "^player_game_epv_", full.names = TRUE)
  epv_data <- if (length(epv_files) > 0) {
    data.table::rbindlist(lapply(epv_files, readRDS), fill = TRUE)
  } else NULL

  # --- Load per-game WPA ---
  wpa_files <- list.files("data-raw/cache/epv/players",
                           pattern = "^player_game_wpa_", full.names = TRUE)
  wpa_data <- if (length(wpa_files) > 0) {
    data.table::rbindlist(lapply(wpa_files, readRDS), fill = TRUE)
  } else NULL

  # --- Load per-game PSV ---
  psv_path <- "data-raw/cache-skills/player_game_psv.rds"
  psv_data <- if (file.exists(psv_path)) readRDS(psv_path) else NULL

  # --- Match player name ---
  .find_player <- function(dt, target) {
    if (is.null(dt) || !"player_name" %in% names(dt)) return(NULL)
    # Exact match first
    idx <- which(dt$player_name == target)
    if (length(idx) == 0) {
      # Case-insensitive partial match
      pattern <- gsub("([.()])", "\\\\\\1", target)
      idx <- grep(pattern, dt$player_name, ignore.case = TRUE)
    }
    if (length(idx) == 0) {
      # Accent-insensitive
      target_ascii <- iconv(target, to = "ASCII//TRANSLIT")
      names_ascii <- iconv(dt$player_name, to = "ASCII//TRANSLIT")
      idx <- grep(target_ascii, names_ascii, ignore.case = TRUE)
    }
    if (length(idx) == 0) return(NULL)
    dt[idx]
  }

  # Find player in available data
  player_epv <- .find_player(epv_data, target_player)
  player_wpa <- .find_player(wpa_data, target_player)
  player_psv <- .find_player(psv_data, target_player)

  # Resolve actual name
  matched_name <- NULL
  if (!is.null(player_epv) && nrow(player_epv) > 0) {
    matched_name <- player_epv$player_name[1]
  } else if (!is.null(player_wpa) && nrow(player_wpa) > 0) {
    matched_name <- player_wpa$player_name[1]
  } else if (!is.null(player_psv) && nrow(player_psv) > 0) {
    matched_name <- player_psv$player_name[1]
  }

  if (is.null(matched_name)) {
    cli::cli_abort(c(
      "Player {.val {target_player}} not found in any value metric cache.",
      "i" = "Check spelling or run the EPV/WPA/PSV pipelines first."
    ))
  }

  # Filter exact name and season
  .filter <- function(dt, name, szn) {
    if (is.null(dt)) return(NULL)
    dt <- dt[dt$player_name == name, ]
    if (!is.null(szn) && "season" %in% names(dt)) dt <- dt[dt$season == szn, ]
    if (nrow(dt) == 0) return(NULL)
    dt
  }

  player_epv <- .filter(player_epv, matched_name, season)
  player_wpa <- .filter(player_wpa, matched_name, season)
  player_psv <- .filter(player_psv, matched_name, season)

  # --- Build summary ---
  summary_rows <- list()

  if (!is.null(player_epv)) {
    n_epv <- nrow(player_epv)
    mins <- sum(player_epv$minutes_played, na.rm = TRUE)
    mins_safe <- max(mins, 1)
    summary_rows$epv <- data.table::data.table(
      metric = c("EPV Total", "EPV Offensive", "EPV Defensive",
                  "EPV Passing", "EPV Shooting", "EPV Defending"),
      total = c(sum(player_epv$epv_total, na.rm = TRUE),
                 sum(player_epv$epv_offensive, na.rm = TRUE),
                 sum(player_epv$epv_defensive, na.rm = TRUE),
                 sum(player_epv$epv_passing, na.rm = TRUE),
                 sum(player_epv$epv_shooting, na.rm = TRUE),
                 sum(player_epv$epv_defending, na.rm = TRUE)),
      per_90 = NA_real_,
      games = n_epv,
      minutes = mins
    )
    summary_rows$epv[, per_90 := total / (mins_safe / 90)]
  }

  if (!is.null(player_wpa)) {
    n_wpa <- nrow(player_wpa)
    mins <- sum(player_wpa$minutes_played, na.rm = TRUE)
    mins_safe <- max(mins, 1)
    summary_rows$wpa <- data.table::data.table(
      metric = c("WPA Total", "WPA as Actor", "WPA as Receiver"),
      total = c(sum(player_wpa$wpa_total, na.rm = TRUE),
                 sum(player_wpa$wpa_as_actor, na.rm = TRUE),
                 sum(player_wpa$wpa_as_receiver, na.rm = TRUE)),
      per_90 = NA_real_,
      games = n_wpa,
      minutes = mins
    )
    summary_rows$wpa[, per_90 := total / (mins_safe / 90)]
  }

  if (!is.null(player_psv)) {
    n_psv <- nrow(player_psv)
    mins <- sum(player_psv$total_minutes, na.rm = TRUE)
    mins_safe <- max(mins, 1)
    summary_rows$psv <- data.table::data.table(
      metric = c("PSV (Stat Value)", "OSV (Offensive)", "DSV (Defensive)"),
      total = c(sum(player_psv$psv, na.rm = TRUE),
                 sum(player_psv$osv, na.rm = TRUE),
                 sum(player_psv$dsv, na.rm = TRUE)),
      per_90 = NA_real_,
      games = n_psv,
      minutes = mins
    )
    summary_rows$psv[, per_90 := total / (mins_safe / 90)]
  }

  summary <- if (length(summary_rows) > 0) {
    data.table::rbindlist(summary_rows, fill = TRUE)
  } else {
    data.table::data.table(metric = character(0))
  }

  # --- Compute EPR ---
  epr_result <- NULL
  if (!is.null(player_epv) && "match_date" %in% names(player_epv)) {
    if (!"minutes_played" %in% names(player_epv)) {
      player_epv[, minutes_played := 90]
    }
    epr_result <- tryCatch(
      calculate_epr(player_epv, ref_date = Sys.Date()),
      error = function(e) NULL
    )
  }

  # --- Print ---
  cli::cli_h2("Value Profile: {matched_name}")

  if (nrow(summary) > 0) {
    summary[, total := round(total, 3)]
    summary[, per_90 := round(per_90, 3)]
    print(summary)
  }

  if (!is.null(epr_result) && nrow(epr_result) > 0) {
    cat(sprintf("\nEPR Rating: %.3f (off: %.3f, def: %.3f) | %d games\n",
                epr_result$epr[1], epr_result$epr_offensive[1],
                epr_result$epr_defensive[1], epr_result$n_games[1]))
  }

  invisible(list(
    player_name = matched_name,
    summary = summary,
    game_log_epv = player_epv,
    game_log_wpa = player_wpa,
    game_log_psv = player_psv,
    epr = epr_result
  ))
}
