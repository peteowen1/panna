# Fast feature query for the minutes-projection model
#
# `build_minutes_training_data()` does the full per-player rolling-window
# computation across every player in the lineups dataset. That's fine for
# training (one-shot ~5 min) but wasteful for prediction -- we only need
# features for ~26 players in a single upcoming match.
#
# This module:
#   1. `prepare_minutes_cache()` -- one-time precomputation (cached to RDS)
#   2. `query_minutes_features()` -- fast lookup for any (player x date)
#
# Together they keep prediction cost at O(players x log(matches)) per call.

#' Prepare a cached lineups data object for fast minutes-feature queries
#'
#' Runs the global / per-team / per-player passes once and caches the
#' intermediate data structures needed by `query_minutes_features()`.
#' Re-run only when the underlying lineups parquet changes.
#'
#' @param lineups Data.frame/data.table of opta lineups (full, both intl and club).
#' @param intl_comps Character vector of competition codes treated as
#'   "international".
#' @param ratings_path Path to seasonal xRAPM RDS (for panna lookups).
#' @param cache_path Optional RDS file path. If supplied, saves the result.
#' @param verbose Logical.
#' @return A list with:
#'   * `per_player_history` -- keyed data.table per (player_id, date_int)
#'     with `mins_intl, mins_club, app_intl, start_intl, start_club, is_intl`
#'   * `cumsum_lookup` -- per-player precomputed cumulative-sum arrays for
#'     fast rolling-window queries
#'   * `modal_role` -- modal non-Substitute role per player
#'   * `player_panna` -- panna ratings table (player_id x season_end_year)
#'   * `team_rotation` -- global rotation_idx per team
#'   * `team_intl_count` -- number of intl matches per team
#' @family expected minutes
#' @export
prepare_minutes_cache <- function(lineups,
                                    intl_comps,
                                    ratings_path = "data-raw/cache-opta/07_seasonal_ratings.rds",
                                    cache_path = NULL,
                                    verbose = TRUE) {
  if (!data.table::is.data.table(lineups)) lineups <- data.table::as.data.table(lineups)
  if (verbose) cli::cli_alert_info("Cleaning + flagging {nrow(lineups)} lineup rows...")

  lu <- copy(lineups)
  lu[, match_date  := as.Date(sub("Z$", "", match_date))]
  lu[, minutes_played := pmin(pmax(minutes_played, 0L), 95L)]
  lu[, is_intl     := competition %in% intl_comps]
  lu[, played_flag := as.integer(minutes_played > 0L)]
  lu[, mins_intl  := fifelse(is_intl,  minutes_played, 0L)]
  lu[, mins_club  := fifelse(is_intl,  0L,             minutes_played)]
  lu[, app_intl   := fifelse(is_intl & played_flag == 1L, 1L, 0L)]
  lu[, start_intl := fifelse(is_intl & is_starter,        1L, 0L)]
  lu[, start_club := fifelse(!is_intl & is_starter,       1L, 0L)]
  lu[, role := classify_role(position, position_side)]

  ## Keep only players with intl history (others can't be predicted anyway)
  intl_players <- unique(lu[is_intl == TRUE & played_flag == 1L, player_id])
  lu <- lu[player_id %in% intl_players]
  if (verbose) cli::cli_alert_info("Tracking {length(intl_players)} players with intl history")

  ## --- Sort + index ----------------------------------------------------
  setorder(lu, player_id, match_date)
  lu[, date_int := as.integer(match_date)]

  ## --- Per-player cumulative-sum lookup --------------------------------
  ## For each player, store: vector of date_ints + cumulative sums of each
  ## flag column. To get a rolling-N-day sum at date D, binary-search the
  ## date vector for the position p where date_int[p] >= D-N, then return
  ## cumsum[length] - cumsum[p].
  if (verbose) cli::cli_alert_info("Building per-player cumsum lookup...")
  cumsum_lookup <- lu[, {
    list(date_int = list(date_int),
         cum_mins_intl  = list(cumsum(c(0L, mins_intl))),
         cum_mins_club  = list(cumsum(c(0L, mins_club))),
         cum_app_intl   = list(cumsum(c(0L, app_intl))),
         cum_start_intl = list(cumsum(c(0L, start_intl))),
         cum_start_club = list(cumsum(c(0L, start_club))),
         is_intl_flag   = list(is_intl))
  }, by = player_id]
  setkey(cumsum_lookup, player_id)

  ## --- Modal role per player ------------------------------------------
  pos_lookup <- lu[is_intl == TRUE & played_flag == 1L & role != "UNK",
                   .N, by = .(player_id, role)]
  setorder(pos_lookup, player_id, -N)
  modal_role <- pos_lookup[, .SD[1L], by = player_id][, .(player_id, modal_role = role)]
  setkey(modal_role, player_id)

  ## --- Player panna ratings -------------------------------------------
  player_panna <- data.table::data.table()
  if (file.exists(ratings_path)) {
    rd <- readRDS(ratings_path)
    xrapm <- data.table::as.data.table(rd$seasonal_xrapm)
    setnames(xrapm, "xrapm", "panna")
    rcols <- intersect(c("player_id", "season_end_year",
                         "panna", "offense", "defense"), names(xrapm))
    player_panna <- unique(xrapm[, ..rcols])
    setnames(player_panna, c("offense", "defense"), c("p_off", "p_def"),
             skip_absent = TRUE)
    setkey(player_panna, player_id, season_end_year)
  }

  ## --- Team rotation index --------------------------------------------
  if (verbose) cli::cli_alert_info("Computing team rotation index...")
  intl_starters <- lu[is_intl == TRUE & is_starter == TRUE,
                      .(team_name, match_id, match_date, player_id)]
  setorder(intl_starters, team_name, match_date, player_id)
  unique_team_matches <- unique(intl_starters[, .(team_name, match_id, match_date)])
  setorder(unique_team_matches, team_name, match_date)
  team_rotation <- unique_team_matches[, {
    if (.N < 2L) {
      list(rotation_idx = 0.75)
    } else {
      mids <- match_id
      ovs <- numeric(.N - 1L)
      for (i in seq.int(2L, .N)) {
        a <- intl_starters[match_id == mids[i - 1L], player_id]
        b <- intl_starters[match_id == mids[i],     player_id]
        ovs[i - 1L] <- length(intersect(a, b)) / 11
      }
      list(rotation_idx = mean(ovs))
    }
  }, by = team_name]
  setkey(team_rotation, team_name)

  ## --- Team intl match count ------------------------------------------
  team_intl_count <- lu[is_intl == TRUE,
                        .(team_intl_count = uniqueN(match_id)), by = team_name]
  setkey(team_intl_count, team_name)

  ## --- Last intl date per team (for days_rest_team default) -----------
  team_last_intl <- lu[is_intl == TRUE,
                       .(last_intl_date = max(match_date)), by = team_name]
  setkey(team_last_intl, team_name)

  ## --- All intl match dates per team (for prev-team-match features) ----
  team_intl_dates <- lu[is_intl == TRUE,
                        .(dates = list(sort(unique(date_int)))), by = team_name]
  setkey(team_intl_dates, team_name)

  cache <- list(
    cumsum_lookup   = cumsum_lookup,
    modal_role      = modal_role,
    player_panna    = player_panna,
    team_rotation   = team_rotation,
    team_intl_count = team_intl_count,
    team_last_intl  = team_last_intl,
    team_intl_dates = team_intl_dates,
    base_start_rate = mean(lu$is_starter[lu$is_intl], na.rm = TRUE),
    intl_comps      = intl_comps,
    n_players       = length(intl_players),
    prepared_at     = Sys.time()
  )

  if (!is.null(cache_path)) {
    if (verbose) cli::cli_alert_info("Saving cache to {cache_path}")
    saveRDS(cache, cache_path)
  }
  cache
}

#' Helper -- rolling-sum lookup for a single player as-of a date
#' @keywords internal
.rolling_at <- function(date_int, cum_vec, as_of, window_days) {
  if (length(date_int) == 0L) return(0)
  ## We want sum of values where (as_of - window_days) <= d < as_of
  ## cum_vec[i+1] = sum(values[1:i])
  cutoff_lo <- as_of - window_days
  ## Find: largest i where date_int[i] < as_of (call this hi)
  hi <- sum(date_int < as_of)
  if (hi == 0L) return(0)
  ## Find: largest i where date_int[i] < cutoff_lo (call this lo)
  lo <- sum(date_int < cutoff_lo)
  cum_vec[hi + 1L] - cum_vec[lo + 1L]
}

#' Compute minutes-model features for a specific list of players at a date
#'
#' @param cache Output of `prepare_minutes_cache()`.
#' @param player_ids Character vector of player_ids to predict for.
#' @param team_name The country these players are playing for.
#' @param as_of_date Date -- the upcoming match date.
#' @param tournament_match_num Integer -- which game in their tournament run.
#' @param days_rest_team Integer -- days since this team's last intl match.
#'   If NULL, derived from cache.
#' @param is_tournament Integer 0/1 -- group-stage / knockout (1) vs qualifier (0).
#' @param is_friendly Integer 0/1 -- friendly (1) vs competitive (0). Default 0
#'   for WC/qualifier predictions.
#' @param tournament_start Date. First day of the current tournament. When
#'   supplied, `tourn_mins_sofar` / `tourn_starts_sofar` accumulate the
#'   player's intl minutes/starts in `[tournament_start, as_of_date)`;
#'   otherwise they are 0 (matches the training convention where
#'   non-tournament rows are zeroed).
#' @return Data.table with one row per player, columns matching the model's
#'   `feature_cols`. Pass directly to `predict_minutes()`.
#' @family expected minutes
#' @export
query_minutes_features <- function(cache,
                                     player_ids,
                                     team_name,
                                     as_of_date,
                                     tournament_match_num = 1L,
                                     days_rest_team = NULL,
                                     is_tournament = 1L,
                                     is_friendly = 0L,
                                     tournament_start = NULL) {
  as_of_date <- as.Date(as_of_date)
  as_of_int  <- as.integer(as_of_date)
  tourn_int  <- if (is.null(tournament_start))
    NA_integer_ else as.integer(as.Date(tournament_start))

  ## Team's previous intl match strictly before as_of (for the
  ## started_prev_team_match / mins_prev_team_match features).
  ## Caches built before these fields existed degrade to feature = 0 /
  ## base rate 0.5 rather than erroring.
  base_sr <- if (is.null(cache$base_start_rate)) 0.5 else cache$base_start_rate
  prev_team_int <- NA_integer_
  if (!is.null(cache$team_intl_dates)) {
    tdates <- cache$team_intl_dates[team_name, on = "team_name", dates]
    if (length(tdates) == 1L && !is.null(tdates[[1]])) {
      past <- tdates[[1]][tdates[[1]] < as_of_int]
      if (length(past)) prev_team_int <- past[length(past)]
    }
  }

  ## Find each player's cumsum data
  player_rows <- cache$cumsum_lookup[J(player_ids), nomatch = NA]
  ## Players with no history -> keep as zeros. Unmatched rows surface as
  ## NULL list-column elements (is.na() on those is FALSE), so test for
  ## emptiness, not NA-ness.
  found <- vapply(player_rows$cum_mins_intl,
                  function(v) !is.null(v) && length(v) > 0, logical(1))

  ## Compute the per-player rolling windows
  out <- data.table::data.table(player_id = player_ids)
  out[, intl_mins_30d  := 0]; out[, intl_mins_180d := 0]; out[, intl_mins_540d := 0]
  out[, intl_apps_180d := 0]; out[, intl_starts_180d := 0]
  out[, club_mins_30d  := 0]; out[, club_mins_90d  := 0]; out[, club_starts_90d := 0]
  out[, days_since_last_intl := 999L]
  out[, days_since_last_club := 999L]
  out[, career_intl_apps := 0]
  out[, caps_decay := 0]; out[, p_start_decay := base_sr]
  out[, tourn_mins_sofar := 0]; out[, tourn_starts_sofar := 0]
  out[, started_prev_team_match := 0L]; out[, mins_prev_team_match := 0]

  for (i in which(found)) {
    di <- player_rows$date_int[[i]]
    intl_flag <- player_rows$is_intl_flag[[i]]
    cm_i <- player_rows$cum_mins_intl[[i]]
    cm_c <- player_rows$cum_mins_club[[i]]
    ca_i <- player_rows$cum_app_intl[[i]]
    cs_i <- player_rows$cum_start_intl[[i]]
    cs_c <- player_rows$cum_start_club[[i]]

    out$intl_mins_30d[i]    <- .rolling_at(di, cm_i, as_of_int, 30L)
    out$intl_mins_180d[i]   <- .rolling_at(di, cm_i, as_of_int, 180L)
    out$intl_mins_540d[i]   <- .rolling_at(di, cm_i, as_of_int, 540L)
    out$intl_apps_180d[i]   <- .rolling_at(di, ca_i, as_of_int, 180L)
    out$intl_starts_180d[i] <- .rolling_at(di, cs_i, as_of_int, 180L)
    out$club_mins_30d[i]    <- .rolling_at(di, cm_c, as_of_int, 30L)
    out$club_mins_90d[i]    <- .rolling_at(di, cm_c, as_of_int, 90L)
    out$club_starts_90d[i]  <- .rolling_at(di, cs_c, as_of_int, 90L)

    ## Days since last intl/club
    intl_dates <- di[intl_flag == TRUE & di < as_of_int]
    if (length(intl_dates)) out$days_since_last_intl[i] <- as_of_int - max(intl_dates)
    club_dates <- di[intl_flag == FALSE & di < as_of_int]
    if (length(club_dates)) out$days_since_last_club[i] <- as_of_int - max(club_dates)

    ## Career intl apps strictly before this date
    out$career_intl_apps[i] <- ca_i[sum(di < as_of_int) + 1L]

    ## Per-row values recovered from the cumulative vectors
    mins_intl_rows  <- diff(cm_i)
    start_intl_rows <- diff(cs_i)

    ## Decay-weighted intl start rate with Beta prior (mirrors
    ## build_minutes_training_data: 365d half-life, k = 3, squad base rate)
    w <- ifelse(di < as_of_int & intl_flag,
                0.5 ^ ((as_of_int - di) / 365), 0)
    out$caps_decay[i] <- sum(w)
    out$p_start_decay[i] <- (sum(w * start_intl_rows) + 3 * base_sr) /
      (sum(w) + 3)

    ## Within-current-tournament accumulation
    if (!is.na(tourn_int)) {
      in_t <- intl_flag & di >= tourn_int & di < as_of_int
      out$tourn_mins_sofar[i]   <- sum(mins_intl_rows[in_t])
      out$tourn_starts_sofar[i] <- sum(start_intl_rows[in_t])
    }

    ## Involvement in the team's previous intl match
    if (!is.na(prev_team_int)) {
      idx <- which(di == prev_team_int & intl_flag)
      if (length(idx)) {
        out$started_prev_team_match[i] <- start_intl_rows[idx[1L]]
        out$mins_prev_team_match[i]    <- mins_intl_rows[idx[1L]]
      }
    }
  }

  ## Modal role
  out <- merge(out, cache$modal_role, by = "player_id", all.x = TRUE)
  out[is.na(modal_role), modal_role := "CM"]
  role_levels <- c("GK","CB","LB","RB","LWB","RWB","DM","CM","LM","RM",
                   "CAM","LW","RW","CF","LF","RF")
  for (r in role_levels) out[, (paste0("is_", tolower(r))) := as.integer(modal_role == r)]
  out[, is_gkb  := as.integer(modal_role == "GK")]
  out[, is_defb := as.integer(modal_role %in% c("CB","LB","RB","LWB","RWB"))]
  out[, is_midb := as.integer(modal_role %in% c("DM","CM","LM","RM","CAM"))]
  out[, is_fwdb := as.integer(modal_role %in% c("LW","RW","CF","LF","RF"))]

  ## Panna ratings -- use season_end_year of the upcoming match. The cache
  ## stores a zero-column table when no ratings file was available; degrade
  ## to all-zero ratings instead of erroring on the season subset.
  sey <- if (data.table::month(as_of_date) >= 7L)
    data.table::year(as_of_date) + 1L else data.table::year(as_of_date)
  if (nrow(cache$player_panna) > 0L) {
    pp <- cache$player_panna[season_end_year == sey,
                              .(player_id, panna, p_off, p_def)]
    out <- merge(out, pp, by = "player_id", all.x = TRUE)
  } else {
    out[, `:=`(panna = NA_real_, p_off = NA_real_, p_def = NA_real_)]
  }
  for (col in c("panna", "p_off", "p_def"))
    out[is.na(get(col)), (col) := 0]

  ## Team-level features
  rot <- cache$team_rotation[team_name, on = "team_name", rotation_idx]
  if (length(rot) == 0L || is.na(rot)) rot <- 0.75
  out[, rotation_idx := rot]

  cnt <- cache$team_intl_count[team_name, on = "team_name", team_intl_count]
  if (length(cnt) == 0L || is.na(cnt)) cnt <- 0L
  out[, team_intl_count := cnt]

  if (is.null(days_rest_team)) {
    last <- cache$team_last_intl[team_name, on = "team_name", last_intl_date]
    days_rest_team <- if (length(last) == 0L || is.na(last))
      365L else as.integer(as_of_date - last)
  }
  out[, days_rest_team := days_rest_team]
  out[, tournament_match_num := tournament_match_num]
  out[, is_tournament := is_tournament]
  out[, is_friendly := is_friendly]

  out
}
