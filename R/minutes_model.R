# Minutes-projection model for national-team matches
#
# Given a player and a specific intl match (real or hypothetical), predict
# their expected minutes played. Features are strictly lagged from the
# match date -- no leakage. Trains an XGBoost regression on every historical
# (player x intl match) tuple in our Opta lineups data.

#' Map raw Opta (position, side) pair to a 16-role taxonomy
#'
#' Granular roles capture position-specific sub patterns the broad
#' GK/DEF/MID/FWD bucketing washes out (e.g. RBs are subbed off 18%
#' more often than CBs; AMs come off twice as often as CBs).
#'
#' @param position Character vector of `position` column from lineups.
#' @param side Character vector of `position_side` column.
#' @return Character vector of role codes:
#'   GK, CB, LB, RB, LWB, RWB, DM, CM, LM, RM, CAM, LW, RW, CF, LF, RF
#'   (or `"UNK"` for blanks / "Substitute" / unrecognized).
#' @export
classify_role <- function(position, side) {
  pos <- ifelse(is.na(position), "", as.character(position))
  sd  <- ifelse(is.na(side),     "", as.character(side))
  ## Treat compound sides (Centre/Right, Left/Centre) as the "central" anchor
  is_central <- sd %in% c("Centre", "Centre/Right", "Left/Centre")
  is_left    <- sd == "Left"
  is_right   <- sd == "Right"
  out <- rep("UNK", length(pos))

  out[pos == "Goalkeeper"] <- "GK"

  out[pos == "Defender" & is_central] <- "CB"
  out[pos == "Defender" & is_left]    <- "LB"
  out[pos == "Defender" & is_right]   <- "RB"

  out[pos == "Wing Back" & is_left]   <- "LWB"
  out[pos == "Wing Back" & is_right]  <- "RWB"
  out[pos == "Wing Back" & is_central] <- "RWB"  # rare; treat as wing-back-y

  out[pos == "Defensive Midfielder"] <- "DM"

  out[pos == "Midfielder" & is_central] <- "CM"
  out[pos == "Midfielder" & is_left]    <- "LM"
  out[pos == "Midfielder" & is_right]   <- "RM"

  out[pos == "Attacking Midfielder" & is_central] <- "CAM"
  out[pos == "Attacking Midfielder" & is_left]    <- "LW"
  out[pos == "Attacking Midfielder" & is_right]   <- "RW"

  out[pos == "Striker" & is_central] <- "CF"
  out[pos == "Striker" & is_left]    <- "LF"
  out[pos == "Striker" & is_right]   <- "RF"

  out
}

#' Decay-weighted strictly-past sum for one ordered series
#'
#' `S_i = sum_{j < i} value_j * 0.5 ^ ((date_i - date_j) / half_life)` via the
#' incremental recurrence `S_i = (S_{i-1} + value_{i-1}) * q ^ (date_i -
#' date_{i-1})`, so it runs in O(n) per player.
#' @keywords internal
.decay_past_sum <- function(value, date_int, half_life) {
  n <- length(value)
  out <- numeric(n)
  if (n < 2L) return(out)
  q <- 0.5 ^ (1 / half_life)
  s <- 0
  for (i in 2:n) {
    s <- (s + value[i - 1L]) * q ^ (date_int[i] - date_int[i - 1L])
    out[i] <- s
  }
  out
}

#' Compute strictly-past rolling-sum within a window for one ordered series
#' @keywords internal
.rolling_past_sum <- function(value, date_int, window_days) {
  n <- length(value)
  out <- numeric(n)
  cumv <- cumsum(c(0, value))
  j <- 0L
  for (i in seq_len(n)) {
    target <- date_int[i] - window_days
    while (j < n && date_int[j + 1L] < target) j <- j + 1L
    start <- max(1L, j + 1L)
    if (start < i) out[i] <- cumv[i] - cumv[start]
  }
  out
}

#' Build training dataset for the minutes-projection model
#'
#' For each (player_id x match) in the international competitions, computes
#' backward-looking features as of the match date and pairs them with the
#' realized `minutes_played` (the supervised target).
#'
#' @param lineups Data.table of opta lineups (full, not just intl).
#' @param intl_comps Character vector of competition codes treated as
#'   "international". Training rows are restricted to these.
#' @param ratings_path Path to seasonal xRAPM RDS.
#' @param min_team_matches Drop teams with fewer than this many intl matches.
#' @param verbose Logical.
#' @return A data.table with features + `minutes_played` target. The vector
#'   of training feature column names is in `attr(result, "feature_cols")`.
#' @export
build_minutes_training_data <- function(lineups,
                                          intl_comps,
                                          ratings_path = "data-raw/cache-opta/07_seasonal_ratings.rds",
                                          min_team_matches = 5L,
                                          verbose = TRUE) {
  if (!data.table::is.data.table(lineups)) lineups <- data.table::as.data.table(lineups)

  ## --- 1. Clean & flag --------------------------------------------------
  lu <- copy(lineups)
  lu[, match_date  := as.Date(sub("Z$", "", match_date))]
  lu[, minutes_played := pmin(pmax(minutes_played, 0L), 95L)]
  lu[, is_intl     := competition %in% intl_comps]
  lu[, played_flag := as.integer(minutes_played > 0L)]

  ## Pre-built per-row contributions for the rolling sums
  lu[, mins_intl  := fifelse(is_intl,  minutes_played, 0L)]
  lu[, mins_club  := fifelse(is_intl,  0L,             minutes_played)]
  lu[, app_intl   := fifelse(is_intl & played_flag == 1L, 1L, 0L)]
  lu[, start_intl := fifelse(is_intl & is_starter,        1L, 0L)]
  lu[, start_club := fifelse(!is_intl & is_starter,       1L, 0L)]

  ## Drop players who never played intl -- they don't show up in test/training
  intl_players <- unique(lu[is_intl == TRUE & played_flag == 1L, player_id])
  lu <- lu[player_id %in% intl_players]
  if (verbose) cli::cli_alert_info("Tracking {length(intl_players)} players with intl history")

  ## --- 2. Per-player rolling windows (strictly past) --------------------
  setorder(lu, player_id, match_date)
  lu[, date_int := as.integer(match_date)]

  if (verbose) cli::cli_alert_info("Computing rolling windows...")
  lu[, intl_mins_30d   := .rolling_past_sum(mins_intl,  date_int, 30L),  by = player_id]
  lu[, intl_mins_180d  := .rolling_past_sum(mins_intl,  date_int, 180L), by = player_id]
  lu[, intl_mins_540d  := .rolling_past_sum(mins_intl,  date_int, 540L), by = player_id]
  lu[, intl_apps_180d  := .rolling_past_sum(app_intl,   date_int, 180L), by = player_id]
  lu[, intl_starts_180d := .rolling_past_sum(start_intl, date_int, 180L), by = player_id]
  lu[, club_mins_30d   := .rolling_past_sum(mins_club,  date_int, 30L),  by = player_id]
  lu[, club_mins_90d   := .rolling_past_sum(mins_club,  date_int, 90L),  by = player_id]
  lu[, club_starts_90d := .rolling_past_sum(start_club, date_int, 90L),  by = player_id]

  ## Days-since features (strictly previous match). Work entirely in integer
  ## days-since-epoch space to avoid R's Date class auto-dispatching subtraction.
  lu[, prev_intl_dateint := shift(fifelse(is_intl == TRUE, date_int, NA_integer_)),
     by = player_id]
  lu[, prev_intl_dateint := nafill(prev_intl_dateint, "locf"), by = player_id]
  lu[, days_since_last_intl := date_int - prev_intl_dateint]
  lu[is.na(days_since_last_intl), days_since_last_intl := 999L]

  lu[, prev_club_dateint := shift(fifelse(is_intl == FALSE, date_int, NA_integer_)),
     by = player_id]
  lu[, prev_club_dateint := nafill(prev_club_dateint, "locf"), by = player_id]
  lu[, days_since_last_club := date_int - prev_club_dateint]
  lu[is.na(days_since_last_club), days_since_last_club := 999L]

  ## Career intl caps strictly before current row
  lu[, career_intl_apps := cumsum(app_intl) - app_intl, by = player_id]

  ## Decay-weighted (365d half-life) intl start rate with a Beta prior --
  ## the expected-minutes heuristic's core signal, stacked in as a feature.
  ## Denominator counts every intl LINEUP row (bench included), matching
  ## build_team_expected_minutes()'s weight_total.
  lu[, caps_decay   := .decay_past_sum(as.numeric(is_intl), date_int, 365), by = player_id]
  lu[, starts_decay := .decay_past_sum(as.numeric(start_intl), date_int, 365), by = player_id]
  base_start <- mean(lu$is_starter[lu$is_intl], na.rm = TRUE)
  lu[, p_start_decay := (starts_decay + 3 * base_start) / (caps_decay + 3)]

  ## --- 3. Filter to intl rows (training set) ----------------------------
  train <- lu[is_intl == TRUE]
  if (verbose) cli::cli_alert_info("{nrow(train)} intl player-match rows in training")

  ## --- 4. Player panna rating (from seasonal xRAPM) ---------------------
  if (file.exists(ratings_path)) {
    rd <- readRDS(ratings_path)
    xrapm <- data.table::as.data.table(rd$seasonal_xrapm)
    setnames(xrapm, "xrapm", "panna")
    train[, season_end_year := fifelse(
      data.table::month(match_date) >= 7L, data.table::year(match_date) + 1L,
      data.table::year(match_date))]
    rcols <- intersect(c("player_id", "season_end_year", "panna", "offense", "defense"),
                       names(xrapm))
    train <- merge(train,
                   unique(xrapm[, ..rcols]),
                   by = c("player_id", "season_end_year"), all.x = TRUE)
    setnames(train, c("offense", "defense"), c("p_off", "p_def"), skip_absent = TRUE)
    for (col in c("panna", "p_off", "p_def"))
      if (col %in% names(train)) train[is.na(get(col)), (col) := 0]
  } else {
    train[, `:=`(panna = 0, p_off = 0, p_def = 0)]
  }

  ## --- 5. Granular role dummies (modal non-Substitute) ------------------
  ## Use the (position, position_side) joint label and map to our 16-role
  ## taxonomy. Modal role per player = whichever code they appeared in most
  ## often when actually playing.
  lu[, role := classify_role(position, position_side)]
  role_lookup <- lu[is_intl == TRUE & played_flag == 1L & role != "UNK",
                    .N, by = .(player_id, role)]
  setorder(role_lookup, player_id, -N)
  modal_role <- role_lookup[, .SD[1L], by = player_id][, .(player_id, modal_role = role)]
  train <- merge(train, modal_role, by = "player_id", all.x = TRUE)
  train[is.na(modal_role), modal_role := "CM"]

  ## 16 binary dummies (XGBoost handles factors via one-hot internally too
  ## but explicit dummies make for cleaner feature inspection downstream)
  role_levels <- c("GK","CB","LB","RB","LWB","RWB","DM","CM","LM","RM",
                   "CAM","LW","RW","CF","LF","RF")
  for (r in role_levels) {
    train[, (paste0("is_", tolower(r))) := as.integer(modal_role == r)]
  }
  ## Broad-bucket flags retained for ease of interpretation / fallback
  train[, is_gkb  := as.integer(modal_role == "GK")]
  train[, is_defb := as.integer(modal_role %in% c("CB","LB","RB","LWB","RWB"))]
  train[, is_midb := as.integer(modal_role %in% c("DM","CM","LM","RM","CAM"))]
  train[, is_fwdb := as.integer(modal_role %in% c("LW","RW","CF","LF","RF"))]

  ## --- 6. Match-level: days rest for team -------------------------------
  team_dates <- unique(train[, .(team_name, match_date)])
  setorder(team_dates, team_name, match_date)
  team_dates[, days_rest_team := as.integer(match_date - shift(match_date)), by = team_name]
  train <- merge(train, team_dates, by = c("team_name", "match_date"), all.x = TRUE)
  train[is.na(days_rest_team), days_rest_team := 365L]

  ## --- 7. Tournament context flag + match num ---------------------------
  tournament_comps <- c("AFCON", "AFC_Asian_Cup", "Copa_America", "UEFA_Euros",
                        "CONCACAF_Gold_Cup", "Gulf_Cup_of_Nations", "World_Cup")
  train[, is_tournament := as.integer(competition %in% tournament_comps)]
  ## Friendly flag -- managers rotate heavily, give debutants 45 min, sub
  ## starters at the hour mark. Distinct minute distribution from competitive
  ## matches; explicit flag lets the model learn separate leaf weights.
  train[, is_friendly := as.integer(competition == "Intl_Friendlies")]
  setorder(train, team_name, competition, season, match_date)
  train[, tournament_match_num := seq_len(.N), by = .(team_name, competition, season)]

  ## --- 7b. Within-current-tournament accumulation ------------------------
  ## Minutes/starts already banked in THIS tournament instance (player x
  ## team x competition x season, strictly before the current match). The
  ## WC2022 backtest showed in-tournament selections are the strongest
  ## predictor of the next game's XI -- these give the model that signal
  ## directly instead of diluted through the 30/180d windows.
  setorder(train, player_id, team_name, competition, season, match_date)
  train[, tourn_mins_sofar :=
          cumsum(as.numeric(minutes_played)) - as.numeric(minutes_played),
        by = .(player_id, team_name, competition, season)]
  train[, tourn_starts_sofar :=
          cumsum(as.integer(is_starter)) - as.integer(is_starter),
        by = .(player_id, team_name, competition, season)]
  train[is_tournament == 0L, `:=`(tourn_mins_sofar = 0, tourn_starts_sofar = 0L)]

  ## --- 7c. Player's involvement in the team's previous intl match --------
  team_seq <- unique(train[, .(team_name, match_id, match_date)])
  setorder(team_seq, team_name, match_date)
  team_seq[, prev_match_id := shift(match_id), by = team_name]
  train <- merge(train, team_seq[, .(team_name, match_id, prev_match_id)],
                 by = c("team_name", "match_id"), all.x = TRUE)
  prev_lu <- train[, .(team_name, prev_match_id = match_id, player_id,
                       started_prev_team_match = as.integer(is_starter),
                       mins_prev_team_match = minutes_played)]
  train <- merge(train, prev_lu,
                 by = c("team_name", "prev_match_id", "player_id"),
                 all.x = TRUE)
  ## Absent from the previous matchday squad (or no previous match) -> 0
  train[is.na(started_prev_team_match), started_prev_team_match := 0L]
  train[is.na(mins_prev_team_match), mins_prev_team_match := 0L]

  ## --- 8. Team rotation index -------------------------------------------
  starters <- train[is_starter == TRUE,
                    .(team_name, match_id, match_date, player_id)]
  setorder(starters, team_name, match_date, player_id)
  unique_team_matches <- unique(starters[, .(team_name, match_id, match_date)])
  setorder(unique_team_matches, team_name, match_date)
  rotation_per_team <- unique_team_matches[, {
    if (.N < 2L) {
      list(rotation_idx = 0.75)
    } else {
      mids <- match_id
      ovs <- numeric(.N - 1L)
      for (i in seq.int(2L, .N)) {
        a <- starters[match_id == mids[i - 1L], player_id]
        b <- starters[match_id == mids[i],     player_id]
        ovs[i - 1L] <- length(intersect(a, b)) / 11
      }
      list(rotation_idx = mean(ovs))
    }
  }, by = team_name]
  train <- merge(train, rotation_per_team, by = "team_name", all.x = TRUE)
  train[is.na(rotation_idx), rotation_idx := 0.75]

  ## --- 9. Drop sparse teams ---------------------------------------------
  team_counts <- train[, .(team_intl_count = uniqueN(match_id)), by = team_name]
  train <- merge(train, team_counts, by = "team_name", all.x = TRUE)
  train <- train[team_intl_count >= min_team_matches]
  if (verbose) cli::cli_alert_info("After sparse-team filter: {nrow(train)} rows")

  ## --- 10. Final feature list + target ----------------------------------
  ## 16 granular role dummies + 4 broad-bucket fallbacks
  role_dummies <- paste0("is_", tolower(role_levels))
  feature_cols <- c(
    "panna", "p_off", "p_def",
    role_dummies, "is_gkb", "is_defb", "is_midb", "is_fwdb",
    "intl_mins_30d", "intl_mins_180d", "intl_mins_540d",
    "intl_apps_180d", "intl_starts_180d",
    "club_mins_30d", "club_mins_90d", "club_starts_90d",
    "days_since_last_intl", "days_since_last_club",
    "career_intl_apps", "caps_decay", "p_start_decay",
    "days_rest_team",
    "is_tournament", "is_friendly", "tournament_match_num",
    "tourn_mins_sofar", "tourn_starts_sofar",
    "started_prev_team_match", "mins_prev_team_match",
    "rotation_idx", "team_intl_count"
  )
  feature_cols <- intersect(feature_cols, names(train))
  data.table::setattr(train, "feature_cols", feature_cols)
  train[]
}
