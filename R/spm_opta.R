# Opta SPM (Statistical Plus-Minus) model functions
#
# Functions for building SPM models from Opta/TheAnalyst data.
# Opta data has 263 columns with different naming conventions than FBref.
# Depends on spm_model.R for core model fitting functions.


# ============================================================================
# Internal helpers for aggregate_opta_stats
# ============================================================================

#' Get Opta column mapping (new_name = opta_column_name)
#' @return Named character vector mapping panna names to Opta column names
#' @keywords internal
.get_opta_col_mapping <- function() {
  c(
    total_minutes = "minsPlayed",
    goals = "goals", goals_openplay = "goalsOpenplay",
    shots = "totalScoringAtt", shots_ibox = "attemptsIbox",
    shots_obox = "attemptsObox", shots_on_target = "ontargetScoringAtt",
    shot_off_target = "shotOffTarget", shots_blocked = "blockedScoringAtt",
    big_chance_created = "bigChanceCreated", big_chance_scored = "bigChanceScored",
    big_chance_missed = "bigChanceMissed",
    assists = "goalAssist", assists_openplay = "goalAssistOpenplay",
    assists_setplay = "goalAssistSetplay", second_assists = "secondGoalAssist",
    total_att_assist = "totalAttAssist", on_target_att_assist = "ontargetAttAssist",
    passes = "totalPass", passes_accurate = "accuratePass",
    final_third_passes = "totalFinalThirdPasses",
    final_third_passes_acc = "successfulFinalThirdPasses",
    long_balls = "totalLongBalls", long_balls_acc = "accurateLongBalls",
    through_balls = "totalThroughBall", through_balls_acc = "accurateThroughBall",
    crosses = "totalCross", crosses_acc = "accurateCross",
    key_passes = "putThrough", key_passes_success = "successfulPutThrough",
    backward_pass = "backwardPass", forward_pass = "fwdPass",
    layoffs = "totalLayoffs", layoffs_acc = "accurateLayoffs",
    tackles = "totalTackle", tackles_won = "wonTackle",
    interceptions = "interception", interceptions_won = "interceptionWon",
    interceptions_ibox = "interceptionsInBox",
    clearances = "totalClearance", clearances_effective = "effectiveClearance",
    head_clearances = "headClearance",
    head_clearances_effective = "effectiveHeadClearance",
    blocks = "outfielderBlock", blocked_passes = "blockedPass",
    blocked_crosses = "blockedCross",
    duel_won = "duelWon", duel_lost = "duelLost",
    aerial_won = "aerialWon", aerial_lost = "aerialLost",
    contest_won = "wonContest", contest_total = "totalContest",
    challenge_lost = "challengeLost",
    touches = "touches", touches_opp_box = "touchesInOppBox",
    poss_won_def3rd = "possWonDef3rd", poss_won_mid3rd = "possWonMid3rd",
    poss_won_att3rd = "possWonAtt3rd", ball_recovery = "ballRecovery",
    dispossessed = "dispossessed", turnover = "turnover",
    poss_lost_all = "possLostAll", times_tackled = "timesTackled",
    fouls = "fouls", was_fouled = "wasFouled",
    fouled_final_third = "fouledFinalThird",
    yellow_cards = "yellowCard", red_cards = "redCard",
    second_yellows = "secondYellow",
    corners_taken = "cornerTaken", corners_won = "wonCorners",
    corners_lost = "lostCorners", corners_intobox = "totalCornersIntobox",
    corners_intobox_acc = "accurateCornersIntobox",
    freekick_crosses = "freekickCross",
    saves = "saves", saves_ibox = "savedIbox", saves_obox = "savedObox",
    goals_conceded = "goalsConceded",
    goals_conceded_ibox = "goalsConcededIbox",
    high_claim = "totalHighClaim", good_high_claim = "goodHighClaim",
    punches = "punches", keeper_throws = "keeperThrows",
    keeper_throws_acc = "accurateKeeperThrows",
    fwd_zone_pass = "totalFwdZonePass",
    fwd_zone_pass_acc = "accurateFwdZonePass",
    open_play_pass = "openPlayPass",
    open_play_pass_acc = "successfulOpenPlayPass",
    error_lead_to_shot = "errorLeadToShot",
    error_lead_to_goal = "errorLeadToGoal",
    att_fastbreak = "attFastbreak", shot_fastbreak = "shotFastbreak",
    att_openplay = "attOpenplay", att_setpiece = "attSetpiece",
    att_headed = "attHdTotal", att_headed_goal = "attHdGoal",
    att_one_on_one = "attOneOnOne",
    crosses_open_play = "totalCrossNocorner",
    crosses_open_play_acc = "accurateCrossNocorner",
    penalty_won = "penaltyWon", penalty_conceded = "penaltyConceded",
    offtarget_att_assist = "offtargetAttAssist",
    last_man_tackle = "lastManTackle", six_yard_block = "sixYardBlock",
    clearance_off_line = "clearanceOffLine",
    keeper_sweeper = "totalKeeperSweeper",
    keeper_sweeper_acc = "accurateKeeperSweeper",
    attempts_conceded_ibox = "attemptsConcededIbox",
    attempts_conceded_obox = "attemptsConcededObox",
    gk_smother = "gkSmother",
    unsuccessful_touch = "unsuccessfulTouch", overrun = "overrun",
    flick_on = "totalFlickOn", flick_on_acc = "accurateFlickOn",
    offsides = "totalOffside", offside_provoked = "offsideProvoked",
    pen_area_entries = "penAreaEntries",
    final_third_entries = "finalThirdEntries",
    pull_backs = "totalPullBack", pull_backs_acc = "accuratePullBack",
    # Round 2
    back_zone_pass = "totalBackZonePass",
    back_zone_pass_acc = "accurateBackZonePass",
    chipped_pass = "totalChippedPass",
    chipped_pass_acc = "accurateChippedPass",
    att_rf_total = "attRfTotal", att_lf_total = "attLfTotal",
    att_ibox_goal = "attIboxGoal", att_obox_goal = "attOboxGoal",
    att_ibox_target = "attIboxTarget", att_obox_target = "attOboxTarget",
    hit_woodwork = "hitWoodwork",
    att_pen_goal = "attPenGoal", att_pen_miss = "attPenMiss",
    pen_goals_conceded = "penGoalsConceded",
    keeper_pickup = "keeperPickUp", poss_lost_ctrl = "possLostCtrl",
    long_pass_own_to_opp = "longPassOwnToOpp",
    long_pass_own_to_opp_acc = "longPassOwnToOppSuccess",
    fifty_fifty = "fiftyFifty", fifty_fifty_won = "successfulFiftyFifty"
  )
}


#' Calculate per-90 rates for Opta player stats
#' @param player_stats Data frame with aggregated counting stats
#' @return player_stats with per-90 columns added
#' @keywords internal
.calculate_opta_per90 <- function(player_stats) {
  mins_per_90 <- player_stats$total_minutes / 90
  player_stats$mins_per_90 <- mins_per_90

  # All counting stats to convert to per-90
  p90_cols <- c(
    "goals", "shots", "shots_on_target", "shots_ibox", "shots_obox",
    "big_chance_scored", "big_chance_missed",
    "assists", "big_chance_created", "total_att_assist", "key_passes",
    "through_balls",
    "passes", "passes_accurate", "final_third_passes", "long_balls",
    "crosses", "forward_pass",
    "tackles", "tackles_won", "interceptions", "interceptions_won",
    "clearances", "clearances_effective", "blocks", "blocked_passes",
    "duel_won", "duel_lost", "aerial_won", "aerial_lost",
    "touches", "touches_opp_box", "poss_won_def3rd", "poss_won_mid3rd",
    "poss_won_att3rd", "ball_recovery", "dispossessed", "turnover",
    "times_tackled",
    "corners_taken", "corners_won", "pen_area_entries", "final_third_entries",
    "fouls", "was_fouled",
    "saves", "goals_conceded",
    "fwd_zone_pass", "open_play_pass",
    "error_lead_to_shot", "error_lead_to_goal",
    "att_fastbreak", "shot_fastbreak",
    "att_openplay", "att_setpiece", "att_headed", "att_one_on_one",
    "crosses_open_play",
    "penalty_won", "penalty_conceded",
    "offtarget_att_assist",
    "last_man_tackle", "six_yard_block", "clearance_off_line",
    "keeper_sweeper", "attempts_conceded_ibox", "attempts_conceded_obox",
    "gk_smother",
    "saves_ibox", "saves_obox",
    "high_claim", "good_high_claim", "punches",
    "keeper_throws",
    "unsuccessful_touch", "overrun",
    "flick_on",
    # Round 2
    "back_zone_pass", "chipped_pass",
    "att_rf_total", "att_lf_total",
    "att_ibox_goal", "att_obox_goal", "att_ibox_target", "att_obox_target",
    "hit_woodwork",
    "att_pen_goal", "att_pen_miss", "pen_goals_conceded",
    "keeper_pickup", "poss_lost_ctrl", "long_pass_own_to_opp",
    "fifty_fifty", "fifty_fifty_won"
  )

  for (col in p90_cols) {
    x <- .safe_col(player_stats, col)
    p90_val <- x / mins_per_90
    p90_val[!is.finite(p90_val)] <- 0
    player_stats[[paste0(col, "_p90")]] <- p90_val
  }

  player_stats
}


#' Calculate derived features (success rates and ratios) for Opta stats
#' @param player_stats Data frame with counting stats and per-90 rates
#' @return player_stats with derived feature columns added
#' @keywords internal
.calculate_opta_derived_features <- function(player_stats) {
  mins_per_90 <- player_stats$total_minutes / 90
  sc <- function(col_name) .safe_col(player_stats, col_name)
  sdiv <- function(num, denom) safe_divide(num, denom, default = 0)

  # Shooting efficiency
  player_stats$shot_accuracy <- sdiv(sc("shots_on_target"), sc("shots"))
  player_stats$goals_per_shot <- sdiv(sc("goals"), sc("shots"))
  player_stats$ibox_shot_ratio <- sdiv(sc("shots_ibox"), sc("shots"))
  player_stats$big_chance_conversion <- sdiv(
    sc("big_chance_scored"),
    sc("big_chance_scored") + sc("big_chance_missed")
  )

  # Passing efficiency
  player_stats$pass_accuracy <- sdiv(sc("passes_accurate"), sc("passes"))
  player_stats$final_third_pass_acc <- sdiv(
    sc("final_third_passes_acc"), sc("final_third_passes")
  )
  player_stats$long_ball_accuracy <- sdiv(sc("long_balls_acc"), sc("long_balls"))
  player_stats$through_ball_accuracy <- sdiv(
    sc("through_balls_acc"), sc("through_balls")
  )
  player_stats$cross_accuracy <- sdiv(sc("crosses_acc"), sc("crosses"))

  # Defensive success rates
  player_stats$tackle_success <- sdiv(sc("tackles_won"), sc("tackles"))
  player_stats$interception_success <- sdiv(
    sc("interceptions_won"), sc("interceptions")
  )
  player_stats$clearance_effectiveness <- sdiv(
    sc("clearances_effective"), sc("clearances")
  )

  # Duel success rates
  player_stats$duel_success <- sdiv(
    sc("duel_won"), sc("duel_won") + sc("duel_lost")
  )
  player_stats$aerial_success <- sdiv(
    sc("aerial_won"), sc("aerial_won") + sc("aerial_lost")
  )

  # Possession balance
  total_poss_won <- sc("poss_won_def3rd") + sc("poss_won_mid3rd") +
    sc("poss_won_att3rd")
  player_stats$poss_won_total_p90 <- total_poss_won / mins_per_90
  player_stats$poss_won_att_ratio <- sdiv(sc("poss_won_att3rd"), total_poss_won)

  # Ball retention
  player_stats$turnovers_p90 <- (sc("dispossessed") + sc("turnover")) / mins_per_90
  player_stats$foul_differential_p90 <- player_stats$was_fouled_p90 - player_stats$fouls_p90

  # Progressive passing accuracy
  player_stats$fwd_zone_pass_accuracy <- sdiv(
    sc("fwd_zone_pass_acc"), sc("fwd_zone_pass")
  )
  player_stats$open_play_pass_accuracy <- sdiv(
    sc("open_play_pass_acc"), sc("open_play_pass")
  )

  # Open-play cross accuracy
  player_stats$crosses_open_play_accuracy <- sdiv(
    sc("crosses_open_play_acc"), sc("crosses_open_play")
  )

  # Touch quality rate
  player_stats$bad_touch_rate <- sdiv(
    sc("unsuccessful_touch") + sc("overrun"), sc("touches")
  )

  # Error total per-90
  player_stats$errors_total_p90 <- (sc("error_lead_to_shot") +
    sc("error_lead_to_goal")) / mins_per_90

  # Headed goal rate
  player_stats$headed_goal_rate <- sdiv(sc("att_headed_goal"), sc("att_headed"))

  # Flick-on accuracy
  player_stats$flick_on_accuracy <- sdiv(sc("flick_on_acc"), sc("flick_on"))

  # GK sweeper accuracy
  player_stats$keeper_sweeper_accuracy <- sdiv(
    sc("keeper_sweeper_acc"), sc("keeper_sweeper")
  )

  # Round 2 derived features
  player_stats$back_zone_pass_accuracy <- sdiv(
    sc("back_zone_pass_acc"), sc("back_zone_pass")
  )
  player_stats$chipped_pass_accuracy <- sdiv(
    sc("chipped_pass_acc"), sc("chipped_pass")
  )
  player_stats$ibox_goal_rate <- sdiv(sc("att_ibox_goal"), sc("shots_ibox"))
  player_stats$obox_goal_rate <- sdiv(sc("att_obox_goal"), sc("shots_obox"))
  player_stats$penalty_conversion <- sdiv(
    sc("att_pen_goal"), sc("att_pen_goal") + sc("att_pen_miss")
  )
  player_stats$long_pass_own_to_opp_accuracy <- sdiv(
    sc("long_pass_own_to_opp_acc"), sc("long_pass_own_to_opp")
  )
  player_stats$fifty_fifty_success <- sdiv(
    sc("fifty_fifty_won"), sc("fifty_fifty")
  )
  player_stats$poss_lost_ctrl_per_touch <- sdiv(
    sc("poss_lost_ctrl"), sc("touches")
  )

  # Goalkeeper metrics
  shots_faced <- sc("saves") + sc("goals_conceded")
  player_stats$save_percentage <- sdiv(sc("saves"), shots_faced)
  player_stats$keeper_throws_accuracy <- sdiv(
    sc("keeper_throws_acc"), sc("keeper_throws")
  )

  # Position dummies
  if ("primary_position" %in% names(player_stats)) {
    pos <- player_stats$primary_position
    player_stats$is_gk <- as.integer(grepl("Goalkeeper", pos, ignore.case = TRUE))
    player_stats$is_df <- as.integer(grepl("Defender", pos, ignore.case = TRUE))
    player_stats$is_mf <- as.integer(grepl("Midfielder", pos, ignore.case = TRUE))
    player_stats$is_fw <- as.integer(grepl("Forward|Striker", pos, ignore.case = TRUE))
  }

  player_stats
}


# ============================================================================
# Shared column preparation helpers
# ============================================================================

#' Ensure player_id column exists in an Opta data.table
#' @param dt data.table with player data
#' @param fn_name Character function name for warning messages
#' @return dt (modified by reference)
#' @keywords internal
.ensure_player_id <- function(dt, fn_name = "spm_opta") {
  if (!"player_id" %in% names(dt)) {
    dt[, player_id := clean_player_name(player_name)]
  } else {
    n_na <- sum(is.na(dt$player_id))
    if (n_na > 0) {
      cli::cli_warn("{.fn {fn_name}}: {n_na}/{nrow(dt)} rows have NA {.field player_id}, using {.fn clean_player_name} for those rows.")
      na_mask <- is.na(dt$player_id)
      dt[na_mask, player_id := clean_player_name(player_name)]
    }
  }
  dt
}


#' Rename Opta columns to panna names using the standard mapping
#' @param dt data.table or data.frame
#' @return The input with columns renamed
#' @keywords internal
.rename_opta_columns <- function(dt) {
  opta_cols <- .get_opta_col_mapping()
  existing_cols <- opta_cols[opta_cols %in% names(dt)]
  if (length(existing_cols) > 0) {
    data.table::setnames(dt, old = unname(existing_cols),
                         new = names(existing_cols), skip_absent = TRUE)
  }
  existing_cols
}


#' Replace NA/Inf with 0 in numeric columns and log summary
#' @param df data.frame with numeric columns
#' @param check_inf Whether to also replace Inf values (default TRUE)
#' @return df with NAs/Inf replaced
#' @keywords internal
.clean_numeric_na <- function(df, check_inf = TRUE) {
  numeric_cols <- vapply(df, is.numeric, logical(1))
  if (!any(numeric_cols)) return(df)

  na_counts <- vapply(df[numeric_cols], function(x) {
    sum(is.na(x) | (check_inf & is.infinite(x)))
  }, integer(1))
  n_replaced <- sum(na_counts)

  if (n_replaced > 0) {
    df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
      bad <- is.na(x) | (check_inf & is.infinite(x))
      x[bad] <- 0
      x
    })
    total_cells <- sum(numeric_cols) * nrow(df)
    pct <- round(n_replaced / total_cells * 100, 1)
    top_cols <- head(names(sort(na_counts[na_counts > 0], decreasing = TRUE)), 5)
    msg <- sprintf("Replaced %d NA%s values with 0 (%.1f%%, top: %s)",
                   n_replaced, if (check_inf) "/Inf" else "",
                   pct, paste(top_cols, collapse = ", "))
    if (pct > 5) cli::cli_warn(msg) else progress_msg(msg)
  }
  df
}


# ============================================================================
# Match-level Opta stats (for estimated skills pipeline)
# ============================================================================

#' Compute match-level Opta statistics with per-90 rates
#'
#' Like \code{aggregate_opta_stats()} but preserves individual player-match rows
#' instead of aggregating across matches. Each row gets per-90 rates and derived
#' features computed from that single match's data. Used as input for the
#' estimated skills pipeline where decay-weighted averaging replaces aggregation.
#'
#' @param opta_stats Data frame from \code{load_opta_stats()}, one row per
#'   player-match. Must contain \code{match_id}, \code{match_date},
#'   \code{player_name}, \code{team_name}, \code{team_position}, and
#'   \code{minsPlayed}.
#' @param min_minutes Minimum minutes in a single match for inclusion (default 10).
#'   Filters out very short cameos where per-90 rates are unreliable.
#'
#' @return A data.table with one row per player-match containing:
#'   \itemize{
#'     \item Identity columns: match_id, match_date, player_id, player_name,
#'       team_name, position, competition, season
#'     \item Context columns: opponent_team, is_home
#'     \item Minutes: total_minutes
#'     \item Per-90 rate columns (same names as aggregate_opta_stats output)
#'     \item Derived efficiency columns (same names as aggregate_opta_stats output)
#'   }
#'
#' @family spm opta
#' @export
#' @examples
#' \dontrun{
#' opta_stats <- load_opta_stats("ENG", "2024-2025")
#' match_stats <- compute_match_level_opta_stats(opta_stats)
#' }
compute_match_level_opta_stats <- function(opta_stats, min_minutes = 10) {
  if (is.null(opta_stats) || nrow(opta_stats) == 0) {
    cli::cli_warn("No Opta stats provided.")
    return(NULL)
  }

  progress_msg(sprintf("Computing match-level stats for %d player-match rows...",
                        nrow(opta_stats)))

  dt <- data.table::copy(data.table::as.data.table(opta_stats))

  .ensure_player_id(dt, "compute_match_level_opta_stats")
  .rename_opta_columns(dt)

  # Ensure total_minutes exists
  if (!"total_minutes" %in% names(dt) && "minsPlayed" %in% names(dt)) {
    dt[, total_minutes := as.numeric(minsPlayed)]
  }

  # Filter by minimum minutes
  dt <- dt[!is.na(total_minutes) & total_minutes >= min_minutes]
  if (nrow(dt) == 0) {
    cli::cli_warn("No rows remaining after filtering by min_minutes = {min_minutes}.")
    return(NULL)
  }

  # Derive context columns
  if ("team_position" %in% names(dt)) {
    dt[, is_home := as.integer(tolower(team_position) == "home")]
  }

  # Derive opponent_team from lineups within same match
  if ("match_id" %in% names(dt) && "team_name" %in% names(dt)) {
    # Build match-team lookup
    match_teams <- unique(dt[, .(match_id, team_name)])
    # For each match, find the other team
    opponent_lookup <- match_teams[match_teams, on = .(match_id),
                                   allow.cartesian = TRUE][team_name != i.team_name]
    data.table::setnames(opponent_lookup, c("team_name", "i.team_name"),
                         c("opponent_team", "team_name"))
    # Some matches may have > 2 teams (shouldn't happen); take first
    opponent_lookup <- opponent_lookup[, .SD[1], by = .(match_id, team_name)]
    dt <- opponent_lookup[, .(match_id, team_name, opponent_team)][dt, on = .(match_id, team_name)]
  }

  # Add competition/league if available
  if (!"competition" %in% names(dt) && "league" %in% names(dt)) {
    data.table::setnames(dt, "league", "competition", skip_absent = TRUE)
  }

  # Convert to data.frame for the helper functions (they use $ and [[ ]])
  df <- data.table::setDF(data.table::copy(dt))

  # Compute per-90 rates
  df <- .calculate_opta_per90(df)

  # Compute derived features
  df <- .calculate_opta_derived_features(df)

  # Replace NAs/Inf with 0 in numeric columns (required for glmnet)
  df <- .clean_numeric_na(df, check_inf = TRUE)

  result <- data.table::as.data.table(df)

  progress_msg(sprintf("Computed match-level stats: %d rows, %d features",
                        nrow(result), ncol(result)))
  result
}


# ============================================================================
# Main Opta stats aggregation
# ============================================================================

#' Aggregate Opta player statistics to per-90 rates
#'
#' Combines match-level Opta statistics into per-90-minute rates for each player.
#' Creates comprehensive features for SPM modeling from Opta's 263 columns.
#'
#' @param opta_stats Data frame from load_opta_stats()
#' @param min_minutes Minimum total minutes for inclusion (default 450)
#'
#' @return Data frame with per-90 rates for each player
#' @keywords internal
#' @examples
#' \dontrun{
#' opta_stats <- load_opta_stats("ENG", "2024-2025")
#' player_features <- aggregate_opta_stats(opta_stats, min_minutes = 450)
#' }
aggregate_opta_stats <- function(opta_stats, min_minutes = 450) {
  if (is.null(opta_stats) || nrow(opta_stats) == 0) {
    cli::cli_warn("No Opta stats provided.")
    return(NULL)
  }

  progress_msg(sprintf("Aggregating %d Opta player-match rows...", nrow(opta_stats)))

  opta_dt <- data.table::as.data.table(opta_stats)
  .ensure_player_id(opta_dt, "aggregate_opta_stats")

  # Get column mapping and filter to existing columns (before rename for aggregation)
  opta_cols <- .get_opta_col_mapping()
  existing_cols <- opta_cols[opta_cols %in% names(opta_dt)]

  # Sum numeric columns + count matches in one pass
  player_stats <- opta_dt[, c(
    lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
    list(n_matches = data.table::uniqueN(match_id))
  ), by = player_id, .SDcols = existing_cols]
  data.table::setnames(player_stats, old = existing_cols,
                       new = names(existing_cols), skip_absent = TRUE)

  # Get canonical player_name (modal name per player)
  cleaned_names_vec <- trimws(gsub("\u00A0", " ", opta_dt$player_name))
  opta_dt[, clean_name := cleaned_names_vec]
  name_lookup <- opta_dt[, {
    tbl <- table(clean_name)
    list(player_name = names(tbl)[which.max(tbl)])
  }, by = player_id]
  player_stats <- name_lookup[player_stats, on = "player_id"]
  opta_dt[, clean_name := NULL]

  # Get primary position
  if ("position" %in% names(opta_dt)) {
    pos_mode <- opta_dt[!is.na(position) & position != "", {
      tbl <- table(position)
      list(primary_position = names(tbl)[which.max(tbl)])
    }, by = player_id]
    player_stats <- pos_mode[player_stats, on = "player_id"]
  }

  data.table::setDF(player_stats)

  # Filter by minimum minutes
  player_stats <- player_stats[player_stats$total_minutes >= min_minutes, ]
  if (nrow(player_stats) == 0) {
    cli::cli_warn(c(
      "No players meet minimum minutes threshold.",
      "i" = "Current threshold: {min_minutes} minutes."
    ))
    return(NULL)
  }

  # Calculate per-90 rates and derived features
  player_stats <- .calculate_opta_per90(player_stats)
  player_stats <- .calculate_opta_derived_features(player_stats)

  # Replace NAs with 0 (required for glmnet/SPM model fitting)
  player_stats <- .clean_numeric_na(player_stats, check_inf = FALSE)

  progress_msg(sprintf("Aggregated Opta stats for %d players with %d features",
                       nrow(player_stats), ncol(player_stats)))

  player_stats
}


#' Canonical list of above-expected xMetrics columns SPM enrichment can produce
#'
#' The full set of \verb{*_per90} column names \code{.aggregate_xmetrics_for_spm()}
#' MAY produce, independent of which are derivable from a given xmetrics
#' vintage. Callers that need to guarantee a fixed column set on the output
#' data frame (e.g. so a model's \code{predictor_cols} always resolves,
#' even for a season/subset with zero coverage of some columns) should
#' ensure exactly this list exists, defaulting missing ones to 0 — see
#' \code{.spm_opta_predictor_cols()} for why 0 is the correct fallback
#' (population mean for mean-zero above-expected metrics).
#'
#' @return Character vector of canonical `*_per90` column names
#' @keywords internal
.spm_xmetrics_per90_cols <- function() {
  c("xg_per90", "npxg_per90", "xa_per90_xmetrics",
    "xpass_overperformance_per90_xmetrics",
    "aerial_woe_per90", "aerial_poss_woe_per90", "takeon_woe_per90",
    "tackle_poss_woe_per90", "containment_woe_per90",
    "npg_minus_npxg_per90", "ibox_g_minus_xg_per90", "obox_g_minus_xg_per90",
    "placement_added_per90", "gsaa_per90")
}


#' Aggregate an xMetrics table to player-level per-90 SPM features
#'
#' THE ONE implementation of "given an xmetrics table (any subset — full
#' history or a single season), compute player-level per-90 above-expected
#' features for SPM enrichment." Extracted 2026-07-08 (panna#87) after this
#' exact logic was independently duplicated in \code{05_spm.R} (all-history
#' SPM fit) and \code{07_seasonal_ratings.R} (per-season SPM breakdown) —
#' the second script's copy never got the xDuel WOE / finishing
#' over-performance columns added to the first, so a season-level
#' \code{calculate_spm_ratings()} call errored with "undefined columns
#' selected" the moment the fitted model's \code{predictor_cols} included
#' any of them (every one of 14 seasons failed identically on the first
#' cloud run after the SPM modernization shipped). One implementation
#' closes the class of bug, not just this instance.
#'
#' @param xmetrics Data frame with (at least) \code{player_id}, \code{minutes},
#'   \code{xg}, \code{npxg}, \code{xa}, \code{xpass_overperformance}, plus
#'   whichever above-expected columns this vintage carries (schema-defensive
#'   — the five xDuel WOE columns, finishing over-performance, placement,
#'   gsaa; see \code{.spm_xmetrics_per90_cols()}).
#' @return Data frame keyed by \code{player_id} with whichever of
#'   \code{.spm_xmetrics_per90_cols()} are derivable from \code{xmetrics}
#'   (fewer columns for an older vintage or a thin season/subset — the
#'   caller ensures the full canonical set exists before modeling).
#' @keywords internal
.aggregate_xmetrics_for_spm <- function(xmetrics) {
  xm_extra_totals <- intersect(
    c("aerial_woe", "aerial_poss_woe", "takeon_woe",
      "tackle_poss_woe", "containment_woe",
      "npg_minus_npxg", "ibox_g_minus_xg", "obox_g_minus_xg",
      "placement_added", "gsaa"),
    names(xmetrics)
  )

  # Per-90 denominator for the above-expected columns is the player's COVERED
  # minutes (rows where that column is non-NA), not all xmetrics minutes —
  # an all-minutes denominator dilutes a player with partial coverage toward
  # 0 (panna#87 review finding). xg/npxg/xa don't need this: uncovered rows
  # have no xmetrics row at all.
  xmetrics_agg <- xmetrics %>%
    dplyr::group_by(player_id) %>%
    dplyr::summarise(
      xg_total = sum(xg, na.rm = TRUE),
      npxg_total = sum(npxg, na.rm = TRUE),
      xa_total = sum(xa, na.rm = TRUE),
      xmetrics_minutes = sum(minutes, na.rm = TRUE),
      xpass_overperformance_total = sum(xpass_overperformance, na.rm = TRUE),
      dplyr::across(dplyr::all_of(xm_extra_totals), ~ sum(.x, na.rm = TRUE),
                    .names = "{.col}_total"),
      dplyr::across(dplyr::all_of(xm_extra_totals), ~ sum(minutes[!is.na(.x)], na.rm = TRUE),
                    .names = "{.col}_covmins"),
      .groups = "drop"
    ) %>%
    dplyr::filter(xmetrics_minutes > 0) %>%
    dplyr::mutate(
      xg_per90 = xg_total / xmetrics_minutes * 90,
      npxg_per90 = npxg_total / xmetrics_minutes * 90,
      xa_per90_xmetrics = xa_total / xmetrics_minutes * 90,
      xpass_overperformance_per90_xmetrics = xpass_overperformance_total / xmetrics_minutes * 90
    )
  for (tot in xm_extra_totals) {
    covmins <- xmetrics_agg[[paste0(tot, "_covmins")]]
    xmetrics_agg[[paste0(tot, "_per90")]] <-
      ifelse(covmins > 0,
             xmetrics_agg[[paste0(tot, "_total")]] / covmins * 90,
             0)  # zero coverage -> population mean (0 for above-expected)
  }

  # recycle0 makes paste0 propagate zero-length inputs (default
  # paste0(character(0), x) returns x — the bogus-column-name trap).
  xm_cols <- c("xg_per90", "npxg_per90", "xa_per90_xmetrics",
               "xpass_overperformance_per90_xmetrics",
               paste0(xm_extra_totals, "_per90", recycle0 = TRUE))
  xmetrics_agg %>% dplyr::select(dplyr::all_of(c("player_id", xm_cols)))
}


#' Canonical SPM-Opta predictor selection
#'
#' The ONE place the Opta SPM feature set is defined, shared by
#' \code{fit_spm_opta()} (glmnet half) and passed explicitly to
#' \code{fit_spm_xgb()} by \code{05_spm.R} (XGBoost half) so the two halves
#' of the shipped 50/50 blend can never train on divergent feature sets —
#' the failure mode behind the "SPM was xG-blind" bug, where the enrichment
#' join was dead code because the fit-time grep didn't match the joined
#' column names.
#'
#' Selection: all per-90 rates, BOTH suffix spellings (`_p90` box-score,
#' `_per90` xMetrics model outputs) + the `_xmetrics`-suffixed pair + the
#' kept efficiency ratios + position dummies. The ratios with a direct
#' above-expected replacement were REMOVED (mirrors the PSR/PSV redesign,
#' panna#116): duel/aerial/tackle success -> the five *_woe_per90 counts;
#' goals_per_shot / big_chance_conversion / headed_goal_rate /
#' ibox/obox_goal_rate / penalty_conversion -> the finishing over-performance
#' family (scale-free ratios discard volume: 1/1 == 10/10). Ratios WITHOUT a
#' modeled replacement (zone pass accuracies, bad touches, 50/50s, possession
#' control) are kept.
#'
#' @param data Data frame of candidate features
#' @return Character vector of predictor column names present in `data`
#' @keywords internal
.spm_opta_predictor_cols <- function(data) {
  predictor_cols <- names(data)[grepl("_p90$|_per90$", names(data))]
  xm_suffixed <- c("xa_per90_xmetrics", "xpass_overperformance_per90_xmetrics")
  success_cols <- c("shot_accuracy", "pass_accuracy",
                    "final_third_pass_acc",
                    "long_ball_accuracy", "cross_accuracy",
                    "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
                    "crosses_open_play_accuracy", "bad_touch_rate",
                    "keeper_sweeper_accuracy", "errors_total_p90",
                    "flick_on_accuracy",
                    "back_zone_pass_accuracy", "chipped_pass_accuracy",
                    "long_pass_own_to_opp_accuracy",
                    "fifty_fifty_success", "poss_lost_ctrl_per_touch")
  pos_cols <- c("is_gk", "is_df", "is_mf", "is_fw")
  unique(c(predictor_cols,
           intersect(c(xm_suffixed, success_cols, pos_cols), names(data))))
}

#' Build league fixed-effect dummy columns for SPM
#'
#' SPM maps per-90 box-score rates onto RAPM. RAPM is already opponent-adjusted
#' at player level (the design matrix carries \code{_off}/\code{_def} columns for
#' both teams) and league-season centred, but the box-score rates it is regressed
#' on are neither. What survives is residual stat inflation: the same per-90 line
#' means less in a weaker league. Measured 2026-09-02 on 22,755 players, the
#' fitted league effect spans **0.95 sd of RAPM** end to end (EPL to CAF_CL),
#' worth ~3.3\% RMSE — and without it Saudi Arabia supplies 5 of SPM's top 20,
#' as many as the Bundesliga (9 EPL / 2 Saudi once the term is added).
#'
#' Dummies are 0/1 with one level held out as the reference, so an **unseen
#' league at predict time gets all-zero dummies and falls back to that reference**
#' rather than erroring. That is the deliberate behaviour: a new competition
#' should be priced as the reference league, not dropped.
#'
#' @param data Data frame carrying a \code{competition} or \code{league} column.
#' @param levels Character vector of league levels from the fitted model. When
#'   \code{NULL} they are derived from \code{data} (levels with at least
#'   \code{min_n} rows), which is the fit-time path.
#' @param min_n Minimum rows for a league to get its own dummy (default 50).
#'   Thinner leagues fold into the reference level rather than fitting a
#'   coefficient on a handful of players.
#' @return A list with \code{data} (input plus dummy columns), \code{levels}
#'   (the non-reference levels, for storing in model metadata) and \code{cols}
#'   (the dummy column names).
#' @keywords internal
# Minutes-share league controls for a player-grain SPM frame.
#
# Why shares and not dummies. Stage 2's SPM aggregates 3.46M player-match rows
# to one row per player, and NO league column survives that aggregation, so a
# league fixed effect there needs a league to be DERIVED per player. Measured on
# 48,377 players: only 55.1% appear in a single competition, 12.0% have no
# competition holding even 60% of their minutes, and 0.3% are exact ties decided
# by nothing at all. The median dominant share is 1.000 -- the same reassuring
# statistic that said panna#222's inputs were fine while its league tag was a
# coin-flip.
#
# A share vector removes the decision instead of making it badly: a player on
# 70% EPL / 30% UCL gets 0.7 and 0.3, and for the 55% in one competition it
# degenerates exactly to the dummy it replaces. Shares sum to 1, so one column
# is dropped as the reference to avoid collinearity with the intercept.
#
# `stats` must be player-match grain with player_id, a competition column and a
# minutes column. Returns one row per player_id.
.spm_league_shares <- function(stats, min_n = 50, prefix = "lgshare_") {
  dt <- data.table::as.data.table(stats)
  lg <- intersect(c("competition", "league"), names(dt))[1]
  mn <- intersect(c("minsPlayed", "minutes", "minutes_played", "mins"), names(dt))[1]
  if (is.na(lg) || is.na(mn) || !"player_id" %in% names(dt)) {
    return(list(data = NULL, cols = character(0), levels = character(0)))
  }
  ## Coverage, not presence. `minsPlayed` is ~62.6% non-NA on the Opta stats
  ## table, and rows without it are dropped here -- so if that missingness were
  ## concentrated in a few competitions, those leagues' shares would be built
  ## from a biased slice while every column still looked populated. Report the
  ## per-competition retention rather than assume it is uniform.
  n0 <- nrow(dt)
  cov <- dt[, .(rows = .N,
                usable = sum(!is.na(get(mn)) & as.numeric(get(mn)) > 0)),
            by = c(lg)]
  cov[, pct := 100 * usable / rows]
  thin <- cov[pct < 50]
  if (nrow(thin) > 0) {
    cli::cli_alert_warning(
      "League shares: {nrow(thin)} competition{?s} under 50% minutes coverage: {paste(sprintf('%s %.0f%%', thin[[lg]], thin$pct), collapse=', ')}")
  }
  dt <- dt[!is.na(get(lg)) & !is.na(get(mn)) & as.numeric(get(mn)) > 0]
  cli::cli_alert_info("League shares: {nrow(dt)} of {n0} rows usable ({round(100*nrow(dt)/n0, 1)}%), {nrow(cov)} competitions")
  if (!nrow(dt)) return(list(data = NULL, cols = character(0), levels = character(0)))

  keep <- dt[, .(n = .N), by = c(lg)][n >= min_n][[lg]]
  if (length(keep) < 2) return(list(data = NULL, cols = character(0), levels = character(0)))
  dt[, .lg := data.table::fifelse(get(lg) %in% keep, as.character(get(lg)), "OTHER")]

  m <- dt[, .(mins = sum(as.numeric(get(mn)))), by = .(player_id, .lg)]
  m[, share := mins / sum(mins), by = player_id]
  w <- data.table::dcast(m, player_id ~ .lg, value.var = "share", fill = 0)

  ## Drop the largest competition as the reference level -- shares sum to 1, so
  ## keeping every column would be perfectly collinear with the intercept.
  lv <- setdiff(names(w), "player_id")
  ref <- lv[which.max(vapply(lv, function(k) sum(w[[k]]), numeric(1)))]
  lv <- setdiff(lv, ref)
  data.table::setnames(w, lv, paste0(prefix, make.names(lv)))
  w[, (ref) := NULL]
  list(data = as.data.frame(w),
       cols = paste0(prefix, make.names(lv)),
       levels = lv, reference = ref)
}


.spm_league_dummies <- function(data, levels = NULL, min_n = 50) {
  league_col <- intersect(c("competition", "league"), names(data))[1]
  if (is.na(league_col)) {
    return(list(data = data, levels = character(0), cols = character(0)))
  }
  lg <- as.character(data[[league_col]])
  lg[is.na(lg) | lg == ""] <- "__unknown__"

  if (is.null(levels)) {
    tab <- table(lg)
    keep <- names(tab)[tab >= min_n]
    keep <- setdiff(keep, "__unknown__")
    if (length(keep) < 2) {
      return(list(data = data, levels = character(0), cols = character(0)))
    }
    ## Reference = the largest league, so the held-out level is the best
    ## estimated one and every other coefficient is read against it.
    ref <- keep[which.max(tab[keep])]
    levels <- sort(setdiff(keep, ref))
  }

  cols <- paste0("lg_", make.names(levels))
  for (i in seq_along(levels)) {
    data[[cols[i]]] <- as.numeric(lg == levels[i])
  }
  list(data = data, levels = levels, cols = cols)
}

#' Fit SPM model using Opta features
#'
#' Fits an elastic net model predicting RAPM from Opta box score statistics.
#' Feature selection is delegated to \code{.spm_opta_predictor_cols()} — the
#' canonical Opta-SPM feature set shared with the XGBoost half of the blend.
#'
#' @param data Data frame from aggregate_opta_stats joined with RAPM ratings
#' @param alpha Elastic net mixing (0=ridge, 1=lasso, default 0.5)
#' @param nfolds Number of CV folds (default 10)
#' @param weight_by_minutes Whether to weight by minutes (default TRUE)
#' @param weight_transform Transform for weighting: "sqrt", "linear", "log"
#'
#' @return Fitted glmnet model with metadata
#' @family spm opta
#' @export
#' @examples
#' \dontrun{
#' # Aggregate Opta stats
#' opta_features <- aggregate_opta_stats(opta_stats)
#'
#' # Join with RAPM
#' spm_data <- opta_features |>
#'   inner_join(rapm_ratings |> select(player_id, rapm), by = "player_id")
#'
#' # Fit Opta SPM
#' opta_spm <- fit_spm_opta(spm_data)
#' }
fit_spm_opta <- function(data, alpha = 0.5, nfolds = 10,
                          weight_by_minutes = TRUE, weight_transform = "sqrt",
                          league_fe = FALSE, league_min_n = 50,
                          league_shares = FALSE) {
  predictor_cols <- .spm_opta_predictor_cols(data)

  ## Minutes-share league controls. The caller joins `lgshare_*` columns onto
  ## `data` (see .spm_league_shares()); this just enters them UNPENALIZED, for
  ## the same reason the dummies are: they are controls, not skills, and elastic
  ## net would shrink them away exactly in the thin leagues where they matter.
  ## Separate from league_fe because the two are alternatives, not additions -
  ## enabling both would enter the same information twice.
  share_cols <- character(0)
  if (isTRUE(league_shares)) {
    share_cols <- grep("^lgshare_", names(data), value = TRUE)
    if (length(share_cols) == 0) {
      cli::cli_abort(c(
        "league_shares = TRUE but no {.field lgshare_*} columns on the data.",
        "i" = "Build them with {.fn .spm_league_shares} and join by player_id first.",
        "x" = "Fitting without them would silently drop the league control."
      ))
    }
    if (isTRUE(league_fe)) {
      cli::cli_abort("Set league_fe OR league_shares, not both - they encode the same information.")
    }
    ## NAs come from players absent from the shares frame (no qualifying
    ## minutes). 0 across every share column is the correct encoding: it places
    ## them on the reference level.
    for (cc in share_cols) data[[cc]][is.na(data[[cc]])] <- 0
    predictor_cols <- c(predictor_cols, share_cols)
    progress_msg(sprintf("League shares: %d columns (reference dropped), unpenalized",
                          length(share_cols)))
  }

  ## League fixed effects (opt-in). Entered UNPENALIZED: they are controls, not
  ## skills, and elastic net would otherwise shrink or select them away exactly
  ## where they matter most (the thin weak leagues). Defaults to FALSE so this
  ## ships as a single testable axis rather than silently changing the prior
  ## every downstream rating shrinks toward.
  league_levels <- character(0)
  penalty_factor <- NULL
  if (isTRUE(league_fe)) {
    dm <- .spm_league_dummies(data, levels = NULL, min_n = league_min_n)
    if (length(dm$cols) == 0) {
      cli::cli_warn(paste(
        "league_fe = TRUE but no usable league column (need {.field competition}",
        "or {.field league} with >= 2 levels of {league_min_n}+ rows); fitting without it."))
    } else {
      data <- dm$data
      league_levels <- dm$levels
      predictor_cols <- c(predictor_cols, dm$cols)
      penalty_factor <- stats::setNames(rep(0, length(dm$cols)), dm$cols)
      progress_msg(sprintf("League FE: %d levels (reference held out), unpenalized",
                            length(dm$levels)))
    }
  }
  if (length(share_cols) > 0) {
    penalty_factor <- stats::setNames(rep(0, length(share_cols)), share_cols)
  }

  # NA-safety for the widened `_per90` selection: fit_spm_model() keeps only
  # complete.cases rows, so an un-imputed NA xMetrics column would silently
  # DROP those players from training (05_spm.R imputes its own join, but
  # other callers — e.g. the skills-pipeline SPM — may not). For
  # above-expected metrics 0 IS the population mean by construction, and for
  # xg/xa-style volumes NA means "no SPADL coverage" = no modeled volume, so
  # 0 is the meaningful imputation for both. Surfaced, never silent.
  per90_cols <- predictor_cols[grepl("_per90(_xmetrics)?$", predictor_cols)]
  na_counts <- vapply(per90_cols, function(cc) sum(is.na(data[[cc]])), integer(1))
  if (any(na_counts > 0)) {
    progress_msg(sprintf(
      "Imputing 0 (population mean / no-coverage) for NAs in %d _per90 columns (max %d rows): %s",
      sum(na_counts > 0), max(na_counts),
      paste(utils::head(names(na_counts)[na_counts > 0], 5), collapse = ", ")))
    for (cc in names(na_counts)[na_counts > 0]) {
      data[[cc]][is.na(data[[cc]])] <- 0
    }
  }

  progress_msg(sprintf("Fitting Opta SPM with %d features", length(predictor_cols)))

  fit <- fit_spm_model(
    data = data,
    predictor_cols = predictor_cols,
    alpha = alpha,
    nfolds = nfolds,
    weight_by_minutes = weight_by_minutes,
    weight_transform = weight_transform,
    penalty_factor = penalty_factor
  )

  ## Stash the levels so calculate_spm_ratings() can rebuild identical dummies
  ## at predict time. Without this the predict matrix would be missing columns
  ## the model was fitted on, which errors rather than silently mispredicting --
  ## but only if someone reaches predict, so store it at fit time.
  fit$panna_metadata$league_levels <- league_levels
  fit
}


#' Fit SPM for a custom target variable
#'
#' Convenience wrapper around \code{\link{fit_spm_opta}} that allows fitting
#' SPM on any target column (not just the default \code{rapm}). Useful for
#' multi-target RAPM where each value metric (EPV, WPA, PSV) has its own
#' RAPM rating that needs an SPM predictor.
#'
#' @param data Player features data with RAPM ratings. Must contain a column
#'   named \code{target_col}.
#' @param target_col Name of the target column (e.g., \code{"rapm_epv"},
#'   \code{"rapm_wpa"}, \code{"rapm_psv"}). This column is temporarily
#'   renamed to \code{"rapm"} for compatibility with \code{fit_spm_model()}.
#' @param ... Additional arguments passed to \code{\link{fit_spm_opta}}.
#'
#' @return Fitted SPM model (same as \code{fit_spm_opta}).
#'
#' @keywords internal
fit_spm_opta_target <- function(data, target_col = "rapm", ...) {
  dt <- data.table::as.data.table(data)

  if (!target_col %in% names(dt)) {
    cli::cli_abort("Target column {.val {target_col}} not found in data")
  }

  if (target_col != "rapm") {
    # Temporarily rename target to "rapm" for fit_spm_model compatibility
    if ("rapm" %in% names(dt)) {
      dt[, rapm_orig := rapm]
    }
    data.table::setnames(dt, target_col, "rapm")
  }

  result <- fit_spm_opta(dt, ...)

  result
}


# ============================================================================
# Skill-based SPM (estimated-skills pipeline) O/D column sets
# ============================================================================

#' Canonical offense predictor columns for the SKILL-based SPM
#'
#' Skill-SPM (`estimated-skills/03_skill_spm.R`, and the expanding-window
#' as-of variant in `R/spm_asof.R`) trains on decay-weighted skill features
#' rather than raw box-score aggregates, so its offense/defense hand-curated
#' column lists are a DIFFERENT (smaller) set than the box-score SPM's
#' (`05_spm.R` / `.spm_opta_predictor_cols()`) -- some raw box columns (e.g.
#' `hit_woodwork_p90`, `att_pen_goal_p90`) aren't carried as skill features.
#' Extracted to ONE place so the all-history fit (`03_skill_spm.R` section
#' 10) and the expanding-window per-year fits (`fit_expanding_skill_spm()`)
#' can never drift apart -- hand-copied O/D feature lists are a recurring
#' drift bug in this repo (see `.spm_opta_predictor_cols()`'s own history).
#'
#' @param data Data frame of candidate features (e.g. `spm_train_data`)
#' @return Character vector of offense predictor columns present in `data`
#' @keywords internal
.skill_spm_offense_cols <- function(data) {
  offense_cols <- c(
    "goals_p90", "shots_p90", "shots_on_target_p90",
    "big_chance_scored_p90", "big_chance_created_p90",
    "att_openplay_p90", "att_headed_p90", "att_one_on_one_p90",
    "assists_p90", "key_passes_p90", "through_balls_p90", "total_att_assist_p90",
    "touches_opp_box_p90", "pen_area_entries_p90", "final_third_entries_p90",
    "final_third_passes_p90", "fwd_zone_pass_p90", "open_play_pass_p90",
    "att_fastbreak_p90", "shot_fastbreak_p90",
    "crosses_p90", "crosses_open_play_p90", "forward_pass_p90",
    "was_fouled_p90", "penalty_won_p90",
    # Conversion ratios with above-expected replacements removed 2026-07-07
    # (mirrors 05_spm.R; volume-blind ratios rewarded 1/1 == 10/10)
    "shot_accuracy",
    "fwd_zone_pass_accuracy", "open_play_pass_accuracy", "crosses_open_play_accuracy",
    "att_ibox_goal_p90", "att_obox_goal_p90",
    "chipped_pass_p90", "chipped_pass_accuracy",
    "att_rf_total_p90", "att_lf_total_p90"
  )
  if ("xg_per90" %in% names(data)) {
    offense_cols <- c(offense_cols, "xg_per90", "npxg_per90", "xa_per90_xmetrics")
  }
  offense_cols <- c(offense_cols, intersect(
    c("npg_minus_npxg_per90", "ibox_g_minus_xg_per90", "obox_g_minus_xg_per90",
      "placement_added_per90", "takeon_woe_per90", "aerial_woe_per90"),
    names(data)
  ))
  intersect(offense_cols, names(data))
}


#' Canonical defense predictor columns for the SKILL-based SPM
#'
#' See `.skill_spm_offense_cols()` for why this is a separate (smaller) set
#' from the box-score SPM's defense columns.
#'
#' @param data Data frame of candidate features
#' @return Character vector of defense predictor columns present in `data`
#' @keywords internal
.skill_spm_defense_cols <- function(data) {
  defense_cols <- c(
    "tackles_p90", "tackles_won_p90",
    "interceptions_p90", "interceptions_won_p90",
    "clearances_p90", "clearances_effective_p90",
    "blocks_p90", "blocked_passes_p90",
    "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
    "aerial_won_p90", "aerial_lost_p90",
    "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
    "fouls_p90", "penalty_conceded_p90",
    "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
    # tackle_success/aerial_success removed 2026-07-07 -> defensive WOE below
    "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch",
    "fifty_fifty_p90", "fifty_fifty_won_p90", "fifty_fifty_success",
    "back_zone_pass_p90", "back_zone_pass_accuracy",
    "long_pass_own_to_opp_p90", "long_pass_own_to_opp_accuracy",
    "tackle_poss_woe_per90", "containment_woe_per90",
    "aerial_woe_per90", "aerial_poss_woe_per90", "gsaa_per90"
  )
  intersect(defense_cols, names(data))
}


#' Sign-constraint feature lists for the SKILL-based SPM defense model
#'
#' In the negative-is-good defense convention, "good defense" features must
#' get a non-positive SPM coefficient (more = better defender) and
#' "bad defense" features a non-negative one. Mirrors `05_spm.R`'s
#' `defense_good_features`/`defense_bad_features` for the box-score SPM,
#' restricted to the skill-SPM's smaller feature set.
#'
#' @return List with `good` and `bad` character vectors of feature names.
#' @keywords internal
.skill_spm_defense_constraints <- function() {
  list(
    good = c(
      "tackles_p90", "tackles_won_p90",
      "interceptions_p90", "interceptions_won_p90",
      "clearances_p90", "clearances_effective_p90",
      "blocks_p90", "blocked_passes_p90",
      "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
      "aerial_won_p90",
      "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
      "tackle_poss_woe_per90", "containment_woe_per90",
      "aerial_woe_per90", "aerial_poss_woe_per90", "gsaa_per90",
      "fifty_fifty_won_p90", "fifty_fifty_success",
      "back_zone_pass_accuracy"
    ),
    bad = c(
      "fouls_p90", "penalty_conceded_p90",
      "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
      "aerial_lost_p90",
      "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch"
    )
  )
}


#' Compare SPM feature importance between two models
#'
#' Compares which features are most important between two fitted SPM models
#' (e.g. different seasons, targets, or feature sets). Useful for understanding
#' which features drive each model's ratings.
#'
#' @param fbref_model Fitted SPM model to compare (labeled "FBref" in the
#'   output's `source` column for historical reasons; any fitted SPM model works)
#' @param opta_model Fitted SPM model to compare against (labeled "Opta" in
#'   the output's `source` column)
#' @param n Number of top features to compare (default 20)
#'
#' @return Data frame comparing feature importance
#' @keywords internal
compare_spm_features <- function(fbref_model, opta_model, n = 20) {
  fbref_imp <- get_spm_feature_importance(fbref_model, n = n)
  fbref_imp$source <- "FBref"

  opta_imp <- get_spm_feature_importance(opta_model, n = n)
  opta_imp$source <- "Opta"

  # Combine
  comparison <- rbindlist(list(fbref_imp, opta_imp), use.names = TRUE, fill = TRUE)
  comparison <- comparison[, .(source, feature, coefficient, abs_coef)]
  setorder(comparison, -abs_coef)

  as.data.frame(comparison)
}
