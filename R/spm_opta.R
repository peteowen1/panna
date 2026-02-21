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
    if (col %in% names(player_stats)) {
      x <- as.numeric(player_stats[[col]])
      x[is.na(x)] <- 0
    } else {
      x <- rep(0, nrow(player_stats))
    }
    player_stats[[paste0(col, "_p90")]] <- x / mins_per_90
  }

  player_stats
}


#' Calculate derived features (success rates and ratios) for Opta stats
#' @param player_stats Data frame with counting stats and per-90 rates
#' @return player_stats with derived feature columns added
#' @keywords internal
.calculate_opta_derived_features <- function(player_stats) {
  n_players <- nrow(player_stats)
  mins_per_90 <- player_stats$total_minutes / 90

  safe_col <- function(col_name) {
    if (col_name %in% names(player_stats)) {
      x <- as.numeric(player_stats[[col_name]])
      ifelse(is.na(x), 0, x)
    } else {
      rep(0, n_players)
    }
  }

  safe_div_local <- function(num, denom) {
    num <- as.numeric(num)
    denom <- as.numeric(denom)
    ifelse(is.na(denom) | denom == 0, 0, num / denom)
  }

  # Shooting efficiency
  player_stats$shot_accuracy <- safe_div_local(safe_col("shots_on_target"), safe_col("shots"))
  player_stats$goals_per_shot <- safe_div_local(safe_col("goals"), safe_col("shots"))
  player_stats$ibox_shot_ratio <- safe_div_local(safe_col("shots_ibox"), safe_col("shots"))
  player_stats$big_chance_conversion <- safe_div_local(
    safe_col("big_chance_scored"),
    safe_col("big_chance_scored") + safe_col("big_chance_missed")
  )

  # Passing efficiency
  player_stats$pass_accuracy <- safe_div_local(safe_col("passes_accurate"), safe_col("passes"))
  player_stats$final_third_pass_acc <- safe_div_local(
    safe_col("final_third_passes_acc"), safe_col("final_third_passes")
  )
  player_stats$long_ball_accuracy <- safe_div_local(safe_col("long_balls_acc"), safe_col("long_balls"))
  player_stats$through_ball_accuracy <- safe_div_local(
    safe_col("through_balls_acc"), safe_col("through_balls")
  )
  player_stats$cross_accuracy <- safe_div_local(safe_col("crosses_acc"), safe_col("crosses"))

  # Defensive success rates
  player_stats$tackle_success <- safe_div_local(safe_col("tackles_won"), safe_col("tackles"))
  player_stats$interception_success <- safe_div_local(
    safe_col("interceptions_won"), safe_col("interceptions")
  )
  player_stats$clearance_effectiveness <- safe_div_local(
    safe_col("clearances_effective"), safe_col("clearances")
  )

  # Duel success rates
  player_stats$duel_success <- safe_div_local(
    safe_col("duel_won"), safe_col("duel_won") + safe_col("duel_lost")
  )
  player_stats$aerial_success <- safe_div_local(
    safe_col("aerial_won"), safe_col("aerial_won") + safe_col("aerial_lost")
  )

  # Possession balance
  total_poss_won <- safe_col("poss_won_def3rd") + safe_col("poss_won_mid3rd") +
    safe_col("poss_won_att3rd")
  player_stats$poss_won_total_p90 <- total_poss_won / mins_per_90
  player_stats$poss_won_att_ratio <- safe_div_local(safe_col("poss_won_att3rd"), total_poss_won)

  # Ball retention
  player_stats$turnovers_p90 <- (safe_col("dispossessed") + safe_col("turnover")) / mins_per_90
  player_stats$foul_differential_p90 <- player_stats$was_fouled_p90 - player_stats$fouls_p90

  # Progressive passing accuracy
  player_stats$fwd_zone_pass_accuracy <- safe_div_local(
    safe_col("fwd_zone_pass_acc"), safe_col("fwd_zone_pass")
  )
  player_stats$open_play_pass_accuracy <- safe_div_local(
    safe_col("open_play_pass_acc"), safe_col("open_play_pass")
  )

  # Open-play cross accuracy
  player_stats$crosses_open_play_accuracy <- safe_div_local(
    safe_col("crosses_open_play_acc"), safe_col("crosses_open_play")
  )

  # Touch quality rate
  player_stats$bad_touch_rate <- safe_div_local(
    safe_col("unsuccessful_touch") + safe_col("overrun"), safe_col("touches")
  )

  # Error total per-90
  player_stats$errors_total_p90 <- (safe_col("error_lead_to_shot") +
    safe_col("error_lead_to_goal")) / mins_per_90

  # Headed goal rate
  player_stats$headed_goal_rate <- safe_div_local(safe_col("att_headed_goal"), safe_col("att_headed"))

  # Flick-on accuracy
  player_stats$flick_on_accuracy <- safe_div_local(safe_col("flick_on_acc"), safe_col("flick_on"))

  # GK sweeper accuracy
  player_stats$keeper_sweeper_accuracy <- safe_div_local(
    safe_col("keeper_sweeper_acc"), safe_col("keeper_sweeper")
  )

  # Round 2 derived features
  player_stats$back_zone_pass_accuracy <- safe_div_local(
    safe_col("back_zone_pass_acc"), safe_col("back_zone_pass")
  )
  player_stats$chipped_pass_accuracy <- safe_div_local(
    safe_col("chipped_pass_acc"), safe_col("chipped_pass")
  )
  player_stats$ibox_goal_rate <- safe_div_local(safe_col("att_ibox_goal"), safe_col("shots_ibox"))
  player_stats$obox_goal_rate <- safe_div_local(safe_col("att_obox_goal"), safe_col("shots_obox"))
  player_stats$penalty_conversion <- safe_div_local(
    safe_col("att_pen_goal"), safe_col("att_pen_goal") + safe_col("att_pen_miss")
  )
  player_stats$long_pass_own_to_opp_accuracy <- safe_div_local(
    safe_col("long_pass_own_to_opp_acc"), safe_col("long_pass_own_to_opp")
  )
  player_stats$fifty_fifty_success <- safe_div_local(
    safe_col("fifty_fifty_won"), safe_col("fifty_fifty")
  )
  player_stats$poss_lost_ctrl_per_touch <- safe_div_local(
    safe_col("poss_lost_ctrl"), safe_col("touches")
  )

  # Goalkeeper metrics
  shots_faced <- safe_col("saves") + safe_col("goals_conceded")
  player_stats$save_percentage <- safe_div_local(safe_col("saves"), shots_faced)

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

  dt <- data.table::as.data.table(opta_stats)

  # Clean player ID

  dt[, player_id := clean_player_name(player_name)]

  # Get column mapping and rename
  opta_cols <- .get_opta_col_mapping()
  existing_cols <- opta_cols[opta_cols %in% names(dt)]

  # Rename Opta columns to panna names (only the ones that exist)
  for (i in seq_along(existing_cols)) {
    panna_name <- names(existing_cols)[i]
    opta_name <- existing_cols[i]
    if (panna_name != opta_name && opta_name %in% names(dt)) {
      data.table::setnames(dt, opta_name, panna_name, skip_absent = TRUE)
    }
  }

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

  # Replace NAs with 0 in numeric columns
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
    ifelse(is.na(x) | is.infinite(x), 0, x)
  })

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
#' @export
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

  opta_stats$player_id <- clean_player_name(opta_stats$player_name)

  # Get column mapping and filter to existing columns
  opta_cols <- .get_opta_col_mapping()
  existing_cols <- opta_cols[opta_cols %in% names(opta_stats)]

  # Aggregate by player_id
  player_stats <- stats::aggregate(
    opta_stats[, existing_cols, drop = FALSE],
    by = list(player_id = opta_stats$player_id),
    FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
  )
  player_stats <- rename_columns(player_stats, existing_cols)

  # Count matches
  match_counts <- stats::aggregate(
    opta_stats$match_id,
    by = list(player_id = opta_stats$player_id),
    FUN = function(x) length(unique(x))
  )
  names(match_counts)[2] <- "n_matches"
  player_stats <- data.table::as.data.table(player_stats)[data.table::as.data.table(match_counts), on = "player_id", nomatch = NULL]
  data.table::setDF(player_stats)

  # Get canonical player_name
  cleaned_names <- gsub("\u00A0", " ", opta_stats$player_name)
  cleaned_names <- trimws(cleaned_names)

  player_name_lookup <- stats::aggregate(
    cleaned_names,
    by = list(player_id = opta_stats$player_id),
    FUN = function(x) {
      tbl <- table(x)
      names(tbl)[which.max(tbl)]
    }
  )
  names(player_name_lookup)[2] <- "player_name"
  player_stats <- data.table::as.data.table(player_name_lookup)[data.table::as.data.table(player_stats), on = "player_id"]
  data.table::setDF(player_stats)

  # Get primary position
  if ("position" %in% names(opta_stats)) {
    pos_mode <- stats::aggregate(
      opta_stats$position,
      by = list(player_id = opta_stats$player_id),
      FUN = function(x) {
        tbl <- table(x[!is.na(x) & x != ""])
        if (length(tbl) == 0) return(NA_character_)
        names(tbl)[which.max(tbl)]
      }
    )
    names(pos_mode)[2] <- "primary_position"
    player_stats <- data.table::as.data.table(pos_mode)[data.table::as.data.table(player_stats), on = "player_id"]
    data.table::setDF(player_stats)
  }

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

  # Replace NAs with 0
  numeric_cols <- sapply(player_stats, is.numeric)
  player_stats[numeric_cols] <- lapply(player_stats[numeric_cols], function(x) {
    ifelse(is.na(x), 0, x)
  })

  progress_msg(sprintf("Aggregated Opta stats for %d players with %d features",
                       nrow(player_stats), ncol(player_stats)))

  player_stats
}


#' Fit SPM model using Opta features
#'
#' Fits an elastic net model predicting RAPM from Opta box score statistics.
#' Uses Opta-specific per-90 features for prediction.
#'
#' @param data Data frame from aggregate_opta_stats joined with RAPM ratings
#' @param alpha Elastic net mixing (0=ridge, 1=lasso, default 0.5)
#' @param nfolds Number of CV folds (default 10)
#' @param weight_by_minutes Whether to weight by minutes (default TRUE)
#' @param weight_transform Transform for weighting: "sqrt", "linear", "log"
#'
#' @return Fitted glmnet model with metadata
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
                          weight_by_minutes = TRUE, weight_transform = "sqrt") {
  # Use all _p90 columns as predictors
  predictor_cols <- names(data)[grepl("_p90$", names(data))]

  # Add success rate columns
  success_cols <- c("shot_accuracy", "goals_per_shot", "pass_accuracy",
                    "tackle_success", "duel_success", "aerial_success",
                    "big_chance_conversion", "final_third_pass_acc",
                    "long_ball_accuracy", "cross_accuracy",
                    "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
                    "crosses_open_play_accuracy", "bad_touch_rate",
                    "keeper_sweeper_accuracy", "errors_total_p90",
                    "headed_goal_rate", "flick_on_accuracy",
                    # Round 2
                    "back_zone_pass_accuracy", "chipped_pass_accuracy",
                    "ibox_goal_rate", "obox_goal_rate",
                    "penalty_conversion", "long_pass_own_to_opp_accuracy",
                    "fifty_fifty_success", "poss_lost_ctrl_per_touch")
  success_cols <- intersect(success_cols, names(data))
  predictor_cols <- c(predictor_cols, success_cols)

  # Add position dummies if available
  pos_cols <- c("is_gk", "is_df", "is_mf", "is_fw")
  pos_cols <- intersect(pos_cols, names(data))
  predictor_cols <- c(predictor_cols, pos_cols)

  progress_msg(sprintf("Fitting Opta SPM with %d features", length(predictor_cols)))

  fit_spm_model(
    data = data,
    predictor_cols = predictor_cols,
    alpha = alpha,
    nfolds = nfolds,
    weight_by_minutes = weight_by_minutes,
    weight_transform = weight_transform
  )
}


#' Compare FBref and Opta SPM feature importance
#'
#' Compares which features are most important in FBref vs Opta SPM models.
#' Useful for understanding which data source captures different aspects of play.
#'
#' @param fbref_model Fitted SPM model from FBref data
#' @param opta_model Fitted SPM model from Opta data
#' @param n Number of top features to compare (default 20)
#'
#' @return Data frame comparing feature importance
#' @export
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
