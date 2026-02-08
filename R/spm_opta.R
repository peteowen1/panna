# Opta SPM (Statistical Plus-Minus) model functions
#
# Functions for building SPM models from Opta/TheAnalyst data.
# Opta data has 263 columns with different naming conventions than FBref.
# Depends on spm_model.R for core model fitting functions.


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

  # Create player_id from player_name for consistent matching
  opta_stats$player_id <- clean_player_name(opta_stats$player_name)

  # Define column mappings: new_name = opta_column_name
  # These are the raw counting stats we'll sum across matches
  opta_cols <- c(
    # Minutes
    total_minutes = "minsPlayed",
    # Goals and shots
    goals = "goals",
    goals_openplay = "goalsOpenplay",
    shots = "totalScoringAtt",
    shots_ibox = "attemptsIbox",
    shots_obox = "attemptsObox",
    shots_on_target = "ontargetScoringAtt",
    shot_off_target = "shotOffTarget",
    shots_blocked = "blockedScoringAtt",
    big_chance_created = "bigChanceCreated",
    big_chance_scored = "bigChanceScored",
    big_chance_missed = "bigChanceMissed",
    # Assists and creativity
    assists = "goalAssist",
    assists_openplay = "goalAssistOpenplay",
    assists_setplay = "goalAssistSetplay",
    second_assists = "secondGoalAssist",
    total_att_assist = "totalAttAssist",
    on_target_att_assist = "ontargetAttAssist",
    # Passing
    passes = "totalPass",
    passes_accurate = "accuratePass",
    final_third_passes = "totalFinalThirdPasses",
    final_third_passes_acc = "successfulFinalThirdPasses",
    long_balls = "totalLongBalls",
    long_balls_acc = "accurateLongBalls",
    through_balls = "totalThroughBall",
    through_balls_acc = "accurateThroughBall",
    crosses = "totalCross",
    crosses_acc = "accurateCross",
    key_passes = "putThrough",
    key_passes_success = "successfulPutThrough",
    backward_pass = "backwardPass",
    forward_pass = "fwdPass",
    layoffs = "totalLayoffs",
    layoffs_acc = "accurateLayoffs",
    # Defending
    tackles = "totalTackle",
    tackles_won = "wonTackle",
    interceptions = "interception",
    interceptions_won = "interceptionWon",
    interceptions_ibox = "interceptionsInBox",
    clearances = "totalClearance",
    clearances_effective = "effectiveClearance",
    head_clearances = "headClearance",
    head_clearances_effective = "effectiveHeadClearance",
    blocks = "outfielderBlock",
    blocked_passes = "blockedPass",
    blocked_crosses = "blockedCross",
    # Duels
    duel_won = "duelWon",
    duel_lost = "duelLost",
    aerial_won = "aerialWon",
    aerial_lost = "aerialLost",
    contest_won = "wonContest",
    contest_total = "totalContest",
    challenge_lost = "challengeLost",
    # Possession
    touches = "touches",
    touches_opp_box = "touchesInOppBox",
    poss_won_def3rd = "possWonDef3rd",
    poss_won_mid3rd = "possWonMid3rd",
    poss_won_att3rd = "possWonAtt3rd",
    ball_recovery = "ballRecovery",
    dispossessed = "dispossessed",
    turnover = "turnover",
    poss_lost_all = "possLostAll",
    times_tackled = "timesTackled",
    # Fouls and cards
    fouls = "fouls",
    was_fouled = "wasFouled",
    fouled_final_third = "fouledFinalThird",
    yellow_cards = "yellowCard",
    red_cards = "redCard",
    second_yellows = "secondYellow",
    # Set pieces
    corners_taken = "cornerTaken",
    corners_won = "wonCorners",
    corners_lost = "lostCorners",
    corners_intobox = "totalCornersIntobox",
    corners_intobox_acc = "accurateCornersIntobox",
    freekick_crosses = "freekickCross",
    # Goalkeeper stats
    saves = "saves",
    saves_ibox = "savedIbox",
    saves_obox = "savedObox",
    goals_conceded = "goalsConceded",
    goals_conceded_ibox = "goalsConcededIbox",
    high_claim = "totalHighClaim",
    good_high_claim = "goodHighClaim",
    punches = "punches",
    keeper_throws = "keeperThrows",
    keeper_throws_acc = "accurateKeeperThrows",
    # Progressive passing
    fwd_zone_pass = "totalFwdZonePass",
    fwd_zone_pass_acc = "accurateFwdZonePass",
    open_play_pass = "openPlayPass",
    open_play_pass_acc = "successfulOpenPlayPass",
    # Errors
    error_lead_to_shot = "errorLeadToShot",
    error_lead_to_goal = "errorLeadToGoal",
    # Counter-attacks
    att_fastbreak = "attFastbreak",
    shot_fastbreak = "shotFastbreak",
    # Shot types
    att_openplay = "attOpenplay",
    att_setpiece = "attSetpiece",
    att_headed = "attHdTotal",
    att_headed_goal = "attHdGoal",
    att_one_on_one = "attOneOnOne",
    # Open-play crosses
    crosses_open_play = "totalCrossNocorner",
    crosses_open_play_acc = "accurateCrossNocorner",
    # Penalties
    penalty_won = "penaltyWon",
    penalty_conceded = "penaltyConceded",
    # Assist quality
    offtarget_att_assist = "offtargetAttAssist",
    # Defensive
    last_man_tackle = "lastManTackle",
    six_yard_block = "sixYardBlock",
    clearance_off_line = "clearanceOffLine",
    # GK advanced
    keeper_sweeper = "totalKeeperSweeper",
    keeper_sweeper_acc = "accurateKeeperSweeper",
    attempts_conceded_ibox = "attemptsConcededIbox",
    attempts_conceded_obox = "attemptsConcededObox",
    gk_smother = "gkSmother",
    # Touch quality
    unsuccessful_touch = "unsuccessfulTouch",
    overrun = "overrun",
    # Flick-ons
    flick_on = "totalFlickOn",
    flick_on_acc = "accurateFlickOn",
    # Other
    offsides = "totalOffside",
    offside_provoked = "offsideProvoked",
    pen_area_entries = "penAreaEntries",
    final_third_entries = "finalThirdEntries",
    pull_backs = "totalPullBack",
    pull_backs_acc = "accuratePullBack",
    # === Round 2 additions ===
    # Backward passing
    back_zone_pass = "totalBackZonePass",
    back_zone_pass_acc = "accurateBackZonePass",
    # Chipped passes
    chipped_pass = "totalChippedPass",
    chipped_pass_acc = "accurateChippedPass",
    # Foot preference
    att_rf_total = "attRfTotal",
    att_lf_total = "attLfTotal",
    # Shot location goals
    att_ibox_goal = "attIboxGoal",
    att_obox_goal = "attOboxGoal",
    # Shot zone targets
    att_ibox_target = "attIboxTarget",
    att_obox_target = "attOboxTarget",
    # Shooting
    hit_woodwork = "hitWoodwork",
    # Penalties
    att_pen_goal = "attPenGoal",
    att_pen_miss = "attPenMiss",
    pen_goals_conceded = "penGoalsConceded",
    # GK
    keeper_pickup = "keeperPickUp",
    # Possession control
    poss_lost_ctrl = "possLostCtrl",
    # Long pass own-to-opp
    long_pass_own_to_opp = "longPassOwnToOpp",
    long_pass_own_to_opp_acc = "longPassOwnToOppSuccess",
    # Fifty-fifty duels
    fifty_fifty = "fiftyFifty",
    fifty_fifty_won = "successfulFiftyFifty"
  )

  # Filter to columns that exist
  existing_cols <- opta_cols[opta_cols %in% names(opta_stats)]

  # Aggregate by player_id
  player_stats <- stats::aggregate(
    opta_stats[, existing_cols, drop = FALSE],
    by = list(player_id = opta_stats$player_id),
    FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
  )

  # Rename columns
  player_stats <- rename_columns(player_stats, existing_cols)

  # Count matches
  match_counts <- stats::aggregate(
    opta_stats$match_id,
    by = list(player_id = opta_stats$player_id),
    FUN = function(x) length(unique(x))
  )
  names(match_counts)[2] <- "n_matches"
  player_stats <- merge(player_stats, match_counts, by = "player_id")

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
  player_stats <- merge(player_stats, player_name_lookup, by = "player_id", all.x = TRUE)

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
    player_stats <- merge(player_stats, pos_mode, by = "player_id", all.x = TRUE)
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

  # Calculate per-90 rates
  mins_per_90 <- player_stats$total_minutes / 90
  player_stats$mins_per_90 <- mins_per_90
  n_players <- nrow(player_stats)

  # Helper functions
  safe_col <- function(col_name) {
    if (col_name %in% names(player_stats)) {
      x <- as.numeric(player_stats[[col_name]])
      ifelse(is.na(x), 0, x)
    } else {
      rep(0, n_players)
    }
  }

  safe_p90 <- function(col_name) {
    safe_col(col_name) / mins_per_90
  }

  safe_div <- function(num, denom) {
    num <- as.numeric(num)
    denom <- as.numeric(denom)
    ifelse(is.na(denom) | denom == 0, 0, num / denom)
  }

  # ===== PER-90 RATES =====

  # Shooting per-90
  player_stats$goals_p90 <- safe_p90("goals")
  player_stats$shots_p90 <- safe_p90("shots")
  player_stats$shots_on_target_p90 <- safe_p90("shots_on_target")
  player_stats$shots_ibox_p90 <- safe_p90("shots_ibox")
  player_stats$shots_obox_p90 <- safe_p90("shots_obox")
  player_stats$big_chance_scored_p90 <- safe_p90("big_chance_scored")
  player_stats$big_chance_missed_p90 <- safe_p90("big_chance_missed")

  # Creativity per-90
  player_stats$assists_p90 <- safe_p90("assists")
  player_stats$big_chance_created_p90 <- safe_p90("big_chance_created")
  player_stats$total_att_assist_p90 <- safe_p90("total_att_assist")
  player_stats$key_passes_p90 <- safe_p90("key_passes")
  player_stats$through_balls_p90 <- safe_p90("through_balls")

  # Passing per-90
  player_stats$passes_p90 <- safe_p90("passes")
  player_stats$passes_accurate_p90 <- safe_p90("passes_accurate")
  player_stats$final_third_passes_p90 <- safe_p90("final_third_passes")
  player_stats$long_balls_p90 <- safe_p90("long_balls")
  player_stats$crosses_p90 <- safe_p90("crosses")
  player_stats$forward_pass_p90 <- safe_p90("forward_pass")

  # Defensive per-90
  player_stats$tackles_p90 <- safe_p90("tackles")
  player_stats$tackles_won_p90 <- safe_p90("tackles_won")
  player_stats$interceptions_p90 <- safe_p90("interceptions")
  player_stats$interceptions_won_p90 <- safe_p90("interceptions_won")
  player_stats$clearances_p90 <- safe_p90("clearances")
  player_stats$clearances_effective_p90 <- safe_p90("clearances_effective")
  player_stats$blocks_p90 <- safe_p90("blocks")
  player_stats$blocked_passes_p90 <- safe_p90("blocked_passes")

  # Duels per-90
  player_stats$duel_won_p90 <- safe_p90("duel_won")
  player_stats$duel_lost_p90 <- safe_p90("duel_lost")
  player_stats$aerial_won_p90 <- safe_p90("aerial_won")
  player_stats$aerial_lost_p90 <- safe_p90("aerial_lost")

  # Possession per-90
  player_stats$touches_p90 <- safe_p90("touches")
  player_stats$touches_opp_box_p90 <- safe_p90("touches_opp_box")
  player_stats$poss_won_def3rd_p90 <- safe_p90("poss_won_def3rd")
  player_stats$poss_won_mid3rd_p90 <- safe_p90("poss_won_mid3rd")
  player_stats$poss_won_att3rd_p90 <- safe_p90("poss_won_att3rd")
  player_stats$ball_recovery_p90 <- safe_p90("ball_recovery")
  player_stats$dispossessed_p90 <- safe_p90("dispossessed")
  player_stats$turnover_p90 <- safe_p90("turnover")
  player_stats$times_tackled_p90 <- safe_p90("times_tackled")

  # Set pieces per-90
  player_stats$corners_taken_p90 <- safe_p90("corners_taken")
  player_stats$corners_won_p90 <- safe_p90("corners_won")
  player_stats$pen_area_entries_p90 <- safe_p90("pen_area_entries")
  player_stats$final_third_entries_p90 <- safe_p90("final_third_entries")

  # Fouls per-90
  player_stats$fouls_p90 <- safe_p90("fouls")
  player_stats$was_fouled_p90 <- safe_p90("was_fouled")

  # Goalkeeper per-90
  player_stats$saves_p90 <- safe_p90("saves")
  player_stats$goals_conceded_p90 <- safe_p90("goals_conceded")

  # Progressive passing per-90
  player_stats$fwd_zone_pass_p90 <- safe_p90("fwd_zone_pass")
  player_stats$open_play_pass_p90 <- safe_p90("open_play_pass")

  # Errors per-90
  player_stats$error_lead_to_shot_p90 <- safe_p90("error_lead_to_shot")
  player_stats$error_lead_to_goal_p90 <- safe_p90("error_lead_to_goal")

  # Counter-attacks per-90
  player_stats$att_fastbreak_p90 <- safe_p90("att_fastbreak")
  player_stats$shot_fastbreak_p90 <- safe_p90("shot_fastbreak")

  # Shot types per-90
  player_stats$att_openplay_p90 <- safe_p90("att_openplay")
  player_stats$att_setpiece_p90 <- safe_p90("att_setpiece")
  player_stats$att_headed_p90 <- safe_p90("att_headed")
  player_stats$att_one_on_one_p90 <- safe_p90("att_one_on_one")

  # Open-play crosses per-90
  player_stats$crosses_open_play_p90 <- safe_p90("crosses_open_play")

  # Penalties per-90
  player_stats$penalty_won_p90 <- safe_p90("penalty_won")
  player_stats$penalty_conceded_p90 <- safe_p90("penalty_conceded")

  # Assist quality per-90
  player_stats$offtarget_att_assist_p90 <- safe_p90("offtarget_att_assist")

  # Defensive per-90 (new)
  player_stats$last_man_tackle_p90 <- safe_p90("last_man_tackle")
  player_stats$six_yard_block_p90 <- safe_p90("six_yard_block")
  player_stats$clearance_off_line_p90 <- safe_p90("clearance_off_line")

  # GK advanced per-90
  player_stats$keeper_sweeper_p90 <- safe_p90("keeper_sweeper")
  player_stats$attempts_conceded_ibox_p90 <- safe_p90("attempts_conceded_ibox")
  player_stats$attempts_conceded_obox_p90 <- safe_p90("attempts_conceded_obox")
  player_stats$gk_smother_p90 <- safe_p90("gk_smother")

  # Touch quality per-90
  player_stats$unsuccessful_touch_p90 <- safe_p90("unsuccessful_touch")
  player_stats$overrun_p90 <- safe_p90("overrun")

  # Flick-ons per-90
  player_stats$flick_on_p90 <- safe_p90("flick_on")

  # === Round 2 per-90 rates ===

  # Backward passing per-90
  player_stats$back_zone_pass_p90 <- safe_p90("back_zone_pass")

  # Chipped passes per-90
  player_stats$chipped_pass_p90 <- safe_p90("chipped_pass")

  # Foot preference per-90
  player_stats$att_rf_total_p90 <- safe_p90("att_rf_total")
  player_stats$att_lf_total_p90 <- safe_p90("att_lf_total")

  # Shot location per-90
  player_stats$att_ibox_goal_p90 <- safe_p90("att_ibox_goal")
  player_stats$att_obox_goal_p90 <- safe_p90("att_obox_goal")
  player_stats$att_ibox_target_p90 <- safe_p90("att_ibox_target")
  player_stats$att_obox_target_p90 <- safe_p90("att_obox_target")

  # Shooting misc per-90
  player_stats$hit_woodwork_p90 <- safe_p90("hit_woodwork")

  # Penalties per-90
  player_stats$att_pen_goal_p90 <- safe_p90("att_pen_goal")
  player_stats$att_pen_miss_p90 <- safe_p90("att_pen_miss")
  player_stats$pen_goals_conceded_p90 <- safe_p90("pen_goals_conceded")

  # GK per-90
  player_stats$keeper_pickup_p90 <- safe_p90("keeper_pickup")

  # Possession control per-90
  player_stats$poss_lost_ctrl_p90 <- safe_p90("poss_lost_ctrl")

  # Long pass own-to-opp per-90
  player_stats$long_pass_own_to_opp_p90 <- safe_p90("long_pass_own_to_opp")

  # Fifty-fifty duels per-90
  player_stats$fifty_fifty_p90 <- safe_p90("fifty_fifty")
  player_stats$fifty_fifty_won_p90 <- safe_p90("fifty_fifty_won")

  # ===== DERIVED FEATURES (success rates and ratios) =====

  # Shooting efficiency
  player_stats$shot_accuracy <- safe_div(safe_col("shots_on_target"), safe_col("shots"))
  player_stats$goals_per_shot <- safe_div(safe_col("goals"), safe_col("shots"))
  player_stats$ibox_shot_ratio <- safe_div(safe_col("shots_ibox"), safe_col("shots"))
  player_stats$big_chance_conversion <- safe_div(
    safe_col("big_chance_scored"),
    safe_col("big_chance_scored") + safe_col("big_chance_missed")
  )

  # Passing efficiency
  player_stats$pass_accuracy <- safe_div(safe_col("passes_accurate"), safe_col("passes"))
  player_stats$final_third_pass_acc <- safe_div(
    safe_col("final_third_passes_acc"), safe_col("final_third_passes")
  )
  player_stats$long_ball_accuracy <- safe_div(safe_col("long_balls_acc"), safe_col("long_balls"))
  player_stats$through_ball_accuracy <- safe_div(
    safe_col("through_balls_acc"), safe_col("through_balls")
  )
  player_stats$cross_accuracy <- safe_div(safe_col("crosses_acc"), safe_col("crosses"))

  # Defensive success rates
  player_stats$tackle_success <- safe_div(safe_col("tackles_won"), safe_col("tackles"))
  player_stats$interception_success <- safe_div(
    safe_col("interceptions_won"), safe_col("interceptions")
  )
  player_stats$clearance_effectiveness <- safe_div(
    safe_col("clearances_effective"), safe_col("clearances")
  )

  # Duel success rates
  player_stats$duel_success <- safe_div(
    safe_col("duel_won"), safe_col("duel_won") + safe_col("duel_lost")
  )
  player_stats$aerial_success <- safe_div(
    safe_col("aerial_won"), safe_col("aerial_won") + safe_col("aerial_lost")
  )

  # Possession balance
  total_poss_won <- safe_col("poss_won_def3rd") + safe_col("poss_won_mid3rd") +
    safe_col("poss_won_att3rd")
  player_stats$poss_won_total_p90 <- total_poss_won / mins_per_90
  player_stats$poss_won_att_ratio <- safe_div(safe_col("poss_won_att3rd"), total_poss_won)

  # Ball retention
  player_stats$turnovers_p90 <- (safe_col("dispossessed") + safe_col("turnover")) / mins_per_90
  player_stats$foul_differential_p90 <- player_stats$was_fouled_p90 - player_stats$fouls_p90

  # Progressive passing accuracy
  player_stats$fwd_zone_pass_accuracy <- safe_div(
    safe_col("fwd_zone_pass_acc"), safe_col("fwd_zone_pass")
  )
  player_stats$open_play_pass_accuracy <- safe_div(
    safe_col("open_play_pass_acc"), safe_col("open_play_pass")
  )

  # Open-play cross accuracy
  player_stats$crosses_open_play_accuracy <- safe_div(
    safe_col("crosses_open_play_acc"), safe_col("crosses_open_play")
  )

  # Touch quality rate
  player_stats$bad_touch_rate <- safe_div(
    safe_col("unsuccessful_touch") + safe_col("overrun"),
    safe_col("touches")
  )

  # Error total per-90
  player_stats$errors_total_p90 <- (safe_col("error_lead_to_shot") +
    safe_col("error_lead_to_goal")) / mins_per_90

  # Headed goal rate
  player_stats$headed_goal_rate <- safe_div(
    safe_col("att_headed_goal"), safe_col("att_headed")
  )

  # Flick-on accuracy
  player_stats$flick_on_accuracy <- safe_div(
    safe_col("flick_on_acc"), safe_col("flick_on")
  )

  # GK sweeper accuracy
  player_stats$keeper_sweeper_accuracy <- safe_div(
    safe_col("keeper_sweeper_acc"), safe_col("keeper_sweeper")
  )

  # === Round 2 derived features ===

  # Backward pass accuracy
  player_stats$back_zone_pass_accuracy <- safe_div(
    safe_col("back_zone_pass_acc"), safe_col("back_zone_pass")
  )

  # Chipped pass accuracy
  player_stats$chipped_pass_accuracy <- safe_div(
    safe_col("chipped_pass_acc"), safe_col("chipped_pass")
  )

  # In-box goal rate (goals per ibox shot)
  player_stats$ibox_goal_rate <- safe_div(
    safe_col("att_ibox_goal"), safe_col("shots_ibox")
  )

  # Out-of-box goal rate (goals per obox shot)
  player_stats$obox_goal_rate <- safe_div(
    safe_col("att_obox_goal"), safe_col("shots_obox")
  )

  # Penalty conversion
  player_stats$penalty_conversion <- safe_div(
    safe_col("att_pen_goal"),
    safe_col("att_pen_goal") + safe_col("att_pen_miss")
  )

  # Long pass own-to-opp accuracy
  player_stats$long_pass_own_to_opp_accuracy <- safe_div(
    safe_col("long_pass_own_to_opp_acc"), safe_col("long_pass_own_to_opp")
  )

  # Fifty-fifty success rate
  player_stats$fifty_fifty_success <- safe_div(
    safe_col("fifty_fifty_won"), safe_col("fifty_fifty")
  )

  # Possession lost control per touch
  player_stats$poss_lost_ctrl_per_touch <- safe_div(
    safe_col("poss_lost_ctrl"), safe_col("touches")
  )

  # Goalkeeper metrics
  shots_faced <- safe_col("saves") + safe_col("goals_conceded")
  player_stats$save_percentage <- safe_div(safe_col("saves"), shots_faced)

  # Position dummies
  if ("primary_position" %in% names(player_stats)) {
    pos <- player_stats$primary_position
    player_stats$is_gk <- as.integer(grepl("Goalkeeper", pos, ignore.case = TRUE))
    player_stats$is_df <- as.integer(grepl("Defender", pos, ignore.case = TRUE))
    player_stats$is_mf <- as.integer(grepl("Midfielder", pos, ignore.case = TRUE))
    player_stats$is_fw <- as.integer(grepl("Forward|Striker", pos, ignore.case = TRUE))
  }

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
#' spm_data <- opta_features %>%
#'   inner_join(rapm_ratings %>% select(player_id, rapm), by = "player_id")
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
  comparison <- dplyr::bind_rows(fbref_imp, opta_imp) %>%
    dplyr::select(source, feature, coefficient, abs_coef) %>%
    dplyr::arrange(dplyr::desc(abs_coef))

  comparison
}
