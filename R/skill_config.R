# Soccer Stat Rating Configuration
# =================================
# Structured definitions for all stats that flow through the skill estimation
# pipeline, mirroring torpverse's skill_config.R pattern.


#' Soccer stat rating definitions
#'
#' Returns a data.frame describing all stats used in the skill estimation and
#' PSR/PSV pipelines. Each stat has a type (rate or efficiency), category
#' (offensive, defensive, goalkeeper, xmetrics), and metadata about adjustment.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{stat_name}{Column name in match stats}
#'     \item{type}{"rate" (Gamma-Poisson) or "efficiency" (Beta-Binomial)}
#'     \item{category}{"offensive", "defensive", "goalkeeper", "xmetrics",
#'       or "general"}
#'     \item{pos_adjusted}{Logical; TRUE if prior is position-specific}
#'   }
#'
#' @export
soccer_stat_rating_definitions <- function() {
  defs <- rbind(
    # --- Offensive rate stats ---
    .srd("goals_p90", "rate", "offensive"),
    .srd("shots_p90", "rate", "offensive"),
    .srd("shots_on_target_p90", "rate", "offensive"),
    .srd("shots_ibox_p90", "rate", "offensive"),
    .srd("shots_obox_p90", "rate", "offensive"),
    .srd("big_chance_scored_p90", "rate", "offensive"),
    .srd("big_chance_missed_p90", "rate", "offensive"),
    .srd("assists_p90", "rate", "offensive"),
    .srd("big_chance_created_p90", "rate", "offensive"),
    .srd("total_att_assist_p90", "rate", "offensive"),
    .srd("key_passes_p90", "rate", "offensive"),
    .srd("through_balls_p90", "rate", "offensive"),
    .srd("touches_opp_box_p90", "rate", "offensive"),
    .srd("pen_area_entries_p90", "rate", "offensive"),
    .srd("final_third_entries_p90", "rate", "offensive"),
    .srd("att_fastbreak_p90", "rate", "offensive"),
    .srd("shot_fastbreak_p90", "rate", "offensive"),
    .srd("att_openplay_p90", "rate", "offensive"),
    .srd("att_setpiece_p90", "rate", "offensive"),
    .srd("att_headed_p90", "rate", "offensive"),
    .srd("att_one_on_one_p90", "rate", "offensive"),
    .srd("penalty_won_p90", "rate", "offensive"),
    .srd("corners_taken_p90", "rate", "offensive"),
    .srd("corners_won_p90", "rate", "offensive"),

    # --- Passing rate stats ---
    .srd("passes_p90", "rate", "offensive"),
    .srd("passes_accurate_p90", "rate", "offensive"),
    .srd("final_third_passes_p90", "rate", "offensive"),
    .srd("long_balls_p90", "rate", "offensive"),
    .srd("crosses_p90", "rate", "offensive"),
    .srd("crosses_open_play_p90", "rate", "offensive"),
    .srd("forward_pass_p90", "rate", "offensive"),
    .srd("fwd_zone_pass_p90", "rate", "offensive"),
    .srd("open_play_pass_p90", "rate", "offensive"),

    # --- Defensive rate stats ---
    .srd("tackles_p90", "rate", "defensive"),
    .srd("tackles_won_p90", "rate", "defensive"),
    .srd("interceptions_p90", "rate", "defensive"),
    .srd("interceptions_won_p90", "rate", "defensive"),
    .srd("clearances_p90", "rate", "defensive"),
    .srd("clearances_effective_p90", "rate", "defensive"),
    .srd("blocks_p90", "rate", "defensive"),
    .srd("blocked_passes_p90", "rate", "defensive"),
    .srd("last_man_tackle_p90", "rate", "defensive"),
    .srd("six_yard_block_p90", "rate", "defensive"),
    .srd("clearance_off_line_p90", "rate", "defensive"),
    .srd("poss_won_def3rd_p90", "rate", "defensive"),
    .srd("poss_won_mid3rd_p90", "rate", "defensive"),
    .srd("poss_won_att3rd_p90", "rate", "defensive"),
    .srd("ball_recovery_p90", "rate", "defensive"),
    .srd("penalty_conceded_p90", "rate", "defensive"),

    # --- Duel rate stats ---
    .srd("duel_won_p90", "rate", "general"),
    .srd("duel_lost_p90", "rate", "general"),
    .srd("aerial_won_p90", "rate", "general"),
    .srd("aerial_lost_p90", "rate", "general"),

    # --- General rate stats ---
    .srd("touches_p90", "rate", "general"),
    .srd("dispossessed_p90", "rate", "general"),
    .srd("turnover_p90", "rate", "general"),
    .srd("times_tackled_p90", "rate", "general"),
    .srd("fouls_p90", "rate", "general"),
    .srd("was_fouled_p90", "rate", "general"),
    .srd("unsuccessful_touch_p90", "rate", "general"),
    .srd("overrun_p90", "rate", "general"),
    .srd("flick_on_p90", "rate", "general"),
    .srd("error_lead_to_shot_p90", "rate", "general"),
    .srd("error_lead_to_goal_p90", "rate", "general"),
    .srd("offtarget_att_assist_p90", "rate", "general"),

    # --- Goalkeeper rate stats ---
    .srd("saves_p90", "rate", "goalkeeper"),
    .srd("goals_conceded_p90", "rate", "goalkeeper"),
    .srd("keeper_sweeper_p90", "rate", "goalkeeper"),
    .srd("attempts_conceded_ibox_p90", "rate", "goalkeeper"),
    .srd("attempts_conceded_obox_p90", "rate", "goalkeeper"),
    .srd("gk_smother_p90", "rate", "goalkeeper"),

    # --- Efficiency stats (Beta-Binomial) ---
    .srd("shot_accuracy", "efficiency", "offensive"),
    .srd("pass_accuracy", "efficiency", "offensive"),
    .srd("final_third_pass_acc", "efficiency", "offensive"),
    .srd("long_ball_accuracy", "efficiency", "offensive"),
    .srd("cross_accuracy", "efficiency", "offensive"),
    .srd("fwd_zone_pass_accuracy", "efficiency", "offensive"),
    .srd("open_play_pass_accuracy", "efficiency", "offensive"),
    .srd("crosses_open_play_accuracy", "efficiency", "offensive"),
    .srd("tackle_success", "efficiency", "defensive"),
    .srd("duel_success", "efficiency", "general"),
    .srd("aerial_success", "efficiency", "general"),
    .srd("bad_touch_rate", "efficiency", "general"),
    .srd("flick_on_accuracy", "efficiency", "general"),
    .srd("keeper_sweeper_accuracy", "efficiency", "goalkeeper"),
    .srd("back_zone_pass_accuracy", "efficiency", "defensive"),
    .srd("chipped_pass_accuracy", "efficiency", "offensive"),
    .srd("long_pass_own_to_opp_accuracy", "efficiency", "offensive"),
    .srd("fifty_fifty_success", "efficiency", "general"),
    .srd("poss_lost_ctrl_per_touch", "efficiency", "general"),

    # --- xMetrics rate stats ---
    .srd("xg_per90", "rate", "xmetrics"),
    .srd("npxg_per90", "rate", "xmetrics"),
    .srd("xa_per90_xmetrics", "rate", "xmetrics"),
    .srd("xpass_overperformance_per90_xmetrics", "rate", "xmetrics"),
    # Finishing over-performance (goals above xG, per-90): signed continuous,
    # treated as rate stats (Gaussian shrink toward ~0 — finishing is noisy).
    .srd("npg_minus_npxg_per90", "rate", "xmetrics"),
    .srd("ibox_g_minus_xg_per90", "rate", "xmetrics"),
    .srd("obox_g_minus_xg_per90", "rate", "xmetrics"),
    # Keeper shot-stopping above expected (GSAA per 90): replaces save_percentage.
    .srd("gsaa_per90", "rate", "goalkeeper")
  )

  defs$pos_adjusted <- TRUE
  defs
}


#' Helper to create a stat rating definition row
#' @keywords internal
.srd <- function(stat_name, type, category) {
  data.frame(
    stat_name = stat_name,
    type = type,
    category = category,
    stringsAsFactors = FALSE
  )
}


#' Soccer position group mapping
#'
#' Maps detailed positions to the four broad groups used for position-specific
#' priors: GK, DEF, MID, FWD.
#'
#' @return A named character vector where names are position patterns and
#'   values are group labels.
#'
#' @export
soccer_position_map <- function() {
  c(
    "GK"         = "GK",
    "Goalkeeper"  = "GK",
    "DEF"        = "DEF",
    "CB"         = "DEF",
    "LB"         = "DEF",
    "RB"         = "DEF",
    "LWB"        = "DEF",
    "RWB"        = "DEF",
    "Back"       = "DEF",
    "WB"         = "DEF",
    "MID"        = "MID",
    "CM"         = "MID",
    "CDM"        = "MID",
    "DM"         = "MID",
    "CAM"        = "MID",
    "AM"         = "MID",
    "LM"         = "MID",
    "RM"         = "MID",
    "Wing"       = "MID",
    "LW"         = "FWD",
    "RW"         = "FWD",
    "FWD"        = "FWD",
    "Forward"    = "FWD",
    "Striker"    = "FWD",
    "CF"         = "FWD",
    "ST"         = "FWD"
  )
}


#' Default stat rating hyperparameters
#'
#' Returns default decay rates and prior strengths for the soccer stat rating
#' estimation pipeline. Can be customized per-stat via named overrides.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{rate}{Decay lambda for rate (per-90) stats, default 0.003
#'       (~231 day half-life)}
#'     \item{efficiency}{Decay lambda for efficiency stats, default 0.002
#'       (~347 day half-life)}
#'     \item{xmetrics}{Decay lambda for xMetrics stats, default 0.003}
#'     \item{prior_90s}{Gamma prior strength in equivalent 90-minute matches,
#'       default 2}
#'     \item{prior_attempts}{Beta prior strength in equivalent attempts,
#'       default 50}
#'   }
#'
#' @export
default_stat_rating_params <- function() {
  get_default_decay_params()
}


#' Get all stat rating column names
#'
#' Returns the stat names from \code{\link{soccer_stat_rating_definitions}},
#' optionally filtered by type or category.
#'
#' @param type Filter: "rate", "efficiency", or NULL for all.
#' @param category Filter: "offensive", "defensive", "goalkeeper", "xmetrics",
#'   "general", or NULL for all.
#'
#' @return Character vector of stat names.
#'
#' @export
stat_rating_names <- function(type = NULL, category = NULL) {
  defs <- soccer_stat_rating_definitions()
  if (!is.null(type)) defs <- defs[defs$type %in% type, ]
  if (!is.null(category)) defs <- defs[defs$category %in% category, ]
  defs$stat_name
}
