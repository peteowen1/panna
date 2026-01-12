# SPM (Statistical Plus-Minus) model functions for panna package
#
# SPM predicts RAPM ratings from box score statistics.
# This serves as the Bayesian prior for RAPM estimation, helping to
# separate players who always appear together (teammate confounding).


#' Aggregate player statistics to per-90 rates
#'
#' Combines match-level statistics into per-90-minute rates for each player.
#' Extracts comprehensive features from all available stat tables for SPM modeling.
#' Includes derived features like success rates and ratios.
#'
#' @param stats_summary Summary stats data frame from process_all_data
#' @param stats_passing Passing stats data frame (optional)
#' @param stats_defense Defense stats data frame (optional)
#' @param stats_possession Possession stats data frame (optional)
#' @param stats_misc Miscellaneous stats data frame (optional) - fouls, aerials, recoveries
#' @param stats_passing_types Passing types data frame (optional) - through balls, switches
#' @param stats_keeper Goalkeeper stats data frame (optional) - saves, post-shot xG
#' @param min_minutes Minimum total minutes for inclusion
#'
#' @return Data frame with per-90 rates for each player
#' @export
aggregate_player_stats <- function(stats_summary,
                                    stats_passing = NULL,
                                    stats_defense = NULL,
                                    stats_possession = NULL,
                                    stats_misc = NULL,
                                    stats_passing_types = NULL,
                                    stats_keeper = NULL,
                                    min_minutes = 450) {
  if (is.null(stats_summary) || nrow(stats_summary) == 0) {
    warning("No summary stats provided")
    return(NULL)
  }

  progress_msg(sprintf("Aggregating %d player-match rows...", nrow(stats_summary)))

  # ==========================================================================
  # CREATE PLAYER_ID FOR CONSISTENT MATCHING
  # ==========================================================================
  # ALWAYS use clean_player_name() to normalize case/whitespace variations
  # This ensures "kylian Mbappé" and "Kylian Mbappé" map to same player
  # We recalculate from player_name even if player_id exists, to ensure

  # consistent normalization (cached data may have old player_id values)
  stats_summary$player_id <- clean_player_name(stats_summary$player_name)

  # Create lookup for canonical player_name (most common spelling per player_id)
  # First clean all player names (remove non-breaking spaces, trim whitespace)
  cleaned_names <- gsub("\u00A0", " ", stats_summary$player_name)
  cleaned_names <- trimws(cleaned_names)

  player_name_lookup <- stats::aggregate(
    cleaned_names,
    by = list(player_id = stats_summary$player_id),
    FUN = function(x) {
      # Return most frequent cleaned name variant, with proper title case
      tbl <- table(x)
      best_name <- names(tbl)[which.max(tbl)]
      # Apply title case for consistent capitalization
      tools::toTitleCase(tolower(best_name))
    }
  )
  names(player_name_lookup)[2] <- "player_name"

  # ==========================================================================
  # SUMMARY STATS - pannadata column mapping
  # ==========================================================================
  summary_cols <- c(
    # Minutes and basic
    total_minutes = "min",
    # Goals and shots
    goals = "gls", assists = "ast", pens = "pk", pen_att = "p_katt",
    shots = "sh", shots_on_target = "so_t",
    # Cards
    yellow_cards = "crd_y", red_cards = "crd_r",
    # Basic actions
    touches = "touches", tackles = "tkl", interceptions = "int", blocks = "blocks",
    # Expected stats
    xg = "x_g", npxg = "npx_g", xa = "x_ag",
    # Shot-creating actions
    sca = "sca", gca = "gca",
    # Passing
    passes_completed = "cmp", passes_attempted = "att",
    pass_completion_pct = "cmp_percent", progressive_passes = "prg_p",
    # Carries
    carries = "carries", progressive_carries = "prg_c",
    # Take-ons (att_2 is take-on attempts in pannadata)
    take_ons_att = "att_2", take_ons_succ = "succ"
  )

  existing_summary <- summary_cols[summary_cols %in% names(stats_summary)]

  player_stats <- stats::aggregate(
    stats_summary[, existing_summary, drop = FALSE],
    by = list(player_id = stats_summary$player_id),
    FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
  )

  # Count matches
  match_counts <- stats::aggregate(
    stats_summary$player_id,
    by = list(player_id = stats_summary$player_id),
    FUN = length
  )
  names(match_counts)[2] <- "n_matches"
  player_stats <- merge(player_stats, match_counts, by = "player_id")

  # Get primary position (most common)
  if ("pos" %in% names(stats_summary)) {
    pos_mode <- stats::aggregate(
      stats_summary$pos,
      by = list(player_id = stats_summary$player_id),
      FUN = function(x) {
        tbl <- table(x[!is.na(x) & x != ""])
        if (length(tbl) == 0) return(NA_character_)
        names(tbl)[which.max(tbl)]
      }
    )
    names(pos_mode)[2] <- "primary_position"
    player_stats <- merge(player_stats, pos_mode, by = "player_id", all.x = TRUE)
  }

  # Add canonical player_name from lookup
  player_stats <- merge(player_stats, player_name_lookup, by = "player_id", all.x = TRUE)

  # Rename columns
  for (new_name in names(existing_summary)) {
    old_name <- existing_summary[new_name]
    if (old_name %in% names(player_stats)) {
      names(player_stats)[names(player_stats) == old_name] <- new_name
    }
  }


  # Filter by min minutes
  player_stats <- player_stats[player_stats$total_minutes >= min_minutes, ]
  if (nrow(player_stats) == 0) {
    warning("No players meet minimum minutes threshold")
    return(NULL)
  }

  mins_per_90 <- player_stats$total_minutes / 90
  player_stats$mins_per_90 <- mins_per_90

  # ==========================================================================
  # PASSING STATS - pannadata columns
  # ==========================================================================
  if (!is.null(stats_passing) && nrow(stats_passing) > 0) {
    passing_cols <- c(
      # Totals
      pass_cmp = "cmp", pass_att = "att", pass_pct = "cmp_percent",
      pass_tot_dist = "tot_dist", pass_prg_dist = "prg_dist",
      # Short passes
      pass_short_cmp = "cmp_2", pass_short_att = "att_2", pass_short_pct = "cmp_percent_2",
      # Medium passes
      pass_med_cmp = "cmp_3", pass_med_att = "att_3", pass_med_pct = "cmp_percent_3",
      # Long passes
      pass_long_cmp = "cmp_4", pass_long_att = "att_4", pass_long_pct = "cmp_percent_4",
      # Key passes and assists
      key_passes = "kp", final_third_passes = "x1_3",
      passes_into_box = "ppa", crosses_into_box = "crs_pa",
      pass_xa = "x_ag", pass_xa2 = "x_a",
      progressive_passes_p = "prg_p"
    )

    existing_pass <- passing_cols[passing_cols %in% names(stats_passing)]

    if (length(existing_pass) > 0) {
      # Create player_id for passing stats
      stats_passing$player_id <- clean_player_name(stats_passing$player_name)

      pass_agg <- stats::aggregate(
        stats_passing[, existing_pass, drop = FALSE],
        by = list(player_id = stats_passing$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_pass)) {
        old_name <- existing_pass[new_name]
        if (old_name %in% names(pass_agg)) {
          names(pass_agg)[names(pass_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, pass_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # DEFENSE STATS - pannadata columns
  # ==========================================================================
  if (!is.null(stats_defense) && nrow(stats_defense) > 0) {
    defense_cols <- c(
      # Tackles
      tackles_def = "tkl", tackles_won = "tkl_w",
      tackles_def_3rd = "def_3rd", tackles_mid_3rd = "mid_3rd",
      tackles_att_3rd = "att_3rd",
      # Challenges (tkl_2 = dribblers tackled, att = challenges attempted)
      challenges_tkl = "tkl_2", challenges_att = "att",
      challenges_pct = "tkl_percent", challenges_lost = "lost",
      # Blocks
      blocks_def = "blocks", blocks_shots = "sh", blocks_pass = "pass",
      # Other
      interceptions_def = "int", tkl_plus_int = "tkl_int",
      clearances = "clr", errors = "err"
    )

    existing_def <- defense_cols[defense_cols %in% names(stats_defense)]

    if (length(existing_def) > 0) {
      # Create player_id for defense stats
      stats_defense$player_id <- clean_player_name(stats_defense$player_name)

      def_agg <- stats::aggregate(
        stats_defense[, existing_def, drop = FALSE],
        by = list(player_id = stats_defense$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_def)) {
        old_name <- existing_def[new_name]
        if (old_name %in% names(def_agg)) {
          names(def_agg)[names(def_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, def_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # POSSESSION STATS - pannadata columns
  # ==========================================================================
  if (!is.null(stats_possession) && nrow(stats_possession) > 0) {
    possession_cols <- c(
      # Touches by location
      touches_poss = "touches",
      touches_def_pen = "def_pen", touches_def_3rd = "def_3rd",
      touches_mid_3rd = "mid_3rd", touches_att_3rd = "att_3rd",
      touches_att_pen = "att_pen", touches_live = "live",
      # Take-ons
      take_ons_att_poss = "att", take_ons_succ_poss = "succ",
      take_ons_pct = "succ_percent", take_ons_tkld = "tkld",
      # Carries
      carries_poss = "carries",
      carry_dist_total = "tot_dist", carry_dist_prg = "prg_dist",
      carries_prg = "prg_c",
      carries_final_3rd = "x1_3", carries_into_box = "cpa",
      miscontrols = "mis", dispossessed = "dis",
      # Receiving
      passes_received = "rec", prg_passes_received = "prg_r"
    )

    existing_poss <- possession_cols[possession_cols %in% names(stats_possession)]

    if (length(existing_poss) > 0) {
      # Create player_id for possession stats
      stats_possession$player_id <- clean_player_name(stats_possession$player_name)

      poss_agg <- stats::aggregate(
        stats_possession[, existing_poss, drop = FALSE],
        by = list(player_id = stats_possession$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_poss)) {
        old_name <- existing_poss[new_name]
        if (old_name %in% names(poss_agg)) {
          names(poss_agg)[names(poss_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, poss_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # MISC STATS - pannadata columns (fouls, aerials, recoveries)
  # ==========================================================================
  if (!is.null(stats_misc) && nrow(stats_misc) > 0) {
    misc_cols <- c(
      # Cards (may duplicate summary but more detailed)
      yellow_cards_misc = "crd_y", red_cards_misc = "crd_r",
      second_yellows = "x2crd_y",
      # Fouls
      fouls_committed = "fls", fouls_drawn = "fld",
      # Other
      offsides = "off", crosses_misc = "crs",
      tackles_won_misc = "tklw",
      # Penalties
      penalties_won = "pkwon", penalties_conceded = "pkcon",
      own_goals = "og",
      # Ball recovery
      recoveries = "recov",
      # Aerials
      aerials_won = "won_aerial", aerials_lost = "lost_aerial",
      aerials_pct = "won_percent_aerial"
    )

    existing_misc <- misc_cols[misc_cols %in% names(stats_misc)]

    if (length(existing_misc) > 0) {
      # Create player_id for misc stats
      stats_misc$player_id <- clean_player_name(stats_misc$player_name)

      misc_agg <- stats::aggregate(
        stats_misc[, existing_misc, drop = FALSE],
        by = list(player_id = stats_misc$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_misc)) {
        old_name <- existing_misc[new_name]
        if (old_name %in% names(misc_agg)) {
          names(misc_agg)[names(misc_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, misc_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # PASSING TYPES - pannadata columns (through balls, switches, set pieces)
  # ==========================================================================
  if (!is.null(stats_passing_types) && nrow(stats_passing_types) > 0) {
    pass_type_cols <- c(
      # Pass types
      passes_live = "live_pass_types",
      passes_dead = "dead_pass_types",
      passes_fk = "fk_pass_types",
      through_balls = "tb_pass_types",
      switches = "sw_pass_types",
      crosses_pt = "crs_pass_types",
      throw_ins = "ti_pass_types",
      corner_kicks = "ck_pass_types",
      # Corner kick types
      corners_inswing = "in_corner_kicks",
      corners_outswing = "out_corner_kicks",
      corners_straight = "str_corner_kicks",
      # Outcomes
      passes_offside = "off_outcomes",
      passes_blocked = "blocks_outcomes"
    )

    existing_pt <- pass_type_cols[pass_type_cols %in% names(stats_passing_types)]

    if (length(existing_pt) > 0) {
      # Create player_id for passing types stats
      stats_passing_types$player_id <- clean_player_name(stats_passing_types$player_name)

      pt_agg <- stats::aggregate(
        stats_passing_types[, existing_pt, drop = FALSE],
        by = list(player_id = stats_passing_types$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_pt)) {
        old_name <- existing_pt[new_name]
        if (old_name %in% names(pt_agg)) {
          names(pt_agg)[names(pt_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, pt_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # KEEPER STATS - pannadata columns (GK-specific)
  # ==========================================================================
  if (!is.null(stats_keeper) && nrow(stats_keeper) > 0) {
    keeper_cols <- c(
      # Shot stopping
      shots_on_target_against = "so_ta",
      goals_against = "ga",
      saves = "saves",
      save_pct = "save_percent",
      # Post-shot xG (quality of saves)
      psxg = "ps_xg",
      # Distribution
      gk_launch_pct = "launch_percent_launched",
      gk_launch_avg_len = "avg_len_launched",
      # Sweeper actions
      gk_opp_passes = "opp_opp",
      gk_passes_stopped = "stp_opp",
      gk_stop_pct = "stp_percent_opp",
      gk_sweeper_actions = "att_sweeper",
      gk_sweeper_dist = "avg_dist_sweeper"
    )

    existing_keeper <- keeper_cols[keeper_cols %in% names(stats_keeper)]

    if (length(existing_keeper) > 0) {
      # Create player_id for keeper stats
      stats_keeper$player_id <- clean_player_name(stats_keeper$player_name)

      keeper_agg <- stats::aggregate(
        stats_keeper[, existing_keeper, drop = FALSE],
        by = list(player_id = stats_keeper$player_id),
        FUN = function(x) sum(as.numeric(x), na.rm = TRUE)
      )

      for (new_name in names(existing_keeper)) {
        old_name <- existing_keeper[new_name]
        if (old_name %in% names(keeper_agg)) {
          names(keeper_agg)[names(keeper_agg) == old_name] <- new_name
        }
      }

      player_stats <- merge(player_stats, keeper_agg, by = "player_id", all.x = TRUE)
    }
  }

  # ==========================================================================
  # CALCULATE PER-90 RATES
  # ==========================================================================
  n_players <- nrow(player_stats)

  # Helper to safely get column or return zeros
  safe_col <- function(col_name) {
    if (col_name %in% names(player_stats)) {
      x <- as.numeric(player_stats[[col_name]])
      ifelse(is.na(x), 0, x)
    } else {
      rep(0, n_players)
    }
  }

  # Helper to safely calculate per-90
  safe_p90 <- function(col_name) {
    safe_col(col_name) / mins_per_90
  }

  # Core per-90 stats
  player_stats$goals_p90 <- safe_p90("goals")
  player_stats$assists_p90 <- safe_p90("assists")
  player_stats$shots_p90 <- safe_p90("shots")
  player_stats$shots_on_target_p90 <- safe_p90("shots_on_target")
  player_stats$xg_p90 <- safe_p90("xg")
  player_stats$npxg_p90 <- safe_p90("npxg")
  player_stats$xa_p90 <- safe_p90("xa")
  player_stats$sca_p90 <- safe_p90("sca")
  player_stats$gca_p90 <- safe_p90("gca")

  # Defensive per-90
  player_stats$tackles_p90 <- safe_p90("tackles")
  player_stats$interceptions_p90 <- safe_p90("interceptions")
  player_stats$blocks_p90 <- safe_p90("blocks")
  player_stats$clearances_p90 <- safe_p90("clearances")
  player_stats$tackles_won_p90 <- safe_p90("tackles_won")
  player_stats$tackles_def_3rd_p90 <- safe_p90("tackles_def_3rd")
  player_stats$tackles_mid_3rd_p90 <- safe_p90("tackles_mid_3rd")
  player_stats$tackles_att_3rd_p90 <- safe_p90("tackles_att_3rd")
  player_stats$blocks_shots_p90 <- safe_p90("blocks_shots")
  player_stats$blocks_pass_p90 <- safe_p90("blocks_pass")
  player_stats$errors_p90 <- safe_p90("errors")

  # Passing per-90
  player_stats$touches_p90 <- safe_p90("touches")
  player_stats$progressive_passes_p90 <- safe_p90("progressive_passes")
  player_stats$key_passes_p90 <- safe_p90("key_passes")
  player_stats$final_third_passes_p90 <- safe_p90("final_third_passes")
  player_stats$passes_into_box_p90 <- safe_p90("passes_into_box")
  player_stats$crosses_into_box_p90 <- safe_p90("crosses_into_box")
  player_stats$pass_long_cmp_p90 <- safe_p90("pass_long_cmp")
  player_stats$pass_short_cmp_p90 <- safe_p90("pass_short_cmp")
  player_stats$pass_med_cmp_p90 <- safe_p90("pass_med_cmp")
  player_stats$pass_tot_dist_p90 <- safe_p90("pass_tot_dist")
  player_stats$pass_prg_dist_p90 <- safe_p90("pass_prg_dist")

  # Possession per-90
  player_stats$carries_p90 <- safe_p90("carries")
  player_stats$progressive_carries_p90 <- safe_p90("progressive_carries")
  player_stats$carries_final_3rd_p90 <- safe_p90("carries_final_3rd")
  player_stats$carries_into_box_p90 <- safe_p90("carries_into_box")
  player_stats$take_ons_att_p90 <- safe_p90("take_ons_att")
  player_stats$take_ons_succ_p90 <- safe_p90("take_ons_succ")
  player_stats$miscontrols_p90 <- safe_p90("miscontrols")
  player_stats$dispossessed_p90 <- safe_p90("dispossessed")
  player_stats$prg_passes_received_p90 <- safe_p90("prg_passes_received")

  # Touches by location per-90
  player_stats$touches_def_3rd_p90 <- safe_p90("touches_def_3rd")
  player_stats$touches_mid_3rd_p90 <- safe_p90("touches_mid_3rd")
  player_stats$touches_att_3rd_p90 <- safe_p90("touches_att_3rd")
  player_stats$touches_att_pen_p90 <- safe_p90("touches_att_pen")

  # Misc stats per-90
  player_stats$fouls_committed_p90 <- safe_p90("fouls_committed")
  player_stats$fouls_drawn_p90 <- safe_p90("fouls_drawn")
  player_stats$offsides_p90 <- safe_p90("offsides")
  player_stats$recoveries_p90 <- safe_p90("recoveries")
  player_stats$aerials_won_p90 <- safe_p90("aerials_won")
  player_stats$aerials_lost_p90 <- safe_p90("aerials_lost")
  player_stats$penalties_won_p90 <- safe_p90("penalties_won")
  player_stats$penalties_conceded_p90 <- safe_p90("penalties_conceded")

  # Passing types per-90
  player_stats$through_balls_p90 <- safe_p90("through_balls")
  player_stats$switches_p90 <- safe_p90("switches")
  player_stats$crosses_p90 <- safe_p90("crosses_pt")
  player_stats$corner_kicks_p90 <- safe_p90("corner_kicks")
  player_stats$passes_dead_p90 <- safe_p90("passes_dead")

  # Keeper stats per-90 (only meaningful for GKs)
  player_stats$saves_p90 <- safe_p90("saves")
  player_stats$goals_against_p90 <- safe_p90("goals_against")
  player_stats$gk_sweeper_actions_p90 <- safe_p90("gk_sweeper_actions")

  # ==========================================================================
  # DERIVED FEATURES (ratios and success rates)
  # ==========================================================================
  safe_div <- function(num, denom) {
    num <- as.numeric(num)
    denom <- as.numeric(denom)
    ifelse(is.na(denom) | denom == 0, 0, num / denom)
  }

  # Shooting efficiency
  player_stats$shot_accuracy <- safe_div(safe_col("shots_on_target"), safe_col("shots"))
  player_stats$goals_per_shot <- safe_div(safe_col("goals"), safe_col("shots"))
  player_stats$xg_per_shot <- safe_div(safe_col("xg"), safe_col("shots"))
  player_stats$goals_minus_xg <- player_stats$goals_p90 - player_stats$xg_p90
  player_stats$npxg_plus_xa_p90 <- player_stats$npxg_p90 + player_stats$xa_p90

  # Passing efficiency
  player_stats$pass_completion <- safe_div(safe_col("passes_completed"), safe_col("passes_attempted"))
  player_stats$pass_short_success <- safe_div(safe_col("pass_short_cmp"), safe_col("pass_short_att"))
  player_stats$pass_med_success <- safe_div(safe_col("pass_med_cmp"), safe_col("pass_med_att"))
  player_stats$pass_long_success <- safe_div(safe_col("pass_long_cmp"), safe_col("pass_long_att"))
  player_stats$long_pass_ratio <- safe_div(safe_col("pass_long_att"), safe_col("pass_att"))

  # Take-on success
  player_stats$take_on_success <- safe_div(safe_col("take_ons_succ"), safe_col("take_ons_att"))

  # Tackle success
  player_stats$tackle_success <- safe_div(safe_col("tackles_won"), safe_col("tackles"))
  player_stats$challenge_success <- safe_div(safe_col("challenges_tkl"), safe_col("challenges_att"))

  # Touch location ratios (indicates where player operates on pitch)
  total_touches <- safe_col("touches_poss")
  total_touches <- ifelse(total_touches == 0, safe_col("touches"), total_touches)
  player_stats$touch_def_3rd_pct <- safe_div(safe_col("touches_def_3rd"), total_touches)
  player_stats$touch_mid_3rd_pct <- safe_div(safe_col("touches_mid_3rd"), total_touches)
  player_stats$touch_att_3rd_pct <- safe_div(safe_col("touches_att_3rd"), total_touches)
  player_stats$touch_att_pen_pct <- safe_div(safe_col("touches_att_pen"), total_touches)

  # Ball retention
  turnovers <- safe_col("miscontrols") + safe_col("dispossessed")
  player_stats$turnovers_p90 <- turnovers / mins_per_90
  total_carries <- safe_col("carries_poss")
  total_carries <- ifelse(total_carries == 0, safe_col("carries"), total_carries)
  player_stats$carry_retention <- 1 - safe_div(turnovers, total_carries)

  # Progressive actions per touch
  prg_actions <- safe_col("progressive_carries") + safe_col("progressive_passes")
  player_stats$prg_actions_per_touch <- safe_div(prg_actions, total_touches)

  # Aerial duel success
  total_aerials <- safe_col("aerials_won") + safe_col("aerials_lost")
  player_stats$aerial_success <- safe_div(safe_col("aerials_won"), total_aerials)
  player_stats$aerials_total_p90 <- total_aerials / mins_per_90

  # Foul differential (fouls drawn - committed, higher = better)
  player_stats$foul_differential_p90 <- player_stats$fouls_drawn_p90 - player_stats$fouls_committed_p90

  # Goalkeeper metrics
  player_stats$gk_save_pct <- safe_div(safe_col("saves"), safe_col("shots_on_target_against"))
  # Goals prevented = PSxG - Goals Against (higher = better shot stopping)
  player_stats$gk_goals_prevented <- safe_col("psxg") - safe_col("goals_against")
  player_stats$gk_goals_prevented_p90 <- player_stats$gk_goals_prevented / mins_per_90

  # ==========================================================================
  # POSITION DUMMIES
  # ==========================================================================
  if ("primary_position" %in% names(player_stats)) {
    pos <- player_stats$primary_position
    # Extract first position if multiple (e.g., "MF,FW" -> "MF")
    pos <- sapply(strsplit(as.character(pos), ","), `[`, 1)
    player_stats$is_gk <- as.integer(grepl("GK", pos, ignore.case = TRUE))
    player_stats$is_df <- as.integer(grepl("DF|CB|LB|RB|WB", pos, ignore.case = TRUE))
    player_stats$is_mf <- as.integer(grepl("MF|CM|DM|AM", pos, ignore.case = TRUE))
    player_stats$is_fw <- as.integer(grepl("FW|ST|CF|LW|RW", pos, ignore.case = TRUE))
  }

  # Replace any remaining NAs with 0 for numeric columns
  numeric_cols <- sapply(player_stats, is.numeric)
  player_stats[numeric_cols] <- lapply(player_stats[numeric_cols], function(x) {
    ifelse(is.na(x), 0, x)
  })

  progress_msg(sprintf("Aggregated stats for %d players with %d features",
                       nrow(player_stats), ncol(player_stats)))

  player_stats
}


#' Create SPM prior vector for RAPM
#'
#' Creates a prior vector aligned with RAPM player IDs.
#'
#' @param spm_predictions Named vector or data frame of SPM predictions
#' @param player_mapping Data frame with player_id and player_name
#' @param default_prior Value for players without SPM prediction
#'
#' @return Named vector of priors (keyed by player_id)
#' @export
create_spm_prior <- function(spm_predictions, player_mapping, default_prior = 0) {
  # Handle data frame input
  if (is.data.frame(spm_predictions)) {
    if ("spm" %in% names(spm_predictions) && "player_name" %in% names(spm_predictions)) {
      spm_predictions <- stats::setNames(spm_predictions$spm, spm_predictions$player_name)
    } else {
      stop("spm_predictions data frame must have 'spm' and 'player_name' columns")
    }
  }

  # Create lookup from player_name to player_id
  name_to_id <- stats::setNames(
    player_mapping$player_id,
    player_mapping$player_name
  )

  # Initialize prior vector for all players in mapping
  all_player_ids <- unique(player_mapping$player_id)
  prior <- stats::setNames(rep(default_prior, length(all_player_ids)), all_player_ids)

  # Fill in SPM predictions where available
  matched <- 0
  for (player_name in names(spm_predictions)) {
    if (player_name %in% names(name_to_id)) {
      player_id <- name_to_id[player_name]
      if (player_id %in% names(prior)) {
        prior[player_id] <- spm_predictions[player_name]
        matched <- matched + 1
      }
    }
  }

  progress_msg(sprintf("SPM prior: matched %d of %d players", matched, length(spm_predictions)))

  prior
}


#' Build prior vector for RAPM from SPM predictions
#'
#' Creates a named prior vector aligned with player IDs from SPM rating predictions.
#' This is a vectorized helper used by xRAPM and seasonal ratings to build priors
#' from SPM predictions without manual for-loops.
#'
#' @param spm_data Data frame with player_name and the SPM column to use
#' @param spm_col Name of the column containing SPM predictions
#' @param player_mapping Data frame with player_id and player_name from RAPM
#' @param default Value for players without SPM prediction (default 0)
#'
#' @return Named vector of priors keyed by player_id
#' @export
#'
#' @examples
#' \dontrun{
#' offense_prior <- build_prior_vector(
#'   spm_data = offense_spm_ratings,
#'   spm_col = "offense_spm",
#'   player_mapping = rapm_data$player_mapping
#' )
#' }
build_prior_vector <- function(spm_data, spm_col, player_mapping, default = 0) {
  # Create lookup from player_name to player_id
  name_to_id <- stats::setNames(
    player_mapping$player_id,
    player_mapping$player_name
  )

  # Initialize prior vector for all players in mapping
  all_player_ids <- unique(player_mapping$player_id)
  prior <- stats::setNames(rep(default, length(all_player_ids)), all_player_ids)

  # Vectorized matching: find SPM data rows that match player names in mapping
  matched_names <- intersect(spm_data$player_name, names(name_to_id))

  for (pname in matched_names) {
    pid <- name_to_id[pname]
    spm_idx <- which(spm_data$player_name == pname)[1]
    if (!is.na(spm_idx) && pid %in% names(prior)) {
      prior[pid] <- spm_data[[spm_col]][spm_idx]
    }
  }

  n_matched <- sum(prior != default)
  progress_msg(sprintf("Prior '%s': matched %d of %d players", spm_col, n_matched, nrow(spm_data)))

  prior
}


#' Prepare SPM regression data
#'
#' Joins player features with RAPM ratings for SPM model fitting.
#'
#' @param player_features Data frame from create_player_feature_matrix
#' @param rapm_ratings Data frame from extract_rapm_coefficients
#'
#' @return Data frame ready for SPM regression
#' @export
prepare_spm_regression_data <- function(player_features, rapm_ratings) {
  # Match on player name or ID
  if ("player_id" %in% names(player_features) && "player_id" %in% names(rapm_ratings)) {
    data <- player_features %>%
      dplyr::inner_join(
        rapm_ratings %>% dplyr::select(player_id, rapm),
        by = "player_id"
      )
  } else if ("player_name" %in% names(player_features) && "player_name" %in% names(rapm_ratings)) {
    data <- player_features %>%
      dplyr::inner_join(
        rapm_ratings %>% dplyr::select(player_name, rapm),
        by = "player_name"
      )
  } else {
    stop("Cannot match player_features and rapm_ratings: no common ID column")
  }

  data
}


#' Fit SPM model
#'
#' Fits an elastic net model predicting RAPM from box score statistics.
#' Weights observations by minutes played (sqrt transform) by default to reduce
#' influence of noisy low-minute players whose RAPM and per-90 stats are unreliable.
#'
#' @param data Data frame from prepare_spm_regression_data or aggregate_player_stats
#'   joined with RAPM ratings
#' @param predictor_cols Character vector of predictor column names
#' @param alpha Elastic net mixing (0=ridge, 1=lasso, default 0.5)
#' @param nfolds Number of CV folds
#' @param weight_by_minutes Whether to weight observations by total_minutes (default TRUE).
#'   Reduces influence of noisy low-minute estimates on model coefficients.
#' @param weight_transform How to transform minutes for weighting:
#'   "sqrt" (default) - square root of minutes (moderate weighting)
#'   "linear" - raw minutes (strong weighting toward high-minute players)
#'   "log" - log of minutes (gentle weighting)
#'   "none" - equal weights
#'
#' @return Fitted glmnet model with metadata
#' @export
fit_spm_model <- function(data, predictor_cols = NULL, alpha = 0.5, nfolds = 10,
                          weight_by_minutes = TRUE, weight_transform = "sqrt") {
  # Default predictors: per-90 stats that predict impact
  if (is.null(predictor_cols)) {
    # Try _p90 columns first, then _p100 for backward compatibility
    predictor_cols <- names(data)[grepl("_p90$", names(data))]
    if (length(predictor_cols) == 0) {
      predictor_cols <- names(data)[grepl("_p100$", names(data))]
    }
  }

  available_cols <- intersect(predictor_cols, names(data))
  if (length(available_cols) == 0) {
    stop("No valid predictor columns found")
  }

  # Prepare data
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data$rapm

  # Calculate weights based on minutes played
  weights <- NULL
  if (weight_by_minutes && "total_minutes" %in% names(data)) {
    mins <- data$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      "none" = rep(1, length(mins)),
      sqrt(mins)  # default to sqrt
    )
    # Normalize weights to sum to n (so scale is comparable to unweighted)
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]
  if (!is.null(weights)) {
    weights <- weights[complete_idx]
  }

  progress_msg(paste("Fitting SPM model with", ncol(X), "predictors on", nrow(X), "players"))
  if (!is.null(weights)) {
    progress_msg(sprintf("  Weighting by minutes (%s transform)", weight_transform))
  }

  # Fit cross-validated elastic net
  cv_fit <- glmnet::cv.glmnet(
    x = X,
    y = y,
    weights = weights,
    alpha = alpha,
    standardize = TRUE,
    nfolds = nfolds,
    type.measure = "mse"
  )

  # Add metadata
  cv_fit$panna_metadata <- list(
    type = "spm",
    alpha = alpha,
    predictor_cols = available_cols,
    n_observations = length(y),
    lambda_min = cv_fit$lambda.min,
    lambda_1se = cv_fit$lambda.1se,
    weight_by_minutes = weight_by_minutes,
    weight_transform = if (weight_by_minutes) weight_transform else "none"
  )

  # Calculate in-sample R-squared using actual predictions
  lambda_min <- cv_fit$lambda.min
  y_pred <- as.vector(stats::predict(cv_fit, newx = X, s = lambda_min))

  if (!is.null(weights)) {
    # Weighted R-squared for weighted models
    w <- weights / sum(weights)
    y_mean_w <- sum(w * y)
    ss_res <- sum(weights * (y - y_pred)^2)
    ss_tot <- sum(weights * (y - y_mean_w)^2)
    r_squared <- 1 - ss_res / ss_tot
    progress_msg(sprintf("SPM fit complete. R-squared: %.3f (weighted in-sample)", r_squared))
  } else {
    # Unweighted R-squared
    ss_res <- sum((y - y_pred)^2)
    ss_tot <- sum((y - mean(y))^2)
    r_squared <- 1 - ss_res / ss_tot
    progress_msg(sprintf("SPM fit complete. R-squared: %.3f (in-sample)", r_squared))
  }

  cv_fit
}


#' Fit SPM model using XGBoost
#'
#' Fits an XGBoost model predicting RAPM from box score statistics.
#' Uses xgb.cv to find optimal number of boosting rounds via early stopping.
#'
#' @param data Data frame from prepare_spm_regression_data or aggregate_player_stats
#'   joined with RAPM ratings
#' @param predictor_cols Character vector of predictor column names
#' @param nfolds Number of CV folds (default 10)
#' @param max_depth Maximum tree depth (default 4)
#' @param eta Learning rate (default 0.1)
#' @param subsample Row subsampling ratio (default 0.8)
#' @param colsample_bytree Column subsampling ratio (default 0.8)
#' @param nrounds Maximum boosting rounds (default 500, uses early stopping)
#' @param early_stopping_rounds Stop if no improvement for this many rounds (default 20)
#' @param weight_by_minutes Whether to weight observations by total_minutes (default TRUE)
#' @param weight_transform How to transform minutes for weighting: "sqrt", "linear", "log"
#' @param verbose Print progress (0=silent, 1=performance, 2=details)
#'
#' @return List with xgb model, cv results, and metadata
#' @export
fit_spm_xgb <- function(data, predictor_cols = NULL, nfolds = 10,
                         max_depth = 4, eta = 0.1,
                         subsample = 0.8, colsample_bytree = 0.8,
                         nrounds = 500, early_stopping_rounds = 20,
                         weight_by_minutes = TRUE, weight_transform = "sqrt",
                         verbose = 1) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop("xgboost package required. Install with: install.packages('xgboost')")
  }

  # Default predictors: per-90 stats
 if (is.null(predictor_cols)) {
    predictor_cols <- names(data)[grepl("_p90$", names(data))]
    if (length(predictor_cols) == 0) {
      predictor_cols <- names(data)[grepl("_p100$", names(data))]
    }
  }

  available_cols <- intersect(predictor_cols, names(data))
  if (length(available_cols) == 0) {
    stop("No valid predictor columns found")
  }

  # Prepare data
  X <- as.matrix(data[, available_cols, drop = FALSE])
  y <- data$rapm

  # Calculate weights
  weights <- NULL
  if (weight_by_minutes && "total_minutes" %in% names(data)) {
    mins <- data$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      "none" = rep(1, length(mins)),
      sqrt(mins)
    )
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Remove rows with NA
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]
  if (!is.null(weights)) {
    weights <- weights[complete_idx]
  }

  progress_msg(sprintf("Fitting XGBoost SPM with %d predictors on %d players", ncol(X), nrow(X)))

  # Create DMatrix
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = weights)

  # XGBoost parameters
  params <- list(
    objective = "reg:squarederror",
    max_depth = max_depth,
    eta = eta,
    subsample = subsample,
    colsample_bytree = colsample_bytree,
    eval_metric = "rmse"
  )

  # Cross-validation to find optimal nrounds
  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nfold = nfolds,
    early_stopping_rounds = early_stopping_rounds,
    verbose = verbose,
    print_every_n = 50
  )

  # Extract best iteration (handle different xgboost versions)
  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || length(best_nrounds) == 0) {
    # Fallback: find iteration with minimum test RMSE
    eval_log <- cv_result$evaluation_log
    best_nrounds <- which.min(eval_log$test_rmse_mean)
  }
  best_rmse <- cv_result$evaluation_log$test_rmse_mean[best_nrounds]

  progress_msg(sprintf("XGBoost CV: best iteration = %d, CV RMSE = %.4f", best_nrounds, best_rmse))

  # Fit final model with optimal nrounds
  final_model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0
  )

  # Calculate in-sample metrics
  y_pred <- stats::predict(final_model, dtrain)
  if (!is.null(weights)) {
    w <- weights / sum(weights)
    y_mean_w <- sum(w * y)
    ss_res <- sum(weights * (y - y_pred)^2)
    ss_tot <- sum(weights * (y - y_mean_w)^2)
    train_rmse <- sqrt(sum(weights * (y - y_pred)^2) / sum(weights))
  } else {
    ss_res <- sum((y - y_pred)^2)
    ss_tot <- sum((y - mean(y))^2)
    train_rmse <- sqrt(mean((y - y_pred)^2))
  }
  r_squared <- 1 - ss_res / ss_tot

  progress_msg(sprintf("XGBoost fit complete. Train RMSE: %.4f, CV RMSE: %.4f, R^2: %.3f",
                       train_rmse, best_rmse, r_squared))

  # Get feature importance
  importance <- xgboost::xgb.importance(
    feature_names = available_cols,
    model = final_model
  )

  # Return results
  result <- list(
    model = final_model,
    cv_result = cv_result,
    importance = importance,
    best_nrounds = best_nrounds,
    train_rmse = train_rmse,
    best_cv_rmse = best_rmse,
    r_squared = r_squared,
    panna_metadata = list(
      type = "spm_xgb",
      predictor_cols = available_cols,
      n_observations = length(y),
      params = params,
      best_nrounds = best_nrounds,
      weight_by_minutes = weight_by_minutes,
      weight_transform = if (weight_by_minutes) weight_transform else "none"
    )
  )

  class(result) <- c("spm_xgb", "list")
  result
}


#' Calculate SPM ratings using XGBoost model
#'
#' @param player_features Data frame of player features
#' @param spm_xgb_model Fitted XGBoost SPM model from fit_spm_xgb
#'
#' @return Data frame with SPM ratings
#' @export
calculate_spm_ratings_xgb <- function(player_features, spm_xgb_model) {
  predictor_cols <- spm_xgb_model$panna_metadata$predictor_cols

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- stats::predict(spm_xgb_model$model, X)

  # Create output
  result <- player_features %>%
    dplyr::select(dplyr::any_of(c("player_id", "player_name", "n_matches", "total_minutes"))) %>%
    dplyr::mutate(spm = spm_pred) %>%
    dplyr::arrange(dplyr::desc(.data$spm))

  result
}


#' Calculate blended SPM ratings from Elastic Net and XGBoost
#'
#' Combines predictions from both model types with configurable weighting.
#' The blend can improve robustness by capturing both linear (Elastic Net)
#' and non-linear (XGBoost) relationships between box scores and RAPM.
#'
#' @param player_features Data frame of player features
#' @param model_glmnet Fitted Elastic Net SPM model from fit_spm_model
#' @param model_xgb Fitted XGBoost SPM model from fit_spm_xgb
#' @param weight_glmnet Weight for Elastic Net predictions (default 0.5)
#'
#' @return Data frame with blended SPM ratings plus individual model predictions
#' @export
calculate_spm_blend <- function(player_features, model_glmnet, model_xgb,
                                weight_glmnet = 0.5) {
  # Get predictions from each model
  spm_glmnet <- calculate_spm_ratings(player_features, model_glmnet)
  spm_xgb <- calculate_spm_ratings_xgb(player_features, model_xgb)

  # Blend predictions
  result <- spm_glmnet %>%
    dplyr::rename(spm_glmnet = spm) %>%
    dplyr::inner_join(
      spm_xgb %>% dplyr::select(player_id, spm_xgb = spm),
      by = "player_id"
    ) %>%
    dplyr::mutate(
      spm = weight_glmnet * spm_glmnet + (1 - weight_glmnet) * spm_xgb
    ) %>%
    dplyr::arrange(dplyr::desc(.data$spm))

  result
}


#' Extract SPM coefficients
#'
#' Gets feature weights from fitted SPM model.
#'
#' @param model Fitted SPM model from fit_spm_model
#' @param lambda Which lambda to use ("min" or "1se")
#'
#' @return Named vector of coefficients
#' @export
extract_spm_coefficients <- function(model, lambda = "min") {
  lambda_val <- if (lambda == "min") model$lambda.min else model$lambda.1se

  coefs <- stats::coef(model, s = lambda_val)
  coef_vec <- as.vector(coefs)
  names(coef_vec) <- rownames(coefs)

  # Remove intercept for display
  coef_vec
}


#' Calculate SPM ratings for all players
#'
#' Applies SPM model to predict RAPM for all players with features.
#'
#' @param player_features Data frame of player features
#' @param spm_model Fitted SPM model
#' @param lambda Which lambda to use
#'
#' @return Data frame with SPM ratings
#' @export
calculate_spm_ratings <- function(player_features, spm_model, lambda = "min") {
  predictor_cols <- spm_model$panna_metadata$predictor_cols
  lambda_val <- if (lambda == "min") spm_model$lambda.min else spm_model$lambda.1se

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])

  # Handle missing values
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- as.vector(stats::predict(spm_model, newx = X, s = lambda_val))

  # Create output data frame
  result <- player_features %>%
    dplyr::select(dplyr::any_of(c("player_id", "player_name", "n_games", "total_minutes"))) %>%
    dplyr::mutate(spm = spm_pred) %>%
    dplyr::arrange(dplyr::desc(.data$spm))

  result
}


#' Calculate offensive SPM
#'
#' Fits SPM model for offensive contribution only.
#'
#' @param data SPM regression data
#' @param offensive_cols Offensive predictor columns
#' @param alpha Elastic net mixing
#'
#' @return Fitted model for offensive SPM
#' @export
calculate_offensive_spm <- function(data, offensive_cols = NULL, alpha = 0.5) {
  if (is.null(offensive_cols)) {
    offensive_cols <- c("npxG_p100", "xG_p100", "Sh_p100", "SoT_p100",
                        "Ast_p100", "xAG_p100", "SCA_p100", "GCA_p100",
                        "PrgP_p100", "PrgC_p100", "Carries_p100")
  }

  fit_spm_model(data, predictor_cols = offensive_cols, alpha = alpha)
}


#' Calculate defensive SPM
#'
#' Fits SPM model for defensive contribution only.
#'
#' @param data SPM regression data
#' @param defensive_cols Defensive predictor columns
#' @param alpha Elastic net mixing
#'
#' @return Fitted model for defensive SPM
#' @export
calculate_defensive_spm <- function(data, defensive_cols = NULL, alpha = 0.5) {
  if (is.null(defensive_cols)) {
    defensive_cols <- c("Tkl_p100", "Int_p100", "Blocks_p100",
                        "TklWon_p100", "Clr_p100")
  }

  fit_spm_model(data, predictor_cols = defensive_cols, alpha = alpha)
}


#' Validate SPM prediction accuracy
#'
#' Assesses how well SPM predicts RAPM. Supports weighted metrics to match
#' weighted model fitting - we care more about accuracy for high-minute players
#' whose RAPM estimates are more reliable.
#'
#' @param spm_ratings Data frame with SPM predictions (must include total_minutes for weighting)
#' @param rapm_ratings Data frame with actual RAPM
#' @param weight_by_minutes Whether to weight metrics by minutes (default TRUE)
#' @param weight_transform Transform for weights: "sqrt" (default), "linear", "log"
#'
#' @return List with validation metrics (both weighted and unweighted)
#' @export
validate_spm_prediction <- function(spm_ratings, rapm_ratings,
                                     weight_by_minutes = TRUE,
                                     weight_transform = "sqrt") {
  # Join predictions with actuals
  # Find common join columns that exist in BOTH dataframes
  possible_keys <- c("player_id", "player_name")
  join_cols <- intersect(intersect(names(spm_ratings), names(rapm_ratings)), possible_keys)

  if (length(join_cols) == 0) {
    warning("No common join columns (player_id or player_name) found")
    return(NULL)
  }

  comparison <- spm_ratings %>%
    dplyr::inner_join(
      rapm_ratings %>%
        dplyr::select(dplyr::all_of(join_cols), rapm),
      by = join_cols
    )

  if (nrow(comparison) == 0) {
    warning("No matching players between SPM and RAPM ratings")
    return(NULL)
  }

  # Calculate weights
  weights <- rep(1, nrow(comparison))
  if (weight_by_minutes && "total_minutes" %in% names(comparison)) {
    mins <- comparison$total_minutes
    weights <- switch(weight_transform,
      "sqrt" = sqrt(mins),
      "linear" = mins,
      "log" = log(mins + 1),
      sqrt(mins)
    )
    weights <- weights / mean(weights, na.rm = TRUE)
  }

  # Unweighted metrics
  residuals <- comparison$rapm - comparison$spm
  ss_res <- sum(residuals^2, na.rm = TRUE)
  ss_tot <- sum((comparison$rapm - mean(comparison$rapm, na.rm = TRUE))^2, na.rm = TRUE)

  r_squared_unweighted <- 1 - ss_res / ss_tot
  rmse_unweighted <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae_unweighted <- mean(abs(residuals), na.rm = TRUE)

 # Weighted metrics
  weighted_mean_rapm <- sum(weights * comparison$rapm, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  ss_res_w <- sum(weights * residuals^2, na.rm = TRUE)
  ss_tot_w <- sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE)

  r_squared_weighted <- 1 - ss_res_w / ss_tot_w
  rmse_weighted <- sqrt(sum(weights * residuals^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  mae_weighted <- sum(weights * abs(residuals), na.rm = TRUE) / sum(weights, na.rm = TRUE)

  # Weighted correlation
  cov_w <- sum(weights * (comparison$spm - mean(comparison$spm)) *
               (comparison$rapm - weighted_mean_rapm), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  sd_spm_w <- sqrt(sum(weights * (comparison$spm - mean(comparison$spm))^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  sd_rapm_w <- sqrt(sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  cor_weighted <- cov_w / (sd_spm_w * sd_rapm_w)

  metrics <- list(
    n_players = nrow(comparison),
    # Unweighted
    r_squared = r_squared_unweighted,
    rmse = rmse_unweighted,
    mae = mae_unweighted,
    correlation = stats::cor(comparison$spm, comparison$rapm, use = "complete.obs"),
    # Weighted
    r_squared_weighted = r_squared_weighted,
    rmse_weighted = rmse_weighted,
    mae_weighted = mae_weighted,
    correlation_weighted = cor_weighted,
    # Data
    comparison = comparison
  )

  progress_msg(sprintf("SPM validation: R^2 = %.3f (unweighted), R^2 = %.3f (weighted by %s mins)",
                       r_squared_unweighted, r_squared_weighted, weight_transform))

  metrics
}


#' Get top SPM feature importance
#'
#' Identifies the most important features in the SPM model.
#'
#' @param model Fitted SPM model
#' @param n Number of top features to return
#' @param lambda Which lambda to use
#'
#' @return Data frame of top features by absolute coefficient
#' @export
get_spm_feature_importance <- function(model, n = 10, lambda = "min") {
  coefs <- extract_spm_coefficients(model, lambda)

  # Remove intercept
  coefs <- coefs[names(coefs) != "(Intercept)"]

  # Sort by absolute value
  importance <- data.frame(
    feature = names(coefs),
    coefficient = as.vector(coefs),
    abs_coef = abs(as.vector(coefs)),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::filter(.data$coefficient != 0) %>%
    dplyr::arrange(dplyr::desc(.data$abs_coef)) %>%
    utils::head(n)

  importance
}
