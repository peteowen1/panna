# SPM (Statistical Plus-Minus) model functions for panna package
#
# SPM predicts RAPM ratings from box score statistics.
# This serves as the Bayesian prior for RAPM estimation, helping to
# separate players who always appear together (teammate confounding).
#
# For Opta-specific SPM functions, see spm_opta.R


# =============================================================================
# Internal Helper Functions for aggregate_player_stats()
# =============================================================================

#' Aggregate a single stat table by player_id
#'
#' Internal helper to aggregate match-level stats to player totals.
#'
#' @param stats_df Data frame with player stats
#' @param col_mapping Named vector mapping output col names to input col names
#'
#' @return Data frame aggregated by player_id with renamed columns
#' @keywords internal
.aggregate_stat_table <- function(stats_df, col_mapping) {
  if (is.null(stats_df) || nrow(stats_df) == 0) {
    return(NULL)
  }

  existing_cols <- col_mapping[col_mapping %in% names(stats_df)]
  if (length(existing_cols) == 0) {
    return(NULL)
  }

  # Create player_id for consistent matching
  # NOTE: FBref stats tables don't include player_href, so we use name-derived IDs.
  # Real FBref 8-char hex IDs are only available in lineups/events data.
  # The Opta pipeline uses real Opta player IDs throughout.
  dt <- data.table::as.data.table(stats_df)
  dt[, player_id := clean_player_name(player_name)]

  # Aggregate using data.table (much faster than stats::aggregate)
  agg <- dt[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
            by = player_id, .SDcols = existing_cols]
  data.table::setDF(agg)

  rename_columns(agg, existing_cols)
}


#' Get summary stats column mapping
#' @keywords internal
.get_summary_col_mapping <- function() {
  c(
    total_minutes = "min",
    goals = "gls", assists = "ast", pens = "pk", pen_att = "p_katt",
    shots = "sh", shots_on_target = "so_t",
    yellow_cards = "crd_y", red_cards = "crd_r",
    touches = "touches", tackles = "tkl", interceptions = "int", blocks = "blocks",
    xg = "x_g", npxg = "npx_g", xa = "x_ag",
    sca = "sca", gca = "gca",
    passes_completed = "cmp", passes_attempted = "att",
    pass_completion_pct = "cmp_percent", progressive_passes = "prg_p",
    carries = "carries", progressive_carries = "prg_c",
    take_ons_att = "att_2", take_ons_succ = "succ"
  )
}


#' Get passing stats column mapping
#' @keywords internal
.get_passing_col_mapping <- function() {
  c(
    pass_cmp = "cmp", pass_att = "att", pass_pct = "cmp_percent",
    pass_tot_dist = "tot_dist", pass_prg_dist = "prg_dist",
    pass_short_cmp = "cmp_2", pass_short_att = "att_2", pass_short_pct = "cmp_percent_2",
    pass_med_cmp = "cmp_3", pass_med_att = "att_3", pass_med_pct = "cmp_percent_3",
    pass_long_cmp = "cmp_4", pass_long_att = "att_4", pass_long_pct = "cmp_percent_4",
    key_passes = "kp", final_third_passes = "x1_3",
    passes_into_box = "ppa", crosses_into_box = "crs_pa",
    pass_xa = "x_ag", pass_xa2 = "x_a",
    progressive_passes_p = "prg_p"
  )
}


#' Get defense stats column mapping
#' @keywords internal
.get_defense_col_mapping <- function() {
  c(
    tackles_def = "tkl", tackles_won = "tkl_w",
    tackles_def_3rd = "def_3rd", tackles_mid_3rd = "mid_3rd",
    tackles_att_3rd = "att_3rd",
    challenges_tkl = "tkl_2", challenges_att = "att",
    challenges_pct = "tkl_percent", challenges_lost = "lost",
    blocks_def = "blocks", blocks_shots = "sh", blocks_pass = "pass",
    interceptions_def = "int", tkl_plus_int = "tkl_int",
    clearances = "clr", errors = "err"
  )
}


#' Get possession stats column mapping
#' @keywords internal
.get_possession_col_mapping <- function() {
  c(
    touches_poss = "touches",
    touches_def_pen = "def_pen", touches_def_3rd = "def_3rd",
    touches_mid_3rd = "mid_3rd", touches_att_3rd = "att_3rd",
    touches_att_pen = "att_pen", touches_live = "live",
    take_ons_att_poss = "att", take_ons_succ_poss = "succ",
    take_ons_pct = "succ_percent", take_ons_tkld = "tkld",
    carries_poss = "carries",
    carry_dist_total = "tot_dist", carry_dist_prg = "prg_dist",
    carries_prg = "prg_c",
    carries_final_3rd = "x1_3", carries_into_box = "cpa",
    miscontrols = "mis", dispossessed = "dis",
    passes_received = "rec", prg_passes_received = "prg_r"
  )
}


#' Get misc stats column mapping
#' @keywords internal
.get_misc_col_mapping <- function() {
  c(
    yellow_cards_misc = "crd_y", red_cards_misc = "crd_r",
    second_yellows = "x2crd_y",
    fouls_committed = "fls", fouls_drawn = "fld",
    offsides = "off", crosses_misc = "crs",
    tackles_won_misc = "tklw",
    penalties_won = "pkwon", penalties_conceded = "pkcon",
    own_goals = "og",
    recoveries = "recov",
    aerials_won = "won_aerial", aerials_lost = "lost_aerial",
    aerials_pct = "won_percent_aerial"
  )
}


#' Get passing types column mapping
#' @keywords internal
.get_passing_types_col_mapping <- function() {
  c(
    passes_live = "live_pass_types",
    passes_dead = "dead_pass_types",
    passes_fk = "fk_pass_types",
    through_balls = "tb_pass_types",
    switches = "sw_pass_types",
    crosses_pt = "crs_pass_types",
    throw_ins = "ti_pass_types",
    corner_kicks = "ck_pass_types",
    corners_inswing = "in_corner_kicks",
    corners_outswing = "out_corner_kicks",
    corners_straight = "str_corner_kicks",
    passes_offside = "off_outcomes",
    passes_blocked = "blocks_outcomes"
  )
}


#' Get keeper stats column mapping
#' @keywords internal
.get_keeper_col_mapping <- function() {
  c(
    shots_on_target_against = "so_ta",
    goals_against = "ga",
    saves = "saves",
    save_pct = "save_percent",
    psxg = "ps_xg",
    gk_launch_pct = "launch_percent_launched",
    gk_launch_avg_len = "avg_len_launched",
    gk_opp_passes = "opp_opp",
    gk_passes_stopped = "stp_opp",
    gk_stop_pct = "stp_percent_opp",
    gk_sweeper_actions = "att_sweeper",
    gk_sweeper_dist = "avg_dist_sweeper"
  )
}


#' Calculate per-90 rates for player stats
#'
#' Internal helper to add per-90 stats to player data frame.
#'
#' @param player_stats Data frame with aggregated player stats
#' @param mins_per_90 Numeric vector of minutes/90 for each player
#'
#' @return player_stats with per-90 columns added
#' @keywords internal
.calculate_per90_rates <- function(player_stats, mins_per_90) {
  # All counting stats to convert to per-90
  p90_cols <- c(
    "goals", "assists", "shots", "shots_on_target",
    "xg", "npxg", "xa", "sca", "gca",
    "tackles", "interceptions", "blocks", "clearances",
    "tackles_won", "tackles_def_3rd", "tackles_mid_3rd", "tackles_att_3rd",
    "blocks_shots", "blocks_pass", "errors",
    "touches", "progressive_passes", "key_passes", "final_third_passes",
    "passes_into_box", "crosses_into_box",
    "pass_long_cmp", "pass_short_cmp", "pass_med_cmp",
    "pass_tot_dist", "pass_prg_dist",
    "carries", "progressive_carries", "carries_final_3rd", "carries_into_box",
    "take_ons_att", "take_ons_succ", "miscontrols", "dispossessed",
    "prg_passes_received",
    "touches_def_3rd", "touches_mid_3rd", "touches_att_3rd", "touches_att_pen",
    "fouls_committed", "fouls_drawn", "offsides", "recoveries",
    "aerials_won", "aerials_lost", "penalties_won", "penalties_conceded",
    "through_balls", "switches", "crosses_pt", "corner_kicks", "passes_dead",
    "saves", "goals_against", "gk_sweeper_actions"
  )

  for (col in p90_cols) {
    x <- .safe_col(player_stats, col)
    p90_val <- x / mins_per_90
    p90_val[!is.finite(p90_val)] <- 0
    player_stats[[paste0(col, "_p90")]] <- p90_val
  }

  # Alias: crosses_pt_p90 -> crosses_p90 (FBref SPM and PSR coefficients expect crosses_p90)
  if ("crosses_pt_p90" %in% names(player_stats)) {
    player_stats$crosses_p90 <- player_stats$crosses_pt_p90
  }

  player_stats
}


#' Calculate derived features (ratios and success rates)
#'
#' Internal helper to add derived features to player data frame.
#'
#' @param player_stats Data frame with aggregated player stats
#' @param mins_per_90 Numeric vector of minutes/90 for each player
#'
#' @return player_stats with derived feature columns added
#' @keywords internal
.calculate_derived_features <- function(player_stats, mins_per_90) {
  sc <- function(col_name) .safe_col(player_stats, col_name)

  # Shooting efficiency (default = 0 when denominator is 0 but data exists)
  player_stats$shot_accuracy <- safe_divide(sc("shots_on_target"), sc("shots"), default = 0)
  player_stats$goals_per_shot <- safe_divide(sc("goals"), sc("shots"), default = 0)
  player_stats$xg_per_shot <- safe_divide(sc("xg"), sc("shots"), default = 0)
  player_stats$goals_minus_xg <- player_stats$goals_p90 - player_stats$xg_p90
  player_stats$npxg_plus_xa_p90 <- player_stats$npxg_p90 + player_stats$xa_p90

  # Passing efficiency
  player_stats$pass_completion <- safe_divide(sc("passes_completed"), sc("passes_attempted"), default = 0)
  player_stats$pass_short_success <- safe_divide(sc("pass_short_cmp"), sc("pass_short_att"), default = 0)
  player_stats$pass_med_success <- safe_divide(sc("pass_med_cmp"), sc("pass_med_att"), default = 0)
  player_stats$pass_long_success <- safe_divide(sc("pass_long_cmp"), sc("pass_long_att"), default = 0)
  player_stats$long_pass_ratio <- safe_divide(sc("pass_long_att"), sc("pass_att"), default = 0)

  # Take-on success
  player_stats$take_on_success <- safe_divide(sc("take_ons_succ"), sc("take_ons_att"), default = 0)

  # Tackle success
  player_stats$tackle_success <- safe_divide(sc("tackles_won"), sc("tackles"), default = 0)
  player_stats$challenge_success <- safe_divide(sc("challenges_tkl"), sc("challenges_att"), default = 0)

  # Touch location ratios (indicates where player operates on pitch)
  total_touches <- sc("touches_poss")
  total_touches <- ifelse(total_touches == 0, sc("touches"), total_touches)
  player_stats$touch_def_3rd_pct <- safe_divide(sc("touches_def_3rd"), total_touches, default = 0)
  player_stats$touch_mid_3rd_pct <- safe_divide(sc("touches_mid_3rd"), total_touches, default = 0)
  player_stats$touch_att_3rd_pct <- safe_divide(sc("touches_att_3rd"), total_touches, default = 0)
  player_stats$touch_att_pen_pct <- safe_divide(sc("touches_att_pen"), total_touches, default = 0)

  # Ball retention
  turnovers <- sc("miscontrols") + sc("dispossessed")
  player_stats$turnovers_p90 <- turnovers / mins_per_90
  total_carries <- sc("carries_poss")
  total_carries <- ifelse(total_carries == 0, sc("carries"), total_carries)
  player_stats$carry_retention <- ifelse(total_carries > 0, 1 - turnovers / total_carries, NA_real_)

  # Progressive actions per touch
  prg_actions <- sc("progressive_carries") + sc("progressive_passes")
  player_stats$prg_actions_per_touch <- safe_divide(prg_actions, total_touches, default = 0)

  # Aerial duel success
  total_aerials <- sc("aerials_won") + sc("aerials_lost")
  player_stats$aerial_success <- safe_divide(sc("aerials_won"), total_aerials, default = 0)
  player_stats$aerials_total_p90 <- total_aerials / mins_per_90

  # Foul differential (fouls drawn - committed, higher = better)
  player_stats$foul_differential_p90 <- player_stats$fouls_drawn_p90 - player_stats$fouls_committed_p90

  # Goalkeeper metrics
  player_stats$gk_save_pct <- safe_divide(sc("saves"), sc("shots_on_target_against"), default = 0)
  player_stats$gk_goals_prevented <- sc("psxg") - sc("goals_against")
  player_stats$gk_goals_prevented_p90 <- player_stats$gk_goals_prevented / mins_per_90

  player_stats
}


#' Add position dummy variables
#'
#' Internal helper to add position indicator columns.
#'
#' @param player_stats Data frame with primary_position column
#'
#' @return player_stats with position dummy columns added
#' @keywords internal
.add_position_dummies <- function(player_stats) {
  if (!"primary_position" %in% names(player_stats)) {
    return(player_stats)
  }

  pos <- player_stats$primary_position
  # Extract first position if multiple (e.g., "MF,FW" -> "MF")
  pos <- sapply(strsplit(as.character(pos), ","), `[`, 1)
  player_stats$is_gk <- as.integer(grepl("GK", pos, ignore.case = TRUE))
  player_stats$is_df <- as.integer(grepl("DF|CB|LB|RB|WB", pos, ignore.case = TRUE))
  player_stats$is_mf <- as.integer(grepl("MF|CM|DM|AM", pos, ignore.case = TRUE))
  player_stats$is_fw <- as.integer(grepl("FW|ST|CF|LW|RW", pos, ignore.case = TRUE))

  player_stats
}


# =============================================================================
# Main aggregate_player_stats() Function
# =============================================================================

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
  # Validate inputs
  validate_dataframe(stats_summary, required_cols = "player_name", arg_name = "stats_summary")

  progress_msg(sprintf("Aggregating %d player-match rows...", nrow(stats_summary)))

  # Create player_id for consistent matching (name-derived; see .aggregate_stat_table note)
  stats_summary$player_id <- clean_player_name(stats_summary$player_name)

  # Create lookup for canonical player_name using data.table
  cleaned_names_vec <- trimws(gsub("\u00A0", " ", stats_summary$player_name))
  dt_names <- data.table::data.table(
    player_id = stats_summary$player_id,
    clean_name = cleaned_names_vec
  )
  player_name_lookup <- dt_names[, {
    tbl <- table(clean_name)
    list(player_name = stringi::stri_trans_totitle(tolower(names(tbl)[which.max(tbl)])))
  }, by = player_id]
  data.table::setDF(player_name_lookup)

  # Aggregate summary stats using data.table for performance
  summary_cols <- .get_summary_col_mapping()
  existing_summary <- summary_cols[summary_cols %in% names(stats_summary)]

  dt <- data.table::as.data.table(stats_summary)
  agg_exprs <- lapply(existing_summary, function(col) {
    bquote(sum(as.numeric(.(as.name(col))), na.rm = TRUE))
  })
  names(agg_exprs) <- existing_summary
  agg_exprs$n_matches <- quote(.N)

  # Add position mode if available
  if ("pos" %in% names(stats_summary)) {
    agg_exprs$primary_position <- quote({
      valid_pos <- pos[!is.na(pos) & pos != ""]
      if (length(valid_pos) == 0) NA_character_
      else names(which.max(table(valid_pos)))
    })
  }

  player_stats <- dt[, eval(as.call(c(quote(list), agg_exprs))), by = player_id]
  player_stats <- as.data.frame(player_stats)
  player_stats <- data.table::as.data.table(player_name_lookup)[data.table::as.data.table(player_stats), on = "player_id"]
  data.table::setDF(player_stats)
  player_stats <- rename_columns(player_stats, existing_summary)

  # Filter by min minutes
  player_stats <- player_stats[player_stats$total_minutes >= min_minutes, ]
  if (nrow(player_stats) == 0) {
    cli::cli_warn("No players meet minimum minutes threshold")
    return(NULL)
  }

  mins_per_90 <- player_stats$total_minutes / 90
  player_stats$mins_per_90 <- mins_per_90

  # Aggregate additional stat tables and merge
  stat_tables <- list(
    passing = list(data = stats_passing, cols = .get_passing_col_mapping()),
    defense = list(data = stats_defense, cols = .get_defense_col_mapping()),
    possession = list(data = stats_possession, cols = .get_possession_col_mapping()),
    misc = list(data = stats_misc, cols = .get_misc_col_mapping()),
    passing_types = list(data = stats_passing_types, cols = .get_passing_types_col_mapping()),
    keeper = list(data = stats_keeper, cols = .get_keeper_col_mapping())
  )

  for (table_info in stat_tables) {
    agg_result <- .aggregate_stat_table(table_info$data, table_info$cols)
    if (!is.null(agg_result)) {
      player_stats <- data.table::as.data.table(agg_result)[data.table::as.data.table(player_stats), on = "player_id"]
      data.table::setDF(player_stats)
    }
  }

  # Calculate per-90 rates
  player_stats <- .calculate_per90_rates(player_stats, mins_per_90)

  # Calculate derived features
  player_stats <- .calculate_derived_features(player_stats, mins_per_90)

  # Add position dummies
  player_stats <- .add_position_dummies(player_stats)


  # Replace NAs with 0 for counting and per-90 columns only.
  # Rate/derived columns (pass_completion, tackle_success, etc.) keep NAs
  # so XGBoost can learn the "no data" signal rather than treating missing as 0%.
  rate_cols <- c(
    "shot_accuracy", "goals_per_shot", "xg_per_shot",
    "pass_completion", "pass_short_success", "pass_med_success",
    "pass_long_success", "long_pass_ratio",
    "take_on_success", "tackle_success", "challenge_success",
    "touch_def_3rd_pct", "touch_mid_3rd_pct", "touch_att_3rd_pct",
    "touch_att_pen_pct", "carry_retention", "prg_actions_per_touch",
    "aerial_success", "gk_save_pct"
  )
  counting_cols <- setdiff(names(player_stats)[sapply(player_stats, is.numeric)], rate_cols)
  player_stats[counting_cols] <- lapply(player_stats[counting_cols], function(x) {
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
#' @keywords internal
create_spm_prior <- function(spm_predictions, player_mapping, default_prior = 0) {
  # Handle data frame input
  if (is.data.frame(spm_predictions)) {
    if ("spm" %in% names(spm_predictions) && "player_name" %in% names(spm_predictions)) {
      spm_predictions <- stats::setNames(spm_predictions$spm, spm_predictions$player_name)
    } else {
      cli::cli_abort(c(
        "{.arg spm_predictions} data frame must have {.field spm} and {.field player_name} columns.",
        "i" = "Use {.fn calculate_spm_ratings} to generate SPM predictions."
      ))
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

  # Fill in SPM predictions where available (vectorized)
  common_names <- intersect(names(spm_predictions), names(name_to_id))
  matched_ids <- name_to_id[common_names]
  valid <- matched_ids %in% names(prior)
  if (any(valid)) {
    prior[matched_ids[valid]] <- spm_predictions[common_names[valid]]
  }
  matched <- sum(valid)

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
#' @keywords internal
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
  # Initialize prior vector for all players in mapping
  all_player_ids <- unique(player_mapping$player_id)
  prior <- stats::setNames(rep(default, length(all_player_ids)), all_player_ids)

  # Try direct player_id matching (Opta pipeline — both use same numeric ID)
  join_method <- "player_name"
  if ("player_id" %in% names(spm_data) &&
      any(as.character(spm_data$player_id) %in% as.character(all_player_ids))) {
    join_method <- "player_id"
    spm_lookup <- stats::setNames(spm_data[[spm_col]], as.character(spm_data$player_id))
    matched_ids <- intersect(names(spm_lookup), names(prior))
    prior[matched_ids] <- spm_lookup[matched_ids]
  } else {
    # Name-based matching fallback (FBref pipeline — different ID systems)
    name_to_id <- stats::setNames(
      player_mapping$player_id,
      player_mapping$player_name
    )
    matched_names <- intersect(spm_data$player_name, names(name_to_id))
    if (length(matched_names) > 0) {
      matched_ids <- name_to_id[matched_names]
      spm_idx <- match(matched_names, spm_data$player_name)
      valid <- !is.na(spm_idx) & matched_ids %in% names(prior)
      if (any(valid)) {
        prior[matched_ids[valid]] <- spm_data[[spm_col]][spm_idx[valid]]
      }
    }
  }

  n_matched <- sum(prior != default)
  progress_msg(sprintf("Prior '%s': matched %d of %d players [via %s]",
                       spm_col, n_matched, nrow(spm_data), join_method))

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
#' @keywords internal
prepare_spm_regression_data <- function(player_features, rapm_ratings) {
  # Match on player name or ID
  if ("player_id" %in% names(player_features) && "player_id" %in% names(rapm_ratings)) {
    rapm_dt <- data.table::as.data.table(rapm_ratings[, c("player_id", "rapm"), drop = FALSE])
    data <- data.table::as.data.table(player_features)[rapm_dt, on = "player_id", nomatch = NULL]
    data.table::setDF(data)
  } else if ("player_name" %in% names(player_features) && "player_name" %in% names(rapm_ratings)) {
    rapm_dt <- data.table::as.data.table(rapm_ratings[, c("player_name", "rapm"), drop = FALSE])
    data <- data.table::as.data.table(player_features)[rapm_dt, on = "player_name", nomatch = NULL]
    data.table::setDF(data)
  } else {
    cli::cli_abort(c(
      "Cannot match {.arg player_features} and {.arg rapm_ratings}.",
      "x" = "No common ID column found (expected {.field player_id} or {.field player_name})."
    ))
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
  # Validate input
  validate_dataframe(data, required_cols = "rapm", arg_name = "data")

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
    cli::cli_abort(c(
      "No valid predictor columns found in {.arg data}.",
      "i" = "Columns should end with '_p90' or '_p100'.",
      "i" = "Use {.fn aggregate_player_stats} to generate predictor columns."
    ))
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

  # Store feature SDs for standardised importance (glmnet standardize=TRUE

  # returns coefficients on original scale; multiply by SD for comparison)
  feature_sds <- apply(X, 2, stats::sd, na.rm = TRUE)
  feature_sds[feature_sds == 0 | is.na(feature_sds)] <- 1

  # Add metadata
  cv_fit$panna_metadata <- list(
    type = "spm",
    alpha = alpha,
    predictor_cols = available_cols,
    feature_sds = feature_sds,
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
#' @keywords internal
fit_spm_xgb <- function(data, predictor_cols = NULL, nfolds = 10,
                         max_depth = 4, eta = 0.1,
                         subsample = 0.8, colsample_bytree = 0.8,
                         nrounds = 500, early_stopping_rounds = 20,
                         weight_by_minutes = TRUE, weight_transform = "sqrt",
                         verbose = 1) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required. Install with: {.code install.packages('xgboost')}")
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
    cli::cli_abort(c(
      "No valid predictor columns found in {.arg data}.",
      "i" = "Columns should end with '_p90' or '_p100'.",
      "i" = "Use {.fn aggregate_player_stats} to generate predictor columns."
    ))
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

  # Ensure data.frame (data.table subsetting interprets predictor_cols as column name)
  player_features <- as.data.frame(player_features)

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- stats::predict(spm_xgb_model$model, X)

  # Create output
  keep_cols <- intersect(c("player_id", "player_name", "n_matches", "total_minutes"),
                         names(player_features))
  result <- player_features[, keep_cols, drop = FALSE]
  result$spm <- spm_pred
  result <- result[order(-result$spm), ]

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
#' @keywords internal
calculate_spm_blend <- function(player_features, model_glmnet, model_xgb,
                                weight_glmnet = 0.5) {
  # Get predictions from each model
  spm_glmnet <- calculate_spm_ratings(player_features, model_glmnet)
  spm_xgb <- calculate_spm_ratings_xgb(player_features, model_xgb)

  # Blend predictions
  names(spm_glmnet)[names(spm_glmnet) == "spm"] <- "spm_glmnet"
  xgb_df <- spm_xgb[, c("player_id", "spm"), drop = FALSE]
  names(xgb_df)[names(xgb_df) == "spm"] <- "spm_xgb"
  result <- data.table::as.data.table(spm_glmnet)[data.table::as.data.table(xgb_df), on = "player_id", nomatch = NULL]
  data.table::setDF(result)
  result$spm <- weight_glmnet * result$spm_glmnet + (1 - weight_glmnet) * result$spm_xgb
  result <- result[order(-result$spm), ]

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
#' @keywords internal
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

  # Ensure data.frame (data.table subsetting interprets predictor_cols as column name)
  player_features <- as.data.frame(player_features)

  # Prepare prediction matrix
  X <- as.matrix(player_features[, predictor_cols, drop = FALSE])

  # Handle missing values
  X[is.na(X)] <- 0

  # Predict
  spm_pred <- as.vector(stats::predict(spm_model, newx = X, s = lambda_val))

  # Create output data frame
  keep_cols <- intersect(c("player_id", "player_name", "n_games", "total_minutes"),
                         names(player_features))
  result <- player_features[, keep_cols, drop = FALSE]
  result$spm <- spm_pred
  result <- result[order(-result$spm), ]

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
#' @keywords internal
calculate_offensive_spm <- function(data, offensive_cols = NULL, alpha = 0.5) {
  if (is.null(offensive_cols)) {
    # Use _p90 naming (current), fall back to _p100 for backward compatibility
    suffix <- if (any(grepl("_p90$", names(data)))) "_p90" else "_p100"
    offensive_cols <- paste0(c("npxg", "xg", "shots", "shots_on_target",
                               "assists", "xa", "sca", "gca",
                               "progressive_passes", "progressive_carries", "carries"), suffix)
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
#' @keywords internal
calculate_defensive_spm <- function(data, defensive_cols = NULL, alpha = 0.5) {
  if (is.null(defensive_cols)) {
    suffix <- if (any(grepl("_p90$", names(data)))) "_p90" else "_p100"
    defensive_cols <- paste0(c("tackles", "interceptions", "blocks",
                               "tackles_won", "clearances"), suffix)
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
#' @keywords internal
validate_spm_prediction <- function(spm_ratings, rapm_ratings,
                                     weight_by_minutes = TRUE,
                                     weight_transform = "sqrt") {
  # Join predictions with actuals
  # Find common join columns that exist in BOTH dataframes
  possible_keys <- c("player_id", "player_name")
  join_cols <- intersect(intersect(names(spm_ratings), names(rapm_ratings)), possible_keys)

  if (length(join_cols) == 0) {
    cli::cli_warn(c(
      "No common join columns found.",
      "i" = "Expected {.field player_id} or {.field player_name} in both data frames."
    ))
    return(NULL)
  }

  rapm_keep <- c(join_cols, "rapm")
  rapm_dt <- data.table::as.data.table(rapm_ratings[, rapm_keep, drop = FALSE])
  comparison <- data.table::as.data.table(spm_ratings)[rapm_dt, on = join_cols, nomatch = NULL]
  data.table::setDF(comparison)

  if (nrow(comparison) == 0) {
    cli::cli_warn("No matching players between SPM and RAPM ratings.")
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
  rmse_unweighted <- sqrt(mean(residuals^2, na.rm = TRUE))
  mae_unweighted <- mean(abs(residuals), na.rm = TRUE)

  # Weighted metrics
  weighted_mean_rapm <- sum(weights * comparison$rapm, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  ss_res_w <- sum(weights * residuals^2, na.rm = TRUE)
  ss_tot_w <- sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE)
  rmse_weighted <- sqrt(sum(weights * residuals^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  mae_weighted <- sum(weights * abs(residuals), na.rm = TRUE) / sum(weights, na.rm = TRUE)

  # Weighted correlation (handle zero variance edge case)
  cov_w <- sum(weights * (comparison$spm - mean(comparison$spm)) *
               (comparison$rapm - weighted_mean_rapm), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  sd_spm_w <- sqrt(sum(weights * (comparison$spm - mean(comparison$spm))^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  sd_rapm_w <- sqrt(sum(weights * (comparison$rapm - weighted_mean_rapm)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  cor_weighted <- if (sd_spm_w > 0 && sd_rapm_w > 0) cov_w / (sd_spm_w * sd_rapm_w) else NA_real_

  # Unweighted correlation (handle zero variance edge case)
  sd_spm <- stats::sd(comparison$spm, na.rm = TRUE)
  sd_rapm <- stats::sd(comparison$rapm, na.rm = TRUE)
  cor_unweighted <- if (sd_spm > 0 && sd_rapm > 0) {
    stats::cor(comparison$spm, comparison$rapm, use = "complete.obs")
  } else {
    NA_real_
  }

  # Handle edge case where R-squared calculation has zero total variance
  r_squared_unweighted <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  r_squared_weighted <- if (ss_tot_w > 0) 1 - ss_res_w / ss_tot_w else NA_real_

  metrics <- list(
    n_players = nrow(comparison),
    # Unweighted
    r_squared = r_squared_unweighted,
    rmse = rmse_unweighted,
    mae = mae_unweighted,
    correlation = cor_unweighted,
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

  # Get feature SDs for standardised importance
  feature_sds <- model$panna_metadata$feature_sds

  importance <- data.frame(
    feature = names(coefs),
    coefficient = as.vector(coefs),
    abs_coef = abs(as.vector(coefs)),
    stringsAsFactors = FALSE
  )

  # Standardised importance: |beta * sd| = effect of 1-SD change
  if (!is.null(feature_sds)) {
    sd_vals <- feature_sds[importance$feature]
    sd_vals[is.na(sd_vals)] <- 1
    importance$sd <- as.numeric(sd_vals)
    importance$std_importance <- abs(importance$coefficient) * importance$sd
  } else {
    importance$std_importance <- importance$abs_coef
  }

  importance <- importance[importance$coefficient != 0, ]
  importance <- importance[order(-importance$std_importance), ]
  importance <- head(importance, n)

  importance
}
