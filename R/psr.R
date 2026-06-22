# Player Skill Rating (PSR)
# =========================
# Predict match xG/goal differential from team-aggregated player skills via
# glmnet, then apportion coefficients back to individual players.
# PSR = "predicted xG differential contribution" above league average.
#
# Adapted from the torpverse PSR methodology (AFL) for football (soccer).


# ============================================================================
# PSR skill feature columns
# ============================================================================

#' Get PSR skill feature column names
#'
#' Returns the per-90 rate and efficiency columns used as PSR features.
#' This matches the feature set used in \code{fit_spm_opta()} minus position
#' dummies -- the elastic net selects relevant features automatically.
#'
#' @return Character vector of column names
#' @keywords internal
.get_psr_skill_cols <- function() {

  # Per-90 rate columns -- outfield player actions only
  # GK/team-level stats (saves, attempts_conceded, etc.) moved to .get_gk_skill_cols()
  rate_cols <- c(
    "goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
    "shots_obox_p90", "big_chance_scored_p90", "big_chance_missed_p90",
    "assists_p90", "big_chance_created_p90", "total_att_assist_p90",
    "key_passes_p90", "through_balls_p90",
    "passes_p90", "passes_accurate_p90", "final_third_passes_p90",
    "long_balls_p90", "crosses_p90", "forward_pass_p90",
    "tackles_p90", "tackles_won_p90", "interceptions_p90",
    "interceptions_won_p90", "clearances_p90", "clearances_effective_p90",
    "blocks_p90", "blocked_passes_p90",
    "duel_won_p90", "duel_lost_p90", "aerial_won_p90", "aerial_lost_p90",
    "touches_p90", "touches_opp_box_p90",
    "poss_won_def3rd_p90", "poss_won_mid3rd_p90", "poss_won_att3rd_p90",
    "ball_recovery_p90", "dispossessed_p90", "turnover_p90",
    "times_tackled_p90",
    "corners_taken_p90", "corners_won_p90",
    "pen_area_entries_p90", "final_third_entries_p90",
    "fouls_p90", "was_fouled_p90",
    "fwd_zone_pass_p90", "open_play_pass_p90",
    "error_lead_to_shot_p90", "error_lead_to_goal_p90",
    "att_fastbreak_p90", "shot_fastbreak_p90",
    "att_openplay_p90", "att_setpiece_p90", "att_headed_p90",
    "att_one_on_one_p90", "crosses_open_play_p90",
    "penalty_won_p90", "penalty_conceded_p90",
    "offtarget_att_assist_p90",
    "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
    "unsuccessful_touch_p90", "overrun_p90", "flick_on_p90"
  )

  # Efficiency/proportion columns.
  # NOTE: the low-volume *finishing* ratios (goals_per_shot, big_chance_conversion,
  # headed_goal_rate, ibox_goal_rate, obox_goal_rate, penalty_conversion) were
  # REMOVED — a per-game ratio is scale-free (1/1 == 10/10) and discards volume.
  # They are replaced by the xG over-performance counts in xmetrics_cols below
  # (npg_minus_npxg_per90 etc.). High-volume accuracy ratios stay (large
  # denominators → the scale defect doesn't bite; glmnet shrinks any redundancy).
  efficiency_cols <- c(
    "shot_accuracy", "pass_accuracy",
    "tackle_success", "duel_success", "aerial_success",
    "final_third_pass_acc",
    "long_ball_accuracy", "cross_accuracy",
    "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
    "crosses_open_play_accuracy", "bad_touch_rate",
    "errors_total_p90",
    "flick_on_accuracy",
    "back_zone_pass_accuracy", "chipped_pass_accuracy",
    "long_pass_own_to_opp_accuracy",
    "fifty_fifty_success", "poss_lost_ctrl_per_touch"
  )

  # xMetrics columns (if available). Finishing over-performance (goals above xG,
  # volume-correct & additive) replaces the removed finishing ratios.
  xmetrics_cols <- c(
    "xg_per90", "npxg_per90", "xa_per90_xmetrics",
    "xpass_overperformance_per90_xmetrics",
    "npg_minus_npxg_per90", "ibox_g_minus_xg_per90", "obox_g_minus_xg_per90",
    "placement_added_per90"
  )

  c(rate_cols, efficiency_cols, xmetrics_cols)
}


#' Get GK-specific PSR skill feature column names
#'
#' Returns the per-90 rate and efficiency columns used for the GK sub-model.
#' GK model uses goal differential (not xG diff) as target, so GK action stats
#' like save percentage can have meaningful signal.
#'
#' @return Character vector of column names
#' @keywords internal
.get_gk_skill_cols <- function() {

  # GK action stats -- things the keeper actually does.
  # gsaa_per90 = shot-stopping above expected (expected goals faced - goals
  # conceded, per 90); a volume-correct value that replaces save_percentage.
  gk_action_cols <- c(
    "saves_p90", "saves_ibox_p90", "saves_obox_p90",
    "keeper_sweeper_p90", "gk_smother_p90",
    "high_claim_p90", "good_high_claim_p90",
    "punches_p90", "keeper_throws_p90", "keeper_pickup_p90",
    "gsaa_per90"
  )

  # GK efficiency stats (save_percentage removed -> replaced by gsaa_per90 above)
  gk_efficiency_cols <- c(
    "keeper_sweeper_accuracy",
    "keeper_throws_accuracy"
  )

  # Distribution / passing -- GKs contribute here meaningfully
  distribution_cols <- c(
    "passes_p90", "passes_accurate_p90", "pass_accuracy",
    "long_balls_p90", "long_ball_accuracy",
    "long_pass_own_to_opp_accuracy",
    "goals_conceded_p90"
  )

  # Shared outfield stats that GKs also accumulate
  shared_cols <- c(
    "clearances_p90", "aerial_won_p90", "aerial_lost_p90",
    "aerial_success", "touches_p90",
    "error_lead_to_shot_p90", "error_lead_to_goal_p90"
  )

  c(gk_action_cols, gk_efficiency_cols, distribution_cols, shared_cols)
}


# ============================================================================
# Batch pre-match skill estimation
# ============================================================================

#' Estimate pre-match skills at multiple dates (incremental batch version)
#'
#' Highly optimized for sequential date estimation. Instead of re-processing
#' all historical data at each date, maintains running cumulative sums that
#' are decayed forward and incrementally updated with new observations.
#' Uses \code{rowsum()} (C-level) for grouped matrix sums.
#'
#' Complexity: O(N + D * new_rows_per_date) instead of O(N * D).
#' For typical data (~1M rows, 659 dates), this is ~100-300x faster.
#'
#' @param match_stats Match-level stats (output of
#'   \code{compute_match_level_opta_stats}).
#' @param ref_dates Character or Date vector of dates to estimate skills at.
#' @param decay_params Decay parameters (default: \code{get_default_decay_params()}).
#' @param min_weighted_90s Minimum weighted 90s for inclusion (default 3).
#' @param verbose Print progress (default TRUE).
#'
#' @return Named list of data.tables (one per ref_date), keyed by date string.
#'   Each table has one row per player with skill columns.
#'
#' Canonical stat-column detector — ONE source of truth
#'
#' Returns the modelled stat columns present in \code{dt}: per-90 rates
#' (\code{_p90} AND \code{_per90} — the xMetrics over-performance rates), the
#' efficiency/ratio stats from \code{.classify_skill_stats()}, and the
#' registered PSR/GK skill-col lists (catches \code{_xmetrics}-suffixed cols).
#'
#' Why this exists: the same `grep("_p90$")` detection was duplicated across
#' \code{.estimate_prematch_skills_batch} (psr.R), \code{estimate_player_skills},
#' \code{compute_position_multipliers}, and \code{adjust_match_stats_for_context}
#' (estimated_skills.R). The `_p90$`-only pattern silently dropped EVERY
#' `_per90` xMetrics column — and fixing one copy left the others broken (the
#' train/serve skew we hit twice). All detectors now route through here so a new
#' feature can't be dropped by one divergent copy.
#'
#' @param dt data.frame/data.table of match stats.
#' @return Character vector of stat column names present in \code{dt}.
#' @keywords internal
.detect_skill_stat_cols <- function(dt) {
  nm <- names(dt)
  p90 <- grep("_p90$|_per90$", nm, value = TRUE)
  eff <- intersect(names(.classify_skill_stats()), nm)
  registered <- intersect(c(.get_psr_skill_cols(), .get_gk_skill_cols()), nm)
  unique(intersect(c(p90, eff, registered), nm))
}

#' @keywords internal
.estimate_prematch_skills_batch <- function(match_stats, ref_dates,
                                            decay_params = NULL,
                                            min_weighted_90s = 3,
                                            verbose = TRUE) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()

  ref_dates <- sort(unique(as.Date(ref_dates)))
  n_dates <- length(ref_dates)
  results <- vector("list", n_dates)
  names(results) <- as.character(ref_dates)

  if (verbose) {
    progress_msg(sprintf("Estimating pre-match skills at %d dates (%s to %s)",
                          n_dates, ref_dates[1], ref_dates[n_dates]))
  }

  # === PRE-COMPUTE SHARED STATE ===

  dt <- data.table::as.data.table(match_stats)
  if (!inherits(dt$match_date, "Date")) dt[, match_date := as.Date(match_date)]
  data.table::setkey(dt, match_date)

  # Resolve positions once
  dt <- .resolve_positions(dt)
  player_pos <- dt[!is.na(pos_group), {
    tt <- table(pos_group)
    list(pos_group = names(tt)[which.max(tt)])
  }, by = player_id]
  player_names <- dt[, .(player_name = player_name[1]), by = player_id]

  # Auto-detect stat columns via the ONE shared detector (catches _p90 AND
  # _per90 xMetrics rates + registered skill cols). See .detect_skill_stat_cols.
  eff_map <- .classify_skill_stats()
  stat_cols <- .detect_skill_stat_cols(dt)
  eff_cols <- intersect(names(eff_map), names(dt))  # subset needing denominators

  if (length(stat_cols) == 0) {
    cli::cli_warn("No stat columns found in match_stats.")
    return(list())
  }

  # Resolve lambda per stat
  stat_lambdas <- vapply(stat_cols, function(sc) {
    .resolve_lambda(sc, decay_params, eff_map)
  }, numeric(1))

  # Position multipliers & grand means -- compute once
  pos_multipliers <- if (!is.null(decay_params$position_multipliers)) {
    decay_params$position_multipliers
  } else {
    compute_position_multipliers(dt, stat_cols)
  }

  grand_means <- numeric(length(stat_cols))
  names(grand_means) <- stat_cols
  prior_centers_cached <- decay_params$prior_centers
  wts_all <- data.table::fifelse(is.na(dt$total_minutes), 0,
                                  as.numeric(dt$total_minutes))
  total_wt <- sum(wts_all)
  for (sc in stat_cols) {
    if (!is.null(prior_centers_cached) && sc %in% names(prior_centers_cached)) {
      grand_means[sc] <- prior_centers_cached[sc]
    } else if (sc %in% names(dt)) {
      vals <- as.numeric(dt[[sc]])
      vals[is.na(vals)] <- 0
      if (total_wt > 0) grand_means[sc] <- sum(vals * wts_all) / total_wt
    }
  }

  # Pre-compute minutes/90
  dt[, .mins_90 := data.table::fifelse(is.na(total_minutes), 0,
                                        as.numeric(total_minutes) / 90)]

  # Pre-convert stat values to numeric, NA -> 0
  for (sc in stat_cols) {
    if (!is.numeric(dt[[sc]])) {
      data.table::set(dt, j = sc, value = as.numeric(dt[[sc]]))
    }
    na_idx <- which(is.na(dt[[sc]]))
    if (length(na_idx) > 0) data.table::set(dt, i = na_idx, j = sc, value = 0)
  }

  # Pre-compute denominators for efficiency stats (skip missing)
  eff_denoms <- list()
  skip_stats <- character(0)
  for (sc in eff_cols) {
    if (sc %in% names(eff_map)) {
      d <- .compute_denominator(dt, eff_map[[sc]])
      if (is.null(d)) { skip_stats <- c(skip_stats, sc); next }
      eff_denoms[[sc]] <- d
    }
  }
  if (length(skip_stats) > 0) {
    stat_cols <- setdiff(stat_cols, skip_stats)
    stat_lambdas <- stat_lambdas[stat_cols]
    if (verbose) {
      progress_msg(sprintf("  Skipped %d stats (missing denominators): %s",
                            length(skip_stats), paste(skip_stats, collapse = ", ")))
    }
  }

  # Classify stats
  rate_stats <- setdiff(stat_cols, names(eff_map))
  eff_stats <- intersect(stat_cols, names(eff_map))
  unique_lambdas <- unique(stat_lambdas)
  rate_lam <- decay_params$rate

  # Prior strengths per stat
  stat_prior_strengths <- vapply(stat_cols, function(sc) {
    .resolve_prior_strength(sc, decay_params, sc %in% names(eff_map))
  }, numeric(1))

  # Build player index: integer mapping for rowsum()
  all_player_ids <- sort(unique(dt$player_id))
  n_players <- length(all_player_ids)
  pid_int <- match(dt$player_id, all_player_ids)  # integer index per row

  # Pre-compute position-specific prior centers per player (constant across dates)
  player_pg <- player_pos$pos_group[match(all_player_ids, player_pos$player_id)]

  # Rate stat priors: alpha0 matrix (n_players x n_rate_stats)
  rate_alpha0 <- matrix(0, nrow = n_players, ncol = length(rate_stats))
  colnames(rate_alpha0) <- rate_stats
  for (ci in seq_along(rate_stats)) {
    sc <- rate_stats[ci]
    ps <- stat_prior_strengths[sc]
    gm <- grand_means[sc]
    pm_lookup <- if (sc %in% names(pos_multipliers)) pos_multipliers[[sc]] else NULL
    a0 <- rep(gm, n_players)
    if (!is.null(pm_lookup)) {
      for (pg in names(pm_lookup)) a0[player_pg == pg] <- gm * pm_lookup[pg]
    }
    rate_alpha0[, ci] <- pmax(a0 * ps, 1e-4)
  }

  # Efficiency stat priors: mu0 per stat (n_players vector each)
  eff_mu0 <- list()
  for (sc in eff_stats) {
    gm <- grand_means[sc]
    pm_lookup <- if (sc %in% names(pos_multipliers)) pos_multipliers[[sc]] else NULL
    m0 <- rep(gm, n_players)
    if (!is.null(pm_lookup)) {
      for (pg in names(pm_lookup)) m0[player_pg == pg] <- gm * pm_lookup[pg]
    }
    eff_mu0[[sc]] <- pmax(pmin(m0, 1 - 1e-6), 1e-6)
  }

  # Player name/position lookup aligned to all_player_ids
  pname_lookup <- player_names$player_name[match(all_player_ids, player_names$player_id)]
  ppos_lookup <- player_pg

  if (verbose) {
    progress_msg(sprintf("  Pre-computed: %d stats (%d rate + %d eff), %d lambdas, %d players",
                          length(stat_cols), length(rate_stats), length(eff_stats),
                          length(unique_lambdas), n_players))
  }

  # === INCREMENTAL CUMULATIVE APPROACH ===
  #
  # Key insight: for exponential decay weights,
  #   w_num_D' = exp(-lam * delta) * w_num_D + sum_{new rows} contribution
  # So we maintain running sums and only add new data at each step.

  # Group rate stats by lambda for shared running sums
  rate_by_lambda <- split(rate_stats, stat_lambdas[rate_stats])
  eff_by_lambda <- split(eff_stats, stat_lambdas[eff_stats])

  # Initialize running sum accumulators per lambda group
  # For rate stats: w_num (n_players x n_cols), w_den (n_players x 1)
  # For eff stats: w_num and w_den per stat (n_players x 1 each)

  run_rate <- list()
  for (lam_key in names(rate_by_lambda)) {
    cols <- rate_by_lambda[[lam_key]]
    run_rate[[lam_key]] <- list(
      w_num = matrix(0, nrow = n_players, ncol = length(cols),
                     dimnames = list(NULL, cols)),
      w_den = numeric(n_players)  # shared denominator for rate stats
    )
  }

  run_eff <- list()
  for (sc in eff_stats) {
    run_eff[[sc]] <- list(w_num = numeric(n_players), w_den = numeric(n_players))
  }

  # Running weighted 90s (uses rate lambda)
  run_w90 <- numeric(n_players)

  # Track cursor: which rows have been incorporated
  prev_date <- ref_dates[1]  # will be adjusted
  cursor <- 0L  # index into dt -- rows 1:cursor have been processed

  # Extract vectors we'll index repeatedly (avoid repeated dt[[]] access)
  dt_dates <- dt$match_date
  dt_mins90 <- dt$.mins_90
  dt_pid_int <- pid_int

  # Pre-extract stat value vectors
  dt_stat_vals <- list()
  for (sc in stat_cols) dt_stat_vals[[sc]] <- dt[[sc]]

  # Pre-extract efficiency denominators (already computed)
  # eff_denoms[[sc]] is a full-length vector aligned with dt rows

  # Pre-compute cursor positions for all ref_dates (vectorized binary search)
  # cursor_positions[i] = number of rows in dt with match_date < ref_dates[i]
  dt_dates_num <- as.numeric(dt_dates)
  ref_dates_num <- as.numeric(ref_dates)
  # findInterval returns last index where dt_dates_num <= x
  # We want strict < so subtract a small epsilon (dates are integers, 0.5 works)
  cursor_positions <- findInterval(ref_dates_num - 0.5, dt_dates_num)

  cursor <- 0L

  for (i in seq_along(ref_dates)) {
    rd <- ref_dates[i]

    if (verbose && (i %% 50 == 0 || i == 1 || i == n_dates)) {
      progress_msg(sprintf("  Date %d/%d: %s", i, n_dates, rd))
    }

    results[[i]] <- tryCatch({
      new_cursor <- cursor_positions[i]

      # Skip dates with no prior data (use NULL, not return() -- we're in tryCatch)
      if (new_cursor == 0) {
        NULL
      } else {

      new_rows <- if (new_cursor > cursor) (cursor + 1L):new_cursor else integer(0)
      has_new <- length(new_rows) > 0

      # Days between this ref_date and the previous one (for decay of existing sums)
      if (i == 1) {
        delta_days <- 0  # no previous sums to decay
      } else {
        delta_days <- as.numeric(rd - ref_dates[i - 1L])
      }

      # --- Update running sums ---
      # Step 1: Decay existing sums by exp(-lam * delta_days)
      # Step 2: Add contributions from new rows

      # Rate stats (grouped by lambda)
      for (lam_key in names(rate_by_lambda)) {
        lam <- as.numeric(lam_key)
        cols <- rate_by_lambda[[lam_key]]
        rs <- run_rate[[lam_key]]

        # Decay existing
        if (i > 1 && delta_days > 0) {
          decay_factor <- exp(-lam * delta_days)
          rs$w_num <- rs$w_num * decay_factor
          rs$w_den <- rs$w_den * decay_factor
        }

        # Add new rows
        if (has_new) {
          new_pid <- dt_pid_int[new_rows]
          # Days from each new match to THIS ref_date
          new_days <- as.numeric(rd - dt_dates[new_rows])
          new_w <- exp(-lam * new_days)
          new_w_mins <- new_w * dt_mins90[new_rows]

          # Denominator: rowsum of w * mins_90 by player
          den_contrib <- rowsum(new_w_mins, new_pid, reorder = FALSE)
          den_players <- as.integer(rownames(den_contrib))
          rs$w_den[den_players] <- rs$w_den[den_players] + den_contrib[, 1]

          # Numerator: rowsum of stat_mat * w_mins by player (all cols at once)
          stat_mat <- matrix(0, nrow = length(new_rows), ncol = length(cols))
          for (ci in seq_along(cols)) {
            stat_mat[, ci] <- dt_stat_vals[[cols[ci]]][new_rows]
          }
          num_contrib <- rowsum(stat_mat * new_w_mins, new_pid, reorder = FALSE)
          rs$w_num[den_players, ] <- rs$w_num[den_players, ] + num_contrib
        }

        run_rate[[lam_key]] <- rs
      }

      # Efficiency stats
      for (sc in eff_stats) {
        lam <- stat_lambdas[sc]
        rs <- run_eff[[sc]]

        if (i > 1 && delta_days > 0) {
          decay_factor <- exp(-lam * delta_days)
          rs$w_num <- rs$w_num * decay_factor
          rs$w_den <- rs$w_den * decay_factor
        }

        if (has_new) {
          new_pid <- dt_pid_int[new_rows]
          new_days <- as.numeric(rd - dt_dates[new_rows])
          new_w <- exp(-lam * new_days)
          new_denom <- eff_denoms[[sc]][new_rows]

          num_v <- new_w * dt_stat_vals[[sc]][new_rows] * new_denom
          den_v <- new_w * new_denom

          num_contrib <- rowsum(num_v, new_pid, reorder = FALSE)
          den_contrib <- rowsum(den_v, new_pid, reorder = FALSE)
          rp <- as.integer(rownames(num_contrib))
          rs$w_num[rp] <- rs$w_num[rp] + num_contrib[, 1]
          rs$w_den[rp] <- rs$w_den[rp] + den_contrib[, 1]
        }

        run_eff[[sc]] <- rs
      }

      # Weighted 90s (rate lambda)
      if (i > 1 && delta_days > 0) {
        run_w90 <- run_w90 * exp(-rate_lam * delta_days)
      }
      if (has_new) {
        new_pid <- dt_pid_int[new_rows]
        new_days <- as.numeric(rd - dt_dates[new_rows])
        w90_v <- exp(-rate_lam * new_days) * dt_mins90[new_rows]
        w90_contrib <- rowsum(w90_v, new_pid, reorder = FALSE)
        rp <- as.integer(rownames(w90_contrib))
        run_w90[rp] <- run_w90[rp] + w90_contrib[, 1]
      }

      cursor <- new_cursor

      # --- Compute posteriors from running sums ---

      # Rate stats
      rate_skill_mat <- matrix(0, nrow = n_players, ncol = length(rate_stats))
      colnames(rate_skill_mat) <- rate_stats
      col_offset <- 0
      for (lam_key in names(rate_by_lambda)) {
        cols <- rate_by_lambda[[lam_key]]
        rs <- run_rate[[lam_key]]
        for (ci in seq_along(cols)) {
          sc <- cols[ci]
          ps <- stat_prior_strengths[sc]
          si <- match(sc, rate_stats)
          rate_skill_mat[, si] <- (rate_alpha0[, si] + rs$w_num[, ci]) /
                                   (ps + rs$w_den)
        }
      }

      # Efficiency stats
      eff_skill_vals <- list()
      for (sc in eff_stats) {
        ps <- stat_prior_strengths[sc]
        rs <- run_eff[[sc]]
        eff_skill_vals[[sc]] <- (eff_mu0[[sc]] * ps + rs$w_num) / (ps + rs$w_den)
      }

      # --- Build result data.table ---
      result <- data.table::data.table(
        player_id = all_player_ids,
        player_name = pname_lookup,
        primary_position = ppos_lookup,
        date = rd,
        weighted_90s = run_w90
      )

      for (ci in seq_along(rate_stats)) {
        data.table::set(result, j = rate_stats[ci], value = rate_skill_mat[, ci])
      }
      for (sc in eff_stats) {
        data.table::set(result, j = sc, value = eff_skill_vals[[sc]])
      }

      result
      }  # end else (has prior data)
    },
    error = function(e) {
      if (verbose) cat(sprintf("  ERROR at %s: %s\n", rd, e$message))
      NULL
    })
  }

  # Drop NULLs
  results[!vapply(results, is.null, logical(1))]
}


# ============================================================================
# Core PSR calculation
# ============================================================================

#' Calculate Player Skill Ratings (PSR)
#'
#' Computes PSR for each player by applying pre-trained glmnet coefficients to
#' individual player skill values. PSR represents each player's predicted
#' contribution to xG/goal differential based on their skill profile.
#'
#' @param skills A data.table/data.frame with player skill estimates,
#'   containing identity columns (\code{player_id}, \code{player_name}) and
#'   numeric skill columns matching the \code{stat_name} values in \code{coef_df}.
#' @param coef_df A data.frame with columns \code{stat_name} and \code{beta}.
#'   If an \code{sd} column is present, each skill is divided by its SD before
#'   multiplying by beta (i.e. the coefficients are on the standardized scale).
#' @param center Logical. If TRUE (default), subtract the league mean so
#'   PSR = contribution above average player.
#'
#' @return A data.table with identity columns plus \code{psr_raw} and \code{psr}.
#'
#' @export
calculate_psr <- function(skills, coef_df, center = TRUE) {
  dt <- data.table::as.data.table(skills)

  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  # Filter to non-zero coefficients
  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    cli::cli_warn("All coefficients are zero -- PSR will be zero for all players")
    dt[, c("psr_raw", "psr") := 0]
    id_cols <- intersect(
      c("player_id", "player_name", "season_end_year", "primary_position",
        "weighted_90s", "total_minutes", "competition"),
      names(dt)
    )
    return(dt[, c(id_cols, "psr_raw", "psr"), with = FALSE])
  }

  # Match stat_name to columns in data
  available <- coef_df$stat_name %in% names(dt)

  if (sum(available) == 0) {
    cli::cli_abort("No matching skill columns found in data")
  }
  if (any(!available)) {
    missing <- coef_df$stat_name[!available]
    cli::cli_warn("Skill columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  skill_cols <- coef_df$stat_name
  betas <- coef_df$beta

  # Build skill matrix
  mat <- as.matrix(dt[, skill_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # Standardize if SD provided
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psr_raw := as.numeric(mat %*% betas)]

  if (center) {
    league_mean <- mean(dt$psr_raw, na.rm = TRUE)
    dt[, psr := psr_raw - league_mean]
  } else {
    dt[, psr := psr_raw]
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season_end_year", "primary_position",
      "weighted_90s", "total_minutes", "competition", "n_matches"),
    names(dt)
  )

  dt[, c(id_cols, "psr_raw", "psr"), with = FALSE]
}


#' Calculate PSR with Offensive/Defensive Decomposition
#'
#' Computes the margin-based PSR (best single predictor of match outcomes),
#' then decomposes it into offensive (OSR) and defensive (DSR) components using
#' separately trained coefficient models. The decomposition uses an additive
#' shift so that \code{osr + dsr = psr} exactly.
#'
#' @inheritParams calculate_psr
#' @param osr_coef_df Coefficient data.frame for the offensive model (same
#'   format as \code{coef_df}: columns \code{stat_name}, \code{beta},
#'   optionally \code{sd}).
#' @param dsr_coef_df Coefficient data.frame for the defensive model.
#'
#' @return A data.table with columns: identity columns, \code{psr_raw},
#'   \code{psr}, \code{osr}, \code{dsr}.
#'
#' @keywords internal
calculate_psr_components <- function(skills, coef_df, osr_coef_df, dsr_coef_df,
                                     center = TRUE) {
  # Margin PSR (the authoritative total)
  psr_result <- calculate_psr(skills, coef_df, center = center)

  # Raw offensive and defensive scores
  osr_result <- calculate_psr(skills, osr_coef_df, center = center)
  dsr_result <- calculate_psr(skills, dsr_coef_df, center = center)

  # Additive shift: distribute residual evenly so osr + dsr = psr
  raw_osr <- osr_result$psr
  raw_dsr <- dsr_result$psr
  delta <- (psr_result$psr - raw_osr - raw_dsr) / 2

  psr_result[, osr := raw_osr + delta]
  psr_result[, dsr := raw_dsr + delta]

  psr_result
}


# ============================================================================
# Per-Game Player Stat Value (PSV/OSV/DSV)
# ============================================================================

#' Calculate Per-Game Player Stat Value
#'
#' Applies pre-trained glmnet coefficients to raw per-game box-score stats,
#' producing a single-game "stat contribution" value. This is the per-game
#' analogue of \code{\link{calculate_psr}}, which operates on smoothed skill
#' ratings.
#'
#' Stats are minutes-adjusted (divided by \code{minutes_played / 90}) to get
#' per-90 rates, then optionally standardized using training SDs from the
#' coefficient file. Efficiency stats (ratios) are excluded from PSV by default.
#'
#' @param player_match_stats Data.frame/data.table with one row per player per
#'   match. Must contain raw stat columns matching \code{coef_df$stat_name},
#'   plus optionally \code{minutes_played} (or \code{total_minutes}).
#' @param coef_df Coefficient data.frame with columns \code{stat_name},
#'   \code{beta}, and optionally \code{sd}.
#' @param min_adjust Logical. Divide raw counts by \code{minutes_played / 90}
#'   to get per-90 rates before applying coefficients. Default \code{TRUE}.
#' @param center Logical. Center PSV within each matchday/round so
#'   PSV = contribution above average that round. Default \code{TRUE}.
#' @param exclude_efficiency Logical. Exclude efficiency/ratio stats from PSV
#'   calculation. Default \code{TRUE}.
#' @param scale_to_minutes Logical. If TRUE, multiply the (per-90) PSV by
#'   \code{minutes_played / 90} so the result is additive over a player's
#'   games (like EPV), rather than a per-90 rate. Default \code{FALSE}
#'   (per-90, the form consumed by the multi-target RAPM and skills pipeline).
#'
#' @return A data.table with identifier columns plus \code{psv_raw} and
#'   \code{psv}.
#'
#' @export
calculate_psv <- function(player_match_stats, coef_df, min_adjust = TRUE,
                           center = TRUE, exclude_efficiency = TRUE,
                           scale_to_minutes = FALSE) {
  dt <- data.table::as.data.table(player_match_stats)

  if (!all(c("stat_name", "beta") %in% names(coef_df))) {
    cli::cli_abort("{.arg coef_df} must have columns {.val stat_name} and {.val beta}")
  }

  coef_df <- coef_df[coef_df$beta != 0, , drop = FALSE]

  if (nrow(coef_df) == 0) {
    dt[, c("psv_raw", "psv") := 0]
    id_cols <- intersect(
      c("player_id", "player_name", "season", "round", "match_id",
        "team_name", "match_date", "minutes_played", "total_minutes"),
      names(dt)
    )
    return(dt[, c(id_cols, "psv_raw", "psv"), with = FALSE])
  }

  # Exclude efficiency stats from PSV (they're ratios, not additive counts)
  if (exclude_efficiency) {
    eff_stats <- .get_psr_efficiency_cols()
    keep <- !coef_df$stat_name %in% eff_stats
    coef_df <- coef_df[keep, , drop = FALSE]
  }

  stat_cols <- coef_df$stat_name
  available <- stat_cols %in% names(dt)

  if (sum(available) == 0) {
    cli::cli_abort("No matching stat columns found in data for PSV calculation")
  }
  if (any(!available)) {
    missing <- stat_cols[!available]
    cli::cli_warn("Stat columns not found (skipped): {paste(missing, collapse = ', ')}")
  }

  coef_df <- coef_df[available, , drop = FALSE]
  stat_cols <- stat_cols[available]
  betas <- coef_df$beta

  # Extract raw stat values
  mat <- as.matrix(dt[, stat_cols, with = FALSE])
  mat[is.na(mat)] <- 0

  # Minutes-adjust: divide counts by minutes/90 to get per-90 rates.
  # Efficiency/ratio stats (e.g. ibox_goal_rate, pass_accuracy) are already
  # rates, not additive counts — dividing them by minutes/90 is meaningless,
  # so exempt them from the per-90 scaling. (Matters only when efficiency stats
  # are kept, i.e. exclude_efficiency = FALSE; otherwise they aren't in `mat`.)
  if (min_adjust) {
    mins_col <- if ("minutes_played" %in% names(dt)) "minutes_played"
                else if ("total_minutes" %in% names(dt)) "total_minutes"
                else NULL
    if (!is.null(mins_col)) {
      mins <- as.numeric(dt[[mins_col]])
      mins[is.na(mins) | mins <= 0] <- 90  # default to 90 for missing
      ratio_mask <- stat_cols %in% .get_psr_efficiency_cols()
      if (any(!ratio_mask)) {
        mat[, !ratio_mask] <- mat[, !ratio_mask, drop = FALSE] / (mins / 90)
      }
    }
  }

  # Standardize using SDs from coefficient file (same scale as PSR training)
  if ("sd" %in% names(coef_df)) {
    sds <- coef_df$sd
    sds[sds == 0 | is.na(sds)] <- 1
    mat <- sweep(mat, 2, sds, "/")
  }

  dt[, psv_raw := as.numeric(mat %*% betas)]

  if (center) {
    group_cols <- intersect(c("season", "round"), names(dt))
    if (length(group_cols) > 0) {
      dt[, psv := psv_raw - mean(psv_raw, na.rm = TRUE), by = group_cols]
    } else {
      dt[, psv := psv_raw - mean(psv_raw, na.rm = TRUE)]
    }
  } else {
    dt[, psv := psv_raw]
  }

  # Scale per-90 value to the player's actual minutes so PSV is additive
  # (like EPV): a 90-min game keeps its per-90 value, a cameo gets a fraction.
  # Summing across games is then meaningful; consumers can divide by
  # total_minutes/90 to recover the per-90 rate. Preserves osv + dsv = psv
  # (both components scaled by the same per-row factor).
  if (scale_to_minutes) {
    mins_col <- if ("minutes_played" %in% names(dt)) "minutes_played"
                else if ("total_minutes" %in% names(dt)) "total_minutes"
                else NULL
    if (is.null(mins_col)) {
      cli::cli_warn(
        "scale_to_minutes = TRUE but no minutes column found; PSV left per-90"
      )
    } else {
      scale_fac <- as.numeric(dt[[mins_col]]) / 90
      scale_fac[is.na(scale_fac) | scale_fac < 0] <- 0
      dt[, psv_raw := psv_raw * scale_fac]
      dt[, psv := psv * scale_fac]
    }
  }

  id_cols <- intersect(
    c("player_id", "player_name", "season", "round", "match_id",
      "team_name", "match_date", "minutes_played", "total_minutes",
      "position", "primary_position"),
    names(dt)
  )

  dt[, c(id_cols, "psv_raw", "psv"), with = FALSE]
}


#' Calculate PSV with Offensive/Defensive Decomposition
#'
#' Applies offensive and defensive coefficient models to per-game stats,
#' producing \code{psv}, \code{osv}, and \code{dsv} columns where
#' \code{osv + dsv = psv} exactly (via additive reconciliation).
#'
#' @inheritParams calculate_psv
#' @param osr_coef_df Coefficient data.frame for the offensive model
#'   (predicting goals scored / xG for).
#' @param dsr_coef_df Coefficient data.frame for the defensive model
#'   (predicting goals conceded / xG against).
#'
#' @return A data.table with identifier columns plus \code{psv_raw},
#'   \code{psv}, \code{osv}, \code{dsv}.
#'
#' @export
calculate_psv_components <- function(player_match_stats, coef_df, osr_coef_df,
                                      dsr_coef_df, min_adjust = TRUE,
                                      center = TRUE, scale_to_minutes = FALSE,
                                      exclude_efficiency = TRUE) {
  psv_result <- calculate_psv(player_match_stats, coef_df,
                               min_adjust = min_adjust, center = center,
                               scale_to_minutes = scale_to_minutes,
                               exclude_efficiency = exclude_efficiency)
  osv_result <- calculate_psv(player_match_stats, osr_coef_df,
                               min_adjust = min_adjust, center = center,
                               scale_to_minutes = scale_to_minutes,
                               exclude_efficiency = exclude_efficiency)
  dsv_result <- calculate_psv(player_match_stats, dsr_coef_df,
                               min_adjust = min_adjust, center = center,
                               scale_to_minutes = scale_to_minutes,
                               exclude_efficiency = exclude_efficiency)

  # Additive shift so osv + dsv = psv
  raw_osv <- osv_result$psv
  raw_dsv <- dsv_result$psv
  delta <- (psv_result$psv - raw_osv - raw_dsv) / 2

  psv_result[, osv := raw_osv + delta]
  psv_result[, dsv := raw_dsv + delta]

  psv_result
}


#' Compute PSV from bundled coefficient files
#'
#' Convenience wrapper that loads pre-trained coefficients and calls
#' \code{\link{calculate_psv_components}}.
#'
#' @param player_match_stats Per-game player stats (one row per player per
#'   match).
#' @param min_adjust Logical. Minutes-adjust raw counts. Default \code{TRUE}.
#' @param center Logical. Center within each round. Default \code{TRUE}.
#' @param scale_to_minutes Logical. Multiply the per-90 PSV by
#'   \code{minutes_played / 90} so values are additive over games (like EPV).
#'   Default \code{FALSE}. See \code{\link{calculate_psv}}.
#' @param exclude_efficiency Logical. Exclude efficiency/ratio stats. Default
#'   \code{TRUE}. Set \code{FALSE} to score with the full trained coefficient
#'   vector (the form used for the displayed blog PSV). See
#'   \code{\link{calculate_psv}}.
#' @param target One of \code{"xg"} (default, xG differential), \code{"goals"}
#'   (goal differential), or \code{"blend"} (alpha*xG + (1-alpha)*goals — the
#'   displayed value model; falls back to \code{"xg"} until the blend is
#'   trained).
#'
#' @return A data.table with \code{psv}, \code{osv}, \code{dsv} columns.
#'
#' @export
compute_player_psv <- function(player_match_stats, min_adjust = TRUE,
                                center = TRUE, target = c("xg", "goals", "blend"),
                                scale_to_minutes = FALSE,
                                exclude_efficiency = TRUE) {
  target <- match.arg(target)
  dt <- data.table::as.data.table(player_match_stats)

  # Route keepers through the GK sub-model (which carries gsaa_per90 and GK
  # features), outfield through the target model — mirroring compute_player_psr.
  # Without this, keepers are scored as bad outfielders (no GK shot-stopping
  # credit). Splitting also centers GKs vs GKs and outfield vs outfield.
  pos_col <- if ("primary_position" %in% names(dt)) "primary_position"
             else if ("position" %in% names(dt)) "position" else NULL
  is_gk <- if (!is.null(pos_col)) {
    grepl("GK|Goalkeeper", dt[[pos_col]], ignore.case = TRUE)
  } else rep(FALSE, nrow(dt))
  is_gk[is.na(is_gk)] <- FALSE

  .score <- function(sub, tgt, model) {
    margin <- load_psr_coefficients("margin", target = tgt, model = model)
    osr <- tryCatch(load_psr_coefficients("offense", target = tgt, model = model),
                    error = function(e) NULL)
    dsr <- tryCatch(load_psr_coefficients("defense", target = tgt, model = model),
                    error = function(e) NULL)
    if (!is.null(osr) && !is.null(dsr)) {
      calculate_psv_components(sub, margin, osr, dsr, min_adjust = min_adjust,
        center = center, scale_to_minutes = scale_to_minutes,
        exclude_efficiency = exclude_efficiency)
    } else {
      cli::cli_inform("OSR/DSR coefficient files not found -- computing PSV only")
      calculate_psv(sub, margin, min_adjust = min_adjust, center = center,
        scale_to_minutes = scale_to_minutes, exclude_efficiency = exclude_efficiency)
    }
  }

  parts <- list()
  if (any(!is_gk)) parts$outfield <- .score(dt[!is_gk], target, "outfield")
  # GK sub-model is trained on goal-diff regardless of the outfield target.
  if (any(is_gk))  parts$gk <- .score(dt[is_gk], "goals", "gk")
  data.table::rbindlist(parts, fill = TRUE, use.names = TRUE)
}


# ============================================================================
# PSV helper: efficiency stat exclusion list
# ============================================================================

#' Get efficiency stat columns to exclude from PSV
#'
#' Returns the subset of \code{.get_psr_skill_cols()} that are efficiency/ratio
#' stats. These are excluded from PSV because they are ratios (not additive
#' counts) and are redundant when their numerator and denominator are already
#' included as rate stats.
#'
#' @return Character vector
#' @keywords internal
.get_psr_efficiency_cols <- function() {
  c(
    "shot_accuracy", "goals_per_shot", "pass_accuracy",
    "tackle_success", "duel_success", "aerial_success",
    "big_chance_conversion", "final_third_pass_acc",
    "long_ball_accuracy", "cross_accuracy",
    "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
    "crosses_open_play_accuracy", "bad_touch_rate",
    "keeper_sweeper_accuracy", "keeper_throws_accuracy", "errors_total_p90",
    "headed_goal_rate", "flick_on_accuracy",
    "back_zone_pass_accuracy", "chipped_pass_accuracy",
    "ibox_goal_rate", "obox_goal_rate",
    "penalty_conversion", "long_pass_own_to_opp_accuracy",
    "fifty_fifty_success", "poss_lost_ctrl_per_touch",
    "save_percentage"
  )
}


# ============================================================================
# Coefficient loading
# ============================================================================

#' Load bundled PSR coefficients
#'
#' Loads pre-trained PSR coefficient CSV files from the package's
#' \code{inst/extdata} directory.
#'
#' @param type One of \code{"margin"}, \code{"offense"}, or \code{"defense"}.
#' @param target One of \code{"xg"} (default, xG differential), \code{"goals"}
#'   (goal differential), or \code{"blend"} (alpha*xG + (1-alpha)*goals; falls
#'   back to \code{"xg"} if the blend files are not yet generated).
#' @param model One of \code{"outfield"} (default) or \code{"gk"} (goalkeeper
#'   sub-model, trained on goal differential).
#'
#' @return A data.frame with columns \code{stat_name}, \code{beta}, and
#'   optionally \code{sd}.
#'
#' @keywords internal
load_psr_coefficients <- function(type = c("margin", "offense", "defense"),
                                   target = c("xg", "goals", "blend"),
                                   model = c("outfield", "gk")) {
  type <- match.arg(type)
  target <- match.arg(target)
  model <- match.arg(model)

  if (model == "gk") {
    # GK sub-model always uses goal diff target
    prefix <- "gk_"
  } else {
    prefix <- switch(target, goals = "gd_", blend = "blend_", "")
  }

  stub <- switch(type, margin = "psr", offense = "osr", defense = "dsr")
  filename <- paste0(prefix, stub, "_coefficients.csv")
  path <- system.file("extdata", filename, package = "panna")

  # Graceful fallback: the blend_ models are generated by a retrain; until then
  # fall back to the xG ("") set so callers (e.g. the blog export) keep working.
  if (path == "" && prefix == "blend_") {
    cli::cli_warn(c(
      "Blended coefficient file not found: {.file {filename}}",
      "i" = "Falling back to xG coefficients. Re-run 07_train_psr_model.R to generate the blend."
    ))
    filename <- paste0(stub, "_coefficients.csv")
    path <- system.file("extdata", filename, package = "panna")
  }

  if (path == "") {
    cli::cli_abort(c(
      "PSR coefficient file not found: {.file {filename}}",
      "i" = "Run {.file data-raw/estimated-skills/07_train_psr_model.R} to generate coefficients."
    ))
  }

  utils::read.csv(path, stringsAsFactors = FALSE)
}


#' Compute PSR from skills using bundled coefficients
#'
#' Convenience wrapper that loads pre-trained coefficients and computes
#' PSR with OSR/DSR decomposition. Automatically routes goalkeepers through
#' a separate GK sub-model (trained on goal differential with GK-specific
#' features) and outfield players through the standard xG-based model.
#'
#' GKs and outfield players are centered separately within their respective
#' populations, then combined.
#'
#' @param skills Player skill data (output of \code{estimate_player_skills()}
#'   or \code{load_opta_skills()}).
#' @param center Logical. Center PSR around position-group mean (default TRUE).
#' @param target One of \code{"xg"} (default) or \code{"goals"} for the
#'   outfield model. GK model always uses goal differential.
#'
#' @return A data.table with \code{psr}, \code{osr}, \code{dsr} columns.
#'
#' @keywords internal
compute_player_psr <- function(skills, center = TRUE,
                                target = c("xg", "goals")) {
  target <- match.arg(target)
  dt <- data.table::as.data.table(skills)

  # Split GKs from outfield players
  is_gk <- dt$primary_position == "GK"
  has_gks <- any(is_gk, na.rm = TRUE)
  has_outfield <- any(!is_gk, na.rm = TRUE)

  results <- list()

  # --- Outfield model: xG/GD target, outfield features ---
  if (has_outfield) {
    outfield_skills <- dt[!is_gk]
    margin_coef <- load_psr_coefficients("margin", target = target)

    prefix <- if (target == "goals") "gd_" else ""
    osr_path <- system.file("extdata", paste0(prefix, "osr_coefficients.csv"),
                             package = "panna")
    dsr_path <- system.file("extdata", paste0(prefix, "dsr_coefficients.csv"),
                             package = "panna")

    if (osr_path != "" && dsr_path != "") {
      osr_coef <- utils::read.csv(osr_path, stringsAsFactors = FALSE)
      dsr_coef <- utils::read.csv(dsr_path, stringsAsFactors = FALSE)
      results$outfield <- calculate_psr_components(
        outfield_skills, margin_coef, osr_coef, dsr_coef, center = center
      )
    } else {
      results$outfield <- calculate_psr(outfield_skills, margin_coef, center = center)
    }
  }

  # --- GK model: goal diff target, GK-specific features ---
  if (has_gks) {
    gk_skills <- dt[is_gk]

    # Check if trained GK coefficients exist
    gk_psr_path <- system.file("extdata", "gk_psr_coefficients.csv",
                                package = "panna")
    gk_osr_path <- system.file("extdata", "gk_osr_coefficients.csv",
                                package = "panna")
    gk_dsr_path <- system.file("extdata", "gk_dsr_coefficients.csv",
                                package = "panna")

    if (gk_psr_path != "") {
      gk_margin_coef <- utils::read.csv(gk_psr_path, stringsAsFactors = FALSE)

      # Check if all betas are zero (placeholder -- model not yet trained)
      if (all(gk_margin_coef$beta == 0)) {
        cli::cli_inform(c(
          "i" = "GK PSR coefficients are placeholders (all zero).",
          "i" = "Run {.file 07_train_psr_model.R} to train the GK sub-model.",
          "i" = "GKs will have PSR = 0 until trained."
        ))
      }

      if (gk_osr_path != "" && gk_dsr_path != "") {
        gk_osr_coef <- utils::read.csv(gk_osr_path, stringsAsFactors = FALSE)
        gk_dsr_coef <- utils::read.csv(gk_dsr_path, stringsAsFactors = FALSE)
        results$gk <- calculate_psr_components(
          gk_skills, gk_margin_coef, gk_osr_coef, gk_dsr_coef, center = center
        )
      } else {
        results$gk <- calculate_psr(gk_skills, gk_margin_coef, center = center)
      }
    } else {
      # No GK model at all -- warn and assign zeros
      cli::cli_warn("GK coefficient files not found. GKs will have PSR = 0.")
      id_cols <- intersect(
        c("player_id", "player_name", "season_end_year", "primary_position",
          "weighted_90s", "total_minutes", "competition", "n_matches"),
        names(gk_skills)
      )
      gk_result <- gk_skills[, id_cols, with = FALSE]
      gk_result[, c("psr_raw", "psr", "osr", "dsr") := 0]
      results$gk <- gk_result
    }
  }

  # Combine results
  data.table::rbindlist(results, fill = TRUE, use.names = TRUE)
}


# ============================================================================
# Cross-league PSR calibration (transfer-graph offsets)
# ============================================================================

#' Estimate cross-league PSR offsets from player transfers
#'
#' PSR is computed from box-score skill rates, which barely vary across leagues
#' (a midfielder completes ~80% of passes in the A-League and the EPL alike), so
#' a strong player in a weak league posts an inflated PSR. The leagues where this
#' bites (A-League, Brazil, MLS, ...) are barely connected to the rest of
#' football by match results, so Elo / opponent-strength controls (which power
#' EPR and Panna) cannot reach them. The ONE thing that does connect them is
#' \strong{player transfers}.
#'
#' This estimates a per-league additive offset on the PSR scale from
#' \emph{adjacent-season} same-player league changes. For a player who is in
#' league A in season \eqn{t} and league B in \eqn{t+1},
#' \deqn{\Delta = \mathrm{PSR}_{t+1} - \mathrm{PSR}_t \approx \mathrm{ease}_B - \mathrm{ease}_A,}
#' because over one season the player's true skill barely changes, so the
#' difference isolates the league-ease gap (the career-stage confound that
#' contaminates career-aggregate bridges is removed by the one-season window).
#' Stacking all transitions gives a linear system solved by weighted ridge with
#' the Big-5 pool anchored to 0. The result is added to a player's PSR
#' (\code{offset < 0} for inflated weak leagues) to put everyone on a
#' Big-5-equivalent scale. This is the PSR analogue of
#' \code{\link{compute_league_offsets}} (which does the same for EPV).
#'
#' @param psr_panel A data.frame/data.table with one row per player-season:
#'   \code{player_id}, \code{season_end_year}, \code{league} (or
#'   \code{competition}), \code{psr}, and a minutes weight
#'   (\code{total_minutes} or \code{weighted_90s}).
#' @param big5 Character vector of league labels treated as the (pooled) anchor
#'   at offset 0. Default the five major European leagues.
#' @param max_season_gap Maximum gap (seasons) between the two stints of a
#'   transition; \code{2} allows a single gap year (injury/loan). Default 2.
#' @param half_life Recency half-life (years) for weighting transitions toward
#'   the present. Default 3.
#' @param ridge_frac Ridge penalty as a fraction of the mean transition weight,
#'   to keep weakly-connected leagues stable (shrinks them toward 0). Default
#'   \code{1e-3}.
#' @param verbose Print the offset table. Default TRUE.
#'
#' @return A data.table with columns \code{league}, \code{offset} (add to PSR),
#'   \code{ease} (\code{-offset}), \code{n_bridge} (distinct bridging players),
#'   and \code{method}. Big-5 rows have \code{offset = 0}.
#'
#' @seealso \code{\link{apply_psr_league_offsets}}, \code{\link{compute_league_offsets}}
#' @export
compute_psr_league_offsets <- function(psr_panel,
                                       big5 = c("EPL", "La_Liga", "Bundesliga",
                                                "Serie_A", "Ligue_1"),
                                       max_season_gap = 2L,
                                       half_life = 3,
                                       ridge_frac = 1e-3,
                                       verbose = TRUE) {
  d <- data.table::as.data.table(psr_panel)
  if (!"league" %in% names(d) && "competition" %in% names(d)) {
    data.table::setnames(d, "competition", "league")
  }
  req <- c("player_id", "season_end_year", "league", "psr")
  miss <- setdiff(req, names(d))
  if (length(miss)) {
    cli::cli_abort("compute_psr_league_offsets: missing columns {.field {miss}}")
  }
  d <- d[!is.na(league) & !is.na(psr) & !is.na(season_end_year)]
  wcol <- if ("total_minutes" %in% names(d)) "total_minutes"
          else if ("weighted_90s" %in% names(d)) "weighted_90s" else NULL
  d[, .mins := if (is.null(wcol)) 90 else pmax(as.numeric(get(wcol)), 1)]

  d[, node := data.table::fifelse(league %in% big5, "BIG5", league)]
  # One row per (player, season, node): pick the node with the most minutes that
  # season (player already has a single primary league per season upstream, but
  # be defensive).
  d <- d[, .(psr = stats::weighted.mean(psr, w = .mins, na.rm = TRUE),
             mins = sum(.mins, na.rm = TRUE)),
         by = .(player_id, season_end_year, node)]

  # --- adjacent-season transitions per player ---
  data.table::setorder(d, player_id, season_end_year)
  d[, `:=`(nxt_node = data.table::shift(node, -1L),
           nxt_season = data.table::shift(season_end_year, -1L),
           nxt_psr = data.table::shift(psr, -1L),
           nxt_mins = data.table::shift(mins, -1L)), by = player_id]
  tr <- d[!is.na(nxt_node) & nxt_node != node &
            (nxt_season - season_end_year) <= max_season_gap]
  if (nrow(tr) < 1) {
    cli::cli_warn("No cross-league transitions found; returning zero offsets.")
    lgs <- unique(c(psr_panel$league, psr_panel$competition))
    return(data.table::data.table(league = lgs[!is.na(lgs)], offset = 0,
                                  ease = 0, n_bridge = 0L, method = "none"))
  }
  tr[, delta := nxt_psr - psr]                      # ~ ease(to) - ease(from)
  ref_year <- max(d$season_end_year, na.rm = TRUE) + 1L
  tr[, w := pmin(mins, nxt_mins) * 2 ^ (-(ref_year - nxt_season) / half_life)]

  # --- weighted ridge solve: ease_L with BIG5 fixed at 0 ---
  lvls <- setdiff(sort(unique(c(tr$node, tr$nxt_node))), "BIG5")
  idx <- stats::setNames(seq_along(lvls), lvls)
  M <- matrix(0, nrow = nrow(tr), ncol = length(lvls))
  from <- tr$node; to <- tr$nxt_node
  for (r in seq_len(nrow(tr))) {
    if (to[r]   != "BIG5") M[r, idx[[to[r]]]]   <- M[r, idx[[to[r]]]]   + 1
    if (from[r] != "BIG5") M[r, idx[[from[r]]]] <- M[r, idx[[from[r]]]] - 1
  }
  W <- tr$w
  lambda <- ridge_frac * sum(W) / length(lvls)
  ease <- as.numeric(solve(t(M) %*% (W * M) + lambda * diag(length(lvls)),
                           t(M) %*% (W * tr$delta)))
  names(ease) <- lvls

  nbridge <- vapply(lvls, function(L)
    data.table::uniqueN(tr[node == L | nxt_node == L]$player_id), integer(1))
  out <- data.table::data.table(
    league = lvls, offset = -ease, ease = ease,
    n_bridge = nbridge, method = "transfer-graph")
  out <- data.table::rbindlist(list(out, data.table::data.table(
    league = big5, offset = 0, ease = 0, n_bridge = NA_integer_,
    method = "anchor")), fill = TRUE)
  data.table::setorder(out, offset)

  if (isTRUE(verbose)) {
    cat(sprintf("\n== PSR league offsets (Big-5-equivalent; %d transitions) ==\n",
                nrow(tr)))
    cat("offset is ADDED to psr; negative = league inflates PSR.\n")
    print(out[, .(league, method, n_bridge, offset = round(offset, 4))])
  }
  out[]
}


#' Apply cross-league PSR offsets to a PSR table
#'
#' Adds the per-league offset from \code{\link{compute_psr_league_offsets}} to
#' each row's \code{psr}, putting weak-league players on a Big-5-equivalent
#' scale. If \code{osr}/\code{dsr} are present, the offset is split evenly so the
#' \code{osr + dsr = psr} identity is preserved.
#'
#' @param psr_dt A data.table/data.frame with a \code{league} column and a
#'   \code{psr} column (optionally \code{osr}, \code{dsr}).
#' @param offsets Offset table from \code{compute_psr_league_offsets} (columns
#'   \code{league}, \code{offset}).
#' @param verbose Report how many rows / leagues were adjusted. Default FALSE.
#'
#' @return \code{psr_dt} (as data.table) with \code{psr} (and \code{osr},
#'   \code{dsr}) shifted, plus a \code{psr_league_offset} column recording the
#'   applied value. Rows whose league has no offset are unchanged (offset 0).
#'
#' @seealso \code{\link{compute_psr_league_offsets}}
#' @export
apply_psr_league_offsets <- function(psr_dt, offsets, verbose = FALSE) {
  dt <- data.table::as.data.table(psr_dt)
  if (!"league" %in% names(dt) && "competition" %in% names(dt)) {
    dt[, league := competition]
  }
  if (!"league" %in% names(dt)) {
    cli::cli_warn("apply_psr_league_offsets: no {.field league} column; returning unchanged.")
    dt[, psr_league_offset := 0]
    return(dt[])
  }
  off <- data.table::as.data.table(offsets)[, .(league, .off = offset)]
  dt <- merge(dt, off, by = "league", all.x = TRUE, sort = FALSE)
  dt[is.na(.off), .off := 0]
  dt[, psr := psr + .off]
  if (all(c("osr", "dsr") %in% names(dt))) {
    dt[, osr := osr + .off / 2]
    dt[, dsr := dsr + .off / 2]
  }
  data.table::setnames(dt, ".off", "psr_league_offset")
  if (isTRUE(verbose)) {
    n_adj <- dt[psr_league_offset != 0, .N]
    cli::cli_inform("Applied PSR league offsets to {n_adj} of {nrow(dt)} rows.")
  }
  dt[]
}


# ============================================================================
# Convenience wrapper: player_psr()
# ============================================================================

#' Get Player Skill Ratings
#'
#' Returns a ranked PSR leaderboard using pre-computed weekly snapshots.
#' Snaps to the nearest weekly date at or before \code{date}.
#'
#' @param date Date to query as a \code{Date} or \code{"YYYY-MM-DD"} string.
#'   Defaults to the latest available snapshot.
#' @param player Optional player name filter (partial match, case-insensitive).
#'   E.g., \code{"Salah"} matches "Mohamed Salah".
#' @param n Number of top players to show (default 50, NULL for all).
#' @param position Filter by position group: \code{"GK"}, \code{"DEF"},
#'   \code{"MID"}, \code{"FWD"}, or NULL for all.
#' @param target One of \code{"xg"} (default, xG differential) or
#'   \code{"goals"} (goal differential). Note: weekly snapshots are xG-based;
#'   \code{"goals"} recomputes from skills on-demand (slower).
#' @param source Data source: \code{"remote"} (default, GitHub Releases) or
#'   \code{"local"}.
#'
#' @return A data.table with columns: \code{snapshot_date}, \code{player_name},
#'   \code{primary_position}, \code{psr}, \code{osr}, \code{dsr},
#'   \code{weighted_90s}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Latest PSR leaderboard
#' player_psr()
#'
#' # As of a specific date
#' player_psr(date = "2026-03-18")
#'
#' # Look up a specific player
#' player_psr(date = "2026-03-18", player = "Salah")
#'
#' # Top midfielders
#' player_psr(position = "MID")
#' }
player_psr <- function(date = NULL, player = NULL, n = 50,
                        position = NULL, target = c("xg", "goals"),
                        source = c("remote", "local")) {
  source <- match.arg(source)
  target <- match.arg(target)

  if (target == "goals") {
    cli::cli_warn(c(
      "Weekly snapshots are xG-based.",
      "i" = "goal-diff PSR is not pre-computed; returning xG-based PSR."
    ))
  }

  # Load from weekly snapshot parquet (snaps to nearest date <= requested)
  psr <- data.table::as.data.table(
    load_opta_psr_weekly(date = date, source = source)
  )

  if (is.null(psr) || nrow(psr) == 0) {
    cli::cli_abort("No weekly PSR data available{if (!is.null(date)) paste0(' for date ', date) else ''}.")
  }

  # Filter by player name (partial, case-insensitive)
  if (!is.null(player)) {
    target_clean <- tolower(gsub("[^a-zA-Z0-9]", "", player))
    clean_names  <- tolower(gsub("[^a-zA-Z0-9]", "", psr$player_name))
    psr <- psr[grepl(target_clean, clean_names, fixed = TRUE)]
    if (nrow(psr) == 0) {
      cli::cli_abort("No players found matching {.val {player}}.")
    }
  }

  # Filter by position (primary_position values are GK/DEF/MID/FWD)
  if (!is.null(position)) {
    position <- toupper(position)
    valid_positions <- c("GK", "DEF", "MID", "FWD")
    if (!position %in% valid_positions) {
      cli::cli_abort(c(
        "Invalid position: {.val {position}}.",
        "i" = "Valid positions: {paste(valid_positions, collapse = ', ')}"
      ))
    }
    psr <- psr[primary_position == position]
  }

  # Sort by PSR descending
  data.table::setorder(psr, -psr)

  # Limit output
  if (!is.null(n) && nrow(psr) > n) {
    psr <- psr[seq_len(n)]
  }

  # Round numeric columns for display
  num_cols <- names(psr)[vapply(psr, is.numeric, logical(1))]
  for (col in setdiff(num_cols, "weighted_90s")) {
    data.table::set(psr, j = col, value = round(psr[[col]], 3))
  }

  psr
}
