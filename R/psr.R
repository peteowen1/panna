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
#' dummies — the elastic net selects relevant features automatically.
#'
#' @return Character vector of column names
#' @keywords internal
.get_psr_skill_cols <- function() {
  # Per-90 rate columns
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
    "saves_p90", "goals_conceded_p90",
    "fwd_zone_pass_p90", "open_play_pass_p90",
    "error_lead_to_shot_p90", "error_lead_to_goal_p90",
    "att_fastbreak_p90", "shot_fastbreak_p90",
    "att_openplay_p90", "att_setpiece_p90", "att_headed_p90",
    "att_one_on_one_p90", "crosses_open_play_p90",
    "penalty_won_p90", "penalty_conceded_p90",
    "offtarget_att_assist_p90",
    "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
    "keeper_sweeper_p90", "attempts_conceded_ibox_p90",
    "attempts_conceded_obox_p90", "gk_smother_p90",
    "unsuccessful_touch_p90", "overrun_p90", "flick_on_p90"
  )

  # Efficiency/proportion columns
  efficiency_cols <- c(
    "shot_accuracy", "goals_per_shot", "pass_accuracy",
    "tackle_success", "duel_success", "aerial_success",
    "big_chance_conversion", "final_third_pass_acc",
    "long_ball_accuracy", "cross_accuracy",
    "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
    "crosses_open_play_accuracy", "bad_touch_rate",
    "keeper_sweeper_accuracy", "errors_total_p90",
    "headed_goal_rate", "flick_on_accuracy",
    "back_zone_pass_accuracy", "chipped_pass_accuracy",
    "ibox_goal_rate", "obox_goal_rate",
    "penalty_conversion", "long_pass_own_to_opp_accuracy",
    "fifty_fifty_success", "poss_lost_ctrl_per_touch",
    "save_percentage"
  )

  # xMetrics columns (if available)
  xmetrics_cols <- c(
    "xg_per90", "npxg_per90", "xa_per90_xmetrics",
    "xpass_overperformance_per90_xmetrics"
  )

  c(rate_cols, efficiency_cols, xmetrics_cols)
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

  # Auto-detect stat columns
  eff_map <- .classify_skill_stats()
  p90_cols <- grep("_p90$", names(dt), value = TRUE)
  eff_cols <- intersect(names(eff_map), names(dt))
  stat_cols <- intersect(c(p90_cols, eff_cols), names(dt))

  if (length(stat_cols) == 0) {
    cli::cli_warn("No stat columns found in match_stats.")
    return(list())
  }

  # Resolve lambda per stat
  stat_lambdas <- vapply(stat_cols, function(sc) {
    .resolve_lambda(sc, decay_params, eff_map)
  }, numeric(1))

  # Position multipliers & grand means — compute once
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

  # Rate stat priors: alpha0 matrix (n_players × n_rate_stats)
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
  # For rate stats: w_num (n_players × n_cols), w_den (n_players × 1)
  # For eff stats: w_num and w_den per stat (n_players × 1 each)

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
  cursor <- 0L  # index into dt — rows 1:cursor have been processed

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

      # Skip dates with no prior data (use NULL, not return() — we're in tryCatch)
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
# Coefficient loading
# ============================================================================

#' Load bundled PSR coefficients
#'
#' Loads pre-trained PSR coefficient CSV files from the package's
#' \code{inst/extdata} directory.
#'
#' @param type One of \code{"margin"}, \code{"offense"}, or \code{"defense"}.
#' @param target One of \code{"xg"} (default, xG differential) or
#'   \code{"goals"} (goal differential).
#'
#' @return A data.frame with columns \code{stat_name}, \code{beta}, and
#'   optionally \code{sd}.
#'
#' @keywords internal
load_psr_coefficients <- function(type = c("margin", "offense", "defense"),
                                   target = c("xg", "goals")) {
  type <- match.arg(type)
  target <- match.arg(target)

  prefix <- if (target == "goals") "gd_" else ""
  filename <- switch(type,
    margin  = paste0(prefix, "psr_coefficients.csv"),
    offense = paste0(prefix, "osr_coefficients.csv"),
    defense = paste0(prefix, "dsr_coefficients.csv")
  )

  path <- system.file("extdata", filename, package = "panna")
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
#' PSR with OSR/DSR decomposition (if offensive/defensive coefficient files
#' are available).
#'
#' @param skills Player skill data (output of \code{estimate_player_skills()}
#'   or \code{load_opta_skills()}).
#' @param center Logical. Center PSR around league mean (default TRUE).
#' @param target One of \code{"xg"} (default) or \code{"goals"}.
#'
#' @return A data.table with \code{psr}, \code{osr}, \code{dsr} columns.
#'
#' @keywords internal
compute_player_psr <- function(skills, center = TRUE,
                                target = c("xg", "goals")) {
  target <- match.arg(target)
  margin_coef <- load_psr_coefficients("margin", target = target)

  prefix <- if (target == "goals") "gd_" else ""
  osr_path <- system.file("extdata", paste0(prefix, "osr_coefficients.csv"),
                           package = "panna")
  dsr_path <- system.file("extdata", paste0(prefix, "dsr_coefficients.csv"),
                           package = "panna")

  if (osr_path != "" && dsr_path != "") {
    osr_coef <- utils::read.csv(osr_path, stringsAsFactors = FALSE)
    dsr_coef <- utils::read.csv(dsr_path, stringsAsFactors = FALSE)
    calculate_psr_components(skills, margin_coef, osr_coef, dsr_coef, center = center)
  } else {
    cli::cli_inform("OSR/DSR coefficient files not found -- computing PSR only (no osr/dsr decomposition)")
    calculate_psr(skills, margin_coef, center = center)
  }
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
