# Skill Optimization
#
# Optimization of decay rates and Bayesian prior strengths for estimated skills.
# Uses DEoptim for multi-dimensional decay optimization and Brent's method for
# per-stat prior strength optimization. Objective: minimize weighted MSE of
# next-match prediction across all player-matches.


# ============================================================================
# Stat tier classification
# ============================================================================

#' Get stat tiers for optimization
#'
#' Classifies stats into tiers for optimization. Tier 1 stats get individual
#' lambda optimization. Tier 2 stats share a group lambda.
#'
#' @return Named list with elements \code{tier1} (individually optimized),
#'   \code{tier2} (group optimized), and \code{efficiency} (attempt-weighted group).
#'
#' @export
get_stat_tiers <- function() {
  list(
    tier1 = c(
      # Goals and shooting
      "goals_p90", "shots_p90", "shots_on_target_p90",
      "big_chance_scored_p90", "big_chance_created_p90",
      # Assists and creativity
      "assists_p90", "key_passes_p90", "total_att_assist_p90",
      "through_balls_p90",
      # Possession and progression
      "touches_opp_box_p90", "pen_area_entries_p90",
      "final_third_entries_p90", "final_third_passes_p90",
      # Defending
      "tackles_p90", "tackles_won_p90",
      "interceptions_p90", "interceptions_won_p90",
      "clearances_p90", "ball_recovery_p90",
      # Duels
      "aerial_won_p90", "duel_won_p90",
      # Goalkeeper
      "saves_p90", "goals_conceded_p90",
      # Progressive passing
      "fwd_zone_pass_p90", "open_play_pass_p90"
    ),
    tier2 = c(
      # Less individually important per-90 stats
      "shots_ibox_p90", "shots_obox_p90",
      "passes_p90", "passes_accurate_p90",
      "long_balls_p90", "crosses_p90", "forward_pass_p90",
      "blocks_p90", "blocked_passes_p90",
      "touches_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
      "poss_won_att3rd_p90",
      "dispossessed_p90", "turnover_p90",
      "fouls_p90", "was_fouled_p90",
      "corners_taken_p90",
      "error_lead_to_shot_p90", "error_lead_to_goal_p90",
      "att_fastbreak_p90", "att_openplay_p90",
      "crosses_open_play_p90",
      "last_man_tackle_p90", "six_yard_block_p90",
      "unsuccessful_touch_p90", "overrun_p90"
    ),
    efficiency = c(
      "shot_accuracy", "goals_per_shot", "pass_accuracy",
      "tackle_success", "duel_success", "aerial_success",
      "big_chance_conversion", "final_third_pass_acc",
      "long_ball_accuracy", "cross_accuracy",
      "fwd_zone_pass_accuracy", "open_play_pass_accuracy",
      "crosses_open_play_accuracy", "bad_touch_rate",
      "keeper_sweeper_accuracy", "save_percentage"
    ),
    xmetrics = c(
      "xg_per90", "npxg_per90", "xa_per90_xmetrics",
      "xpass_overperformance_per90_xmetrics"
    )
  )
}


# ============================================================================
# Precomputation for batch optimization
# ============================================================================

# Precompute stat-independent per-player data (dates, minutes, positions).
# Called once by optimize_all_priors() to avoid repeating 68x per stat.
.precompute_player_data <- function(match_stats, sample_n = 500, seed = 42) {
  dt <- data.table::as.data.table(match_stats)
  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }
  data.table::setorder(dt, player_id, match_date)
  dt <- .resolve_positions(dt)

  # Sample players
  players <- unique(dt$player_id)
  set.seed(seed)
  if (length(players) > sample_n) players <- sample(players, sample_n)
  dt <- dt[player_id %in% players]

  # Player modal position lookup (named vector for fast access)
  pos_dt <- dt[!is.na(pos_group), {
    tt <- table(pos_group)
    list(pos_group = names(tt)[which.max(tt)])
  }, by = player_id]
  player_pos_map <- stats::setNames(pos_dt$pos_group, as.character(pos_dt$player_id))

  # Split into per-player lists with precomputed date/minutes arrays
  # Keep all columns so stat-specific extraction is just ps[[stat_name]]
  player_ids <- unique(dt$player_id)
  player_splits <- lapply(player_ids, function(pid) {
    pdf <- dt[player_id == pid]
    dates_num <- as.numeric(pdf$match_date)
    mins <- as.numeric(pdf$total_minutes)
    mins[is.na(mins)] <- 0

    # Store as a list: all columns + precomputed fields
    ps <- as.list(pdf)
    ps$.player_id <- as.character(pid)
    ps$.n <- nrow(pdf)
    ps$.d_rel <- dates_num - dates_num[1]
    ps$.mins <- mins
    ps$.mins_90 <- mins / 90
    ps
  })
  names(player_splits) <- as.character(player_ids)

  list(player_splits = player_splits, player_pos_map = player_pos_map)
}


# ============================================================================
# Per-stat optimization (prior strength + lambda)
# ============================================================================

#' Optimize Bayesian prior strength for a single stat
#'
#' Finds the optimal prior strength (prior_90s for rate stats, prior_attempts
#' for efficiency stats) that minimizes next-match prediction MSE using the
#' Gamma-Poisson or Beta-Binomial posterior.
#'
#' @param match_stats A data.table with match-level stats.
#' @param stat_name Name of the stat column to optimize.
#' @param lambda Decay rate to use. Default 0.003.
#' @param pos_mean Population mean for the stat (prior center). If NULL,
#'   computed as minutes-weighted mean from the data.
#' @param is_efficiency If TRUE, uses Beta-Binomial with attempt-weighting.
#' @param denom_col Denominator column for efficiency stats.
#' @param prior_bounds Numeric vector of length 2: search range for prior
#'   strength. Default c(0.1, 30) for rate stats, c(5, 200) for efficiency.
#' @param min_history Minimum prior matches for a prediction to count.
#' @param sample_n Max number of players to sample (for speed).
#' @param seed Random seed for player sampling.
#'
#' @return A list with \code{stat}, \code{optimal_prior}, \code{mse}, and
#'   \code{n_predictions}.
#'
#' @export
optimize_stat_prior <- function(match_stats = NULL, stat_name,
                                 lambda = 0.003,
                                 pos_mean = NULL,
                                 is_efficiency = FALSE,
                                 denom_col = NULL,
                                 prior_bounds = NULL,
                                 lambda_bounds = NULL,
                                 quantile_bounds = NULL,
                                 optimize_lambda = FALSE,
                                 optimize_quantile = FALSE,
                                 pos_multipliers = NULL,
                                 min_history = 5,
                                 sample_n = 500,
                                 seed = 42,
                                 precomputed = NULL) {
  if (is.null(prior_bounds)) {
    prior_bounds <- if (is_efficiency) c(0.1, 500) else c(0.1, 100)
  }
  if (is.null(lambda_bounds)) {
    lambda_bounds <- c(0, 0.02)
  }
  if (is.null(quantile_bounds)) {
    quantile_bounds <- c(0.05, 0.95)
  }

  # Use precomputed data if available, otherwise compute from scratch
  if (!is.null(precomputed)) {
    player_splits <- precomputed$player_splits
    player_pos_map <- precomputed$player_pos_map

    if (!stat_name %in% names(player_splits[[1]])) {
      return(NULL)
    }

    stat_pos_mult <- if (!is.null(pos_multipliers) && stat_name %in% names(pos_multipliers)) {
      pos_multipliers[[stat_name]]
    } else NULL

    # Compute population mean from precomputed totals
    if (is.null(pos_mean)) {
      total_wt_sum <- 0
      total_wt <- 0
      for (ps in player_splits) {
        v <- as.numeric(ps[[stat_name]])
        v[is.na(v)] <- 0
        w <- ps$.mins
        total_wt_sum <- total_wt_sum + sum(v * w)
        total_wt <- total_wt + sum(w)
      }
      pos_mean <- if (total_wt > 0) total_wt_sum / total_wt else 0
    }

    player_avgs <- vapply(player_splits, function(ps) {
      v <- as.numeric(ps[[stat_name]])
      v[is.na(v)] <- 0
      w <- ps$.mins
      tw <- sum(w)
      if (tw > 0) sum(v * w) / tw else 0
    }, numeric(1))

    # Build player_data from precomputed splits
    player_data <- lapply(player_splits, function(ps) {
      if (ps$.n <= min_history) return(NULL)

      vals <- as.numeric(ps[[stat_name]])
      vals[is.na(vals)] <- 0
      mins_90 <- ps$.mins_90

      denom <- mins_90
      if (is_efficiency && !is.null(denom_col)) {
        if (grepl("\\+", denom_col)) {
          parts <- strsplit(denom_col, "\\+")[[1]]
          denom <- rep(0, length(vals))
          for (p in parts) {
            if (p %in% names(ps)) {
              d <- as.numeric(ps[[p]])
              d[is.na(d)] <- 0
              denom <- denom + d
            }
          }
        } else if (denom_col %in% names(ps)) {
          denom <- as.numeric(ps[[denom_col]])
          denom[is.na(denom)] <- 0
        }
      }

      # Position multiplier
      pos_mult <- 1.0
      pg <- player_pos_map[ps$.player_id]
      if (!is.null(stat_pos_mult) && !is.na(pg) && pg %in% names(stat_pos_mult)) {
        pos_mult <- stat_pos_mult[pg]
      }

      out <- list(vals = vals, d_rel = ps$.d_rel, mins_90 = mins_90,
                  n = ps$.n, pos_mult = pos_mult)
      if (is_efficiency) {
        out$successes <- vals * denom
        out$attempts <- denom
      } else {
        out$events <- vals * mins_90
        out$exposure <- mins_90
      }
      out
    })
    player_data <- Filter(Negate(is.null), player_data)

  } else {
    # Standalone mode: compute everything from match_stats
    dt <- data.table::as.data.table(match_stats)
    if (!inherits(dt$match_date, "Date")) {
      dt[, match_date := as.Date(match_date)]
    }

    if (!stat_name %in% names(dt)) {
      return(NULL)
    }

    data.table::setorder(dt, player_id, match_date)
    dt <- .resolve_positions(dt)

    stat_pos_mult <- if (!is.null(pos_multipliers) && stat_name %in% names(pos_multipliers)) {
      pos_multipliers[[stat_name]]
    } else {
      mults <- compute_position_multipliers(dt, stat_name)
      if (stat_name %in% names(mults)) mults[[stat_name]] else NULL
    }

    player_pos_lkp <- dt[!is.na(pos_group), {
      tt <- table(pos_group)
      list(pos_group = names(tt)[which.max(tt)])
    }, by = player_id]

    player_avgs <- dt[, {
      v <- as.numeric(get(stat_name))
      v[is.na(v)] <- 0
      w <- as.numeric(total_minutes)
      w[is.na(w)] <- 0
      tw <- sum(w)
      list(wavg = if (tw > 0) sum(v * w) / tw else 0)
    }, by = player_id]$wavg

    if (is.null(pos_mean)) {
      vals <- as.numeric(dt[[stat_name]])
      wts <- as.numeric(dt$total_minutes)
      vals[is.na(vals)] <- 0
      wts[is.na(wts)] <- 0
      pos_mean <- if (sum(wts) > 0) sum(vals * wts) / sum(wts) else 0
    }

    players <- unique(dt$player_id)
    set.seed(seed)
    if (length(players) > sample_n) players <- sample(players, sample_n)

    player_data <- lapply(players, function(pid) {
      pdf <- dt[player_id == pid]
      if (nrow(pdf) <= min_history) return(NULL)

      vals <- as.numeric(pdf[[stat_name]])
      vals[is.na(vals)] <- 0
      dates <- as.numeric(pdf$match_date)
      mins <- as.numeric(pdf$total_minutes)
      mins[is.na(mins)] <- 0
      mins_90 <- mins / 90
      d_rel <- dates - dates[1]

      denom <- mins_90
      if (is_efficiency && !is.null(denom_col)) {
        if (grepl("\\+", denom_col)) {
          parts <- strsplit(denom_col, "\\+")[[1]]
          denom <- rep(0, length(vals))
          for (p in parts) {
            if (p %in% names(pdf)) {
              d <- as.numeric(pdf[[p]])
              d[is.na(d)] <- 0
              denom <- denom + d
            }
          }
        } else if (denom_col %in% names(pdf)) {
          denom <- as.numeric(pdf[[denom_col]])
          denom[is.na(denom)] <- 0
        }
      }

      pos_row <- player_pos_lkp[player_id == pid]
      pos_mult <- 1.0
      if (nrow(pos_row) > 0 && !is.null(stat_pos_mult)) {
        pg <- pos_row$pos_group
        if (!is.na(pg) && pg %in% names(stat_pos_mult)) {
          pos_mult <- stat_pos_mult[pg]
        }
      }

      out <- list(vals = vals, d_rel = d_rel, mins_90 = mins_90,
                  n = nrow(pdf), pos_mult = pos_mult)
      if (is_efficiency) {
        out$successes <- vals * denom
        out$attempts <- denom
      } else {
        out$events <- vals * mins_90
        out$exposure <- mins_90
      }
      out
    })
    player_data <- Filter(Negate(is.null), player_data)
  }

  if (length(player_data) == 0) return(NULL)

  # Determine which parameters are free vs fixed
  # prior_bounds[1] == prior_bounds[2] means prior is fixed
  optimize_prior <- prior_bounds[1] != prior_bounds[2]
  fixed_prior <- if (!optimize_prior) prior_bounds[1] else NULL

  # Vectorized objective using cumsum trick: O(n) per player instead of O(n^2)
  # Key identity: exp(-lam*(d_j - d_i)) = exp(-lam*d_j) * exp(lam*d_i)
  # So decay-weighted sums become: exp(-lam*d_j) * cumsum(exp(lam*d_i) * x_i)
  objective <- function(par) {
    idx <- 0

    if (optimize_prior) {
      idx <- idx + 1
      prior_strength <- par[idx]
    } else {
      prior_strength <- fixed_prior
    }

    if (optimize_lambda) {
      idx <- idx + 1
      lam <- par[idx]
    } else {
      lam <- lambda
    }

    if (optimize_quantile) {
      idx <- idx + 1
      q <- par[idx]
      mu0_raw <- stats::quantile(player_avgs, probs = q, na.rm = TRUE,
                                  names = FALSE)
    } else {
      mu0_raw <- pos_mean
    }

    total_loss <- 0
    n_preds <- 0

    for (pd in player_data) {
      player_mu0 <- mu0_raw * pd$pos_mult
      j_idx <- (min_history + 1):pd$n

      # Vectorized decay weights via cumsum
      exp_pos <- exp(lam * pd$d_rel)
      exp_neg <- exp(-lam * pd$d_rel)

      if (is_efficiency) {
        cum_succ <- cumsum(exp_pos * pd$successes)
        cum_att <- cumsum(exp_pos * pd$attempts)

        w_succ <- exp_neg[j_idx] * cum_succ[j_idx - 1]
        w_att <- exp_neg[j_idx] * cum_att[j_idx - 1]

        mu <- max(min(player_mu0, 1 - 1e-6), 1e-6)
        alpha0 <- mu * prior_strength
        beta0 <- (1 - mu) * prior_strength

        predicted <- (alpha0 + w_succ) / (alpha0 + beta0 + w_att)
        predicted <- pmax(pmin(predicted, 1 - 1e-8), 1e-8)
        actual <- pmax(pmin(pd$vals[j_idx], 1 - 1e-8), 1e-8)
        loss <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))

        match_w <- pd$attempts[j_idx]
        ok <- pd$mins_90[j_idx] > 0
        total_loss <- total_loss + sum(match_w[ok] * loss[ok])
        n_preds <- n_preds + sum(match_w[ok])
      } else {
        cum_events <- cumsum(exp_pos * pd$events)
        cum_exposure <- cumsum(exp_pos * pd$exposure)

        w_events <- exp_neg[j_idx] * cum_events[j_idx - 1]
        w_exposure <- exp_neg[j_idx] * cum_exposure[j_idx - 1]

        alpha0 <- player_mu0 * prior_strength
        beta0 <- prior_strength
        predicted <- (alpha0 + w_events) / (beta0 + w_exposure)

        w <- pd$mins_90[j_idx]
        ok <- w > 0
        total_loss <- total_loss + sum(w[ok] * (predicted[ok] - pd$vals[j_idx][ok])^2)
        n_preds <- n_preds + sum(w[ok])
      }
    }

    if (n_preds > 0) total_loss / n_preds else 1e6
  }

  # Build parameter bounds based on what we're optimizing
  par_init <- c()
  par_lower <- c()
  par_upper <- c()

  if (optimize_prior) {
    par_init <- c(par_init, mean(prior_bounds))
    par_lower <- c(par_lower, prior_bounds[1])
    par_upper <- c(par_upper, prior_bounds[2])
  }
  if (optimize_lambda) {
    par_init <- c(par_init, lambda)
    par_lower <- c(par_lower, lambda_bounds[1])
    par_upper <- c(par_upper, lambda_bounds[2])
  }
  if (optimize_quantile) {
    par_init <- c(par_init, 0.5)  # start at median
    par_lower <- c(par_lower, quantile_bounds[1])
    par_upper <- c(par_upper, quantile_bounds[2])
  }

  n_params <- length(par_init)

  if (n_params == 1) {
    # 1D: Brent's method
    interval <- if (optimize_prior) prior_bounds else
                if (optimize_quantile) quantile_bounds else lambda_bounds
    result <- stats::optimize(objective, interval = interval, tol = 0.01)
    out <- list(stat = stat_name, loss = result$objective)

    if (optimize_prior) out$optimal_prior <- result$minimum
    else out$optimal_prior <- fixed_prior

    if (optimize_lambda) out$optimal_lambda <- result$minimum

    if (optimize_quantile) {
      out$optimal_quantile <- result$minimum
      out$prior_center <- stats::quantile(player_avgs, probs = result$minimum,
                                           na.rm = TRUE, names = FALSE)
    }
  } else {
    # Multi-dimensional: L-BFGS-B
    result <- stats::optim(
      par = par_init, fn = objective, method = "L-BFGS-B",
      lower = par_lower, upper = par_upper
    )
    if (result$convergence != 0) {
      cli::cli_warn("L-BFGS-B failed to converge for stat {.val {stat_name}} (code {result$convergence}).")
    }
    idx <- 0
    out <- list(stat = stat_name, loss = result$value)

    if (optimize_prior) {
      idx <- idx + 1
      out$optimal_prior <- result$par[idx]
    } else {
      out$optimal_prior <- fixed_prior
    }
    if (optimize_lambda) {
      idx <- idx + 1
      out$optimal_lambda <- result$par[idx]
    }
    if (optimize_quantile) {
      idx <- idx + 1
      out$optimal_quantile <- result$par[idx]
      out$prior_center <- stats::quantile(player_avgs, probs = out$optimal_quantile,
                                           na.rm = TRUE, names = FALSE)
    }
  }

  out$loss_type <- if (is_efficiency) "logloss" else "mse"
  out$is_efficiency <- is_efficiency
  out
}


#' Optimize prior strengths for all stats
#'
#' Finds the optimal Bayesian prior strength per stat. Rate stats get
#' per-stat \code{prior_90s}, efficiency stats get per-stat
#' \code{prior_attempts}. Results are stored in \code{decay_params$stat_priors}.
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#' @param decay_params Base decay parameters (provides lambda values).
#' @param stat_tiers Output of \code{get_stat_tiers()}, or NULL for defaults.
#' @param sample_n Max players to sample per stat (default 500).
#' @param verbose Print progress.
#'
#' @return Updated decay_params with \code{stat_priors} element: a named
#'   numeric vector mapping stat names to optimal prior strengths.
#'
#' @export
optimize_all_priors <- function(match_stats, decay_params = NULL,
                                 stat_tiers = NULL,
                                 optimize_lambda = TRUE,
                                 optimize_quantile = FALSE,
                                 sample_n = 500,
                                 n_cores = 1,
                                 verbose = TRUE) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()
  if (is.null(stat_tiers)) stat_tiers <- get_stat_tiers()

  eff_map <- .classify_skill_stats()
  stat_priors <- c()
  prior_quantiles <- c()
  prior_centers <- c()
  all_results <- list()

  # Collect all stats to optimize
  all_rate_stats <- intersect(
    c(stat_tiers$tier1, stat_tiers$tier2, stat_tiers$xmetrics),
    names(match_stats)
  )
  all_eff_stats <- intersect(stat_tiers$efficiency, names(match_stats))

  # Compute position multipliers once for all stats
  all_stat_cols <- c(all_rate_stats, all_eff_stats)
  pos_multipliers <- compute_position_multipliers(match_stats, all_stat_cols)
  if (verbose) progress_msg("Computed position multipliers (GK/DEF/MID/FWD)")

  # Precompute shared per-player data (sort, positions, sample, split)
  # This avoids repeating expensive work 68x (once per stat)
  if (verbose) progress_msg("Precomputing player splits...")
  precomputed <- .precompute_player_data(match_stats, sample_n = sample_n, seed = 42)
  if (verbose) progress_msg(sprintf("  %d players, %d with >5 matches",
                                     length(precomputed$player_splits),
                                     sum(vapply(precomputed$player_splits,
                                                function(x) x$.n > 5, logical(1)))))

  # Helper: format and print result
  print_result <- function(stat, result, is_eff) {
    parts <- sprintf("prior=%5.1f", result$optimal_prior)
    if (!is.null(result$optimal_lambda)) {
      hl <- if (result$optimal_lambda > 0) log(2) / result$optimal_lambda else Inf
      parts <- paste0(parts, sprintf("  lam=%.5f(%.0fd)", result$optimal_lambda, hl))
    }
    if (!is.null(result$optimal_quantile)) {
      parts <- paste0(parts, sprintf("  q=%.0f%%", result$optimal_quantile * 100))
    }
    loss_str <- if (is_eff) sprintf("ll=%.4f", result$loss) else sprintf("RMSE=%.4f", sqrt(result$loss))
    parts <- paste0(parts, sprintf("  %s", loss_str))
    progress_msg(sprintf("  %-35s %s", stat, parts))
  }

  all_stats <- c(all_rate_stats, all_eff_stats)
  is_eff_flags <- c(rep(FALSE, length(all_rate_stats)), rep(TRUE, length(all_eff_stats)))

  # Build argument list for one optimization pass
  build_pass_args <- function(opt_prior, opt_lam, opt_q) {
    lapply(seq_along(all_stats), function(i) {
      stat <- all_stats[i]
      is_eff <- is_eff_flags[i]
      lam <- if (stat %in% names(decay_params)) decay_params[[stat]] else {
        if (is_eff) decay_params$efficiency else decay_params$rate
      }
      denom_col <- if (is_eff && stat %in% names(eff_map)) eff_map[[stat]] else NULL
      p_bounds <- if (opt_prior) NULL else {
        p <- if (stat %in% names(stat_priors)) stat_priors[stat] else {
          if (is_eff) decay_params$prior_attempts %||% 50 else decay_params$prior_90s %||% 2
        }
        c(p, p)
      }
      list(stat = stat, is_eff = is_eff, lam = lam, denom_col = denom_col,
           p_bounds = p_bounds, opt_lam = opt_lam, opt_q = opt_q)
    })
  }

  # Set up parallel cluster if requested
  cl <- NULL
  if (n_cores > 1) {
    pkg_path <- tryCatch(pkgload::pkg_path(), error = function(e) getwd())
    cl <- tryCatch({
      cl <- parallel::makeCluster(n_cores)
      parallel::clusterExport(cl, "pkg_path", envir = environment())
      parallel::clusterEvalQ(cl, {
        tryCatch(
          devtools::load_all(pkg_path, quiet = TRUE),
          error = function(e) library(panna)
        )
        library(data.table)
      })
      parallel::clusterExport(cl, c("precomputed", "pos_multipliers", "sample_n"),
                              envir = environment())
      if (verbose) progress_msg(sprintf("Using %d cores for parallel optimization", n_cores))
      cl
    }, error = function(e) {
      if (verbose) progress_msg(sprintf("Parallel setup failed (%s), using sequential", e$message))
      NULL
    })
    if (!is.null(cl)) on.exit(parallel::stopCluster(cl), add = TRUE)
  }

  # Run a batch of stat optimizations (parallel or sequential)
  run_batch <- function(args_list) {
    worker <- function(a) {
      optimize_stat_prior(
        stat_name = a$stat,
        lambda = a$lam,
        is_efficiency = a$is_eff,
        denom_col = a$denom_col,
        prior_bounds = a$p_bounds,
        optimize_lambda = a$opt_lam,
        optimize_quantile = a$opt_q,
        pos_multipliers = pos_multipliers,
        sample_n = sample_n,
        precomputed = precomputed
      )
    }
    if (!is.null(cl)) {
      # Strip environment to avoid serializing large parent frame
      environment(worker) <- .GlobalEnv
      parallel::parLapplyLB(cl, args_list, worker)
    } else {
      lapply(args_list, worker)
    }
  }

  # --- Joint 2D optimization: prior strength + lambda (L-BFGS-B) ---
  if (optimize_lambda) {
    if (verbose) {
      progress_msg(sprintf("Optimizing prior + lambda jointly (2D L-BFGS-B) for %d stats...",
                            length(all_stats)))
    }

    pass_args <- build_pass_args(opt_prior = TRUE, opt_lam = TRUE, opt_q = FALSE)
    pass_results <- run_batch(pass_args)

    for (i in seq_along(all_stats)) {
      result <- pass_results[[i]]
      if (!is.null(result)) {
        stat <- all_stats[i]
        is_eff <- is_eff_flags[i]
        stat_priors[stat] <- result$optimal_prior
        if (!is.null(result$optimal_lambda)) {
          decay_params[[stat]] <- result$optimal_lambda
        }
        all_results[[stat]] <- result
        if (verbose) {
          hl <- if (!is.null(result$optimal_lambda) && result$optimal_lambda > 0) {
            log(2) / result$optimal_lambda
          } else Inf
          loss_str <- if (is_eff) sprintf("ll=%.4f", result$loss) else sprintf("RMSE=%.4f", sqrt(result$loss))
          progress_msg(sprintf("  %-35s prior=%6.1f  lam=%.5f (%.0fd)  %s",
                                stat, result$optimal_prior,
                                result$optimal_lambda %||% NA_real_, hl, loss_str))
        }
      }
    }
  } else {
    # Prior-only optimization (1D Brent)
    if (verbose) {
      progress_msg(sprintf("Optimizing prior strength for %d stats...",
                            length(all_stats)))
    }

    pass_args <- build_pass_args(opt_prior = TRUE, opt_lam = FALSE, opt_q = FALSE)
    pass_results <- run_batch(pass_args)

    for (i in seq_along(all_stats)) {
      result <- pass_results[[i]]
      if (!is.null(result)) {
        stat <- all_stats[i]
        is_eff <- is_eff_flags[i]
        stat_priors[stat] <- result$optimal_prior
        all_results[[stat]] <- result
        if (verbose) print_result(stat, result, is_eff)
      }
    }
  }

  # --- Pass 3: optimize quantile only (prior+lambda fixed, 1D Brent) ---
  if (optimize_quantile) {
    if (verbose) {
      progress_msg(sprintf("\nPass 3: Optimizing quantile for %d stats (prior+lambda fixed)...",
                            length(all_stats)))
    }

    pass_args <- build_pass_args(opt_prior = FALSE, opt_lam = FALSE, opt_q = TRUE)
    pass_results <- run_batch(pass_args)

    for (i in seq_along(all_stats)) {
      result <- pass_results[[i]]
      if (!is.null(result) && !is.null(result$optimal_quantile)) {
        stat <- all_stats[i]
        is_eff <- is_eff_flags[i]
        prior_quantiles[stat] <- result$optimal_quantile
        prior_centers[stat] <- result$prior_center
        all_results[[stat]]$optimal_quantile <- result$optimal_quantile
        all_results[[stat]]$prior_center <- result$prior_center
        all_results[[stat]]$loss <- result$loss
        if (verbose) {
          loss_str <- if (is_eff) sprintf("ll=%.4f", result$loss) else sprintf("RMSE=%.4f", sqrt(result$loss))
          progress_msg(sprintf("  %-35s q=%.0f%% (center=%.4f)  %s",
                                stat, result$optimal_quantile * 100,
                                result$prior_center, loss_str))
        }
      }
    }
  }

  decay_params$stat_priors <- stat_priors
  decay_params$position_multipliers <- pos_multipliers
  if (optimize_quantile) {
    decay_params$prior_quantiles <- prior_quantiles
    decay_params$prior_centers <- prior_centers
  }
  attr(decay_params, "prior_optimization_results") <- all_results

  if (verbose) {
    rate_priors <- stat_priors[all_rate_stats[all_rate_stats %in% names(stat_priors)]]
    eff_priors <- stat_priors[all_eff_stats[all_eff_stats %in% names(stat_priors)]]
    progress_msg(sprintf(
      "Optimized %d stats. Rate prior median: %.1f, Efficiency prior median: %.1f",
      length(stat_priors),
      if (length(rate_priors) > 0) stats::median(rate_priors) else NA,
      if (length(eff_priors) > 0) stats::median(eff_priors) else NA
    ))
    if (optimize_quantile && length(prior_quantiles) > 0) {
      progress_msg(sprintf(
        "Quantile median: %.0f%%, range: [%.0f%%, %.0f%%]",
        stats::median(prior_quantiles) * 100,
        min(prior_quantiles) * 100,
        max(prior_quantiles) * 100
      ))
    }
  }

  decay_params
}
