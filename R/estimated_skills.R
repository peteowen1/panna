# Estimated Skills Framework
#
# Per-stat decay-weighted skill estimation for football players.
# Replaces noisy season averages with optimally decay-weighted career data
# as SPM inputs. Adapted from the EPM methodology for basketball.
#
# Core idea: For each stat, a player's "true skill" at a point in time is
# estimated as a weighted average of all prior match observations, where
# weights decay exponentially with time and scale by sample size (minutes
# or attempts). Low-sample players are regressed toward position means.


# ============================================================================
# Default decay parameters
# ============================================================================

#' Get default decay parameters for skill estimation
#'
#' Returns sensible default exponential decay rates (lambda) per stat category.
#' Lambda controls how quickly old observations lose influence:
#' \code{weight = exp(-lambda * days_since_match)}.
#'
#' Half-life = ln(2) / lambda. Higher lambda = faster decay = more recency bias.
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{rate}{Lambda for per-90 rate stats (~8 month half-life)}
#'     \item{efficiency}{Lambda for efficiency/accuracy stats (~1 year half-life)}
#'     \item{xmetrics}{Lambda for xG/xA/xPass metrics (~8 month half-life)}
#'     \item{prior_90s}{Gamma prior strength for rate stats, in equivalent 90s.
#'       Higher = more shrinkage toward position mean. Default 2.}
#'     \item{prior_attempts}{Beta prior strength for efficiency stats, in
#'       equivalent attempts. Higher = more shrinkage. Default 50.}
#'   }
#'
#' @export
#' @examples
#' params <- get_default_decay_params()
#' # Half-life in days: log(2) / params$rate
get_default_decay_params <- function() {
  list(
    rate = 0.003,           # half-life ~231 days (~8 months)
    efficiency = 0.002,     # half-life ~347 days (~1 year)
    xmetrics = 0.003,       # half-life ~231 days (~8 months)
    prior_90s = 2,          # Gamma prior strength (equiv. full matches)
    prior_attempts = 50     # Beta prior strength (equiv. attempts)
  )
}


# ============================================================================
# Position helpers
# ============================================================================

#' Simplify match positions to 4 groups
#'
#' Maps Opta match positions (Goalkeeper, Defender, Wing Back, Midfielder,
#' Defensive Midfielder, Attacking Midfielder, Striker, Substitute) to
#' GK/DEF/MID/FWD.
#'
#' @param pos Character vector of position strings.
#' @return Character vector with simplified positions (NA for unrecognized).
#' @keywords internal
.simplify_position <- function(pos) {
  data.table::fcase(
    pos == "Goalkeeper", "GK",
    pos %in% c("Defender", "Wing Back"), "DEF",
    pos %in% c("Midfielder", "Defensive Midfielder", "Attacking Midfielder"), "MID",
    pos == "Striker", "FWD",
    default = NA_character_
  )
}


#' Resolve substitute positions using each player's modal starting position
#'
#' Players listed as "Substitute" get their most common starting position from
#' other matches. Players who only ever appear as Substitute are left as NA.
#'
#' @param dt A data.table with player_id and position columns.
#' @return The input data.table with an added \code{pos_group} column.
#' @keywords internal
.resolve_positions <- function(dt) {
  dt[, pos_group := .simplify_position(position)]

  # For NA (mostly Substitutes), fill with player's modal starting position
  na_idx <- which(is.na(dt$pos_group))
  if (length(na_idx) > 0) {
    modal_pos <- dt[!is.na(pos_group), {
      tt <- table(pos_group)
      list(modal_pos = names(tt)[which.max(tt)])
    }, by = player_id]

    dt[modal_pos, on = "player_id", modal_pos := i.modal_pos]
    dt[is.na(pos_group) & !is.na(modal_pos), pos_group := modal_pos]
    dt[, modal_pos := NULL]
  }

  dt
}


# Compute denominator vector for efficiency stats from a denom_spec string.
# Supports "col1+col2" for summing multiple columns.
.compute_denominator <- function(dt_sub, denom_spec) {
  if (grepl("\\+", denom_spec)) {
    parts <- strsplit(denom_spec, "\\+")[[1]]
    result <- rep(0, nrow(dt_sub))
    found_any <- FALSE
    for (p in parts) {
      if (p %in% names(dt_sub)) {
        v <- as.numeric(dt_sub[[p]])
        v[is.na(v)] <- 0
        result <- result + v
        found_any <- TRUE
      }
    }
    if (!found_any) {
      cli::cli_abort("No denominator columns found for {.val {denom_spec}}. Cannot compute efficiency stat.")
    }
    return(result)
  }
  if (denom_spec %in% names(dt_sub)) {
    v <- as.numeric(dt_sub[[denom_spec]])
    v[is.na(v)] <- 0
    return(v)
  }
  cli::cli_abort("Denominator column {.val {denom_spec}} not found. Cannot compute efficiency stat.")
}


#' Compute position-specific multipliers for prior centers
#'
#' For each stat, computes the ratio of position-specific weighted average to
#' the overall weighted average: \code{multiplier = pos_avg / global_avg}.
#' This allows the prior center to be position-specific while keeping the
#' global optimization.
#'
#' @param match_stats A data.table with match-level stats and a \code{position}
#'   column.
#' @param stat_cols Character vector of stat columns. If NULL, auto-detects.
#' @return A named list where each element is a named numeric vector of length 4
#'   (GK, DEF, MID, FWD) giving the multiplier for that stat and position.
#'
#' @export
compute_position_multipliers <- function(match_stats, stat_cols = NULL) {
  dt <- data.table::copy(data.table::as.data.table(match_stats))
  dt <- .resolve_positions(dt)

  if (is.null(stat_cols)) {
    p90_cols <- grep("_p90$", names(dt), value = TRUE)
    eff_cols <- intersect(names(.classify_skill_stats()), names(dt))
    stat_cols <- c(p90_cols, eff_cols)
  }
  stat_cols <- intersect(stat_cols, names(dt))

  pos_groups <- c("GK", "DEF", "MID", "FWD")
  multipliers <- list()

  for (sc in stat_cols) {
    vals <- as.numeric(dt[[sc]])
    vals[is.na(vals)] <- 0
    wts <- as.numeric(dt$total_minutes)
    wts[is.na(wts)] <- 0

    global_avg <- if (sum(wts) > 0) sum(vals * wts) / sum(wts) else 0

    pos_mults <- stats::setNames(rep(1.0, 4), pos_groups)

    if (global_avg > 0) {
      for (pg in pos_groups) {
        idx <- which(dt$pos_group == pg)
        if (length(idx) > 0) {
          pw <- wts[idx]
          pv <- vals[idx]
          pos_avg <- if (sum(pw) > 0) sum(pv * pw) / sum(pw) else 0
          pos_mults[pg] <- pos_avg / global_avg
        }
      }
    }

    multipliers[[sc]] <- pos_mults
  }

  multipliers
}


# ============================================================================
# Stat classification
# ============================================================================

#' Classify stats into rate vs efficiency categories
#'
#' Per-90 rate stats are weighted by minutes; efficiency stats (success rates,
#' conversion rates) are weighted by their denominator (attempts). This function
#' returns the classification and the denominator column for efficiency stats.
#'
#' @return A data.table with columns: stat, category, denominator
#' @keywords internal
.classify_skill_stats <- function() {
  # Efficiency stats: column name -> denominator column name

  efficiency_map <- list(
    shot_accuracy = "shots",
    goals_per_shot = "shots",
    ibox_shot_ratio = "shots",
    big_chance_conversion = "big_chance_scored+big_chance_missed",
    pass_accuracy = "passes",
    final_third_pass_acc = "final_third_passes",
    long_ball_accuracy = "long_balls",
    through_ball_accuracy = "through_balls",
    cross_accuracy = "crosses",
    tackle_success = "tackles",
    interception_success = "interceptions",
    clearance_effectiveness = "clearances",
    duel_success = "duel_won+duel_lost",
    aerial_success = "aerial_won+aerial_lost",
    fwd_zone_pass_accuracy = "fwd_zone_pass",
    open_play_pass_accuracy = "open_play_pass",
    crosses_open_play_accuracy = "crosses_open_play",
    bad_touch_rate = "touches",
    headed_goal_rate = "att_headed",
    flick_on_accuracy = "flick_on",
    keeper_sweeper_accuracy = "keeper_sweeper",
    back_zone_pass_accuracy = "back_zone_pass",
    chipped_pass_accuracy = "chipped_pass",
    ibox_goal_rate = "shots_ibox",
    obox_goal_rate = "shots_obox",
    penalty_conversion = "att_pen_goal+att_pen_miss",
    long_pass_own_to_opp_accuracy = "long_pass_own_to_opp",
    fifty_fifty_success = "fifty_fifty",
    poss_lost_ctrl_per_touch = "touches",
    save_percentage = "saves+goals_conceded"
  )

  efficiency_map
}


# ============================================================================
# Core skill estimation
# ============================================================================

#' Estimate player skills using Bayesian conjugate priors with decay weighting
#'
#' For each player, estimates "true skill" at a point in time using all matches
#' before \code{target_date}. Uses conjugate Bayesian updating with
#' exponential time decay:
#'
#' \strong{Rate stats (per-90):} Gamma-Poisson model. Raw event counts are
#' back-calculated from \code{stat_p90 * (minutes/90)}, then decay-weighted.
#' The Gamma prior is centered on the position mean with strength controlled
#' by \code{prior_90s}. Posterior mean is in per-90 units.
#'
#' \strong{Efficiency stats (proportions):} Beta-Binomial model. Successes are
#' back-calculated from \code{proportion * attempts}, then decay-weighted.
#' The Beta prior is centered on the position mean with strength controlled
#' by \code{prior_attempts}.
#'
#' Shrinkage toward position means happens naturally through the prior —
#' players with little data stay close to the prior, while players with
#' abundant data are driven by their observations.
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#'   Must contain: match_date, player_id, player_name, total_minutes, position,
#'   and the stat columns to estimate.
#' @param decay_params Named list of decay rates and prior strengths. Output of
#'   \code{get_default_decay_params()} or custom. Supports per-stat lambda
#'   overrides (named by stat column). Prior strength via \code{prior_90s}
#'   (Gamma, default 10) and \code{prior_attempts} (Beta, default 50).
#' @param target_date Date object. Only matches before this date are used.
#'   If NULL, uses all available data.
#' @param min_weighted_90s Not used for shrinkage (handled by Bayesian prior).
#'   Retained for backward compatibility.
#' @param stat_cols Character vector of stat columns to estimate. If NULL,
#'   auto-detects all _p90 columns plus efficiency columns.
#'
#' @return A data.table with one row per player containing estimated skill
#'   values for each stat, plus player_id, player_name, position, and
#'   weighted_90s (effective sample size in decay-weighted 90-minute units).
#'
#' @export
estimate_player_skills <- function(match_stats, decay_params = NULL,
                                    target_date = NULL, min_weighted_90s = 5,
                                    stat_cols = NULL) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()

  dt <- data.table::copy(data.table::as.data.table(match_stats))

  # Ensure match_date is Date

  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }

  # Filter to before target_date
  if (!is.null(target_date)) {
    target_date <- as.Date(target_date)
    dt <- dt[match_date < target_date]
  }

  if (nrow(dt) == 0) {
    cli::cli_warn("No match data available before target_date.")
    return(NULL)
  }

  # Auto-detect stat columns
  if (is.null(stat_cols)) {
    p90_cols <- grep("_p90$", names(dt), value = TRUE)
    efficiency_stats <- names(.classify_skill_stats())
    eff_cols <- intersect(efficiency_stats, names(dt))
    # Position dummies
    pos_cols <- intersect(c("is_gk", "is_df", "is_mf", "is_fw"), names(dt))
    stat_cols <- c(p90_cols, eff_cols)
  }

  stat_cols <- intersect(stat_cols, names(dt))
  if (length(stat_cols) == 0) {
    cli::cli_warn("No stat columns found in match_stats.")
    return(NULL)
  }

  # Compute days since match (relative to target_date or max date)
  ref_date <- if (!is.null(target_date)) target_date else max(dt$match_date, na.rm = TRUE)
  dt[, days_since := as.numeric(ref_date - match_date)]

  # Classify stats
  eff_map <- .classify_skill_stats()

  # Resolve lambda per stat
  get_lambda <- function(stat_name) {
    # If decay_params has per-stat overrides
    if (stat_name %in% names(decay_params)) return(decay_params[[stat_name]])
    # Category-based defaults
    if (stat_name %in% names(eff_map)) return(decay_params$efficiency)
    if (grepl("^(xg|npxg|xa|xpass)", stat_name)) return(decay_params$xmetrics)
    decay_params$rate
  }

  compute_denominator <- .compute_denominator

  # Resolve positions to GK/DEF/MID/FWD groups
  dt <- .resolve_positions(dt)

  # Get position group for each player (mode of pos_group)
  player_pos <- dt[!is.na(pos_group), {
    tt <- table(pos_group)
    list(pos_group = names(tt)[which.max(tt)])
  }, by = player_id]

  # Compute position multipliers: pos_avg / global_avg per stat
  pos_multipliers <- if (!is.null(decay_params$position_multipliers)) {
    decay_params$position_multipliers
  } else {
    compute_position_multipliers(dt, stat_cols)
  }

  # Build global prior center lookup per stat (minutes-weighted mean)
  grand_means <- numeric(length(stat_cols))
  names(grand_means) <- stat_cols
  prior_centers_cached <- decay_params$prior_centers
  total_wt <- sum(data.table::fifelse(is.na(dt$total_minutes), 0,
                                       as.numeric(dt$total_minutes)))
  for (sc in stat_cols) {
    if (!is.null(prior_centers_cached) && sc %in% names(prior_centers_cached)) {
      grand_means[sc] <- prior_centers_cached[sc]
    } else if (sc %in% names(dt)) {
      vals <- as.numeric(dt[[sc]])
      vals[is.na(vals)] <- 0
      wts <- data.table::fifelse(is.na(dt$total_minutes), 0,
                                  as.numeric(dt$total_minutes))
      if (total_wt > 0) grand_means[sc] <- sum(vals * wts) / total_wt
    }
  }

  # Prior strength
  default_prior_90s <- decay_params$prior_90s %||% 2
  default_prior_attempts <- decay_params$prior_attempts %||% 50
  stat_priors <- decay_params$stat_priors

  get_prior <- function(stat_name, is_eff) {
    if (!is.null(stat_priors) && stat_name %in% names(stat_priors)) {
      return(stat_priors[[stat_name]])
    }
    if (is_eff) default_prior_attempts else default_prior_90s
  }

  # --- Vectorized skill estimation via data.table grouped operations ---
  # Instead of looping per player × per stat, process each stat as one
  # grouped aggregation across all players (C-level data.table code)

  dt[, .mins_90 := data.table::fifelse(is.na(total_minutes), 0,
                                         as.numeric(total_minutes) / 90)]

  # Player metadata (one row per player)
  player_meta <- dt[, .(player_name = player_name[1]), by = player_id]
  player_meta[player_pos, pos_group := i.pos_group, on = "player_id"]

  # Precompute decay weights grouped by unique lambda values
  stat_lambdas <- vapply(stat_cols, get_lambda, numeric(1))
  unique_lambdas <- unique(stat_lambdas)
  decay_cols <- character(length(unique_lambdas))
  names(decay_cols) <- as.character(unique_lambdas)
  for (j in seq_along(unique_lambdas)) {
    lam <- unique_lambdas[j]
    col_name <- sprintf(".w%d", j)
    decay_cols[as.character(lam)] <- col_name
    data.table::set(dt, j = col_name, value = exp(-lam * dt$days_since))
  }

  # Weighted 90s for player context (use default rate lambda)
  rate_w_col <- decay_cols[as.character(decay_params$rate)]
  if (is.null(rate_w_col) || is.na(rate_w_col)) {
    dt[, .w_rate := exp(-decay_params$rate * days_since)]
    rate_w_col <- ".w_rate"
  }
  w90_agg <- dt[, .(weighted_90s = sum(get(rate_w_col) * .mins_90, na.rm = TRUE)),
                by = player_id]

  # Process each stat: one data.table grouped sum per stat
  skill_results <- vector("list", length(stat_cols))
  names(skill_results) <- stat_cols

  for (sc in stat_cols) {
    lam <- stat_lambdas[sc]
    w_col <- decay_cols[as.character(lam)]
    w_vec <- dt[[w_col]]
    is_eff <- sc %in% names(eff_map)
    prior_strength <- get_prior(sc, is_eff)

    vals <- as.numeric(dt[[sc]])
    vals[is.na(vals)] <- 0

    if (is_eff) {
      denom <- compute_denominator(dt, eff_map[[sc]])
      data.table::set(dt, j = ".wnum", value = w_vec * vals * denom)
      data.table::set(dt, j = ".wden", value = w_vec * denom)
      agg <- dt[, .(w_num = sum(.wnum), w_den = sum(.wden)), by = player_id]

      # Apply position-specific Beta-Binomial posterior
      agg[player_meta, pos_group := i.pos_group, on = "player_id"]
      gm <- grand_means[sc]
      pm_lookup <- if (sc %in% names(pos_multipliers)) pos_multipliers[[sc]] else NULL
      agg[, mu0 := {
        m <- rep(gm, .N)
        if (!is.null(pm_lookup)) {
          for (pg in names(pm_lookup)) {
            m[pos_group == pg] <- gm * pm_lookup[pg]
          }
        }
        pmax(pmin(m, 1 - 1e-6), 1e-6)
      }]
      agg[, (sc) := (mu0 * prior_strength + w_num) /
                     (prior_strength + w_den)]
      skill_results[[sc]] <- agg[, .(player_id, skill = get(sc))]
    } else {
      data.table::set(dt, j = ".wnum", value = w_vec * vals * dt$.mins_90)
      data.table::set(dt, j = ".wden", value = w_vec * dt$.mins_90)
      agg <- dt[, .(w_num = sum(.wnum), w_den = sum(.wden)), by = player_id]

      # Apply position-specific Gamma-Poisson posterior
      agg[player_meta, pos_group := i.pos_group, on = "player_id"]
      gm <- grand_means[sc]
      pm_lookup <- if (sc %in% names(pos_multipliers)) pos_multipliers[[sc]] else NULL
      agg[, alpha0 := {
        m <- rep(gm, .N)
        if (!is.null(pm_lookup)) {
          for (pg in names(pm_lookup)) {
            m[pos_group == pg] <- gm * pm_lookup[pg]
          }
        }
        m * prior_strength
      }]
      agg[, (sc) := (alpha0 + w_num) / (prior_strength + w_den)]
      skill_results[[sc]] <- agg[, .(player_id, skill = get(sc))]
    }
  }

  # Clean up temporary columns
  tmp_cols <- c(".mins_90", ".wnum", ".wden",
                grep("^\\.w[0-9]+$", names(dt), value = TRUE),
                if (".w_rate" %in% names(dt)) ".w_rate")
  for (tc in tmp_cols) {
    if (tc %in% names(dt)) data.table::set(dt, j = tc, value = NULL)
  }

  # Combine: start with player metadata, join each stat result
  result <- data.table::copy(player_meta)
  result[, date := ref_date]
  result[w90_agg, weighted_90s := i.weighted_90s, on = "player_id"]
  data.table::setnames(result, "pos_group", "primary_position")

  for (sc in stat_cols) {
    sr <- skill_results[[sc]]
    data.table::setnames(sr, "skill", sc)
    result[sr, (sc) := get(paste0("i.", sc)), on = "player_id"]
  }

  progress_msg(sprintf("Estimated skills for %d players (%d stats)",
                        nrow(result), length(stat_cols)))
  result
}


# ============================================================================
# Skill inspection / diagnostics
# ============================================================================

#' Inspect skill estimate breakdown for a single stat
#'
#' Shows the full Bayesian decomposition for every player: raw totals,
#' career average, prior center, prior weight, decay-weighted evidence,
#' and the final skill estimate.
#'
#' @param stat_name Name of the stat (e.g. \code{"goals_p90"}).
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#' @param decay_params Decay parameters list.
#' @param target_date Date to estimate skills as of. Default today.
#' @return A data.table sorted by skill estimate (descending), one row per player.
#'
#' @export
inspect_skill <- function(stat_name, match_stats, decay_params = NULL,
                           target_date = Sys.Date()) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()
  dt <- data.table::copy(data.table::as.data.table(match_stats))
  if (!inherits(dt$match_date, "Date")) dt[, match_date := as.Date(match_date)]

  target_date <- as.Date(target_date)
  dt <- dt[match_date < target_date]

  if (!stat_name %in% names(dt)) {
    cli::cli_abort("Stat {.val {stat_name}} not found in match_stats.")
  }

  eff_map <- .classify_skill_stats()
  is_eff <- stat_name %in% names(eff_map)

  # Lambda
  lam <- if (stat_name %in% names(decay_params)) decay_params[[stat_name]]
         else if (is_eff) decay_params$efficiency
         else if (grepl("^(xg|npxg|xa|xpass)", stat_name)) decay_params$xmetrics
         else decay_params$rate
  half_life <- if (lam > 0) log(2) / lam else Inf

  # Prior strength
  stat_priors <- decay_params$stat_priors
  prior_strength <- if (!is.null(stat_priors) && stat_name %in% names(stat_priors)) {
    stat_priors[[stat_name]]
  } else if (is_eff) {
    decay_params$prior_attempts %||% 50
  } else {
    decay_params$prior_90s %||% 2
  }

  # Positions
  dt <- .resolve_positions(dt)
  player_pos <- dt[!is.na(pos_group), {
    tt <- table(pos_group)
    list(pos_group = names(tt)[which.max(tt)])
  }, by = player_id]

  # Grand mean
  vals_all <- as.numeric(dt[[stat_name]])
  vals_all[is.na(vals_all)] <- 0
  wts_all <- data.table::fifelse(is.na(dt$total_minutes), 0,
                                  as.numeric(dt$total_minutes))
  grand_mean <- if (sum(wts_all) > 0) sum(vals_all * wts_all) / sum(wts_all) else 0

  # Position multipliers
  pm <- if (!is.null(decay_params$position_multipliers) &&
            stat_name %in% names(decay_params$position_multipliers)) {
    decay_params$position_multipliers[[stat_name]]
  } else {
    mults <- compute_position_multipliers(dt, stat_name)
    if (stat_name %in% names(mults)) mults[[stat_name]] else NULL
  }

  # Compute per-player aggregates
  dt[, .mins_90 := data.table::fifelse(is.na(total_minutes), 0,
                                         as.numeric(total_minutes) / 90)]
  dt[, days_since := as.numeric(target_date - match_date)]
  dt[, .w := exp(-lam * days_since)]

  data.table::set(dt, j = ".vals", value = {
    v <- as.numeric(dt[[stat_name]]); v[is.na(v)] <- 0; v
  })

  if (is_eff) {
    data.table::set(dt, j = ".denom", value = .compute_denominator(dt, eff_map[[stat_name]]))
    data.table::set(dt, j = ".wnum", value = dt$.w * dt$.vals * dt$.denom)
    data.table::set(dt, j = ".wden", value = dt$.w * dt$.denom)
    agg <- dt[, .(
      total_minutes = sum(total_minutes, na.rm = TRUE),
      matches = .N,
      raw_successes = sum(.vals * .denom),
      raw_attempts = sum(.denom),
      w_successes = sum(.wnum),
      w_attempts = sum(.wden),
      weighted_90s = sum(.w * .mins_90)
    ), by = player_id]
    agg[, raw_rate := data.table::fifelse(raw_attempts > 0,
                                           raw_successes / raw_attempts, 0)]
  } else {
    data.table::set(dt, j = ".wnum", value = dt$.w * dt$.vals * dt$.mins_90)
    data.table::set(dt, j = ".wden", value = dt$.w * dt$.mins_90)
    agg <- dt[, .(
      total_minutes = sum(total_minutes, na.rm = TRUE),
      matches = .N,
      raw_total = sum(.vals * .mins_90),
      total_90s = sum(.mins_90),
      w_events = sum(.wnum),
      w_exposure = sum(.wden),
      weighted_90s = sum(.w * .mins_90)
    ), by = player_id]
    agg[, raw_p90 := data.table::fifelse(total_90s > 0,
                                          raw_total / total_90s, 0)]
  }

  # Join player metadata
  agg[dt[, .(player_name = player_name[1]), by = player_id],
      player_name := i.player_name, on = "player_id"]
  agg[player_pos, pos_group := i.pos_group, on = "player_id"]

  # Position-specific prior center
  agg[, prior_center := {
    m <- rep(grand_mean, .N)
    if (!is.null(pm)) {
      for (pg in names(pm)) m[pos_group == pg] <- grand_mean * pm[pg]
    }
    m
  }]
  agg[, prior_strength := prior_strength]

  # Skill estimate (Bayesian posterior)
  if (is_eff) {
    agg[, mu0 := pmax(pmin(prior_center, 1 - 1e-6), 1e-6)]
    agg[, skill := (mu0 * prior_strength + w_successes) /
                   (prior_strength + w_attempts)]
    agg[, mu0 := NULL]
  } else {
    agg[, skill := (prior_center * prior_strength + w_events) /
                   (prior_strength + w_exposure)]
  }

  # Clean up temp columns from dt
  for (tc in c(".mins_90", ".w", ".wnum", ".wden", ".vals", ".denom", "days_since")) {
    if (tc %in% names(dt)) data.table::set(dt, j = tc, value = NULL)
  }

  # Reorder columns for readability
  agg[, lambda := lam]
  agg[, half_life_days := half_life]

  if (is_eff) {
    data.table::setnames(agg, c("w_successes", "w_attempts"),
                         c("decay_successes", "decay_attempts"))
    col_order <- c("player_name", "pos_group", "total_minutes", "matches",
                   "raw_successes", "raw_attempts", "raw_rate",
                   "prior_center", "prior_strength", "lambda", "half_life_days",
                   "decay_successes", "decay_attempts",
                   "weighted_90s", "skill")
  } else {
    data.table::setnames(agg, c("w_events", "w_exposure"),
                         c("decay_events", "decay_exposure"))
    col_order <- c("player_name", "pos_group", "total_minutes", "matches",
                   "raw_total", "total_90s", "raw_p90",
                   "prior_center", "prior_strength", "lambda", "half_life_days",
                   "decay_events", "decay_exposure",
                   "weighted_90s", "skill")
  }

  col_order <- intersect(col_order, names(agg))
  data.table::setcolorder(agg, col_order)
  data.table::setorderv(agg, "skill", order = -1)
  agg
}


# ============================================================================
# Season-level aggregation (SPM-compatible output)
# ============================================================================

#' Aggregate skills for SPM input (one row per player per season)
#'
#' Produces a data frame in the same format as \code{aggregate_opta_stats()},
#' where each row is one player's estimated skill profile at the end of a season.
#' This is a drop-in replacement for SPM input.
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#' @param decay_params Decay parameters (from \code{get_default_decay_params()}
#'   or optimized).
#' @param season_end_dates Named list mapping season_end_year (integer) to
#'   a Date. Skills are estimated as of this date for each season. If NULL,
#'   uses June 30 of each year.
#' @param min_minutes Minimum total minutes in the season for inclusion.
#' @param min_weighted_90s Minimum weighted 90s for regression threshold.
#'
#' @return A data.table with one row per player per season_end_year, containing
#'   skill estimates for all stats plus identity/context columns. Compatible
#'   with \code{fit_spm_opta()}.
#'
#' @export
aggregate_skills_for_spm <- function(match_stats, decay_params = NULL,
                                      season_end_dates = NULL,
                                      min_minutes = 450,
                                      min_weighted_90s = 5) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()

  dt <- data.table::copy(data.table::as.data.table(match_stats))
  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }

  # Determine seasons from data
  if (!"season_end_year" %in% names(dt)) {
    if ("season" %in% names(dt)) {
      dt[, season_end_year := .extract_end_year(season)]
    } else {
      # Infer from match_date: season ending in June
      dt[, season_end_year := data.table::fifelse(
        data.table::month(match_date) >= 7L,
        data.table::year(match_date) + 1L,
        data.table::year(match_date)
      )]
    }
  }

  seasons <- sort(unique(dt$season_end_year))
  seasons <- seasons[!is.na(seasons)]

  # Build season end dates
  if (is.null(season_end_dates)) {
    season_end_dates <- stats::setNames(
      as.Date(paste0(seasons, "-06-30")),
      as.character(seasons)
    )
  }

  results_list <- vector("list", length(seasons))

  for (idx in seq_along(seasons)) {
    s <- seasons[idx]
    target_date <- season_end_dates[as.character(s)]
    if (is.na(target_date)) target_date <- as.Date(paste0(s, "-06-30"))

    # Season-specific minutes filter: sum minutes in this season only
    season_start <- as.Date(paste0(s - 1, "-07-01"))
    season_dt <- dt[match_date >= season_start & match_date < target_date]

    if (nrow(season_dt) == 0) {
      cli::cli_alert_warning("Season {s}: no matches found, skipping.")
      next
    }

    season_minutes <- season_dt[, .(season_minutes = sum(total_minutes, na.rm = TRUE)),
                                 by = player_id]
    eligible_players <- season_minutes[season_minutes >= min_minutes]$player_id

    if (length(eligible_players) == 0) {
      cli::cli_alert_warning("Season {s}: no players with >= {min_minutes} minutes, skipping.")
      next
    }

    # Estimate skills using ALL history up to target_date (not just this season)
    skills <- estimate_player_skills(
      match_stats = dt,
      decay_params = decay_params,
      target_date = target_date,
      min_weighted_90s = min_weighted_90s
    )

    if (is.null(skills) || nrow(skills) == 0) next

    # Filter to eligible players
    skills <- skills[player_id %in% eligible_players]

    # Add season context
    season_info <- season_dt[player_id %in% eligible_players,
                              .(total_minutes = sum(total_minutes, na.rm = TRUE),
                                n_matches = data.table::uniqueN(match_id)),
                              by = .(player_id)]

    skills <- season_info[skills, on = "player_id", nomatch = NULL]
    skills[, season_end_year := s]

    # primary_position already set by estimate_player_skills()

    results_list[[idx]] <- skills
  }

  result <- data.table::rbindlist(results_list, fill = TRUE)

  if (nrow(result) == 0) {
    cli::cli_warn("No players met minimum minutes threshold across all seasons.")
    return(NULL)
  }

  # Compute position dummies if primary_position exists
  if ("primary_position" %in% names(result)) {
    pos <- result$primary_position
    result[, is_gk := as.integer(grepl("GK|Goalkeeper", pos, ignore.case = TRUE))]
    result[, is_df := as.integer(grepl("DEF|Defender", pos, ignore.case = TRUE))]
    result[, is_mf := as.integer(grepl("MID|Midfielder", pos, ignore.case = TRUE))]
    result[, is_fw := as.integer(grepl("FWD|Forward|Striker", pos, ignore.case = TRUE))]
  }

  # Replace NAs with 0 in numeric columns
  num_cols <- names(result)[vapply(result, is.numeric, logical(1))]
  for (col in num_cols) {
    data.table::set(result, which(is.na(result[[col]])), col, 0)
  }

  progress_msg(sprintf("Skill-based features: %d player-seasons, %d seasons",
                        nrow(result), length(unique(result$season_end_year))))

  result
}


# ============================================================================
# Point-in-time skill estimation (for match predictions)
# ============================================================================

#' Estimate player skills at a specific date
#'
#' Computes current skill vectors for specified players at a given date.
#' Useful for match prediction where you need skill estimates for upcoming
#' fixtures rather than end-of-season snapshots.
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#' @param decay_params Decay parameters.
#' @param player_ids Character vector of player_ids to estimate. If NULL,
#'   estimates all players with data before the date.
#' @param date Date to estimate skills at.
#' @param min_weighted_90s Regression threshold.
#'
#' @return A data.table with one row per player containing skill estimates.
#'
#' @export
estimate_player_skills_at_date <- function(match_stats, decay_params = NULL,
                                            player_ids = NULL, date = Sys.Date(),
                                            min_weighted_90s = 5) {
  dt <- data.table::as.data.table(match_stats)

  # Filter to requested players if specified

  if (!is.null(player_ids)) {
    dt <- dt[player_id %in% player_ids]
  }

  estimate_player_skills(
    match_stats = dt,
    decay_params = decay_params,
    target_date = date,
    min_weighted_90s = min_weighted_90s
  )
}


# ============================================================================
# Backtesting
# ============================================================================

#' Backtest skill predictions against actual match performance
#'
#' For each player-match in the data, computes the Bayesian skill estimate
#' using all prior matches (with decay weighting), then compares to actual
#' performance. This is equivalent to computing weekly skill estimates and
#' evaluating how well they predict the next match.
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#' @param decay_params Decay parameters (with optional \code{stat_priors}).
#' @param stat_cols Character vector of stats to evaluate. If NULL, uses a
#'   default set of key stats.
#' @param min_history Minimum prior matches before predictions count (default 5).
#' @param sample_n Max players to sample for speed (default NULL = all).
#' @param seed Random seed for sampling.
#'
#' @return A list with:
#'   \describe{
#'     \item{predictions}{data.table of individual predictions: player_id,
#'       match_date, stat, predicted, actual, minutes}
#'     \item{accuracy}{data.table of per-stat accuracy metrics: RMSE,
#'       improvement vs simple average, improvement vs last match}
#'   }
#'
#' @export
backtest_skill_predictions <- function(match_stats, decay_params = NULL,
                                        stat_cols = NULL, min_history = 5,
                                        sample_n = NULL, seed = 42) {
  if (is.null(decay_params)) decay_params <- get_default_decay_params()

  dt <- data.table::copy(data.table::as.data.table(match_stats))
  if (!inherits(dt$match_date, "Date")) dt[, match_date := as.Date(match_date)]
  data.table::setorder(dt, player_id, match_date)

  eff_map <- .classify_skill_stats()
  stat_priors <- decay_params$stat_priors
  default_prior_90s <- decay_params$prior_90s %||% 2
  default_prior_attempts <- decay_params$prior_attempts %||% 50

  # Default stats to evaluate
  if (is.null(stat_cols)) {
    stat_cols <- c(
      "goals_p90", "shots_p90", "shots_on_target_p90", "assists_p90",
      "key_passes_p90", "big_chance_created_p90", "touches_opp_box_p90",
      "passes_p90", "final_third_passes_p90", "fwd_zone_pass_p90",
      "tackles_won_p90", "interceptions_p90", "clearances_p90",
      "ball_recovery_p90", "aerial_won_p90", "duel_won_p90",
      "fouls_p90", "was_fouled_p90", "dispossessed_p90",
      "saves_p90", "goals_conceded_p90",
      "shot_accuracy", "pass_accuracy", "tackle_success",
      "duel_success", "aerial_success", "save_percentage"
    )
  }
  stat_cols <- intersect(stat_cols, names(dt))

  # Resolve positions and compute multipliers
  dt <- .resolve_positions(dt)
  pos_multipliers <- if (!is.null(decay_params$position_multipliers)) {
    decay_params$position_multipliers
  } else {
    compute_position_multipliers(dt, stat_cols)
  }

  # Player modal position lookup
  player_pos_lookup <- dt[!is.na(pos_group), {
    tt <- table(pos_group)
    list(pos_group = names(tt)[which.max(tt)])
  }, by = player_id]

  # Compute global prior centers: use optimized quantiles if available, else weighted mean
  prior_quantiles <- decay_params$prior_quantiles
  prior_centers_cached <- decay_params$prior_centers

  pop_means <- vapply(stat_cols, function(s) {
    if (!is.null(prior_centers_cached) && s %in% names(prior_centers_cached)) {
      return(prior_centers_cached[s])
    }
    if (!is.null(prior_quantiles) && s %in% names(prior_quantiles)) {
      pavg <- dt[, {
        v <- as.numeric(get(s)); v[is.na(v)] <- 0
        w <- as.numeric(total_minutes); w[is.na(w)] <- 0
        tw <- sum(w)
        list(wavg = if (tw > 0) sum(v * w) / tw else 0)
      }, by = player_id]$wavg
      return(stats::quantile(pavg, probs = prior_quantiles[s], na.rm = TRUE,
                              names = FALSE))
    }
    vals <- as.numeric(dt[[s]])
    wts <- as.numeric(dt$total_minutes)
    vals[is.na(vals)] <- 0
    wts[is.na(wts)] <- 0
    if (sum(wts) > 0) sum(vals * wts) / sum(wts) else 0
  }, numeric(1))

  # Sample players
  players <- unique(dt$player_id)
  if (!is.null(sample_n) && length(players) > sample_n) {
    set.seed(seed)
    players <- sample(players, sample_n)
  }

  # Collect predictions
  pred_list <- vector("list", length(stat_cols))
  names(pred_list) <- stat_cols

  for (si in seq_along(stat_cols)) {
    sc <- stat_cols[si]
    is_eff <- sc %in% names(eff_map)
    lambda <- if (sc %in% names(decay_params)) {
      decay_params[[sc]]
    } else if (is_eff) {
      decay_params$efficiency
    } else if (grepl("^(xg|npxg|xa|xpass)", sc)) {
      decay_params$xmetrics
    } else {
      decay_params$rate
    }

    prior_strength <- if (!is.null(stat_priors) && sc %in% names(stat_priors)) {
      stat_priors[[sc]]
    } else if (is_eff) {
      default_prior_attempts
    } else {
      default_prior_90s
    }

    mu0 <- pop_means[sc]
    match_preds <- list()
    k <- 0

    # Pre-compute denominator column for efficiency stats
    denom_vals_all <- NULL
    if (is_eff) {
      denom_spec <- eff_map[[sc]]
      if (grepl("\\+", denom_spec)) {
        parts <- strsplit(denom_spec, "\\+")[[1]]
        denom_vals_all <- rep(0, nrow(dt))
        for (p in parts) {
          if (p %in% names(dt)) {
            d <- as.numeric(dt[[p]])
            d[is.na(d)] <- 0
            denom_vals_all <- denom_vals_all + d
          }
        }
      } else if (denom_spec %in% names(dt)) {
        denom_vals_all <- as.numeric(dt[[denom_spec]])
        denom_vals_all[is.na(denom_vals_all)] <- 0
      }
    }

    for (pid in players) {
      pdf_idx <- which(dt$player_id == pid)
      if (length(pdf_idx) <= min_history) next

      vals <- as.numeric(dt[[sc]][pdf_idx])
      vals[is.na(vals)] <- 0
      dates <- as.numeric(dt$match_date[pdf_idx])
      mins <- as.numeric(dt$total_minutes[pdf_idx])
      mins[is.na(mins)] <- 0
      mins_90 <- mins / 90

      # Get denominator for this player's matches
      denoms <- if (!is.null(denom_vals_all)) denom_vals_all[pdf_idx] else mins_90

      # Apply position multiplier to prior center
      player_pos_row <- player_pos_lookup[player_id == pid]
      player_mu0 <- mu0
      if (nrow(player_pos_row) > 0) {
        ppos <- player_pos_row$pos_group
        if (!is.na(ppos) && sc %in% names(pos_multipliers) &&
            ppos %in% names(pos_multipliers[[sc]])) {
          player_mu0 <- mu0 * pos_multipliers[[sc]][ppos]
        }
      }

      for (j in (min_history + 1):length(pdf_idx)) {
        if (mins_90[j] == 0) next

        days_since <- dates[j] - dates[1:(j - 1)]
        time_decay <- exp(-lambda * days_since)
        h_vals <- vals[1:(j - 1)]

        if (is_eff) {
          # Beta-Binomial with actual attempt counts
          attempts <- denoms[1:(j - 1)]
          successes <- h_vals * attempts
          w_succ <- sum(time_decay * successes)
          w_att <- sum(time_decay * attempts)
          if (w_att == 0) next

          mu <- max(min(player_mu0, 1 - 1e-6), 1e-6)
          alpha0 <- mu * prior_strength
          beta0 <- (1 - mu) * prior_strength
          bayes_pred <- (alpha0 + w_succ) / (alpha0 + beta0 + w_att)
        } else {
          # Gamma-Poisson
          events <- h_vals * mins_90[1:(j - 1)]
          w_events <- sum(time_decay * events)
          w_exposure <- sum(time_decay * mins_90[1:(j - 1)])
          alpha0 <- player_mu0 * prior_strength
          beta0 <- prior_strength
          bayes_pred <- (alpha0 + w_events) / (beta0 + w_exposure)
        }

        if (is_eff) {
          # Attempt-weighted career average (total successes / total attempts)
          h_attempts <- denoms[1:(j - 1)]
          total_att <- sum(h_attempts)
          avg_pred <- if (total_att > 0) sum(h_vals * h_attempts) / total_att else mean(h_vals)
        } else {
          # 90s-weighted career average (total events / total exposure)
          h_mins90 <- mins_90[1:(j - 1)]
          total_exp <- sum(h_mins90)
          avg_pred <- if (total_exp > 0) sum(h_vals * h_mins90) / total_exp else mean(h_vals)
        }
        last_pred <- vals[j - 1]

        k <- k + 1
        match_preds[[k]] <- list(
          player_id = pid,
          match_date = dt$match_date[pdf_idx[j]],
          predicted = bayes_pred,
          avg_predicted = avg_pred,
          last_predicted = last_pred,
          actual = vals[j],
          minutes = mins[j],
          attempts = if (is_eff) denoms[j] else NA_real_
        )
      }
    }

    pred_list[[si]] <- data.table::rbindlist(match_preds)
    if (nrow(pred_list[[si]]) > 0) pred_list[[si]][, stat := sc]
  }

  all_preds <- data.table::rbindlist(pred_list, fill = TRUE)

  # Compute per-stat accuracy (RMSE for rate stats, log-loss for efficiency)
  eff_stats <- names(eff_map)
  accuracy <- all_preds[, {
    is_eff <- stat[1] %in% eff_stats

    if (is_eff) {
      # Log-loss for efficiency stats, weighted by attempts
      w <- attempts
      w[is.na(w)] <- 0
      clip <- function(x) pmax(pmin(x, 1 - 1e-8), 1e-8)
      ll <- function(y, p) -(clip(y) * log(clip(p)) + (1 - clip(y)) * log(1 - clip(p)))
      bayes_ll <- sum(w * ll(actual, predicted))
      avg_ll <- sum(w * ll(actual, avg_predicted))
      last_ll <- sum(w * ll(actual, last_predicted))
      tw <- sum(w)
      safe_pct <- function(x, ref) if (ref > 0) round((1 - x / ref) * 100, 1) else NA_real_
      list(
        metric = "logloss",
        value = if (tw > 0) bayes_ll / tw else NA_real_,
        avg_value = if (tw > 0) avg_ll / tw else NA_real_,
        last_value = if (tw > 0) last_ll / tw else NA_real_,
        pct_vs_avg = safe_pct(bayes_ll, avg_ll),
        pct_vs_last = safe_pct(bayes_ll, last_ll),
        n_predictions = .N
      )
    } else {
      # RMSE for rate stats, weighted by minutes/90
      w <- minutes / 90
      bayes_se <- sum(w * (predicted - actual)^2)
      avg_se <- sum(w * (avg_predicted - actual)^2)
      last_se <- sum(w * (last_predicted - actual)^2)
      tw <- sum(w)
      safe_pct <- function(x, ref) if (ref > 0) round((1 - x / ref) * 100, 1) else NA_real_
      list(
        metric = "rmse",
        value = if (tw > 0) sqrt(bayes_se / tw) else NA_real_,
        avg_value = if (tw > 0) sqrt(avg_se / tw) else NA_real_,
        last_value = if (tw > 0) sqrt(last_se / tw) else NA_real_,
        pct_vs_avg = safe_pct(bayes_se, avg_se),
        pct_vs_last = safe_pct(bayes_se, last_se),
        n_predictions = .N
      )
    }
  }, by = stat]

  data.table::setorder(accuracy, -pct_vs_avg)

  list(predictions = all_preds, accuracy = accuracy)
}


# ============================================================================
# Context Adjustments
# ============================================================================

#' Adjust match stats for opponent quality, venue, and league level
#'
#' Normalizes raw match-level stats to account for contextual factors before
#' they enter the skill estimation pipeline. Applying these adjustments before
#' decay-weighting produces cleaner skill estimates.
#'
#' Three adjustments are applied multiplicatively:
#' \enumerate{
#'   \item \strong{Opponent strength}: Offensive stats scaled by
#'     \code{league_avg_elo / opponent_elo}. Harder opponents inflate your stats;
#'     easier opponents deflate them.
#'   \item \strong{Home/away}: Per-stat home/away differential computed from
#'     the full dataset. Away stats multiplied up to home-equivalent.
#'   \item \strong{League quality}: Cross-league normalization using Big 5
#'     average as reference level.
#' }
#'
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}
#'   with columns: match_id, player_id, team_name, opponent_team, is_home,
#'   competition, total_minutes, and stat columns.
#' @param elo_ratings Optional named numeric vector of team Elo ratings.
#'   If NULL, opponent adjustment is skipped.
#' @param adjust_opponent Logical, apply opponent strength adjustment (default TRUE).
#' @param adjust_venue Logical, apply home/away adjustment (default TRUE).
#' @param adjust_league Logical, apply cross-league adjustment (default TRUE).
#' @param big5_leagues Character vector of Big 5 league codes for reference level.
#'
#' @return The input data.table with stat columns adjusted in place.
#'
#' @export
adjust_match_stats_for_context <- function(match_stats, elo_ratings = NULL,
                                            adjust_opponent = TRUE,
                                            adjust_venue = TRUE,
                                            adjust_league = TRUE,
                                            big5_leagues = c("EPL", "La_Liga", "Bundesliga",
                                                              "Serie_A", "Ligue_1",
                                                              "ENG", "ESP", "GER", "ITA", "FRA")) {
  dt <- data.table::copy(data.table::as.data.table(match_stats))

  # Identify stat columns to adjust (per-90 rates and efficiency stats)
  p90_cols <- grep("_p90$", names(dt), value = TRUE)
  eff_stats <- names(.classify_skill_stats())
  eff_cols <- intersect(eff_stats, names(dt))
  # Only adjust per-90 rates (efficiency stats are ratios, less affected by context)
  stat_cols <- p90_cols

  if (length(stat_cols) == 0) {
    cli::cli_warn("No stat columns found to adjust.")
    return(dt)
  }

  n_adjusted <- 0L

  # --- Opponent strength adjustment ---
  if (isTRUE(adjust_opponent) && !is.null(elo_ratings) && "opponent_team" %in% names(dt)) {
    league_avg_elo <- mean(elo_ratings, na.rm = TRUE)

    dt[, opp_elo := elo_ratings[opponent_team]]
    dt[is.na(opp_elo), opp_elo := league_avg_elo]

    # Ratio: facing weak opponent -> deflate stats, strong opponent -> inflate
    dt[, opp_factor := league_avg_elo / opp_elo]
    # Clamp to reasonable range
    dt[, opp_factor := pmax(pmin(opp_factor, 1.5), 0.67)]

    for (col in stat_cols) {
      data.table::set(dt, j = col, value = dt[[col]] * dt$opp_factor)
    }

    dt[, c("opp_elo", "opp_factor") := NULL]
    n_adjusted <- n_adjusted + 1L
    progress_msg("Applied opponent strength adjustment")
  }

  # --- Home/away adjustment ---
  if (isTRUE(adjust_venue) && "is_home" %in% names(dt)) {
    # Compute per-stat home/away mean ratio from the full dataset
    home_means <- dt[is_home == 1, lapply(.SD, mean, na.rm = TRUE), .SDcols = stat_cols]
    away_means <- dt[is_home == 0, lapply(.SD, mean, na.rm = TRUE), .SDcols = stat_cols]

    # Venue factor: away stats multiplied by (home_avg / away_avg) to normalize to home
    venue_factors <- as.numeric(home_means) / pmax(as.numeric(away_means), 1e-6)
    venue_factors <- pmax(pmin(venue_factors, 1.5), 0.67)  # clamp
    names(venue_factors) <- stat_cols

    # Only adjust away matches
    away_idx <- which(dt$is_home == 0)
    if (length(away_idx) > 0) {
      for (col in stat_cols) {
        vals <- dt[[col]]
        vals[away_idx] <- vals[away_idx] * venue_factors[col]
        data.table::set(dt, j = col, value = vals)
      }
    }

    n_adjusted <- n_adjusted + 1L
    progress_msg("Applied home/away venue adjustment")
  }

  # --- League quality adjustment ---
  if (isTRUE(adjust_league) && "competition" %in% names(dt)) {
    leagues_in_data <- unique(dt$competition)

    # Use Big 5 leagues as reference level
    big5_in_data <- intersect(leagues_in_data, big5_leagues)
    if (length(big5_in_data) > 0 && length(leagues_in_data) > 1) {
      big5_means <- dt[competition %in% big5_in_data,
                        lapply(.SD, mean, na.rm = TRUE), .SDcols = stat_cols]

      for (lg in setdiff(leagues_in_data, big5_in_data)) {
        lg_means <- dt[competition == lg, lapply(.SD, mean, na.rm = TRUE), .SDcols = stat_cols]
        lg_factors <- as.numeric(big5_means) / pmax(as.numeric(lg_means), 1e-6)
        lg_factors <- pmax(pmin(lg_factors, 1.5), 0.67)

        lg_idx <- which(dt$competition == lg)
        for (col in stat_cols) {
          vals <- dt[[col]]
          vals[lg_idx] <- vals[lg_idx] * lg_factors[match(col, stat_cols)]
          data.table::set(dt, j = col, value = vals)
        }
      }

      n_adjusted <- n_adjusted + 1L
      progress_msg(sprintf("Applied league quality adjustment (%d leagues, %d Big 5 reference)",
                            length(leagues_in_data), length(big5_in_data)))
    }
  }

  progress_msg(sprintf("Context adjustments complete (%d/3 applied)", n_adjusted))
  dt
}


# ============================================================================
# Player Skill Profiles
# ============================================================================

#' Generate a player's estimated skill profile
#'
#' Returns a clean summary of a player's current estimated skills: per-stat
#' skill estimates, league averages, percentile ranks, and confidence levels.
#' Groups stats by category for easy interpretation.
#'
#' When called without \code{match_stats} or \code{skills}, automatically loads
#' pre-computed skill estimates via \code{load_opta_skills()} (~2-3 MB download).
#' This covers all 15 Opta leagues across all available seasons.
#'
#' @param player_name Character string of the player name to profile.
#'   Supports exact names ("H. Kane"), abbreviations ("H.Kane"),
#'   surnames ("Kane"), and accent-insensitive input ("Mbappe").
#' @param match_stats A data.table from \code{compute_match_level_opta_stats()}.
#'   If provided, runs full skill estimation from raw data. When omitted (and
#'   \code{skills} is also NULL), downloads pre-computed skills instead.
#' @param decay_params Decay parameters list. Only used when
#'   \code{match_stats} is provided.
#' @param date Date to estimate skills at (default today). Only used when
#'   \code{match_stats} is provided.
#' @param min_weighted_90s Regression threshold. Only used when
#'   \code{match_stats} is provided.
#' @param skills Pre-computed skills data.table from
#'   \code{estimate_player_skills()} or \code{load_opta_skills()}. If
#'   provided, skips estimation and uses these directly.
#' @param source Data source for auto-loading: "remote" (default) or "local".
#'   Only used when both \code{match_stats} and \code{skills} are NULL.
#'
#' @return A data.table with columns: category, stat, skill, league_avg,
#'   league_pct, pos_avg, pos_pct, weighted_90s, confidence.
#'
#' @export
player_skill_profile <- function(player_name, match_stats = NULL,
                                  decay_params = NULL, date = Sys.Date(),
                                  min_weighted_90s = 5, skills = NULL,
                                  source = c("remote", "local")) {
  source <- match.arg(source)

  # Capture search target before entering data.table scope (avoid NSE shadowing)
  target_player <- player_name
  if (is.null(decay_params)) {
    # Try to load optimized per-stat decay params from skill pipeline cache
    opt_path <- file.path("data-raw", "cache-skills", "02b_decay_params.rds")
    if (file.exists(opt_path)) {
      decay_params <- readRDS(opt_path)
    } else {
      decay_params <- get_default_decay_params()
    }
  }

  # Use pre-computed skills if provided, otherwise estimate from match_stats
  if (!is.null(skills)) {
    all_skills <- data.table::as.data.table(skills)
  } else if (is.null(match_stats)) {
    # Auto-load pre-computed skills (~4 MB) and match stats (~15 MB)
    cli::cli_alert_info("Loading pre-computed skills from GitHub...")
    all_skills <- tryCatch(
      data.table::as.data.table(load_opta_skills(source = source)),
      error = function(e) {
        cli::cli_abort(c(
          "Could not load pre-computed skills.",
          "i" = "Provide {.arg match_stats} or {.arg skills} directly.",
          "x" = e$message
        ))
      }
    )
    # Also load match stats for raw_avg/attempts/w90 columns
    match_stats <- tryCatch(
      data.table::as.data.table(load_opta_match_stats(source = source)),
      error = function(e) {
        cli::cli_alert_warning("Could not load match stats: {e$message} (raw_avg/attempts will be NA).")
        NULL
      }
    )
  } else {
    dt <- data.table::as.data.table(match_stats)
    all_skills <- estimate_player_skills(
      match_stats = dt,
      decay_params = decay_params,
      target_date = date,
      min_weighted_90s = min_weighted_90s
    )
  }

  if (is.null(all_skills) || nrow(all_skills) == 0) {
    cli::cli_warn("No skill estimates available.")
    return(NULL)
  }

  # Find the target player using flexible matching:
  # 1. Exact clean match (e.g., "Harry Kane" matches "Harry Kane")
  # 2. Partial match on cleaned names (e.g., "Kane" matches "Harry Kane")
  # 3. Partial match on raw names (e.g., "H.Kane" matches "H. Kane")
  pnames <- all_skills$player_name
  all_skills[, clean_name := clean_player_name(pnames)]
  target_clean <- clean_player_name(target_player)

  # Helper: strip accents for comparison (é → e, ü → u, etc.)
  strip_accents <- function(x) iconv(x, to = "ASCII//TRANSLIT")

  # Try exact clean match first
  player_row <- all_skills[clean_name == target_clean]

  # If no exact match, try substring match on cleaned names (e.g., "Kane" matches "harrykane")
  if (nrow(player_row) == 0) {
    player_row <- all_skills[grepl(target_clean, clean_name, fixed = TRUE)]
  }

  # If still no match, retry with accents stripped (e.g., "Mbappe" matches "Mbappé")
  if (nrow(player_row) == 0) {
    ascii_target <- strip_accents(target_clean)
    ascii_names <- strip_accents(all_skills$clean_name)
    # Exact then substring
    idx <- which(ascii_names == ascii_target)
    if (length(idx) == 0) idx <- which(grepl(ascii_target, ascii_names, fixed = TRUE))
    if (length(idx) > 0) player_row <- all_skills[idx]
  }

  # If still no match, try surname-based matching (e.g., "H.Kane" → match on "Kane")
  if (nrow(player_row) == 0) {
    tokens <- strsplit(target_player, "[.\\s]+")[[1]]
    surname <- tokens[length(tokens)]
    if (nchar(surname) >= 3) {
      surname_clean <- strip_accents(clean_player_name(surname))
      ascii_names <- strip_accents(all_skills$clean_name)
      idx <- which(grepl(paste0(surname_clean, "$"), ascii_names))
      if (length(idx) > 0) player_row <- all_skills[idx]
    }
  }

  if (nrow(player_row) == 0) {
    cli::cli_warn("Player '{target_player}' not found in skill estimates.")
    return(NULL)
  }

  if (nrow(player_row) > 1) {
    # Pick the one with most weighted_90s (most data)
    player_row <- player_row[which.max(weighted_90s)]
  }

  matched_name <- player_row$player_name
  matched_pos <- player_row$primary_position
  name_str <- if (matched_name != target_player) {
    sprintf("Matched '%s' -> %s", target_player, matched_name)
  } else {
    matched_name
  }

  # Identify stat columns
  meta_cols <- c("player_id", "player_name", "primary_position", "date",
                  "weighted_90s", "clean_name", "season_end_year",
                  "total_minutes", "n_matches", "is_gk", "is_df", "is_mf",
                  "is_fw", "competition", "league")
  stat_cols <- setdiff(names(all_skills), meta_cols)
  stat_cols <- stat_cols[vapply(all_skills, is.numeric, logical(1))[stat_cols]]

  # Categorize stats
  attacking <- c("goals_p90", "shots_p90", "shots_on_target_p90", "shots_ibox_p90",
                  "big_chance_scored_p90", "assists_p90", "key_passes_p90",
                  "big_chance_created_p90", "through_balls_p90",
                  "shot_accuracy", "goals_per_shot", "big_chance_conversion")
  passing <- c("total_passes_p90", "open_play_pass_p90", "final_third_passes_p90",
                "fwd_zone_pass_p90", "crosses_p90", "crosses_open_play_p90",
                "forward_pass_p90", "chipped_pass_p90",
                "pass_accuracy", "fwd_zone_pass_accuracy", "open_play_pass_accuracy")
  defending <- c("tackles_p90", "tackles_won_p90", "interceptions_p90",
                  "clearances_p90", "blocks_p90", "aerial_won_p90",
                  "ball_recovery_p90", "poss_won_def3rd_p90",
                  "tackle_success", "aerial_success")
  possession <- c("touches_p90", "touches_opp_box_p90", "pen_area_entries_p90",
                   "final_third_entries_p90", "successful_dribbles_p90",
                   "poss_lost_ctrl_p90")

  categorize <- function(stat) {
    if (stat %in% attacking) return("Attacking")
    if (stat %in% passing) return("Passing")
    if (stat %in% defending) return("Defending")
    if (stat %in% possession) return("Possession")
    if (grepl("^(xg|npxg|xa|xpass)", stat)) return("xMetrics")
    "Other"
  }

  # Get player's position group and same-position peers
  player_pos <- player_row$primary_position
  pos_peers <- all_skills[primary_position == player_pos]

  # Compute raw career stats and per-stat weighted 90s from match_stats
  player_id_val <- player_row$player_id
  raw_avgs <- NULL
  stat_w90s <- NULL
  stat_raw_attempts <- NULL
  stat_w_attempts <- NULL
  total_90s <- NA_real_
  n_matches_val <- NA_integer_
  if (!is.null(match_stats)) {
    dt <- data.table::as.data.table(match_stats)
    if (!inherits(dt$match_date, "Date")) dt[, match_date := as.Date(match_date)]
    player_matches <- dt[player_id == player_id_val]
    if (nrow(player_matches) > 0) {
      n_matches_val <- nrow(player_matches)
      total_90s <- sum(player_matches$total_minutes, na.rm = TRUE) / 90

      # Raw average per stat
      raw_avgs <- vapply(stat_cols, function(s) {
        if (s %in% names(player_matches)) {
          mean(player_matches[[s]], na.rm = TRUE)
        } else NA_real_
      }, numeric(1))

      # Per-stat effective sample size using each stat's own lambda
      ref_date <- if (!is.null(date)) as.Date(date) else max(player_matches$match_date)
      days_since <- as.numeric(ref_date - player_matches$match_date)
      mins_90 <- player_matches$total_minutes / 90
      eff_map <- .classify_skill_stats()
      compute_denominator <- .compute_denominator

      resolve_lambda <- function(stat_name) {
        if (stat_name %in% names(decay_params)) return(decay_params[[stat_name]])
        if (stat_name %in% names(eff_map)) return(decay_params$efficiency)
        if (grepl("^(xg|npxg|xa|xpass)", stat_name)) return(decay_params$xmetrics)
        decay_params$rate
      }

      # Classify each stat and compute appropriate effective sample size
      stat_types <- vapply(stat_cols, function(s) {
        if (s %in% names(eff_map)) "efficiency" else "rate"
      }, character(1))

      stat_w90s <- vapply(stat_cols, function(s) {
        lam <- resolve_lambda(s)
        w <- exp(-lam * days_since)
        if (s %in% names(eff_map)) {
          NA_real_  # w90 not meaningful for efficiency stats
        } else {
          round(sum(w * mins_90, na.rm = TRUE), 1)
        }
      }, numeric(1))

      # Raw (unweighted) attempts per efficiency stat
      # tryCatch handles missing denominator columns (e.g., fifty_fifty
      # may not exist in all data sources)
      stat_raw_attempts <- vapply(stat_cols, function(s) {
        if (s %in% names(eff_map)) {
          tryCatch({
            denom <- compute_denominator(player_matches, eff_map[[s]])
            round(sum(denom, na.rm = TRUE), 0)
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      }, numeric(1))

      stat_w_attempts <- vapply(stat_cols, function(s) {
        lam <- resolve_lambda(s)
        w <- exp(-lam * days_since)
        if (s %in% names(eff_map)) {
          tryCatch({
            denom <- compute_denominator(player_matches, eff_map[[s]])
            round(sum(w * denom, na.rm = TRUE), 0)
          }, error = function(e) NA_real_)
        } else {
          NA_real_
        }
      }, numeric(1))
    }
  }

  # Classify stats
  eff_map <- .classify_skill_stats()
  stat_types <- vapply(stat_cols, function(s) {
    if (s %in% names(eff_map)) "efficiency" else "rate"
  }, character(1))

  # Build profile table
  profile <- data.table::data.table(
    category = vapply(stat_cols, categorize, character(1)),
    stat = stat_cols,
    type = stat_types,
    skill = as.numeric(player_row[, stat_cols, with = FALSE]),
    raw_avg = if (!is.null(raw_avgs)) raw_avgs else NA_real_,
    league_avg = vapply(stat_cols, function(s) mean(all_skills[[s]], na.rm = TRUE), numeric(1)),
    league_pct = vapply(stat_cols, function(s) {
      round(100 * mean(all_skills[[s]] <= player_row[[s]], na.rm = TRUE), 1)
    }, numeric(1)),
    pos_avg = vapply(stat_cols, function(s) mean(pos_peers[[s]], na.rm = TRUE), numeric(1)),
    pos_pct = vapply(stat_cols, function(s) {
      round(100 * mean(pos_peers[[s]] <= player_row[[s]], na.rm = TRUE), 1)
    }, numeric(1)),
    n90 = if (!is.na(total_90s)) ifelse(stat_types == "rate", round(total_90s, 1), NA_real_) else NA_real_,
    w90 = if (!is.null(stat_w90s)) stat_w90s else NA_real_,
    attempts = if (!is.null(stat_raw_attempts)) stat_raw_attempts else NA_real_,
    w_attempts = if (!is.null(stat_w_attempts)) stat_w_attempts else NA_real_
  )

  # Print player summary
  info_parts <- sprintf("%s | %s", name_str, matched_pos)
  if (!is.na(total_90s)) {
    info_parts <- sprintf("%s | %d matches, %.0f 90s", info_parts, n_matches_val, total_90s)
  }
  progress_msg(info_parts)

  # Sort by category then position percentile
  category_order <- c("Attacking", "Passing", "Defending", "Possession", "xMetrics", "Other")
  profile[, category := factor(category, levels = category_order)]
  data.table::setorder(profile, category, -pos_pct)

  # Round
  profile[, skill := round(skill, 4)]
  profile[, raw_avg := round(raw_avg, 4)]
  profile[, league_avg := round(league_avg, 4)]
  profile[, pos_avg := round(pos_avg, 4)]

  profile[]
}


# ============================================================================
# Internal helpers
# ============================================================================

#' Extract season end year from season string
#' @param season Character vector of season strings
#' @return Integer vector of end years
#' @keywords internal
.extract_end_year <- function(season) {
  vapply(season, function(s) {
    # "2024-2025" -> 2025
    if (grepl("^\\d{4}-\\d{4}$", s)) return(as.integer(substr(s, 6, 9)))
    # "2018 Russia" -> 2018
    year <- suppressWarnings(as.integer(sub("^(\\d{4}).*", "\\1", s)))
    if (!is.na(year)) return(year)
    NA_integer_
  }, integer(1), USE.NAMES = FALSE)
}
