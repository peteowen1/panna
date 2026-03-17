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

#' Estimate pre-match skills at multiple dates
#'
#' Calls \code{\link{estimate_player_skills}} at each reference date in
#' chronological order, using all match data strictly before each date.
#' This mirrors torpverse's \code{.estimate_skills_batch()} approach.
#'
#' @param match_stats Match-level stats (output of
#'   \code{compute_match_level_opta_stats}).
#' @param ref_dates Character or Date vector of dates to estimate skills at.
#'   Typically unique match dates from the training data.
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

  for (i in seq_along(ref_dates)) {
    rd <- ref_dates[i]

    if (verbose && (i %% 50 == 0 || i == 1 || i == n_dates)) {
      progress_msg(sprintf("  Date %d/%d: %s", i, n_dates, rd))
    }

    results[[i]] <- tryCatch(
      estimate_player_skills(
        match_stats = match_stats,
        decay_params = decay_params,
        target_date = rd,
        min_weighted_90s = min_weighted_90s
      ),
      error = function(e) {
        if (verbose) {
          cli::cli_warn("Skills estimation failed for {rd}: {e$message}")
        }
        NULL
      }
    )
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
#' @export
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
#' @export
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
#' @export
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
#' Loads pre-computed skill estimates and applies PSR coefficients to produce
#' a ranked table of player ratings. PSR predicts each player's contribution
#' to xG differential based on their Bayesian skill profile.
#'
#' @param season Season filter as end year (e.g., 2025 for 2024-2025).
#'   If NULL, returns all available seasons.
#' @param league League code filter (e.g., \code{"ENG"}). Ignored for now
#'   since skills are cross-league.
#' @param n Number of top players to show (default 50, NULL for all).
#' @param position Filter by position group: \code{"GK"}, \code{"DEF"},
#'   \code{"MID"}, \code{"FWD"}, or NULL for all.
#' @param target One of \code{"xg"} (default, xG differential) or
#'   \code{"goals"} (goal differential).
#' @param source Data source: \code{"remote"} (default, GitHub Releases) or
#'   \code{"local"}.
#'
#' @return A data.table with columns: \code{player_name}, \code{primary_position},
#'   \code{psr}, \code{osr}, \code{dsr}, \code{weighted_90s}, and key skill
#'   columns for context.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Top 50 players in 2024-2025 (xG-based PSR)
#' player_psr(season = 2025)
#'
#' # Top midfielders
#' player_psr(season = 2025, position = "MID")
#'
#' # Goal-differential-based PSR
#' player_psr(season = 2025, target = "goals")
#' }
player_psr <- function(season = NULL, league = NULL, n = 50,
                        position = NULL, target = c("xg", "goals"),
                        source = c("remote", "local")) {
  source <- match.arg(source)
  target <- match.arg(target)

  # Load skills
  skills <- load_opta_skills(season = season, source = source)
  if (is.null(skills) || nrow(skills) == 0) {
    cli::cli_abort("No skill data available for season {.val {season}}")
  }

  # Compute PSR
  psr <- compute_player_psr(skills, center = TRUE, target = target)

  # Filter by position
  if (!is.null(position)) {
    position <- toupper(position)
    if ("primary_position" %in% names(psr)) {
      pos_map <- c(
        GK = "Goalkeeper", DEF = "Defender", MID = "Midfielder", FWD = "Striker"
      )
      if (position %in% names(pos_map)) {
        psr <- psr[grepl(pos_map[position], primary_position, ignore.case = TRUE)]
      }
    }
  }

  # Add key skill columns for context
  context_cols <- c(
    "goals_p90", "assists_p90", "xg_per90", "key_passes_p90",
    "tackles_won_p90", "interceptions_p90", "pass_accuracy",
    "duel_success", "touches_p90"
  )
  context_available <- intersect(context_cols, names(skills))
  if (length(context_available) > 0) {
    skill_dt <- data.table::as.data.table(skills)
    context_data <- skill_dt[, c("player_id", context_available), with = FALSE]
    psr <- context_data[psr, on = "player_id"]
  }

  # Sort by PSR descending
  data.table::setorder(psr, -psr)

  # Limit output
  if (!is.null(n) && nrow(psr) > n) {
    psr <- psr[seq_len(n)]
  }

  # Round numeric columns for display
  num_cols <- names(psr)[vapply(psr, is.numeric, logical(1))]
  display_cols <- setdiff(num_cols, c("season_end_year", "total_minutes", "n_matches"))
  for (col in display_cols) {
    data.table::set(psr, j = col, value = round(psr[[col]], 3))
  }

  psr
}
