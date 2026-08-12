# Match Prediction Functions
#
# Reusable functions for predicting match outcomes using player ratings,
# rolling form features, and Elo ratings. Supports XGBoost Poisson models
# for goal counts and multinomial models for W/D/L probabilities.


# =============================================================================
# Team Rating Aggregation
# =============================================================================

#' Augment a ratings table with time-decayed historical fallback
#'
#' For each player_id, finds their MOST recent non-zero rated season. If
#' that season is older than `current_sey`, applies an exponential decay
#' `decay_factor ^ years_gap` to the rating and emits a synthetic row
#' under `current_sey`. This lets unrated-in-current-season players (who
#' moved to a non-covered league like Saudi PL / MLS / Liga MX) still
#' contribute a sensible non-zero panna estimate when their lineup is
#' aggregated.
#'
#' @param ratings Data.table with at least `player_id`, `season_end_year`,
#'   `panna`, `offense`, `defense` (and optionally `spm`).
#' @param current_sey The season we want imputed rows for (e.g., 2026).
#' @param decay_factor Per-year decay. Default 0.85 (15% decline per year
#'   away from a player's last-rated season -- accounts for both ageing and
#'   uncertainty about their current form).
#' @param max_years_back Cap how far back to look. Default 5 years.
#'   Beyond that the decay is so heavy the imputation is near-zero anyway.
#' @return The input `ratings` with extra synthetic rows for `current_sey`
#'   covering players who weren't already rated there.
#' @keywords internal
augment_ratings_with_history <- function(ratings, current_sey,
                                            decay_factor = 0.85,
                                            max_years_back = 5L) {
  dt <- data.table::as.data.table(ratings)

  ## Players already rated in current_sey -- leave them alone
  curr_ids <- dt[season_end_year == current_sey &
                  !is.na(panna) & panna != 0, unique(player_id)]

  ## For everyone else: most recent rated season within max_years_back
  candidates <- dt[!player_id %in% curr_ids &
                    season_end_year >= current_sey - max_years_back &
                    season_end_year < current_sey &
                    !is.na(panna) & panna != 0]
  if (nrow(candidates) == 0L) return(dt)
  data.table::setorder(candidates, player_id, -season_end_year)
  most_recent <- candidates[, .SD[1L], by = player_id]
  most_recent[, years_gap := current_sey - season_end_year]
  most_recent[, decay := decay_factor ^ years_gap]

  ## Decay panna + components
  for (col in intersect(c("panna","offense","defense","spm"), names(most_recent))) {
    most_recent[, (col) := get(col) * decay]
  }
  ## Stamp as current_sey so downstream lookups find them
  most_recent[, season_end_year := current_sey]
  ## Tag for diagnostics (won't break callers -- extra column)
  most_recent[, imputed_from_history := TRUE]
  most_recent[, c("years_gap","decay") := NULL]

  ## Append to original
  data.table::rbindlist(list(dt, most_recent), fill = TRUE, use.names = TRUE)
}

#' Aggregate Player Ratings to Team Level
#'
#' For a given match, takes the starting XI from lineups and joins to seasonal
#' player ratings (xRAPM/SPM/RAPM). Computes team-level summary statistics
#' including sum, mean, max, min, stdev, goalkeeper, and positional group averages.
#'
#' @param lineups Data frame of match lineups with player_name, team_name,
#'   team_position (home/away), position, is_starter columns
#' @param ratings Data frame of seasonal player ratings with player_name,
#'   season_end_year, panna, offense, defense, spm columns
#' @param season_end_year Numeric season end year for rating lookup
#' @param prev_season_decay Decay factor for previous season fallback (default 0.8)
#'
#' @return Data frame with one row per match, team-level rating features
#' @family match prediction
#' @export
aggregate_lineup_ratings <- function(lineups, ratings, season_end_year,
                                      prev_season_decay = 0.8) {
  dt_lineups <- data.table::as.data.table(lineups)
  dt_ratings <- data.table::as.data.table(ratings)

  # Local copies to avoid data.table .. scoping issues
  sey_curr <- season_end_year
  sey_prev <- season_end_year - 1L

  # Filter to starters only
  starters <- dt_lineups[is_starter == TRUE]

  # Honour optional lineup_weight column (0..1; sum across a side ~= 11).
  # When absent, default to 1.0 so the non-override path stays unchanged --
  # 11 starters at weight 1 reproduces the previous sum/mean semantics.
  # Used by the WC2026 announced-squad override (step 02 / 02b) to pass
  # all 26 players weighted by expected minutes per match.
  if (!"lineup_weight" %in% names(starters)) {
    starters[, lineup_weight := 1.0]
  } else {
    starters[is.na(lineup_weight) | lineup_weight < 0, lineup_weight := 1.0]
  }

  # Determine join key: use player_id when both sides have it and IDs overlap
  has_id_both <- "player_id" %in% names(dt_lineups) &&
    "player_id" %in% names(dt_ratings)
  use_id_join <- FALSE
  if (has_id_both) {
    if (any(dt_lineups$player_id %in% dt_ratings$player_id, na.rm = TRUE)) {
      use_id_join <- TRUE
    } else {
      cli::cli_warn(c(
        "Both lineups and ratings have {.field player_id} but 0 IDs overlap.",
        "i" = "Lineups: {length(unique(dt_lineups$player_id))} unique, Ratings: {length(unique(dt_ratings$player_id))} unique.",
        "!" = "Falling back to name-based join."
      ))
    }
  }

  if (use_id_join) {
    join_key <- "player_id"
  } else {
    join_key <- "clean_name"
    starters[, clean_name := clean_player_name(player_name)]
    dt_ratings <- data.table::copy(dt_ratings)
    dt_ratings[, clean_name := clean_player_name(player_name)]
  }

  # Current season ratings. Carry through any value-metric columns that are
  # present in dt_ratings so the downstream aggregator (has_epr / has_psr /
  # has_wpa / has_psv checks in this function) can pick them up. Without
  # this, those columns got silently dropped before the aggregator saw them,
  # so sum_epr / sum_psr / etc were never created in team-level features.
  core_cols <- c("panna", "offense", "defense", "spm")
  known_optional <- c("epr", "epr_offensive", "epr_defensive",
                       "psr", "osr", "dsr",
                       "wpa_rating", "psv_rating",
                       "centrality")
  optional_cols <- intersect(known_optional, names(dt_ratings))
  # Warn if any expected optional cols are NOT in dt_ratings -- catches the
  # pre-2026-05-19 silent-drop pattern where downstream `has_psr`/`has_epr`
  # checks in this function would always evaluate FALSE, so no team-level
  # PSR/EPR features ever got created. If we expect these columns to be
  # passed in but they're missing, surface that loudly.
  missing_optional <- setdiff(known_optional, names(dt_ratings))
  if (length(missing_optional) > 0 && getOption("panna.verbose_ratings", FALSE)) {
    cli::cli_warn(c(
      "Ratings table is missing optional value-metric columns: {.field {missing_optional}}",
      "i" = "Set {.code options(panna.verbose_ratings = FALSE)} to silence."
    ))
  }
  rating_cols <- c(join_key, core_cols, optional_cols)
  curr <- dt_ratings[season_end_year == sey_curr, rating_cols, with = FALSE]
  curr <- curr[!duplicated(curr[[join_key]])]

  # Previous season ratings (fallback with decay)
  prev <- dt_ratings[season_end_year == sey_prev, rating_cols, with = FALSE]
  prev <- prev[!duplicated(prev[[join_key]])]
  decay_cols <- c(core_cols, optional_cols)
  for (c in decay_cols) {
    if (c %in% names(prev)) prev[, (c) := get(c) * prev_season_decay]
  }
  # Rename to *_prev so we can fall back without column-name collisions
  prev_renamed <- paste0(decay_cols, "_prev")
  data.table::setnames(prev, decay_cols, prev_renamed)

  # Join current ratings
  starters <- curr[starters, on = join_key]
  # Fallback to previous season
  starters <- prev[starters, on = join_key]
  for (c in decay_cols) {
    prev_c <- paste0(c, "_prev")
    if (c %in% names(starters) && prev_c %in% names(starters)) {
      starters[is.na(get(c)), (c) := get(prev_c)]
    }
  }

  # Capture pre-imputation rated status BEFORE the imputation block below
  # rewrites NA panna with team-mean estimates. The post-imputation
  # `sum(panna != 0)` count was effectively "always 11" because almost every
  # NA was filled with a non-zero shrunk-mean -- making `n_rated_players`
  # near-constant and the feature's name a lie. `was_rated` captures the
  # honest signal: did this player have a real current-or-previous-season
  # panna rating? (TRUE iff the join in the block above found one.)
  starters[, was_rated := !is.na(panna)]

  # Coverage-shrunk team-mean imputation for unrated players
  #
  # Why: a Brazil player without a panna rating (e.g. Brasileir\u00e3o-based, not
  # in our European-focused RAPM pipeline) used to get panna = 0, which
  # systematically depressed `home_sum_panna` for low-coverage countries.
  # Naive team-mean imputation overshoots in the opposite direction: Saudi
  # Arabia with only 1 rated player (a top star) would impute 25 others at
  # that star's panna, inflating the whole team.
  #
  # Shrinkage formula: imputed = team_mean * coverage / (coverage + SHRINK_K)
  #
  # - coverage = n_rated_starters / n_total_starters per team (across the
  #   season's matches passed in)
  # - SHRINK_K = 0.3 -- when coverage equals k, imputation gets half the
  #   team mean (a Bayesian-flavoured shrinkage toward 0/replacement)
  # - Brazil (cov ~0.7): imputed ~= 0.70 * team_mean (mostly trust team mean)
  # - Saudi  (cov ~0.04): imputed ~= 0.12 * team_mean (mostly shrink to 0)
  # - Qatar  (cov 0):    imputed = 0 (final fallback below handles)
  ## Increased from 0.3 to 0.6: at 0.3, USA (50% coverage) imputed unrated
  ## MLS players at 62% of team mean -- selection bias because rated 13 are
  ## all Europe-based stars. At 0.6, USA shrinks to 45% -- more honest.
  SHRINK_K <- 0.6
  # All numeric rating columns get coverage-shrunk team-mean imputation:
  # core (panna/offense/defense/spm) AND optional value metrics (EPR/PSR/etc.).
  # Without this, non-European teams whose players lack EPR/PSR snapshots
  # have sum_epr / sum_psr biased low because missing values fall back to 0,
  # dragging team totals down vs full-coverage European squads.
  shrink_cols <- intersect(c("panna","offense","defense","spm",
                              "epr","epr_offensive","epr_defensive",
                              "psr","osr","dsr"),
                             names(starters))
  if ("team_id" %in% names(starters) && length(shrink_cols) > 0) {
    # Compute per-team count + per-column means via .SDcols
    team_stats <- starters[, c(
      list(team_n_total = .N, team_n_rated = sum(!is.na(panna))),
      lapply(.SD, function(x) mean(x, na.rm = TRUE))
    ), by = team_id, .SDcols = shrink_cols]
    avg_cols <- paste0("team_avg_", shrink_cols)
    data.table::setnames(team_stats, shrink_cols, avg_cols)
    team_stats[, coverage := team_n_rated / team_n_total]
    team_stats[, shrink_w := coverage / (coverage + SHRINK_K)]
    for (c in avg_cols) team_stats[, (c) := get(c) * shrink_w]
    starters <- team_stats[, c("team_id", avg_cols), with = FALSE][
                  starters, on = "team_id"]
    for (i in seq_along(shrink_cols)) {
      c <- shrink_cols[i]; avg_c <- avg_cols[i]
      starters[is.na(get(c)) & !is.na(get(avg_c)), (c) := get(avg_c)]
    }
    starters[, (avg_cols) := NULL]
  }
  # Final fallback: anything still NA becomes 0
  for (c in shrink_cols) starters[is.na(get(c)), (c) := 0]

  # Classify positions
  starters[, pos_group := data.table::fcase(
    grepl("goalkeeper", tolower(position)), "gk",
    grepl("defender", tolower(position)), "def",
    grepl("midfielder", tolower(position)), "mid",
    grepl("forward|striker", tolower(position)), "fwd",
    default = "mid"
  )]

  # Aggregate per match-side. Weighted by `lineup_weight` so the WC2026
  # override (26-man squad, weights = expected_minutes/90 summing to ~11)
  # produces team features on the same numeric scale as the non-override
  # path (11 starters * weight 1, also summing to 11).
  #
  # max/min/stdev and n_rated stay UNWEIGHTED: they describe the squad
  # (best-player-in-the-team, spread), not pitch-time contribution.
  team_stats <- starters[, {
    w <- lineup_weight
    w_sum <- sum(w, na.rm = TRUE)

    # Honest pre-imputation count (see was_rated assignment above). The
    # post-imputation `sum(panna != 0)` was ~always 11 and useless as a
    # feature; this is what the column name promises.
    n_rated <- if (exists("was_rated", inherits = FALSE)) sum(was_rated) else sum(panna != 0)
    # Tie-break for GK pick: explicit ordering so the choice is
    # deterministic and meaningful, not dependent on row order across two
    # unrelated parquets. Priority:
    #   1. is_starter_pred == TRUE  (announced-squad path marks the
    #      predicted starter)
    #   2. largest lineup_weight    (override: highest EM share)
    #   3. largest panna            (final tie-break: best GK on paper)
    gk_idx <- which(pos_group == "gk")
    gk_panna_val <- if (length(gk_idx) > 0) {
      gk_priority <- if (exists("is_starter_pred", inherits = FALSE)) {
        as.integer(is_starter_pred[gk_idx])
      } else {
        rep(0L, length(gk_idx))
      }
      gk_order <- order(-gk_priority, -w[gk_idx], -panna[gk_idx])
      panna[gk_idx[gk_order[1L]]]
    } else 0

    def_idx <- pos_group == "def"
    mid_idx <- pos_group == "mid"
    fwd_idx <- pos_group == "fwd"

    # Weighted mean over a boolean mask. Returns 0 when the mask is empty,
    # all values are NA, or total weight in the mask is 0 -- matching the
    # previous `safe_mean` semantics.
    wmean <- function(x, mask) {
      if (sum(mask) == 0 || all(is.na(x[mask]))) return(0)
      wm <- sum(w[mask], na.rm = TRUE)
      if (wm == 0) return(0)
      sum(w[mask] * x[mask], na.rm = TRUE) / wm
    }

    # Value metric columns (included only when ALL bare-referenced columns
    # for that block are present). Earlier the EPR block bare-referenced
    # epr_offensive / epr_defensive while only gating on `epr` -- one column
    # trim upstream would crash the aggregator inside a data.table NSE block
    # with a useless error. Same logic for PSR (osr/dsr handled by per-block
    # if() guards below).
    has_epr <- all(c("epr", "epr_offensive", "epr_defensive") %in% names(.SD))
    has_wpa <- "wpa_rating" %in% names(.SD)
    has_psv <- "psv_rating" %in% names(.SD)
    has_psr <- "psr" %in% names(.SD)
    has_centrality <- "centrality" %in% names(.SD)

    base <- list(
      sum_panna = sum(w * panna), sum_offense = sum(w * offense),
      sum_defense = sum(w * defense), sum_spm = sum(w * spm),
      avg_panna = if (w_sum > 0) sum(w * panna) / w_sum else 0,
      max_panna = max(panna), min_panna = min(panna),
      stdev_panna = if (.N > 1) stats::sd(panna) else 0,
      gk_panna = gk_panna_val,
      avg_def_panna = wmean(panna, def_idx),
      avg_mid_panna = wmean(panna, mid_idx),
      avg_fwd_panna = wmean(panna, fwd_idx),
      avg_def_offense = wmean(offense, def_idx),
      avg_def_defense = wmean(defense, def_idx),
      avg_mid_offense = wmean(offense, mid_idx),
      avg_mid_defense = wmean(defense, mid_idx),
      avg_fwd_offense = wmean(offense, fwd_idx),
      avg_fwd_defense = wmean(defense, fwd_idx),
      n_rated_players = n_rated
    )

    # EPR features (from EPV-based ratings)
    if (has_epr) {
      base$sum_epr <- sum(w * epr)
      base$sum_epr_off <- sum(w * epr_offensive)
      base$sum_epr_def <- sum(w * epr_defensive)
    }

    # WPA rating features
    if (has_wpa) {
      base$sum_wpa <- sum(w * wpa_rating)
    }

    # PSV rating features
    if (has_psv) {
      base$sum_psv <- sum(w * psv_rating)
    }

    # Centrality features (opponent quality adjustment)
    if (has_centrality) {
      base$avg_centrality <- if (w_sum > 0) sum(w * centrality) / w_sum else 0
      base$min_centrality <- min(centrality)
    }

    # PSR features (Player Skill Rating with O/D decomposition)
    if (has_psr) {
      base$sum_psr <- sum(w * psr)
      if ("osr" %in% names(.SD)) {
        base$sum_osr <- sum(w * osr)
        base$avg_def_osr <- wmean(osr, def_idx)
        base$avg_mid_osr <- wmean(osr, mid_idx)
        base$avg_fwd_osr <- wmean(osr, fwd_idx)
      }
      if ("dsr" %in% names(.SD)) {
        base$sum_dsr <- sum(w * dsr)
        base$avg_def_dsr <- wmean(dsr, def_idx)
        base$avg_mid_dsr <- wmean(dsr, mid_idx)
        base$avg_fwd_dsr <- wmean(dsr, fwd_idx)
      }
    }

    base
  }, by = .(match_id, team_name, team_position)]

  # Pivot to wide (home_ / away_ prefix)
  home <- team_stats[tolower(team_position) == "home"]
  away <- team_stats[tolower(team_position) == "away"]

  rating_cols <- setdiff(names(team_stats), c("match_id", "team_name", "team_position"))
  data.table::setnames(home, rating_cols, paste0("home_", rating_cols))
  data.table::setnames(away, rating_cols, paste0("away_", rating_cols))

  result <- home[away, on = "match_id", nomatch = NULL]

  # Add differentials
  result[, panna_diff := home_sum_panna - away_sum_panna]
  result[, offense_diff := home_sum_offense - away_sum_offense]
  result[, defense_diff := home_sum_defense - away_sum_defense]
  result[, spm_diff := home_sum_spm - away_sum_spm]

  # Value metric differentials (if available)
  if ("home_sum_epr" %in% names(result)) {
    result[, epr_diff := home_sum_epr - away_sum_epr]
    result[, epr_off_diff := home_sum_epr_off - away_sum_epr_off]
    result[, epr_def_diff := home_sum_epr_def - away_sum_epr_def]
  }
  if ("home_sum_wpa" %in% names(result)) {
    result[, wpa_diff := home_sum_wpa - away_sum_wpa]
  }
  if ("home_sum_psv" %in% names(result)) {
    result[, psv_diff := home_sum_psv - away_sum_psv]
  }
  if ("home_sum_psr" %in% names(result)) {
    result[, psr_diff := home_sum_psr - away_sum_psr]
  }
  if ("home_sum_osr" %in% names(result)) {
    result[, osr_diff := home_sum_osr - away_sum_osr]
  }
  if ("home_sum_dsr" %in% names(result)) {
    result[, dsr_diff := home_sum_dsr - away_sum_dsr]
  }
  if ("home_avg_centrality" %in% names(result)) {
    result[, centrality_diff := home_avg_centrality - away_avg_centrality]
  }

  # Clean up team_position columns
  cols_to_drop <- grep("team_position|team_name", names(result), value = TRUE)
  result[, (cols_to_drop) := NULL]

  data.table::setDF(result)
  result
}


# =============================================================================
# Team-Level Skill Feature Aggregation
# =============================================================================

#' Aggregate Player Skills to Team-Level Features
#'
#' For each match, takes the starting XI and their skill estimates, then
#' aggregates key skills to team level. Produces granular skill features
#' (e.g., team average shooting skill, team average tackling skill) that
#' give the XGBoost model richer signal beyond a single panna rating.
#'
#' @param lineups Data frame of match lineups with player_name, team_name,
#'   team_position (home/away), position, is_starter columns
#' @param skill_estimates Data frame from \code{estimate_player_skills()} with
#'   player_name and per-stat skill columns (e.g., goals_p90, tackles_won_p90)
#' @param attacking_stats Character vector of attacking skill columns to aggregate
#' @param defensive_stats Character vector of defensive skill columns to aggregate
#'
#' @return Data frame with one row per match, team-level skill features
#' @family match prediction
#' @export
aggregate_lineup_skills <- function(lineups, skill_estimates,
                                     attacking_stats = NULL,
                                     defensive_stats = NULL) {
  dt_lineups <- data.table::as.data.table(lineups)
  dt_skills <- data.table::copy(data.table::as.data.table(skill_estimates))

  # Default stat groups
  if (is.null(attacking_stats)) {
    attacking_stats <- c("goals_p90", "shots_p90", "shots_on_target_p90",
                          "key_passes_p90", "assists_p90", "big_chance_created_p90",
                          "touches_opp_box_p90", "crosses_p90")
  }
  if (is.null(defensive_stats)) {
    defensive_stats <- c("tackles_won_p90", "interceptions_p90", "clearances_p90",
                          "blocks_p90", "aerial_won_p90", "ball_recovery_p90")
  }

  # Filter to columns that exist in the data
  att_orig <- attacking_stats
  def_orig <- defensive_stats
  attacking_stats <- intersect(attacking_stats, names(dt_skills))
  defensive_stats <- intersect(defensive_stats, names(dt_skills))
  att_dropped <- setdiff(att_orig, attacking_stats)
  def_dropped <- setdiff(def_orig, defensive_stats)
  if (length(att_dropped) > 0) {
    cli::cli_warn("Dropped unknown attacking stats: {paste(att_dropped, collapse = ', ')}")
  }
  if (length(def_dropped) > 0) {
    cli::cli_warn("Dropped unknown defensive stats: {paste(def_dropped, collapse = ', ')}")
  }
  all_stats <- c(attacking_stats, defensive_stats)
  if (length(all_stats) == 0) {
    available <- setdiff(names(dt_skills), c("player_name", "clean_name"))
    cli::cli_abort(c(
      "No skill stat columns found in {.arg skill_estimates}.",
      "i" = "Available columns: {paste(head(available, 10), collapse = ', ')}{if (length(available) > 10) ', ...' else ''}"
    ))
  }

  # Filter to starters
  starters <- dt_lineups[is_starter == TRUE]
  starters[, clean_name := clean_player_name(player_name)]

  # Honour optional lineup_weight column (see aggregate_lineup_ratings for
  # rationale). Default 1.0 keeps non-override behaviour identical.
  if (!"lineup_weight" %in% names(starters)) {
    starters[, lineup_weight := 1.0]
  } else {
    starters[is.na(lineup_weight) | lineup_weight < 0, lineup_weight := 1.0]
  }

  # Join skills
  dt_skills[, clean_name := clean_player_name(player_name)]
  skill_lookup <- dt_skills[!duplicated(clean_name), c("clean_name", all_stats), with = FALSE]
  starters <- skill_lookup[starters, on = "clean_name"]

  # Warn about unmatched players
  n_unmatched <- sum(is.na(starters[[all_stats[1]]]))
  if (n_unmatched > 0) {
    n_total <- nrow(starters)
    cli::cli_warn("{n_unmatched}/{n_total} starters had no matching skill estimates (filled with 0).")
  }

  # Aggregate by match + side (minute-weighted)
  team_skills <- starters[, {
    w <- lineup_weight
    w_sum <- sum(w, na.rm = TRUE)
    result <- list()
    stat_means <- numeric(length(all_stats))
    for (j in seq_along(all_stats)) {
      stat <- all_stats[j]
      vals <- .SD[[stat]]
      vals[is.na(vals)] <- 0
      prefix <- if (stat %in% attacking_stats) "sk_att" else "sk_def"
      col_name <- paste0(prefix, "_", sub("_p90$", "", stat))
      stat_means[j] <- if (w_sum > 0) sum(w * vals) / w_sum else 0
      result[[col_name]] <- stat_means[j]
    }
    # Composites: mean-of-means (equal stat weighting, not pooled across all player-stat values)
    att_idx <- which(all_stats %in% attacking_stats)
    def_idx <- which(all_stats %in% defensive_stats)
    if (length(att_idx) > 0) {
      result[["sk_att_composite"]] <- mean(stat_means[att_idx])
    }
    if (length(def_idx) > 0) {
      result[["sk_def_composite"]] <- mean(stat_means[def_idx])
    }
    result
  }, by = .(match_id, team_name, team_position), .SDcols = all_stats]

  # Pivot to wide (home_ / away_ prefix)
  home <- team_skills[tolower(team_position) == "home"]
  away <- team_skills[tolower(team_position) == "away"]

  skill_cols <- setdiff(names(team_skills), c("match_id", "team_name", "team_position"))
  data.table::setnames(home, skill_cols, paste0("home_", skill_cols))
  data.table::setnames(away, skill_cols, paste0("away_", skill_cols))

  result <- home[away, on = "match_id", nomatch = NULL]

  # Add differentials
  if ("home_sk_att_composite" %in% names(result)) {
    result[, sk_att_diff := home_sk_att_composite - away_sk_att_composite]
  }
  if ("home_sk_def_composite" %in% names(result)) {
    result[, sk_def_diff := home_sk_def_composite - away_sk_def_composite]
  }

  # Clean up
  cols_to_drop <- grep("team_position|team_name", names(result), value = TRUE)
  result[, (cols_to_drop) := NULL]

  data.table::setDF(result)
  result
}


# =============================================================================
# Elo Rating System
# =============================================================================

#' Initialize Team Elo Ratings
#'
#' Creates a named vector of initial Elo ratings for all teams.
#' Filters NA team names defensively -- they would otherwise create an
#' NA-named entry that `NA %in% names(elos)` returns TRUE for, opening
#' the door to NA cascades when bad upstream data sneaks through.
#'
#' @param teams Character vector of team names
#' @param initial_elo Starting Elo rating (default 1500)
#'
#' @return Named numeric vector of Elo ratings (one entry per non-NA team)
#' @family match prediction
#' @export
init_team_elos <- function(teams, initial_elo = 1500) {
  teams <- teams[!is.na(teams)]
  elos <- rep(initial_elo, length(teams))
  names(elos) <- teams
  elos
}


#' Update Elo Ratings After a Match
#'
#' Updates Elo ratings for home and away teams based on match result.
#' Uses standard Elo formula with configurable K-factor and home advantage.
#'
#' @param home_elo Current home team Elo
#' @param away_elo Current away team Elo
#' @param home_goals Goals scored by home team
#' @param away_goals Goals scored by away team
#' @param k K-factor controlling update magnitude (default 20)
#' @param home_advantage Home advantage in Elo points (default 88)
#' @param update_mode "outcome" (default) = W/D/L surprise x goal-difference
#'   multiplier, the v6 production form. "margin_sqrt" = update toward a blended
#'   goals/xG margin, sqrt-dampened (the xG-Elo form).
#' @param home_xg,away_xg Expected goals per team (margin_sqrt mode only). When
#'   either is NA the target falls back to actual goal difference (~35% of
#'   matches have no shot data, so xG is unavailable for them).
#' @param margin_slope Expected goal-margin per 400 Elo of gap, used as the
#'   reference the result is judged against (margin_sqrt mode). Default 1.66.
#' @param blend_w Weight on actual goal diff vs xG diff in the target margin:
#'   perf = blend_w*GD + (1-blend_w)*xGD (margin_sqrt mode). Default 0.5.
#'
#' @return Named list with new_home_elo, new_away_elo
#' @family match prediction
#' @export
update_elo <- function(home_elo, away_elo, home_goals, away_goals,
                        k = 20, home_advantage = 88,
                        update_mode = c("outcome", "margin_sqrt"),
                        home_xg = NA_real_, away_xg = NA_real_,
                        margin_slope = 1.66, blend_w = 0.5) {
  stopifnot(length(home_goals) == 1, length(away_goals) == 1)
  update_mode <- match.arg(update_mode)

  # Elo gap (already includes any home-advantage / venue adjustment passed in).
  diff <- (home_elo + home_advantage - away_elo) / 400

  if (update_mode == "outcome") {
    exp_home <- 1 / (1 + 10^(-diff))
    exp_away <- 1 - exp_home
    # Actual scores (1 = win, 0.5 = draw, 0 = loss)
    if (home_goals > away_goals) {
      actual_home <- 1
    } else if (home_goals == away_goals) {
      actual_home <- 0.5
    } else {
      actual_home <- 0
    }
    actual_away <- 1 - actual_home
    # Goal difference multiplier (rewards larger margins)
    goal_diff <- abs(home_goals - away_goals)
    gd_mult <- log(goal_diff + 1) + 1
    return(list(
      new_home_elo = home_elo + k * gd_mult * (actual_home - exp_home),
      new_away_elo = away_elo + k * gd_mult * (actual_away - exp_away)
    ))
  }

  # margin_sqrt: update toward a blended "true score" margin. perf_margin blends
  # actual goal diff with xG diff (xG carries the bulk of the signal — less noisy
  # per match), falling back to goal diff alone where xG is missing. The surprise
  # (perf_margin - expected_margin) is sqrt-dampened so a blowout or a mis-scraped
  # scoreline can't detonate the rating; expected_margin scales with the Elo gap.
  gd <- home_goals - away_goals
  perf_margin <- if (!is.na(home_xg) && !is.na(away_xg)) {
    blend_w * gd + (1 - blend_w) * (home_xg - away_xg)
  } else {
    gd
  }
  dev <- perf_margin - margin_slope * diff
  u <- sign(dev) * sqrt(abs(dev))
  # Zero-sum: u is odd in the home POV, so away moves by exactly -u.
  list(
    new_home_elo = home_elo + k * u,
    new_away_elo = away_elo - k * u
  )
}


#' Compute Elo Ratings for All Matches
#'
#' Iterates through matches chronologically and computes Elo ratings.
#' Returns BOTH per-match pre-match Elos (for joining onto the match
#' dataset) AND the final post-iteration team-Elo state (for looking up
#' the current Elo of teams in upcoming fixtures). Returning both is what
#' prevents step 03 from having to duplicate the iteration -- the previous
#' duplicate-iteration approach was missing the same NA guards as this
#' function, which caused the 2026-05-28 NA-cascade bug where a single
#' NA-team friendly poisoned every team's Elo via NA-named-lookup.
#'
#' @param results Data frame with match_date, home_team, away_team,
#'   home_goals, away_goals columns, sorted by date
#' @param k K-factor (default 20)
#' @param home_advantage Home advantage in Elo points (default 65)
#' @param initial_elo Starting Elo (default 1500)
#' @param k_table Optional named numeric vector mapping league codes to
#'   per-match-type K values (e.g., `ELO_MATCH_TYPE_K`). When supplied,
#'   `elo_match_k()` selects the K for each match by its league; otherwise
#'   every match uses the single `k` argument.
#' @param cross_conf_mult Numeric multiplier (default 1.0 = disabled) applied
#'   to K when home and away teams are in different confederations. Lets the
#'   model learn faster from cross-confederation matches that constrain the
#'   relative ordering between confederation prior centers.
#' @param conf_priors Optional named numeric vector of starting Elos per
#'   confederation (e.g., `c(UEFA=1500, CONMEBOL=1500, ...)`). When supplied,
#'   teams are initialized from their confederation's prior instead of the
#'   single `initial_elo`. Requires `build_team_confederations()` to be able
#'   to derive each team's confederation from `results`.
#' @param use_venue_factor Logical (default FALSE for backwards compat). When
#'   TRUE, `home_advantage` is scaled by `compute_venue_factor()` per match --
#'   +1 for true home, 0 for neutral tournament, -1 when the designated
#'   "home_team" is actually visiting the host country.
#' @param time_decay_halflife Optional numeric (days, default NULL = disabled).
#'   When set, scales K by `0.5 ^ ((reference_date - match_date) / halflife)`
#'   so older matches contribute less to the Elo iteration. Useful for
#'   recency-weighting; v5 optimization converged near "off" (~6500 days), so
#'   not default but available for callers wanting FIFA/SPI-style recency.
#' @param decay_reference_date Optional Date or date-coercible string used as
#'   "now" for the decay calculation. Defaults to `max(match_date)` in
#'   `results`.
#'
#' @param update_mode Passed to [update_elo()]: "outcome" (default) or
#'   "margin_sqrt" (the xG-Elo form). When "margin_sqrt" and `results` carries
#'   `home_xg`/`away_xg` columns, the update targets a blended goals/xG margin
#'   (goal-diff fallback per row where xG is NA).
#' @param blend_w,margin_slope Passed to [update_elo()] in margin_sqrt mode
#'   (weight on goals vs xG, and expected-margin-per-400-Elo).
#'
#' @return A list with two elements:
#'   - `per_match`: data frame with match_id, home_elo, away_elo, elo_diff
#'     (pre-match Elo for each match in the input order)
#'   - `final_elos`: named numeric vector of post-iteration team Elos,
#'     for use with upcoming fixtures
#' @family match prediction
#' @export
compute_match_elos <- function(results, k = 20, home_advantage = 88,
                                initial_elo = 1500,
                                k_table = NULL,
                                cross_conf_mult = 1.0,
                                conf_priors = NULL,
                                use_venue_factor = FALSE,
                                time_decay_halflife = NULL,
                                decay_reference_date = NULL,
                                update_mode = c("outcome", "margin_sqrt"),
                                blend_w = 0.5, margin_slope = 1.66) {
  update_mode <- match.arg(update_mode)
  # margin_sqrt can use per-match xG when results carries it; else falls back to
  # goal diff inside update_elo(). Detect the columns once.
  has_xg_cols <- all(c("home_xg", "away_xg") %in% names(results))
  # time_decay_halflife (days, NULL = disabled): when set, scale K by
  # 0.5 ^ ((reference_date - match_date) / halflife) so older training
  # matches contribute exponentially less to the Elo trajectory than
  # recent ones. Useful when the model is expected to predict matches at
  # the END of the data -- older form should fade. Set decay_reference_date
  # to control "now"; defaults to max(match_date) in `results`.
  # Sort by date
  results <- results[order(results$match_date), ]

  all_teams <- unique(c(results$home_team, results$away_team))

  # Build confederation lookup once if needed for EITHER cross_conf_mult
  # OR conf_priors (most callers want both, so build it if either flag is on).
  need_lookup <- cross_conf_mult != 1.0 || !is.null(conf_priors)
  conf_lookup <- if (need_lookup) build_team_confederations(results) else NULL

  # Initial elos: confederation-prior if conf_priors supplied, else single
  # initial_elo for every team (preserves legacy behaviour).
  elos <- if (!is.null(conf_priors)) {
    init_team_elos_with_priors(all_teams, conf_lookup,
                                conf_priors = conf_priors,
                                initial_elo = initial_elo)
  } else {
    init_team_elos(all_teams, initial_elo)  # filters NAs internally
  }

  # Per-match base K. If `k_table` is supplied use elo_match_k(); else
  # fall back to the single `k` argument for every match (preserves the
  # old single-K behaviour for any caller that hasn't opted in).
  base_k_per_match <- if (!is.null(k_table)) {
    elo_match_k(results$league, k_table = k_table, default = k)
  } else {
    rep(k, nrow(results))
  }

  # Per-match time-decay multiplier. Exponential decay relative to
  # `decay_reference_date` (default: most recent match in results). The
  # decay scales K -- older matches still update Elo, just by less -- so
  # the long-run baseline is preserved but recent form dominates.
  # When time_decay_halflife is NULL or <= 0, every match gets 1.0 (no decay).
  decay_per_match <- if (!is.null(time_decay_halflife) &&
                          is.finite(time_decay_halflife) &&
                          time_decay_halflife > 0) {
    dts <- suppressWarnings(as.Date(sub("Z$", "", results$match_date)))
    ref_dt <- if (!is.null(decay_reference_date)) {
      as.Date(decay_reference_date)
    } else {
      max(dts, na.rm = TRUE)
    }
    days_back <- pmax(0, as.numeric(ref_dt - dts))
    0.5 ^ (days_back / time_decay_halflife)
  } else {
    rep(1.0, nrow(results))
  }

  # Per-match venue factor: scales home_advantage per match. See
  # compute_venue_factor() docs. When use_venue_factor=FALSE (default for
  # backwards compat), every match gets +1 (= old behaviour). When TRUE,
  # tournament matches at neutral venues get 0 and host-country matches
  # for visitors get -1, fixing the bias where Opta's arbitrary "home_team"
  # designation was getting +65 advantage on actually-neutral matches.
  venue_factor <- if (isTRUE(use_venue_factor) &&
                      all(c("league", "season") %in% names(results))) {
    compute_venue_factor(results$home_team, results$away_team,
                          results$league, results$season)
  } else {
    rep(1, nrow(results))
  }

  n <- nrow(results)
  home_elo_pre <- numeric(n)
  away_elo_pre <- numeric(n)

  for (i in seq_len(n)) {
    ht <- results$home_team[i]
    at <- results$away_team[i]

    # Skip rows with missing team names -- they cannot be processed (no
    # team to update). Records NA pre-match Elo for the row so the join
    # downstream sees the gap, but does NOT touch the elos vector.
    if (is.na(ht) || is.na(at)) {
      home_elo_pre[i] <- NA_real_
      away_elo_pre[i] <- NA_real_
      next
    }

    # Record pre-match Elo
    home_elo_pre[i] <- elos[ht]
    away_elo_pre[i] <- elos[at]

    # Update only for played matches
    if (!is.na(results$home_goals[i]) && !is.na(results$away_goals[i])) {
      # Effective K = base_K(match_type) * cross_conf_mult(team1, team2).
      # The goal-difference multiplier is applied inside update_elo().
      # Only apply the cross-conf multiplier when it's actually != 1.0;
      # conf_lookup might be non-NULL purely for conf_priors init while
      # cross_conf_mult=1.0 (disabled).
      k_eff <- base_k_per_match[i] * decay_per_match[i]
      if (!is.null(conf_lookup) && cross_conf_mult != 1.0) {
        k_eff <- k_eff * cross_conf_multiplier(ht, at, conf_lookup,
                                                mult = cross_conf_mult)
      }
      # Effective home_advantage scales by venue_factor (+1/0/-1).
      ha_eff <- home_advantage * venue_factor[i]
      updated <- update_elo(elos[ht], elos[at],
                            results$home_goals[i], results$away_goals[i],
                            k = k_eff, home_advantage = ha_eff,
                            update_mode = update_mode,
                            home_xg = if (has_xg_cols) results$home_xg[i] else NA_real_,
                            away_xg = if (has_xg_cols) results$away_xg[i] else NA_real_,
                            margin_slope = margin_slope, blend_w = blend_w)
      elos[ht] <- updated$new_home_elo
      elos[at] <- updated$new_away_elo
    }
  }

  # Mid-season-rename smoke test: this function keys Elo by team_name, so
  # if Opta ever renames a team mid-flow (e.g., "Rangers" -> "Rangers FC")
  # the iteration silently treats them as two distinct teams each starting
  # at initial_elo. Surface very-low-match teams as a soft warning.
  #
  # Threshold tuned to <3L from the prior <10L: at <10L the warning fired
  # on ~215 legitimate UCL/UEL/UECL qualifying-round clubs eliminated in
  # rounds 1-3 (pure noise — 2-8 matches in the entire dataset window is
  # their physical max). At <3L the noise floor is intl qualifying one-offs
  # + small national teams (Cayman Islands, Aruba) only. Audited 2026-05-29
  # via debug/keep/split_identity_audit.R: zero domestic-league teams trigger
  # the warning; the team_id canonicalization block in
  # 01_build_fixture_results.R ("Normalize fixture team names to the variant
  # Opta uses in its lineup feed") shows zero team_ids with multiple name
  # variants. The smoke test here is a secondary check; the 01 normalization
  # is the real defense.
  match_counts <- sort(table(c(results$home_team[!is.na(results$home_team)],
                                results$away_team[!is.na(results$away_team)])))
  low_match_teams <- names(match_counts)[match_counts < 3L]
  if (length(low_match_teams) > 10L) {
    cli::cli_inform(c(
      "i" = "{length(low_match_teams)} team(s) with <3 matches in Elo iteration -- possible split-identity from a mid-season rename. Investigate before publishing if any are domestic-league teams.",
      "*" = "Sample: {paste(head(low_match_teams, 10), collapse = ', ')}..."
    ))
  }

  list(
    per_match = data.frame(
      match_id = results$match_id,
      home_elo = home_elo_pre,
      away_elo = away_elo_pre,
      elo_diff = home_elo_pre - away_elo_pre,
      stringsAsFactors = FALSE
    ),
    final_elos = elos
  )
}


# Helper: create a dummy lineup (11 players, replacement-level ratings) for
# teams with no lineup history. Positions: 1 GK, 4 DEF, 4 MID, 2 FWD.
#' @keywords internal
make_dummy_lineup <- function(match_id, team_id, team_name, team_position) {
  positions <- c("Goalkeeper", rep("Defender", 4), rep("Midfielder", 4),
                 rep("Forward", 2))
  data.frame(
    match_id = match_id, team_id = team_id, team_name = team_name,
    team_position = team_position, player_name = paste0("Unknown_", seq(11)),
    position = positions, is_starter = TRUE,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Rolling Features
# =============================================================================

#' Compute Team Rolling Features
#'
#' Calculates rolling averages of team performance metrics using strictly
#' lagged windows (no data leakage). Uses data.table frollmean + shift.
#'
#' @param results Data frame of match results with match_id, match_date,
#'   home_team, away_team, home_goals, away_goals, home_xg, away_xg
#' @param windows Rolling window sizes (default c(5, 10, 20))
#'
#' @return Data frame with match_id and rolling features for home/away
#' @family match prediction
#' @export
compute_team_rolling_features <- function(results, windows = c(5L, 10L, 20L)) {
  dt <- data.table::as.data.table(results)
  dt <- dt[order(match_date)]

  # Build team-match rows (each match generates 2 rows: one per team)
  home <- dt[, .(match_id, match_date, team = home_team,
                 goals_scored = home_goals, goals_conceded = away_goals,
                 xg_for = home_xg, xg_against = away_xg,
                 is_home = 1L)]
  away <- dt[, .(match_id, match_date, team = away_team,
                 goals_scored = away_goals, goals_conceded = home_goals,
                 xg_for = away_xg, xg_against = home_xg,
                 is_home = 0L)]
  team_matches <- data.table::rbindlist(list(home, away))
  team_matches <- team_matches[order(match_date)]

  # Compute derived metrics
  team_matches[, points := data.table::fifelse(
    goals_scored > goals_conceded, 3L,
    data.table::fifelse(goals_scored == goals_conceded, 1L, 0L)
  )]
  team_matches[, win := as.integer(goals_scored > goals_conceded)]
  team_matches[, clean_sheet := as.integer(goals_conceded == 0)]
  team_matches[, npxgd := xg_for - xg_against]

  # Rolling features per team, strictly lagged
  metrics <- c("goals_scored", "goals_conceded", "xg_for", "xg_against",
               "points", "win", "clean_sheet", "npxgd")

  for (w in windows) {
    for (m in metrics) {
      col_name <- sprintf("%s_last_%d", m, w)
      team_matches[, (col_name) := data.table::shift(
        data.table::frollmean(get(m), n = w, align = "right", na.rm = TRUE)
      ), by = team]
    }
  }

  # Days since last match
  team_matches[, days_since_last := as.numeric(
    difftime(match_date, data.table::shift(match_date, 1L), units = "days")
  ), by = team]

  # Pivot back to match level (home/away)
  roll_cols <- grep("_last_\\d+$|days_since_last", names(team_matches), value = TRUE)
  keep_cols <- c("match_id", "is_home", roll_cols)

  home_feats <- team_matches[is_home == 1L, ..keep_cols]
  away_feats <- team_matches[is_home == 0L, ..keep_cols]

  data.table::setnames(home_feats, roll_cols, paste0("home_", roll_cols))
  data.table::setnames(away_feats, roll_cols, paste0("away_", roll_cols))

  home_feats[, is_home := NULL]
  away_feats[, is_home := NULL]

  result <- home_feats[away_feats, on = "match_id", nomatch = NULL]

  # Add rolling differentials for key metrics
  for (w in windows) {
    for (m in c("goals_scored", "xg_for", "points", "npxgd")) {
      h_col <- sprintf("home_%s_last_%d", m, w)
      a_col <- sprintf("away_%s_last_%d", m, w)
      d_col <- sprintf("diff_%s_last_%d", m, w)
      if (h_col %in% names(result) && a_col %in% names(result)) {
        result[, (d_col) := get(h_col) - get(a_col)]
      }
    }
  }

  # Rest difference
  result[, rest_diff := home_days_since_last - away_days_since_last]

  data.table::setDF(result)
  result
}


# =============================================================================
# XGBoost Model Wrappers
# =============================================================================

# Helper: extract best nrounds from CV result with fallback
.get_best_nrounds <- function(cv_result) {
  n <- cv_result$best_iteration
  if (is.null(n) || length(n) == 0) {
    eval_log <- cv_result$evaluation_log
    metric_col <- grep("test.*mean", names(eval_log), value = TRUE)[1]
    if (is.na(metric_col)) {
      cli::cli_abort(c(
        "Cannot determine best nrounds from CV result.",
        "x" = "No {.field best_iteration} and no {.field test.*mean} column in evaluation_log.",
        "i" = "Available columns: {paste(names(eval_log), collapse = ', ')}"
      ))
    }
    n <- which.min(eval_log[[metric_col]])
  }
  if (length(n) == 0 || is.na(n)) {
    cli::cli_abort("Failed to determine best nrounds from CV result.")
  }
  n
}


#' Reshape a multi:softprob prediction into one row per observation
#'
#' \code{xgboost >= 2.0} returns an \code{n x n_class} matrix from
#' \code{predict()}; older versions return a flat, ROW-major vector. Reshaping
#' the flat form with \code{byrow = FALSE} silently scrambles classes across
#' observations, and the obvious guard does not catch it -- a column-major
#' reshape of a row-major softprob vector can still produce rows summing to
#' one. Every call site goes through this helper rather than reshaping
#' inline, so the xgboost return contract is interpreted in exactly one place.
#'
#' @param probs Raw \code{predict()} output from a \code{multi:softprob} model.
#' @param n_rows Number of observations predicted.
#' @param n_class Number of classes (default 3: home / draw / away).
#'
#' @return An \code{n_rows} x \code{n_class} numeric matrix.
#' @keywords internal
softprob_matrix <- function(probs, n_rows, n_class = 3L) {
  m <- if (is.matrix(probs)) {
    probs
  } else {
    matrix(probs, ncol = n_class, byrow = TRUE)
  }
  if (nrow(m) != n_rows || ncol(m) != n_class) {
    cli::cli_abort(c(
      "Unexpected probability matrix dimensions from the XGBoost outcome model.",
      "x" = "Expected {n_rows} rows x {n_class} columns, got {nrow(m)} x {ncol(m)}."
    ))
  }
  m
}


#' Fit XGBoost Model with Cross-Validation
#'
#' Shared helper for training XGBoost models with k-fold cross-validation
#' and early stopping. Used by \code{\link{fit_goals_xgb}} (Poisson) and
#' \code{\link{fit_outcome_xgb}} (multinomial).
#'
#' @param X Feature matrix
#' @param y Target vector (goal counts for Poisson, integer labels for multinomial)
#' @param params XGBoost parameters list (objective, eval_metric, etc.)
#' @param nfolds Number of CV folds (default 5)
#' @param nrounds Max boosting rounds (default 500)
#' @param early_stopping Patience for early stopping (default 30)
#' @param verbose Print progress (default 1)
#'
#' @return List with model, cv_result, best_nrounds, metadata
#' @keywords internal
.fit_xgb_model <- function(X, y, params, nfolds = 5L, nrounds = 500L,
                           early_stopping = 30L, verbose = 1L) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required.")
  }

  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    nfold = nfolds,
    early_stopping_rounds = early_stopping,
    verbose = verbose,
    print_every_n = 50L
  )

  best_nrounds <- .get_best_nrounds(cv_result)

  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    verbose = 0L
  )

  importance <- xgboost::xgb.importance(
    feature_names = colnames(X),
    model = model
  )

  list(
    model = model,
    cv_result = cv_result,
    importance = importance,
    best_nrounds = best_nrounds,
    params = params,
    feature_names = colnames(X)
  )
}

#' Fit XGBoost Poisson Model for Goal Prediction
#'
#' Wrapper around \code{\link{.fit_xgb_model}} with Poisson regression defaults.
#'
#' @param X Feature matrix
#' @param y Target vector (goal counts)
#' @param nfolds Number of CV folds (default 5)
#' @param params XGBoost parameters (default: Poisson regression with eta=0.05)
#' @param nrounds Max boosting rounds (default 500)
#' @param early_stopping Patience for early stopping (default 30)
#' @param verbose Print progress (default 1)
#'
#' @return List with model, cv_result, best_nrounds, metadata
#' @keywords internal
fit_goals_xgb <- function(X, y, nfolds = 5L, params = NULL,
                           nrounds = 500L, early_stopping = 30L,
                           verbose = 1L) {
  if (is.null(params)) {
    params <- list(
      objective = "count:poisson",
      eval_metric = "poisson-nloglik",
      max_depth = 5L,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 10
    )
  }
  .fit_xgb_model(X, y, params, nfolds, nrounds, early_stopping, verbose)
}


#' Fit XGBoost Multinomial Model for Match Outcome
#'
#' Fits XGBoost multi:softprob for P(Home Win), P(Draw), P(Away Win).
#' Labels: 0 = Home Win, 1 = Draw, 2 = Away Win.
#'
#' @param X Feature matrix
#' @param y Integer labels (0=H, 1=D, 2=A)
#' @param nfolds Number of CV folds (default 5)
#' @param params XGBoost parameters (default multinomial)
#' @param nrounds Max boosting rounds (default 500)
#' @param early_stopping Patience for early stopping (default 30)
#' @param verbose Print progress (default 1)
#'
#' @return List with model, cv_result, best_nrounds, metadata
#' @keywords internal
fit_outcome_xgb <- function(X, y, nfolds = 5L, params = NULL,
                              nrounds = 500L, early_stopping = 30L,
                              verbose = 1L) {
  if (is.null(params)) {
    params <- list(
      objective = "multi:softprob",
      num_class = 3L,
      eval_metric = "mlogloss",
      max_depth = 5L,
      eta = 0.05,
      subsample = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 10
    )
  }
  .fit_xgb_model(X, y, params, nfolds, nrounds, early_stopping, verbose)
}


#' Predict Match Outcome Probabilities
#'
#' Given fitted goals and outcome models, predicts P(H), P(D), P(A) and
#' expected goals for a set of matches.
#'
#' @param goals_home_model Fitted XGBoost Poisson model for home goals
#' @param goals_away_model Fitted XGBoost Poisson model for away goals
#' @param outcome_model Fitted XGBoost multinomial model
#' @param X_goals Feature matrix for goals models
#' @param X_outcome Feature matrix for outcome model (without goal predictions)
#'
#' @return Data frame with pred_home_goals, pred_away_goals, prob_H, prob_D, prob_A
#' @family match prediction
#' @export
predict_match <- function(goals_home_model, goals_away_model, outcome_model,
                           X_goals, X_outcome) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required.")
  }

  # Predict goals
  d_goals <- xgboost::xgb.DMatrix(data = X_goals)
  pred_home_goals <- stats::predict(goals_home_model$model, d_goals)
  pred_away_goals <- stats::predict(goals_away_model$model, d_goals)

  # Augment outcome features with predicted goals
  goal_features <- cbind(
    pred_home_goals = pred_home_goals,
    pred_away_goals = pred_away_goals,
    pred_goal_diff = pred_home_goals - pred_away_goals,
    pred_total_goals = pred_home_goals + pred_away_goals
  )
  X_full <- cbind(X_outcome, goal_features)

  # Predict outcome probabilities
  d_outcome <- xgboost::xgb.DMatrix(data = X_full)
  probs <- stats::predict(outcome_model$model, d_outcome)
  prob_matrix <- softprob_matrix(probs, nrow(X_full))

  data.frame(
    pred_home_goals = pred_home_goals,
    pred_away_goals = pred_away_goals,
    prob_H = prob_matrix[, 1],
    prob_D = prob_matrix[, 2],
    prob_A = prob_matrix[, 3],
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# Evaluation Metrics
# =============================================================================

#' Compute Multi-Class Log Loss
#'
#' @param y_true Integer vector of true labels (0, 1, 2)
#' @param prob_matrix Matrix with 3 columns (P(0), P(1), P(2))
#' @param eps Clipping epsilon to avoid log(0) (default 1e-15)
#'
#' @return Scalar log loss value
#' @family match prediction
#' @export
compute_multiclass_logloss <- function(y_true, prob_matrix, eps = 1e-15) {
  stopifnot(all(y_true %in% 0:2))
  prob_matrix <- pmax(pmin(prob_matrix, 1 - eps), eps)
  n <- length(y_true)
  # Vectorized log-loss
  idx <- cbind(seq_len(n), y_true + 1L)
  -mean(log(prob_matrix[idx]))
}


#' Create Calibration Table
#'
#' Groups predictions into bins and compares predicted vs actual probabilities.
#'
#' @param y_true Integer vector of true outcomes (0=H, 1=D, 2=A)
#' @param prob_matrix Matrix with 3 columns of predicted probabilities
#' @param n_bins Number of calibration bins (default 10)
#'
#' @return Data frame with bin midpoints, predicted and actual probabilities
#' @family match prediction
#' @export
calibration_table <- function(y_true, prob_matrix, n_bins = 10L) {
  results <- list()
  outcome_labels <- c("Home", "Draw", "Away")

  for (k in seq_len(3)) {
    probs <- prob_matrix[, k]
    actual <- as.integer(y_true == (k - 1L))

    breaks <- seq(0, 1, length.out = n_bins + 1L)
    bins <- cut(probs, breaks = breaks, include.lowest = TRUE)

    dt <- data.table::data.table(prob = probs, actual = actual, bin = bins)
    cal <- dt[, .(
      pred_mean = mean(prob),
      actual_mean = mean(actual),
      n = .N
    ), by = bin]

    cal$outcome <- outcome_labels[k]
    results[[k]] <- cal
  }

  data.table::rbindlist(results)
}
