# Match Prediction Functions
#
# Reusable functions for predicting match outcomes using player ratings,
# rolling form features, and Elo ratings. Supports XGBoost Poisson models
# for goal counts and multinomial models for W/D/L probabilities.


# =============================================================================
# Team Rating Aggregation
# =============================================================================

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

  # Determine join key: use player_id when both sides have it and IDs overlap
  has_id_both <- "player_id" %in% names(dt_lineups) &&
    "player_id" %in% names(dt_ratings)
  use_id_join <- FALSE
  if (has_id_both) {
    if (any(dt_lineups$player_id %in% dt_ratings$player_id, na.rm = TRUE)) {
      use_id_join <- TRUE
    } else {
      warning(sprintf(
        "Both lineups and ratings have player_id but 0 IDs overlap (lineups: %d unique, ratings: %d unique). Falling back to name-based join.",
        length(unique(dt_lineups$player_id)),
        length(unique(dt_ratings$player_id))
      ), call. = FALSE)
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

  # Current season ratings
  rating_cols <- c(join_key, "panna", "offense", "defense", "spm")
  curr <- dt_ratings[season_end_year == sey_curr, rating_cols, with = FALSE]
  curr <- curr[!duplicated(curr[[join_key]])]

  # Previous season ratings (fallback with decay)
  prev <- dt_ratings[season_end_year == sey_prev, rating_cols, with = FALSE]
  prev <- prev[!duplicated(prev[[join_key]])]
  prev[, panna := panna * prev_season_decay]
  prev[, offense := offense * prev_season_decay]
  prev[, defense := defense * prev_season_decay]
  prev[, spm := spm * prev_season_decay]
  data.table::setnames(prev, c("panna", "offense", "defense", "spm"),
                       c("panna_prev", "offense_prev", "defense_prev", "spm_prev"))

  # Join current ratings
  starters <- curr[starters, on = join_key]
  # Fallback to previous season
  starters <- prev[starters, on = join_key]
  starters[is.na(panna), panna := panna_prev]
  starters[is.na(offense), offense := offense_prev]
  starters[is.na(defense), defense := defense_prev]
  starters[is.na(spm), spm := spm_prev]
  # Replacement level for unrated players

  starters[is.na(panna), panna := 0]
  starters[is.na(offense), offense := 0]
  starters[is.na(defense), defense := 0]
  starters[is.na(spm), spm := 0]

  # Classify positions
  starters[, pos_group := data.table::fcase(
    grepl("goalkeeper", tolower(position)), "gk",
    grepl("defender", tolower(position)), "def",
    grepl("midfielder", tolower(position)), "mid",
    grepl("forward|striker", tolower(position)), "fwd",
    default = "mid"
  )]

  # Aggregate per match-side
  team_stats <- starters[, {
    n_rated <- sum(panna != 0)
    gk_idx <- which(pos_group == "gk")
    gk_panna_val <- if (length(gk_idx) > 0) panna[gk_idx[1]] else 0

    # Positional group averages
    def_idx <- pos_group == "def"
    mid_idx <- pos_group == "mid"
    fwd_idx <- pos_group == "fwd"

    safe_mean <- function(x) if (length(x) == 0 || all(is.na(x))) 0 else mean(x, na.rm = TRUE)

    list(
      sum_panna = sum(panna), sum_offense = sum(offense),
      sum_defense = sum(defense), sum_spm = sum(spm),
      avg_panna = mean(panna), max_panna = max(panna),
      min_panna = min(panna),
      stdev_panna = if (.N > 1) stats::sd(panna) else 0,
      gk_panna = gk_panna_val,
      avg_def_panna = safe_mean(panna[def_idx]),
      avg_mid_panna = safe_mean(panna[mid_idx]),
      avg_fwd_panna = safe_mean(panna[fwd_idx]),
      avg_def_offense = safe_mean(offense[def_idx]),
      avg_def_defense = safe_mean(defense[def_idx]),
      avg_mid_offense = safe_mean(offense[mid_idx]),
      avg_mid_defense = safe_mean(defense[mid_idx]),
      avg_fwd_offense = safe_mean(offense[fwd_idx]),
      avg_fwd_defense = safe_mean(defense[fwd_idx]),
      n_rated_players = n_rated
    )
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

  # Aggregate by match + side
  team_skills <- starters[, {
    result <- list()
    stat_means <- numeric(length(all_stats))
    for (j in seq_along(all_stats)) {
      stat <- all_stats[j]
      vals <- .SD[[stat]]
      vals[is.na(vals)] <- 0
      prefix <- if (stat %in% attacking_stats) "sk_att" else "sk_def"
      col_name <- paste0(prefix, "_", sub("_p90$", "", stat))
      stat_means[j] <- mean(vals)
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
#'
#' @param teams Character vector of team names
#' @param initial_elo Starting Elo rating (default 1500)
#'
#' @return Named numeric vector of Elo ratings
#' @export
init_team_elos <- function(teams, initial_elo = 1500) {
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
#' @param home_advantage Home advantage in Elo points (default 65)
#'
#' @return Named list with new_home_elo, new_away_elo
#' @export
update_elo <- function(home_elo, away_elo, home_goals, away_goals,
                        k = 20, home_advantage = 65) {
  # Expected scores
  diff <- (home_elo + home_advantage - away_elo) / 400
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

  list(
    new_home_elo = home_elo + k * gd_mult * (actual_home - exp_home),
    new_away_elo = away_elo + k * gd_mult * (actual_away - exp_away)
  )
}


#' Compute Elo Ratings for All Matches
#'
#' Iterates through matches chronologically and computes Elo ratings.
#' Returns match-level Elo features (pre-match ratings for each team).
#'
#' @param results Data frame with match_date, home_team, away_team,
#'   home_goals, away_goals columns, sorted by date
#' @param k K-factor (default 20)
#' @param home_advantage Home advantage in Elo points (default 65)
#' @param initial_elo Starting Elo (default 1500)
#'
#' @return Data frame with match_id, home_elo, away_elo, elo_diff columns
#' @export
compute_match_elos <- function(results, k = 20, home_advantage = 65,
                                initial_elo = 1500) {
  # Sort by date
  results <- results[order(results$match_date), ]

  all_teams <- unique(c(results$home_team, results$away_team))
  elos <- init_team_elos(all_teams, initial_elo)

  n <- nrow(results)
  home_elo_pre <- numeric(n)
  away_elo_pre <- numeric(n)

  for (i in seq_len(n)) {
    ht <- results$home_team[i]
    at <- results$away_team[i]

    # Record pre-match Elo
    home_elo_pre[i] <- elos[ht]
    away_elo_pre[i] <- elos[at]

    # Update only for played matches
    if (!is.na(results$home_goals[i]) && !is.na(results$away_goals[i])) {
      updated <- update_elo(elos[ht], elos[at],
                            results$home_goals[i], results$away_goals[i],
                            k = k, home_advantage = home_advantage)
      elos[ht] <- updated$new_home_elo
      elos[at] <- updated$new_away_elo
    }
  }

  data.frame(
    match_id = results$match_id,
    home_elo = home_elo_pre,
    away_elo = away_elo_pre,
    elo_diff = home_elo_pre - away_elo_pre,
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
    n <- which.min(eval_log[[metric_col]])
  }
  n
}


#' Fit XGBoost Poisson Model for Goal Prediction
#'
#' Fits an XGBoost model with Poisson objective for predicting goal counts.
#' Uses time-based cross-validation with early stopping.
#'
#' @param X Feature matrix
#' @param y Target vector (goal counts)
#' @param nfolds Number of CV folds (default 5)
#' @param params XGBoost parameters (default Poisson regression)
#' @param nrounds Max boosting rounds (default 500)
#' @param early_stopping Patience for early stopping (default 30)
#' @param verbose Print progress (default 1)
#'
#' @return List with model, cv_result, best_nrounds, metadata
#' @export
fit_goals_xgb <- function(X, y, nfolds = 5L, params = NULL,
                           nrounds = 500L, early_stopping = 30L,
                           verbose = 1L) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required.")
  }

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
#' @export
fit_outcome_xgb <- function(X, y, nfolds = 5L, params = NULL,
                              nrounds = 500L, early_stopping = 30L,
                              verbose = 1L) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required.")
  }

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
  # xgboost >= 2.0 returns a matrix; older versions return a flat vector
  if (is.matrix(probs)) {
    prob_matrix <- probs
  } else {
    prob_matrix <- matrix(probs, ncol = 3, byrow = TRUE)
  }

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
