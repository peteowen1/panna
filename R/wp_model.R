# Win Probability Model for Soccer
# =================================
# Predicts P(home_win), P(draw), P(away_win) from in-match game state.
# Draw is valued at 0.5, so WPA measures contribution toward match points.
#
# WP = P(home_win) + 0.5 * P(draw)   (from home team's perspective)
#
# This follows the torpverse pattern (R/add_variables.R) adapted for soccer's
# 3-outcome structure and low-scoring nature.


# ============================================================================
# Feature engineering
# ============================================================================

#' Attach a per-action red_card flag to possession chains (#93)
#'
#' SPADL carries only on-ball gameplay actions, so card events (Opta
#' \code{type_id == 17}) are filtered out in \code{convert_opta_to_spadl()} and
#' never reach the possession chains. As a result the red-card block in
#' \code{\link{create_wp_features}} (\code{if ("red_card" \%in\% names(dt))})
#' always took its else branch and \code{red_card_diff} was a constant 0 -- a
#' dead feature. This helper re-derives red cards from the RAW events and joins
#' a 0/1 \code{red_card} column onto the chains so that block activates.
#'
#' Detection mirrors \code{extract_player_timing_from_events()} in
#' \code{splint_creation.R}: \code{type_id == 17} (Card) carrying qualifier 33
#' (straight red) or 14 (second yellow). The earliest such card per
#' (match, team) is taken, and the flag is set on the single chain action of
#' the carded team nearest that card's time (matching the SPADL clock,
#' \code{time_seconds = minute*60 + second}). One flagged action per red card is
#' exactly what \code{create_wp_features}' \code{cumsum()} logic expects.
#'
#' @param chains Possession chains (output of \code{create_possession_chains()}).
#'   Must contain \code{match_id}, \code{team_id}, \code{time_seconds},
#'   \code{period_id}.
#' @param events Raw Opta match events with \code{match_id}, \code{type_id},
#'   \code{team_id}, \code{minute}, \code{qualifier_json} (and optionally
#'   \code{second}, \code{period_id}).
#'
#' @return \code{chains} with an integer \code{red_card} column (1 on the action
#'   nearest each carded team's red-card time, else 0). If no reds are detected
#'   (or required columns are missing) every row gets \code{red_card = 0}, which
#'   reproduces the previous constant-0 behaviour for that match.
#'
#' @export
add_red_card_to_chains <- function(chains, events) {
  dt <- data.table::as.data.table(chains)
  # Default: no reds. Keeps callers simple -- create_wp_features then computes
  # red_card_diff == 0 for these matches (same as before, but now data-driven).
  dt[, red_card := 0L]

  ev <- data.table::as.data.table(events)
  needed <- c("match_id", "type_id", "team_id", "minute", "qualifier_json")
  if (length(setdiff(needed, names(ev))) > 0L) {
    cli::cli_warn(c(
      "add_red_card_to_chains: raw events missing {.field {setdiff(needed, names(ev))}} ",
      "- red_card left at 0 (red_card_diff stays the dead constant)."
    ))
    return(dt[])
  }

  # Card events only (type_id 17). Same red/second-yellow qualifier test as
  # splint_creation.R::extract_player_timing_from_events / detect_red_in_qj.
  cards <- ev[type_id == 17L]
  if (nrow(cards) == 0L) return(dt[])

  detect_red_in_qj <- function(qj) {
    if (is.na(qj)) return(FALSE)
    parsed <- tryCatch(jsonlite::fromJSON(qj), error = function(e) NULL)
    if (is.null(parsed)) return(FALSE)
    any(c("33", "14") %in% names(parsed))
  }
  cards[, is_red := vapply(qualifier_json, detect_red_in_qj, logical(1))]
  reds <- cards[is_red == TRUE]
  if (nrow(reds) == 0L) return(dt[])

  # Card time on the SPADL clock: time_seconds = minute*60 + second.
  reds[, card_time := as.numeric(minute) * 60 +
         (if ("second" %in% names(reds)) as.numeric(second) else 0)]
  # Earliest red per (match, team) -- a team's first dismissal is the one that
  # changes the man-count game state.
  reds <- reds[!is.na(card_time),
               .(card_time = min(card_time, na.rm = TRUE)),
               by = .(match_id, team_id)]
  if (nrow(reds) == 0L) return(dt[])

  dt[, .row_id := .I]
  # For each carded (match, team), flag the team's chain action nearest the
  # card time. nearest = smallest |time_seconds - card_time|.
  flag_rows <- integer(0)
  for (i in seq_len(nrow(reds))) {
    cand <- dt[match_id == reds$match_id[i] & team_id == reds$team_id[i]]
    if (nrow(cand) == 0L) next
    j <- which.min(abs(cand$time_seconds - reds$card_time[i]))
    flag_rows <- c(flag_rows, cand$.row_id[j])
  }
  if (length(flag_rows) > 0L) dt[.row_id %in% flag_rows, red_card := 1L]
  dt[, .row_id := NULL]
  dt[]
}


#' Create win probability features from SPADL actions
#'
#' Builds the game-state feature set at each action for WP model training
#' and prediction. Features capture score state, expected goals state,
#' time remaining, and team strength indicators.
#'
#' @param spadl_with_epv SPADL actions with EPV. Must contain: \code{match_id},
#'   \code{team_id}, \code{time_seconds}, \code{period_id}.
#' @param match_results Data.frame with \code{match_id}, \code{home_team_id},
#'   \code{away_team_id}, \code{home_goals}, \code{away_goals} for training
#'   labels. If NULL, labels are not added (prediction mode).
#' @param home_teams Optional data.frame with \code{match_id}, \code{home_team_id}
#'   to determine home/away. If NULL, derived from match_results.
#'
#' @return A data.table with one row per action, containing WP features
#'   and optionally training labels.
#'
#' @export
create_wp_features <- function(spadl_with_epv, match_results = NULL,
                                home_teams = NULL) {
  dt <- data.table::as.data.table(spadl_with_epv)
  data.table::setorder(dt, match_id, period_id, time_seconds)

  # Determine home team per match

  if (!is.null(home_teams)) {
    ht <- data.table::as.data.table(home_teams)
  } else if (!is.null(match_results)) {
    ht <- data.table::as.data.table(match_results)[, .(match_id, home_team_id)]
  } else {
    cli::cli_abort("Either {.arg match_results} or {.arg home_teams} must be provided")
  }
  dt[ht, home_team_id := i.home_team_id, on = "match_id"]

  # Determine is_home for each action
  dt[, is_home := as.integer(team_id == home_team_id)]

  # --- Time features ---
  # ⚠ LIVE-USE / LEAKAGE NOTE: match_reached_et below is computed over the WHOLE
  # match (by = match_id), so a regulation action "knows" the match will later
  # reach ET. This is a deliberate, benign lookahead for RETROSPECTIVE WPA:
  #   (1) WPA = wp(t+1) - wp(t) is a delta and the per-match-constant denominator
  #       cancels between the two events; (2) the denominator is identical for
  #       every row of a match, so there is NO within-match discontinuity (a
  #       per-EVENT denominator WOULD create one — see below); (3) add_wp_vars()
  #       centers WPA per match, removing any residual offset. WPA is always
  #       computed on COMPLETED matches, so match_reached_et is genuinely known.
  # This model is therefore NOT safe for LIVE in-progress win probability — you
  # cannot know match_reached_et mid-match, so reusing it for a live ticker would
  # cause train/serve skew. panna's live match prediction is a separate
  # team-level model; do not repurpose this one for live WP without making the
  # denominator causal (5400 until an ET period actually starts).
  #
  # Two extra-time concepts live here and MUST NOT be confused — mixing them
  # silently reintroduces a WPA-inflation bug:
  #
  #   match_reached_et  PER-MATCH  (same for every row of a match) — did this
  #                     match go to ET at all? Drives the time DENOMINATOR.
  #   is_extra_time     PER-EVENT  (per row) — is THIS action in an ET period?
  #                     The model FEATURE.
  #
  # They differ within one match: a regulation pass in a match that later went
  # to ET has match_reached_et == TRUE but is_extra_time == 0.
  #
  # The denominator is decided PER MATCH on purpose. A fixed 5400 cap would
  # clamp every ET action to time_remaining == 0 — telling the model the match
  # is over for the full 30 min of ET, making each ET swing read as
  # near-decisive (the PSG-Arsenal WPA-inflation symptom). Widening to 7200 for
  # ALL matches instead leaves a regulation full-time whistle at 0.25,
  # deflating real late-game WPA. Per-match avoids both.
  #
  # ⚠ Do NOT swap in is_extra_time here: a per-EVENT denominator would give
  # regulation rows 5400 and ET rows 7200 WITHIN THE SAME MATCH, producing a
  # discontinuous jump in time_remaining at the 90' boundary (~0.01 -> 0.25) —
  # a fake WPA spike at exactly the moment knockouts are decided.
  #
  # Shootout events (period_id >= 5) are dropped upstream in
  # convert_opta_to_spadl(), so they never reach here.
  dt[, match_reached_et := any(period_id %in% OPTA_EXTRA_TIME_PERIODS), by = match_id]
  dt[, match_seconds := data.table::fifelse(match_reached_et,
                                            EXTRA_TIME_SECONDS, REGULATION_SECONDS)]
  dt[, time_remaining := pmax(0, (match_seconds - time_seconds)) / match_seconds]
  dt[, time_elapsed_frac := pmin(time_seconds / match_seconds, 1)]
  # Per-event ET indicator (the model feature): 1 for periods 3-4, else 0. Lets
  # the model learn ET-specific dynamics (every chance suddenly decisive,
  # fatigue) rather than extrapolating the regulation curve.
  dt[, is_extra_time := as.integer(period_id %in% OPTA_EXTRA_TIME_PERIODS)]
  # NOTE: time_in_half_remaining is deliberately NOT a WP feature — validation
  # (2026-06-18) found it dead in WP (Gain 0.0016). It IS used by the EPV model
  # (epv_features.R), where it's marginal but kept.
  # Drop the per-match scratch columns so they can't leak downstream and be
  # mistaken for the per-event feature. time_remaining/time_elapsed_frac/
  # is_extra_time carry all the signal the model needs from here.
  dt[, c("match_reached_et", "match_seconds") := NULL]

  # --- Score state ---
  # Detect goals: action_type == "shot" & result == "success" in SPADL
  dt[, is_goal := as.integer(action_type == "shot" & result == "success")]

  # Cumulative goals per team per match
  dt[, home_goal := as.integer(is_goal == 1L & is_home == 1L)]
  dt[, away_goal := as.integer(is_goal == 1L & is_home == 0L)]
  dt[, cum_home_goals := cumsum(home_goal) - home_goal, by = match_id]
  dt[, cum_away_goals := cumsum(away_goal) - away_goal, by = match_id]
  dt[, score_diff := cum_home_goals - cum_away_goals]  # from home perspective

  # --- POSSESSION-POV convention ---
  # Everything from here on is from the possession team's perspective. The
  # model predicts P(possession_team wins match), not P(home wins). is_home
  # becomes a home-advantage feature. xmargin and wp_label follow naturally:
  #
  #   margin_poss = possession_team_score - opponent_score  (sign follows poss)
  #   epv is already possession-POV (positive = possessor about to score)
  #   xmargin = margin_poss + epv   (no sign flipping needed -- both poss-POV)
  #   wp_label = did possession_team win? (1 / 0.5 / 0)
  #
  # Benefits: (1) no sym-flip needed -- both home and away possession actions
  # naturally appear in training, (2) EPV and WP now share the same reference
  # frame, (3) home_advantage is a clean feature not a perspective question.
  dt[, margin_poss := data.table::fifelse(is_home == 1L,
                                          cum_home_goals - cum_away_goals,
                                          cum_away_goals - cum_home_goals)]
  if ("epv" %in% names(dt)) {
    dt[, xmargin := margin_poss + epv]
  } else {
    cli::cli_warn("epv column not found - xmargin falls back to margin_poss")
    dt[, xmargin := as.numeric(margin_poss)]
  }

  # --- xG state ---
  # Cumulative xG differential (if xg column available)
  if ("xg" %in% names(dt)) {
    dt[, home_xg_action := data.table::fifelse(is_home == 1L & action_type == "shot",
                                                xg, 0)]
    dt[, away_xg_action := data.table::fifelse(is_home == 0L & action_type == "shot",
                                                xg, 0)]
    dt[, cum_home_xg := cumsum(home_xg_action), by = match_id]
    dt[, cum_away_xg := cumsum(away_xg_action), by = match_id]
    dt[, xg_diff := cum_home_xg - cum_away_xg]
  } else {
    cli::cli_warn("xg column not found - WP model will operate without xG differential")
    dt[, xg_diff := 0]
  }

  # --- Red card state (if available) ---
  if ("red_card" %in% names(dt)) {
    dt[, home_red := as.integer(red_card == 1L & is_home == 1L)]
    dt[, away_red := as.integer(red_card == 1L & is_home == 0L)]
    dt[, cum_home_reds := cumsum(home_red), by = match_id]
    dt[, cum_away_reds := cumsum(away_red), by = match_id]
    dt[, red_card_diff := cum_home_reds - cum_away_reds]
  } else {
    dt[, red_card_diff := 0L]
  }

  # --- Period ---
  dt[, is_second_half := as.integer(period_id == 2L)]

  # --- Add training labels (possession-POV) ---
  # wp_label = did the POSSESSION team win the match? Not "did home win".
  # For home-possession action: label = (home won ? 1 : lost ? 0 : 0.5)
  # For away-possession action: label = (away won ? 1 : lost ? 0 : 0.5)
  # Draw is 0.5 either way. This is what the possession-POV model predicts.
  if (!is.null(match_results)) {
    mr <- data.table::as.data.table(match_results)
    dt[mr, `:=`(home_goals = i.home_goals, away_goals = i.away_goals), on = "match_id"]
    dt[, wp_label := data.table::fcase(
      home_goals == away_goals, WP_DRAW_VALUE,
      is_home == 1L & home_goals > away_goals, 1,
      is_home == 1L & home_goals < away_goals, 0,
      is_home == 0L & away_goals > home_goals, 1,
      is_home == 0L & away_goals < home_goals, 0
    )]
  }

  # Select feature columns. score_diff (home-POV) and margin_poss (possession-POV)
  # both surfaced -- training uses xmargin, downstream may want score_diff for
  # compatibility.
  # #92: keep standalone `epv` (the fractional in-possession threat) alongside
  # `xmargin`. xmargin = margin_poss + epv is dominated by the integer
  # margin_poss, so trees never split inside the epv band -> live threat never
  # moves WP. Surfacing epv as its own feature lets the model split the threat
  # band independently. epv may be absent (warned upstream); intersect drops it.
  feature_cols <- c("match_id", "team_id", "player_id", "player_name",
                     "action_type", "time_seconds", "period_id",
                     "time_remaining", "time_elapsed_frac",
                     "score_diff", "margin_poss", "xmargin", "epv",
                     "xg_diff", "red_card_diff",
                     "is_home", "is_second_half", "is_extra_time", "is_goal")
  if ("wp_label" %in% names(dt)) feature_cols <- c(feature_cols, "wp_label")

  # Keep additional columns needed for WPA computation
  keep_cols <- intersect(
    c(feature_cols, "result", "home_team_id",
      "receiver_player_id", "receiver_player_name",
      "player_credit", "epv_delta"),
    names(dt)
  )

  dt[, ..keep_cols]
}


#' Train a win probability model
#'
#' Fits an XGBoost model to predict match outcome (home win / draw / away win)
#' from in-match game state features. Uses \code{WP_DRAW_VALUE} (0.5) for draws
#' so the model predicts home's expected points fraction. Uses binary:logistic
#' (cross-entropy with fractional labels) for consistency with torp's AFL WP
#' training harness and natural \verb{[0,1]} output via sigmoid.
#'
#' Two-step training (matches torp::train_live_wp_xgb.R):
#'   1. 5-fold match-grouped xgb.cv with early_stopping_rounds=20 to find
#'      optimal nrounds
#'   2. Final xgb.train at the optimal round on all data
#'
#' @param wp_features Output of \code{\link{create_wp_features}} with
#'   \code{wp_label} column and \code{match_id} (for group-aware folds).
#' @param nrounds Maximum boosting rounds (default 500; early stopping
#'   typically halts well before).
#' @param max_depth Maximum tree depth (default 4).
#' @param eta Learning rate (default 0.05).
#' @param nfolds Number of CV folds (default 5).
#' @param early_stopping_rounds Stop CV if logloss hasn't improved in this
#'   many rounds (default 20).
#' @param seed Random seed for reproducibility (default 42).
#' @param ... Additional parameters passed to \code{xgboost::xgb.train()}.
#'
#' @return A list with:
#'   \describe{
#'     \item{model}{Trained xgboost model object}
#'     \item{feature_names}{Character vector of feature column names}
#'     \item{cv_logloss}{Best CV logloss (held-out mean)}
#'     \item{optimal_nrounds}{The nrounds selected by early stopping}
#'   }
#'
#' @export
train_wp_model <- function(wp_features, nrounds = 500L, max_depth = 4L,
                            eta = 0.05, nfolds = 5L,
                            early_stopping_rounds = 20L, seed = 42L, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} required for WP model training")
  }

  dt <- data.table::as.data.table(wp_features)

  if (!"wp_label" %in% names(dt)) {
    cli::cli_abort("wp_features must contain {.val wp_label} column (from create_wp_features with match_results)")
  }
  if (!"match_id" %in% names(dt)) {
    cli::cli_abort("wp_features must contain {.val match_id} column (for match-grouped CV folds)")
  }

  # xmargin (score_diff + signed EPV) replaces score_diff -- collapses to a
  # single composite feature rather than asking the model to discover the
  # interaction. Kept score_diff out of the training set on purpose: xmargin
  # subsumes it when EPV=0, so keeping both would mostly be redundant.
  #
  # #92: `epv` is added as a STANDALONE feature alongside `xmargin`. xmargin's
  # fractional EPV component is dead -- the integer margin_poss dominates the
  # composite, so the trees never split inside the sub-1.0 epv band and live
  # threat never moves WP. A separate epv feature lets the model split the
  # threat band on its own. (intersect() drops it if upstream had no epv.)
  feature_names <- c("time_remaining",
                      "xmargin", "epv", "xg_diff",
                      "red_card_diff", "is_home", "is_second_half",
                      "is_extra_time")
  feature_names <- intersect(feature_names, names(dt))

  # Filter to valid rows (non-NA label)
  dt <- dt[!is.na(wp_label)]

  # No sym-flip needed under possession-POV convention: training data already
  # contains both home-possession and away-possession actions naturally. Every
  # match contributes ~700 home-possession rows and ~700 away-possession rows,
  # so the symmetry is built into the data distribution. Monotonicity + data
  # symmetry emerges from the natural feature structure, not from a hack.

  mat <- as.matrix(dt[, ..feature_names])
  label <- dt$wp_label
  match_ids <- dt$match_id

  dtrain <- xgboost::xgb.DMatrix(data = mat, label = label)

  # Match-grouped CV folds: keep all rows of the same match in the same fold
  # so we don't leak match-level signal across train/test.
  unique_matches <- unique(match_ids)
  set.seed(seed)
  match_fold <- sample(rep(seq_len(nfolds), length.out = length(unique_matches)))
  names(match_fold) <- unique_matches
  row_fold <- match_fold[match_ids]
  folds <- lapply(seq_len(nfolds), function(k) which(row_fold == k))

  # Monotonicity constraint on xmargin: WP must be non-decreasing in xmargin.
  # Prevents the trees from producing wrong-direction splits (e.g. "higher
  # xmargin -> lower WP in some leaf"). Order matches feature_names.
  mono_vec <- rep(0L, length(feature_names))
  names(mono_vec) <- feature_names
  if ("xmargin" %in% feature_names) mono_vec["xmargin"] <- 1L
  # #92: more in-possession threat must not LOWER the possessor's WP, matching
  # xmargin's +1 sign (both are possession-POV: positive = possessor advantage).
  if ("epv" %in% feature_names) mono_vec["epv"] <- 1L
  mono_str <- paste0("(", paste(mono_vec, collapse = ","), ")")

  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "logloss",
    tree_method = "hist",
    max_depth = max_depth,
    eta = eta,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 50,
    monotone_constraints = mono_str,
    ...
  )

  cli::cli_alert_info("Running {nfolds}-fold match-grouped CV...")
  set.seed(seed)
  cv_result <- xgboost::xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds,
    folds = folds,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = 25L,
    verbose = 1
  )

  optimal_nrounds <- which.min(cv_result$evaluation_log$test_logloss_mean)
  best_logloss <- min(cv_result$evaluation_log$test_logloss_mean)
  cli::cli_alert_info("Optimal nrounds: {optimal_nrounds}, CV logloss: {round(best_logloss, 6)}")

  set.seed(seed)
  model <- xgboost::xgb.train(
    params = params,
    data = dtrain,
    nrounds = optimal_nrounds,
    verbose = 0
  )

  cli::cli_alert_success("Trained WP model on {nrow(mat)} actions ({length(unique_matches)} matches) -- optimal nrounds={optimal_nrounds}, CV logloss={round(best_logloss, 6)}")

  list(
    model = model,
    feature_names = feature_names,
    cv_logloss = best_logloss,
    optimal_nrounds = optimal_nrounds
  )
}


#' Predict win probability
#'
#' Scores each action's game state with win probability using a trained model.
#'
#' @param wp_model Output of \code{\link{train_wp_model}}.
#' @param wp_features SPADL features from \code{\link{create_wp_features}}.
#'
#' @return Numeric vector of win probabilities (home team perspective),
#'   same length as \code{nrow(wp_features)}.
#'
#' @export
predict_wp <- function(wp_model, wp_features) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} required for WP prediction")
  }

  dt <- data.table::as.data.table(wp_features)
  feature_names <- wp_model$feature_names
  mat <- as.matrix(dt[, ..feature_names])

  preds <- predict(wp_model$model, mat)

  # The clamp below silently turns out-of-range values into 0 or 1, which
  # would hide a future model regression (e.g. accidentally saved with
  # binary:logitraw / objective swapped to score-margin, etc.). Warn when
  # >0.1% of predictions are substantially outside [0, 1] -- that signals
  # the model is not producing probabilities anymore.
  out_of_range_frac <- mean(preds < -0.01 | preds > 1.01)
  if (out_of_range_frac > 0.001) {
    cli::cli_warn(c(
      "{round(100 * out_of_range_frac, 2)}% of WP predictions are outside [0, 1] ",
      "({round(min(preds), 2)} to {round(max(preds), 2)}). Model may be returning ",
      "logits or some other scale rather than probabilities. Inspect ",
      "{.code wp_model$model$params$objective} -- expected {.val binary:logistic}."
    ))
  }
  # Clamp to [0, 1]
  pmax(pmin(preds, 1), 0)
}


#' Add win probability and WPA to SPADL data
#'
#' Adds \code{wp} (model's possession-POV win probability at each action
#' — P(team that performed the action wins the match)) and \code{wpa}
#' (change in the acting team's win probability between the current and
#' the next event) columns.
#'
#' WPA delta accounts for possession switches:
#' \itemize{
#'   \item Same team in possession at t+1 (\code{team_id_next == team_id}):
#'     \code{wpa = wp_next - wp} — both values are P(same team wins)
#'   \item Possession switched at t+1: \code{wp_next} is P(other team wins),
#'     so from the t-team's POV the post-event probability is
#'     \code{(1 - wp_next)}, giving \code{wpa = (1 - wp_next) - wp}
#' }
#'
#' Pre-2026-05-29 implementation took raw \code{wp_next - wp} deltas
#' which silently inflated WPA ~30x once the model was retrained to
#' possession-POV (see \code{C:/dev/pannaverse/panna/debug/demo_wpa_logic.R}
#' for a worked example). Mirrors torpverse's \code{add_variables.R}
#' case_when on \code{team_id_next} vs \code{team_id_mdl}.
#'
#' WPA is centered per-match (\code{wpa - mean(wpa, na.rm=TRUE)} by
#' \code{match_id}) to remove model-calibration bias.
#'
#' @param wp_features SPADL features with WP model features. MUST contain:
#'   \itemize{
#'     \item \code{match_id} — match identifier for centering + shift bounds
#'     \item \code{team_id} — acting team (load-bearing for the WPA POV pivot
#'       on \code{team_id_next})
#'     \item \code{is_home} — POV indicator (currently unused but reserved)
#'     \item \code{wp_label} — last-action fallback target
#'     \item plus the feature columns the model was trained on
#'   }
#'   Missing \code{team_id} silently produces wrong WPA via the
#'   case_when on \code{team_id_next == team_id} — see WPA scale regression
#'   retro at \code{CLAUDE_TODO_WPA_SCALE_REGRESSION.md}.
#' @param wp_model Trained WP model from \code{\link{train_wp_model}}.
#'   Predictions are clamped to \code{[0, 1]} by \code{\link{predict_wp}}.
#'
#' @return The input data.table with added \code{wp} (possession-POV
#'   probability) and \code{wpa} (acting-team-POV delta) columns.
#'
#' @export
add_wp_vars <- function(wp_features, wp_model) {
  required <- c("match_id", "team_id", "is_home", "wp_label")
  missing <- setdiff(required, names(wp_features))
  if (length(missing) > 0L) {
    stop(sprintf("add_wp_vars: wp_features missing required column(s): %s",
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  dt <- data.table::as.data.table(wp_features)

  # The WP model predicts P(possession team wins) — so `wp` at event t
  # is always from the acting team's POV. WPA = how the acting-team-at-t's
  # win probability changed by the next event.
  #
  # Two cases:
  #   (a) Same team still in possession at t+1 -> `wp_next` is from the
  #       same POV, so direct delta is correct: wpa = wp_next - wp
  #   (b) Possession switched at t+1 -> `wp_next` is P(other team wins),
  #       so from acting-team-at-t's POV the post-event probability is
  #       (1 - wp_next), giving wpa = (1 - wp_next) - wp
  #
  # This mirrors torp's add_variables.R lines 122-124 which uses the same
  # possession-POV pattern. The PRE-2026-05-29 implementation used raw
  # `wp_next - wp` deltas (case-a only), which silently produced ~0.7
  # WPA swings every possession switch -> 30x season-WPA inflation when
  # the new possession-POV model went live with step 10b on 2026-05-29.
  dt[, wp := predict_wp(wp_model, dt)]

  dt[, wp_next       := data.table::shift(wp, type = "lead"),      by = match_id]
  dt[, team_id_next  := data.table::shift(team_id, type = "lead"), by = match_id]

  # Last action in match: fall back to wp_label (also possession-POV;
  # 1 if acting team won the match). team_id_next defaults to current
  # team_id so the case-a (same-team) branch applies cleanly.
  if ("wp_label" %in% names(dt)) {
    dt[is.na(wp_next),      wp_next      := wp_label]
  } else {
    dt[is.na(wp_next),      wp_next      := wp]  # no change at end
  }
  dt[is.na(team_id_next), team_id_next := team_id]

  # WPA from the acting-team-at-t's perspective.
  # Positive = good for the team that acted at event t.
  dt[, wpa := data.table::fifelse(
    team_id_next == team_id,
    wp_next - wp,
    (1 - wp_next) - wp
  )]

  # Center WPA per match so it sums to zero (removes WP model calibration bias)
  dt[, wpa := wpa - mean(wpa, na.rm = TRUE), by = match_id]

  # Clean up internal columns
  dt[, c("wp_next", "team_id_next") := NULL]

  dt
}


# ============================================================================
# Model persistence
# ============================================================================

#' Save WP model
#'
#' @param wp_model WP model from \code{\link{train_wp_model}}.
#' @param path Directory to save. If NULL, uses \code{pannadata/data/opta/models/}.
#'
#' @return Invisibly returns the file path.
#' @export
save_wp_model <- function(wp_model, path = NULL) {
  if (is.null(path)) {
    path <- file.path(opta_data_dir(), "models")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

  model_path <- file.path(path, "wp_model.rds")
  saveRDS(wp_model, model_path)

  cli::cli_alert_success("Saved WP model to {model_path}")
  invisible(model_path)
}


#' Load WP model
#'
#' Resolution order, matching load_epv_model() / load_xpass_model():
#'   1. Explicit \code{path} (if supplied and the file exists)
#'   2. pannamodels package (preferred — distributes wp_model via the
#'      `epv` release tag, downloaded + cached on first call)
#'   3. Local fallback at \code{pannadata/data/opta/models/wp_model.rds}
#'
#' @param path Directory to load from. If NULL, tries pannamodels first
#'   then falls back to \code{pannadata/data/opta/models/}.
#'
#' @return WP model list (model + feature_names).
#' @export
load_wp_model <- function(path = NULL) {
  # 1. Explicit path: hard requirement if supplied. A caller passing
  # `path = "/my/training/output"` because they want THAT model is silently
  # ignored if we fall through to pannamodels — wrong UX. Abort if the
  # caller's explicit path doesn't exist rather than substituting a
  # different model.
  if (!is.null(path)) {
    model_path <- file.path(path, "wp_model.rds")
    if (!file.exists(model_path)) {
      cli::cli_abort(c(
        "WP model not found at explicit path: {.file {model_path}}",
        "i" = "Drop the {.arg path} argument to fall back to pannamodels + local."
      ))
    }
    cli::cli_alert_success("Loaded WP model from {model_path}")
    return(readRDS(model_path))
  }

  # 2. Try pannamodels (the canonical distribution)
  if (requireNamespace("pannamodels", quietly = TRUE)) {
    model <- tryCatch(
      pannamodels::load_panna_model("wp_model", verbose = FALSE),
      error = function(e) {
        cli::cli_alert_info("pannamodels wp_model lookup failed: {e$message}. Trying local path.")
        NULL
      }
    )
    if (!is.null(model)) {
      cli::cli_alert_success("Loaded WP model from pannamodels")
      return(model)
    }
  }

  # 3. Local fallback
  default_path <- file.path(opta_data_dir(), "models", "wp_model.rds")
  if (file.exists(default_path)) {
    cli::cli_alert_success("Loaded WP model from {default_path}")
    return(readRDS(default_path))
  }

  cli::cli_abort(c(
    "WP model not found.",
    "i" = "Install pannamodels: devtools::install_github('peteowen1/pannamodels')",
    "i" = "Or train locally: source('data-raw/epv/05_train_wp_model.R')"
  ))
}
