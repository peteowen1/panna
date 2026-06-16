# xGOT (Expected Goals On Target / Post-Shot xG) Model
#
# Companion to xg_model.R. Where xG answers "how good was this CHANCE?",
# xGOT answers "given WHERE the shot ended up in the goal frame, how likely
# was it to be a goal?". It is defined only for ON-TARGET shots (Opta
# type_id 15 = saved, 16 = goal) and trained on the goal-mouth crossing
# point (goalmouth_y / goalmouth_z, Opta qualifiers 102/103).
#
# The decomposition that motivates it (per shot, summed over a player):
#   Goals - xG  =  (xGOT - xG)        +     (Goals - xGOT)
#                  placement/targeting       keeper + luck
#                  -> the SHOOTER's "finds the corners" skill
# Off-target shots have xGOT = 0 (they cannot score), so missing the target
# costs the shooter -xG. A shot genuinely MISSING goalmouth coords gets
# xGOT = NA (surfaced, never imputed to 0).
#
# Design notes:
#  - Feature set = the SAME pre-shot features as xG (so chance quality
#    cancels in xGOT - xG) PLUS placement features from the goal-mouth
#    coordinates. See .create_placement_features().
#  - Training window: goalmouth coords are only complete from the 2021-22
#    season onward (older Opta feeds carry them on ~55% of shots, missing
#    not-at-random). prepare_shots_for_xgot() gates to that window.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
NULL

# --- Goal-frame geometry (empirically derived from where GOALS land) -------
# Goals span goalmouth_y in [45.6, 54.3]; posts sit just outside. Crossbar
# from goals' gm_z 99th pct (~32) up to woodwork hits (~38-42).
GOAL_POST_Y_LEFT  <- 45.2   # near (left) post, 0-100 pitch-width scale
GOAL_POST_Y_RIGHT <- 54.8   # far (right) post
GOAL_POST_Y_MID   <- 50.0   # goal centre
GOAL_CROSSBAR_Z   <- 38.0   # crossbar height on Opta's gm_z scale
# Season from which goalmouth coords are reliably complete (see header).
XGOT_MIN_SEASON_END_YEAR <- 2021L
# On-target Opta shot type_ids (woodwork=14 excluded: never crossed the line).
XGOT_ON_TARGET_TYPE_IDS <- c(15L, 16L)


#' Create goal-mouth placement features
#'
#' Turns the raw goal-mouth crossing point (goalmouth_y, goalmouth_z) into the
#' feature(s) the xGOT model learns from. This is the heart of the model: how
#' you encode "where in the frame" decides what the tree can discover.
#'
#' The empirical signal from EPL 2024-25 on-target shots (your data):
#'   distance to nearest post:  hug-post(<1)=0.358  near=0.248  mid=0.106  central=0.069
#'   height band:               low(<5)=0.386  mid(5-12)=0.284  high(12-20)=0.056  top(>20)=0.287
#' Note the height effect is U-SHAPED (mid-height = keeper's easy reach = worst;
#' both low-and-tucked and top-corner convert well). XGBoost handles
#' non-linearity, so RAW gm_y/gm_z already let it find the corners - but
#' engineered features (distance-to-near-post, height) sharpen it on limited
#' data and make feature-importance readable.
#'
#' Geometry constants available: GOAL_POST_Y_LEFT (45.2), GOAL_POST_Y_RIGHT
#' (54.8), GOAL_POST_Y_MID (50), GOAL_CROSSBAR_Z (38).
#'
#' @param gm_y,gm_z Numeric vectors of goal-mouth y (horizontal) and z
#'   (height) coordinates. May contain NA (off-target / missing).
#' @return data.frame of placement features, one row per shot. Column names
#'   become model features - keep them descriptive (e.g. dist_to_near_post).
#' @keywords internal
.create_placement_features <- function(gm_y, gm_z) {
  # Horizontal distance to the nearer post (small = tucked by a post). The
  # strongest single placement signal in the data (hug-post converts ~5x
  # central). NA in -> NA out (never imputed; prepare step drops those rows).
  dist_to_near_post <- pmin(abs(gm_y - GOAL_POST_Y_LEFT),
                            abs(gm_y - GOAL_POST_Y_RIGHT))

  # "Finding the top corner" = near a post AND near the bar. One feature so a
  # single split can isolate the unsaveable region. y- and z-units differ, so
  # this is an index, not a metric distance - fine for tree splits.
  # (We deliberately do NOT expose `dist_below_bar = 38 - gm_z` as its own
  # feature: it is a perfect linear transform of gm_z, so trees gain nothing
  # from it - importance analysis confirmed it only siphoned gain from gm_z.)
  dist_to_top_corner <- sqrt(dist_to_near_post^2 + (GOAL_CROSSBAR_Z - gm_z)^2)

  data.frame(
    gm_y = gm_y,
    gm_z = gm_z,
    dist_to_near_post = dist_to_near_post,
    dist_to_top_corner = dist_to_top_corner
  )
}


#' Prepare on-target shots for xGOT modeling
#'
#' Filters to on-target shots within the complete goalmouth window, then
#' builds pre-shot features (shared with xG via .create_shot_features()) plus
#' placement features (.create_placement_features()).
#'
#' @param shot_events Data frame from load_opta_shot_events(); must include
#'   goalmouth_y / goalmouth_z (run pannadata backfill_goalmouth.py first).
#' @param min_season_end_year Earliest season end-year to keep (default 2021).
#' @return Data frame of features + target is_goal, ready for fit_xgot_model().
#' @keywords internal
prepare_shots_for_xgot <- function(shot_events,
                                   min_season_end_year = XGOT_MIN_SEASON_END_YEAR) {
  if (is.null(shot_events) || nrow(shot_events) == 0) {
    cli::cli_abort("No shot events provided")
  }
  required <- c("x", "y", "is_goal", "type_id", "goalmouth_y", "goalmouth_z")
  missing <- setdiff(required, names(shot_events))
  if (length(missing)) {
    cli::cli_abort(c(
      "Missing required columns: {paste(missing, collapse=', ')}",
      "i" = "goalmouth_y/z come from pannadata backfill_goalmouth.py + the
             updated scraper."
    ))
  }

  # On-target only.
  on_target <- shot_events$type_id %in% XGOT_ON_TARGET_TYPE_IDS
  shot_events <- shot_events[on_target, , drop = FALSE]

  # Complete-window gate (goalmouth coords reliable only from 2021-22+).
  if ("season" %in% names(shot_events)) {
    ey <- vapply(shot_events$season, extract_season_end_year, numeric(1))
    keep <- !is.na(ey) & ey >= min_season_end_year
    n_drop <- sum(!keep)
    if (n_drop) {
      cli::cli_alert_info(
        "Dropping {n_drop} on-target shot{?s} before {min_season_end_year} (incomplete goalmouth coverage)."
      )
    }
    shot_events <- shot_events[keep, , drop = FALSE]
  }

  # Surface, never impute: on-target shots that still lack placement are a
  # data gap, not a 0 - drop them from TRAINING with a loud count.
  has_gm <- !is.na(shot_events$goalmouth_y) & !is.na(shot_events$goalmouth_z)
  if (any(!has_gm)) {
    cli::cli_warn("{sum(!has_gm)} on-target shot{?s} missing goalmouth coords - dropped from training (not imputed).")
    shot_events <- shot_events[has_gm, , drop = FALSE]
  }
  if (nrow(shot_events) == 0) {
    cli::cli_abort("No on-target shots with goalmouth coords after filtering.")
  }

  cli::cli_alert_info("Preparing {format(nrow(shot_events), big.mark=',')} on-target shots for xGOT...")

  # Pre-shot features (shared with xG so chance quality cancels in xGOT - xG).
  bodypart  <- if ("body_part" %in% names(shot_events)) shot_events$body_part else NULL
  situation <- if ("situation" %in% names(shot_events)) shot_events$situation else NULL
  big_chance <- if ("big_chance" %in% names(shot_events)) as.integer(shot_events$big_chance) else 0L
  features <- .create_shot_features(
    x = shot_events$x, y = shot_events$y,
    bodypart = bodypart, situation = situation, is_big_chance = big_chance
  )

  # Placement features (your contribution).
  placement <- .create_placement_features(shot_events$goalmouth_y, shot_events$goalmouth_z)
  features <- cbind(features, placement)

  # Metadata + penalty flag (penalties excluded at fit, mirroring xG).
  features$match_id <- shot_events$match_id
  features$event_id <- if ("event_id" %in% names(shot_events)) shot_events$event_id else seq_len(nrow(shot_events))
  features$player_id <- shot_events$player_id
  features$player_name <- shot_events$player_name
  features$is_penalty <- if ("situation" %in% names(shot_events)) {
    as.integer(grepl("penalty", tolower(shot_events$situation)))
  } else 0L

  features$is_goal <- as.integer(shot_events$is_goal)
  attr(features, "placement_cols") <- names(placement)
  cli::cli_alert_success("Prepared xGOT features: {sum(features$is_goal)} goals from {nrow(features)} on-target shots")
  features
}


#' Fit xGOT model using XGBoost
#'
#' Mirrors fit_xg_model() (same XGBoost binary:logistic setup) but trains on
#' on-target shots with placement features added. Calibrate target: mean
#' predicted xGOT should approximate the on-target goal rate.
#'
#' @param shot_features Data frame from prepare_shots_for_xgot().
#' @param exclude_penalties Exclude penalties from training (default TRUE).
#' @inheritParams fit_xg_model
#' @return List with model, cv_result, importance, calibration, panna_metadata.
#' @export
fit_xgot_model <- function(shot_features,
                           exclude_penalties = TRUE,
                           nfolds = 5, max_depth = 6, eta = 0.05,
                           subsample = 0.8, colsample_bytree = 0.8,
                           nrounds = 500, early_stopping_rounds = 50,
                           verbose = 1) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. Install with: install.packages('xgboost')")
  }
  if (exclude_penalties && "is_penalty" %in% names(shot_features)) {
    shot_features <- shot_features[shot_features$is_penalty == 0, ]
    cli::cli_alert_info("Excluded penalties, {nrow(shot_features)} on-target shots remaining")
  }

  base_cols <- c(
    "x", "y", "distance_to_goal", "angle_to_goal",
    "in_penalty_area", "in_six_yard_box",
    "is_header", "is_right_foot", "is_left_foot",
    "is_open_play", "is_set_piece", "is_corner", "is_direct_freekick",
    "is_big_chance"
  )
  placement_cols <- attr(shot_features, "placement_cols")
  feature_cols <- c(base_cols, placement_cols)
  available_features <- intersect(feature_cols, names(shot_features))
  if (length(intersect(placement_cols, available_features)) == 0) {
    cli::cli_abort("No placement features present - did .create_placement_features() run?")
  }
  cli::cli_alert_info("Fitting xGOT with {length(available_features)} features ({length(placement_cols)} placement) on {nrow(shot_features)} shots...")

  X <- as.matrix(shot_features[, available_features, drop = FALSE])
  y <- shot_features$is_goal
  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]; y <- y[complete_idx]
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)

  params <- list(
    objective = "binary:logistic", eval_metric = "logloss",
    max_depth = max_depth, eta = eta, subsample = subsample,
    colsample_bytree = colsample_bytree, min_child_weight = 10
  )
  cv_result <- xgboost::xgb.cv(
    params = params, data = dtrain, nrounds = nrounds, nfold = nfolds,
    early_stopping_rounds = early_stopping_rounds, verbose = verbose,
    print_every_n = 50, prediction = TRUE
  )
  best_nrounds <- cv_result$best_iteration
  if (is.null(best_nrounds) || length(best_nrounds) == 0) {
    best_nrounds <- which.min(cv_result$evaluation_log$test_logloss_mean)
  }
  best_logloss <- cv_result$evaluation_log$test_logloss_mean[best_nrounds]
  cli::cli_alert_info("xGOT CV: best iteration = {best_nrounds}, CV LogLoss = {round(best_logloss, 4)}")

  final_model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = best_nrounds, verbose = 0)
  y_pred <- cv_result$pred
  if (is.null(y_pred) || length(y_pred) == 0) y_pred <- stats::predict(final_model, dtrain)
  calibration <- calculate_xg_calibration(y, y_pred)
  importance <- xgboost::xgb.importance(feature_names = available_features, model = final_model)

  cli::cli_alert_success(paste0(
    "xGOT model fit complete. LogLoss: ", round(best_logloss, 4),
    ", Mean xGOT: ", round(mean(y_pred), 4),
    ", On-target goal rate: ", round(mean(y), 4)
  ))

  result <- list(
    model = final_model, cv_result = cv_result, importance = importance,
    calibration = calibration, best_nrounds = best_nrounds, best_logloss = best_logloss,
    panna_metadata = list(
      type = "xgot_model", feature_cols = available_features,
      placement_cols = placement_cols, n_shots = length(y), n_goals = sum(y),
      goal_rate = mean(y), params = params, exclude_penalties = exclude_penalties,
      min_season_end_year = XGOT_MIN_SEASON_END_YEAR
    )
  )
  class(result) <- c("xgot_model", "list")
  result
}


#' Predict xGOT for prepared shot features
#'
#' @param xgot_model Fitted model from fit_xgot_model().
#' @param shot_features Data frame with the model's feature columns.
#' @return Numeric vector of xGOT predictions.
#' @export
predict_xgot <- function(xgot_model, shot_features) {
  feature_cols <- xgot_model$panna_metadata$feature_cols
  missing_cols <- setdiff(feature_cols, names(shot_features))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "xGOT prediction: missing feature{?s}: {paste(missing_cols, collapse=', ')}",
      "i" = "Placement features must be built with .create_placement_features() first."
    ))
  }
  X <- as.matrix(shot_features[, feature_cols, drop = FALSE])
  as.numeric(stats::predict(xgot_model$model, X))
}


#' Add xGOT to SPADL Actions
#'
#' Adds post-shot xG to shot actions. xGOT is defined only for ON-TARGET
#' shots, so this needs the goal-mouth crossing point, which SPADL drops -
#' it is joined back from \code{goalmouth_lookup} via the preserved
#' \code{original_event_id}. Assignment:
#'   on-target + coords    -> model prediction
#'   on-target, no coords  -> NA  (surfaced, never imputed)
#'   off-target            -> 0   (cannot score)
#'   non-shot / unmatched  -> NA
#'
#' @param spadl_actions SPADL actions data frame (must carry
#'   \code{original_event_id} and \code{match_id}).
#' @param xgot_model Fitted xGOT model.
#' @param goalmouth_lookup Data frame keyed by (\code{match_id},
#'   \code{event_id}) with \code{type_id}, \code{goalmouth_y},
#'   \code{goalmouth_z} for shot events - e.g. from match_events /
#'   opta_shot_events. (Opta q102/103 live in match_events qualifier_json, so
#'   no backfill is needed to build this on the inference path.)
#' @return SPADL actions with an \code{xgot} column added.
#' @keywords internal
add_xgot_to_spadl <- function(spadl_actions, xgot_model, goalmouth_lookup) {
  spadl_actions$xgot <- NA_real_
  spadl_actions$shot_on_target <- NA   # logical; set for shots below
  shot_idx <- which(spadl_actions$action_type == "shot")
  if (length(shot_idx) == 0) {
    cli::cli_warn("No shots found in SPADL actions")
    return(spadl_actions)
  }
  if (!all(c("match_id", "original_event_id") %in% names(spadl_actions))) {
    cli::cli_abort("spadl_actions must carry match_id and original_event_id to join goalmouth coords.")
  }

  shots <- spadl_actions[shot_idx, ]
  key <- data.frame(
    match_id = shots$match_id,
    event_id = shots$original_event_id,
    stringsAsFactors = FALSE
  )
  lk <- goalmouth_lookup[, c("match_id", "event_id", "type_id",
                             "goalmouth_y", "goalmouth_z")]
  joined <- merge(key, lk, by = c("match_id", "event_id"),
                  all.x = TRUE, sort = FALSE)
  # merge() may reorder; realign to shots order via a stable key match.
  ord <- match(paste(key$match_id, key$event_id),
               paste(joined$match_id, joined$event_id))
  joined <- joined[ord, ]

  on_target <- joined$type_id %in% XGOT_ON_TARGET_TYPE_IDS
  has_gm <- !is.na(joined$goalmouth_y) & !is.na(joined$goalmouth_z)
  predable <- on_target & has_gm
  predable[is.na(predable)] <- FALSE

  xgot_vec <- rep(NA_real_, nrow(shots))
  xgot_vec[!is.na(on_target) & !on_target] <- 0   # off-target: cannot score

  if (any(predable)) {
    is_big_chance <- if ("is_big_chance" %in% names(shots)) as.integer(shots$is_big_chance[predable]) else 0L
    bodypart <- if ("bodypart" %in% names(shots)) shots$bodypart[predable] else NULL
    base <- .create_shot_features(
      x = shots$start_x[predable], y = shots$start_y[predable],
      bodypart = bodypart, situation = NULL, is_big_chance = is_big_chance
    )
    plc <- .create_placement_features(joined$goalmouth_y[predable], joined$goalmouth_z[predable])
    xgot_vec[predable] <- predict_xgot(xgot_model, cbind(base, plc))
  }

  spadl_actions$xgot[shot_idx] <- xgot_vec
  spadl_actions$shot_on_target[shot_idx] <- on_target  # TRUE/FALSE/NA per raw type_id
  n_pred <- sum(predable)
  n_na_ot <- sum(on_target & !has_gm, na.rm = TRUE)
  cli::cli_alert_success(
    "Added xGOT to {length(shot_idx)} shots ({n_pred} on-target scored, {n_na_ot} on-target missing coords -> NA, rest off-target -> 0)"
  )
  spadl_actions
}


#' Load Pre-trained xGOT Model
#'
#' @param path Optional path to a model RDS. If NULL, tries pannamodels then
#'   the local pannadata models dir (mirrors load_xg_model()).
#' @return Fitted xGOT model, or NULL if unavailable.
#' @export
load_xgot_model <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) {
    cli::cli_alert_success("Loaded xGOT model from {path}")
    return(readRDS(path))
  }
  if (requireNamespace("pannamodels", quietly = TRUE)) {
    model <- tryCatch(pannamodels::load_panna_model("xgot_model", verbose = FALSE),
                      error = function(e) NULL)
    if (!is.null(model)) {
      cli::cli_alert_success("Loaded xGOT model from pannamodels")
      return(model)
    }
  }
  local_path <- file.path(opta_data_dir(), "models", "xgot_model.rds")
  if (file.exists(local_path)) {
    cli::cli_alert_success("Loaded xGOT model from {local_path}")
    return(readRDS(local_path))
  }
  cli::cli_warn("xGOT model not found (pannamodels or {local_path}). Run the EPV pipeline with goalmouth-enabled shots.")
  NULL
}
