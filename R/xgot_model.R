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
#' The empirical signal from EPL 2024-25 on-target shots (illustrative):
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
#'   Should also include is_blocked (run pannadata
#'   backfill_blocked_shots.py) to exclude blocked shots from on-target;
#'   without it, blocked shots remain in training with placeholder
#'   goalmouth coords (panna#176).
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

  # On-target only. type_id==15 ("Attempt Saved") is an umbrella Opta uses
  # for BOTH real keeper saves and shots blocked by an outfield defender
  # before reaching the goal frame -- a blocked shot never crosses the
  # goal-line plane, so its goalmouth_y/z is a placeholder (typically the
  # frame-height midpoint, ~98% of blocked shots land on goalmouth_z==19),
  # not a real crossing point. Matches OPTA_REFERENCE.md's on-target
  # formula: (type 15 & !q82) | type 16 (panna#176).
  on_target <- shot_events$type_id %in% XGOT_ON_TARGET_TYPE_IDS
  if ("is_blocked" %in% names(shot_events)) {
    is_blocked <- shot_events$is_blocked %in% TRUE
    if (any(on_target & is_blocked)) {
      cli::cli_alert_info("Excluding {sum(on_target & is_blocked)} blocked shot{?s} (q82) from xGOT on-target population.")
    }
    on_target <- on_target & !is_blocked
  } else {
    cli::cli_warn("shot_events lacks is_blocked -- blocked shots (Attempt Saved with q82) will remain in training with placeholder goalmouth coords; re-sync opta_shot_events.parquet for qualifier-based exclusion.")
  }
  shot_events <- shot_events[on_target, , drop = FALSE]

  # Drop own goals from TRAINING: they enter as guaranteed-goal rows with
  # degenerate pre-shot features (the xG model learned exactly this pattern
  # -- ~0.98 at own-end coords, see epv_model.R's own-goal override).
  # pannadata#105: prefer the real Opta qualifier-28 marker (`is_own_goal`,
  # scraped/backfilled onto opta_shot_events.parquet) over the positional
  # type-16-at-x<50 heuristic -- same upgrade already made on the serving
  # path in add_xgot_to_spadl() (#148). Positional fallback for any
  # shot_events snapshot that predates the backfill.
  if ("is_own_goal" %in% names(shot_events)) {
    is_og <- shot_events$is_own_goal %in% TRUE
  } else {
    cli::cli_warn("shot_events lacks is_own_goal -- using positional own-goal fallback (type-16 goal at x < 50); re-sync opta_shot_events.parquet for qualifier-based detection.")
    is_og <- shot_events$type_id == 16L & !is.na(shot_events$x) & shot_events$x < 50
  }
  if (any(is_og)) {
    cli::cli_alert_info("Dropping {sum(is_og)} own goal{?s} from xGOT training.")
    shot_events <- shot_events[!is_og, , drop = FALSE]
  }

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

  # Placement features.
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
#' @family epv
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
    "is_open_play", "is_set_piece", "is_corner",
    # is_direct_freekick removed 2026-09-03: constant 0 on every shot
    # (no Opta `situation` value contains "free"). See .create_shot_features().
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
#' @family epv
#' @export
predict_xgot <- function(xgot_model, shot_features) {
  feature_cols <- xgot_model$panna_metadata$feature_cols
  missing_cols <- setdiff(feature_cols, names(shot_features))

  # Back-compat for models trained BEFORE 2026-09-03. `is_direct_freekick` was
  # removed that day because it was constant 0 on all 3,289,256 shots (no Opta
  # `situation` value contains "free"), so any xgboost split on it is
  # unreachable and restoring it as 0 reproduces the trained model EXACTLY.
  # This is not imputation - it is replaying a value that was never anything
  # else. Scoped to this one named column so a genuinely missing feature (a
  # real placement column, say) still aborts below.
  # TODO: retrain xgot_model without it, then delete this block. Held back
  # deliberately today so the stage-1 rebuild changes xG ONLY and its
  # calibration result stays attributable.
  if ("is_direct_freekick" %in% missing_cols) {
    cli::cli_warn(c(
      "xGOT model is a pre-2026-09-03 build that still lists {.field is_direct_freekick}.",
      "i" = "Restoring it as constant 0 (its only value in training). Retrain to drop it."
    ))
    shot_features$is_direct_freekick <- 0
    missing_cols <- setdiff(missing_cols, "is_direct_freekick")
  }

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
#'   \code{goalmouth_z}, \code{situation}, \code{is_blocked} and
#'   \code{body_part} for shot events - e.g. from match_events /
#'   opta_shot_events. Pass it via \code{load_opta_shot_events()}, NOT by
#'   reading the parquet directly: that returns \code{event_id} as integer64 and
#'   \code{merge()} against SPADL's numeric \code{original_event_id} matches
#'   nothing (a guard now aborts on this rather than reporting "0 scored").
#'   \code{body_part} is required for the header and footedness features -
#'   SPADL's own \code{bodypart} says "foot" for every shot. \code{situation} is
#'   required to avoid train/serve skew (the model trained on real
#'   situations); without it, set-piece/corner/free-kick shots are scored as
#'   open-play. \code{is_blocked} excludes shots blocked by an outfield
#'   defender (q82) from on-target, matching training (panna#176); without
#'   it, blocked shots are scored as real on-target attempts.
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
  # `situation` is needed to match training features (open/set-piece/corner/
  # free-kick); warn loudly if absent rather than silently skew predictions.
  have_situation <- "situation" %in% names(goalmouth_lookup)
  if (!have_situation) {
    cli::cli_warn("goalmouth_lookup lacks `situation` - set-piece shots will be scored as open-play (train/serve skew).")
  }
  have_is_blocked <- "is_blocked" %in% names(goalmouth_lookup)
  if (!have_is_blocked) {
    cli::cli_warn("goalmouth_lookup lacks `is_blocked` - blocked shots (Attempt Saved with q82) will be scored as real on-target attempts (train/serve skew, panna#176).")
  }
  # `body_part` matters as much as `situation` did. prepare_shots_for_xgot()
  # trains on shot_events$body_part (RightFoot / LeftFoot / Head), but SPADL's
  # own `bodypart` is a stub that says "foot" for every shot -- see
  # map_opta_bodypart() -- so reading it here left is_header, is_right_foot and
  # is_left_foot constant 0 at inference while training saw the real values.
  # Same train/serve skew that cost +6.30% on xG (2026-09-03).
  have_body_part <- "body_part" %in% names(goalmouth_lookup)
  if (!have_body_part) {
    cli::cli_alert_warning(
      "goalmouth_lookup lacks `body_part` - headers scored as foot shots (train/serve skew).")
    cli::cli_warn("goalmouth_lookup lacks `body_part` - header/footedness features are dead.")
  }
  lk_cols <- c("match_id", "event_id", "type_id", "goalmouth_y", "goalmouth_z",
               if (have_situation) "situation",
               if (have_is_blocked) "is_blocked",
               if (have_body_part) "body_part")
  lk <- goalmouth_lookup[, lk_cols, drop = FALSE]
  # De-dup on the join key: a duplicated (match_id, event_id) would let merge()
  # inflate rows and misalign coords to the wrong shot (silent wrong xGOT).
  lk <- lk[!duplicated(lk[, c("match_id", "event_id")]), , drop = FALSE]

  joined <- merge(key, lk, by = c("match_id", "event_id"),
                  all.x = TRUE, sort = FALSE)
  # merge() may reorder; realign to shots order via a stable key match.
  ord <- match(paste(key$match_id, key$event_id),
               paste(joined$match_id, joined$event_id))
  joined <- joined[ord, ]
  stopifnot(nrow(joined) == nrow(key))   # 1:1 invariant (guaranteed by de-dup)

  # A join that matches NOTHING is indistinguishable from "every shot was
  # off-target": both end with xgot 0/NA and a cheerful "0 scored" in the log.
  # It happens easily -- `event_id` is integer64 straight from the parquet but
  # numeric via load_opta_shot_events(), and merge() across those two types
  # matches 0% without complaint (confirmed 2026-09-03 on EPL 2025-2026: 0.0%
  # by merge, 100% by paste). Fail loudly instead.
  matched <- mean(!is.na(joined$type_id))
  if (matched < 0.5) {
    cli::cli_abort(c(
      "goalmouth_lookup matched only {round(100 * matched, 1)}% of {nrow(key)} shots.",
      "x" = "xGOT would be silently empty rather than wrong-looking.",
      "i" = "Check the join key types: {.field event_id} is integer64 from the raw parquet and numeric via {.fn load_opta_shot_events}."
    ))
  }
  cli::cli_alert_info("Goalmouth lookup matched {round(100 * matched, 1)}% of shots")

  # NA-preserving: `%in%` collapses a missing type_id (unmatched shot) to
  # FALSE, which would mislabel it off-target (xgot=0) instead of unknown (NA).
  on_target <- ifelse(is.na(joined$type_id), NA,
                      joined$type_id %in% XGOT_ON_TARGET_TYPE_IDS)
  # Blocked shots (q82) never crossed the goal-line plane -- exclude them the
  # same way training does (panna#176). Matches OPTA_REFERENCE.md's on-target
  # formula: (type 15 & !q82) | type 16.
  if (have_is_blocked) {
    on_target <- on_target & !(joined$is_blocked %in% TRUE)
  }
  has_gm <- !is.na(joined$goalmouth_y) & !is.na(joined$goalmouth_z)
  predable <- on_target & has_gm
  predable[is.na(predable)] <- FALSE

  xgot_vec <- rep(NA_real_, nrow(shots))
  xgot_vec[!is.na(on_target) & !on_target] <- 0   # off-target: cannot score

  if (any(predable)) {
    is_big_chance <- if ("is_big_chance" %in% names(shots)) as.integer(shots$is_big_chance[predable]) else 0L
    # joined$body_part (Opta's own), NOT shots$bodypart (the SPADL stub).
    bodypart <- if (have_body_part) joined$body_part[predable] else NULL
    # Real situation (not NULL) so set-piece/corner/FK shots match training.
    situation <- if (have_situation) joined$situation[predable] else NULL
    base <- .create_shot_features(
      x = shots$start_x[predable], y = shots$start_y[predable],
      bodypart = bodypart, situation = situation, is_big_chance = is_big_chance
    )
    plc <- .create_placement_features(joined$goalmouth_y[predable], joined$goalmouth_z[predable])
    xgot_vec[predable] <- predict_xgot(xgot_model, cbind(base, plc))
  }

  # Own-goal guard: goal-mouth placement is meaningless for the "shooter" ->
  # NA, mirroring the own-goal xG convention (CLAUDE.md). Prefer the explicit
  # Opta qualifier-28 marker (optional SPADL column); the positional fallback
  # (type-16 goal at the scorer's own end) only covers stale cached SPADL that
  # predates the column -- it misreads a legitimate goal logged past halfway
  # (#148).
  pos_og <- joined$type_id == 16L & !is.na(shots$start_x) & shots$start_x < 50
  pos_og[is.na(pos_og)] <- FALSE
  if (!"is_own_goal" %in% names(shots)) {
    cli::cli_warn("spadl_actions lacks is_own_goal - using positional own-goal fallback (type-16 goal at start_x < 50); rebuild the SPADL cache for qualifier-based detection.")
    is_og <- pos_og
  } else {
    is_og <- shots$is_own_goal %in% TRUE
    # rbindlist(fill=TRUE) over mixed-vintage SPADL chunks yields NA for rows
    # from caches that predate the column -- fall back per-row there rather
    # than silently treating NA as "not an own goal".
    og_na <- is.na(shots$is_own_goal)
    if (any(og_na)) {
      cli::cli_warn("{sum(og_na)} shot{?s} carry NA is_own_goal (mixed-vintage SPADL cache?) - positional own-goal fallback applied to those rows.")
      is_og <- is_og | (og_na & pos_og)
    }
  }
  xgot_vec[is_og] <- NA_real_

  spadl_actions$xgot[shot_idx] <- xgot_vec
  spadl_actions$shot_on_target[shot_idx] <- on_target  # TRUE/FALSE/NA per type_id, blocked shots excluded (panna#176)
  n_pred <- sum(predable)
  n_na_ot <- sum(on_target & !has_gm, na.rm = TRUE)
  cli::cli_alert_success(
    "Added xGOT to {length(shot_idx)} shots ({n_pred} scored, {n_na_ot} on-target no-coords -> NA, {sum(is_og)} own-goal -> NA, rest off-target -> 0)"
  )
  spadl_actions
}


#' Load Pre-trained xGOT Model
#'
#' @param path Optional path to a model RDS. If NULL, tries pannamodels then
#'   the local pannadata models dir (mirrors load_xg_model()).
#' @return Fitted xGOT model, or NULL if unavailable.
#' @family epv
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
