# xDuel — Expected Duel models (5 contest sub-models)
#
# Player-AGNOSTIC contextual win-probability for physical contests, mirroring the
# xPass / xG pattern. FIVE separately-calibrated sub-models, each unbiased on its
# own base rate. Every player-match gets an ABOVE-EXPECTED count (won − Σ context
# win-prob) per model — the volume-correct replacement for the scale-free accuracy
# RATIOS (duel/aerial/tackle_success — 1/1 == 10/10) in PSR/PSV.
#
#   1. aerial_win   (Opta 44)  — win the header. Balanced 50/50 (both participants
#                                recorded, mirrored coords; pair P(win) sums to 1).
#   2. aerial_poss  (44 won → next event) — keep the ball after winning a header.
#                                Defender-ish: a CB winning one in his own box keeps
#                                it ~0.61 vs ~0.95 in attack.
#   3. takeon       (Opta 3)   — attacker beats his man. FOUL-AWARE: a take-on that
#                                "loses" but the dribbler wins a free kick ≤2s later
#                                is excluded (don't punish a foul-drawing dribble).
#   4. tackle_poss  (Opta 7)   — win the BALL when you tackle (outcome 1), not just
#                                stop the carrier.
#   5. containment  (3 ↔ defender) — defender's side of the dribble. Anchored on the
#                                take-on outcome, attributed to the opponent who made
#                                the stop. CONTAINED (win) = take-on lost & defender
#                                made a stop (tackle/interception/clearance/recovery/
#                                smother/blocked). BEATEN (loss) = take-on won (paired
#                                challenge 45) OR take-on lost via a conceded foul.
#                                Genuine attacker error (ball runs out, no opponent
#                                event) is left unattributed.
#
# Coordinates are team-relative Opta 0-100 (x = toward the actor's attacking goal),
# so location already encodes attacking-vs-defending. Containment/aerial-possession
# need the FULL event stream (any-type next event + defensive stops), so callers
# pass complete per-league events. See PSV_EFFICIENCY_REDESIGN_PLAN.md +
# PLAYER_BASED_SUCCESS_MODELS_IDEA.md.

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT .SD .N := fifelse as.data.table
NULL

.DUEL_AERIAL_TYPE    <- 44L
.DUEL_TAKEON_TYPE    <- 3L
.DUEL_TACKLE_TYPE    <- 7L
.DUEL_FOUL_TYPE      <- 4L
.DUEL_CHALLENGE_TYPE <- 45L
# Defender "stopped the dribble" events (the contained side of a take-on loss):
#   7 tackle, 8 interception, 12 clearance, 49 ball recovery, 54 smother, 74 blocked,
#   + keeper stops (52 pick-up, 11 claim, 41 punch), 61 ball-touch, 56 shield-out.
# Anything not in here (opponent just passes next, ball runs out) is left
# unattributed — no clear defender to credit.
.DUEL_STOP_TYPES <- c(7L, 8L, 12L, 49L, 54L, 74L, 52L, 11L, 41L, 61L, 56L)

# Four context features (team-relative coords). Binary third/box flags dropped (~0 gain).
.DUEL_FEATURE_COLS <- c("start_x", "start_y", "dist_own_goal", "dist_opp_goal")

# Contest -> output-column prefix (prefix_woe is the value-model feature).
.DUEL_CONTESTS <- c(aerial_win = "aerial", aerial_poss = "aerial_poss",
                    takeon = "takeon", tackle_poss = "tackle_poss",
                    containment = "containment")


#' Build duel contest features (internal helper)
#' @param dt data.table with `x`, `y`.
#' @return `dt` with the four context feature columns added.
#' @keywords internal
.create_duel_features <- function(dt) {
  dt[, `:=`(
    start_x = as.numeric(x),
    start_y = as.numeric(y),
    dist_own_goal = sqrt(x^2 + (y - 50)^2),
    dist_opp_goal = sqrt((100 - x)^2 + (y - 50)^2)
  )]
  dt
}


# Time-order a per-league event table (chronological within match/period) and add
# time_seconds + a stable original-order tiebreak. One copy; reused by all preps.
.order_events <- function(events) {
  dt <- data.table::as.data.table(events)
  if (!"time_seconds" %in% names(dt)) {
    if (all(c("minute", "second") %in% names(dt))) {
      dt[, time_seconds := as.integer(minute) * 60L + as.integer(second)]
    } else {
      dt[, time_seconds := 0L]
    }
  }
  dt[, .ord0 := .I]
  if (!"period_id" %in% names(dt)) dt[, period_id := 1L]
  data.table::setorder(dt, match_id, period_id, time_seconds, .ord0)
  dt
}


# Keep only key + feature + label cols and scrub NA/Inf features.
.finalize_duel <- function(out) {
  if (is.null(out) || nrow(out) == 0) return(out)
  keep <- intersect(c("match_id", "player_id", "team_id", .DUEL_FEATURE_COLS, "won"),
                    names(out))
  out <- out[, ..keep]
  for (col in .DUEL_FEATURE_COLS) {
    if (col %in% names(out)) {
      data.table::set(out, which(is.na(out[[col]]) | is.infinite(out[[col]])), col, 0)
    }
  }
  out[]
}


# --- the five contest preps (operate on an ORDERED event table) --------------

.prep_aerial_win <- function(dt) {
  idx <- which(dt$type_id == .DUEL_AERIAL_TYPE & !is.na(dt$x) & !is.na(dt$y) &
                 !is.na(dt$outcome))
  if (!length(idx)) return(dt[0])
  d <- .create_duel_features(dt[idx])
  d[, won := as.integer(outcome == 1L)]
  d
}

.prep_tackle_poss <- function(dt) {
  idx <- which(dt$type_id == .DUEL_TACKLE_TYPE & !is.na(dt$x) & !is.na(dt$y) &
                 !is.na(dt$outcome))
  if (!length(idx)) return(dt[0])
  d <- .create_duel_features(dt[idx])
  d[, won := as.integer(outcome == 1L)]   # won possession
  d
}

# Aerial possession: of headers WON, did the team keep the ball? "Kept" = the NEXT
# on-ball POSSESSION event (pass/shot/take-on) belongs to the winner's team — carried
# backward, so it skips the simultaneous mirror loss, clearances, recoveries, etc.
# A defensive clearance-header therefore does NOT count as retention (the cleared ball
# usually goes to the opponent) — that is the ~0.61-in-own-box signal.
.DUEL_POSSESSION_TYPES <- c(1L, 2L, 3L, 13L, 14L, 15L, 16L)  # pass, offside pass, take-on, shots
.prep_aerial_poss <- function(dt) {
  tcode <- as.integer(factor(dt$team_id))
  np <- data.table::fifelse(dt$type_id %in% .DUEL_POSSESSION_TYPES, tcode, NA_integer_)
  tmp <- data.table::data.table(.m = dt$match_id, np = np)
  tmp[, next_poss := data.table::nafill(np, type = "nocb"), by = .m]
  next_poss <- tmp$next_poss
  idx <- which(dt$type_id == .DUEL_AERIAL_TYPE & dt$outcome == 1L &
                 !is.na(dt$x) & !is.na(dt$y) & !is.na(next_poss))
  if (!length(idx)) return(dt[0])
  d <- .create_duel_features(dt[idx])
  d[, won := as.integer(next_poss[idx] == tcode[idx])]
  d
}

# Take-on (attacker), foul-aware: drop losses the same player converts into a won
# free kick within 2s (so a foul-drawing dribble isn't scored a failure).
.prep_takeon <- function(dt) {
  idx <- which(dt$type_id == .DUEL_TAKEON_TYPE & !is.na(dt$x) & !is.na(dt$y) &
                 !is.na(dt$outcome))
  if (!length(idx)) return(dt[0])
  d <- dt[idx]
  d[, .rid := .I]
  fw <- dt[type_id == .DUEL_FOUL_TYPE & outcome == 1L,
           .(match_id, period_id, player_id, ft = time_seconds)]
  if (nrow(fw) > 0) {
    loss <- d[outcome == 0L, .(match_id, period_id, player_id,
                               ts = time_seconds, .rid)]
    m <- fw[loss, on = .(match_id, period_id, player_id),
            allow.cartesian = TRUE, nomatch = 0L]
    m <- m[abs(ft - ts) <= 2L]
    if (nrow(m) > 0) d <- d[!(.rid %in% unique(m$.rid))]
  }
  d[, .rid := NULL]
  d <- .create_duel_features(d)
  d[, won := as.integer(outcome == 1L)]
  d
}

# Containment (defender): anchor on each take-on, attribute to the opponent who made
# the stop in a nearby row (±4 rows, ±3s, mirror location |x_to+x_def-100|<20, opposite team).
.prep_containment <- function(dt) {
  n <- nrow(dt)
  is_to <- dt$type_id == .DUEL_TAKEON_TYPE & !is.na(dt$x) & !is.na(dt$y) &
    !is.na(dt$outcome)
  mid <- dt$match_id; per <- dt$period_id; ts <- dt$time_seconds; tid <- dt$team_id
  typ <- dt$type_id; xx <- dt$x; yy <- dt$y; out_ <- dt$outcome; plid <- dt$player_id
  def_pid <- rep(NA_character_, n); def_tid <- rep(NA_character_, n)
  def_typ <- rep(NA_integer_, n);   def_x <- rep(NA_real_, n); def_y <- rep(NA_real_, n)
  cand_types <- c(.DUEL_STOP_TYPES, .DUEL_FOUL_TYPE, .DUEL_CHALLENGE_TYPE)
  for (k in c(1L, -1L, 2L, -2L, 3L, -3L, 4L, -4L)) {   # nearest neighbours first
    j <- seq_len(n) + k
    ok <- is_to & is.na(def_pid) & j >= 1 & j <= n
    ii <- which(ok); jj <- j[ii]
    good <- mid[ii] == mid[jj] & per[ii] == per[jj] & abs(ts[ii] - ts[jj]) <= 3L &
      tid[ii] != tid[jj] & typ[jj] %in% cand_types & abs(xx[ii] + xx[jj] - 100) < 20
    good[is.na(good)] <- FALSE
    hi <- ii[good]; hj <- jj[good]
    def_pid[hi] <- plid[hj]; def_tid[hi] <- tid[hj]; def_typ[hi] <- typ[hj]
    def_x[hi] <- xx[hj]; def_y[hi] <- yy[hj]
  }
  sel <- which(is_to & !is.na(def_pid))
  if (!length(sel)) return(dt[0])
  out <- data.table::data.table(
    match_id = mid[sel], player_id = def_pid[sel], team_id = def_tid[sel],
    x = def_x[sel], y = def_y[sel], to_won = as.integer(out_[sel] == 1L),
    def_typ = def_typ[sel]
  )
  out <- .create_duel_features(out)
  # contained = take-on LOST and the defender made a STOP (not a foul, not a challenge)
  out[, won := as.integer(to_won == 0L & def_typ %in% .DUEL_STOP_TYPES)]
  out
}

.DUEL_PREP_FUNS <- list(
  aerial_win = .prep_aerial_win, aerial_poss = .prep_aerial_poss,
  takeon = .prep_takeon, tackle_poss = .prep_tackle_poss,
  containment = .prep_containment
)


#' Prepare one duel contest from raw (per-league) events
#'
#' @param events Raw Opta events (`type_id`, `outcome`, `x`, `y`, `player_id`,
#'   `team_id`, `match_id`, `period_id`, `minute`/`second`). Pass the FULL event
#'   stream — `aerial_poss`/`containment` look at neighbouring rows of any type.
#' @param contest One of `aerial_win`, `aerial_poss`, `takeon`, `tackle_poss`,
#'   `containment`.
#' @return data.table of features + `won`, keyed columns retained.
#' @export
prepare_duels_from_events <- function(events, contest = names(.DUEL_CONTESTS)) {
  contest <- match.arg(contest)
  dt <- .order_events(events)
  .finalize_duel(.DUEL_PREP_FUNS[[contest]](dt))
}


#' Build the per-contest feature tables for one league's events (memory-safe)
#'
#' Orders the event stream ONCE and extracts all five (small) contest tables, so a
#' caller can loop leagues and discard raw events between iterations.
#' @param events Full per-league Opta events.
#' @return Named list of five finalized feature tables.
#' @export
compute_all_duel_preps <- function(events) {
  dt <- .order_events(events)
  out <- lapply(names(.DUEL_CONTESTS), function(cst) .finalize_duel(.DUEL_PREP_FUNS[[cst]](dt)))
  names(out) <- names(.DUEL_CONTESTS)
  out
}


# Fit one XGBoost contest model (internal).
.fit_contest_model <- function(features, label, nfolds, max_depth, eta,
                               subsample, colsample_bytree, nrounds,
                               early_stopping_rounds, verbose) {
  feature_cols <- intersect(.DUEL_FEATURE_COLS, names(features))
  cli::cli_alert_info(
    "Fitting x{label} model ({length(feature_cols)} feats) on ",
    "{format(nrow(features), big.mark=',')} contests..."
  )
  X <- as.matrix(as.data.frame(features)[, feature_cols, drop = FALSE])
  y <- features$won
  ok <- stats::complete.cases(X, y); X <- X[ok, , drop = FALSE]; y <- y[ok]

  dtrain <- xgboost::xgb.DMatrix(data = X, label = y)
  params <- list(objective = "binary:logistic", eval_metric = "logloss",
                 max_depth = max_depth, eta = eta, subsample = subsample,
                 colsample_bytree = colsample_bytree, min_child_weight = 10)
  cv <- xgboost::xgb.cv(params = params, data = dtrain, nrounds = nrounds,
                        nfold = nfolds, early_stopping_rounds = early_stopping_rounds,
                        verbose = verbose, print_every_n = 50)
  best <- cv$best_iteration
  if (is.null(best) || length(best) == 0) best <- which.min(cv$evaluation_log$test_logloss_mean)
  model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = best, verbose = 0)
  y_pred <- stats::predict(model, dtrain)
  cli::cli_alert_success(paste0(
    "x", label, " fit: LogLoss ", round(cv$evaluation_log$test_logloss_mean[best], 4),
    ", mean P(win) ", round(mean(y_pred), 4), " vs actual ", round(mean(y), 4)
  ))
  list(model = model,
       importance = xgboost::xgb.importance(feature_names = feature_cols, model = model),
       feature_cols = feature_cols, best_nrounds = best,
       win_rate = mean(y), n_contests = length(y))
}


#' Fit the xDuel models (five contest sub-models)
#'
#' @param prepped Named list of finalized contest feature tables (from
#'   \code{compute_all_duel_preps}, accumulated across training leagues).
#' @param nfolds,max_depth,eta,subsample,colsample_bytree,nrounds,early_stopping_rounds,verbose
#'   XGBoost controls.
#' @return List of class \code{duel_model} with the five sub-models + metadata.
#' @export
fit_duel_model <- function(prepped, nfolds = 5, max_depth = 5, eta = 0.1,
                           subsample = 0.8, colsample_bytree = 0.8,
                           nrounds = 500, early_stopping_rounds = 30, verbose = 1) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("xgboost package required. install.packages('xgboost')")
  }
  labels <- c(aerial_win = "AerialWin", aerial_poss = "AerialPoss", takeon = "TakeOn",
              tackle_poss = "TacklePoss", containment = "Containment")
  ctl <- list(nfolds = nfolds, max_depth = max_depth, eta = eta, subsample = subsample,
              colsample_bytree = colsample_bytree, nrounds = nrounds,
              early_stopping_rounds = early_stopping_rounds, verbose = verbose)
  models <- list()
  for (cst in names(labels)) {
    f <- prepped[[cst]]
    models[[cst]] <- if (!is.null(f) && nrow(f) > 0)
      do.call(.fit_contest_model, c(list(f, labels[[cst]]), ctl)) else NULL
  }
  if (all(vapply(models, is.null, logical(1)))) cli::cli_abort("No duel contests found to fit")

  result <- c(models, list(
    panna_metadata = list(
      type = "duel_model", feature_cols = .DUEL_FEATURE_COLS,
      contests = names(.DUEL_CONTESTS),
      win_rates = vapply(names(labels),
                         function(c) if (!is.null(models[[c]])) models[[c]]$win_rate else NA_real_,
                         numeric(1))
    )
  ))
  class(result) <- c("duel_model", "list")
  result
}


# Predict P(win) for a feature table using a chosen sub-model (internal).
.predict_contest <- function(sub, features) {
  if (is.null(sub)) return(rep(NA_real_, nrow(features)))
  X <- as.matrix(as.data.frame(features)[, sub$feature_cols, drop = FALSE])
  X[is.na(X)] <- 0
  stats::predict(sub$model, X)
}

#' Predict a duel contest win probability
#' @param duel_model Fitted model from \code{fit_duel_model}/\code{load_duel_model}.
#' @param features data.table of features (from \code{prepare_duels_from_events}).
#' @param contest One of the five contest names.
#' @return Numeric vector of P(win).
#' @export
predict_duel <- function(duel_model, features, contest = names(.DUEL_CONTESTS)) {
  contest <- match.arg(contest)
  .predict_contest(duel_model[[contest]], features)
}


#' Aggregate duels-above-expected per player (optionally per match)
#'
#' Above-expected analogue of the old accuracy ratios: for each of the five
#' contests, contests won minus summed context win-probability (volume-correct,
#' additive — the `npg_minus_npxg` pattern for physical duels).
#'
#' @param events Full per-league Opta events.
#' @param duel_model Fitted xDuel model.
#' @param by_match Logical. One row per player-(team-)match if TRUE.
#' @return data.table keyed by player (team, match) with `<prefix>_won/_exp/_woe`
#'   for prefixes aerial, aerial_poss, takeon, tackle_poss, containment.
#'   Per-90 normalisation is applied by the caller (which holds minutes).
#' @export
compute_duel_woe <- function(events, duel_model, by_match = FALSE) {
  key <- if (by_match) c("player_id", "team_id", "match_id") else c("player_id", "team_id")
  dt <- .order_events(events)
  aggs <- list()
  for (cst in names(.DUEL_CONTESTS)) {
    prefix <- .DUEL_CONTESTS[[cst]]
    f <- .finalize_duel(.DUEL_PREP_FUNS[[cst]](dt))
    g <- intersect(key, names(f))
    if (is.null(f) || nrow(f) == 0 || !length(g)) next
    f[, p_win := .predict_contest(duel_model[[cst]], f)]
    a <- f[, .(won = sum(won), exp = sum(p_win, na.rm = TRUE)), by = g]
    data.table::setnames(a, c("won", "exp"), paste0(prefix, c("_won", "_exp")))
    aggs[[cst]] <- a
  }
  if (!length(aggs)) return(data.table::data.table())
  out <- Reduce(function(a, b) merge(a, b, by = intersect(names(a), names(b)), all = TRUE), aggs)
  for (prefix in .DUEL_CONTESTS) {
    for (sfx in c("_won", "_exp")) {
      col <- paste0(prefix, sfx)
      if (!col %in% names(out)) out[, (col) := 0]
      data.table::set(out, which(is.na(out[[col]])), col, 0)
    }
    out[, (paste0(prefix, "_woe")) := get(paste0(prefix, "_won")) - get(paste0(prefix, "_exp"))]
  }
  out[]
}


#' Load the pre-trained xDuel model
#' @param path Optional explicit path. Falls back to pannamodels, then local.
#' @return Fitted xDuel model.
#' @export
load_duel_model <- function(path = NULL) {
  if (!is.null(path) && file.exists(path)) {
    cli::cli_alert_success("Loaded xDuel model from {path}"); return(readRDS(path))
  }
  if (requireNamespace("pannamodels", quietly = TRUE)) {
    model <- tryCatch(pannamodels::load_panna_model("duel_model", verbose = FALSE),
                      error = function(e) NULL)
    if (!is.null(model)) { cli::cli_alert_success("Loaded xDuel model from pannamodels"); return(model) }
  }
  default_path <- file.path(opta_data_dir(), "models", "duel_model.rds")
  if (file.exists(default_path)) {
    cli::cli_alert_success("Loaded xDuel model from {default_path}"); return(readRDS(default_path))
  }
  cli::cli_abort(c("xDuel model not found.",
                   "i" = "Train it via data-raw/epv/01b_train_duel_model.R"))
}


#' Save the xDuel model
#' @param duel_model Fitted model.
#' @param path Optional path. Defaults to pannadata opta/models/duel_model.rds.
#' @return Invisibly, the path.
#' @export
save_duel_model <- function(duel_model, path = NULL) {
  if (is.null(path)) {
    model_dir <- file.path(opta_data_dir(), "models")
    dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(model_dir, "duel_model.rds")
  }
  saveRDS(duel_model, path)
  cli::cli_alert_success("Saved xDuel model to {path}")
  invisible(path)
}
