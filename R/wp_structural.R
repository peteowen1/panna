# Structural (home-frame) win probability model -- panna#273.
#
# The published WP model (train_wp_model()) predicts P(possession team wins) and,
# because teams that keep the ball are mostly the better teams, learned to pay
# 6-8 points of win chance for simply having the ball at 0-0. Every turnover then
# swings that premium between the teams, so a goal ends up worth ~6 lost passes.
#
# The structural model instead predicts H = P(home wins, draw = 0.5) from the
# scoreboard (home score margin, time, red cards, half, extra time) plus the
# pre-match team-strength gap (Elo, home minus away). Optionally it also sees
# who has the ball (`use_possession = TRUE`), which with strength in the model
# measures what possession is worth NET of team quality. The possession team's
# win chance is H (home) or 1 - H (away), and live threat enters as the chance
# of the next goal: (1 - epv) * H(margin) + epv * H(margin + 1).
#
# Prototype and evidence: pannaverse docs/plans/WPA-LEDGER-273.md and
# data-raw/epv/wpa-ledger-273/wp_strength_arms.R.

.WP_STRUCTURAL_BASE <- c("time_remaining", "m_home", "red_home", "is_second_half",
                         "is_extra_time", "elo_diff")

#' Join pre-match team strength onto WP features
#'
#' Adds \code{elo_diff} (home Elo minus away Elo, BEFORE the match) to the rows
#' of \code{wp_features} by \code{match_id}, for \code{\link{train_wp_structural}}
#' and the structural branch of \code{\link{predict_wp}}. Rows whose match has no
#' Elo are reported and, by default, dropped -- never filled with 0, which would
#' silently read as "evenly matched".
#'
#' @param wp_features Output of \code{\link{create_wp_features}}.
#' @param match_elo Data frame with \code{match_id} and \code{elo_diff}, or with
#'   \code{home_elo} and \code{away_elo}; e.g. the \code{per_match} element of
#'   \code{\link{compute_match_elos}}, whose values are pre-match.
#' @param min_coverage Abort when fewer than this share of matches get an Elo
#'   (default 0.95).
#' @param drop_missing Drop rows with no Elo (default \code{TRUE}).
#'
#' @return \code{wp_features} as a data.table with an \code{elo_diff} column.
#' @family win probability
#' @export
add_wp_match_elo <- function(wp_features, match_elo, min_coverage = 0.95,
                             drop_missing = TRUE) {
  dt <- data.table::as.data.table(wp_features)
  me <- data.table::as.data.table(match_elo)
  if (!"elo_diff" %in% names(me)) {
    if (!all(c("home_elo", "away_elo") %in% names(me))) {
      cli::cli_abort("{.arg match_elo} needs {.val elo_diff} or {.val home_elo} + {.val away_elo}.")
    }
    me[, elo_diff := home_elo - away_elo]
  }
  me <- unique(me[!is.na(elo_diff), .(match_id, elo_diff)], by = "match_id")
  if ("elo_diff" %in% names(dt)) dt[, elo_diff := NULL]
  dt[me, elo_diff := i.elo_diff, on = "match_id"]
  n_m <- data.table::uniqueN(dt$match_id)
  miss <- unique(dt$match_id[is.na(dt$elo_diff)])
  cov <- if (n_m > 0L) 1 - length(miss) / n_m else 0
  if (length(miss) > 0L) {
    cli::cli_alert_warning("No pre-match Elo for {length(miss)} of {n_m} match{?es} ({round(100 * cov, 1)}% covered){if (drop_missing) '; their rows are dropped' else ''}.")
  }
  if (cov < min_coverage) {
    cli::cli_abort("Pre-match Elo covers only {round(100 * cov, 1)}% of matches (need {round(100 * min_coverage)}%).")
  }
  if (isTRUE(drop_missing)) dt <- dt[!is.na(elo_diff)]
  dt
}

#' Join starting-XI strength onto WP features
#'
#' Adds \code{xi_diff} = home XI strength minus away XI strength. A team's XI
#' strength is 11 x the mean rating of its rated starters, each starter's rating
#' taken from the latest as-of snapshot dated BEFORE match day
#' (\code{snapshot <= match_date - 1}, at most \code{roll_days} old). Use a
#' leak-free as-of rating such as \code{career_panna_asof.parquet} (each monthly
#' snapshot fitted only on matches up to its date); the weekly PSR is NOT
#' leak-free for model evaluation, because its coefficients are fitted on all
#' seasons (panna#273, 2026-11-10). A team with fewer than \code{min_rated}
#' rated starters gets \code{NA}, never 0.
#'
#' @param wp_features Output of \code{\link{create_wp_features}} (needs
#'   \code{match_id}, \code{team_id}, \code{is_home}).
#' @param lineups Lineups with \code{match_id}, \code{team_id},
#'   \code{player_id}, \code{match_date}, \code{is_starter}.
#' @param ratings As-of ratings: \code{player_id}, \code{ref_date} (or
#'   \code{snapshot_date}) and \code{panna} (or \code{rating}).
#' @param roll_days Oldest snapshot accepted, in days (default 62: monthly
#'   snapshots).
#' @param min_rated Fewest rated starters for a team value (default 9).
#'
#' @return \code{wp_features} as a data.table with \code{xi_diff}; attribute
#'   \code{xi_table} holds the per team-match values.
#' @family win probability
#' @export
add_wp_xi_strength <- function(wp_features, lineups, ratings, roll_days = 62L,
                               min_rated = 9L) {
  dt <- data.table::as.data.table(wp_features)
  lu <- data.table::as.data.table(lineups)
  rt <- data.table::as.data.table(ratings)
  dcol <- intersect(c("ref_date", "snapshot_date"), names(rt))[1]
  vcol <- intersect(c("panna", "rating"), names(rt))[1]
  if (is.na(dcol) || is.na(vcol) || !"player_id" %in% names(rt)) {
    cli::cli_abort("{.arg ratings} needs {.val player_id}, a date ({.val ref_date}) and a value ({.val panna}).")
  }
  need_lu <- c("match_id", "team_id", "player_id", "match_date", "is_starter")
  miss <- setdiff(need_lu, names(lu))
  if (length(miss)) cli::cli_abort("{.arg lineups} is missing {.val {miss}}.")
  rt <- rt[!is.na(get(vcol)), .(player_id, snap = as.Date(get(dcol)), r = as.numeric(get(vcol)))]
  data.table::setkey(rt, player_id, snap)
  st <- unique(lu[is_starter %in% TRUE & match_id %in% unique(dt$match_id),
                  .(match_id, team_id, player_id, snap = as.Date(substr(as.character(match_date), 1, 10)) - 1L)])
  j <- rt[st, on = .(player_id, snap), roll = roll_days, .(match_id, team_id, r = x.r)]
  xi <- j[, .(n_starters = .N, n_rated = sum(!is.na(r)), xi = 11 * mean(r, na.rm = TRUE)), by = .(match_id, team_id)]
  xi[n_rated < min_rated, xi := NA_real_]
  home <- dt[is_home == 1L, .(home_team_id = team_id[1]), by = match_id]
  away <- dt[is_home == 0L, .(away_team_id = team_id[1]), by = match_id]
  m <- merge(home, away, by = "match_id", all = TRUE)
  m[xi, xi_home := i.xi, on = .(match_id, home_team_id = team_id)]
  m[xi, xi_away := i.xi, on = .(match_id, away_team_id = team_id)]
  m[, xi_diff := xi_home - xi_away]
  n_na <- m[is.na(xi_diff), .N]
  if (n_na > 0L) {
    cli::cli_alert_warning("No XI strength for {n_na} of {nrow(m)} match{?es} (fewer than {min_rated} rated starters on a side, or no lineup); left NA.")
  }
  if ("xi_diff" %in% names(dt)) dt[, xi_diff := NULL]
  dt[m, xi_diff := i.xi_diff, on = "match_id"]
  data.table::setattr(dt, "xi_table", xi)
  dt
}

# Home-frame model inputs from create_wp_features() output. score_diff and
# red_card_diff are already home frame there (R/wp_model.R).
.wp_structural_frame <- function(dt, extra = character()) {
  need <- c("time_remaining", "score_diff", "red_card_diff", "is_second_half",
            "is_extra_time", "is_home", "elo_diff", extra)
  miss <- setdiff(need, names(dt))
  if (length(miss) > 0L) {
    cli::cli_abort(c("WP features missing {.val {miss}} for the structural model.",
                     "i" = "Build them with {.fn create_wp_features} and join Elo with {.fn add_wp_match_elo}."))
  }
  x <- dt[, .(time_remaining, m_home = as.numeric(score_diff),
              red_home = as.numeric(red_card_diff), is_second_half, is_extra_time,
              elo_diff = as.numeric(elo_diff), home_has_ball = as.integer(is_home == 1L))]
  # Extra inputs pass through as numbers; NA is kept (xgboost learns a direction for missing).
  for (f in extra) data.table::set(x, j = f, value = as.numeric(dt[[f]]))
  x
}

#' Train the structural (home-frame) win probability model
#'
#' Fits \eqn{H} = P(home team wins; draw = 0.5) from home score margin, time
#' remaining, red-card difference, half, extra time and the pre-match Elo gap,
#' optionally plus who has the ball. See \code{\link{predict_wp}} for how
#' \eqn{H} becomes the possession team's win chance, and panna#273 for why.
#' Nrounds is chosen by 5-fold match-grouped CV with early stopping, as in
#' \code{\link{train_wp_model}}.
#'
#' @param wp_features Output of \code{\link{create_wp_features}} with
#'   \code{wp_label}, joined to Elo by \code{\link{add_wp_match_elo}}.
#' @param use_possession Add \code{home_has_ball} as an input (default
#'   \code{FALSE}). With strength in the model this measures what possession is
#'   worth beyond team quality; 2026-11-10 EPL hold-out: 1.6 points, no gain in
#'   log loss.
#' @param nrounds,max_depth,eta,nfolds,min_child_weight,early_stopping_rounds,seed
#'   XGBoost / CV settings.
#' @param weight Optional per-row training weights (same length as the rows kept
#'   after dropping NA labels), e.g. to undo row subsampling.
#' @param extra_features Names of further numeric columns in \code{wp_features}
#'   to use as inputs, in the HOME frame or frame-free (e.g. minutes into
#'   stoppage time). No monotone constraint; NA allowed. Stored on the model,
#'   so prediction needs the same columns.
#'
#' @return A list of class \code{panna_wp_structural} with \code{model},
#'   \code{feature_names}, \code{use_possession}, \code{cv_logloss},
#'   \code{optimal_nrounds} and \code{type = "structural"}.
#' @family win probability
#' @export
train_wp_structural <- function(wp_features, use_possession = FALSE,
                                nrounds = 600L, max_depth = 4L, eta = 0.1,
                                nfolds = 5L, min_child_weight = 50L,
                                early_stopping_rounds = 20L, seed = 42L,
                                weight = NULL, extra_features = character()) {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} required for WP model training")
  }
  dt <- data.table::as.data.table(wp_features)
  if (!all(c("wp_label", "match_id") %in% names(dt))) {
    cli::cli_abort("wp_features must contain {.val wp_label} and {.val match_id}.")
  }
  keep <- !is.na(dt$wp_label)
  if (!is.null(weight) && length(weight) != sum(keep)) {
    cli::cli_abort("{.arg weight} has {length(weight)} values for {sum(keep)} labelled rows.")
  }
  dt <- dt[keep]
  extra_features <- as.character(extra_features)
  clash <- intersect(extra_features, c(.WP_STRUCTURAL_BASE, "home_has_ball"))
  if (length(clash)) cli::cli_abort("{.arg extra_features} repeats built-in input{?s} {.val {clash}}.")
  x <- .wp_structural_frame(dt, extra_features)
  feature_names <- c(.WP_STRUCTURAL_BASE, if (isTRUE(use_possession)) "home_has_ball", extra_features)
  # Home frame label: the possession-POV wp_label flipped for away rows.
  y <- data.table::fifelse(dt$is_home == 1L, dt$wp_label, 1 - dt$wp_label)
  # More home goals, more away red cards (red_home > 0 = away down to fewer),
  # and a stronger home side must never LOWER the home win chance.
  mono <- c(time_remaining = 0L, m_home = 1L, red_home = 1L, is_second_half = 0L,
            is_extra_time = 0L, elo_diff = 1L, home_has_ball = 0L,
            stats::setNames(rep(0L, length(extra_features)), extra_features))[feature_names]
  params <- list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                 tree_method = "hist", max_depth = max_depth, eta = eta, subsample = 0.8,
                 colsample_bytree = 0.8, min_child_weight = min_child_weight,
                 monotone_constraints = paste0("(", paste(mono, collapse = ","), ")"))
  dtrain <- xgboost::xgb.DMatrix(as.matrix(x[, ..feature_names]), label = y, weight = weight)
  um <- unique(dt$match_id)
  set.seed(seed)
  fold_of <- stats::setNames(sample(rep(seq_len(nfolds), length.out = length(um))), um)
  row_fold <- fold_of[dt$match_id]
  folds <- lapply(seq_len(nfolds), function(k) which(row_fold == k))
  set.seed(seed)
  cv <- xgboost::xgb.cv(params = params, data = dtrain, nrounds = nrounds, folds = folds,
                        early_stopping_rounds = early_stopping_rounds, verbose = 0)
  ll <- cv$evaluation_log[["test_logloss_mean"]]
  best <- which.min(ll)
  set.seed(seed)
  model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = best, verbose = 0)
  cli::cli_alert_success("Structural WP model ({if (isTRUE(use_possession)) 'with' else 'without'} possession): {nrow(x)} actions, {length(um)} matches, nrounds {best}, CV log loss {round(min(ll), 5)}")
  structure(list(model = model, feature_names = feature_names,
                 use_possession = isTRUE(use_possession), extra_features = extra_features,
                 cv_logloss = min(ll),
                 optimal_nrounds = best, type = "structural"),
            class = c("panna_wp_structural", "list"))
}

.is_wp_structural <- function(wp_model) identical(wp_model$type, "structural")

#' @rdname train_wp_structural
#' @param wp_model A model from \code{train_wp_structural()}.
#' @param team_agnostic Score with the Elo gap set to 0, i.e. as if the two
#'   teams were equally strong (the site's team-agnostic WPA view). Home
#'   advantage stays in.
#' @return \code{predict_wp_structural()}: the possession team's win chance per
#'   row, live threat included via the next-goal mixture.
#' @export
predict_wp_structural <- function(wp_model, wp_features, team_agnostic = FALSE) {
  dt <- data.table::as.data.table(wp_features)
  x <- .wp_structural_frame(dt, as.character(wp_model$extra_features))
  if (isTRUE(team_agnostic)) x[, elo_diff := 0]
  fn <- wp_model$feature_names
  sgn <- data.table::fifelse(x$home_has_ball == 1L, 1, -1)
  H_at <- function(add) {
    xx <- data.table::copy(x)[, m_home := m_home + add * sgn]
    stats::predict(wp_model$model, as.matrix(xx[, ..fn]))
  }
  to_poss <- function(h) data.table::fifelse(x$home_has_ball == 1L, h, 1 - h)
  p0 <- to_poss(H_at(0)); pu <- to_poss(H_at(1)); pd <- to_poss(H_at(-1))
  if (!"epv" %in% names(dt)) {
    cli::cli_abort("wp_features has no {.val epv}: the structural model needs live threat.")
  }
  e <- dt$epv
  n_na <- sum(is.na(e))
  if (n_na > 0L) {
    cli::cli_alert_warning("{n_na} row{?s} with NA epv scored on the scoreboard alone (no threat term).")
    e[is.na(e)] <- 0
  }
  # epv is possession-POV: + = possessor more likely to score next, - = conceding.
  p <- ifelse(e >= 0, (1 - e) * p0 + e * pu, (1 + e) * p0 - e * pd)
  pmax(pmin(p, 1), 0)
}
