# Keeper distribution style: separate the club's habit from the keeper's own (panna#429).
#
# Why: a keeper's long-ball rate is mostly his club's instruction. Of 617 keepers
# who moved clubs between seasons (2026-11-08), the new club's previous keepers
# predicted his rate there with weight 0.66 against 0.19 for his own previous
# rate, so the raw stat in keeper PSR scored team tactics. Adjusting for the
# outfield players' passing in the same match did not help (team style explained
# 13% of keeper-match variation and the movers test barely moved), because
# keeper distribution is its own club instruction (goal kicks and restarts).
#
# Model, per stat, on keeper-matches (minutes-weighted ridge):
#   value = league-season + club + club-season + keeper + noise
# The club term carries a club's lasting habit, the club-season term a
# one-season shift (a new coach), and the keeper term what follows a keeper
# between clubs. Without the lasting club term the ridge loads a club's
# multi-season habit onto its one-club keeper (Alisson -5.3 instead of -2.6 in
# the prototype), because a keeper dummy spans more seasons than a club-season
# dummy.
#
# As-of: the style that applies to season Y is fitted only on seasons before Y,
# so a pre-match rating never sees future team style.

#' Keeper distribution stats adjusted for club style by default
#' @keywords internal
#' @noRd
.keeper_style_stats <- function() c("long_balls_p90", "passes_accurate_p90", "keeper_throws_p90")

#' Name of the club-adjusted column for a keeper stat ("long_balls_p90" -> "long_balls_kadj_p90")
#' @keywords internal
#' @noRd
.keeper_style_col <- function(stat) sub("_p90$", "_kadj_p90", stat)

#' Fit one as-of keeper-style model: club habits from seasons before `cutoff_year`
#'
#' @param k Keeper-match rows: player_id, team_id, league, season_end_year,
#'   total_minutes and the stat column.
#' @param stat Stat column name.
#' @param cutoff_year Fit on season_end_year < cutoff_year.
#' @param lambda Ridge penalty (glmnet scale, weights = 90s played).
#' @return list(club = data.table(team_id, club_style), keeper =
#'   data.table(player_id, keeper_eff), n = rows used), or NULL if too few rows.
#' @keywords internal
#' @noRd
.fit_keeper_style_once <- function(k, stat, cutoff_year, lambda) {
  d <- k[season_end_year < cutoff_year & is.finite(k[[stat]])]
  if (nrow(d) < 200L || data.table::uniqueN(d$team_id) < 5L) return(NULL)
  y <- d[[stat]]; w <- d$total_minutes / 90
  y <- y - stats::weighted.mean(y, w)
  f_ls <- factor(paste(d$league, d$season_end_year))
  f_cl <- factor(d$team_id)
  f_cs <- factor(paste(d$team_id, d$season_end_year))
  f_kp <- factor(d$player_id)
  # One indicator column per level. Built directly because
  # sparse.model.matrix() refuses a one-level factor, which an early cutoff
  # with a single league-season produces.
  ind <- function(f) Matrix::sparseMatrix(i = seq_along(f), j = as.integer(f), x = 1,
                                          dims = c(length(f), nlevels(f)))
  X <- cbind(ind(f_ls), ind(f_cl), ind(f_cs), ind(f_kp))
  # League-season gets a near-zero penalty: it is the level every club is
  # measured against, not a quantity to shrink.
  pf <- c(rep(0.01, nlevels(f_ls)), rep(1, nlevels(f_cl)), rep(1, nlevels(f_cs)), rep(1, nlevels(f_kp)))
  fit <- glmnet::glmnet(X, y, weights = w, alpha = 0, penalty.factor = pf,
                        lambda = lambda, standardize = FALSE, intercept = TRUE)
  b <- as.matrix(stats::coef(fit))[-1, 1]   # drop the intercept; columns are in X's order
  o <- cumsum(c(0L, nlevels(f_ls), nlevels(f_cl), nlevels(f_cs)))
  cl <- data.table::data.table(team_id = levels(f_cl), club_eff = unname(b[o[2] + seq_len(nlevels(f_cl))]))
  cs <- data.table::data.table(cs = levels(f_cs), cs_eff = unname(b[o[3] + seq_len(nlevels(f_cs))]))
  cs[, `:=`(team_id = sub(" [0-9]+$", "", cs), yr = as.integer(sub(".* ", "", cs)))]
  # A club's style going into season Y = its lasting habit + its most recent
  # club-season shift (a coach's change carries forward until seen otherwise).
  last <- cs[order(yr), .SD[.N], by = team_id][, .(team_id, last_cs = cs_eff)]
  cl <- merge(cl, last, by = "team_id", all.x = TRUE)
  cl[is.na(last_cs), last_cs := 0]
  cl[, club_style := club_eff + last_cs]
  list(club = cl[, .(team_id, club_style)],
       keeper = data.table::data.table(player_id = levels(f_kp), keeper_eff = unname(b[o[4] + seq_len(nlevels(f_kp))])),
       n = nrow(d))
}

#' Fit as-of club style for keeper distribution stats, one fit per season
#'
#' For every season Y present in `match_stats`, fits the keeper-style model on
#' seasons before Y and records each club's style going into Y. Season Y's
#' adjusted value then subtracts only past information.
#'
#' @param match_stats Match-level stats (all positions; keepers are selected with
#'   `.detect_gk_rows()`). Needs player_id, team_id, league (or competition),
#'   season_end_year (or season), total_minutes and the stat columns.
#' @param stats Keeper stats to model.
#' @param min_minutes Keeper-match rows below this are left out of the fit.
#' @param lambda Ridge penalty. 0.00127 was the grouped-CV minimum on 163,907
#'   keeper-matches (28 domestic leagues, folds by club-season) for long balls
#'   per 90 on 2026-11-08.
#' @param verbose Print one line per fit.
#' @return data.table(stat, season_end_year, team_id, club_style): the style to
#'   subtract from that club's keepers in that season.
#' @keywords internal
#' @noRd
fit_keeper_style_asof <- function(match_stats, stats = .keeper_style_stats(),
                                  min_minutes = 45, lambda = 0.00127, verbose = TRUE) {
  dt <- if (data.table::is.data.table(match_stats)) match_stats else data.table::as.data.table(match_stats)
  lg_col <- intersect(c("league", "competition"), names(dt))[1]
  if (is.na(lg_col)) cli::cli_abort("match_stats needs a {.field league} or {.field competition} column.")
  need <- c("player_id", "team_id", "total_minutes")
  miss <- setdiff(need, names(dt))
  if (length(miss)) cli::cli_abort("match_stats is missing {.field {miss}}.")
  stats <- intersect(stats, names(dt))
  if (!length(stats)) cli::cli_abort("None of the keeper style stats are in match_stats.")
  yr <- if ("season_end_year" %in% names(dt)) as.integer(dt$season_end_year)
        else as.integer(extract_season_end_year(dt$season))
  is_gk <- .detect_gk_rows(dt)
  keep <- is_gk & !is.na(yr) & as.numeric(dt$total_minutes) >= min_minutes
  k <- data.table::data.table(player_id = as.character(dt$player_id[keep]),
                              team_id = as.character(dt$team_id[keep]),
                              league = as.character(dt[[lg_col]][keep]),
                              season_end_year = yr[keep],
                              total_minutes = as.numeric(dt$total_minutes[keep]))
  for (s in stats) data.table::set(k, j = s, value = as.numeric(dt[[s]][keep]))
  if (!nrow(k)) cli::cli_abort("No keeper rows found (checked with .detect_gk_rows()).")
  years <- sort(unique(k$season_end_year))
  out <- list()
  for (s in stats) for (y in years) {
    f <- .fit_keeper_style_once(k, s, y, lambda)
    if (is.null(f)) next
    out[[length(out) + 1L]] <- f$club[, .(stat = s, season_end_year = y, team_id, club_style)]
    if (verbose) cli::cli_inform("keeper style {s} for {y}: {nrow(f$club)} clubs from {f$n} keeper-matches")
  }
  if (!length(out)) cli::cli_abort("No keeper-style fits had enough data.")
  data.table::rbindlist(out)
}

#' Add club-adjusted keeper distribution columns
#'
#' Writes `<stem>_kadj_p90` for each modelled stat: the raw value minus the
#' as-of club style for that keeper's club and season. ADDITIVE, like
#' `.add_territory_features()`: the raw columns are untouched. Outfield rows,
#' and keeper rows whose club has no style yet (first season in the data, or a
#' club new to it), keep the raw value, and the count is reported rather than
#' silently guessed.
#'
#' @param dt Match-level stats with team_id and season_end_year (or season).
#' @param style Output of `fit_keeper_style_asof()`.
#' @param is_gk Logical keeper flag per row (default `.detect_gk_rows(dt)`).
#' @return dt by reference, with the `_kadj_p90` columns added.
#' @keywords internal
#' @noRd
.add_keeper_style_features <- function(dt, style, is_gk = NULL, verbose = TRUE) {
  if (is.null(is_gk)) is_gk <- .detect_gk_rows(dt)
  yr <- if ("season_end_year" %in% names(dt)) as.integer(dt$season_end_year)
        else as.integer(extract_season_end_year(dt$season))
  key <- paste(dt$team_id, yr)
  for (s in intersect(unique(style$stat), names(dt))) {
    st <- style[stat == s]
    adj <- st$club_style[match(key, paste(st$team_id, st$season_end_year))]
    raw <- as.numeric(dt[[s]])
    use <- is_gk & is.finite(adj)
    out <- raw
    out[use] <- raw[use] - adj[use]
    out_col <- .keeper_style_col(s)
    data.table::set(dt, j = out_col, value = out)
    n_used <- sum(use); n_gk <- sum(is_gk); n_raw <- n_gk - n_used
    if (verbose) cli::cli_inform("{out_col}: club style applied to {n_used} of {n_gk} keeper rows; {n_raw} keep the raw value (no earlier season for that club)")
  }
  dt
}
