# Full-model pairwise knockout lookup for UEFA club cups (UCL/UEL/UECL)
# ========================================================================
# Sibling of R/knockout_model.R's build_knockout_lookup(), which already
# powers the World Cup's wc2026_knockout_probs.parquet (read by the blog's
# football/wc-sim.js koP lookup). Same idea -- predict every possible
# knockout matchup once with the full goals+outcome model instead of
# compressing every team into one rating -- but three real differences from
# the WC case, not just a relabel:
#
#   1. TEAM STATE IS NOT CONSTANT. build_knockout_lookup() asserts every row
#      for a team has identical features and aborts otherwise -- true only
#      because the WHOLE WC group stage is predicted from one frozen
#      pre-tournament snapshot. A club season is LIVE: Elo/rolling-form move
#      match to match. So this takes each team's row NEAREST TODAY'S DATE
#      (played or fixture) rather than an assumed-constant one.
#
#      NOT "most recent played" either. Steps 02/02b already predict a
#      CURRENT lineup (is_starter_pred) for unplayed fixture rows, so a
#      team's NEXT match already carries a fresher squad read than an old
#      played row for a team that hasn't played in months -- the nearest-row
#      choice reuses that machinery instead of hand-rolling a separate squad
#      recomputation. Measured on real 2026-09-10 data across all 36 UCL/
#      UEL/UECL participants: max staleness 33 days (UCL), 7 (UEL), 14
#      (UECL) -- comfortably fresh, including clubs entirely outside panna's
#      10 tracked domestic leagues (Club Brugge, Shakhtar Donetsk, Slavia
#      Praha, ...). An earlier version that searched PLAYED rows only found
#      those same clubs 4-7 MONTHS stale (their domestic form is invisible
#      to panna, so their last panna-visible match was last season's cup
#      run) -- that was a bug in the row selection, not a real data-coverage
#      gap, and is why "current state" here means nearest-in-time, not
#      most-recent-result.
#
#   2. POOLED MODEL ONLY. build_knockout_lookup() blends pooled + the
#      international specialist (MATCH_INTL_BLEND_WEIGHT) because WC matches
#      are international. Club cup ties are not -- 07_predict_fixtures.R
#      already routes UCL/UEL/UECL through the pooled model only
#      (match_is_international() is FALSE for them). Blending in the
#      international specialist here would score club ties with a model
#      never fit for them.
#
#   3. TWO LEGS, NOT ONE AVERAGED NUMBER. WC knockouts are single-match, so
#      build_knockout_lookup() computes both orientations (t1 home, t2 home)
#      and AVERAGES them into one orientation-invariant tie probability. A
#      UEFA club cup tie is two legs -- t1 hosts once, t2 hosts once -- and
#      those two orientations ARE exactly those two legs already (the
#      "mirrored" prediction isn't a relabeling, it's a fresh prediction
#      with home/away features swapped). So this returns them UNAVERAGED:
#      leg1 = t1 hosts, leg2 = t2 hosts. No host-NATION home_field logic
#      either -- a club leg always has a real host, so home_field is simply
#      +1 for whichever team sits in the home slot of that leg's feature
#      row (never negated the way WC's host-nation bonus is).
#
# Team pool: real league-phase rows only, excluding the Jul/Aug qualifying-
# round contamination (Opta tags qualifiers with the same UCL/UEL/UECL
# league code as the league phase -- same exclusion as panna PR #234's
# season_standings fix and the blog's client-side isCupQualifyingRound()).


#' Build a pooled-only, two-legged pairwise knockout lookup for one UEFA cup
#'
#' Predicts every possible league-phase-participant matchup with the full
#' goals + outcome models, two legs each (both hosting orientations, kept
#' separate -- see file header point 3). Companion to
#' [build_knockout_lookup()] for club cup competitions.
#'
#' @param match_dataset The step-04 match dataset.
#' @param goals_models Step-05 goals models (`$feature_cols` top-level,
#'   `$pooled$home` / `$pooled$away`).
#' @param outcome_result Step-06 outcome models (`$augmented_features`
#'   top-level, `$pooled$model`).
#' @param league One of `"UCL"`, `"UEL"`, `"UECL"`.
#' @param season Season label matching `match_dataset$season` for `league`.
#' @param as_of Reference date for "current state" (default: today). Exposed
#'   for tests and reproducible snapshots.
#' @param verbose Print progress.
#'
#' @return A list:
#'   \describe{
#'     \item{probs}{data.table, one row per unordered team pair, with
#'       `leg1_home_goals`/`leg1_away_goals`/`leg1_pH`/`leg1_pD`/`leg1_pA`
#'       (t1 hosts) and the `leg2_*` mirror (t2 hosts).}
#'     \item{team_as_of}{named list, team -> Date of the row its features
#'       were read from (freshness diagnostic).}
#'     \item{n_teams}{league-phase team count (36 for the current UEFA
#'       format).}
#'   }
#' @family cup simulation
#' @export
build_cup_pairwise_lookup <- function(match_dataset, goals_models, outcome_result,
                                       league, season, as_of = Sys.Date(),
                                       verbose = TRUE) {
  if (!league %in% c("UCL", "UEL", "UECL")) {
    cli::cli_abort("build_cup_pairwise_lookup: league must be one of UCL/UEL/UECL, got {league}")
  }
  dt <- as.data.frame(match_dataset, stringsAsFactors = FALSE)
  feature_cols       <- goals_models$feature_cols
  augmented_features <- outcome_result$augmented_features
  gm_pooled <- goals_models$pooled
  om_pooled <- outcome_result$pooled
  if (is.null(gm_pooled) || is.null(om_pooled)) {
    cli::cli_abort("build_cup_pairwise_lookup: expected $pooled sub-models")
  }

  is_league_phase <- dt$league == league & dt$season == season &
    !is.na(dt$home_team) & dt$home_team != "" &
    as.integer(format(as.Date(dt$match_date), "%m")) >= 9L
  phase <- dt[is_league_phase, ]
  if (nrow(phase) == 0) {
    cli::cli_abort("build_cup_pairwise_lookup: no league-phase rows for {league}/{season}")
  }
  teams <- sort(unique(c(phase$home_team, phase$away_team)))
  if (isTRUE(verbose)) {
    cli::cli_alert_info("{league} {season}: {length(teams)} teams in league phase")
  }

  home_cols <- grep("^home_", feature_cols, value = TRUE)
  home_cols <- home_cols[paste0("away_", sub("^home_", "", home_cols)) %in% feature_cols]
  bases <- sub("^home_", "", home_cols)

  dtd <- data.table::as.data.table(dt)
  dtd[, match_date := as.Date(match_date)]

  .nearest_block <- function(tm) {
    hr <- dtd[home_team == tm & !is.na(match_date)]
    ar <- dtd[away_team == tm & !is.na(match_date)]
    away_cols <- paste0("away_", bases)
    cand <- data.table::rbindlist(list(
      if (nrow(hr) > 0) hr[, c("match_date", home_cols), with = FALSE] else NULL,
      if (nrow(ar) > 0) {
        x <- ar[, c("match_date", away_cols), with = FALSE]
        data.table::setnames(x, away_cols, home_cols)
        x
      } else NULL
    ), fill = TRUE)
    if (nrow(cand) == 0) {
      cli::cli_abort("build_cup_pairwise_lookup: no rows at all for team {tm}")
    }
    cand <- cand[order(abs(as.numeric(match_date - as_of)))][1]
    blk <- vapply(home_cols, function(c) as.numeric(cand[[c]]), numeric(1))
    names(blk) <- bases
    list(blk = blk, as_of = cand$match_date)
  }

  team_block <- vector("list", length(teams)); names(team_block) <- teams
  team_as_of <- vector("list", length(teams)); names(team_as_of) <- teams
  for (tm in teams) {
    r <- .nearest_block(tm)
    team_block[[tm]] <- r$blk
    team_as_of[[tm]] <- r$as_of
  }

  # diff-column -> team-feature-base mapping, same convention as
  # build_knockout_lookup() (every *_diff / diff_* column is
  # home_<base> - away_<base>; see R/match_prediction.R's diff construction).
  suffix_base <- c(
    panna_diff = "sum_panna",     offense_diff = "sum_offense",
    defense_diff = "sum_defense", spm_diff = "sum_spm",
    epr_diff = "sum_epr",         epr_off_diff = "sum_epr_off",
    epr_def_diff = "sum_epr_def", psr_diff = "sum_psr",
    osr_diff = "sum_osr",         dsr_diff = "sum_dsr",
    elo_diff = "elo",             rest_diff = "days_since_last",
    wpa_diff = "sum_wpa",         psv_diff = "sum_psv",
    centrality_diff = "avg_centrality",
    sk_att_diff = "sk_att_composite", sk_def_diff = "sk_def_composite")
  diff_cols <- grep("(_diff$|^diff_)", feature_cols, value = TRUE)
  diff_base <- character(0)
  for (dcol in diff_cols) {
    dbase <- if (startsWith(dcol, "diff_")) sub("^diff_", "", dcol) else unname(suffix_base[dcol])
    if (is.na(dbase) || !dbase %in% bases) {
      cli::cli_abort(c(
        "build_cup_pairwise_lookup: cannot resolve diff column {.field {dcol}}",
        "i" = "resolved base {.val {dbase}} is not a team feature",
        "i" = "Add {.field {dcol}} to the suffix_base map in R/cup_pairwise_model.R."))
    }
    diff_base[dcol] <- dbase
  }

  pairs <- t(utils::combn(teams, 2))
  np <- nrow(pairs)
  X <- matrix(0, nrow = np, ncol = length(feature_cols), dimnames = list(NULL, feature_cols))
  for (cn in feature_cols) X[, cn] <- as.numeric(phase[[cn]][1])  # match-level template (league dummies etc.)

  for (i in seq_len(np)) {
    b1 <- team_block[[pairs[i, 1]]]; b2 <- team_block[[pairs[i, 2]]]
    X[i, home_cols]              <- b1[bases]
    X[i, paste0("away_", bases)] <- b2[bases]
    for (d in diff_cols) X[i, d] <- b1[diff_base[d]] - b2[diff_base[d]]
  }
  # Every leg has a real host -- always +1 for whichever team sits in the
  # home slot, never negated the way WC's host-NATION bonus is (file header
  # point 3).
  if ("home_field" %in% feature_cols)       X[, "home_field"] <- 1
  if ("is_neutral_venue" %in% feature_cols) X[, "is_neutral_venue"] <- 0

  leg1 <- .ko_predict(X, gm_pooled, om_pooled, augmented_features)  # t1 hosts

  Xm <- X
  for (bs in bases) {
    h <- paste0("home_", bs); a <- paste0("away_", bs)
    tmp <- Xm[, h]; Xm[, h] <- Xm[, a]; Xm[, a] <- tmp
  }
  for (d in diff_cols) Xm[, d] <- -Xm[, d]
  leg2 <- .ko_predict(Xm, gm_pooled, om_pooled, augmented_features)  # t2 hosts

  probs <- data.table::data.table(
    t1 = pairs[, 1], t2 = pairs[, 2],
    leg1_home_goals = round(leg1$hg, 2), leg1_away_goals = round(leg1$ag, 2),
    leg1_pH = round(leg1$pH, 3), leg1_pD = round(leg1$pD, 3), leg1_pA = round(leg1$pA, 3),
    leg2_home_goals = round(leg2$hg, 2), leg2_away_goals = round(leg2$ag, 2),
    leg2_pH = round(leg2$pH, 3), leg2_pD = round(leg2$pD, 3), leg2_pA = round(leg2$pA, 3)
  )

  if (isTRUE(verbose)) {
    cli::cli_alert_success(
      "{league} pairwise lookup: {np} matchups predicted with the full model ({length(feature_cols)} features)")
  }
  list(probs = probs, team_as_of = team_as_of, n_teams = length(teams))
}
