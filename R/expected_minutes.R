# Expected minutes model for national-team rosters
#
# For each national team, take recent international appearances and compute
# per-player expected minutes for an upcoming match. Inputs: per-game lineups
# with is_starter, minutes_played, sub_on_minute, sub_off_minute.
#
# Output is a per-team data frame of likely-squad players ordered by
# expected minutes, suitable for:
#   - manual sanity-check against announced rosters
#   - weighting player ratings into a minutes-weighted team rating

#' Build expected-minutes profile for a national team
#'
#' @param team Character. National team name (matches `team_name` in lineups).
#' @param lineups Data.table of lineups with columns: team_name, match_id,
#'   match_date, player_id, player_name, position, is_starter, minutes_played,
#'   sub_on_minute, sub_off_minute, competition.
#' @param as_of Date. "Today" for the decay calculation. Default Sys.Date().
#' @param lookback_days Integer. Only consider matches within this window.
#'   Default 730 (2 years -- covers WC qualifiers + most internationals).
#' @param half_life_days Numeric. Exponential decay half-life. Default 365.
#'   Recent matches weight 2x more than matches a year ago.
#' @param squad_size Integer. Cap the returned roster at this many. Default 26.
#' @param international_only Logical. Restrict to international competitions
#'   (WC, EURO, qualifiers, Copa, AFCON, Nations League, friendlies).
#'   Default TRUE.
#' @param min_recent_days Integer. Drop players with no appearances within
#'   this window (relative to `as_of`). Default 540 (~18 months) -- catches
#'   players retired since the last major tournament (EURO/Copa) while still
#'   keeping current squad members who had injury gaps.
#' @param shrinkage_k Numeric. Bayesian pseudo-count: `effective_em = em x
#'   weight_total / (weight_total + k)`. Players with weak evidence get
#'   shrunk toward zero. Default 3 (a player with 3 weighted caps gets
#'   50% of raw EM; 20 weighted caps gets ~87%).
#' @param prob_prior_k Numeric. Beta-prior pseudo-count for the start/sub
#'   probabilities: `p_start = (weight_start + k x base) / (weight_total + k)`
#'   where `base` is the squad-wide decay-weighted start rate. Stops a
#'   single-cap player getting `p_start = 1.00` (and then having it halve
#'   after one omission). Default 1 — the WC2022 backtest showed raw
#'   frequencies are already well calibrated in aggregate (higher k
#'   monotonically worsens minutes MAE), so the prior is kept just strong
#'   enough to damp the thin-history pathologies. Set 0 for legacy raw
#'   frequencies.
#' @param tournament_boost Numeric. Weight multiplier applied to matches in
#'   `tournament_comps` on/after `tournament_start` — lets in-tournament
#'   selections outweigh equally-recent qualifiers/friendlies. Default 1
#'   (no boost). Only applied when `tournament_start` is supplied.
#' @param tournament_comps Character vector of competition codes the boost
#'   applies to. Default `"World_Cup"`.
#' @param tournament_start Date. First day of the current tournament; the
#'   boost applies to `tournament_comps` matches on/after this date only
#'   (so a previous WC four years back is not boosted). Default NULL = off.
#' @return Data frame with one row per likely-squad player and columns:
#'   player_id, player_name, position, n_caps_weighted, p_start,
#'   mins_when_start, p_sub_given_bench, mins_when_sub, expected_minutes,
#'   pct_team_minutes (sums to ~100 across the squad).
#' @family expected minutes
#' @export
build_team_expected_minutes <- function(team,
                                         lineups,
                                         as_of = Sys.Date(),
                                         lookback_days = 730L,
                                         half_life_days = 365,
                                         squad_size = 26L,
                                         international_only = TRUE,
                                         min_recent_days = 540L,
                                         shrinkage_k = 3,
                                         prob_prior_k = 1,
                                         tournament_boost = 1,
                                         tournament_comps = "World_Cup",
                                         tournament_start = NULL) {
  if (!data.table::is.data.table(lineups)) lineups <- data.table::as.data.table(lineups)

  intl_comps <- c("World_Cup", "UEFA_WC_Qualifiers", "UEFA_Euros",
                  "UEFA_Euro_Qualifiers", "UEFA_Nations_League",
                  "Copa_America", "AFCON", "AFCON_Qualifiers",
                  "CONCACAF_Gold_Cup", "UEFA_Super_Cup",
                  "AFC_Asian_Cup", "AFC_WC_Qualifiers",
                  "Asian_Cup_Qualifiers", "Gulf_Cup_of_Nations",
                  "CAF_WC_Qualifiers", "CONMEBOL_WC_Qualifiers",
                  "Intl_Friendlies")

  ## --- 1. Slice down ----------------------------------------------------
  lu <- lineups[team_name == team]
  if (international_only) {
    if ("competition" %in% names(lu)) {
      lu <- lu[competition %in% intl_comps]
    }
  }
  if (nrow(lu) == 0) {
    return(data.frame(player_id = character(0), player_name = character(0),
                      stringsAsFactors = FALSE))
  }
  lu[, match_date := as.Date(sub("Z$", "", match_date))]
  cutoff <- as.Date(as_of) - lookback_days
  lu <- lu[match_date >= cutoff]
  if (nrow(lu) == 0) return(data.frame())

  ## --- 2. Decay weight per match ----------------------------------------
  days_ago <- as.numeric(as.Date(as_of) - lu$match_date)
  lu[, weight := 2 ^ (-days_ago / half_life_days)]
  if (!is.null(tournament_start) && tournament_boost != 1 &&
      "competition" %in% names(lu)) {
    lu[competition %in% tournament_comps &
         match_date >= as.Date(tournament_start),
       weight := weight * tournament_boost]
  }

  ## --- 3. Per-player aggregation ---------------------------------------
  ## Canonicalise the display name per player_id first: Opta feeds mix name
  ## variants for the same id across seasons ("L. Martínez" vs "Lautaro
  ## Martínez"). Aggregating by (player_id, player_name) would split such a
  ## player's appearance evidence into separate rows, each separately shrunk
  ## toward zero — keep the most recent variant for everything.
  lu[, player_name := player_name[which.max(as.numeric(match_date))],
     by = player_id]
  ## Clip minutes to sane range
  lu[, mins_clip := pmin(pmax(minutes_played, 0L), 95L)]
  lu[, played    := mins_clip > 0L]
  ## Pre-compute modal non-"Substitute" position per player
  pos_lookup <- lu[played == TRUE & position != "Substitute",
                   .N, by = .(player_id, position)]
  data.table::setorder(pos_lookup, player_id, -N)
  modal_pos <- pos_lookup[, .SD[1L], by = player_id][, .(player_id, position)]
  ## Starter weight + non-starter weight per player
  agg <- lu[, .(
    weight_total = sum(weight),
    weight_start = sum(weight * as.numeric(is_starter)),
    weight_bench = sum(weight * as.numeric(!is_starter)),
    weight_sub_on = sum(weight * as.numeric(!is_starter & played)),
    mins_start_w = sum(weight * mins_clip * as.numeric(is_starter)),
    mins_sub_w   = sum(weight * mins_clip * as.numeric(!is_starter & played)),
    n_appear     = sum(played),
    n_start      = sum(is_starter & played),
    last_played  = suppressWarnings(max(match_date[played], na.rm = TRUE))
  ), by = .(player_id, player_name)]
  ## Replace position with modal non-Substitute, fallback to "Substitute"
  agg <- merge(agg, modal_pos, by = "player_id", all.x = TRUE)
  agg[is.na(position), position := "Substitute"]
  ## --- 3b. Recency filter: drop players with no recent appearance -----
  ## Fall back to no filter for teams where our data is too sparse to be
  ## picky (e.g. CONMEBOL teams without WC qualifiers in our Opta feed).
  recent_cutoff <- as.Date(as_of) - min_recent_days
  agg_filtered <- agg[!is.infinite(as.numeric(last_played)) &
                      last_played >= recent_cutoff]
  if (nrow(agg_filtered) >= 16L) {  # keep filter when >= realistic squad
    agg <- agg_filtered
  } else {
    ## sparse-data fallback -- keep everyone but mark for diagnostics
    ## (setattr, not attr<-: attr<- copies the data.table and the next :=
    ## then throws the "shallow copy" warning)
    data.table::setattr(agg, "recency_filter_skipped", TRUE)
  }

  ## --- 4. Compute probabilities and conditional means ------------------
  ## Beta prior centred on the squad-wide decay-weighted base rates. A
  ## one-cap starter gets p_start = (1 + k*base)/(1 + k) ~ 0.6, not 1.00 —
  ## and one omission then moves it ~0.1, not 0.5. prob_prior_k = 0
  ## reproduces the raw frequencies.
  base_start <- sum(agg$weight_start) / sum(agg$weight_total)
  base_sub <- if (sum(agg$weight_bench) > 0) {
    sum(agg$weight_sub_on) / sum(agg$weight_bench)
  } else {
    0
  }
  agg[, p_start := (weight_start + prob_prior_k * base_start) /
        (weight_total + prob_prior_k)]
  agg[, p_sub_given_bench := (weight_sub_on + prob_prior_k * base_sub) /
        (weight_bench + prob_prior_k)]
  if (prob_prior_k == 0) {
    ## avoid 0/0 NaN for players with no bench evidence under the legacy path
    agg[weight_bench == 0, p_sub_given_bench := 0]
  }
  agg[, mins_when_start := ifelse(weight_start > 0, mins_start_w / weight_start, 0)]
  agg[, mins_when_sub   := ifelse(weight_sub_on > 0, mins_sub_w / weight_sub_on, 0)]

  ## --- 5. Expected minutes per match (with sample-size shrinkage) -----
  agg[, em_raw := p_start * mins_when_start +
                   (1 - p_start) * p_sub_given_bench * mins_when_sub]
  ## Shrink toward 0 for players with little evidence
  agg[, expected_minutes := em_raw * weight_total / (weight_total + shrinkage_k)]

  ## --- 6. Take top `squad_size` by shrunk EM ----------------------------
  agg[, n_caps_weighted := round(weight_total, 1)]
  setorder(agg, -expected_minutes)
  squad <- head(agg, squad_size)

  ## --- 7. Two-bucket normalization: GKs share 90, outfield share 900 ---
  is_gk <- grepl("Goalkeeper", squad$position, ignore.case = TRUE)
  gk_em  <- sum(squad$expected_minutes[is_gk])
  out_em <- sum(squad$expected_minutes[!is_gk])
  squad[, expected_minutes_norm := 0]
  if (gk_em > 0)  squad[is_gk,  expected_minutes_norm := round(expected_minutes * 90  / gk_em,  1)]
  if (out_em > 0) squad[!is_gk, expected_minutes_norm := round(expected_minutes * 900 / out_em, 1)]
  squad[, pct_team_minutes := round(expected_minutes_norm / 990 * 100, 1)]
  setorder(squad, -expected_minutes_norm)

  as.data.frame(squad[, .(
    player_id, player_name, position,
    n_caps_weighted, n_start, n_appear,
    last_played,
    p_start = round(p_start, 3),
    mins_when_start = round(mins_when_start, 1),
    p_sub_given_bench = round(p_sub_given_bench, 3),
    mins_when_sub = round(mins_when_sub, 1),
    expected_minutes = round(expected_minutes, 1),
    expected_minutes_norm,
    pct_team_minutes
  )])
}

#' Build a minutes-weighted team rating
#'
#' @param team_em Output of `build_team_expected_minutes()`.
#' @param ratings Data frame with player_id + a numeric rating column.
#' @param rating_col Name of the rating column. Default `"panna"`.
#' @return Single numeric -- `sum(rating * expected_minutes_norm) / 990`.
#'   Equivalent to "what's the average panna rating of who'll be on the pitch."
#' @family expected minutes
#' @export
weight_rating_by_minutes <- function(team_em, ratings, rating_col = "panna") {
  if (nrow(team_em) == 0) return(NA_real_)
  ratings <- as.data.frame(ratings)
  merged <- merge(team_em, ratings[, c("player_id", rating_col)],
                  by = "player_id", all.x = TRUE)
  merged[[rating_col]][is.na(merged[[rating_col]])] <- 0
  sum(merged[[rating_col]] * merged$expected_minutes_norm) / 990
}
