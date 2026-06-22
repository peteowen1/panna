# League quality offsets vs UCL group stage
# ===========================================
# For cross-league rating fairness: a player's per-90 EPV in the Eredivisie is
# not directly comparable to their per-90 EPV in the Champions League -- top
# domestic leagues have weaker average opponents so the same player produces
# more per-90 EPV there. To put all leagues on a single "UCL-equivalent" scale
# we estimate, for each league L, an empirical offset:
#
#     offset_L = mean(anchor_y - league_y)         [anchor = UCL group stage]
#
# averaged over players who appear in both league_L and the anchor (and
# recency-weighted toward recent seasons).
#
# Three estimators are used in cascade, picking the best-supported per league:
#
#   1. same-season   -- same player, same season, both leagues  (cleanest, no
#                      career-stage confound; needs European competition matches)
#   2. career-direct -- same player across career, both leagues (broader sample
#                      but contaminated by career stage -- younger in tier-2,
#                      peak in tier-1)
#   3. chained       -- for leagues with no direct anchor bridge (e.g., BRA, ENG2)
#                      chain via an intermediate league L' that has both a
#                      bridge to L and to the anchor: offset_L = (L' - L) + (anchor - L')
#
# Applied in calculate_epr_regression() by subtracting offset from each row's
# y_off / y_def before fitting, so beta_player is a single, globally comparable
# "UCL-equivalent per-90 EPV" number.


#' Compute per-league per-90 EPV offsets vs an anchor league
#'
#' Estimates an additive offset for each league L of the form
#' \code{offset_L = mean(anchor_y - league_y)} on the per-90 EPV scale, where
#' \code{anchor} defaults to UCL group stage. Offsets are recency-weighted
#' (exponential decay, half-life \code{half_life} years).
#'
#' Three estimators are tried per league, in priority order:
#' \enumerate{
#'   \item \code{"same-season"} -- same player, same season, both leagues.
#'         Cleanest because it eliminates career-stage confounds.
#'   \item \code{"career-direct"} -- same player across career, both leagues.
#'         Broader coverage; some bias from career-stage effects.
#'   \item \code{"chained"} -- for leagues with no direct anchor bridge, chain
#'         via an intermediate league with both bridges.
#' }
#'
#' @param game_logs Per-game EPV data. Required columns: \code{player_id},
#'   \code{match_date}, \code{league}, \code{minutes_played} (or
#'   \code{total_minutes}), \code{epv_offensive}, \code{epv_defensive}.
#'   Typically the rbinded \code{game_logs_*.parquet} files in
#'   \code{data-raw/cache-predictions-opta/}.
#' @param ref_year Reference year used to compute recency weight. Defaults to
#'   \code{max(year(match_date)) + 1} (i.e., the upcoming season).
#' @param half_life Years for exponential decay of season weights (default 3).
#' @param anchor_league Anchor league string (default \code{"UCL"}). Offsets
#'   are reported relative to this league's group-stage games.
#' @param exclude_qualifiers If TRUE (default), drops UCL/UEL/UECL qualifier
#'   rounds (matches before September 15 of a season-end-year) which feature
#'   weaker teams and would inflate the anchor's apparent difficulty.
#' @param min_ucl_n,min_dom_n Minimum number of games in each league for a
#'   bridging player-season to count (defaults 4 and 8).
#' @param min_ucl_mins,min_dom_mins Minimum minutes in each league for a
#'   bridging player-season (defaults 180 and 720).
#' @param min_n_for_same_season Minimum player-seasons needed before
#'   \code{"same-season"} is preferred over career-trajectory (default 20).
#' @param min_career_games Minimum games per league for a career-trajectory
#'   bridge (default 10).
#' @param chain_intermediates Candidate intermediate leagues for chaining
#'   leagues without direct anchor bridge.
#' @param prefer_chained_for Character vector of league codes for which the
#'   chained estimate should be used even when a direct career-trajectory
#'   bridge is available. Default \code{c("BRA")} because Brazilian-league
#'   bridges to UCL are dominated by career-stage bias (young-and-developing
#'   in Brazil, peak in Europe), so the direct estimate is unreliable.
#' @param verbose If TRUE (default), prints a per-league summary.
#'
#' @return A data.table with one row per league plus an anchor row:
#'   \describe{
#'     \item{league}{League code.}
#'     \item{method}{\code{"same-season"}, \code{"career-direct"},
#'                    \code{"chained"}, or \code{"anchor"}.}
#'     \item{anchor}{Anchor league used.}
#'     \item{n_obs}{Number of bridging player-seasons (same-season) or
#'                   player-pairs (career/chained).}
#'     \item{offset_off, offset_def, offset_tot}{Per-90 EPV offsets on the
#'                                                 anchor-equivalent scale.}
#'   }
#' @export
compute_league_offsets <- function(game_logs,
                                     ref_year = NULL,
                                     half_life = 3,
                                     anchor_league = "UCL",
                                     exclude_qualifiers = TRUE,
                                     min_ucl_n = 4L,
                                     min_dom_n = 8L,
                                     min_ucl_mins = 180,
                                     min_dom_mins = 720,
                                     min_n_for_same_season = 20L,
                                     min_career_games = 10L,
                                     chain_intermediates = c("POR","ESP","ITA","ENG","FRA","GER"),
                                     prefer_chained_for = c("BRA"),
                                     verbose = TRUE) {
  dt <- data.table::as.data.table(game_logs)
  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(sub("Z$","", as.character(match_date)))]
  }
  # Accept either minutes_played or total_minutes
  if (!"minutes_played" %in% names(dt) && "total_minutes" %in% names(dt)) {
    dt[, minutes_played := total_minutes]
  }
  # Accept epv_offensive_adj / epv_defensive_adj if those are the only available cols
  if (!"epv_offensive" %in% names(dt) && "epv_offensive_adj" %in% names(dt)) {
    dt[, epv_offensive := epv_offensive_adj]
  }
  if (!"epv_defensive" %in% names(dt) && "epv_defensive_adj" %in% names(dt)) {
    dt[, epv_defensive := epv_defensive_adj]
  }
  needed <- c("player_id","match_date","league","minutes_played",
              "epv_offensive","epv_defensive")
  miss <- setdiff(needed, names(dt))
  if (length(miss)) {
    cli::cli_abort("compute_league_offsets: missing required columns: {.field {miss}}")
  }
  dt <- dt[!is.na(epv_offensive) & !is.na(epv_defensive) & !is.na(league)]

  dt[, season_end_year := data.table::fifelse(
    data.table::month(match_date) >= 7L,
    data.table::year(match_date) + 1L,
    data.table::year(match_date))]
  if (isTRUE(exclude_qualifiers)) {
    dt[, .is_q := (league %in% c("UCL","UEL","UECL")) &
                    data.table::month(match_date) < 9L]
    dt <- dt[.is_q == FALSE]
    dt[, .is_q := NULL]
  }
  if (is.null(ref_year)) {
    ref_year <- max(data.table::year(dt$match_date), na.rm = TRUE) + 1L
  }
  dt[, mins_frac := pmax(as.numeric(minutes_played), 1) / 90]
  dt[, y_off := epv_offensive / mins_frac]
  dt[, y_def := epv_defensive / mins_frac]
  dt[, y_tot := y_off + y_def]
  dt[, match_year := as.numeric(format(match_date, "%Y"))]
  dt[, season_weight := 2 ^ (-(ref_year - match_year) / half_life)]

  all_leagues <- setdiff(unique(dt$league), c(anchor_league, NA, ""))

  # --------------------------------------------------------------------------
  # 1. Same-season pairwise vs anchor
  # --------------------------------------------------------------------------
  anchor_stats <- dt[league == anchor_league, .(
    a_mins  = sum(minutes_played, na.rm = TRUE),
    a_y_off = stats::weighted.mean(y_off, w = minutes_played, na.rm = TRUE),
    a_y_def = stats::weighted.mean(y_def, w = minutes_played, na.rm = TRUE),
    a_y_tot = stats::weighted.mean(y_tot, w = minutes_played, na.rm = TRUE),
    a_n     = .N,
    sw      = season_weight[1]
  ), by = .(player_id, season_end_year)]

  ss_results <- list()
  for (lg in all_leagues) {
    dom <- dt[league == lg, .(
      d_mins  = sum(minutes_played, na.rm = TRUE),
      d_y_off = stats::weighted.mean(y_off, w = minutes_played, na.rm = TRUE),
      d_y_def = stats::weighted.mean(y_def, w = minutes_played, na.rm = TRUE),
      d_y_tot = stats::weighted.mean(y_tot, w = minutes_played, na.rm = TRUE),
      d_n     = .N
    ), by = .(player_id, season_end_year)]
    pairs <- merge(anchor_stats, dom, by = c("player_id","season_end_year"))
    pairs <- pairs[a_n >= min_ucl_n & d_n >= min_dom_n &
                     a_mins >= min_ucl_mins & d_mins >= min_dom_mins]
    if (nrow(pairs) < 5) next
    pairs[, w := pmin(a_mins, d_mins) * sw]
    ss_results[[lg]] <- data.table::data.table(
      league = lg, method = "same-season",
      n_obs = nrow(pairs),
      offset_off = stats::weighted.mean(pairs$a_y_off - pairs$d_y_off,
                                          w = pairs$w, na.rm = TRUE),
      offset_def = stats::weighted.mean(pairs$a_y_def - pairs$d_y_def,
                                          w = pairs$w, na.rm = TRUE),
      offset_tot = stats::weighted.mean(pairs$a_y_tot - pairs$d_y_tot,
                                          w = pairs$w, na.rm = TRUE)
    )
  }
  ss <- data.table::rbindlist(ss_results, fill = TRUE)

  # --------------------------------------------------------------------------
  # 2. Career-trajectory pairwise for every league (used for chaining + fallback)
  # --------------------------------------------------------------------------
  pl <- dt[, .(
    tot_w     = sum(minutes_played * season_weight, na.rm = TRUE),
    mean_y_off = stats::weighted.mean(y_off,
                                        w = minutes_played * season_weight,
                                        na.rm = TRUE),
    mean_y_def = stats::weighted.mean(y_def,
                                        w = minutes_played * season_weight,
                                        na.rm = TRUE),
    mean_y_tot = stats::weighted.mean(y_tot,
                                        w = minutes_played * season_weight,
                                        na.rm = TRUE),
    n_games    = .N
  ), by = .(player_id, league)]

  compute_bridge <- function(lg_a, lg_b) {
    a <- pl[league == lg_a & n_games >= min_career_games,
              .(player_id, a_w = tot_w,
                 a_y_off = mean_y_off, a_y_def = mean_y_def, a_y_tot = mean_y_tot)]
    b <- pl[league == lg_b & n_games >= min_career_games,
              .(player_id, b_w = tot_w,
                 b_y_off = mean_y_off, b_y_def = mean_y_def, b_y_tot = mean_y_tot)]
    p <- merge(a, b, by = "player_id")
    if (nrow(p) < 5) return(NULL)
    p[, bw := pmin(a_w, b_w)]
    data.table::data.table(
      from = lg_a, to = lg_b, n_obs = nrow(p),
      gap_off = stats::weighted.mean(p$b_y_off - p$a_y_off, w = p$bw, na.rm = TRUE),
      gap_def = stats::weighted.mean(p$b_y_def - p$a_y_def, w = p$bw, na.rm = TRUE),
      gap_tot = stats::weighted.mean(p$b_y_tot - p$a_y_tot, w = p$bw, na.rm = TRUE)
    )
  }

  # Direct career bridges TO anchor
  ct_results <- list()
  for (lg in all_leagues) {
    b <- compute_bridge(lg, anchor_league)
    if (is.null(b)) next
    ct_results[[lg]] <- data.table::data.table(
      league = lg, method = "career-direct",
      n_obs = b$n_obs,
      offset_off = b$gap_off, offset_def = b$gap_def, offset_tot = b$gap_tot
    )
  }
  ct <- data.table::rbindlist(ct_results, fill = TRUE)

  # --------------------------------------------------------------------------
  # 3. Chained bridges for leagues without direct anchor bridge
  # --------------------------------------------------------------------------
  # Leagues that need (or should also have) a chained estimate:
  #   - any league with no same-season AND no career-direct bridge
  #   - any league explicitly flagged in prefer_chained_for (career-direct
  #     exists but is known to be contaminated, e.g. BRA career-stage bias)
  needs_chain <- union(
    setdiff(all_leagues, c(ss$league, ct$league)),
    intersect(prefer_chained_for, all_leagues)
  )
  chained <- data.table::data.table()
  if (length(needs_chain) && nrow(ct)) {
    chain_results <- list()
    for (lg in needs_chain) {
      via_candidates <- intersect(chain_intermediates, ct$league)
      bv_list <- list()
      for (via in via_candidates) {
        b <- compute_bridge(lg, via)
        if (is.null(b)) next
        ca <- ct[league == via]
        if (nrow(ca) == 0) next
        bv_list[[via]] <- data.table::data.table(
          via = via, n_obs = b$n_obs,
          gap_off = b$gap_off + ca$offset_off,
          gap_def = b$gap_def + ca$offset_def,
          gap_tot = b$gap_tot + ca$offset_tot
        )
      }
      if (length(bv_list) == 0) next
      bv <- data.table::rbindlist(bv_list)
      chain_results[[lg]] <- data.table::data.table(
        league = lg, method = "chained",
        n_obs = sum(bv$n_obs),
        offset_off = stats::weighted.mean(bv$gap_off, w = bv$n_obs, na.rm = TRUE),
        offset_def = stats::weighted.mean(bv$gap_def, w = bv$n_obs, na.rm = TRUE),
        offset_tot = stats::weighted.mean(bv$gap_tot, w = bv$n_obs, na.rm = TRUE)
      )
    }
    chained <- data.table::rbindlist(chain_results, fill = TRUE)
  }

  # --------------------------------------------------------------------------
  # 4. Combine: pick best-supported method per league
  # --------------------------------------------------------------------------
  combined <- data.table::rbindlist(list(ss, ct, chained), fill = TRUE)
  # Per-league method priority. For leagues in prefer_chained_for we shift
  # chained above career-direct so the (career-stage-biased) direct estimate
  # is overridden when a chained alternative exists.
  combined[, priority := data.table::fcase(
    method == "same-season"   & n_obs >= min_n_for_same_season,   1L,
    method == "same-season",                                       2L,
    method == "chained" & league %in% prefer_chained_for,          3L,
    method == "career-direct",                                     4L,
    method == "chained",                                           5L
  )]
  data.table::setorder(combined, league, priority)
  rec <- combined[, .SD[1L], by = league]
  rec[, priority := NULL]
  rec[, anchor := anchor_league]

  # Add anchor row (zero offset by construction)
  rec <- data.table::rbindlist(list(rec, data.table::data.table(
    league = anchor_league, method = "anchor", n_obs = NA_integer_,
    offset_off = 0, offset_def = 0, offset_tot = 0, anchor = anchor_league
  )), fill = TRUE)
  data.table::setorder(rec, offset_tot)

  if (isTRUE(verbose)) {
    cat(sprintf("\n== League offsets vs %s (ref_year=%d, half_life=%g) ==\n",
                anchor_league, ref_year, half_life))
    cat("offset_tot = anchor_y - league_y on per-90 EPV scale.\n")
    cat("Negative = league is structurally weaker (player y inflated there).\n\n")
    print(rec[, .(league, method, n_obs,
                    offset_off = round(offset_off, 3),
                    offset_def = round(offset_def, 3),
                    offset_tot = round(offset_tot, 3))])
  }
  rec[]
}


#' Estimate per-league strength from the full same-season co-occurrence network
#'
#' A connected, all-bridges alternative to \code{\link{compute_league_offsets}}.
#' Instead of bridging every league only to a single anchor (UCL), it uses
#' \strong{every} same-season pairing a player straddles — domestic league,
#' continental cup (UCL/UEL), and international tournament (WC/Euro/Copa) — and
#' solves the whole graph at once via a player-season fixed effect.
#'
#' Concretely it regresses each player-season-league per-90 \code{value_col} on
#' league dummies plus a player-season fixed effect (implemented as a
#' within-player-season demeaning). The fixed effect absorbs each player's
#' overall level, so the league coefficient is the \emph{within-player-season}
#' league effect — i.e. how much more value a player generates in that league
#' than in their other competitions that season ("league ease"). The returned
#' \code{offset} is the negation (the amount to ADD to a player's value to put
#' them on a league-neutral, Big-5-equivalent scale).
#'
#' Versus \code{compute_league_offsets} this (a) multiplies the effective bridge
#' count (UCL goes from a lone anchor to thousands of bridges), (b) connects
#' isolated leagues through the international web (an A-League player who played
#' the World Cup links A-League into the graph without ever touching UCL), and
#' (c) is metric-agnostic — run it on \code{psv} to league-adjust PSR, on
#' \code{epv_total} for EPR, etc. Each metric gets its own table on its own scale.
#'
#' @param game_logs Per-game data.frame/data.table with \code{player_id},
#'   \code{league}, \code{total_minutes}, the \code{value_col}, and either
#'   \code{season} ("YYYY-YYYY"/"YYYY") or \code{season_end_year}.
#' @param value_col Per-game value column to league-adjust (e.g. \code{"psv"},
#'   \code{"epv_total"}). Default \code{"psv"}.
#' @param big5 League codes whose mean is the zero anchor. Default the five
#'   major European leagues (game-log 3-letter codes).
#' @param min_mins Minimum minutes in a (player, season, league) cell for it to
#'   count as a bridge endpoint. Default 270 (≈3 full games).
#' @param shrink_k Small-sample shrinkage: each league's offset is multiplied by
#'   \code{n_bridge / (n_bridge + shrink_k)} so a thin-N league can't over-swing.
#'   Default 3 (gentle — only meaningfully damps n < ~10). Set 0 to disable.
#' @param verbose Print the table. Default TRUE.
#'
#' @return A data.table: \code{league}, \code{strength} (raw league-ease
#'   coefficient), \code{offset} (\code{= -strength * shrink}, add to value to
#'   neutralize), \code{n_bridge} (multi-league player-seasons touching it).
#'
#' @seealso \code{\link{compute_league_offsets}}, \code{\link{compute_psr_league_offsets}}
#' @export
build_league_network <- function(game_logs, value_col = "psv",
                                 big5 = c("ENG", "ESP", "GER", "ITA", "FRA"),
                                 min_mins = 270, shrink_k = 3, verbose = TRUE) {
  dt <- data.table::as.data.table(game_logs)
  if (!value_col %in% names(dt)) {
    cli::cli_abort("build_league_network: value column {.field {value_col}} not found")
  }
  if (!"season_end_year" %in% names(dt)) {
    if ("season" %in% names(dt)) {
      dt[, season_end_year := as.integer(sub(".*-", "", as.character(season)))]
    } else if ("match_date" %in% names(dt)) {
      d <- as.Date(sub("Z$", "", as.character(dt$match_date)))
      dt[, season_end_year := data.table::fifelse(
        data.table::month(d) >= 7L, data.table::year(d) + 1L, data.table::year(d))]
    } else {
      cli::cli_abort("build_league_network: need season / season_end_year / match_date")
    }
  }
  v <- dt[[value_col]]
  pc <- dt[!is.na(v) & !is.na(league),
           .(val = sum(.SD[[1]], na.rm = TRUE) / pmax(sum(total_minutes, na.rm = TRUE) / 90, 1),
             mins = sum(total_minutes, na.rm = TRUE)),
           by = .(player_id, season_end_year, league), .SDcols = value_col]
  pc <- pc[mins >= min_mins]
  pc[, ps := paste(player_id, season_end_year)]
  pc[, nlg := .N, by = ps]
  multi <- pc[nlg >= 2L]                       # player-seasons spanning >=2 leagues
  if (nrow(multi) < 10L) {
    cli::cli_warn("build_league_network: too few multi-league player-seasons; returning zero offsets.")
    lvls <- sort(unique(pc$league))
    return(data.table::data.table(league = lvls, strength = 0, offset = 0, n_bridge = 0L))
  }
  multi[, w := mins]
  multi[, val_dm := val - stats::weighted.mean(val, w), by = ps]   # FE: demean value
  lvls <- sort(unique(multi$league))
  X <- vapply(lvls, function(L) as.numeric(multi$league == L), numeric(nrow(multi)))
  Xdm <- X
  for (j in seq_along(lvls)) {                  # FE: demean each league-incidence column
    multi[, .xj := X[, j]]
    multi[, .xjdm := .xj - stats::weighted.mean(.xj, w), by = ps]
    Xdm[, j] <- multi$.xjdm
  }
  fit <- stats::lm.wfit(Xdm, multi$val_dm, multi$w)
  beta <- fit$coefficients
  names(beta) <- lvls
  beta[is.na(beta)] <- 0
  beta <- beta - mean(beta[intersect(big5, lvls)], na.rm = TRUE)   # anchor: Big-5 mean = 0
  nL <- multi[, .(n_bridge = .N), by = league]
  out <- data.table::data.table(league = lvls, strength = as.numeric(beta))
  out <- merge(out, nL, by = "league", all.x = TRUE)
  out[is.na(n_bridge), n_bridge := 0L]
  out[, offset := -strength * (n_bridge / (n_bridge + shrink_k))]  # discount, small-N shrunk
  data.table::setorder(out, offset)
  if (isTRUE(verbose)) {
    cat(sprintf("\n== League network strength on '%s' (Big-5=0; %d bridges, shrink_k=%g) ==\n",
                value_col, nrow(multi), shrink_k))
    cat("offset = add to a player's value to neutralize league (negative = weaker league).\n")
    print(out[, .(league, n_bridge, offset = round(offset, 3))])
  }
  out[]
}
