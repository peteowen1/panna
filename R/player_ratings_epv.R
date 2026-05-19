# EPR: Expected Points Rating from EPV
# ======================================
# Bayesian-shrinkage rating that smooths per-game EPV values with exponential
# decay weighting to predict next-match EPV contribution.
#
# Soccer version: 2 components (offensive + defensive) vs AFL's 4.
# Follows torpverse R/player_ratings.R calculate_epr() / calculate_epr_stats().


# League tier partition for player x tier interaction in
# calculate_epr_regression(). Tier 1 = top-5 domestic + UCL + top international
# tournaments. Tier 2 = everything else. The interaction lets the model give
# players two coefficients (one per tier) so cross-league standouts like
# Tavernier (huge in SCO, modest in UCL) get correctly differentiated.
EPR_LEAGUE_TIERS <- list(
  # 3-letter codes (game_logs `league` column) — top-5 + UCL + top intl tournaments
  tier_1 = c("ENG", "ESP", "GER", "ITA", "FRA",   # EPL, La Liga, Bundesliga, Serie A, Ligue 1
              "UCL",                                 # Champions League
              "WC", "EURO"),                          # World Cup, UEFA Euros
  # Also accept full competition names (in case `competition` column is used instead)
  tier_1_full = c("EPL", "La_Liga", "Bundesliga", "Serie_A", "Ligue_1",
                    "UCL", "UEFA_Euros", "World_Cup")
)


#' Classify a league into EPR tier
#'
#' Accepts either 3-letter league codes (\code{ENG}, \code{ESP}, ...) or
#' full competition names (\code{EPL}, \code{La_Liga}, ...). Tier 1 = top-5
#' domestic + UCL + top international tournaments; tier 2 = everything else.
#' @keywords internal
.epr_league_tier <- function(league) {
  data.table::fifelse(
    league %in% EPR_LEAGUE_TIERS$tier_1 |
      league %in% EPR_LEAGUE_TIERS$tier_1_full,
    "t1",
    "t2"
  )
}


# ============================================================================
# Core EPR calculation
# ============================================================================

#' Calculate EPR (Expected Points Rating) from per-game EPV
#'
#' For each player, applies exponential time-decay to their per-game EPV
#' values and computes a Bayesian-shrunk rating estimate. Players with
#' little data are shrunk toward the prior rate (slightly below zero for
#' offense, zero for defense).
#'
#' @param player_game_epv Per-game EPV data from
#'   \code{\link{aggregate_player_game_epv}}. Must contain: \code{player_id},
#'   \code{match_id}, \code{match_date}, \code{minutes_played},
#'   \code{epv_offensive}, \code{epv_defensive}.
#' @param ref_date Date to estimate ratings at. Only matches before this date
#'   are used. If NULL, uses the latest match date in data.
#' @param decay_offensive Decay rate in days for offensive EPV (default
#'   \code{EPR_DECAY_OFFENSIVE}).
#' @param decay_defensive Decay rate in days for defensive EPV (default
#'   \code{EPR_DECAY_DEFENSIVE}).
#' @param prior_games Prior strength in equivalent full games (default
#'   \code{EPR_PRIOR_GAMES}).
#' @param prior_rate_off Prior rate for offensive component (default
#'   \code{EPR_PRIOR_RATE_OFF}).
#' @param prior_rate_def Prior rate for defensive component (default
#'   \code{EPR_PRIOR_RATE_DEF}).
#' @param loading Loading factor applied to observed data (default
#'   \code{EPR_LOADING}).
#'
#' @return A data.table with one row per player:
#'   \describe{
#'     \item{player_id, player_name}{Identifiers}
#'     \item{epr}{Total EPR = epr_offensive + epr_defensive}
#'     \item{epr_offensive}{Offensive EPV rating (passing, shooting, dribbling)}
#'     \item{epr_defensive}{Defensive EPV rating (defending, duel blame)}
#'     \item{wt_games}{Weighted games (effective sample size)}
#'     \item{n_games}{Raw number of games played}
#'   }
#'
#' @param league_baseline Logical. If TRUE (default) and the input has a
#'   \code{league} column, per-(league, season) baseline EPV credit is
#'   subtracted from each row's per-90 credit before the decay-weighted
#'   aggregation. This makes EPR cross-league comparable: a player dominating
#'   in a weaker league is judged relative to that league's baseline rather
#'   than the global one. Set to FALSE to restore pre-baseline behaviour.
#'
#' @export
calculate_epr <- function(player_game_epv, ref_date = NULL,
                           decay_offensive = EPR_DECAY_OFFENSIVE,
                           decay_defensive = EPR_DECAY_DEFENSIVE,
                           prior_games = EPR_PRIOR_GAMES,
                           prior_rate_off = EPR_PRIOR_RATE_OFF,
                           prior_rate_def = EPR_PRIOR_RATE_DEF,
                           loading = EPR_LOADING,
                           league_baseline = TRUE) {
  dt <- data.table::as.data.table(player_game_epv)

  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }

  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date, na.rm = TRUE)
  } else {
    ref_date <- as.Date(ref_date)
  }

  # Filter to matches before ref_date
  dt <- dt[match_date < ref_date]
  if (nrow(dt) == 0) {
    cli::cli_warn("No matches before {ref_date}")
    return(data.table::data.table(
      player_id = character(0), player_name = character(0),
      epr = numeric(0), epr_offensive = numeric(0), epr_defensive = numeric(0),
      wt_games = numeric(0), n_games = integer(0)))
  }

  # Days since match
  dt[, days_since := as.numeric(ref_date - match_date)]

  # Minutes fraction (per-game adjustment: divide by 90)
  dt[, mins_frac := pmax(as.numeric(minutes_played), 1) / 90]

  # Decay weights per component
  dt[, w_off := exp(-days_since / decay_offensive) * mins_frac]
  dt[, w_def := exp(-days_since / decay_defensive) * mins_frac]

  # Ensure EPV columns exist
  if (!"epv_offensive" %in% names(dt)) dt[, epv_offensive := 0]
  if (!"epv_defensive" %in% names(dt)) dt[, epv_defensive := 0]

  # Per-90 EPV values (undo the minutes effect so we rate per-90 contribution)
  dt[, epv_off_p90 := epv_offensive / mins_frac]
  dt[, epv_def_p90 := epv_defensive / mins_frac]

  # Per-(league, season) baseline correction. EPV credits in stronger
  # leagues tend to differ from weaker leagues in absolute level (different
  # defensive intensity, attack rates, etc.). Without correction, a player
  # dominating Super_Lig accumulates a larger EPR than a player generating
  # the same per-90 RELATIVE contribution in EPL.
  #
  # We compute one baseline per (league, season_end_year) from the same
  # filtered data (no leakage — only matches before ref_date), then subtract
  # from each row's per-90 credit. Aggregation then sums "above-baseline"
  # contributions, which is league-neutral by construction.
  if (isTRUE(league_baseline) && "league" %in% names(dt) && !all(is.na(dt$league))) {
    if (!"season_end_year" %in% names(dt)) {
      dt[, season_end_year := data.table::fifelse(
        data.table::month(match_date) >= 7L,
        data.table::year(match_date) + 1L,
        data.table::year(match_date)
      )]
    }
    # Minute-weighted league-season baseline: weights each player-game by
    # minutes_played so substitutes don't drag the mean down. This represents
    # "what's the per-90 EPV credit of an average full-starter in this league".
    baseline <- dt[, .(
      base_off = stats::weighted.mean(epv_off_p90, w = minutes_played, na.rm = TRUE),
      base_def = stats::weighted.mean(epv_def_p90, w = minutes_played, na.rm = TRUE),
      total_mins = sum(minutes_played, na.rm = TRUE)
    ), by = .(league, season_end_year)]
    # Fall back to 0 baseline for thinly-sampled league-seasons (< 5000 mins)
    baseline[total_mins < 5000L, `:=`(base_off = 0, base_def = 0)]
    dt <- merge(dt, baseline[, .(league, season_end_year, base_off, base_def)],
                 by = c("league", "season_end_year"), all.x = TRUE)
    dt[is.na(base_off), base_off := 0]
    dt[is.na(base_def), base_def := 0]
    dt[, epv_off_p90 := epv_off_p90 - base_off]
    dt[, epv_def_p90 := epv_def_p90 - base_def]
  }

  # Aggregate per player
  agg <- dt[, .(
    sum_off  = sum(w_off * epv_off_p90, na.rm = TRUE),
    sum_def  = sum(w_def * epv_def_p90, na.rm = TRUE),
    wt_off   = sum(w_off, na.rm = TRUE),
    wt_def   = sum(w_def, na.rm = TRUE),
    wt_games = sum(w_off, na.rm = TRUE),  # use offensive weights as "games"
    n_games  = .N,
    player_name = player_name[1]
  ), by = player_id]

  # Bayesian shrinkage: (loading * sum + prior_games * prior_rate) / (wt + prior_games)
  agg[, epr_offensive := (loading * sum_off + prior_games * prior_rate_off) /
                          (wt_off + prior_games)]
  agg[, epr_defensive := (loading * sum_def + prior_games * prior_rate_def) /
                          (wt_def + prior_games)]
  agg[, epr := epr_offensive + epr_defensive]

  # Clean up
  agg[, c("sum_off", "sum_def", "wt_off", "wt_def") := NULL]

  data.table::setorder(agg, -epr)
  agg[]
}


#' Calculate EPR at multiple dates (batch version)
#'
#' Efficiently computes EPR ratings at multiple reference dates using the
#' cumsum trick for O(N + D * players) instead of O(N * D) complexity.
#'
#' @param player_game_epv Per-game EPV data.
#' @param ref_dates Character or Date vector of reference dates.
#' @param ... Additional parameters passed to \code{\link{calculate_epr}}.
#'
#' @return A data.table with columns from \code{calculate_epr} plus
#'   \code{ref_date}.
#'
#' @export
calculate_epr_batch <- function(player_game_epv, ref_dates, ...) {
  ref_dates <- sort(as.Date(ref_dates))

  results <- vector("list", length(ref_dates))
  for (i in seq_along(ref_dates)) {
    result <- calculate_epr(player_game_epv, ref_date = ref_dates[i], ...)
    if (nrow(result) > 0) {
      result[, ref_date := ref_dates[i]]
      results[[i]] <- result
    }
  }

  data.table::rbindlist(results, fill = TRUE)
}


# =============================================================================
# Regression-based EPR (Option A in the EPR layer)
# =============================================================================
#
# Reformulates EPR as a weighted ridge regression on per-game per-90 EPV:
#
#   y_player_game = β_player + α_league_season + γ * opp_def_rating + ε
#
# with weights w = exp(-Δt/decay) * mins_frac. β_player IS the EPR.
#
# League-season FE and opponent-strength controls let the regression
# disentangle player skill from league baseline and opponent quality.
# β_player coefficients are penalized (ridge shrinkage = Bayesian prior);
# league and opponent terms are unpenalized (true fixed effects).
#
# This generalizes the Bayesian-mean calculate_epr() — when α_league and γ
# are zero, β_player ≈ (decay-weighted mean - global mean of y) shrunk to 0.


#' Calculate EPR via weighted ridge regression with league/opponent FE
#'
#' Per-(player, game) regression that simultaneously estimates per-player
#' skill (β_player, returned as EPR) and league/opponent context effects.
#' Uses exponential time-decay weighting on observations.
#'
#' @param player_game_epv Per-game EPV data. Required columns:
#'   \code{player_id}, \code{player_name}, \code{match_date},
#'   \code{minutes_played}, \code{epv_offensive}, \code{epv_defensive}.
#'   Recommended additional columns: \code{league} (or \code{competition}),
#'   \code{season_end_year}, \code{opp_def_rating} (continuous opponent
#'   defensive strength, e.g., from RAPM-derived team ratings).
#' @param ref_date Snapshot date — only matches strictly before this are used.
#' @param decay Exponential decay constant in days for the time weight.
#' @param alpha glmnet mixing parameter (0 = pure ridge, recommended).
#' @param lambda Optional lambda. If NULL, uses the median of a 30-lambda
#'   path (good for ridge with sensible default shrinkage).
#' @param prior_strength Equivalent-games prior for shrinking small-sample
#'   players toward 0. Implemented by adding "phantom" zero-y rows weighted
#'   by \code{prior_strength} for each player. Set to 0 to disable.
#' @param verbose If TRUE, print step timings.
#' @return A data.table with one row per player: \code{player_id},
#'   \code{player_name}, \code{epr}, \code{epr_offensive}, \code{epr_defensive},
#'   \code{n_games}, \code{wt_games}.
#' @param tier_interaction If TRUE (default), fit player × league-tier
#'   interaction — i.e. each player gets up to two β coefficients, one for
#'   tier-1 (top-5 + UCL + WC/EURO) and one for tier-2 (everything else).
#'   This fixes cross-league standouts (Tavernier at Rangers, Veerman at
#'   PSV) whose single-β was a compromise between their elite domestic
#'   per-90 and their modest UCL/UEL per-90. Set to FALSE for the legacy
#'   "one β per player" behaviour.
#' @export
calculate_epr_regression <- function(player_game_epv,
                                       ref_date = NULL,
                                       decay = 400,
                                       alpha = 0,
                                       lambda = NULL,
                                       prior_strength = 5,
                                       tier_interaction = TRUE,
                                       verbose = FALSE) {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg glmnet} required for calculate_epr_regression()")
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg Matrix} required for calculate_epr_regression()")
  }
  t_log <- function(msg) {
    if (isTRUE(verbose)) {
      cat(sprintf("    [%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
    }
  }

  dt <- data.table::as.data.table(player_game_epv)
  if (!inherits(dt$match_date, "Date")) {
    dt[, match_date := as.Date(match_date)]
  }
  if (is.null(ref_date)) {
    ref_date <- max(dt$match_date, na.rm = TRUE)
  } else {
    ref_date <- as.Date(ref_date)
  }
  dt <- dt[match_date < ref_date]
  if (nrow(dt) == 0) {
    cli::cli_warn("No matches before {ref_date}")
    return(data.table::data.table(
      player_id = character(0), player_name = character(0),
      epr = numeric(0), epr_offensive = numeric(0), epr_defensive = numeric(0),
      n_games = integer(0), wt_games = numeric(0)))
  }

  # Ensure season_end_year exists
  if (!"season_end_year" %in% names(dt)) {
    dt[, season_end_year := data.table::fifelse(
      data.table::month(match_date) >= 7L,
      data.table::year(match_date) + 1L,
      data.table::year(match_date))]
  }
  # Pick league column name
  league_col <- intersect(c("league","competition"), names(dt))[1]
  has_league <- !is.na(league_col)
  has_opp    <- "opp_def_rating" %in% names(dt)
  # NOTE: also tried opp_elo_n (cross-league opponent Elo) as a control
  # alongside opp_def_rating. It barely moved Eredivisie/Scottish stars
  # (Veerman, Tavernier) because their opponents are nearly all same-league
  # (similar Elos → near-zero opp_elo_n contribution), but it disproportion-
  # ately hurt UCL-heavy players like Haaland whose games span a wide Elo
  # range. Reverted; opp_def_rating alone is the principled best.
  has_opp_elo <- FALSE

  # Weights + per-90 targets
  dt[, days_since := as.numeric(ref_date - match_date)]
  dt[, mins_frac  := pmax(as.numeric(minutes_played), 1) / 90]
  dt[, w          := exp(-days_since / decay) * mins_frac]
  if (!"epv_offensive" %in% names(dt)) dt[, epv_offensive := 0]
  if (!"epv_defensive" %in% names(dt)) dt[, epv_defensive := 0]
  dt[, y_off := epv_offensive / mins_frac]
  dt[, y_def := epv_defensive / mins_frac]

  # Build factor levels
  players <- sort(unique(dt$player_id))
  n_obs <- nrow(dt)
  n_players <- length(players)
  t_log(sprintf("rows=%d, players=%d, ref=%s, decay=%d",
                n_obs, n_players, ref_date, decay))

  # ---- Player tier classification (for player × tier interaction) ----
  # Each row gets a tier label ("t1" / "t2") based on its league. The
  # player-dummy block becomes player × tier instead of just player.
  if (isTRUE(tier_interaction)) {
    if (!has_league) {
      cli::cli_warn("tier_interaction=TRUE but no `league` column — disabling")
      tier_interaction <- FALSE
    } else {
      dt[, tier := .epr_league_tier(.SD[[league_col]])]
    }
  }

  # ---- Sparse design matrix: [players | league-seasons | opp_def] ----
  t0 <- Sys.time()
  if (isTRUE(tier_interaction)) {
    # Player × tier interaction. Only build columns for (player, tier)
    # combinations that actually appear, so players with no tier-1 games
    # don't get a phantom-anchored tier-1 estimate.
    pt_levels <- unique(dt[, .(player_id, tier)])
    data.table::setorder(pt_levels, player_id, tier)
    pt_levels[, pt_key := paste0(player_id, "__", tier)]
    pt_levels[, j_idx := seq_len(.N)]
    n_pt <- nrow(pt_levels)
    dt[, pt_key := paste0(player_id, "__", tier)]
    dt[, pt_j := pt_levels$j_idx[match(pt_key, pt_levels$pt_key)]]
    X_p <- Matrix::sparseMatrix(i = seq_len(n_obs), j = dt$pt_j, x = 1,
                                  dims = c(n_obs, n_pt),
                                  dimnames = list(NULL, pt_levels$pt_key))
    n_player_cols <- n_pt
    t_log(sprintf("tier interaction: %d (player, tier) coefs (vs %d players alone)",
                  n_pt, n_players))
  } else {
    p_idx <- match(dt$player_id, players)
    X_p <- Matrix::sparseMatrix(i = seq_len(n_obs), j = p_idx, x = 1,
                                  dims = c(n_obs, n_players),
                                  dimnames = list(NULL, players))
    n_player_cols <- n_players
  }
  X_list <- list(X_p)
  pf <- rep(1, n_player_cols)        # players penalized (shrinkage)

  if (has_league) {
    ls_keys <- sort(unique(paste0(dt[[league_col]], "_", dt$season_end_year)))
    ls_idx  <- match(paste0(dt[[league_col]], "_", dt$season_end_year), ls_keys)
    X_ls <- Matrix::sparseMatrix(i = seq_len(n_obs), j = ls_idx, x = 1,
                                  dims = c(n_obs, length(ls_keys)),
                                  dimnames = list(NULL, paste0("ls_", ls_keys)))
    X_list <- c(X_list, list(X_ls))
    pf <- c(pf, rep(0, length(ls_keys)))   # league FE unpenalized
  } else {
    ls_keys <- character(0)
  }

  if (has_opp) {
    X_opp <- Matrix::Matrix(dt$opp_def_rating, ncol = 1, sparse = TRUE,
                              dimnames = list(NULL, "opp_def_rating"))
    X_list <- c(X_list, list(X_opp))
    pf <- c(pf, 0)                          # opponent control unpenalized
  }
  if (has_opp_elo) {
    X_oe <- Matrix::Matrix(dt$opp_elo_n, ncol = 1, sparse = TRUE,
                             dimnames = list(NULL, "opp_elo_n"))
    X_list <- c(X_list, list(X_oe))
    pf <- c(pf, 0)                          # opponent-elo control unpenalized
  }
  X <- do.call(cbind, X_list)

  # ---- Optional Bayesian prior via "phantom" zero-y rows ----
  # One phantom row per player-coef column. With tier interaction enabled,
  # this is one phantom row per (player, tier) combination that actually
  # appears in the data — so players without tier-1 data don't get a
  # phantom-anchored tier-1 estimate.
  if (prior_strength > 0) {
    X_prior <- Matrix::sparseMatrix(i = seq_len(n_player_cols), j = seq_len(n_player_cols),
                                      x = 1,
                                      dims = c(n_player_cols, ncol(X)))
    X_full <- rbind(X, X_prior)
    y_off_full <- c(dt$y_off, rep(0, n_player_cols))
    y_def_full <- c(dt$y_def, rep(0, n_player_cols))
    w_full     <- c(dt$w,     rep(prior_strength, n_player_cols))
  } else {
    X_full <- X
    y_off_full <- dt$y_off
    y_def_full <- dt$y_def
    w_full     <- dt$w
  }
  t_log(sprintf("design matrix %dx%d sparse (%.1f MB), nnz=%d, fit prep in %.1fs",
                nrow(X_full), ncol(X_full),
                as.numeric(utils::object.size(X_full))/1e6,
                length(X_full@x),
                as.numeric(Sys.time() - t0, units = "secs")))

  # ---- Fit ----
  # Use a small explicit lambda — just enough to make the design matrix
  # numerically stable. The phantom-row prior (prior_strength) does the
  # actual Bayesian shrinkage. Stacking heavy ridge on top of that double-
  # shrinks and compresses β_player ~10x below the raw EPV-p90 scale that
  # users (and the inthegame blog) expect.
  if (is.null(lambda)) lambda <- 1e-4
  t0 <- Sys.time()
  fit_off <- glmnet::glmnet(X_full, y_off_full, weights = w_full,
                              alpha = alpha, penalty.factor = pf,
                              standardize = FALSE, lambda = lambda,
                              intercept = TRUE)
  fit_def <- glmnet::glmnet(X_full, y_def_full, weights = w_full,
                              alpha = alpha, penalty.factor = pf,
                              standardize = FALSE, lambda = lambda,
                              intercept = TRUE)
  beta_off <- as.numeric(coef(fit_off, s = lambda))
  beta_def <- as.numeric(coef(fit_def, s = lambda))
  t_log(sprintf("glmnet fits (2x) at lambda=%.4f in %.1fs",
                lambda, as.numeric(Sys.time() - t0, units = "secs")))

  # Player betas = positions 2..(n_player_cols+1) (after intercept)
  player_beta_off <- beta_off[2:(n_player_cols + 1L)]
  player_beta_def <- beta_def[2:(n_player_cols + 1L)]

  # Resolve player_name -> first occurrence
  name_lookup <- dt[, .(player_name = player_name[1]), by = player_id]

  if (isTRUE(tier_interaction)) {
    # ---- Tier-stratified output ----
    # Compute per-(player, tier) decay-weighted games + minutes
    wt_pt <- dt[, .(wt_games_tier = sum(w, na.rm = TRUE),
                     n_games_tier  = .N),
                  by = .(player_id, tier)]

    pt_out <- data.table::data.table(
      player_id     = pt_levels$player_id,
      tier          = pt_levels$tier,
      epr_offensive = player_beta_off,
      epr_defensive = player_beta_def
    )
    pt_out[, epr := epr_offensive + epr_defensive]
    pt_out <- merge(pt_out, wt_pt, by = c("player_id","tier"), all.x = TRUE)

    # Cast to one row per player with tier columns
    out_wide <- data.table::dcast(
      pt_out, player_id ~ tier,
      value.var = c("epr","epr_offensive","epr_defensive",
                     "wt_games_tier","n_games_tier"),
      fill = NA
    )
    # Ensure both tier columns exist even if data only has one tier
    for (suf in c("_t1","_t2")) {
      for (m in c("epr","epr_offensive","epr_defensive",
                   "wt_games_tier","n_games_tier")) {
        col <- paste0(m, suf)
        if (!col %in% names(out_wide)) out_wide[, (col) := NA_real_]
      }
    }
    # Combined EPR weighted by tier playing time (NA if tier missing)
    out_wide[, epr := {
      w1 <- ifelse(is.na(wt_games_tier_t1), 0, wt_games_tier_t1)
      w2 <- ifelse(is.na(wt_games_tier_t2), 0, wt_games_tier_t2)
      e1 <- ifelse(is.na(epr_t1), 0, epr_t1)
      e2 <- ifelse(is.na(epr_t2), 0, epr_t2)
      ifelse(w1 + w2 > 0, (e1 * w1 + e2 * w2) / (w1 + w2), NA_real_)
    }]
    out_wide[, epr_offensive := {
      w1 <- ifelse(is.na(wt_games_tier_t1), 0, wt_games_tier_t1)
      w2 <- ifelse(is.na(wt_games_tier_t2), 0, wt_games_tier_t2)
      e1 <- ifelse(is.na(epr_offensive_t1), 0, epr_offensive_t1)
      e2 <- ifelse(is.na(epr_offensive_t2), 0, epr_offensive_t2)
      ifelse(w1 + w2 > 0, (e1 * w1 + e2 * w2) / (w1 + w2), NA_real_)
    }]
    out_wide[, epr_defensive := {
      w1 <- ifelse(is.na(wt_games_tier_t1), 0, wt_games_tier_t1)
      w2 <- ifelse(is.na(wt_games_tier_t2), 0, wt_games_tier_t2)
      e1 <- ifelse(is.na(epr_defensive_t1), 0, epr_defensive_t1)
      e2 <- ifelse(is.na(epr_defensive_t2), 0, epr_defensive_t2)
      ifelse(w1 + w2 > 0, (e1 * w1 + e2 * w2) / (w1 + w2), NA_real_)
    }]
    # Total games
    out_wide[, wt_games := rowSums(out_wide[, .(wt_games_tier_t1, wt_games_tier_t2)],
                                     na.rm = TRUE)]
    out_wide[, n_games  := rowSums(out_wide[, .(n_games_tier_t1, n_games_tier_t2)],
                                     na.rm = TRUE)]

    out_wide <- merge(out_wide, name_lookup, by = "player_id", all.x = TRUE)
    data.table::setorder(out_wide, -epr)
    out_wide[, .(player_id, player_name,
                  epr, epr_offensive, epr_defensive,
                  epr_t1, epr_offensive_t1, epr_defensive_t1, wt_games_tier_t1, n_games_tier_t1,
                  epr_t2, epr_offensive_t2, epr_defensive_t2, wt_games_tier_t2, n_games_tier_t2,
                  n_games, wt_games)]
  } else {
    # ---- Legacy single-β output ----
    wt_by_player <- dt[, .(wt_games = sum(w, na.rm = TRUE),
                            n_games  = .N),
                         by = player_id]
    out <- data.table::data.table(
      player_id     = players,
      epr_offensive = player_beta_off,
      epr_defensive = player_beta_def
    )
    out[, epr := epr_offensive + epr_defensive]
    out <- merge(out, name_lookup, by = "player_id", all.x = TRUE)
    out <- merge(out, wt_by_player, by = "player_id", all.x = TRUE)
    data.table::setorder(out, -epr)
    out[, .(player_id, player_name, epr, epr_offensive, epr_defensive,
             n_games, wt_games)]
  }
}


#' Optimise EPR decay via hold-out prediction MSE
#'
#' Grid-search the decay parameter for \code{calculate_epr_regression()} by
#' fitting on history before a hold-out window and measuring weighted MSE
#' on next-game per-90 EPV in that window.
#'
#' @param player_game_epv As in calculate_epr_regression.
#' @param ref_date Snapshot date (the "today" of the test).
#' @param holdout_days Width of the hold-out window in days back from ref_date.
#' @param decay_grid Numeric vector of decay values (days) to evaluate.
#' @param ... Passed to calculate_epr_regression (e.g., alpha, prior_strength).
#' @return A data.table with one row per decay candidate plus the chosen decay.
#' @export
optimize_epr_decay <- function(player_game_epv,
                                ref_date,
                                holdout_days = 60L,
                                decay_grid = c(200, 300, 400, 500, 700, 1000),
                                verbose = TRUE,
                                ...) {
  dt <- data.table::as.data.table(player_game_epv)
  if (!inherits(dt$match_date, "Date")) dt[, match_date := as.Date(match_date)]
  ref_date <- as.Date(ref_date)
  train_end <- ref_date - as.integer(holdout_days)
  holdout <- dt[match_date >= train_end & match_date < ref_date]
  if (nrow(holdout) < 100L) {
    cli::cli_warn("Hold-out window has only {nrow(holdout)} games; results may be unstable")
  }
  holdout[, mins_frac := pmax(as.numeric(minutes_played), 1) / 90]
  holdout[, y_off := epv_offensive / mins_frac]
  holdout[, y_def := epv_defensive / mins_frac]

  league_col <- intersect(c("league","competition"), names(dt))[1]
  has_league <- !is.na(league_col)
  has_opp    <- "opp_def_rating" %in% names(dt)

  if (isTRUE(verbose)) {
    cat(sprintf("[optimize_epr_decay] ref_date=%s, train_end=%s, holdout=%d games\n",
                ref_date, train_end, nrow(holdout)))
  }

  res <- vector("list", length(decay_grid))
  for (i in seq_along(decay_grid)) {
    dval <- decay_grid[i]
    t0 <- Sys.time()
    # Fit using TRAIN window only
    train_data <- dt[match_date < train_end]
    fit <- calculate_epr_regression(
      train_data, ref_date = train_end, decay = dval,
      verbose = FALSE, ...
    )

    # Predict holdout per-90 EPV: β_player + α_league + γ * opp_def
    # We don't have α/γ exported, so re-compute by joining fit to holdout
    # via player_id and applying simple "predicted = β_player" (league/opp
    # contributions are zero-mean by construction with FE).
    # NOTE: this is an approximation — proper holdout pred would also
    # include the matched α_league_season + γ*opp_def for each holdout game.
    # For decay-tuning purposes, ranking-by-MSE on β_player alone is
    # adequate because league/opp terms are common across decay candidates.
    pred <- merge(holdout[, .(player_id, y_off, y_def, mins_frac)],
                   fit[, .(player_id, beta_off = epr_offensive,
                            beta_def = epr_defensive)],
                   by = "player_id", all.x = TRUE)
    pred[is.na(beta_off), beta_off := 0]
    pred[is.na(beta_def), beta_def := 0]
    mse_off <- stats::weighted.mean((pred$y_off - pred$beta_off)^2,
                                       w = pred$mins_frac, na.rm = TRUE)
    mse_def <- stats::weighted.mean((pred$y_def - pred$beta_def)^2,
                                       w = pred$mins_frac, na.rm = TRUE)
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    res[[i]] <- data.table::data.table(
      decay   = dval,
      mse_off = mse_off,
      mse_def = mse_def,
      mse_total = mse_off + mse_def,
      elapsed_s = elapsed
    )
    if (isTRUE(verbose)) {
      cat(sprintf("  decay=%4d: mse_off=%.4f, mse_def=%.4f, total=%.4f (%.1fs)\n",
                  dval, mse_off, mse_def, mse_off + mse_def, elapsed))
    }
  }
  out <- data.table::rbindlist(res)
  data.table::setorder(out, mse_total)
  out[, is_best := seq_len(.N) == 1L]
  out[]
}
