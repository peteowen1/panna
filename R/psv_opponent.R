#' Opponent adjustment for per-match PSV (panna#220)
#'
#' PSV scores a player's box-score line for a single match. Nothing in that line
#' knows who the opposition was, so a defender facing a bottom-of-the-table
#' attack and one facing Manchester City are priced identically. PSR gets an
#' opponent control at training time (`07_train_psr_model.R` passes the
#' opponents' defensive ratings as unpenalized regressors) and EPR gets one in
#' its ridge (`opp_def_rating`); PSV alone had none.
#'
#' The adjustment is additive and deliberately minimal:
#'
#'   \code{psv_adj = psv - gamma * opp_def_rating}
#'
#' `opp_def_rating` is the opposing team's season defensive strength from
#' `cache-opta/team_season_strength.parquet`, in the xRAPM sign convention where
#' **negative is good defence**. So facing a strong defence (negative rating)
#' raises the adjusted value and facing a leaky one lowers it, for `gamma > 0`.
#'
#' @section Why a single global gamma:
#' Per-position or per-league gammas were not fitted. The relationship being
#' estimated is "how much of a player's box output is explained by the
#' opposition", which has no strong prior reason to differ by position, and the
#' project's experience with splitting a calibration across two axes at once is
#' that the joint cells lose power long before the marginals do (see
#' `docs/reference/RATING_CALIBRATION.md` on the position x season grid). Start
#' with the marginal; split it only if a residual check demands it.
#'
#' @section Not enabled by default:
#' This changes PSV, and PSV is the input to `build_league_network()`, which
#' produces the league offsets, which are added to PSR. Turning it on therefore
#' moves three things at once, and a comparison arm that differs on more than one
#' axis measures nothing. It must be validated as its own single-axis change
#' against a re-run baseline, not bundled into another retrain.
#'
#' @family psr
#' @name psv_opponent
NULL

#' Fit the PSV opponent-adjustment coefficient
#'
#' Regresses per-90 PSV on the opponent's defensive rating across player-matches
#' and returns the slope. Fitted on **starters only** and on a leak-free pairing
#' (season S-1 team strength against season S matches) for the same reasons the
#' position factors are: substitution is endogenous to match state, and a
#' same-season team rating contains the matches being explained.
#'
#' @param player_match A data.frame of player-match rows carrying `psv`,
#'   `opp_def_rating`, and a minutes column.
#' @param min_minutes Minimum minutes for a row to count. Default 45.
#' @param starters_only Drop `position == "Substitute"` rows. Default TRUE.
#' @return A list with `gamma`, `se`, `n_obs` and `r_squared`.
#' @family psr
#' @export
fit_psv_opponent_adjustment <- function(player_match,
                                        min_minutes = 45,
                                        starters_only = TRUE) {
  dt <- data.table::as.data.table(player_match)
  req <- c("psv", "opp_def_rating")
  miss <- setdiff(req, names(dt))
  if (length(miss) > 0) {
    cli::cli_abort("fit_psv_opponent_adjustment: missing {.field {miss}}")
  }

  mins_col <- intersect(c("minutes_played", "total_minutes"), names(dt))[1]
  if (is.na(mins_col)) {
    cli::cli_abort("fit_psv_opponent_adjustment: no minutes column found")
  }

  if (isTRUE(starters_only) && "position" %in% names(dt)) {
    n_before <- nrow(dt)
    dt <- dt[!is.na(position) & position != "Substitute" & position != ""]
    cli::cli_inform("Dropped {n_before - nrow(dt)} substitute row{?s}")
  }

  dt <- dt[!is.na(psv) & !is.na(opp_def_rating)]
  dt[, .mins := as.numeric(get(mins_col))]
  dt <- dt[.mins >= min_minutes]
  if (nrow(dt) < 1000) {
    cli::cli_abort("fit_psv_opponent_adjustment: only {nrow(dt)} usable rows")
  }

  ## PSV is scaled to minutes by the exporters, so put it back on a per-90
  ## footing before regressing -- otherwise the slope absorbs playing time.
  dt[, .psv90 := psv / (.mins / 90)]

  fit <- stats::lm(.psv90 ~ opp_def_rating, data = dt)
  co <- summary(fit)$coefficients
  list(
    gamma     = unname(co["opp_def_rating", "Estimate"]),
    se        = unname(co["opp_def_rating", "Std. Error"]),
    n_obs     = nrow(dt),
    r_squared = summary(fit)$r.squared
  )
}

#' Apply the PSV opponent adjustment
#'
#' Subtracts `gamma * opp_def_rating` from `psv`, preserving the
#' `osv + dsv == psv` identity by splitting the adjustment evenly across the two
#' components when they are present -- the same convention
#' `10b_export_game_logs.R` uses for the league offset.
#'
#' Rows with no `opp_def_rating` are left **unadjusted** rather than filled.
#' A constant fill is what made the opponent control inert in eight
#' competitions (panna#224); leaving a row alone is honest and visible in the
#' returned count.
#'
#' @param game_logs A data.frame with `psv` and `opp_def_rating`.
#' @param gamma The coefficient from [fit_psv_opponent_adjustment()].
#' @param verbose Print how many rows were adjusted.
#' @return `game_logs` with `psv` adjusted and `psv_opp_adjustment` added.
#' @family psr
#' @export
apply_psv_opponent_adjustment <- function(game_logs, gamma, verbose = FALSE) {
  dt <- data.table::as.data.table(game_logs)
  if (!"psv" %in% names(dt)) {
    cli::cli_abort("apply_psv_opponent_adjustment: no {.field psv} column")
  }
  if (!"opp_def_rating" %in% names(dt)) {
    cli::cli_warn("apply_psv_opponent_adjustment: no {.field opp_def_rating}; returning unchanged")
    dt[, psv_opp_adjustment := 0]
    return(dt[])
  }
  if (!is.numeric(gamma) || length(gamma) != 1L || !is.finite(gamma)) {
    cli::cli_abort("apply_psv_opponent_adjustment: {.arg gamma} must be one finite number")
  }

  ## Scale the per-90 adjustment to each row's minutes, because psv is
  ## minutes-scaled by the time it reaches here.
  mins_col <- intersect(c("minutes_played", "total_minutes"), names(dt))[1]
  mins_scale <- if (!is.na(mins_col)) {
    s <- as.numeric(dt[[mins_col]]) / 90
    s[is.na(s) | s < 0] <- 0
    s
  } else {
    rep(1, nrow(dt))
  }

  dt[, psv_opp_adjustment := 0]
  ok <- !is.na(dt$opp_def_rating)
  dt[ok, psv_opp_adjustment := -gamma * opp_def_rating * mins_scale[ok]]
  dt[, psv := psv + psv_opp_adjustment]

  ## Keep osv + dsv == psv. An opponent's defensive strength suppresses
  ## offensive output and flatters defensive output, but PSV carries no
  ## per-component evidence to split it any other way, so split it evenly --
  ## the same choice made for the league offset in 10b.
  if (all(c("osv", "dsv") %in% names(dt))) {
    dt[, osv := osv + psv_opp_adjustment / 2]
    dt[, dsv := dsv + psv_opp_adjustment / 2]
  }

  if (isTRUE(verbose)) {
    cli::cli_inform(paste(
      "PSV opponent adjustment (gamma = {round(gamma, 5)}): adjusted {sum(ok)}",
      "of {nrow(dt)} rows ({round(100 * mean(ok), 1)}%);",
      "{sum(!ok)} left unadjusted for want of an opponent rating."
    ))
  }
  dt[]
}
