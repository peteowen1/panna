# Bradley-Terry team rating
#
# Given a set of fixtures with predicted W/D/L probabilities, fit a single
# strength rating per team that best reproduces those probabilities. Used to
# back out a clean national-team strength from match-prediction output for
# tournament displays (e.g. WC 2026).

#' Fit Bradley-Terry-Davidson team ratings
#'
#' Given match-level W/D/L probabilities (from the prediction pipeline), fit a
#' single strength rating per team. Uses the Davidson (1970) extension that
#' folds draws into Bradley-Terry: `P(draw) prop nu.exp((r_i+r_j)/2)`.
#'
#' Optimisation: minimise cross-entropy between predicted probs and BT-implied
#' probs via L-BFGS-B. Ratings are centered to mean zero for interpretability.
#'
#' @param predictions Data frame with columns `home_team`, `away_team`,
#'   `prob_H`, `prob_D`, `prob_A`.
#' @param neutral Logical or vector of same length as `nrow(predictions)`.
#'   If TRUE, the home-field advantage parameter is set to zero for that row.
#'   Default FALSE (every row treated as a home/away pair).
#' @param max_iter Integer. L-BFGS-B max iterations. Default 200.
#' @param verbose Logical. Print fit diagnostics. Default TRUE.
#'
#' @return A list with:
#'   \itemize{
#'     \item `ratings`: data frame of team / rating / rank
#'     \item `home_adv`: scalar home-advantage parameter (log-odds)
#'     \item `nu`: draw-frequency parameter
#'     \item `loss`: final cross-entropy loss
#'     \item `converged`: optim convergence flag
#'   }
#' @family world cup simulation
#' @export
fit_bt_ratings <- function(predictions,
                           neutral = FALSE,
                           max_iter = 200L,
                           verbose = TRUE) {
  req <- c("home_team", "away_team", "prob_H", "prob_D", "prob_A")
  missing <- setdiff(req, names(predictions))
  if (length(missing) > 0) {
    stop("Missing columns in predictions: ", paste(missing, collapse = ", "))
  }

  teams <- sort(unique(c(predictions$home_team, predictions$away_team)))
  teams <- teams[teams != ""]
  n_teams <- length(teams)
  if (n_teams < 3) stop("Need at least 3 teams to fit BT ratings; got ", n_teams)

  ## Filter to rows where both teams are non-empty
  ok <- predictions$home_team %in% teams & predictions$away_team %in% teams
  pred <- predictions[ok, , drop = FALSE]
  if (nrow(pred) == 0) stop("No usable rows after filtering")

  ## Recycle neutral flag
  if (length(neutral) == 1L) neutral <- rep(neutral, nrow(pred))
  neutral <- as.logical(neutral)

  idx_home <- match(pred$home_team, teams)
  idx_away <- match(pred$away_team, teams)

  ## Stack target probs in cross-entropy weights
  p_obs <- cbind(H = pred$prob_H, D = pred$prob_D, A = pred$prob_A)
  ## Guard against zeros (log(0) = -Inf)
  p_obs <- pmax(p_obs, 1e-6)
  p_obs <- p_obs / rowSums(p_obs)

  ## Parameter vector: ratings (n_teams), log_nu, home_adv
  ## Ratings are centered to mean 0 inside the loss function
  init <- c(rep(0, n_teams), log(0.5), 0.25)

  loss_fn <- function(par) {
    r <- par[1:n_teams]
    r <- r - mean(r)                       # center to keep id'd
    nu <- exp(par[n_teams + 1L])
    hadv <- par[n_teams + 2L]
    rh <- r[idx_home] + ifelse(neutral, 0, hadv)
    ra <- r[idx_away]
    eH <- exp(rh)
    eA <- exp(ra)
    eD <- nu * exp((rh + ra) / 2)
    z <- eH + eA + eD
    pH <- eH / z; pD <- eD / z; pA <- eA / z
    ## cross-entropy: -sum p_obs * log(p_model)
    -sum(p_obs[, 1] * log(pH) + p_obs[, 2] * log(pD) + p_obs[, 3] * log(pA))
  }

  fit <- stats::optim(init, loss_fn,
                      method = "L-BFGS-B",
                      control = list(maxit = max_iter, factr = 1e7))

  r <- fit$par[1:n_teams]; r <- r - mean(r)
  out <- data.frame(
    team   = teams,
    rating = round(r, 4),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$rating), ]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL

  if (verbose) {
    cli::cli_alert_success("BT fit: loss={round(fit$value, 3)}, home_adv={round(fit$par[n_teams+2], 3)}, nu={round(exp(fit$par[n_teams+1]), 3)}")
  }

  list(
    ratings   = out,
    home_adv  = fit$par[n_teams + 2L],
    nu        = exp(fit$par[n_teams + 1L]),
    loss      = fit$value,
    converged = fit$convergence == 0L
  )
}

#' Convert BT ratings to a match probability
#'
#' @param r_home Rating of home team
#' @param r_away Rating of away team
#' @param home_adv Home-advantage param (log-odds). Pass 0 for neutral venue.
#' @param nu Draw-frequency param.
#' @return Named vector with `prob_H`, `prob_D`, `prob_A`.
#' @family world cup simulation
#' @export
bt_match_prob <- function(r_home, r_away, home_adv = 0, nu = 0.5) {
  rh <- r_home + home_adv
  ra <- r_away
  eH <- exp(rh); eA <- exp(ra); eD <- nu * exp((rh + ra) / 2)
  z <- eH + eA + eD
  c(prob_H = eH / z, prob_D = eD / z, prob_A = eA / z)
}
