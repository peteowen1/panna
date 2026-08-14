# 07d. Derive PSV_RELIABILITY_GD_SCALE (LIVE-PSV-UNBLOCK D1-v2 FINAL, 2026-07-20)
# ===========================================================================
# WHAT THIS DERIVES: the single multiplicative constant that puts the
# reliability-shrunk display PSV (calculate_psv(reliability = )) in units of
# "expected goal-difference contribution per 90". Per-match lambda-priced
# player scores (beta/sd * lambda * per-90 stat, summed over the coefficient
# vector) are aggregated to team-match level (minutes-weighted sum over each
# team's XI, home minus away) and regressed against match goal_diff; the
# fitted slope IS the scale that makes "sum of players' lambda-priced PSV"
# predict actual match GD with slope 1.
#
# DECISION (Pete-approved 2026-07-20; see
# pannaverse/docs/plans/LIVE-PSV-UNBLOCK-2026-07-20.md, "D1-v2 FINAL"):
#   - A two-regressor fit (goal_diff ~ diff_outfield + diff_gk, SEPARATE
#     coefficients per population) gives c_outfield ~= 5.13 and c_gk ~= 25.
#   - c_gk is REJECTED: it is inflated by team-context leakage in the GK
#     reliability artifact (#159 — the GK-population lambda decomposition
#     still leans on team-level defensive outcomes it shouldn't see), not a
#     genuine ~5x GK skill-to-GD premium. Shipping it would silently
#     double-count team defense on every keeper's display PSV.
#   - Instead there is ONE pooled constant for BOTH populations: of the two
#     fitted coefficients, only c_outfield is adopted -- PSV_RELIABILITY_GD_SCALE
#     in R/constants.R. GKs use it too, until #159 retrains the GK artifact.
#
# REPRODUCES (2026-07-20 run, n = 13,548 matches with full home+away outfield
# AND GK data): c_outfield = 5.134, c_gk = 25.39 (rejected), R^2 = 0.31,
# t(c_outfield) ~= 59.
#
# The GK term in the regression below uses a one-off gamma=1 PREDICTIVE lambda
# (future-window weighted-OLS slope) as a CONTROL variable only -- this is NOT
# a change to production GK pricing (psv_match_reliability.csv / the MoM
# artifact stays the shipped GK lambda everywhere else, per the same D1-v2
# FINAL decision: "predictive-lambda re-estimate is a photo-finish, not worth
# swapping"). It exists here solely so this one-time calibration regression
# reproduces the exact c_gk value that gets rejected, for provenance.
#
# PSV_RELIABILITY_GD_SCALE in R/constants.R is NOT written automatically by
# this script -- update it BY HAND from the printed c_outfield value after any
# retrain of psv_match_reliability.csv (07b) or the PSR/PSV coefficients
# (07_train_psr_model.R).
#
# Run standalone from panna/:
#   cd panna && Rscript data-raw/estimated-skills/07d_derive_psv_gd_scale.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
library(data.table)

SPLIT_DATE <- as.Date("2024-07-01")  # estimation (<) / evaluation (>=) window split
FUTURE_N <- 20L                      # future-window size for the GK predictive-lambda control

## ---------------------------------------------------------------------------
## 1. Load + narrow match_stats to the feature union of every trained
##    coefficient set (margin/offense/defense, blend target, both populations)
## ---------------------------------------------------------------------------
nz_feats <- function(type, model) {
  cd <- load_psr_coefficients(type, target = "blend", model = model)
  cd$stat_name[cd$beta != 0]
}
of_feats <- unique(c(nz_feats("margin", "outfield"), nz_feats("offense", "outfield"),
                     nz_feats("defense", "outfield")))
gk_feats <- unique(c(nz_feats("margin", "gk"), nz_feats("offense", "gk"),
                     nz_feats("defense", "gk")))
cli::cli_alert_info("outfield nonzero-beta features: {length(of_feats)} | gk: {length(gk_feats)}")

ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))
ms[, match_date := as.Date(match_date)]

meta_cols <- intersect(c("match_id", "player_id", "player_name", "team_name", "opponent_team",
                          "match_date", "league", "season", "position", "position_side",
                          "primary_position", "total_minutes", "is_home", "goals", "competition"),
                        names(ms))
raw_feat_cols <- intersect(union(of_feats, gk_feats), names(ms))
ms <- ms[total_minutes >= 20, c(meta_cols, raw_feat_cols), with = FALSE]
cli::cli_alert_info("match_stats rows after minutes>=20 filter: {format(nrow(ms), big.mark=',')}")

ms <- enrich_match_stats_with_xmetrics(ms, verbose = TRUE, fail_if_missing_frac = 0.6)
gc(verbose = FALSE)

## ---------------------------------------------------------------------------
## 2. Population split + position/era centering (identical convention to
##    compute_player_psv / 07b_build_position_means.R)
## ---------------------------------------------------------------------------
is_gk <- .detect_gk_rows(ms)
ms[, population := ifelse(is_gk, "gk", "outfield")]
cli::cli_alert_info("outfield rows: {format(sum(!is_gk), big.mark=',')} | gk rows: {format(sum(is_gk), big.mark=',')}")

position_means <- load_position_role_means()
ms <- .position_normalize_skills(ms, position_means)

ms_of <- ms[population == "outfield"]
ms_gk <- ms[population == "gk"]
of_feats_avail <- intersect(of_feats, names(ms_of))
gk_feats_avail <- intersect(gk_feats, names(ms_gk))

## ---------------------------------------------------------------------------
## 3. Outfield lambda-priced score: the SHIPPED MoM artifact
##    (psv_match_reliability.csv) -- production pricing, no re-estimation.
## ---------------------------------------------------------------------------
reliability <- load_psv_match_reliability()

margin_of <- as.data.table(load_psr_coefficients("margin", target = "blend", model = "outfield"))
margin_of <- margin_of[stat_name %in% of_feats_avail]
margin_of[, sd := ifelse(is.na(sd) | sd == 0, 1, sd)]

score_with_lambda <- function(dt, coef_df, lambda_lookup) {
  feats <- coef_df$stat_name
  lam <- unname(lambda_lookup[feats])
  lam[is.na(lam)] <- 1  # missing/NA lambda -> unshrunk, mirrors calculate_psv's fallback
  Xmat <- as.matrix(dt[, feats, with = FALSE]); Xmat[is.na(Xmat)] <- 0
  as.numeric(Xmat %*% (coef_df$beta / coef_df$sd * lam))
}
lam_mom_of <- setNames(reliability[model == "outfield"]$lambda, reliability[model == "outfield"]$stat_name)
ms_of[, score_mom := score_with_lambda(ms_of, margin_of, lam_mom_of)]

## ---------------------------------------------------------------------------
## 4. GK lambda-priced CONTROL score: gamma=1 predictive lambda (future-window
##    weighted-OLS slope, estimation window < SPLIT_DATE). See header note --
##    NOT shipped GK pricing, exists only to reproduce the rejected c_gk here.
##    Restricted to the (much smaller) GK population for tractability.
## ---------------------------------------------------------------------------
compute_future_means <- function(dt, feats) {
  setorder(dt, player_id, match_date, match_id)
  dt[, .idx := seq_len(.N), by = player_id]
  dt[, .nplayer := .N, by = player_id]
  for (f in feats) {
    ssum_col <- paste0(f, "__ssum")
    dt[, (ssum_col) := rev(cumsum(rev(get(f)))), by = player_id]
    nxt1  <- shift(dt[[ssum_col]], n = 1L, type = "lead")
    nxt21 <- shift(dt[[ssum_col]], n = FUTURE_N + 1L, type = "lead")
    # shift() with no `by` follows global row order; groups are contiguous
    # (sorted by player_id, match_date, match_id), so a plain shift is correct
    # except it can read into the next player's group near a group's tail --
    # null those boundary-crossing positions explicitly via the idx/nplayer bounds.
    nxt1[dt$.idx + 1L > dt$.nplayer] <- NA_real_
    nxt21[dt$.idx + FUTURE_N + 1L > dt$.nplayer] <- NA_real_
    sum_future <- fifelse(is.na(nxt1), 0, nxt1) - fifelse(is.na(nxt21), 0, nxt21)
    count_future <- pmin(FUTURE_N, dt$.nplayer - dt$.idx)
    fut_col <- paste0(f, "__futmean")
    dt[, (fut_col) := fifelse(count_future > 0, sum_future / count_future, NA_real_)]
    dt[, (ssum_col) := NULL]
  }
  dt[, c(".idx", ".nplayer") := NULL]
  dt[]
}
ms_gk <- compute_future_means(ms_gk, gk_feats_avail)

wls_slope <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  if (length(x) < 30) return(NA_real_)
  sw <- sum(w); xbar <- sum(w * x) / sw; ybar <- sum(w * y) / sw
  sxx <- sum(w * (x - xbar)^2); sxy <- sum(w * (x - xbar) * (y - ybar))
  if (sxx <= 0) return(NA_real_)
  sxy / sxx
}
est_gk <- ms_gk[match_date < SPLIT_DATE]
w_est <- as.numeric(est_gk$total_minutes) / 90
lambda_pred_gk <- vapply(gk_feats_avail, function(f) {
  wls_slope(est_gk[[f]], est_gk[[paste0(f, "__futmean")]], w_est)
}, numeric(1))
names(lambda_pred_gk) <- gk_feats_avail

# fallback: predictive -> MoM -> 1 (same resolution the D1-v2 predictive-lambda
# evaluation gate used)
lam_mom_gk <- setNames(reliability[model == "gk"]$lambda, reliability[model == "gk"]$stat_name)
lambda_gk_final <- ifelse(is.na(lambda_pred_gk), unname(lam_mom_gk[names(lambda_pred_gk)]), lambda_pred_gk)
names(lambda_gk_final) <- names(lambda_pred_gk)
lambda_gk_final[is.na(lambda_gk_final)] <- 1

margin_gk <- as.data.table(load_psr_coefficients("margin", target = "blend", model = "gk"))
margin_gk <- margin_gk[stat_name %in% gk_feats_avail]
margin_gk[, sd := ifelse(is.na(sd) | sd == 0, 1, sd)]
ms_gk[, score_pred := score_with_lambda(ms_gk, margin_gk, lambda_gk_final)]

## ---------------------------------------------------------------------------
## 5. Team-match aggregation on the EVALUATION window (>= SPLIT_DATE):
##    minutes-weighted sum of lambda-priced scores per team, home minus away.
## ---------------------------------------------------------------------------
eval_of <- ms_of[match_date >= SPLIT_DATE]
eval_gk <- ms_gk[match_date >= SPLIT_DATE]

team_totals <- rbindlist(list(
  eval_of[, .(match_id, team_name, is_home, goals)],
  eval_gk[, .(match_id, team_name, is_home, goals)]
))[, .(team_goals = sum(goals, na.rm = TRUE), is_home = is_home[1]), by = .(match_id, team_name)]
home_t <- team_totals[is_home == 1]; away_t <- team_totals[is_home == 0]
match_outcome <- merge(
  home_t[, .(match_id, home_goals = team_goals)],
  away_t[, .(match_id, away_goals = team_goals)], by = "match_id")
match_outcome[, goal_diff := home_goals - away_goals]

team_pop_sum <- function(dt, score_col) {
  dt[, .(team_score = sum(get(score_col) * total_minutes / 90), is_home = is_home[1]),
     by = .(match_id, team_name)]
}
of_sum <- team_pop_sum(eval_of, "score_mom")
gk_sum <- team_pop_sum(eval_gk, "score_pred")

home_of <- of_sum[is_home == 1]; away_of <- of_sum[is_home == 0]
home_gk <- gk_sum[is_home == 1]; away_gk <- gk_sum[is_home == 0]
setnames(home_of, "team_score", "hof"); setnames(away_of, "team_score", "aof")
setnames(home_gk, "team_score", "hgk"); setnames(away_gk, "team_score", "agk")

mm <- merge(match_outcome, home_of[, .(match_id, hof)], by = "match_id")
mm <- merge(mm, away_of[, .(match_id, aof)], by = "match_id")
mm <- merge(mm, home_gk[, .(match_id, hgk)], by = "match_id")
mm <- merge(mm, away_gk[, .(match_id, agk)], by = "match_id")
mm[, diff_of := hof - aof]
mm[, diff_gk := hgk - agk]
cli::cli_alert_info("matches with full home+away outfield AND gk data: {nrow(mm)}")

## ---------------------------------------------------------------------------
## 6. Two-regressor GD calibration -- goal_diff ~ diff_of + diff_gk. This
##    reproduces the rejected c_gk (#159 team-context leak) alongside the
##    adopted c_outfield.
## ---------------------------------------------------------------------------
fit2 <- lm(goal_diff ~ diff_of + diff_gk, data = mm)
s2 <- summary(fit2)
c_outfield <- unname(coef(fit2)["diff_of"])
c_gk <- unname(coef(fit2)["diff_gk"])
se_of <- s2$coefficients["diff_of", "Std. Error"]

cat("\n=== Two-regressor GD calibration (goal_diff ~ diff_outfield + diff_gk) ===\n")
print(s2$coefficients)
cat(sprintf("R^2 = %.4f | n = %d | t(c_outfield) = %.1f\n", s2$r.squared, nrow(mm), c_outfield / se_of))

## ---------------------------------------------------------------------------
## 7. Pooled single-c decision: of the two fitted coefficients, adopt ONLY
##    c_outfield as the ONE global scale for BOTH populations -- c_gk is
##    discarded (see header). This is PSV_RELIABILITY_GD_SCALE.
## ---------------------------------------------------------------------------
cat(sprintf("\nc_outfield = %.4f  <- ADOPT as PSV_RELIABILITY_GD_SCALE (BOTH populations)\n", c_outfield))
cat(sprintf("c_gk       = %.4f  <- REJECT (#159 team-context leak in GK reliability); GKs use c_outfield\n", c_gk))
cat(sprintf("\n>>> PSV_RELIABILITY_GD_SCALE = %.3f -- update R/constants.R BY HAND <<<\n", round(c_outfield, 3)))

## Staleness check against the SHIPPED constant. Not an abort: producing a new
## number is the whole point of running this, so a divergence here is normal
## immediately after a retrain. It is loud because the failure mode is silence
## -- the shipped value sat at 5.134 from 2026-07-21 to 2026-08-14 (it should
## have been ~4.888) because two coefficient retrains landed and nobody re-ran
## this script. Nothing else in the pipeline reads the fitted slope back.
shipped <- tryCatch(PSV_RELIABILITY_GD_SCALE, error = function(e) NA_real_)
if (is.finite(shipped)) {
  drift <- abs(c_outfield - shipped) / shipped
  if (drift > 0.02) {
    cat(strrep("!", 78), "\n", sep = "")
    cat(sprintf("STALE CONSTANT: shipped PSV_RELIABILITY_GD_SCALE = %.3f, freshly fitted = %.3f (%.1f%% drift).\n",
                shipped, c_outfield, 100 * drift))
    cat("Everything scored through calculate_psv(reliability=) is using the shipped value.\n")
    cat("Edit R/constants.R to the fitted value, then re-run 07c so the live\n")
    cat("PSV constants are rebuilt against the same scale.\n")
    cat(strrep("!", 78), "\n", sep = "")
  } else {
    cat(sprintf("Shipped constant %.3f agrees with the fresh fit (%.1f%% drift) -- no edit needed.\n",
                shipped, 100 * drift))
  }
}

## ---------------------------------------------------------------------------
## 8. Sanity: top-20 outfield / top-5 GK average per-90 under the adopted
##    scale, most recent complete season (season_end_year heuristic mirrors
##    the D1-v2 predictive-lambda gate's choice).
## ---------------------------------------------------------------------------
sey_of <- .season_end_year_col(eval_of)
panel_max_date <- max(eval_of$match_date, na.rm = TRUE)
sey_diag <- eval_of[, .(n = .N, max_date = max(match_date)), by = .(sey = sey_of)][order(sey)]
candidate_sey <- sey_diag[n >= 20000 & max_date <= panel_max_date - 21][order(-sey)][1]$sey
if (length(candidate_sey) == 0 || is.na(candidate_sey)) candidate_sey <- max(sey_diag$sey, na.rm = TRUE) - 1L

season_agg <- function(dt, sey_col, score_col) {
  dt2 <- copy(dt)
  dt2[, sey := sey_col]
  dt2[sey == candidate_sey, .(minutes = sum(total_minutes),
                              mean90 = 90 * sum(get(score_col) * total_minutes / 90) / sum(total_minutes)),
      by = .(player_id, player_name)]
}
of_season <- season_agg(eval_of, sey_of, "score_mom")[minutes >= 900]
setorder(of_season, -mean90)
top20_of <- head(of_season, 20)[, mean90_scaled := mean90 * c_outfield]

sey_gk <- .season_end_year_col(eval_gk)
gk_season <- season_agg(eval_gk, sey_gk, "score_pred")[minutes >= 900]
setorder(gk_season, -mean90)
top5_gk <- head(gk_season, 5)[, mean90_scaled := mean90 * c_outfield]

cat(sprintf("\nTop-20 outfield (season_end_year=%s, minutes>=900), mean of means under PSV_RELIABILITY_GD_SCALE = %.4f\n",
            candidate_sey, mean(top20_of$mean90_scaled)))
print(top20_of[, .(player_name, minutes, mean90, mean90_scaled)], digits = 4)
cat(sprintf("\nTop-5 GK (season_end_year=%s, minutes>=900), mean of means under PSV_RELIABILITY_GD_SCALE = %.4f\n",
            candidate_sey, mean(top5_gk$mean90_scaled)))
print(top5_gk[, .(player_name, minutes, mean90, mean90_scaled)], digits = 4)

cat("\nDONE 07d\n")
