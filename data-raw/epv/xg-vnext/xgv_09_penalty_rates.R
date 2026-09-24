# Penalty conversion by league-season, from EARLIER seasons only (Pete, 2026-09-24).
# Replaces the hand-set PENALTY_XG = 0.80.
# =============================================================================
# For league L in season y: each earlier season's penalties weighted DECAY^(y - s - 1)
# (older seasons count less), shrunk toward the all-league rate of those same
# earlier seasons by empirical Bayes (beta-binomial, method of moments, re-fitted
# per season from earlier data only). In-match penalties only (periods 1-4):
# shootouts, own goals and goal-only feeds out. A league with no earlier penalties
# gets the all-league rate. One extra row per league for the NEXT season is what
# production reads. Leak test: truncate at a season, recompute, earlier rows identical.
#
# Output: data-raw/cache/epv/xg-vnext/penalty_rates.csv
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_09_penalty_rates.R
suppressMessages({library(data.table); library(arrow)})
X <- "data-raw/cache/epv/xg-vnext"; DECAY <- 0.8
d <- as.data.table(read_parquet(file.path(X, "shot_features.parquet"),
                                col_select = c("competition", "season", "season_num", "is_penalty", "period_id", "goal")))
cov <- d[, .(nongoal = sum(goal == 0)), by = .(competition, season)]
d <- d[!cov[nongoal == 0], on = .(competition, season)]
pk <- d[is_penalty == 1 & period_id %in% 1:4 & !is.na(season_num)]
ls <- pk[, .(pens = .N, goals = sum(goal)), by = .(competition, yr = season_num)]
cat("in-match penalties:", nrow(pk), " scored:", sum(pk$goal), " rate:", round(mean(pk$goal), 5),
    "| league-seasons:", nrow(ls), "\n")

rates_for <- function(ls) {
  yrs <- sort(unique(c(ls$yr, max(ls$yr) + 1L)))
  rbindlist(lapply(yrs, function(y) {
    past <- ls[yr < y]; if (!nrow(past)) return(NULL)
    past[, w := DECAY^(y - yr - 1)]
    agg <- past[, .(g = sum(w * goals), n = sum(w * pens)), by = competition]
    p0 <- sum(agg$g) / sum(agg$n); r <- agg$g / agg$n
    v_obs <- sum(agg$n * (r - p0)^2) / sum(agg$n); v_bin <- p0 * (1 - p0) * nrow(agg) / sum(agg$n)
    k <- if (v_obs > v_bin) p0 * (1 - p0) / (v_obs - v_bin) - 1 else 1e6
    out <- CJ(competition = unique(ls$competition), yr = y)
    out[agg, on = "competition", `:=`(g = i.g, n = i.n)]
    out[, `:=`(p0 = p0, k = k, rate = (fcoalesce(g, 0) + k * p0) / (fcoalesce(n, 0) + k))]
    out[, .(competition, season_num = yr, penalty_xg = rate, prior = p0, prior_strength = k, weighted_pens = fcoalesce(n, 0))]
  }))
}
tab <- rates_for(ls)
# leak test: drop everything from 2020 on, recompute; rows for seasons <= 2020 must not move
tab2 <- rates_for(ls[yr < 2020])
chk <- merge(tab[season_num <= 2020], tab2, by = c("competition", "season_num"))
gap <- max(abs(chk$penalty_xg.x - chk$penalty_xg.y))
cat("LEAK TEST: rows for seasons <= 2020 moved by at most", signif(gap, 3), "over", nrow(chk), "rows\n")
stopifnot(nrow(chk) > 0, gap < 1e-12)
fwrite(tab, file.path(X, "penalty_rates.csv"))
nx <- tab[season_num == max(season_num)]
cat("\nproduction rows (season", max(tab$season_num), "): prior", round(nx$prior[1], 4), " prior strength", round(nx$prior_strength[1]),
    "penalties | range", paste(round(range(nx$penalty_xg), 4), collapse = " to "), "\n")
print(nx[order(-weighted_pens)][1:15, .(competition, penalty_xg = round(penalty_xg, 4), weighted_pens = round(weighted_pens))])
# does it predict? log-loss of in-match penalties, flat 0.80 vs pooled rate vs these rates (as-of, leak-free)
ev <- merge(pk, tab, by.x = c("competition", "season_num"), by.y = c("competition", "season_num"))
ll <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }
cat("\nlog-loss on", nrow(ev), "penalties (lower is better): flat 0.80", round(ll(ev$goal, 0.80), 5),
    "| earlier-seasons pooled", round(ll(ev$goal, ev$prior), 5), "| shrunk league rate", round(ll(ev$goal, ev$penalty_xg), 5), "\n")
