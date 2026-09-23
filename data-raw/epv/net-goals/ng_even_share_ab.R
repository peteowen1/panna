# Even share A/B: does a player keep the same share of his losses as his gains?
# =============================================================================
# 2026-09-23. A failed action's actor keeps exec_blame (0.30) of its loss; every
# successful action keeps 0.90 whatever its sign. np/ng_asymmetry_check.R found
# that excuses attackers' losses (strikers +0.032 goals a game, defenders
# +0.003). One axis: exec_blame 0.30 (shipped) vs 0.90 (even with off_pool
# 0.10). Everything else is current rules, same inputs, one pass.
#
# Anchors set before running:
#   1. Both arms balance: each team lands on its goal difference (asserted by
#      ng_reconcile_margin; residual reported).
#   2. The decision metric is repeatability WITHIN position (year to year, per
#      90, 900+ minutes both seasons): skill persists, noise does not. Position
#      means are reported but are not the target (that would be metric-forcing).
#   3. A gap under ~0.01 in r is not a decision at these pair counts.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_even_share_ab.R
suppressPackageStartupMessages(library(data.table))
NG_INPUTS_ONLY <- TRUE
source("data-raw/epv/net-goals/build_net_goals_artifacts.R")
SEASONS <- c("2022-2023", "2023-2024", "2024-2025")
MIN_MINS <- 900
ARMS <- list(shipped_0.30 = ng_shares(), even_0.90 = ng_shares(exec_blame = 0.90))

inputs <- lapply(SEASONS, function(s) {
  SEASON <<- s
  INPUTS <<- sprintf("data-raw/cache/epv/net-goals/ng_inputs_%s_%s.rds", LEAGUE, s)
  ng_load_inputs()
})
names(inputs) <- SEASONS

per_season <- function(inp, sh) {
  raw <- ng_build_ledger(inp$ep, adj = inp$adj, fixtures = inp$fx, lineups = inp$lineups,
                         shares = sh, verbose = FALSE)
  pay <- ng_spread_pools(raw, inp$ep, inp$lineups, verbose = FALSE)
  pg  <- as.data.table(ng_reconcile_margin(ng_player_game(pay, inp$lineups, verbose = FALSE),
                                           inp$fx, verbose = FALSE))
  mins <- unique(as.data.table(inp$lineups)[, .(match_id, player_id, m = suppressWarnings(as.numeric(minutes_played)))])[m > 0]
  tot <- merge(pg[, .(ng = sum(net_goals)), by = player_id], mins[, .(mins = sum(m), gms = .N), by = player_id], by = "player_id")
  pos <- unique(as.data.table(get_player_positions(inp$lineups, inp$ep))[, .(player_id, pos = position)], by = "player_id")
  merge(tot, pos, by = "player_id")[, p90 := ng / mins * 90][]
}

res <- lapply(names(ARMS), function(a) {
  say("arm ", a); lapply(inputs, per_season, sh = ARMS[[a]])
})
names(res) <- names(ARMS)

rep_r <- function(tots, within) {
  pr <- rbindlist(lapply(1:(length(SEASONS) - 1), function(k) {
    a <- tots[[k]][mins >= MIN_MINS]; b <- tots[[k + 1]][mins >= MIN_MINS]
    if (within) { a[, p90 := p90 - mean(p90), by = pos]; b[, p90 := p90 - mean(p90), by = pos] }
    merge(a[, .(player_id, pos, x = p90)], b[, .(player_id, y = p90)], by = "player_id")
  }))
  list(r = cor(pr$x, pr$y), n = nrow(pr), pr = pr)
}
say("\nYear-to-year repeatability of net goals per 90 (", paste(SEASONS, collapse = ", "),
    "; players with ", MIN_MINS, "+ minutes in both seasons of a pair). Higher is better.")
tab <- rbindlist(lapply(names(res), function(a) {
  o <- rep_r(res[[a]], FALSE); w <- rep_r(res[[a]], TRUE)
  data.table(arm = a, pairs = o$n, r_overall = round(o$r, 4), r_within_position = round(w$r, 4))
}))
print(tab)
# paired bootstrap on the within-position difference (same player pairs in both arms)
set.seed(1)
wa <- rep_r(res[[1]], TRUE)$pr; wb <- rep_r(res[[2]], TRUE)$pr
stopifnot(identical(wa$player_id, wb$player_id))
d <- replicate(2000, { i <- sample(nrow(wa), replace = TRUE); cor(wb$x[i], wb$y[i]) - cor(wa$x[i], wa$y[i]) })
say("within-position r, even minus shipped: ", round(cor(wb$x, wb$y) - cor(wa$x, wa$y), 4),
    "  (95% interval ", round(quantile(d, .025), 4), " to ", round(quantile(d, .975), 4), ", ", nrow(wa), " player pairs)")

say("\nPosition means, ENG 2024-25, net goals per game (players with 10+ games). Higher is better for that position.")
pm <- rbindlist(lapply(names(res), function(a) {
  t <- res[[a]][["2024-2025"]][gms >= 10]
  t[, .(arm = a, players = .N, per_game = round(sum(ng) / sum(gms), 3), sd = round(sd(ng / gms), 3)), by = pos]
}))
print(dcast(pm, pos + players ~ arm, value.var = "per_game"))
say("player spread (sd of net per game): ", paste(pm[, .(s = round(sqrt(mean(sd^2)), 3)), by = arm][, paste(arm, s)], collapse = " | "))
