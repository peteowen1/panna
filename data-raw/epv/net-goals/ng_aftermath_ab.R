# Shot aftermath A/B: price a shot at xG + (1 - xG) * A, not xG (Pete, 2026-09-23)
# =============================================================================
# One axis: ng_build_ledger(shot_aftermath = FALSE vs TRUE), same inputs, one
# pass. Anchors set before running:
#   1. OFF reproduces the pre-change ledger exactly (identity).
#   2. Both balance: every row's two sides still sum to the row (asserted inside
#      the ledger), and each team's raw total stays near its goal difference.
#   3. The won-aerial-then-own-header rows (828) stop being charged the
#      aftermath half of their -0.062 average.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_aftermath_ab.R
suppressPackageStartupMessages(library(data.table))
NG_INPUTS_ONLY <- TRUE
source("data-raw/epv/net-goals/build_net_goals_artifacts.R")
inp <- ng_load_inputs()
say("inputs: ", format(nrow(inp$ep), big.mark = ","), " actions, ", uniqueN(inp$ep$match_id), " matches")

off <- ng_build_ledger(inp$ep, adj = inp$adj, fixtures = inp$fx, lineups = inp$lineups,
                       shot_aftermath = FALSE, verbose = FALSE)
on  <- ng_build_ledger(inp$ep, adj = inp$adj, fixtures = inp$fx, lineups = inp$lineups,
                       shot_aftermath = TRUE, verbose = TRUE)

# 1. identity against the ledger cached before this change
old <- as.data.table(readRDS("data-raw/cache/epv/net-goals/ng_ledger_ENG_2024-2025_pre_aftermath.rds")$raw)
key <- function(p) p[, .(v = sum(value_own)), by = .(match_id, action_id, player_id, team_id, role)][
  order(match_id, action_id, team_id, role, player_id)]
k_old <- key(old); k_off <- key(as.data.table(off))
same <- nrow(k_old) == nrow(k_off) && isTRUE(all.equal(k_old, k_off, tolerance = 1e-12))
say("\n1. OFF vs the pre-change ledger: ", nrow(k_off), " vs ", nrow(k_old), " grouped payments -> ",
    if (same) "IDENTICAL" else "DIFFERENT")
stopifnot(same)

# 2. raw team totals against goal difference (before reconciliation)
gd <- function(p) {
  t <- as.data.table(p)[, .(own = sum(value_own)), by = .(match_id, team_id)]
  f <- inp$fx[, .(match_id, home_team_id, gd_home = as.numeric(home_score) - as.numeric(away_score))]
  t <- merge(t, f, by = "match_id")
  t[, err := own - fifelse(team_id == home_team_id, gd_home, -gd_home)]
  c(median_abs_err = median(abs(t$err)), mean_abs_err = mean(abs(t$err)), team_games = nrow(t))
}
say("\n2. raw team total minus goal difference (goals; lower is better)")
print(round(rbind(off = gd(off), on = gd(on)), 4))

# 3. van Dijk's header against Man City, and the 828 won aerials before an own header
ep <- copy(inp$ep); setorder(ep, match_id, action_id)
ep[, `:=`(nx_type = shift(action_type, -1), nx_pl = shift(player_id, -1)), by = match_id]
hd <- ep[action_type == "aerial" & result %in% "success" & nx_type == "shot" & nx_pl == player_id,
         .(match_id, action_id)]
rowv <- function(p) as.data.table(p)[entry == "offence", .(v = sum(value_own)), by = .(match_id, action_id)]
h <- merge(hd, rowv(off), by = c("match_id", "action_id"))[
  merge(hd, rowv(on), by = c("match_id", "action_id")), on = c("match_id", "action_id")]
say("\n3. won aerial then own header (n = ", nrow(h), "): mean row value (goals, header-taker's side)")
say("   off ", round(mean(h$v), 4), " | on ", round(mean(h$i.v), 4), " | share negative off ",
    round(100 * mean(h$v < 0), 1), "% on ", round(100 * mean(h$i.v < 0), 1), "%")

fx <- inp$fx
m <- fx[grepl("Liverpool", home_team) & grepl("Manchester City", away_team)]$match_id
vv <- ep[match_id %in% m & grepl("van Dijk", player_name) & action_type %in% c("aerial", "shot")][
  , .(match_id, action_id, action_type, result, xg, xgot)]
for (lab in c("off", "on")) {
  p <- as.data.table(get(lab))[match_id %in% m & action_id %in% c(vv$action_id, vv$action_id - 1L)]
  say("\n   ", toupper(lab), ": the cross, van Dijk's aerial and shot, every payment")
  print(p[, .(action_id, play_type, entry, role, who = fifelse(is.na(player_id), "(pool)", player_id),
              value_own = round(value_own, 3))][order(action_id, entry, -abs(value_own))])
}

# 4. players: who moves
pl <- function(p) as.data.table(p)[!is.na(player_id), .(net = sum(value_own)), by = player_id]
pp <- merge(pl(off), pl(on), by = "player_id", suffixes = c("_off", "_on"))
nm <- unique(inp$ep[, .(player_id, player_name)], by = "player_id")
pp <- merge(pp, nm, by = "player_id")[, d := net_on - net_off]
say("\n4. named-player season totals (before pools are shared out): r(off, on) = ",
    round(cor(pp$net_off, pp$net_on), 4), " over ", nrow(pp), " players")
say("   biggest gains:"); print(pp[order(-d)][1:8, .(player_name, off = round(net_off, 2), on = round(net_on, 2), d = round(d, 2))])
say("   biggest falls:"); print(pp[order(d)][1:8, .(player_name, off = round(net_off, 2), on = round(net_on, 2), d = round(d, 2))])
