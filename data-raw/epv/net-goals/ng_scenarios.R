# Net goals edge cases, checked on a real season
# =============================================================================
# The unit tests prove the rules on small fixtures; this checks the awkward
# real cases on ENG 2024-25 (the cached ledger from build_net_goals_artifacts.R)
# and reports each with how many rows it covered, so a check that finds nothing
# to test fails instead of passing vacuously.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_scenarios.R
suppressPackageStartupMessages(library(data.table))
devtools::load_all(quiet = TRUE)
x <- readRDS("data-raw/cache/epv/net-goals/ng_ledger_ENG_2024-2025.rds")
ep <- as.data.table(x$ep); raw <- as.data.table(x$raw); pay <- as.data.table(x$pay)
lu <- as.data.table(x$lineups); fx <- as.data.table(x$fx)
res <- list()
chk <- function(name, n, ok, detail = "") {
  res[[length(res) + 1]] <<- data.table(check = name, n = n,
                                        result = if (n == 0) "NO DATA" else if (isTRUE(ok)) "pass" else "FAIL",
                                        detail = detail)
}
side <- function(p, e) p[entry == e, .(v = sum(value_own)), by = .(match_id, action_id)]

# 1. Every action books its value on both sides, opposite signs.
o <- side(raw, "offence"); d <- side(raw, "defence")
od <- merge(o, d, by = c("match_id", "action_id"))
chk("double entry on every action", nrow(od), max(abs(od$v.x + od$v.y)) < 1e-9,
    sprintf("worst %.1e", max(abs(od$v.x + od$v.y))))

# 2. Goals. A goal's two sides book 1 - xG exactly.
g <- ep[action_type == "shot" & result == "success" & is.finite(xgot)]
gg <- merge(g[, .(match_id, action_id, want = 1 - epv)], o, by = c("match_id", "action_id"))
chk("goals book 1 - xG", nrow(gg), max(abs(gg$v - gg$want)) < 1e-9)

# 3. Penalties: on-target penalties are split, and a saved one names the stopper.
pk <- ep[action_type == "shot" & is.finite(xgot) & epv > 0.6]
pkk <- raw[action_id %in% pk$action_id & match_id %in% pk$match_id & role == "defender"]
chk("high-xG shots (penalties) get the split", nrow(pk),
    nrow(pk) > 0 && all(pk[result != "success" & xgot > 0, paste(match_id, action_id)] %in%
                          raw[role == "defender", paste(match_id, action_id)]),
    sprintf("%d shots with xG > 0.6, %d saved on target", nrow(pk), pk[result != "success" & xgot > 0, .N]))

# 4. Own goals keep the old single-step rule and still conserve.
og <- ep[action_type == "shot" & result == "success" & !is.finite(xgot)]
ogg <- od[paste(match_id, action_id) %in% og[, paste(match_id, action_id)]]
chk("own goals conserve", nrow(ogg), max(abs(ogg$v.x + ogg$v.y)) < 1e-9)

# 5. Keepers: never in the defensive pool; named on on-target shots.
gk <- unique(lu[position == "Goalkeeper", .(match_id, player_id)])
# (The keeper's own side's REBOUND pool -- role pool_def, entry offence, after
# his save spills -- is his team's blame and he shares it; only the DEFENSIVE
# entries are outfield work.)
kp <- merge(pay[role == "pool_def_spread" & entry == "defence"], gk, by = c("match_id", "player_id"))
chk("keepers get no defensive pool share", nrow(gk), sum(abs(kp$value_own)) < 1e-9,
    sprintf("%.2g goals on %d rows", sum(abs(kp$value_own)), nrow(kp)))
ot <- ep[action_type == "shot" & xgot > 0]
named <- raw[role == "defender" & paste(match_id, action_id) %in% ot[, paste(match_id, action_id)]]
chk("a keeper or stopper is named on on-target shots", nrow(ot),
    uniqueN(named[, paste(match_id, action_id)]) / nrow(ot) > 0.95,
    sprintf("%.1f%% named", 100 * uniqueN(named[, paste(match_id, action_id)]) / nrow(ot)))

# 6. A keeper substituted off is not named for goals after he left.
subs <- lu[position == "Goalkeeper" & is.finite(as.numeric(sub_off_minute)) & as.numeric(sub_off_minute) > 0]
late <- merge(ep[action_type == "shot" & result == "success", .(match_id, action_id, min = time_seconds / 60, team_id)],
              subs[, .(match_id, gk = player_id, gk_team = team_id, off = as.numeric(sub_off_minute))],
              by = "match_id", allow.cartesian = TRUE)[team_id != gk_team & min > off]
bad <- raw[paste(match_id, action_id, player_id) %in% late[, paste(match_id, action_id, gk)]]
chk("subbed-off keeper not blamed after leaving", nrow(late), nrow(bad) == 0,
    sprintf("%d goals after a keeper sub", nrow(late)))

# 7. Red cards: a side of 10 still spreads its pools (the 10-11 guard) and the
#    team still lands on its goal difference.
pg <- as.data.table(ng_reconcile_margin(ng_player_game(pay, lu, verbose = FALSE), fx, verbose = FALSE))
tt <- merge(pg[, .(got = sum(net_goals), n = .N), by = .(match_id, team_id)],
            fx[, .(match_id, home_team_id, gd = as.numeric(home_score) - as.numeric(away_score))], by = "match_id")
tt[, want := fifelse(team_id == home_team_id, gd, -gd)]
chk("every team lands on its goal difference", nrow(tt), max(abs(tt$got - tt$want)) < 1e-9)
chk("goalless draws sum to 0 per side", tt[want == 0 & gd == 0, .N],
    tt[gd == 0, max(abs(got))] < 1e-9)

# 8. Plausibility: no single payment beyond a goal, no player-match beyond 3.
# An own goal books -1 - (value before), so it can exceed a goal by that value.
lim <- merge(raw, ep[, .(match_id, action_id, epv)], by = c("match_id", "action_id"), all.x = TRUE)
chk("no single payment beyond 1 goal + its starting value", nrow(lim),
    all(abs(lim$value_own) <= 1 + abs(fifelse(is.na(lim$epv), 0, lim$epv)) + 1e-9),
    sprintf("max %.3f", max(abs(raw$value_own))))
chk("no player-match beyond +/-4 goals (a hat-trick reaches ~3)", nrow(pg), max(abs(pg$net_goals)) < 4,
    sprintf("range %.2f to %.2f", min(pg$net_goals), max(pg$net_goals)))

# 9. Shot chain: the row after a shot starts from 0 in the ledger.
nx <- ep[order(match_id, action_id)][, prev := shift(action_type), by = match_id][prev == "shot" & action_type != "shot"]
nxl <- merge(nx[, .(match_id, action_id, want = epv + epv_delta)], o, by = c("match_id", "action_id"))
chk("row after a shot books from 0", nrow(nxl), max(abs(nxl$v - nxl$want)) < 1e-9)

out <- rbindlist(res)
print(out, row.names = FALSE)
cat("\n", sum(out$result == "pass"), "of", nrow(out), "passed;", sum(out$result == "FAIL"), "failed;",
    sum(out$result == "NO DATA"), "had nothing to test\n")
