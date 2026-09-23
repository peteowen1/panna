# Is goalkeeping paid in full? Keepers' ledger pay vs goals saved against xGOT
# =============================================================================
# Pete, 2026-09-23: keepers average -0.133 a game; is their goalkeeping value
# too small? The ledger names the keeper for 70% of each on-target shot's
# finish (xGOT -> goal / save), so over a season his shot-row pay should track
# 0.7 x (xGOT faced - goals conceded). This checks that, and shows where the
# other 30% goes.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_keeper_check.R
suppressPackageStartupMessages(library(data.table))
say <- function(...) cat(..., "\n", sep = "")
x <- readRDS("data-raw/cache/epv/net-goals/ng_ledger_ENG_2024-2025.rds")
ep <- as.data.table(x$ep); raw <- as.data.table(x$raw); lu <- as.data.table(x$lineups)
fx <- as.data.table(x$fx)

# the keeper each side fielded (most minutes), and the shots his side faced
gk <- lu[position == "Goalkeeper"][order(match_id, team_id, -as.numeric(minutes_played))][
  , .SD[1], by = .(match_id, team_id)][, .(match_id, def_team = team_id, keeper = player_id,
                                          name = player_name)]
sh <- ep[action_type == "shot" & is.finite(xgot) & xgot > 0]
sh <- merge(sh, fx[, .(match_id, home_team_id, away_team_id)], by = "match_id")
sh[, def_team := fifelse(team_id == home_team_id, away_team_id, home_team_id)]
sh <- merge(sh, gk, by = c("match_id", "def_team"))
sh[, gsax := xgot - (result == "success")]
say("on-target shots faced (not own goals): ", nrow(sh), " | keepers: ", uniqueN(sh$keeper))

pay <- raw[entry == "defence" & play_type == "shot", .(match_id, action_id, player_id, role, value_own)]
kp <- merge(sh[, .(match_id, action_id, keeper, gsax)],
            pay, by = c("match_id", "action_id"), allow.cartesian = TRUE)
kp[, who := fifelse(player_id %in% keeper, "keeper",
             fifelse(role == "defender", "other named (blocker)", "team pool"))]
say("\nWhere the defending side's credit on on-target shots goes (season, goals):")
print(kp[, .(goals = round(sum(value_own), 1)), by = who][order(-abs(goals))])

s <- merge(sh[, .(gsax = sum(gsax), shots = .N), by = .(keeper, name)],
           kp[who == "keeper", .(paid = sum(value_own)), by = keeper], by = "keeper", all.x = TRUE)
s[is.na(paid), paid := 0]
s <- s[shots >= 50]
say("\nkeepers with 50+ on-target shots faced: ", nrow(s))
say("paid / goals-saved slope ", round(coef(lm(paid ~ gsax, s))[2], 3), " (0.70 = paid exactly 70%)",
    "  r = ", round(cor(s$paid, s$gsax), 3))
print(s[order(-gsax)][, .(name, shots, goals_saved_vs_xgot = round(gsax, 2), paid = round(paid, 2))])
