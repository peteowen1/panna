# Is the pubv0 repeatability gain player skill or team? (2026-09-24)
# Players mostly stay at one club, so a rating can look repeatable by tracking
# the team. Split the within-position pairs into players who changed club
# between the two seasons (movers) and those who stayed. A real player-level
# gain shows up in both. Run after ng_epv_pack_ab.R's arms (it re-runs them).
source("data-raw/epv/net-goals/ng_epv_pack_ab.R")
club <- rbindlist(lapply(SEASONS, function(s) {
  lu <- as.data.table(inputs_by_epv[[1]][[s]]$lineups)[suppressWarnings(as.numeric(minutes_played)) > 0]
  lu[, .N, by = .(player_id, team_id)][order(-N)][, .SD[1], by = player_id][, .(player_id, team_id, season = s)]
}))
out <- rbindlist(lapply(names(res), function(a) {
  rbindlist(lapply(1:(length(SEASONS) - 1), function(k) {
    x <- res[[a]][[k]][mins >= MIN_MINS]; y <- res[[a]][[k + 1]][mins >= MIN_MINS]
    x[, p90 := p90 - mean(p90), by = pos]; y[, p90 := p90 - mean(p90), by = pos]
    m <- merge(x[, .(player_id, x = p90)], y[, .(player_id, y = p90)], by = "player_id")
    m <- merge(m, club[season == SEASONS[k], .(player_id, t1 = team_id)], by = "player_id")
    m <- merge(m, club[season == SEASONS[k + 1], .(player_id, t2 = team_id)], by = "player_id")
    m[, arm := a]
  }))
}))
out[, group := fifelse(t1 == t2, "stayed", "changed club")]
cat("\nWithin-position year-to-year r, by whether the player changed club (higher is better)\n")
print(dcast(out[, .(pairs = .N, r = round(cor(x, y), 3)), by = .(group, arm)], group ~ arm, value.var = "r"))
print(out[arm == names(res)[1], .N, by = group])
