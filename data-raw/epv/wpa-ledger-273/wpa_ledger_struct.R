# Liverpool's 9 EPL 2026-27 matches through the WPA ledger, with the home-frame structural WP model
# (no possession premium; threat as a next-goal mixture). Compare with the published-model ledger
# and with EPV net goals on the same matches.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table); library(xgboost)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
L <- readRDS(file.path(out, "wpa_ledger_liv_d0.rds")); w <- copy(L$w); sp <- copy(L$spadl_ng); lu <- L$lineups; mr <- L$mr
H <- readRDS(file.path(out, "wp_home_frame.rds"))
liv <- "c8h9bw1l82s06h77xxrelzhur"
fh <- c("time_remaining", "m_home", "red_home", "is_second_half", "is_extra_time")
w[, `:=`(m_home = score_diff, red_home = red_card_diff)]   # both home frame (checked)
Pposs <- function(d, add) { x <- copy(d); x[, m_home := m_home + add * fifelse(is_home == 1, 1, -1)]
  h <- predict(H, as.matrix(x[, ..fh])); fifelse(d$is_home == 1, h, 1 - h) }
p_m <- Pposs(w, 0); p_up <- Pposs(w, 1); p_dn <- Pposs(w, -1); e <- w$epv
w[, wp_s := ifelse(e >= 0, (1 - e) * p_m + e * p_up, (1 + e) * p_m - e * p_dn)]
setorder(w, match_id, period_id, time_seconds)
w[, `:=`(wn = shift(wp_s, type = "lead"), tn2 = shift(team_id, type = "lead")), by = match_id]
w[is.na(wn), `:=`(wn = wp_label, tn2 = team_id)]
w[, wpa_s := fifelse(tn2 == team_id, wn - wp_s, (1 - wn) - wp_s)]
sp[w, epv_delta := i.wpa_s, on = c("match_id", "action_id")]
stopifnot(sum(is.na(sp$epv_delta)) == 0)
ev <- load_opta_match_events("ENG", season = "2026-2027"); ev <- ev[ev$match_id %in% mr$match_id, ]
fx <- as.data.frame(load_opta_fixtures("ENG", season = "2026-2027", source = "local")); fx <- fx[fx$match_id %in% mr$match_id, ]
pay <- ng_build_ledger(sp, adj = ng_build_adjacency(ev, verbose = FALSE), fixtures = fx, lineups = lu,
                       shot_chain = FALSE, shot_aftermath = FALSE, verbose = FALSE)
pay <- ng_spread_pools(pay, sp, lu, dacts_share = 0, verbose = FALSE)
pg <- as.data.table(ng_player_game(pay, lu, verbose = FALSE))
# 50/50 start, spread by minutes (opening win chance minus 0.5)
op <- w[, .(open_home = fifelse(is_home[1] == 1, wp_s[1], 1 - wp_s[1])), by = match_id]
mr[op, open_home := i.open_home, on = "match_id"]
k <- rbind(mr[, .(match_id, team_id = home_team_id, kick = open_home - 0.5)], mr[, .(match_id, team_id = away_team_id, kick = 0.5 - open_home)])
pg[, mins := pmax(minutes_played, 0, na.rm = TRUE)][k, kick := i.kick, on = c("match_id", "team_id")]
pg[, wpa_struct := net_goals + kick * mins / sum(mins), by = .(match_id, team_id)]
tt <- pg[, .(s = sum(wpa_struct)), by = .(match_id, team_id)]
mr[, res_liv := fifelse(home_team_id == liv, res_home, 1 - res_home)]
chk <- merge(tt[team_id == liv], mr[, .(match_id, target = res_liv - 0.5)], by = "match_id")
cat("max |Liverpool team total - target| over 9 matches:", signif(max(abs(chk$s - chk$target)), 3), "\n")

old <- readRDS(file.path(out, "wpa_ledger_pg_d0.rds"))
se <- copy(L$spadl_ng); se[, epv_delta := epv_delta_epv][, xgot := xgot_kept]
pe <- ng_spread_pools(ng_build_ledger(se, adj = ng_build_adjacency(ev, verbose = FALSE), fixtures = fx, lineups = lu, verbose = FALSE), se, lu, verbose = FALSE)
pge <- as.data.table(ng_player_game(pe, lu, verbose = FALSE))
s <- pg[team_id == liv, .(mins = sum(mins), wpa_structural = sum(wpa_struct)), by = player_name]
s[old[team_id == liv, .(v = sum(wpa_neutral)), by = player_name], wpa_published := i.v, on = "player_name"]
s[pge[team_id == liv, .(v = sum(net_goals)), by = player_name], epv_net_goals := i.v, on = "player_name"]
s <- s[mins >= 300]; num <- c("epv_net_goals", "wpa_published", "wpa_structural"); s[, (num) := lapply(.SD, round, 2), .SDcols = num]
cat("\n=== Liverpool players, 9 matches (>= 300 mins): EPV net goals vs WPA ledger on each WP model ===\n")
print(s[order(-epv_net_goals), c("player_name", "mins", num), with = FALSE])
cat(sprintf("Spearman vs EPV net goals: published-model WPA %.2f, structural WPA %.2f (n = %d)\n",
            cor(s$epv_net_goals, s$wpa_published, method = "spearman"), cor(s$epv_net_goals, s$wpa_structural, method = "spearman"), nrow(s)))
f <- mr[away_team_id %in% unique(as.data.table(lu)[grepl("Fulham", team_name), team_id]), match_id]
fp <- pg[match_id == f & mins >= 45]; fp[as.data.table(lu)[, .(player_id, team_name)], team := i.team_name, on = "player_id", mult = "first"]
cat("\nLiverpool 0-0 Fulham, structural ledger (each team sums to 0):\n")
print(fp[order(team, -wpa_struct), .(team, player_name, mins, wpa_struct = round(wpa_struct, 3))])
saveRDS(pg, file.path(out, "wpa_ledger_pg_struct.rds"))
