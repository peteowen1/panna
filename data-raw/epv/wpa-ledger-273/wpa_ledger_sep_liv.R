# panna#273: Liverpool's EPL 2026-27 matches through wpa_build_ledger() with the SEP structural WP
# model (scoreboard + pre-match Elo + who has the ball; Pete chose SEP 2026-11-10), in both site views:
#   agnostic   = Elo gap set to 0 (every match starts level), team sums to result - 0.5
#   team_value = real Elo gap, team sums to result - 0.5; wpa_start = the player's share of the team's
#                pre-match edge, so team_value - wpa_start sums to result - pre-match chance.
# Anchors: EPV net goals and the published-model WPA ledger on the same matches.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
L <- readRDS(file.path(out, "wpa_ledger_liv_d0.rds")); w <- as.data.table(copy(L$w)); sp <- copy(L$spadl_ng)
lu <- L$lineups; mr <- as.data.table(L$mr)
sep <- readRDS(file.path(out, "wp_structural_gate_models.rds"))$SEP
stopifnot(isTRUE(sep$use_possession))
elo <- as.data.table(arrow::read_parquet(file.path(out, "match_elo_all.parquet")))
liv <- "c8h9bw1l82s06h77xxrelzhur"

w <- add_wp_match_elo(w, elo, min_coverage = 0.8)
keep_m <- unique(w$match_id)
sp <- sp[sp$match_id %in% keep_m, ]
fx <- as.data.table(load_opta_fixtures("ENG", season = "2026-2027", source = "local"))[match_id %in% keep_m]
ev <- load_opta_match_events("ENG", season = "2026-2027"); ev <- ev[ev$match_id %in% keep_m, ]
adj <- ng_build_adjacency(ev, verbose = FALSE)
cat(sprintf("matches: %d (Liverpool's ledger file has %d)\n", length(keep_m), uniqueN(L$w$match_id)))
print(unique(elo[match_id %in% keep_m, .(match_id, match_date, elo_diff = round(elo_diff))])[order(match_date)])

run <- function(agnostic) {
  wa <- add_wp_vars(copy(w), sep, centre = FALSE, team_agnostic = agnostic)
  wpa_build_ledger(sp, wa[, .(match_id, action_id, team_id, wp, wpa)], fixtures = fx, lineups = lu,
                   adj = adj, verbose = TRUE)
}
ag <- as.data.table(run(TRUE)); tv <- as.data.table(run(FALSE))

# Anchors (same construction as wpa_ledger_struct.R)
old <- readRDS(file.path(out, "wpa_ledger_pg_d0.rds"))
se <- copy(L$spadl_ng)[match_id %in% keep_m]; se[, epv_delta := epv_delta_epv][, xgot := xgot_kept]
pe <- ng_spread_pools(ng_build_ledger(se, adj = adj, fixtures = as.data.frame(fx), lineups = lu, verbose = FALSE), se, lu, verbose = FALSE)
pge <- as.data.table(ng_player_game(pe, lu, verbose = FALSE))

s <- tv[team_id == liv, .(mins = sum(pmax(minutes_played, 0, na.rm = TRUE)), team_value = sum(wpa_neutral),
                          of_which_edge = sum(wpa_start)), by = player_name]
s[ag[team_id == liv, .(v = sum(wpa_neutral)), by = player_name], agnostic := i.v, on = "player_name"]
s[old[team_id == liv & match_id %in% keep_m, .(v = sum(wpa_neutral)), by = player_name], published := i.v, on = "player_name"]
s[pge[team_id == liv, .(v = sum(net_goals)), by = player_name], epv_net_goals := i.v, on = "player_name"]
s <- s[mins >= 300]
num <- c("epv_net_goals", "published", "agnostic", "team_value", "of_which_edge")
s[, (num) := lapply(.SD, round, 2), .SDcols = num]
cat("\n=== Liverpool players (>= 300 mins), sum over the matches above ===\n")
print(s[order(-agnostic), c("player_name", "mins", num), with = FALSE])
cat(sprintf("\nSpearman vs EPV net goals (n = %d): published %.2f, SEP agnostic %.2f, SEP team-value %.2f\n", nrow(s),
            cor(s$epv_net_goals, s$published, method = "spearman", use = "complete.obs"),
            cor(s$epv_net_goals, s$agnostic, method = "spearman"), cor(s$epv_net_goals, s$team_value, method = "spearman")))
tm <- merge(ag[team_id == liv, .(agnostic = sum(wpa_neutral)), by = match_id],
            tv[team_id == liv, .(team_value = sum(wpa_neutral), edge = sum(wpa_start)), by = match_id], by = "match_id")
tm[fx, `:=`(date = substr(as.character(i.match_date), 1, 10), home = i.home_team_id == liv, score = paste(i.home_score, i.away_score, sep = "-")), on = "match_id"]
cat("\nLiverpool team totals per match (both views = result - 0.5; edge = Liverpool's pre-match edge):\n")
print(tm[order(date), .(date, home, score, agnostic = round(agnostic, 3), team_value = round(team_value, 3), edge = round(edge, 3))])
saveRDS(list(agnostic = ag, team_value = tv, summary = s), file.path(out, "wpa_ledger_sep_liv.rds"))
