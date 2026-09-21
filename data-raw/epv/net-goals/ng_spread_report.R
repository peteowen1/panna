# ng_spread_report.R -- team totals, position spreads, and where net goals come
# from by play type.
#
# Debug artifact for the EPV net goals ledger, team convention. Answers:
#   1. does each team's players sum to that team's own goal difference?
#   2. what is the spread (sd) of net goals within each position?
#   3. what does each play type contribute, and how widely does it vary?
#   4. how much of the ledger is paid to a NAMED player rather than spread by
#      proxy across whoever was on the pitch?
#
# Question 4 is the honest one. Torp declined the team convention precisely
# because AFL's defensive half would be almost entirely proxy
# (torpverse/docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md section 4). Football
# can name a defender more often, but not always, and this reports how often.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_spread_report.R

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE <- "ENG"
SEASON <- "2024-2025"

events  <- load_opta_match_events(LEAGUE, season = SEASON, source = "local")
lineups <- load_opta_lineups(LEAGUE, season = SEASON, source = "local")
shot_lk <- panna:::.epv_shot_lookup(LEAGUE, SEASON)
xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")

spadl <- convert_opta_to_spadl(events)
ch  <- create_possession_chains(spadl)
lab <- label_actions_with_outcomes(ch, add_next_chain_outcome(classify_chain_outcomes(ch)))
lab <- create_next_goal_labels(lab)
if (epv_model$method == "xg") lab <- create_next_xg_labels(lab)
ep  <- calculate_action_epv(lab, create_epv_features(lab, n_prev = 3), epv_model,
                            xg_model = xg_model, league = LEAGUE, season = SEASON,
                            shot_lookup = shot_lk)
ep  <- as.data.table(add_xpass_to_spadl(ep, xpass_model))

fx  <- as.data.table(load_opta_fixtures(LEAGUE, season = SEASON, source = "local"))[
  , .(match_id, home_team_id, away_team_id, home_score, away_score)]
adj <- ng_build_adjacency(events, verbose = FALSE)
lu  <- as.data.table(lineups)

raw <- ng_build_ledger(ep, adj = adj, fixtures = fx, convention = "team",
                       verbose = FALSE)
p   <- ng_spread_pools(raw, ep, lu, verbose = FALSE)

# ---- 1. TEAM TOTALS --------------------------------------------------------
cat("\n==== 1. DOES EACH TEAM SUM TO ITS OWN GOAL DIFFERENCE? ====\n")
cat("Winners of a 3-1 should sum to +2, losers to -2, and the match to zero.\n")
cat("cor and slope nearer 1.000 are better; lower error is better.\n\n")
tt <- ng_check_team_totals(p, fx, verbose = FALSE)
cat(sprintf("team-matches            : %d\n", nrow(tt)))
cat(sprintf("cor(total, own GD)      : %.4f\n", cor(tt$own_total, tt$own_gd)))
cat(sprintf("slope                   : %.4f\n", coef(lm(own_gd ~ own_total, tt))[2]))
cat(sprintf("median |error|          : %.3f goals\n", median(abs(tt$err))))
cat(sprintf("max |error|             : %.3f goals\n", max(abs(tt$err))))
z <- tt[, .(s = sum(own_total)), by = match_id]
cat(sprintf("the two sides cancel to : %.2e\n", max(abs(z$s))))

cat("\nfive example matches (own_total is that team's players summed):\n")
ex <- tt[order(-abs(own_gd))][1:10, .(match_id = substr(match_id, 1, 8),
                                      own_gd, own_total = round(own_total, 3))]
print(ex, row.names = FALSE)

# ---- 2. POSITION SPREAD ----------------------------------------------------
posmap <- as.data.table(get_player_positions(lu, ep))
mins <- unique(lu[, .(match_id, player_id, m = as.numeric(minutes_played))])[m > 0]
pg <- p[, .(ng = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id)]
pg <- merge(merge(pg, mins, by = c("match_id", "player_id")), posmap, by = "player_id")
pg60 <- pg[m >= 60]

cat("\n==== 2. NET GOALS BY POSITION: LEVEL AND SPREAD ====\n")
cat("One row per player-game, minimum 60 minutes so short stints do not distort\n")
cat("the sd. `mean`/`sd` are per player-game in goals; `per90` is minutes-weighted.\n")
cat("A larger sd means the metric separates players within that position more.\n\n")
a <- pg60[, .(player_games = .N,
              mean = round(mean(ng), 4), sd = round(sd(ng), 4),
              p5 = round(quantile(ng, .05), 3), p95 = round(quantile(ng, .95), 3),
              per90 = round(sum(ng) / sum(m) * 90, 4)), by = position][order(-mean)]
print(a, row.names = FALSE)
cat(sprintf("\nall positions pooled: mean %.4f, sd %.4f (n = %d player-games)\n",
            mean(pg60$ng), sd(pg60$ng), nrow(pg60)))

cat("\n==== 2b. OFFENCE / DEFENCE SPLIT BY POSITION (per 90) ====\n")
cat("Oliver publishes these separately (Jokic +365 offence, +61 defence).\n\n")
e <- merge(p[!is.na(entry), .(v = sum(value_own, na.rm = TRUE)),
              by = .(match_id, player_id, entry)],
           pg[, .(match_id, player_id, position, m)], by = c("match_id", "player_id"))
pm <- pg[, .(tot_m = sum(m)), by = position]
e <- merge(e[, .(v = sum(v)), by = .(position, entry)], pm, by = "position")
e[, p90 := round(v / tot_m * 90, 4)]
w <- dcast(e, position ~ entry, value.var = "p90", fill = 0)
w[, total := round(offence + defence, 4)]
print(w[order(-total)], row.names = FALSE)

# ---- 3. PLAY TYPE ----------------------------------------------------------
cat("\n==== 3. NET GOALS BY PLAY TYPE ====\n")
cat("`total` is the season sum in goals across all players (positive = that play\n")
cat("type added value on net). `sd` is per player-game. `share_abs` is the play\n")
cat("type's share of all absolute value in the ledger -- where the action is.\n\n")
pt <- p[play_type != "pool"]
tot_abs <- sum(abs(pt$value_own), na.rm = TRUE)
ptpg <- pt[, .(v = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, play_type)]
b <- merge(pt[, .(n = .N, total = sum(value_own, na.rm = TRUE),
                  abs_v = sum(abs(value_own), na.rm = TRUE)), by = play_type],
           ptpg[, .(sd = sd(v), p95 = quantile(v, .95)), by = play_type],
           by = "play_type")
b[, `:=`(total = round(total, 1), sd = round(sd, 4), p95 = round(p95, 3),
         share_abs = round(100 * abs_v / tot_abs, 1))]
print(b[order(-share_abs), .(play_type, n, total, sd, p95, share_abs)], row.names = FALSE)

cat("\nteam pools (no single play type; spread across the eleven on the pitch):\n")
pl <- p[play_type == "pool", .(n = .N, total = round(sum(value_own, na.rm = TRUE), 1)),
        by = role]
print(pl, row.names = FALSE)

# ---- 4. NAMED vs PROXY -----------------------------------------------------
cat("\n==== 4. HOW MUCH IS NAMED, AND HOW MUCH IS PROXY? ====\n")
cat("A named payment goes to the player the feed identified. A proxy payment is\n")
cat("spread across whoever was on the pitch, because nobody was named. Torp\n")
cat("declined this convention because AFL's defensive half would be ~all proxy.\n\n")
p[, kind := fifelse(play_type == "pool", "proxy (pool)", "named")]
k <- p[, .(abs_v = sum(abs(value_own), na.rm = TRUE)), by = .(entry, kind)]
k[, pct := round(100 * abs_v / sum(abs_v), 1)]
print(k[order(entry, -abs_v), .(entry, kind, abs_value = round(abs_v, 1), pct)],
      row.names = FALSE)
byentry <- p[, .(abs_v = sum(abs(value_own), na.rm = TRUE)), by = .(entry, kind)]
byentry[, pct_within := round(100 * abs_v / sum(abs_v), 1), by = entry]
cat("\nwithin each half:\n")
print(byentry[order(entry, -abs_v), .(entry, kind, pct_within)], row.names = FALSE)
