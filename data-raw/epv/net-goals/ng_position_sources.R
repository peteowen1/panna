# ng_position_sources.R -- where each position's net goals actually come from,
# and why the goalkeeper is the outlier.
#
# Debug artifact for the EPV net goals ledger, team convention. The share sweep
# says outfield positions sit close together once the defensive pool's credit
# half is routed by defensive acts, and the keeper does not. This decomposes
# every position into the payments that built it so the keeper's number can be
# read rather than guessed at.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_position_sources.R

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE <- "ENG"
SEASON <- "2024-2025"
DIALS  <- c(0, 1)   # dacts_share settings to compare

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
base <- ng_build_ledger(ep, adj = adj, fixtures = fx, convention = "team",
                        verbose = FALSE)

posmap <- as.data.table(get_player_positions(lu, ep))
mins <- unique(lu[, .(match_id, player_id, m = as.numeric(minutes_played))])[m > 0]
pm <- merge(mins, posmap, by = "player_id")
tot_m <- pm[, .(tot_m = sum(m)), by = position]

by_role <- function(p) {
  x <- merge(p[, .(v = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, role)],
             pm[, .(match_id, player_id, position)], by = c("match_id", "player_id"))
  x <- merge(x[, .(v = sum(v)), by = .(position, role)], tot_m, by = "position")
  x[, p90 := round(v / tot_m * 90, 4)][]
}

for (ds in DIALS) {
  p <- ng_spread_pools(base, ep, lu, dacts_share = ds, verbose = FALSE)
  cat("\n\n==== NET GOALS PER 90 BY POSITION AND ROLE (dacts_share =", ds, ") ====\n")
  cat("Positive = that role added value for him. Each row sums to `total`.\n\n")
  w <- dcast(by_role(p), position ~ role, value.var = "p90", fill = 0)
  num <- setdiff(names(w), "position")
  w[, total := round(rowSums(.SD), 4), .SDcols = num]
  print(w[order(-total)], row.names = FALSE)
  assign(paste0("w", ds * 100), w)
}

if (all(c(0, 1) %in% DIALS)) {
  cat("\n\n==== WHAT THE DIAL MOVES (dacts_share 0 -> 1, goals per 90) ====\n")
  cat("Only the defensive pool's credit half is routed, so everything else is\n")
  cat("unchanged by construction. Positive = that position gained.\n\n")
  num <- intersect(names(w0), names(w100))
  num <- setdiff(num, "position")
  d <- merge(w0[, c("position", num), with = FALSE],
             w100[, c("position", num), with = FALSE], by = "position",
             suffixes = c("_flat", "_dacts"))
  for (nm in num) d[[nm]] <- round(d[[paste0(nm, "_dacts")]] - d[[paste0(nm, "_flat")]], 4)
  print(d[, c("position", num), with = FALSE][order(-total)], row.names = FALSE)
}

# ---- GOALKEEPER DEEP DIVE ---------------------------------------------------
cat("\n\n==== GOALKEEPER: WHERE THE NUMBER COMES FROM ====\n")
p1 <- ng_spread_pools(base, ep, lu, dacts_share = 1, verbose = FALSE)
gk_ids <- posmap[position == "Goalkeeper"]$player_id
gk_m <- tot_m[position == "Goalkeeper"]$tot_m

g <- p1[player_id %chin% gk_ids]
cat("\nby role and entry (goals per 90 across all keeper minutes):\n")
tab <- g[, .(n = .N, total = sum(value_own, na.rm = TRUE)), by = .(entry, role)]
tab[, per90 := round(total / gk_m * 90, 4)][, total := round(total, 1)]
print(tab[order(-abs(per90))], row.names = FALSE)

cat("\nby play type, named payments only (pool rows have no play type):\n")
tab2 <- g[play_type != "pool", .(n = .N, total = round(sum(value_own, na.rm = TRUE), 1)),
          by = play_type]
tab2[, per90 := round(total / gk_m * 90, 4)]
print(tab2[order(-abs(per90))], row.names = FALSE)

cat("\n==== WHAT COUNTS AS A KEEPER'S 'DEFENSIVE ACT' ====\n")
cat("The dial routes on counts, so this is what it is actually routing on.\n")
cat("A routine catch weighs the same as a goal-line block.\n\n")
da <- ep[player_id %chin% gk_ids & action_type %chin% NG_DEFENSIVE_ACTIONS &
           result %in% "success", .N, by = action_type][order(-N)]
da[, pct := round(100 * N / sum(N), 1)]
da[, per90 := round(N / gk_m * 90, 2)]
print(da, row.names = FALSE)
cat(sprintf("\nkeeper defensive acts per 90: %.2f\n", sum(da$per90)))

dao <- ep[!player_id %chin% gk_ids & action_type %chin% NG_DEFENSIVE_ACTIONS &
            result %in% "success", .N, by = action_type][order(-N)]
cat("\nfor comparison, the same counts for outfielders:\n")
dao[, pct := round(100 * N / sum(N), 1)]
print(dao, row.names = FALSE)

cat("\n==== IF ROUTINE KEEPER HANDLING DID NOT COUNT ====\n")
cat("Dropping keeper_pick_up and keeper_claim from the weighting.\n\n")
ROUTINE <- c("keeper_pick_up", "keeper_claim")
kept <- sum(da[!action_type %chin% ROUTINE]$per90)
cat(sprintf("keeper defensive acts per 90: %.2f -> %.2f\n", sum(da$per90), kept))
outf <- ep[!player_id %chin% gk_ids & action_type %chin% NG_DEFENSIVE_ACTIONS &
             result %in% "success"]
outf_m <- sum(tot_m[position != "Goalkeeper"]$tot_m)
cat(sprintf("outfield defensive acts per 90 (unchanged): %.2f\n",
            nrow(outf) / outf_m * 90))
