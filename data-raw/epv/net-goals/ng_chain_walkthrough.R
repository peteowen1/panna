# ng_chain_walkthrough.R -- follow a chain of events and see who gets paid.
#
# Debug artifact for the EPV net goals ledger. The aggregate checks cannot see
# who was paid, only how much: the two worst defects in the build (a successful
# pass charged as a turnover, and an orphaned save charged for a rebound it got
# no credit for) were both found here and invisible to conservation.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_chain_walkthrough.R
#
# Edit LEAGUE / SEASON / MATCH_ID below. MATCH_ID = NULL picks the first match
# with at least three goals.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE   <- "ENG"
SEASON   <- "2024-2025"
MATCH_ID <- NULL
SHARES   <- ng_shares()

# ---- build -----------------------------------------------------------------
events <- load_opta_match_events(LEAGUE, season = SEASON, source = "local")
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

fx <- as.data.table(load_opta_fixtures(LEAGUE, season = SEASON, source = "local"))[
  , .(match_id, home_team, away_team, home_team_id, away_team_id,
      home_score, away_score)]
adj <- ng_build_adjacency(events, verbose = FALSE)
pay <- ng_build_ledger(ep, adj = adj, fixtures = fx, shares = SHARES)

# ---- pick a match ----------------------------------------------------------
if (is.null(MATCH_ID)) {
  cand <- fx[match_id %in% unique(ep$match_id)][
    as.numeric(home_score) + as.numeric(away_score) >= 3]
  MATCH_ID <- cand$match_id[1]
}
f <- fx[match_id == MATCH_ID]
lab_team <- function(tid) fifelse(tid == f$home_team_id, "HOME", "AWAY")

m <- ep[match_id == MATCH_ID]; setorder(m, action_id)
m[, `:=`(side = lab_team(team_id),
         clock = sprintf("%02d:%02d", time_seconds %/% 60, round(time_seconds %% 60)))]
p <- pay[match_id == MATCH_ID]
p[, side := lab_team(team_id)]
nm <- unique(ep[, .(player_id, player_name)])
p <- merge(p, nm, by = "player_id", all.x = TRUE)

cat("\n", f$home_team, f$home_score, "-", f$away_score, f$away_team, "\n\n")

walk <- function(lo, hi, title) {
  cat("\n#### ", title, "\n\nACTIONS\n", sep = "")
  print(m[action_id %between% c(lo, hi),
          .(id = action_id, clock, side,
            player = substr(player_name, 1, 18), act = action_type, res = result,
            xpass = round(xpass, 2), EPV = round(epv, 3),
            delta = round(epv_delta, 3))], row.names = FALSE)
  cat("\nPAYMENTS (own frame: positive = good for that player)\n")
  y <- p[action_id %between% c(lo, hi),
         .(id = action_id, side,
           player = substr(ifelse(is.na(player_name), "(team pool)", player_name), 1, 18),
           role, paid = round(value_own, 4))]
  setorder(y, id, -paid)
  print(y[abs(paid) >= 5e-4], row.names = FALSE)
}

g <- m[action_type == "shot" & result == "success"]
if (nrow(g)) {
  i <- g$action_id[1]
  walk(max(1, i - 6), i + 1, paste0("A GOAL: ", g$side[1], " ", g$player_name[1]))
}

s <- m[action_type == "keeper_save"]
if (nrow(s)) {
  i <- s$action_id[which.max(abs(s$epv_delta))]
  walk(max(1, i - 4), i + 3, "A SHOT STOPPED: the biggest of the match")
}

miss <- m[action_type == "shot" & result != "success"]
if (nrow(miss)) {
  i <- miss$action_id[which.max(abs(miss$epv_delta))]
  walk(max(1, i - 4), i + 2, "A MISS: the biggest failed shot of the match")
}

# ---- match totals ----------------------------------------------------------
cat("\n\n#### MATCH TOTALS BY PLAYER (own frame, top 6 a side)\n\n")
tot <- p[!is.na(player_id), .(ng = sum(value_own)), by = .(player_id, side)]
tot <- merge(tot, nm, by = "player_id", all.x = TRUE)
for (sd in c("HOME", "AWAY")) {
  cat(sd, "\n")
  print(head(tot[side == sd][order(-ng), .(player_name, net_goals = round(ng, 3))], 6),
        row.names = FALSE)
  cat("\n")
}
cat(sprintf("home-frame total %.3f vs actual goal difference %d\n",
            p[, sum(value_home)],
            as.integer(f$home_score) - as.integer(f$away_score)))
cat("\nNote: pool payments keep their action_id here because ng_spread_pools()\n")
cat("has not been applied -- the spread drops it, by design. The row-sum identity\n")
cat("is asserted inside ng_build_ledger() before any of this.\n")
