# panna#273: WPA through the net-goals engine, Liverpool's 9 EPL 2026-27 matches.
# Agreed with Pete 2026-11-09: everyone starts at 50/50 (win +0.5, draw 0, loss -0.5 per team),
# and the net-goals engine routes the credit unchanged.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
M <- file.path(out, "models")
league <- "ENG"; season <- "2026-2027"; liv <- "c8h9bw1l82s06h77xxrelzhur"

events  <- load_opta_match_events(league, season = season)
lineups <- load_opta_lineups(league, season = season)
lu <- as.data.table(lineups)
liv_matches <- unique(lu[team_id == liv, match_id])
ev <- events[events$match_id %in% liv_matches, ]
lu_l <- lineups[lineups$match_id %in% liv_matches, ]
stopifnot(length(liv_matches) == 9)

spadl <- get_or_build_spadl(ev, league, season, cache_dir = file.path(out, "spadl"))
spadl_chains <- create_possession_chains(spadl)
co  <- add_next_chain_outcome(classify_chain_outcomes(spadl_chains))
lab <- create_next_goal_labels(label_actions_with_outcomes(spadl_chains, co))
spadl_epv <- calculate_action_epv(lab, features = NULL, readRDS(file.path(M, "epv_model.rds")),
                                  xg_model = readRDS(file.path(M, "xg_model.rds")),
                                  league = league, season = season, shot_lookup = NULL, events = ev)
match_results <- .build_match_results_from_events(ev, lu_l)
wp_chains <- as.data.table(copy(spadl_chains))
wp_chains[as.data.table(spadl_epv)[, .(match_id, action_id, epv)], epv := i.epv, on = c("match_id", "action_id")]
wp_chains <- add_red_card_to_chains(wp_chains, ev)
wp_feat <- create_wp_features(wp_chains, match_results)
w <- as.data.table(add_wp_vars(wp_feat, readRDS(file.path(M, "wp_model.rds"))))

# create_wp_features() drops action_id but sorts by (match_id, period_id, time_seconds) with a
# stable setorder, so the same sort of wp_chains lines up row for row. Asserted, not assumed.
ord <- copy(wp_chains); setorder(ord, match_id, period_id, time_seconds)
stopifnot(nrow(ord) == nrow(w), identical(ord$match_id, w$match_id),
          identical(as.character(ord$player_id), as.character(w$player_id)),
          identical(as.character(ord$action_type), as.character(w$action_type)))
w[, action_id := ord$action_id]

# Un-centred WPA: add_wp_vars() subtracts each match's mean (R/wp_model.R:733), which destroys the
# match total the ledger conserves. Same formula as add_wp_vars() without that line.
w[, wp_next := shift(wp, type = "lead"), by = match_id]
w[, team_next := shift(team_id, type = "lead"), by = match_id]
w[is.na(wp_next), `:=`(wp_next = wp_label, team_next = team_id)]
w[, wpa_raw := fifelse(team_next == team_id, wp_next - wp, (1 - wp_next) - wp)]

# Identity check, home frame: the actions' changes must add to (result - opening WP).
w[, home_sign := fifelse(team_id == home_team_id, 1, -1)]
mr <- as.data.table(match_results)
mr[, res_home := fifelse(home_goals > away_goals, 1, fifelse(home_goals < away_goals, 0, 0.5))]
first <- w[, .(wp_open_home = fifelse(team_id[1] == home_team_id[1], wp[1], 1 - wp[1]),
               sum_home = sum(home_sign * wpa_raw)), by = match_id]
mr[first, `:=`(wp_open_home = i.wp_open_home, sum_home = i.sum_home), on = "match_id"]
cat("\n=== Identity: sum of home-frame WPA vs result - opening WP (should match) ===\n")
print(mr[, .(match_id = substr(match_id, 1, 6), home_goals, away_goals, res_home,
             wp_open_home = round(wp_open_home, 3), sum_home = round(sum_home, 3),
             expected = round(res_home - wp_open_home, 3))])

# Engine inputs, as 10b builds them, with epv_delta replaced by the un-centred WPA.
spadl_ng <- add_xpass_to_spadl(spadl_epv, readRDS(file.path(M, "xpass_model.rds")))
shots <- as.data.frame(load_opta_shot_events(league, season = season))
lk <- c("match_id", "event_id", "type_id", "goalmouth_y", "goalmouth_z",
        intersect(c("situation", "is_blocked", "body_part"), names(shots)))
spadl_ng <- as.data.table(add_xgot_to_spadl(spadl_ng, readRDS(file.path(M, "xgot_model.rds")),
                                            shots[shots$match_id %in% liv_matches, lk],
                                            season = season, events = ev))
spadl_ng[, epv_delta_epv := epv_delta]
# The xGOT split prices the strike as xGOT - xG, in GOALS (R/epv_net_goals.R:1349); on a WPA row
# that is a units mismatch, so shots take the single-step rule (no xGOT), as torp's plain mode does.
spadl_ng[, xgot_kept := xgot][, xgot := NA_real_]
spadl_ng[w, epv_delta := i.wpa_raw, on = c("match_id", "action_id")]
stopifnot(sum(is.na(spadl_ng$epv_delta)) == 0)
fx <- as.data.frame(load_opta_fixtures(league, season = season, source = "local"))
fx <- fx[fx$match_id %in% liv_matches, ]
stopifnot(nrow(fx) == 9)

# shot_chain / shot_aftermath are EPV-unit rules (they restart value from 0 goals after a shot);
# off for WPA, as torp ran its engine in the plain configuration for win probability.
pay <- ng_build_ledger(spadl_ng, adj = ng_build_adjacency(ev, verbose = FALSE), fixtures = fx,
                       lineups = lu_l, shot_chain = FALSE, shot_aftermath = FALSE, verbose = TRUE)
dsh <- as.numeric(Sys.getenv("WPA_DACTS", "0.5")); cat("dacts_share =", dsh, "
")
pay <- ng_spread_pools(pay, spadl_ng, lu_l, dacts_share = dsh, verbose = TRUE)
pg <- as.data.table(ng_player_game(pay, lu_l, verbose = TRUE))
saveRDS(list(w = w, spadl_ng = spadl_ng, pay = pay, pg = pg, mr = mr, lineups = lu_l),
        file.path(out, sprintf("wpa_ledger_liv_d%s.rds", dsh)))
cat("\npg columns:\n"); print(names(pg))
