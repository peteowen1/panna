# ng_position_report.R -- where each position gains and loses net goals, and
# which share moves the balance between them.
#
# Debug artifact for the EPV net goals ledger. Answers two questions:
#   1. does a position read plausibly, or does the allocation favour it?
#   2. which of the four shares actually moves the gap between positions?
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_position_report.R
#
# The sweep is the point. `med_err` -- the median |ledger - goal difference| per
# match -- is printed for every configuration and must never move: no share can
# change a match total, only who is paid. If it ever moves, a rule has started
# creating or destroying value and .ng_assert_row_sums() has a hole in it.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE <- "ENG"
SEASON <- "2024-2025"
SWEEP  <- TRUE   # FALSE = baseline shares only, much faster

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

lu <- as.data.table(lineups)
posmap <- as.data.table(get_player_positions(lu, ep))
mins <- unique(lu[, .(match_id, player_id, m = as.numeric(minutes_played))])[m > 0]

# Minutes-weighted, always. Averaging per-game per-90s instead lets a six-minute
# substitute count as much as a full match, which puts "Substitute" top of the
# table for arithmetic reasons rather than football ones.
per90 <- function(p) {
  pg <- p[, .(ng = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id)]
  pg <- merge(pg, mins, by = c("match_id", "player_id"))
  merge(pg, posmap, by = "player_id")
}

build <- function(sh) {
  p <- ng_build_ledger(ep, adj = adj, fixtures = fx, shares = sh, verbose = FALSE)
  ng_spread_pools(p, ep, lu, verbose = FALSE)
}

p4 <- build(ng_shares())
pg <- per90(p4)

cat("\n==== NET GOALS BY POSITION,", LEAGUE, SEASON, "====\n")
cat("Total net goals / total minutes x 90. Positive = added goals to his team.\n")
cat("A large spread means the allocation favours a position, not the players.\n\n")
print(pg[, .(player_games = .N, minutes = round(sum(m)),
             net_goals = round(sum(ng), 1),
             per90 = round(sum(ng) / sum(m) * 90, 4)),
         by = position][order(-per90)], row.names = FALSE)

cat("\n==== WHERE EACH POSITION EARNS IT (goals per 90, by role) ====\n")
cat("Each row sums to that position's per90 above.\n\n")
br <- merge(p4[, .(v = sum(value_own, na.rm = TRUE)),
               by = .(match_id, player_id, role)],
            pg[, .(match_id, player_id, position)], by = c("match_id", "player_id"))
pm <- pg[, .(tot_m = sum(m)), by = position]
br <- merge(br[, .(v = sum(v)), by = .(position, role)], pm, by = "position")
br[, p90 := round(v / tot_m * 90, 4)]
w <- dcast(br, position ~ role, value.var = "p90", fill = 0)
num <- setdiff(names(w), "position")
w[, total := round(rowSums(.SD), 4), .SDcols = num]
print(w[order(-total)], row.names = FALSE)

cat("\n==== SHOT-STOPPING ANCHOR ====\n")
st <- ep[action_type == "keeper_save"]
cat(sprintf("raw epv_delta on a stop row : %+.4f mean, %+.1f season total (n = %d)\n",
            mean(st$epv_delta, na.rm = TRUE), sum(st$epv_delta, na.rm = TRUE), nrow(st)))
cat(sprintf("new, stopper + rebound      : %+.4f mean, %+.1f season total\n",
            p4[role %chin% c("stopper", "stopper_rebound"), sum(value_own)] / nrow(st),
            p4[role %chin% c("stopper", "stopper_rebound"), sum(value_own)]))

if (isTRUE(SWEEP)) {
  grid <- data.table(
    eb = c(0.30, 0.50, 0.15, 0.30, 0.30, 0.30, 0.30, 0.15),
    ns = c(0.70, 0.70, 0.70, 0.40, 1.00, 0.70, 0.70, 1.00),
    op = c(0.10, 0.10, 0.10, 0.10, 0.10, 0.30, 0.00, 0.30))
  res <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
    sh <- ng_shares(exec_blame = grid$eb[i], named_share = grid$ns[i],
                    off_pool = grid$op[i])
    p <- build(sh); a <- per90(p)[, .(v = sum(ng) / sum(m) * 90), by = position]
    gp <- function(x) round(a[position == x]$v, 3)
    cons <- ng_check_conservation(p, fx, verbose = FALSE)
    data.table(exec_blame = grid$eb[i], named_share = grid$ns[i],
               off_pool = grid$op[i],
               ST = gp("Striker"), AM = gp("Attacking Midfielder"),
               MID = gp("Midfielder"), DM = gp("Defensive Midfielder"),
               GK = gp("Goalkeeper"), DEF = gp("Defender"),
               gap = gp("Striker") - gp("Defender"),
               med_err = round(median(abs(cons$err)), 3))
  }))
  cat("\n==== SHARE SWEEP ====\n")
  cat("gap = Striker minus Defender; smaller means positions compare more evenly.\n")
  cat("med_err = median |ledger - goal difference| per match; lower is better and\n")
  cat("MUST be identical in every row, because no share can change a match total.\n\n")
  print(res, row.names = FALSE)
  if (length(unique(res$med_err)) > 1) {
    cat("\n*** med_err MOVED. A rule is creating or destroying value. ***\n")
  }
  cat("\nDo not tune these to close the gap. Nobody has established that a striker\n")
  cat("and a centre-half should contribute equally; fitting to that target is\n")
  cat("metric-forcing. The arbiter is year-over-year repeatability.\n")
}
