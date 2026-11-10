# panna#273, input round 2: starting-XI strength on SEP.
#   xi_diff = home XI strength - away XI strength, where a team's XI strength = 11 x the mean PSR of its
#   rated starters, each starter's PSR taken from the latest weekly snapshot (opta_psr_weekly.parquet)
#   dated BEFORE the match day (snapshot_date <= match_date - 1, at most 400 days old). A team with fewer
#   than 9 rated starters gets NA, never 0.
# One arm (SEP + xi_diff) against a refit SEP; same train / hold-out / Elo as wp_time_arms.R.
# Caveat (not checked): PSR's coefficients were fitted on seasons that overlap the hold-out.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273"); fd <- file.path(out, "feats_all")
say <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
elo <- unique(as.data.table(arrow::read_parquet(file.path(out, "match_elo_all.parquet"),
                                                col_select = c("match_id", "elo_diff")))[!is.na(elo_diff)], by = "match_id")
files <- data.table(f = list.files(fd, full.names = TRUE))
files[, key := sub("^wpfeat_(.*)\\.parquet$", "\\1", basename(f))]
files[, league := sub("_[^_]*$", "", key)][, season := sub("^.*_", "", key)]
files[, end_year := extract_season_end_year(season)]
files[, role := fcase(end_year %in% 2022:2024, "train", end_year == 2026L, "hold", default = NA_character_)]
files <- files[!is.na(role)]

# ---- XI strength per team-match ----
xi_path <- file.path(out, "xi_strength.parquet")
if (!file.exists(xi_path)) {
  psr <- as.data.table(arrow::read_parquet("C:/dev/pannaverse/pannadata/data/opta/opta_psr_weekly.parquet",
                                           col_select = c("snapshot_date", "player_id", "psr")))[!is.na(psr)]
  psr[, snapshot_date := as.Date(snapshot_date)]
  say("PSR weekly: %d rows, %d players, %s to %s", nrow(psr), uniqueN(psr$player_id), min(psr$snapshot_date), max(psr$snapshot_date))
  setkey(psr, player_id, snapshot_date)
  xi <- rbindlist(lapply(seq_len(nrow(files)), function(i) {
    u <- files[i]
    lu <- tryCatch(as.data.table(load_opta_lineups(u$league, season = u$season, source = "local")), error = function(e) NULL)
    if (is.null(lu) || !nrow(lu)) { say("no lineups: %s", u$key); return(NULL) }
    st <- unique(lu[is_starter %in% TRUE, .(match_id, team_id, player_id, d = as.Date(substr(match_date, 1, 10)) - 1L)])
    st[, snap_d := d]
    j <- psr[st, on = .(player_id, snapshot_date = snap_d), roll = 400, .(match_id, team_id, player_id, psr = x.psr)]
    j[, .(n_starters = .N, n_rated = sum(!is.na(psr)), xi_psr = 11 * mean(psr, na.rm = TRUE)), by = .(match_id, team_id)]
  }), fill = TRUE)
  xi[n_rated < 9, xi_psr := NA_real_]
  arrow::write_parquet(xi, xi_path); rm(psr); gc(verbose = FALSE)
}
xi <- as.data.table(arrow::read_parquet(xi_path))
say("XI table: %d team-matches; %.1f%% with 9+ rated starters; XI PSR mean %.2f, sd %.2f", nrow(xi),
    100 * mean(!is.na(xi$xi_psr)), mean(xi$xi_psr, na.rm = TRUE), sd(xi$xi_psr, na.rm = TRUE))

rd <- function(f) {
  d <- as.data.table(arrow::read_parquet(f)); d[elo, elo_diff := i.elo_diff, on = "match_id"]
  d <- d[!is.na(elo_diff)]
  home <- d[is_home == 1L, .(home_team_id = team_id[1]), by = match_id]
  d[home, home_team_id := i.home_team_id, on = "match_id"]
  hx <- xi[, .(match_id, team_id, xi_psr)]
  d[hx, xi_home := i.xi_psr, on = .(match_id, home_team_id = team_id)]
  # away = the other team in the match
  d[, away_team_id := team_id[team_id != home_team_id][1], by = match_id]
  d[hx, xi_away := i.xi_psr, on = .(match_id, away_team_id = team_id)]
  d[, xi_diff := xi_home - xi_away][, c("xi_home", "xi_away", "home_team_id", "away_team_id") := NULL]
  d
}
tr <- rbindlist(lapply(files[role == "train", f], rd), fill = TRUE)
m_cov <- tr[, .(has = !is.na(xi_diff[1]), elo = elo_diff[1], xi = xi_diff[1]), by = match_id]
say("train: %d rows, %d matches; xi_diff present for %.1f%% of matches; correlation with elo_diff %.3f", nrow(tr),
    nrow(m_cov), 100 * mean(m_cov$has), m_cov[has == TRUE, cor(elo, xi)])
fits <- list(SEP = train_wp_structural(tr, use_possession = TRUE),
             SEP_xi = train_wp_structural(tr, use_possession = TRUE, extra_features = "xi_diff"))
saveRDS(fits, file.path(out, "wp_xi_arms_models.rds")); rm(tr); gc(verbose = FALSE)

ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -(y * log(p) + (1 - y) * log(1 - p)) }
acc <- list()
for (f in files[role == "hold", f]) {
  d <- rd(f)
  for (nm in names(fits)) {
    p <- predict_wp(fits[[nm]], d)
    d2 <- copy(d)[, p := p]
    d2[, `:=`(pn = shift(p, type = "lead"), tn = shift(team_id, type = "lead"), sdn = shift(score_diff, type = "lead")), by = match_id]
    d2[, kind := fcase(is.na(tn), NA_character_, sdn != score_diff, "goal", tn == team_id, "same", default = "turnover")]
    d2[, sw := abs(fifelse(tn == team_id, pn - p, (1 - pn) - p))]
    acc[[paste(f, nm)]] <- data.table(unit = basename(f), model = nm, rows = nrow(d), ll = sum(ll(p, d$wp_label)),
                                      xi_rows = sum(!is.na(d$xi_diff)), xi_ll = sum(ll(p, d$wp_label)[!is.na(d$xi_diff)]),
                                      g_sw = d2[kind == "goal", sum(sw)], g_n = d2[kind == "goal", .N],
                                      t_sw = d2[kind == "turnover", sum(sw)], t_n = d2[kind == "turnover", .N])
  }
}
A <- rbindlist(acc)
res <- A[, .(rows = sum(rows), logloss = round(sum(ll) / sum(rows), 5), rows_with_xi = sum(xi_rows),
             logloss_rows_with_xi = round(sum(xi_ll) / sum(xi_rows), 5),
             goal_per_turnover = round((sum(g_sw) / sum(g_n)) / (sum(t_sw) / sum(t_n)), 1)), by = model]
cat("\n=== Hold-out (seasons ending 2026, all rows): log loss lower is better ===\n"); print(res)
by_u <- dcast(A[, .(ll = sum(ll) / sum(rows)), by = .(unit, model)], unit ~ model, value.var = "ll")
by_u[, gain := round(SEP - SEP_xi, 4)]
cat(sprintf("\nCompetitions where SEP + XI beats SEP: %d of %d\n", by_u[, sum(gain > 0)], nrow(by_u)))
print(by_u[order(-gain), .(unit, SEP = round(SEP, 4), SEP_xi = round(SEP_xi, 4), gain)])
fwrite(A, file.path(out, "wp_xi_arms_by_unit.csv")); say("DONE")
