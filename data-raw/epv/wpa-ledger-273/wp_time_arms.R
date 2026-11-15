# panna#273, input round 1 (Pete, 2026-11-10: "more time based vars"): does SEP gain from
#   stoppage_min      minutes past the end of the current period's regulation time (0 before; 90+3 -> 3)
#   since_goal_min    minutes since the last goal (NA before the first goal)
# Each is ONE arm against a refit SEP baseline on the same data (one change per arm).
# Grain: one row per on-ball action; target: match result from the home side (1 / 0.5 / 0).
# Train: the gate's cached features, seasons ending 2022-2024 (1 row in 8 per match); hold-out: seasons
# ending 2026, all rows. Elo: match_elo_all.parquet. Score: log loss (lower is better), overall and on
# stoppage-time rows; goal per turnover (EPV ~35).
# Caveat: in the next-goal mixture the "one goal up" branch keeps since_goal_min unchanged.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273"); fd <- file.path(out, "feats_all")
say <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
elo <- unique(as.data.table(arrow::read_parquet(file.path(out, "match_elo_all.parquet"),
                                                col_select = c("match_id", "elo_diff")))[!is.na(elo_diff)], by = "match_id")
files <- data.table(f = list.files(fd, full.names = TRUE))
files[, key := sub("^wpfeat_(.*)\\.parquet$", "\\1", basename(f))][, season := sub("^.*_", "", key)]
files[, end_year := extract_season_end_year(season)]
files[, role := fcase(end_year %in% 2022:2024, "train", end_year == 2026L, "hold", default = NA_character_)]
files <- files[!is.na(role)]

# Period ends in cumulative seconds (create_wp_features: 2nd half starts at 2700, ET periods 15 min).
PERIOD_END <- c(`1` = 2700, `2` = 5400, `3` = 6300, `4` = 7200)
add_time <- function(d) {
  setorder(d, match_id, period_id, time_seconds)
  d[, stoppage_min := pmax(0, time_seconds - PERIOD_END[as.character(period_id)]) / 60]
  # A goal shows as a change in score_diff between consecutive rows; on the 1-in-8 training rows the
  # goal time is the first row after it (within ~8 actions).
  d[, goal_t := fifelse(score_diff != shift(score_diff, fill = 0L), time_seconds, NA_real_), by = match_id]
  d[, last_goal_t := nafill(goal_t, type = "locf"), by = match_id]
  d[, since_goal_min := (time_seconds - last_goal_t) / 60][, c("goal_t", "last_goal_t") := NULL]
  d
}
rd <- function(f) {
  d <- as.data.table(arrow::read_parquet(f)); d[elo, elo_diff := i.elo_diff, on = "match_id"]
  add_time(d[!is.na(elo_diff)])
}
tr <- rbindlist(lapply(files[role == "train", f], rd), fill = TRUE)
say("train: %d rows, %d matches; stoppage rows %.1f%%; rows before any goal %.1f%%", nrow(tr), uniqueN(tr$match_id),
    100 * mean(tr$stoppage_min > 0), 100 * mean(is.na(tr$since_goal_min)))
arms <- list(SEP = character(), SEP_stoppage = "stoppage_min", SEP_since_goal = "since_goal_min")
fits <- lapply(arms, function(x) train_wp_structural(tr, use_possession = TRUE, extra_features = x))
saveRDS(fits, file.path(out, "wp_time_arms_models.rds")); rm(tr); gc(verbose = FALSE)

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
                                      st_rows = sum(d$stoppage_min > 0), st_ll = sum(ll(p, d$wp_label)[d$stoppage_min > 0]),
                                      g_sw = d2[kind == "goal", sum(sw)], g_n = d2[kind == "goal", .N],
                                      t_sw = d2[kind == "turnover", sum(sw)], t_n = d2[kind == "turnover", .N])
  }
}
A <- rbindlist(acc)
res <- A[, .(rows = sum(rows), logloss = round(sum(ll) / sum(rows), 5), stoppage_rows = sum(st_rows),
             stoppage_logloss = round(sum(st_ll) / sum(st_rows), 5),
             goal_per_turnover = round((sum(g_sw) / sum(g_n)) / (sum(t_sw) / sum(t_n)), 1)), by = model]
cat("\n=== Hold-out (seasons ending 2026, all rows): log loss lower is better ===\n"); print(res)
by_u <- dcast(A[, .(ll = sum(ll) / sum(rows)), by = .(unit, model)], unit ~ model, value.var = "ll")
cat(sprintf("\nCompetitions where the arm beats SEP: stoppage %d of %d, since_goal %d of %d\n",
            by_u[, sum(SEP_stoppage < SEP)], nrow(by_u), by_u[, sum(SEP_since_goal < SEP)], nrow(by_u)))
fwrite(A, file.path(out, "wp_time_arms_by_unit.csv")); say("DONE")
