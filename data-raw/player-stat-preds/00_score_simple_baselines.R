# Score the two simplest next-game player stat baselines on EPL 2025-26.
#
# Baseline 1: a player's per-90 rate over his last 5 appearances (any competition)
#   times his average minutes in those games, Poisson ladder.
# Position average: the 2024-25 EPL per-90 rate for his usual starting slot, same minutes.
# Metric: ranked probability score per player-match (lower is better).
# Plan and first results: pannaverse docs/plans/PLAYER-STAT-MODEL.md section 5.
#
# Opta omits a stat when it is zero, so blanks on a played row are set to 0.
# Hand-run, reads only. Run from PowerShell (arrow segfaults under Git Bash R):
#   Rscript data-raw/player-stat-preds/00_score_simple_baselines.R
# Input: opta_player_stats.parquet (opta-latest); set PLAYER_STATS_PARQUET to its path.

suppressMessages({library(arrow); library(dplyr); library(data.table)})
st <- c(shots="totalScoringAtt", shots_on_target="ontargetScoringAtt", tackles="totalTackle", fouls="fouls", clearances="totalClearance", yellow="yellowCard")
stats_path <- Sys.getenv("PLAYER_STATS_PARQUET", "../pannadata/data/opta/opta_player_stats.parquet")
stopifnot(file.exists(stats_path))
ds <- open_dataset(stats_path)
cat("seasons with EPL:\n"); print(ds |> filter(competition == "EPL") |> count(season) |> collect() |> arrange(season) |> tail(4))
d <- ds |> filter(season %in% c("2025-2026","2025","2024-2025")) |>
  select(all_of(c("match_id","match_date","player_id","team_id","competition","season","position","gameStarted","minsPlayed", unname(st)))) |>
  collect() |> as.data.table()
for (w in c("gameStarted","minsPlayed", st)) { set(d, j = w, value = suppressWarnings(as.numeric(d[[w]]))); set(d, which(is.na(d[[w]])), w, 0) }
d[, md := as.Date(substr(as.character(match_date), 1, 10))]
d <- d[minsPlayed > 0]
cat("played rows", nrow(d), "| EPL 2025-26 rows", d[competition=="EPL" & season=="2025-2026", .N], "matches", d[competition=="EPL" & season=="2025-2026", uniqueN(match_id)], "dates", as.character(range(d[competition=="EPL" & season=="2025-2026"]$md)), "\n")
# position for subs is 'Substitute': use the player's modal starting position in the training season
pos <- d[position != "Substitute", .N, by = .(player_id, position)][order(-N)][, .SD[1], by = player_id][, .(player_id, pos = position)]
d <- merge(d, pos, by = "player_id", all.x = TRUE); d[is.na(pos), pos := "Unknown"]
tr <- d[competition == "EPL" & season == "2024-2025"]
pm <- tr[, c(list(n = .N), lapply(.SD, function(x) sum(x) / sum(minsPlayed) * 90)), by = pos, .SDcols = unname(st)]
te <- d[md >= as.Date("2025-07-01") & md <= as.Date("2026-06-30")][order(player_id, md)]
for (s in st) te[, paste0("r5_", s) := { cs <- shift(frollsum(get(s), 5, adaptive = FALSE, partial = TRUE)); cm <- shift(frollsum(minsPlayed, 5, partial = TRUE)); cs / cm * 90 }, by = player_id]
te[, m5 := shift(frollmean(minsPlayed, 5, partial = TRUE)), by = player_id]
te[, prior_apps := seq_len(.N) - 1L, by = player_id]
ev <- te[competition == "EPL" & season == "2025-2026"]
ev <- merge(ev, pm, by = "pos", all.x = TRUE, suffixes = c("", ".pm"))
cat("eval rows", nrow(ev), "| with >=1 prior app this season", ev[prior_apps >= 1, .N], "| >=5", ev[prior_apps >= 5, .N], "| no position mean", ev[is.na(n), .N], "\n")
rps <- function(y, mu) { K <- 0:15; mapply(function(yy, m) sum((ppois(K, m) - (yy <= K))^2), y, mu) }
out <- rbindlist(lapply(names(st), function(nm) {
  s <- st[[nm]]; e <- ev[prior_apps >= 1 & !is.na(n)]
  mu1 <- pmax(e[[paste0("r5_", s)]] * e$m5 / 90, 0.01)       # baseline 1: last 5, own minutes forecast
  mu3 <- pmax(e[[paste0(s, ".pm")]] * e$m5 / 90, 0.01)       # position mean, same minutes forecast
  mu1o <- pmax(e[[paste0("r5_", s)]] * e$minsPlayed / 90, 0.01) # baseline 1 with ACTUAL minutes (oracle)
  mu3o <- pmax(e[[paste0(s, ".pm")]] * e$minsPlayed / 90, 0.01)
  r1 <- rps(e[[s]], mu1); r3 <- rps(e[[s]], mu3); dd <- r1 - r3
  mt <- tapply(dd, e$match_id, sum); se <- sd(mt) * sqrt(length(mt)) / length(dd)
  data.table(stat = nm, n = nrow(e), mean_actual = round(mean(e[[s]]), 3), rps_last5 = round(mean(r1), 4), rps_pos_mean = round(mean(r3), 4), diff = round(mean(dd), 4), se = round(se, 4),
             rps_last5_true_mins = round(mean(rps(e[[s]], mu1o)), 4), rps_pos_true_mins = round(mean(rps(e[[s]], mu3o)), 4), bias_last5 = round(mean(mu1 - e[[s]]), 3))
}))
print(out)
e <- ev[prior_apps >= 1]; cat("minutes: last-5 average MAE", round(mean(abs(e$m5 - e$minsPlayed)), 1), "| n", nrow(e), "| mean actual", round(mean(e$minsPlayed), 1), "\n")
