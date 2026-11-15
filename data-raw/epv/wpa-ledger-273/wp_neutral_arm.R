# panna#273, input round 3: neutral venue on the current best model (SEP + xi_diff from as-of career Panna).
#   is_neutral = 1 for matches in a tournament competition (WC, EURO, COPA, AFCON, GOLD, ACUP, GULF, CWC),
#   from the step-01 fixture tables elo_all_leagues.R built; 0 otherwise. Caveat: a host nation's matches are
#   flagged neutral too (the pipeline's own is_neutral_venue covers only WC and EURO, with the same caveat).
# One arm against the baseline; same train / hold-out / Elo / XI table as wp_xi_arms.R (XI_SOURCE=panna).
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273"); fd <- file.path(out, "feats_all")
say <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))
TOURN <- c("WC", "EURO", "COPA", "AFCON", "GOLD", "ACUP", "GULF", "CWC")
fxr <- rbindlist(list(readRDS(file.path(out, "fixture_base/01_fixture_results.rds")),
                      readRDS(file.path(out, "fixture_extra/01_fixture_results.rds"))), fill = TRUE)
neut <- unique(as.data.table(fxr)[, .(match_id, is_neutral = as.integer(league %in% TOURN))], by = "match_id")
say("neutral flag: %d of %d fixtures neutral", neut[, sum(is_neutral)], nrow(neut))
elo <- unique(as.data.table(arrow::read_parquet(file.path(out, "match_elo_all.parquet"),
                                                col_select = c("match_id", "elo_diff")))[!is.na(elo_diff)], by = "match_id")
xi <- as.data.table(arrow::read_parquet(file.path(out, "xi_strength_panna.parquet")))
files <- data.table(f = list.files(fd, full.names = TRUE))
files[, key := sub("^wpfeat_(.*)\\.parquet$", "\\1", basename(f))][, season := sub("^.*_", "", key)]
files[, end_year := extract_season_end_year(season)]
files[, role := fcase(end_year %in% 2022:2024, "train", end_year == 2026L, "hold", default = NA_character_)]
files <- files[!is.na(role)]

rd <- function(f) {
  d <- as.data.table(arrow::read_parquet(f)); d[elo, elo_diff := i.elo_diff, on = "match_id"]
  d <- d[!is.na(elo_diff)]
  home <- d[is_home == 1L, .(home_team_id = team_id[1]), by = match_id]
  d[home, home_team_id := i.home_team_id, on = "match_id"]
  d[, away_team_id := team_id[team_id != home_team_id][1], by = match_id]
  d[xi, xi_home := i.xi_psr, on = .(match_id, home_team_id = team_id)]
  d[xi, xi_away := i.xi_psr, on = .(match_id, away_team_id = team_id)]
  d[, xi_diff := xi_home - xi_away][, c("xi_home", "xi_away", "home_team_id", "away_team_id") := NULL]
  d[neut, is_neutral := i.is_neutral, on = "match_id"]
  d
}
tr <- rbindlist(lapply(files[role == "train", f], rd), fill = TRUE)
say("train: %d rows, %d matches; neutral matches %d; is_neutral missing on %d rows", nrow(tr), uniqueN(tr$match_id),
    tr[is_neutral == 1L, uniqueN(match_id)], tr[is.na(is_neutral), .N])
fits <- list(SEP_xi = train_wp_structural(tr, use_possession = TRUE, extra_features = "xi_diff"),
             SEP_xi_neutral = train_wp_structural(tr, use_possession = TRUE, extra_features = c("xi_diff", "is_neutral")))
saveRDS(fits, file.path(out, "wp_neutral_arm_models.rds")); rm(tr); gc(verbose = FALSE)

ll <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -(y * log(p) + (1 - y) * log(1 - p)) }
acc <- list()
for (f in files[role == "hold", f]) {
  d <- rd(f)
  for (nm in names(fits)) {
    p <- predict_wp(fits[[nm]], d)
    acc[[paste(f, nm)]] <- data.table(unit = basename(f), model = nm, rows = nrow(d), ll = sum(ll(p, d$wp_label)),
                                      n_rows = sum(d$is_neutral %in% 1L), n_ll = sum(ll(p, d$wp_label)[d$is_neutral %in% 1L]))
  }
}
A <- rbindlist(acc)
res <- A[, .(rows = sum(rows), logloss = round(sum(ll) / sum(rows), 5), neutral_rows = sum(n_rows),
             neutral_logloss = round(sum(n_ll) / pmax(sum(n_rows), 1), 5)), by = model]
cat("\n=== Hold-out (seasons ending 2026, all rows): log loss lower is better ===\n"); print(res)
by_u <- dcast(A[, .(ll = sum(ll) / sum(rows)), by = .(unit, model)], unit ~ model, value.var = "ll")
cat(sprintf("\nCompetitions where + neutral beats SEP + XI: %d of %d\n", by_u[, sum(SEP_xi_neutral < SEP_xi)], nrow(by_u)))
print(by_u[grepl("WC|EURO|COPA|AFCON|CWC", unit)])
say("DONE")
