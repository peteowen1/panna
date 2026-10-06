# xG next round, step 14: report card for v5.2 / v3.2 (direct corners out, panna#277)
# against v5.1 / v3.1, on the SAME rows: the v5.1 out-of-fold predictions are cut
# to the rows v5.2 trained on, so the comparison moves only the model.
# Logloss is out-of-fold, lower is better. goals/xG: 1.00 is calibrated.
# Run from panna/ after v52_run.ps1:  Rscript data-raw/epv/xg-vnext/xgv_14_direct_corner_report.R
suppressMessages({library(data.table); library(arrow)})
X <- "data-raw/cache/epv/xg-vnext"
.is_direct_corner <- local({ e <- new.env(); sys.source("R/constants.R", e); sys.source("R/xg_model.R", e); e$.is_direct_corner })
ll <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -mean(y * log(p) + (1 - y) * log(1 - p)) }
zone <- function(x, y) fifelse(x >= 97 & (y <= 4 | y >= 96), "corner flag",
                        fifelse(x >= 94 & (y <= 15 | y >= 85), "wide byline", fifelse(x >= 94, "central byline", "rest")))
dcf <- read_parquet(file.path(X, "direct_corner_flags.parquet"))

# ---- xG: rebuild the v5.1 training rows exactly as xgv_06 did, then flag direct corners
d <- as.data.table(read_parquet(file.path(X, "shot_features.parquet")))
d <- d[is_penalty == 0 & period_id %in% 1:4 & !is.na(rebound) & !is.na(season_num)]
cov <- d[, .(nongoal = sum(goal == 0)), by = .(competition, season)]
d <- d[!cov[nongoal == 0], on = .(competition, season)]
fp <- read_parquet(file.path(X, "feed_passes.parquet"))
d <- d[match_id %in% fp$match_id[fp$full_feed]]
d[, oof1 := readRDS(file.path(X, "prod_cv_1.rds"))$oof]
d[, event_id := as.character(event_id)]
d[, q263 := paste(match_id, event_id) %in% paste(dcf$match_id, dcf$event_id)]   # a lookup: row order unchanged
dc <- .is_direct_corner(d$x, d$y, fifelse(d$is_corner %in% 1, "Corner", ""), d$q263)
k <- d[!dc]; oof2 <- readRDS(file.path(X, "prod_cv_2.rds"))$oof
stopifnot(length(oof2) == nrow(k))   # same row order as xgv_06 (lookup, then filter)
k[, oof2 := oof2][, zone := zone(x, y)]
cat(sprintf("xG training rows: v5.1 %s, v5.2 %s (dropped %d direct corners, %d tagged q263)\n",
            format(nrow(d), big.mark = ","), format(nrow(k), big.mark = ","), sum(dc), sum(d$q263 & dc)))
cat("\nxG, out-of-fold, same rows. logloss: lower is better; goals/xG: 1.00 is calibrated\n")
print(rbind(k[, .(cut = "all", shots = .N, goals = sum(goal), ll_v51 = ll(goal, oof1), ll_v52 = ll(goal, oof2),
                  gxg_v51 = sum(goal) / sum(oof1), gxg_v52 = sum(goal) / sum(oof2))],
            k[, .(shots = .N, goals = sum(goal), ll_v51 = ll(goal, oof1), ll_v52 = ll(goal, oof2),
                  gxg_v51 = sum(goal) / sum(oof1), gxg_v52 = sum(goal) / sum(oof2)), by = .(cut = zone)][order(cut)]), digits = 4)
cat("\nxG goals/xG by season (seasons with >= 20,000 shots)\n")
print(k[, .(shots = .N, gxg_v51 = sum(goal) / sum(oof1), gxg_v52 = sum(goal) / sum(oof2)), by = season_num][shots >= 20000][order(season_num)], digits = 4)
cat(sprintf("\nlargest per-shot change on kept rows: %.4f | rows moved more than 0.01: %d\n",
            max(abs(k$oof2 - k$oof1)), sum(abs(k$oof2 - k$oof1) > 0.01)))
fwrite(k[, .(match_id, event_id, x, y, goal, zone, oof1, oof2)], file.path(X, "report_v52_xg_rows.csv.gz"))

# ---- xGOT: the logs carry the cv logloss; the calibration files carry goals/xGOT by season
for (t in c("_1", "_2")) { f <- file.path(X, paste0("xgot_calib_by_season", t, ".csv")); if (file.exists(f)) {
  c0 <- fread(f)[cut == "all"]; cat(sprintf("\nxGOT%s goals/xGOT by season: %.3f to %.3f over %d seasons\n", t, min(c0$goals_per_xgot), max(c0$goals_per_xgot), nrow(c0))) } }
