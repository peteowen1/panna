# score_record.R — score the published match-prediction record by honesty tier
#
# Why: the published predictions mix three kinds of row (red-team 2026-10-25,
# pannaverse/docs/reviews/OPUS-redteam-2026-10-25.md). Only two may be published
# under the publish-only-pre-match rule (pannaverse/docs/plans/HONEST-PREDICTIONS-PLAN.md):
#   locked        the last forecast made before the match day (predictions-locked.parquet)
#   retrodiction  the model never trained on the match (split val/test), but the
#                 number was computed after it was played
#   train         the model was fitted on the match: NOT honest, scored only to
#                 show how much it flatters the record
# This script scores each tier with its n, against each league's base rate from
# train seasons. It reads files only; it publishes nothing.
#
# Usage (from panna/, via powershell Rscript because of arrow):
#   Rscript data-raw/match-predictions-opta/score_record.R <in_dir> <out_dir>
# <in_dir> holds, as published:
#   match_predictions.parquet   blog-latest (the file the blog reads)
#   match_features.parquet      predictions-latest (split, home_goals, away_goals)
#   predictions-locked.parquet  R2 football/predictions-locked.parquet
#   hist/predictions_<date>.parquet   predictions-history snapshots (optional)

suppressPackageStartupMessages({ library(data.table); library(arrow) })

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: score_record.R <in_dir> <out_dir>")
in_dir <- args[1]
out_dir <- args[2]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Load and sanity-check ----

mp <- as.data.table(read_parquet(file.path(in_dir, "match_predictions.parquet")))
mf <- as.data.table(read_parquet(file.path(in_dir, "match_features.parquet"),
                                 col_select = c("match_id", "split", "season_end_year",
                                                "home_goals", "away_goals")))
lk <- as.data.table(read_parquet(file.path(in_dir, "predictions-locked.parquet")))
cat(sprintf("match_predictions: %d rows (%d played, %d fixture), dates %s to %s\n",
            nrow(mp), sum(mp$status == "played"), sum(mp$status == "fixture"),
            min(substr(mp$match_date, 1, 10)), max(substr(mp$match_date, 1, 10))))
cat(sprintf("match_features: %d rows, %d unique match_id\n", nrow(mf), uniqueN(mf$match_id)))
print(mf[, .N, by = split][order(split)])
cat(sprintf("predictions-locked: %d rows, locked_at %s to %s\n",
            nrow(lk), min(lk$locked_at), max(lk$locked_at)))
print(lk[, .N, by = lock_source])
if (anyDuplicated(lk$match_id)) stop("duplicate match_id in the lock file")

res <- unique(mf[!is.na(home_goals) & !is.na(away_goals)], by = "match_id")
res[, result := fifelse(home_goals > away_goals, "H", fifelse(home_goals < away_goals, "A", "D"))]

# 2. Scoring helpers ----

score_rows <- function(d) {
  # d: result, pH, pD, pA, bH, bD, bA (league base rate)
  p_true <- d[, fifelse(result == "H", pH, fifelse(result == "A", pA, pD))]
  b_true <- d[, fifelse(result == "H", bH, fifelse(result == "A", bA, bD))]
  pick <- c("H", "D", "A")[max.col(as.matrix(d[, .(pH, pD, pA)]), ties.method = "first")]
  data.table(
    n = nrow(d),
    log_loss = round(mean(-log(pmax(p_true, 1e-6))), 4),
    base_log_loss = round(mean(-log(b_true)), 4),
    brier = round(mean(d[, (pH - (result == "H"))^2 + (pD - (result == "D"))^2 +
                           (pA - (result == "A"))^2]), 4),
    accuracy = round(mean(pick == d$result), 4)
  )
}

# 3. Tier tables ----

played <- merge(mp[status == "played", .(match_id, league, match_date = substr(match_date, 1, 10),
                                         pH = prob_H, pD = prob_D, pA = prob_A)],
                res[, .(match_id, split, season_end_year, result)], by = "match_id")
cat(sprintf("played rows with a result: %d of %d\n", nrow(played), sum(mp$status == "played")))

# League base rate from train-split matches; leagues with no train rows use the pooled rate.
base <- played[split == "train", .(bH = mean(result == "H"), bD = mean(result == "D"),
                                   bA = mean(result == "A")), by = league]
pooled <- played[split == "train", .(bH = mean(result == "H"), bD = mean(result == "D"),
                                     bA = mean(result == "A"))]
add_base <- function(d) {
  d <- merge(d, base, by = "league", all.x = TRUE)
  miss <- is.na(d$bH)
  if (any(miss)) {
    cat(sprintf("  %d rows in leagues with no train matches use the pooled base rate\n", sum(miss)))
    d[miss, `:=`(bH = pooled$bH, bD = pooled$bD, bA = pooled$bA)]
  }
  d
}
played <- add_base(played)

locked <- merge(lk[, .(match_id, lock_source, locked_at, lH = prob_H_pre, lD = prob_D_pre, lA = prob_A_pre)],
                played, by = "match_id")
bad_lock <- locked[substr(locked_at, 1, 10) >= match_date]
if (nrow(bad_lock) > 0) stop(nrow(bad_lock), " locked rows have locked_at on or after match day")

tiers <- rbindlist(list(
  cbind(tier = "locked (pre-match lock file)",
        score_rows(locked[, .(result, pH = lH, pD = lD, pA = lA, bH, bD, bA)])),
  cbind(tier = "same matches, published regenerated row",
        score_rows(locked[, .(result, pH, pD, pA, bH, bD, bA)])),
  cbind(tier = "retrodiction (split val/test, not locked)",
        score_rows(played[split %in% c("val", "test") & !match_id %in% lk$match_id])),
  cbind(tier = "all out-of-fit (val/test, published row)",
        score_rows(played[split %in% c("val", "test")])),
  cbind(tier = "train (in-sample, to be dropped)",
        score_rows(played[split == "train"]))
))

# 4. Red-team F2 reproduction: latest predictions-history snapshot strictly before match day ----

hist_dir <- file.path(in_dir, "hist")
f2 <- NULL
if (dir.exists(hist_dir)) {
  fs <- list.files(hist_dir, pattern = "^predictions_[0-9-]{10}[.]parquet$", full.names = TRUE)
  sn <- rbindlist(lapply(fs, function(f) {
    d <- as.data.table(read_parquet(f, col_select = c("match_id", "match_date", "prob_H", "prob_D", "prob_A", "status")))
    d[, snap := substr(basename(f), 13, 22)]
    d
  }))
  sn[, match_date := substr(match_date, 1, 10)]
  pre <- sn[snap < match_date & status == "fixture"][order(match_id, -rank(snap))][, .SD[1], by = match_id]
  pre <- merge(pre[, .(match_id, sH = prob_H, sD = prob_D, sA = prob_A)], played, by = "match_id")
  cat(sprintf("history snapshots: %d files; matches with a pre-match snapshot and a result: %d\n",
              length(fs), nrow(pre)))
  f2 <- rbindlist(list(
    cbind(tier = "pre-match history snapshot (red-team F2)",
          score_rows(pre[, .(result, pH = sH, pD = sD, pA = sA, bH, bD, bA)])),
    cbind(tier = "same matches, published regenerated row",
          score_rows(pre[, .(result, pH, pD, pA, bH, bD, bA)]))
  ))
}

# 5. By season and by league, honest tiers only ----

played[, tier := fifelse(match_id %in% lk$match_id, "locked",
                 fifelse(split %in% c("val", "test"), "retrodiction", "train"))]
by_season <- played[, score_rows(.SD), by = .(season_end_year, split, tier)][order(season_end_year, split, tier)]
lk_scored <- locked[, .(league, result, pH = lH, pD = lD, pA = lA, bH, bD, bA)]
by_league_locked <- lk_scored[, score_rows(.SD), by = league][order(-n)]
by_league_retro <- played[tier == "retrodiction", score_rows(.SD), by = league][order(-n)]

# Calibration of the locked tier: stated probability vs how often it happened, per outcome.
cal <- rbindlist(lapply(c("H", "D", "A"), function(o) {
  p <- locked[[paste0("l", o)]]
  data.table(outcome = o, bin = cut(p, c(0, .2, .3, .4, .5, .6, .7, 1)), p = p, hit = locked$result == o)
}))[, .(n = .N, stated = round(mean(p), 3), happened = round(mean(hit), 3)), by = .(outcome, bin)][order(outcome, bin)]

# 6. Write ----

fwrite(tiers, file.path(out_dir, "tiers.csv"))
if (!is.null(f2)) fwrite(f2, file.path(out_dir, "f2_snapshot.csv"))
fwrite(by_season, file.path(out_dir, "by_season.csv"))
fwrite(by_league_locked, file.path(out_dir, "by_league_locked.csv"))
fwrite(by_league_retro, file.path(out_dir, "by_league_retrodiction.csv"))
fwrite(cal, file.path(out_dir, "calibration_locked.csv"))

cat("\n== Tiers (log loss, Brier: lower is better; base = each league's train-season H/D/A rate) ==\n")
print(tiers)
if (!is.null(f2)) { cat("\n== Red-team F2 reproduction ==\n"); print(f2) }
cat("\n== By season ==\n"); print(by_season)
cat("\n== Locked tier calibration ==\n"); print(cal)
cat("\n== Locked tier by league (top 15 by n) ==\n"); print(head(by_league_locked, 15))
cat(sprintf("\nWrote %s\n", out_dir))
