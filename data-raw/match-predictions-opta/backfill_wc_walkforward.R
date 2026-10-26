# backfill_wc_walkforward.R — honest predictions for past World Cups (one-off)
#
# Why: step 07's models are fitted on every season up to last season, so the
# published odds for WC 2014/2018/2022 came from a model that had seen those
# tournaments' results. Pete (2026-10-25) chose a walk-forward backfill for just
# these three tournaments (pannaverse docs/plans/HONEST-PREDICTIONS-PLAN.md,
# section 3 and decision D4): for each one, refit steps 05 and 06 on matches
# played BEFORE its first match, then predict it with step 07. Rows carry
# record = "backtest" (computed after the event, so never "forecast").
#
# What is still not as-of (named, not fixed; plan section 2): the features are
# the match dataset as built today, so frame-wide statistics stay frame-wide
# (EPR/PSR league offsets and scales, Elo hyperparameters, step 04's NA-fill
# means). The cut is on the model fit only.
#
# Cost: one refit is about 10 min and 2.1 GB peak R memory (plan section 3),
# so the three run in about 30-40 min. Each tournament's rows are written as
# soon as its fit finishes, so a killed run keeps what it had.
#
# Run it from the predictions workflow (dispatch with wc_backfill = true): that
# uses a fresh 04_match_dataset.rds, whose played rows carry as-at EPR/PSR/SPM
# (panna#308). A match dataset older than #308 gives end-of-season ratings
# that include the tournament, which is what the first, local copy did.
#
# Usage (from panna/, via powershell Rscript because of arrow):
#   Rscript data-raw/match-predictions-opta/backfill_wc_walkforward.R <04_match_dataset.rds> <work_dir>
# Output: <work_dir>/wc_backfill_predictions.parquet (all finished tournaments),
# read by step 12 from cache_dir when present.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: backfill_wc_walkforward.R <04_match_dataset.rds> <work_dir>")
md_path <- args[1]
work_dir <- args[2]
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
step_dir <- file.path("data-raw", "match-predictions-opta")
TOURNAMENT_YEARS <- c(2014L, 2018L, 2022L)

md <- readRDS(md_path)
md_date <- as.Date(substr(as.character(md$match_date), 1, 10))
is_played <- md$split != "fixture"
cat(sprintf("match dataset: %d rows, %d played, dates %s to %s\n", nrow(md), sum(is_played),
            format(min(md_date)), format(max(md_date))))

out_path <- file.path(work_dir, "wc_backfill_predictions.parquet")
done <- list()
for (yr in TOURNAMENT_YEARS) {
  t0 <- Sys.time()
  wc_rows <- is_played & md$league == "WC" & md$season_end_year == yr
  if (!any(wc_rows)) stop("no played WC rows with season_end_year ", yr)
  start <- min(md_date[wc_rows])
  train_rows <- is_played & md_date < start
  cat(sprintf("\n=== WC %d: %d matches from %s; training on %d matches %s to %s ===\n",
              yr, sum(wc_rows), format(start), sum(train_rows),
              format(min(md_date[train_rows])), format(max(md_date[train_rows]))))

  cut <- md[train_rows | wc_rows, ]
  # The tournament is "val": steps 05/06 only report metrics on val (they fit on
  # train with CV), so this prints an honest score for the tournament itself.
  cut$split <- ifelse(md_date[train_rows | wc_rows] < start, "train", "val")
  cut_dir <- file.path(work_dir, paste0("wc", yr))
  dir.create(cut_dir, showWarnings = FALSE)
  saveRDS(cut, file.path(cut_dir, "04_match_dataset.rds"))
  rm(cut); invisible(gc())

  cache_dir <- cut_dir
  for (s in c("05_fit_goals_model.R", "06_fit_outcome_model.R", "07_predict_fixtures.R")) {
    source(file.path(step_dir, s), local = new.env())
  }
  p <- as.data.table(readRDS(file.path(cut_dir, "07_predictions.rds")))
  p <- p[league == "WC" & split == "val"]
  if (nrow(p) != sum(wc_rows)) stop(sprintf("WC %d: predicted %d of %d matches", yr, nrow(p), sum(wc_rows)))
  p[, `:=`(record = "backtest", backfill_cutoff = format(start), backfill_train_n = sum(train_rows))]
  done[[as.character(yr)]] <- p
  arrow::write_parquet(rbindlist(done, fill = TRUE), out_path)
  res <- fifelse(p$home_goals > p$away_goals, "H", fifelse(p$home_goals < p$away_goals, "A", "D"))
  p_true <- fifelse(res == "H", p$prob_H, fifelse(res == "A", p$prob_A, p$prob_D))
  cat(sprintf("WC %d done in %.1f min: %d matches, log loss %.4f (lower is better); written %s\n",
              yr, as.numeric(difftime(Sys.time(), t0, units = "mins")), nrow(p),
              mean(-log(pmax(p_true, 1e-6))), out_path))
  unlink(list.files(cut_dir, pattern = "^0[456]_.*[.]rds$", full.names = TRUE))
}
all_rows <- rbindlist(done, fill = TRUE)
if (length(done) != length(TOURNAMENT_YEARS) || anyNA(all_rows$prob_H) ||
    !all(all_rows$record == "backtest")) {
  stop("backfill incomplete: ", length(done), " of ", length(TOURNAMENT_YEARS),
       " tournaments, ", sum(is.na(all_rows$prob_H)), " rows with no prob_H")
}
cat(sprintf("\nAll done: %d tournaments, %d rows in %s\n", length(done), nrow(all_rows), out_path))
