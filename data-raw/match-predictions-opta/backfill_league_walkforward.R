# backfill_league_walkforward.R -- honest pre-match predictions for past league
# seasons, refit at the start of every calendar month (one-off, fplverse)
#
# Why: step 07's models are fitted on every season up to last season, so a
# past season's published predictions are in-sample ("none" rows, no odds).
# fplverse backtests ENG 2021-22 to 2024-25 against outside models and needs
# real pre-match numbers (Pete agreed 2026-11-06). Same method as
# backfill_wc_walkforward.R: refit steps 05 and 06 on matches played BEFORE a
# cutoff, then predict with step 07. Cutoffs are the first day of each month
# that has a match; each match is predicted by the newest refit strictly before
# its kickoff day (train = matches dated before the cutoff). Rows carry
# record = "backtest".
#
# What is still not as-of (named, not fixed): the features are the match
# dataset as built by the run that produced it. Since panna#308 the played rows
# carry as-at EPR/PSR/SPM, but frame-wide quantities stay frame-wide: EPR/PSR
# league offsets and scales, Elo hyperparameters, step 04's NA-fill means, and
# every upstream model trained on all history (xG/xGOT/xPass/EPV/WP models,
# PSR/PSV coefficients, the SPM fit, skill shrinkage parameters).
#
# Cost: one refit is about 10 min and 2.1 GB peak R memory (WC backfill
# measurement), so a season of ~10 cutoffs is ~100 min. Results are written
# after every cutoff, so a killed run keeps what it had.
#
# Usage (from panna/):
#   Rscript data-raw/match-predictions-opta/backfill_league_walkforward.R \
#     <04_match_dataset.rds> <01_fixture_results.rds> <work_dir> <league> <season_end_year> [max_cutoffs]
# max_cutoffs (optional) runs only the first N cutoffs, for a timing smoke test;
# the completeness check at the end then fails on purpose.
# Output: <work_dir>/walkforward_<league>_<season_end_year>.parquet

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 5) {
  stop("usage: backfill_league_walkforward.R <04_match_dataset.rds> <01_fixture_results.rds> <work_dir> <league> <season_end_year>")
}
md_path <- args[1]
fx_path <- args[2]
work_dir <- args[3]
league_code <- args[4]
season_year <- as.integer(args[5])
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
step_dir <- file.path("data-raw", "match-predictions-opta")
stopifnot(file.exists(md_path), file.exists(fx_path), !is.na(season_year))

md <- readRDS(md_path)
md_date <- as.Date(substr(as.character(md$match_date), 1, 10))
is_played <- md$split != "fixture"
# An undated played row would slip into every cutoff's cut as an all-NA row.
if (anyNA(md_date[is_played])) stop(sum(is.na(md_date[is_played])), " played rows have no match date")
cat(sprintf("match dataset: %d rows, %d played, dates %s to %s\n", nrow(md), sum(is_played),
            format(min(md_date)), format(max(md_date))))

target_all <- is_played & md$league == league_code & md$season_end_year == season_year
if (!any(target_all)) stop("no played ", league_code, " rows with season_end_year ", season_year)
month_start <- function(d) as.Date(format(d, "%Y-%m-01"))
cutoffs <- sort(unique(month_start(md_date[target_all])))
n_run <- if (length(args) >= 6) min(as.integer(args[6]), length(cutoffs)) else length(cutoffs)
cat(sprintf("%s %d: %d matches, %d monthly cutoffs (%s to %s)\n", league_code, season_year,
            sum(target_all), length(cutoffs), format(min(cutoffs)), format(max(cutoffs))))

out_path <- file.path(work_dir, sprintf("walkforward_%s_%d.parquet", league_code, season_year))
done <- list()
for (i in seq_len(n_run)) {
  t0 <- Sys.time()
  cut_date <- cutoffs[i]
  next_cut <- seq(cut_date, by = "month", length.out = 2)[2]
  target <- target_all & md_date >= cut_date & md_date < next_cut
  train_rows <- is_played & md_date < cut_date
  cat(sprintf("\n=== cutoff %s: %d matches to %s; training on %d matches %s to %s ===\n",
              format(cut_date), sum(target), format(next_cut - 1), sum(train_rows),
              format(min(md_date[train_rows])), format(max(md_date[train_rows]))))

  keep <- train_rows | target
  cut <- md[keep, ]
  # The month's matches are "val": steps 05/06 fit on train with CV and only
  # report metrics on val, so the log prints an honest score for the month.
  cut$split <- ifelse(md_date[keep] < cut_date, "train", "val")
  cut_dir <- file.path(work_dir, sprintf("%s_%d_%s", league_code, season_year, format(cut_date, "%Y%m")))
  dir.create(cut_dir, showWarnings = FALSE)
  saveRDS(cut, file.path(cut_dir, "04_match_dataset.rds"))
  file.copy(fx_path, file.path(cut_dir, "01_fixture_results.rds"), overwrite = TRUE)
  rm(cut); invisible(gc())

  cache_dir <- cut_dir
  for (s in c("05_fit_goals_model.R", "06_fit_outcome_model.R", "07_predict_fixtures.R")) {
    source(file.path(step_dir, s), local = new.env())
  }
  p <- as.data.table(readRDS(file.path(cut_dir, "07_predictions.rds")))
  p <- p[league == league_code & split == "val"]
  if (nrow(p) != sum(target)) {
    stop(sprintf("cutoff %s: predicted %d of %d matches", format(cut_date), nrow(p), sum(target)))
  }
  p[, `:=`(record = "backtest", season_end_year = season_year,
           cutoff_date = format(cut_date), backfill_train_n = sum(train_rows))]
  done[[format(cut_date)]] <- p
  arrow::write_parquet(rbindlist(done, fill = TRUE), out_path)
  res <- fifelse(p$home_goals > p$away_goals, "H", fifelse(p$home_goals < p$away_goals, "A", "D"))
  p_true <- fifelse(res == "H", p$prob_H, fifelse(res == "A", p$prob_A, p$prob_D))
  cat(sprintf("cutoff %s done in %.1f min: %d matches, log loss %.4f (lower is better); written %s\n",
              format(cut_date), as.numeric(difftime(Sys.time(), t0, units = "mins")), nrow(p),
              mean(-log(pmax(p_true, 1e-6))), out_path))
  unlink(cut_dir, recursive = TRUE)
}

all_rows <- rbindlist(done, fill = TRUE)
bad <- anyNA(all_rows$pred_home_goals) || anyNA(all_rows$pred_away_goals) || anyNA(all_rows$prob_H)
if (length(done) != length(cutoffs) || nrow(all_rows) != sum(target_all) || bad ||
    anyDuplicated(all_rows$match_id) > 0) {
  stop(sprintf("backfill incomplete: %d of %d cutoffs, %d of %d matches, NA predictions: %s, duplicate match_ids: %d",
               length(done), length(cutoffs), nrow(all_rows), sum(target_all), bad,
               sum(duplicated(all_rows$match_id))))
}
cat(sprintf("\nAll done: %s %d, %d matches over %d cutoffs, all predicted, in %s\n",
            league_code, season_year, nrow(all_rows), length(cutoffs), out_path))
