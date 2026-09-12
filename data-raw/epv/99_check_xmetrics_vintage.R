## 99_check_xmetrics_vintage.R ---------------------------------------------
## DIAGNOSTIC, not a pipeline step. Answers ONE question:
##
##     which xG model actually built this xMetrics file?
##
##   Rscript data-raw/epv/99_check_xmetrics_vintage.R [path]
##   XM_PATH=... Rscript data-raw/epv/99_check_xmetrics_vintage.R
##
## WHY THIS EXISTS. On 2026-09-12 a season-aware xG model was published, and
## stage 2 was dispatched to rebuild ratings on it. The dispatch was wasted: the
## daily epv-pipeline had refreshed opta_xmetrics at 11:16Z using the
## then-published (old) model, 70 minutes before the new one went live at 12:29Z,
## and stage 2 consumes those xmetrics rather than loading the xG model itself.
## The run would have COMPLETED CLEANLY and produced passing rating anchors --
## on a model that was never in the pipeline.
##
## A timestamp cannot catch that. It says when a file was written, not which
## model wrote it. This does, because the season term leaves a signature in the
## data that nothing else produces.
##
## THE SIGNATURE. Aggregate npgoals/npxg by season and take the spread. The old
## model has no season term and drifts monotonically across eras; the new one
## de-biases it. Measured values, both reproduced independently from
## PIPELINE-REBUILD-2026-09.md's own record:
##
##   OLD xG (no season term)  by-year spread 21.9 pts, range 0.8825-1.1019,
##                            overall npgoals/npxg 0.9728
##   NEW xG (+ season_num)    by-year spread  4.0 pts, range 0.9668-1.0066,
##                            overall 0.9851
##
## NB a season feature CANNOT extrapolate -- it de-biases the training corpus
## rather than predicting forward -- so this must be re-derived after each
## seasonal retrain rather than assumed to hold.

suppressPackageStartupMessages({library(data.table); library(arrow)})

args <- commandArgs(trailingOnly = TRUE)
P <- if (length(args) >= 1 && nzchar(args[1])) args[1] else
  Sys.getenv("XM_PATH", "../pannadata/data/opta/opta_xmetrics_bymatch.parquet")

if (!file.exists(P)) {
  cat("not found:", P, "\n")
  cat("pass a path, or set XM_PATH. To check the PUBLISHED vintage, download it\n")
  cat("to a SEPARATE path first -- never over a local baseline you still need.\n")
  quit(status = 1)
}

cat("file :", P, "\n")
cat("size :", round(file.size(P) / 1048576, 1), "MB | mtime:", format(file.mtime(P)), "\n\n")

d <- as.data.table(arrow::read_parquet(P))
cat("=== coverage ===\n")
cat("rows:", nrow(d), "| cols:", ncol(d), "\n")
for (c in intersect(c("npgoals", "npxg"), names(d))) {
  cat(sprintf("  %-8s %.2f%% populated\n", c, 100 * mean(!is.na(d[[c]]))))
}
if (!all(c("npgoals", "npxg", "season") %in% names(d))) {
  cat("\nneeds npgoals/npxg/season; cannot compute the signature\n"); quit(status = 1)
}

# season labels come in three shapes ("2025-2026", "2026", "2026 Country") --
# take the END year, never the start, or calendar-year leagues mis-bucket.
d[, sey := suppressWarnings(as.integer(substr(as.character(season), 1, 4)))]
d[grepl("^[0-9]{4}-[0-9]{4}$", as.character(season)),
  sey := as.integer(substr(as.character(season), 6, 9))]

by <- d[!is.na(sey) & !is.na(npgoals) & !is.na(npxg),
        .(npgoals = sum(npgoals, na.rm = TRUE),
          npxg    = sum(npxg, na.rm = TRUE), n = .N),
        by = sey][npxg > 0][order(sey)]
by[, ratio := npgoals / npxg]
by <- by[n >= 1000]                      # ignore sliver seasons
if (nrow(by) < 3) { cat("\ntoo few seasons to judge drift\n"); quit(status = 1) }

cat("\n=== npgoals/npxg by season (1.000 = calibrated; drift is the signal) ===\n")
print(by[, .(sey, n, ratio = round(ratio, 4))])

spread <- 100 * (max(by$ratio) - min(by$ratio))
cat(sprintf("\noverall npgoals/npxg : %.4f\n", sum(by$npgoals) / sum(by$npxg)))
cat(sprintf("BY-YEAR SPREAD       : %.1f pts (range %.4f - %.4f)\n",
            spread, min(by$ratio), max(by$ratio)))

verdict <- if (spread > 12) {
  "OLD xG vintage (no season term) -- near the documented 21.9 pts"
} else if (spread < 8) {
  "NEW xG vintage (has season_num) -- near the documented 4.0 pts"
} else {
  "AMBIGUOUS -- between the documented signatures; do not assume, investigate"
}
cat("\nVERDICT:", verdict, "\n")

cat("\nTo confirm against the model itself rather than the data:\n")
cat("  m <- load_xg_model(); m$panna_metadata$n_shots   # 3,237,375 = new, 1,080,653 = old\n")
cat("  any(grepl('season', m$importance$Feature))       # TRUE = has the season term\n")
