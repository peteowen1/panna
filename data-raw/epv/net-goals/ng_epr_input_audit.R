# ng_epr_input_audit.R -- what does build_epr_weekly.R actually read?
#
# It globs EVERY game_logs_*.parquet in the cache, which is more than the
# backfill writes: pre-2015 seasons, the upcoming season, and any stray a debug
# script left behind. Run this before trusting a coverage number.
#
# It is how `game_logs_BRA.parquet` was found -- 14,707 rows duplicating 467
# matches already present in the season files, double-counted in every EPR fit
# since May 2026. `build_epr_weekly.R` now dedups, but a new stray would still
# widen the denominator here and that is worth seeing.
#
#   powershell.exe -Command 'Rscript "data-raw/epv/net-goals/ng_epr_input_audit.R"'

suppressPackageStartupMessages({library(data.table); library(arrow)})
# build_epr_weekly.R:58 globs EVERY game_logs_*.parquet in the cache, so its
# net_goals coverage denominator is all of them -- not just the eleven the
# backfill rebuilt.
files <- list.files("data-raw/cache-predictions-opta",
                    pattern = "^game_logs_.*[.]parquet$", full.names = TRUE)
d <- rbindlist(lapply(files, function(f) {
  x <- as.data.table(read_parquet(f))
  data.table(file = basename(f), rows = nrow(x),
             ng = if ("net_goals" %in% names(x)) sum(!is.na(x$net_goals)) else 0L)
}))
d[, pct := round(100 * ng / rows, 1)]
print(d[order(file)], row.names = FALSE)
cat("\nfiles:", nrow(d), " total rows:", format(sum(d$rows), big.mark = ","),
    " with net_goals:", format(sum(d$ng), big.mark = ","), "\n")
cat("OVERALL COVERAGE:", round(100 * sum(d$ng) / sum(d$rows), 2), "%\n")
cat("build_epr_weekly's own guard needs >= 95%:",
    if (100 * sum(d$ng) / sum(d$rows) >= 95) "PASSES" else "FAILS", "\n")
cat("\nfiles with NO net_goals:\n"); print(d[ng == 0][order(file)], row.names = FALSE)
