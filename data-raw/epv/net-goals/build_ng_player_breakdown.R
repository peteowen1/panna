# Build the per-player season totals (ng_player_breakdown_<season>.parquet) from
# per-match breakdown files already on disk -- no 10b rerun. 10b writes both
# files itself from now on; this is for seasons built before it did.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/build_ng_player_breakdown.R
suppressMessages(devtools::load_all(quiet = TRUE))
C <- "data-raw/cache-predictions-opta"
files <- list.files(C, pattern = "^ng_breakdown_.*[.]parquet$", full.names = TRUE)
if (!length(files)) stop("no ng_breakdown_<season>.parquet in ", C)
for (f in files) {
  season <- sub(".parquet", "", sub("ng_breakdown_", "", basename(f), fixed = TRUE), fixed = TRUE)
  bd <- data.table::as.data.table(arrow::read_parquet(f, col_select = c("match_id", "player_id", "category", "value")))
  pl <- .ng_breakdown_players(bd)
  out <- file.path(C, sprintf("ng_player_breakdown_%s.parquet", season))
  arrow::write_parquet(pl, out, chunk_size = 5000L)
  cat(sprintf("%s: %d match rows -> %d rows, %d players\n", season, nrow(bd), nrow(pl),
              data.table::uniqueN(pl$player_id)))
}
