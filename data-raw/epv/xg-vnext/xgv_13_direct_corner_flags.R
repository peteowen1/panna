# xG next round, step 13: Opta's direct-corner tag (qualifier 263) for every shot.
# =============================================================================
# shot_features.parquet has no q263 column, and xgv_06 / xgv_08 must drop the same
# shots add_xg_to_spadl() prices at DIRECT_CORNER_XG (panna#277): q263, or a
# Corner-situation shot inside the corner-flag box. This reads the tag from the
# events, one competition file at a time, with the filter pushed into arrow.
# Resumable: each events file writes its own part, and a re-run skips parts on disk.
#
# Output: data-raw/cache/epv/xg-vnext/direct_corner_flags.parquet (match_id, event_id, q263)
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_13_direct_corner_flags.R
suppressMessages({library(data.table); library(arrow); library(dplyr)})
X <- "data-raw/cache/epv/xg-vnext"; PART <- file.path(X, "dcf"); dir.create(PART, showWarnings = FALSE)
OD <- "C:/dev/pannaverse/pannadata/data/opta/events_consolidated"
files <- list.files(OD, "^events_.*[.]parquet$", full.names = TRUE)
stopifnot(length(files) > 0)
t0 <- Sys.time()
for (p in files) {
  out <- file.path(PART, basename(p)); if (file.exists(out)) next
  q <- as.data.table(open_dataset(p) |>
    filter(type_id %in% c(13L, 14L, 15L, 16L), grepl('"263":', qualifier_json, fixed = TRUE)) |>
    select(match_id, event_id) |> collect())
  # event_id is int64 in most files but double in a few: as.character() on a double can give "2.107e+09"
  q[, `:=`(match_id = as.character(match_id), event_id = format(event_id, scientific = FALSE, trim = TRUE), q263 = TRUE)]
  write_parquet(q, out)   # an empty part still marks the file as done
}
dcf <- unique(rbindlist(lapply(list.files(PART, full.names = TRUE), read_parquet)), by = c("match_id", "event_id"))
stopifnot(nrow(dcf) > 0)
write_parquet(dcf, file.path(X, "direct_corner_flags.parquet"))
cat("q263 shots:", nrow(dcf), "in", uniqueN(dcf$match_id), "matches from", length(files), "events files |",
    round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s\n")
