# xG next round, step 12: which matches have a real event feed.
# =============================================================================
# 3,296 matches in the Opta data are goals-only feeds (kick-off, the goals,
# full-time: 0-9 passes, and 99.6% of their "shots" are goals); 161 more are
# partial feeds (10-199 passes, goal rate 13-16% against the normal 10%). xG v5
# and xGOT v3 trained on them and learned to price them near 1 (pannaverse
# docs/plans/XG-VNEXT-2026-09.md, "goals-only feeds"). Pete, 2026-09-28: never
# train on goals-only matches. The rule: a match trains only if its event feed
# has at least MIN_PASSES passes. Full matches: 1st percentile 680 passes.
#
# Output: data-raw/cache/epv/xg-vnext/feed_passes.parquet (match_id, passes,
# events, full_feed), read by xgv_06, xgv_08 and the EPV pack retrain.
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_12_feed_passes.R
suppressMessages({library(data.table); library(arrow); library(dplyr); devtools::load_all(quiet = TRUE)})
X <- "data-raw/cache/epv/xg-vnext"
MIN_PASSES <- 200L
ed <- file.path(opta_data_dir(), "events_consolidated")
files <- list.files(ed, pattern = "^events_.*[.]parquet$", full.names = TRUE)
stopifnot(length(files) > 50)
fp <- rbindlist(lapply(files, function(f) as.data.table(open_dataset(f) |> group_by(match_id) |>
  summarise(passes = sum(type_id == 1L), events = n()) |> collect())))
fp <- fp[, .(passes = sum(passes), events = sum(events)), by = match_id]   # a match in two files counts once
fp[, full_feed := passes >= MIN_PASSES]
write_parquet(fp, file.path(X, "feed_passes.parquet"))
cat(sprintf("event files %d | matches %d | full feed (>= %d passes) %d | thin or goals-only %d\n",
            length(files), nrow(fp), MIN_PASSES, fp[full_feed == TRUE, .N], fp[full_feed == FALSE, .N]))
