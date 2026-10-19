# check_game_logs_coverage.R -- refuse a rebuilt game-logs file that covers less than the
# published one.
#
# Rows, columns, duplicate keys and sha256 all passed on the 2026-10-19 republish while it
# blanked piero on 15k-94k rows a season, spm_overall on tens of thousands and 2025-26 net
# goals for three leagues (rolled back the same day). This compares the non-NA count of every
# shared column, per league, on the player-matches both files hold, and fails on any drop
# over 1% of the league's rows that today's code does not explain.
#
# Explained (reported, not failed), row by row:
#   - PSV and everything built on it (psv/osv/dsv/psv_league_offset/piero/piero_value_p90/
#     piero_percentile) for:
#       * cameos under 10 minutes: PSV needs 10+ (min_match_minutes, 01_compute_match_stats.R);
#       * matches with no xMetrics (opta_xmetrics_bymatch.parquet): PSV needs them, and
#         enrich_match_stats_with_xmetrics() aborts a league with none;
#       * rows whose published psv was only the league offset: the pre-R007 code filled a
#         missing PSV with 0, so that "PSV" never existed.
#
# Usage (from panna/):
#   Rscript data-raw/match-predictions-opta/check_game_logs_coverage.R \
#     --new <rebuilt.parquet> --old <published.parquet> [--xmetrics <opta_xmetrics_bymatch.parquet>]
# Exit 1 on an unexplained loss; prints one line per (league, column) that lost coverage.

suppressPackageStartupMessages({library(data.table); library(arrow)})
args <- commandArgs(TRUE)
arg <- function(k) { i <- match(k, args); if (is.na(i)) NA_character_ else args[i + 1L] }
new_f <- arg("--new"); old_f <- arg("--old"); xm_f <- arg("--xmetrics")
if (is.na(new_f) || is.na(old_f)) stop("usage: --new <file> --old <file> [--xmetrics <file>]")
# Read through a buffer, not a memory map, so either file can be replaced afterwards.
rd <- function(f) as.data.table(read_parquet(arrow::buffer(readBin(f, "raw", file.size(f)))))
d <- rd(new_f); p <- rd(old_f)
# The 2026-10 rename: a file published before it says panna/offense/defense/panna_percentile.
ren <- c(panna = "piero", offense = "epv_offensive", defense = "epv_defensive",
         panna_percentile = "piero_percentile", panna_value_p90 = "piero_value_p90")
for (x in list(d, p)) for (o in names(ren)) {
  if (o %in% names(x) && !ren[[o]] %in% names(x)) setnames(x, o, ren[[o]])  # by reference
}
cat(sprintf("new %s: %d rows, %d leagues | published %s: %d rows, %d leagues\n",
            basename(new_f), nrow(d), uniqueN(d$league), basename(old_f), nrow(p), uniqueN(p$league)))
stopifnot(nrow(d) > 0, nrow(p) > 0)

# Leagues the rebuild covers (a part holds a subset). A published league missing here entirely
# is the caller's check (the refuse step compares league lists); this file judges coverage.
p <- p[league %in% unique(d$league)]
# 10b writes no PSV columns when PSV failed for every league in the file (pre-2014 seasons have
# no xMetrics). Judge them as all-NA columns, so the row rules below decide, and say so.
psv_all <- c("psv", "osv", "dsv", "psv_league_offset", "piero", "piero_value_p90", "piero_percentile")
for (cc in intersect(setdiff(names(p), names(d)), psv_all)) {
  cat(sprintf("rebuild has no %s column: judged as all-NA\n", cc))
  d[, (cc) := NA_real_]
}
lost_cols <- setdiff(names(p), names(d))
key <- c("player_id", "match_id")
cols <- setdiff(intersect(names(d), names(p)), c(key, "league", "season", "player_name", "team_id", "match_date"))
m <- merge(d[, c(key, "league", cols), with = FALSE], p[, c(key, cols), with = FALSE],
           by = key, suffixes = c(".n", ".o"))
cat(sprintf("shared player-matches: %d of %d rebuilt, %d published\n", nrow(m), nrow(d), nrow(p)))
if (nrow(m) == 0) stop("no shared player-matches: the files cannot be compared")
# Published rows the rebuild dropped altogether: no column comparison can see those.
gone <- p[!d, on = key][, .(gone = .N), by = league]
gone <- merge(gone, p[, .(rows = .N), by = league], by = "league")[gone > 0.01 * rows]
if (nrow(gone)) {
  cat("\nleagues where the rebuild DROPPED more than 1% of the published player-matches:\n")
  print(gone)
}

psv_family <- intersect(psv_all, cols)
mins <- if ("total_minutes.o" %in% names(m)) m[["total_minutes.o"]] else m[["total_minutes.n"]]
expl_cameo <- !is.na(mins) & mins < 10
expl_fake <- if (all(c("psv.o", "psv_league_offset.o") %in% names(m))) {
  !is.na(m[["psv.o"]]) & !is.na(m[["psv_league_offset.o"]]) & abs(m[["psv.o"]] - m[["psv_league_offset.o"]]) < 1e-4
} else rep(FALSE, nrow(m))
expl_noxm <- rep(FALSE, nrow(m))
if (!is.na(xm_f)) {
  xm_ids <- unique(read_parquet(xm_f, col_select = "match_id")$match_id)
  stopifnot(length(xm_ids) > 0)
  expl_noxm <- !m$match_id %in% xm_ids
} else {
  cat("no --xmetrics given: losses on matches without xMetrics count as unexplained\n")
}
m[, .expl_psv := expl_cameo | expl_fake | expl_noxm]

out <- rbindlist(lapply(cols, function(cc) {
  n <- m[[paste0(cc, ".n")]]; o <- m[[paste0(cc, ".o")]]
  lost_row <- is.na(n) & !is.na(o)
  ex <- if (cc %in% psv_family) m$.expl_psv else rep(FALSE, nrow(m))
  data.table(league = m$league, lost = lost_row, unexpl = lost_row & !ex, gained = !is.na(n) & is.na(o))[
    , .(rows = .N, lost = sum(lost), unexplained = sum(unexpl), gained = sum(gained)), by = league][
    , col := cc]
}))
out[, fail := unexplained > pmax(0.01 * rows, 0)]
rep <- out[lost > 0.01 * rows | fail][order(-fail, league, col)]
options(width = 200)
if (nrow(rep)) {
  cat("\n(league, column) cells that lost coverage (lost = published non-NA now NA; unexplained = not cameo/no-xMetrics/fake-PSV):\n")
  print(rep[, .(league, col, rows, lost, unexplained, gained, fail)], nrows = 500)
} else cat("\nno column lost more than 1% of a league's rows\n")
if (length(lost_cols)) cat("\nCOLUMNS ABSENT from the rebuild:", paste(lost_cols, collapse = " "), "\n")
nfail <- sum(rep$fail) + length(lost_cols) + nrow(gone)
cat(sprintf("\nCOVERAGE %s: %d failing cell(s), %d column(s) absent, %d league(s) short of rows\n",
            if (nfail) "FAILED" else "OK", sum(rep$fail), length(lost_cols), nrow(gone)))
if (nfail) quit(status = 1)
