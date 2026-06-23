# FAST PATH: offset-only refresh of opta_psr_weekly.parquet.
#
# Why this exists: the weekly PSR raw skills did NOT change this deploy — only
# the per-league DISPLAY offset did. 08b re-estimates ~28k players' skills at
# every one of 234 snapshot dates (~89s/date, ~6h) purely to reproduce numbers
# that already sit in the released parquet, then adds the offset at the end.
#
# Since the released parquet is confirmed OFFSET-FREE (raw psr), we can take it
# as-is, attach each player's as-of-date primary league, and add the network
# offset additively — identical result, minutes instead of hours. This is the
# `offset_only_refresh` speedup from project_psr_xgot_deploy_handoff.
#
# Set TEST_DATES to a small vector to dry-run a couple of dates first.
suppressMessages({devtools::load_all(".", quiet = TRUE); library(arrow); library(data.table)})

cache_dir <- file.path("data-raw", "cache-skills")
opta_dir  <- opta_data_dir()
existing_path <- file.path(opta_dir, "opta_psr_weekly.parquet")

TEST_DATES <- if (exists("TEST_DATES")) TEST_DATES else NULL   # NULL = full run

# 1. Inputs --------------------------------------------------------------
cat("=== Loading inputs ===\n")
weekly <- as.data.table(read_parquet(existing_path))
weekly[, snapshot_date := as.Date(snapshot_date)]
out_cols <- names(weekly)   # exact output schema to preserve
cat(sprintf("  existing weekly: %s rows, %d dates (%s..%s)\n",
            format(nrow(weekly), big.mark=","), uniqueN(weekly$snapshot_date),
            min(weekly$snapshot_date), max(weekly$snapshot_date)))

offsets <- as.data.table(read_parquet(file.path(cache_dir, "psr_league_offsets.parquet")))
cat(sprintf("  offsets: %d leagues\n", nrow(offsets)))

ms_path <- { p <- file.path(cache_dir, "01_match_stats_slim.rds")
             if (file.exists(p)) p else file.path(cache_dir, "01_match_stats.rds") }
match_stats <- as.data.table(readRDS(ms_path))
if (!inherits(match_stats$match_date, "Date")) match_stats[, match_date := as.Date(match_date)]
lgcol <- if ("competition" %in% names(match_stats)) "competition" else "league"

# 2. As-of-date primary league (verbatim logic from 08b) -----------------
pl_src <- match_stats[!is.na(get(lgcol)),
                      .(player_id, .lg = get(lgcol), match_date, total_minutes)]
PSR_LEAGUE_WINDOW_DAYS <- 365L
.primary_league_asof <- function(d) {
  hist <- pl_src[match_date < d]
  if (!nrow(hist)) return(NULL)
  .pick <- function(x) {
    if (!nrow(x)) return(NULL)
    a <- x[, .(m = sum(total_minutes, na.rm = TRUE)), by = .(player_id, .lg)]
    setorder(a, player_id, -m)
    a[, .(league = .lg[1L]), by = player_id]
  }
  recent <- .pick(hist[match_date >= d - PSR_LEAGUE_WINDOW_DAYS])
  allt   <- .pick(hist)
  if (is.null(allt)) return(recent)
  if (is.null(recent)) return(allt)
  out <- merge(allt, recent, by = "player_id", all.x = TRUE, suffixes = c("_all", "_recent"))
  out[, league := fifelse(is.na(league_recent), league_all, league_recent)]
  out[, .(player_id, league)]
}

# 3. Apply offset per snapshot date --------------------------------------
dates <- sort(unique(weekly$snapshot_date))
if (!is.null(TEST_DATES)) dates <- as.Date(TEST_DATES)
cat(sprintf("\n=== Applying offsets across %d dates ===\n", length(dates)))

start <- Sys.time()
res <- vector("list", length(dates))
for (i in seq_along(dates)) {
  d <- dates[i]
  pdt <- weekly[snapshot_date == d]
  if ("league" %in% names(pdt)) pdt[, league := NULL]
  pl_d <- .primary_league_asof(d)
  if (!is.null(pl_d)) {
    pdt <- merge(pdt, pl_d, by = "player_id", all.x = TRUE)
    pdt <- apply_psr_league_offsets(pdt, offsets)
  }
  res[[i]] <- pdt[, ..out_cols]
  if (i %% 25 == 0 || i == 1L || i == length(dates)) {
    el <- as.numeric(difftime(Sys.time(), start, units = "secs"))
    cat(sprintf("  [%d/%d] %s  (%.0fs, ~%.1fm left)\n", i, length(dates), d, el,
                if (i>1) (el/i)*(length(dates)-i)/60 else 0))
  }
}
out <- rbindlist(res, use.names = TRUE)
setorder(out, snapshot_date, player_id)

# 4. Verify + (test) report ----------------------------------------------
latest <- out[snapshot_date == max(snapshot_date)][order(-psr)]
cat(sprintf("\n=== Top 12 @ %s (offset-applied) ===\n", as.character(max(out$snapshot_date))))
print(latest[1:12, .(player_name, primary_position, psr=round(psr,3), weighted_90s=round(weighted_90s,1))])
rj <- out[grepl("Randall", player_name) & snapshot_date == max(snapshot_date)]
if (nrow(rj)) cat(sprintf("\n  J. Randall now psr=%.3f (rank %d of %d)\n",
                          rj$psr[1], which(latest$player_id == rj$player_id[1])[1], nrow(latest)))

if (is.null(TEST_DATES)) {
  arrow::write_parquet(as.data.frame(out), existing_path)
  cat(sprintf("\n=== WROTE %s (%s rows, %d dates, %.1f MB) ===\n",
              existing_path, format(nrow(out), big.mark=","), uniqueN(out$snapshot_date),
              file.info(existing_path)$size/1024^2))
} else {
  cat("\n[TEST MODE] not written.\n")
}
