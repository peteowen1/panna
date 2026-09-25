# Publish the twelve rebuilt game-log seasons, and their net goals breakdowns, to pannadata@blog-latest.
# =============================================================================
# Built by _run_10b_pack_2026_09.R on the new EPV + xGOT (published 2026-09-24);
# every season verified by ng_verify_seasons.R (each team-match within 5e-4 of its
# own goal difference, none over 0.001, 100% coverage). game_logs.parquet is the
# current-season alias the blog reads, so it gets the 2026-27 file.
#
# Rollback: the previous assets are replaced; re-running 10b with the old models
# (backup C:/dev/_model-backups/2026-09-23) rebuilds them.
#
# Run from panna/:  Rscript data-raw/epv/pack_publish_game_logs.R            (dry run)
#                   PACK_PUBLISH=1 Rscript data-raw/epv/pack_publish_game_logs.R  (publish)
suppressMessages(devtools::load_all(quiet = TRUE))
C <- "data-raw/cache-predictions-opta"
SEASONS <- c("2015-2016", "2016-2017", "2017-2018", "2018-2019", "2019-2020", "2020-2021",
             "2021-2022", "2022-2023", "2023-2024", "2024-2025", "2025-2026", "2026-2027")
src <- file.path(C, sprintf("game_logs_%s.parquet", SEASONS))
stopifnot(all(file.exists(src)))
age_h <- as.numeric(difftime(Sys.time(), file.mtime(src), units = "hours"))
if (any(age_h > 48)) stop("a season file is older than 48 h -- not from today's rebuild: ", paste(src[age_h > 48], collapse = ", "))
# A FIXED list every season must carry, not only "what the other seasons have":
# a systemic failure strips every season alike, which a cross-season comparison
# cannot see (review finding, 2026-09-24).
REQUIRED <- c("net_goals", "ng_recon", "epv_total", "psv",
              "goals_minus_xgot", "placement_added", "xgot", "gsaa", "gsaa_per90", "xgot_faced",
              "goals_conceded", "aerial_woe_per90", "aerial_poss_woe_per90", "takeon_woe_per90",
              "tackle_poss_woe_per90", "containment_woe_per90")
for (f in src) {
  cols <- names(arrow::open_dataset(f))
  miss <- setdiff(REQUIRED, cols)
  if (length(miss)) stop(basename(f), " lacks required columns: ", paste(miss, collapse = ", "))
}
# Every season must carry every column any season carries. 2026-09-24: the
# 2026-27 file was published without its 12 xGOT / GSAA / duel columns because
# 10b's xMetrics join failed on a stale local table and only warned; the
# conservation check could not see a dropped column.
all_cols <- lapply(src, function(f) names(arrow::open_dataset(f)))
want <- Reduce(union, all_cols)
short <- vapply(all_cols, function(cc) paste(setdiff(want, cc), collapse = ", "), character(1))
if (any(nzchar(short))) stop("seasons missing columns other seasons have:\n",
                             paste(basename(src)[nzchar(short)], short[nzchar(short)], sep = ": ", collapse = "\n"))
# Net goals by play type (the player page's "where their EPV comes from"), one
# file per rebuilt season. Published WITH the game logs because the chart hides
# a player whose breakdown does not add up to the game logs it sits beside, so
# the two must come from the same build. Checked here as well as in 10b: every
# player-match with net_goals has a breakdown, the parts add up to the rounded
# net_goals (the game logs carry 4 dp), and nothing is missing either way.
bd_src <- file.path(C, sprintf("ng_breakdown_%s.parquet", SEASONS))
stopifnot(all(file.exists(bd_src)))
bd_age <- as.numeric(difftime(Sys.time(), file.mtime(bd_src), units = "hours"))
if (any(abs(bd_age - age_h) > 1)) stop("breakdown and game logs are from different builds: ",
                                        paste(basename(bd_src)[abs(bd_age - age_h) > 1], collapse = ", "))
for (i in seq_along(SEASONS)) {
  bd <- data.table::as.data.table(arrow::read_parquet(bd_src[i], col_select = c("match_id", "player_id", "value")))
  gl <- data.table::as.data.table(arrow::read_parquet(src[i], col_select = c("match_id", "player_id", "net_goals")))
  b <- bd[, .(b = sum(value)), by = .(match_id, player_id)]
  m <- merge(b, gl[!is.na(net_goals)], by = c("match_id", "player_id"), all = TRUE)
  if (!nrow(m)) stop(basename(bd_src[i]), " is empty")
  n_miss <- sum(is.na(m$b) | is.na(m$net_goals))
  gap <- max(abs(m$b - m$net_goals), na.rm = TRUE)
  if (n_miss > 0 || gap > 1e-4) stop(sprintf("%s: %d player-matches unmatched, worst gap %.2g", basename(bd_src[i]), n_miss, gap))
}
cat("breakdown check: all", length(SEASONS), "seasons add up to their game logs
")
# The player season totals the page reads: each player's total must equal the
# sum of their game-log net_goals for the season, and their games count the
# number of their game-log rows with net_goals.
pl_src <- file.path(C, sprintf("ng_player_breakdown_%s.parquet", SEASONS))
stopifnot(all(file.exists(pl_src)))
for (i in seq_along(SEASONS)) {
  pl <- unique(data.table::as.data.table(arrow::read_parquet(pl_src[i]))[, .(player_id, games, net_goals)])
  gl <- data.table::as.data.table(arrow::read_parquet(src[i], col_select = c("player_id", "net_goals")))[
    !is.na(net_goals), .(n = .N, t = sum(net_goals)), by = player_id]
  m <- merge(pl, gl, by = "player_id", all = TRUE)
  bad <- m[is.na(games) | is.na(n) | games != n | abs(net_goals - t) > 1e-3 * pmax(n, 1)]
  if (!nrow(m) || nrow(bad)) stop(sprintf("%s: %d of %d players do not match their game logs",
                                          basename(pl_src[i]), nrow(bad), nrow(m)))
}
cat("player totals check: all", length(SEASONS), "seasons match their game logs
")
stage <- file.path(tempdir(), "gl-pack"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, basename(src)); stopifnot(all(file.copy(src, paths, overwrite = TRUE)))
alias <- file.path(stage, "game_logs.parquet"); stopifnot(file.copy(src[length(src)], alias, overwrite = TRUE))
paths <- c(paths, alias)
bd_paths <- file.path(stage, basename(bd_src)); stopifnot(all(file.copy(bd_src, bd_paths, overwrite = TRUE)))
paths <- c(paths, bd_paths)
pl_paths <- file.path(stage, basename(pl_src)); stopifnot(all(file.copy(pl_src, pl_paths, overwrite = TRUE)))
paths <- c(paths, pl_paths)
rows <- setNames(vapply(paths, function(p) nrow(arrow::open_dataset(p)), numeric(1)), basename(paths))
print(data.frame(asset = names(rows), rows = unname(rows), md5 = unname(tools::md5sum(paths))))
dry <- !identical(Sys.getenv("PACK_PUBLISH"), "1")
vb_publish(paths, repo = "peteowen1/pannadata", tag = "blog-latest", rows = as.list(rows),
           carry_forward = TRUE, min_row_frac = 0.9, dry_run = dry)
if (dry) cat("\nDRY RUN: nothing uploaded. Set PACK_PUBLISH=1 to publish.\n")
