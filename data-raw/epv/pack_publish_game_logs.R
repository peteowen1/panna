# Publish rebuilt game-log seasons, and their net goals breakdowns, to pannadata@blog-latest.
# =============================================================================
# Originally the twelve 2015-27 seasons built by _run_10b_pack_2026_09.R (2026-09-24).
# Since panna#465 (2026-11) the input is the 23-season game-logs-rebuild.yml run,
# joined per season by its part artifacts into one directory (GL_PACK_DIR). Every
# season must already have passed check_game_logs_coverage.R against blog-latest.
# game_logs.parquet is the current-season alias the blog reads, so it gets the
# newest season's file.
#
# Two kinds of season are allowed to differ from the rest, and nothing else:
#   * Before 2013-14 there is no PSV (no xMetrics; DECISIONS 2026-11-10/14), and
#     10b writes no PSV columns at all. They are added here as all-NA columns, and
#     the log says so, so every published season has the same columns.
#   * A season with no net goals at all (2007-08: no match had them) has no
#     breakdown files and no net-goals / WPA-ledger columns (NG_COLS). Those are
#     added as all-NA and it is published without breakdowns; a breakdown file
#     for such a season stops the run as a wrong build.
#
# Rollback: carry_forward keeps every asset this run does not replace; the
# replaced ones come back by re-running 10b with the old models, or from the
# previous blog-latest generation's backups.
#
# Run from panna/:  Rscript data-raw/epv/pack_publish_game_logs.R            (dry run)
#                   PACK_PUBLISH=1 Rscript data-raw/epv/pack_publish_game_logs.R  (publish)
# Env:  GL_PACK_DIR  directory holding game_logs_/ng_breakdown_/ng_player_breakdown_<season>.parquet
#                    (default data-raw/cache-predictions-opta)
#       GL_SEASONS   space-separated season labels (default: the 23 published seasons)
#       GL_NO_NG_SEASONS  seasons allowed to have no net goals (default "2007-2008")
suppressMessages(devtools::load_all(quiet = TRUE))
C <- Sys.getenv("GL_PACK_DIR", "data-raw/cache-predictions-opta")
SEASONS <- strsplit(trimws(Sys.getenv("GL_SEASONS", paste(
  "1999-2000 2001-2002 2003-2004 2005-2006 2007-2008 2009-2010 2010-2011 2011-2012",
  "2012-2013 2013-2014 2014-2015 2015-2016 2016-2017 2017-2018 2018-2019 2019-2020",
  "2020-2021 2021-2022 2022-2023 2023-2024 2024-2025 2025-2026 2026-2027"))), "\\s+")[[1]]
SEASONS <- sort(unique(SEASONS))  # the alias below takes the LAST, i.e. newest, season
cat(sprintf("Packing %d season(s) from %s: %s\n", length(SEASONS), C, paste(SEASONS, collapse = " ")))
src <- file.path(C, sprintf("game_logs_%s.parquet", SEASONS))
if (!all(file.exists(src))) stop("missing game logs: ", paste(basename(src)[!file.exists(src)], collapse = ", "))
age_h <- as.numeric(difftime(Sys.time(), file.mtime(src), units = "hours"))
if (any(age_h > 48)) stop("a season file is older than 48 h -- not from today's rebuild: ", paste(src[age_h > 48], collapse = ", "))

PSV_COLS <- c("psv", "osv", "dsv", "psv_league_offset", "pos_grp")   # pos_grp comes out of PSV scoring
pre_psv <- extract_season_end_year(SEASONS) < 2014L   # 2013-14 has PSV; before it, none by design
# spm_overall is the raw-stat seasonal SPM (cache-opta/07), whose first season end
# year is 2013: none exists before 2012-13. From 2012-13 on a missing column stops.
pre_spm <- extract_season_end_year(SEASONS) < 2013L
# Net goals and the WPA net ledger built on them. A season where no match has net
# goals (2007-08) gets none of these from 10b, the published copy included.
NG_COLS <- c("net_goals", "ng_offensive", "ng_defensive", "ng_recon", "wpa_neutral", "wpa_neutral_own",
             "wpa_neutral_won", "wpa_neutral_shared", "wpa_team", "wpa_team_own", "wpa_team_won",
             "wpa_team_shared", "wpa_team_start")
n_ng <- vapply(src, function(f) {
  if (!"net_goals" %in% names(arrow::open_dataset(f))) return(0)
  x <- arrow::read_parquet(f, col_select = "net_goals")$net_goals; sum(!is.na(x))
}, numeric(1))
has_bd <- n_ng > 0
# Which seasons may have no net goals is a FIXED list, not read off the data: 10b
# drops the net-goals columns on a reconcile error (it logs "net goals DROPPED"),
# and a season that lost them that way must stop here, not publish without them.
NO_NG_SEASONS <- strsplit(trimws(Sys.getenv("GL_NO_NG_SEASONS", "2007-2008")), "\\s+")[[1]]
if (any(!has_bd & !SEASONS %in% NO_NG_SEASONS))
  stop("season(s) with no net goals that are not on the no-net-goals list (", paste(NO_NG_SEASONS, collapse = ", "),
       "): ", paste(SEASONS[!has_bd & !SEASONS %in% NO_NG_SEASONS], collapse = ", "), " -- check 10b's log for 'net goals DROPPED'")
if (any(has_bd & SEASONS %in% NO_NG_SEASONS))
  stop("season(s) on the no-net-goals list now HAVE net goals -- update GL_NO_NG_SEASONS: ",
       paste(SEASONS[has_bd & SEASONS %in% NO_NG_SEASONS], collapse = ", "))
if (any(!has_bd)) cat(sprintf("NO NET GOALS (on the list; absent or NA on every row, so no breakdown to publish): %s\n",
                              paste(SEASONS[!has_bd], collapse = ", ")))
# Columns a season is allowed to lack; added as all-NA when staged.
fill_cols <- lapply(seq_along(SEASONS), function(i) c(if (pre_psv[i]) PSV_COLS, if (pre_spm[i]) "spm_overall",
                                                       if (!has_bd[i]) NG_COLS))
CHAR_COLS <- "pos_grp"   # filled as NA_character_; every other filled column is numeric
# A FIXED list every season must carry, not only "what the other seasons have":
# a systemic failure strips every season alike, which a cross-season comparison
# cannot see (review finding, 2026-09-24). PSV is checked only from 2013-14 on.
REQUIRED <- c("net_goals", "ng_recon", "epv_total", "psv",
              "goals_minus_xgot", "placement_added", "xgot", "gsaa", "gsaa_per90", "xgot_faced",
              "goals_conceded", "aerial_woe_per90", "aerial_poss_woe_per90", "takeon_woe_per90",
              "tackle_poss_woe_per90", "containment_woe_per90")
all_cols <- lapply(src, function(f) names(arrow::open_dataset(f)))
for (i in seq_along(src)) {
  miss <- setdiff(setdiff(REQUIRED, fill_cols[[i]]), all_cols[[i]])
  if (length(miss)) stop(basename(src[i]), " lacks required columns: ", paste(miss, collapse = ", "))
  all_cols[[i]] <- union(all_cols[[i]], fill_cols[[i]])  # added as NA when staged
}
# Every season must carry every column any season carries. 2026-09-24: the
# 2026-27 file was published without its 12 xGOT / GSAA / duel columns because
# 10b's xMetrics join failed on a stale local table and only warned; the
# conservation check could not see a dropped column.
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
bd_src <- file.path(C, sprintf("ng_breakdown_%s.parquet", SEASONS))[has_bd]
pl_src <- file.path(C, sprintf("ng_player_breakdown_%s.parquet", SEASONS))[has_bd]
stray <- file.exists(file.path(C, sprintf("ng_breakdown_%s.parquet", SEASONS))[!has_bd])
if (any(stray)) stop("a season with no net_goals has a breakdown file -- wrong build? ", paste(SEASONS[!has_bd][stray], collapse = ", "))
if (!all(file.exists(bd_src))) stop("missing breakdowns: ", paste(basename(bd_src)[!file.exists(bd_src)], collapse = ", "))
bd_age <- as.numeric(difftime(Sys.time(), file.mtime(bd_src), units = "hours"))
if (any(abs(bd_age - age_h[has_bd]) > 1)) stop("breakdown and game logs are from different builds: ",
                                               paste(basename(bd_src)[abs(bd_age - age_h[has_bd]) > 1], collapse = ", "))
for (i in seq_along(bd_src)) {
  bd <- data.table::as.data.table(arrow::read_parquet(bd_src[i], col_select = c("match_id", "player_id", "value")))
  gl <- data.table::as.data.table(arrow::read_parquet(src[has_bd][i], col_select = c("match_id", "player_id", "net_goals")))
  b <- bd[, .(b = sum(value)), by = .(match_id, player_id)]
  m <- merge(b, gl[!is.na(net_goals)], by = c("match_id", "player_id"), all = TRUE)
  if (!nrow(m)) stop(basename(bd_src[i]), " is empty")
  n_miss <- sum(is.na(m$b) | is.na(m$net_goals))
  gap <- max(abs(m$b - m$net_goals), na.rm = TRUE)
  if (n_miss > 0 || gap > 1e-4) stop(sprintf("%s: %d player-matches unmatched, worst gap %.2g", basename(bd_src[i]), n_miss, gap))
}
cat("breakdown check:", length(bd_src), "season(s) add up to their game logs\n")
# The player season totals the page reads: each player's total must equal the
# sum of their game-log net_goals for the season, and their games count the
# number of their game-log rows with net_goals.
if (!all(file.exists(pl_src))) stop("missing player breakdowns: ", paste(basename(pl_src)[!file.exists(pl_src)], collapse = ", "))
for (i in seq_along(pl_src)) {
  pl <- unique(data.table::as.data.table(arrow::read_parquet(pl_src[i]))[, .(player_id, games, net_goals)])
  gl <- data.table::as.data.table(arrow::read_parquet(src[has_bd][i], col_select = c("player_id", "net_goals")))[
    !is.na(net_goals), .(n = .N, t = sum(net_goals)), by = player_id]
  m <- merge(pl, gl, by = "player_id", all = TRUE)
  bad <- m[is.na(games) | is.na(n) | games != n | abs(net_goals - t) > 1e-3 * pmax(n, 1)]
  if (!nrow(m) || nrow(bad)) stop(sprintf("%s: %d of %d players do not match their game logs",
                                          basename(pl_src[i]), nrow(bad), nrow(m)))
}
cat("player totals check:", length(pl_src), "season(s) match their game logs\n")

stage <- file.path(tempdir(), "gl-pack"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, basename(src))
for (i in seq_along(src)) {
  add <- setdiff(fill_cols[[i]], names(arrow::open_dataset(src[i])))
  if (length(add)) {
    # No PSV before 2013-14 / no net goals in that season, by design: write the
    # columns as NA so the blog sees one schema. Fabricated columns, all NA, said so.
    gl <- data.table::as.data.table(arrow::read_parquet(src[i]))
    for (cc in add) data.table::set(gl, j = cc, value = if (cc %in% CHAR_COLS) NA_character_ else NA_real_)
    arrow::write_parquet(gl, paths[i])
    cat(sprintf("  %s: added %d all-NA column(s): %s\n", basename(src[i]), length(add), paste(add, collapse = ", ")))
  } else {
    stopifnot(file.copy(src[i], paths[i], overwrite = TRUE))
  }
}
alias <- file.path(stage, "game_logs.parquet"); stopifnot(file.copy(paths[length(paths)], alias, overwrite = TRUE))
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
