# Model-pack step 9: publish the twelve rebuilt game-log seasons to pannadata@blog-latest.
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
for (f in src) {
  cols <- names(arrow::open_dataset(f))
  if (!all(c("net_goals", "ng_recon") %in% cols)) stop(f, " lacks net_goals / ng_recon")
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
stage <- file.path(tempdir(), "gl-pack"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, basename(src)); stopifnot(all(file.copy(src, paths, overwrite = TRUE)))
alias <- file.path(stage, "game_logs.parquet"); stopifnot(file.copy(src[length(src)], alias, overwrite = TRUE))
paths <- c(paths, alias)
rows <- setNames(vapply(paths, function(p) nrow(arrow::open_dataset(p)), numeric(1)), basename(paths))
print(data.frame(asset = names(rows), rows = unname(rows), md5 = unname(tools::md5sum(paths))))
dry <- !identical(Sys.getenv("PACK_PUBLISH"), "1")
vb_publish(paths, repo = "peteowen1/pannadata", tag = "blog-latest", rows = as.list(rows),
           carry_forward = TRUE, min_row_frac = 0.9, dry_run = dry)
if (dry) cat("\nDRY RUN: nothing uploaded. Set PACK_PUBLISH=1 to publish.\n")
