# 10e_export_keeper_style.R
# Register the keeper style table for blog-latest (panna#452).
#
# The site's live PSV scorer prices keepers on five club-adjusted stats
# (`<stat>_kadj`, panna#429) and cannot fit the keeper-style model in the
# browser. Skills step 01c writes keeper-style.parquet (club style + in-season
# shift, built by the same code as the `_kadj` columns) next to
# keeper_style_asof.rds; this step only checks it is fresh and adds it to the
# single step 13 publish. pannadata build-blog-data passes it to R2 as
# football/keeper-style.parquet.
#
# Run via run_predictions_opta.R (publish_files lives there).

if (!exists("skills_cache_dir")) skills_cache_dir <- file.path("data-raw", "cache-skills")
ks_parquet <- file.path(skills_cache_dir, "keeper-style.parquet")
ks_csv <- sub("\\.parquet$", ".csv", ks_parquet)

if (!file.exists(ks_parquet) || !file.exists(ks_csv)) {
  stop(sprintf("%s (or its .csv) not found: skills step 01c writes it; run it before this step.", ks_parquet))
}
age_days <- as.numeric(difftime(Sys.time(), file.mtime(ks_parquet), units = "days"))
if (age_days > 2) {
  stop(sprintf("%s is %.1f days old; step 01c should have rebuilt it this run.", ks_parquet, age_days))
}
kst <- arrow::read_parquet(ks_parquet)
message(sprintf("keeper-style.parquet: %s rows, seasons %s, latest after_date %s",
                format(nrow(kst), big.mark = ","), paste(sort(unique(kst$season_end_year)), collapse = "/"),
                format(max(kst$after_date))))
need <- c("team_id", "season_end_year", "stat", "after_date", "club_style", "shift", "n90_before")
miss <- setdiff(need, names(kst))
if (length(miss)) stop(sprintf("keeper-style.parquet lacks %s", paste(miss, collapse = ", ")))
if (!nrow(kst) || anyNA(kst$club_style) || anyNA(kst$shift)) stop("keeper-style.parquet is empty or has NA style/shift values.")

if (exists("publish_files", envir = .GlobalEnv)) {
  publish_files$blog_latest <<- c(publish_files$blog_latest, ks_parquet, ks_csv)
}
