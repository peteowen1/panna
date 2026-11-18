# 08_export_skills.R
# Export pre-computed skill features and match stats as parquet for remote loading
#
# Exports two files to the opta-latest GitHub release:
#   - opta_skills.parquet (~4 MB): one row per player-season, Bayesian skill estimates
#   - opta_match_stats.parquet (~15 MB): one row per player-match, processed _p90 stats
#
# opta_skills.parquet powers the fast player_skill_profile() auto-load.
# opta_match_stats.parquet adds raw_avg/attempts/w90 diagnostic columns.

# 1. Setup ----

library(arrow)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
skills_rds <- file.path(cache_dir, "02_skill_features.rds")
match_stats_rds <- file.path(cache_dir, "01_match_stats.rds")

if (!file.exists(skills_rds)) {
  stop("Missing cache-skills/02_skill_features.rds. Run steps 01-02 first.")
}
if (!file.exists(match_stats_rds)) {
  stop("Missing cache-skills/01_match_stats.rds. Run step 01 first.")
}

repo <- "peteowen1/pannadata"
tag <- "opta-latest"
opta_dir <- file.path("..", "pannadata", "data", "opta")
dir.create(opta_dir, showWarnings = FALSE, recursive = TRUE)

# 3. Export Skills ----

cat("\n=== Exporting Skill Features ===\n")

skill_features <- readRDS(skills_rds)
cat(sprintf("  Rows: %s\n", format(nrow(skill_features), big.mark = ",")))
cat(sprintf("  Columns: %d\n", ncol(skill_features)))
cat(sprintf("  Unique players: %s\n",
            format(data.table::uniqueN(skill_features$player_id), big.mark = ",")))

skills_path <- file.path(opta_dir, "opta_skills.parquet")
arrow::write_parquet(.as_df_no_copy(skill_features), skills_path)
skills_mb <- round(file.info(skills_path)$size / (1024 * 1024), 1)
cat(sprintf("  Written: %s (%s MB)\n", skills_path, skills_mb))

# 4. Export Match Stats ----

cat("\n=== Exporting Match Stats ===\n")

match_stats <- readRDS(match_stats_rds)
cat(sprintf("  Rows: %s\n", format(nrow(match_stats), big.mark = ",")))
cat(sprintf("  Columns: %d\n", ncol(match_stats)))
cat(sprintf("  Unique players: %s\n",
            format(data.table::uniqueN(match_stats$player_id), big.mark = ",")))

ms_path <- file.path(opta_dir, "opta_match_stats.parquet")
# .as_df_no_copy(), not as.data.frame(): the latter deep-copies all 422
# columns and doubled this step's peak to 13.7 GB on GHA (run 38860800656).
.log_rss("08: before writing match_stats")
arrow::write_parquet(.as_df_no_copy(match_stats), ms_path)
.log_rss("08: after writing match_stats")
ms_mb <- round(file.info(ms_path)$size / (1024 * 1024), 1)
cat(sprintf("  Written: %s (%s MB)\n", ms_path, ms_mb))

# 5. Upload to GitHub Release ----

# Upload toggle (added 2026-09-04 - this step previously published
# UNCONDITIONALLY, no flag at all, which caused a real unauthorized publish
# incident). Set upload_skills <- FALSE before sourcing to write the two
# parquets locally (already done above, under opta_dir) without publishing.
# Default TRUE preserves the scheduled-workflow behaviour.
if (!exists("upload_skills")) upload_skills <- TRUE
# R026: CI sets PANNA_PUBLISH_DISABLED on any ref other than main/dev.
if (tolower(Sys.getenv("PANNA_PUBLISH_DISABLED", "")) %in% c("true", "1")) {
  cat("PANNA_PUBLISH_DISABLED is set -- forcing upload_skills = FALSE\n")
  upload_skills <- FALSE
}

if (!isTRUE(upload_skills)) {

  cat(sprintf("\nupload_skills = FALSE -- wrote %s and %s locally, NOT publishing.\n",
              skills_path, ms_path))

} else {

# R064: refuse to publish a table that would regress what is live. Runs before
# any upload; reads the currently published assets (skipped, with a message, if
# they cannot be read). See R/skills_export_guard.R.
cat("\n=== Pre-publish checks ===\n")
# Only the columns the guard reads from the published copy (row count,
# season_end_year, league): reading all 422 match-stats columns OOM-killed
# this step on GHA (run 38836860556).
guard_cols <- c("season_end_year", "league")
.log_rss("08: before pre-publish checks")
assert_skills_export_ok(skill_features,
                        read_published_asset("opta_skills.parquet", columns = guard_cols),
                        label = "opta_skills")
assert_skills_export_ok(match_stats,
                        read_published_asset("opta_match_stats.parquet", columns = guard_cols),
                        label = "opta_match_stats")
.log_rss("08: after pre-publish checks")

cat("\n=== Uploading to GitHub Release ===\n")

# vb_publish() (R/versebus.R), not pb_upload(overwrite = TRUE): an overwrite deletes the live
# asset before the new one lands, so a failed upload leaves the release without it. vb_publish
# keeps the old asset until the new one is verified, and records both in the bus manifest.
manifest <- tryCatch(
  vb_publish(c(skills_path, ms_path), repo = repo, tag = tag,
             rows = stats::setNames(c(nrow(skill_features), nrow(match_stats)),
                                    basename(c(skills_path, ms_path)))),
  error = function(e) stop(sprintf("Upload failed: %s. Skills are NOT published.", conditionMessage(e)),
                           call. = FALSE))
cat(sprintf("  Uploaded %s to %s (%s), generation %s\n",
            paste(basename(c(skills_path, ms_path)), collapse = " + "), repo, tag, manifest$generation))

}

cat("\n=== COMPLETE ===\n")
