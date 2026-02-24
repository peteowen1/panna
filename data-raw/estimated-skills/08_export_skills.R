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
arrow::write_parquet(as.data.frame(skill_features), skills_path)
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
arrow::write_parquet(as.data.frame(match_stats), ms_path)
ms_mb <- round(file.info(ms_path)$size / (1024 * 1024), 1)
cat(sprintf("  Written: %s (%s MB)\n", ms_path, ms_mb))

# 5. Upload to GitHub Release ----

cat("\n=== Uploading to GitHub Release ===\n")

if (!requireNamespace("piggyback", quietly = TRUE)) {
  stop("Package 'piggyback' is required for upload.")
}

for (f in c(skills_path, ms_path)) {
  fname <- basename(f)
  tryCatch({
    piggyback::pb_upload(file = f, repo = repo, tag = tag, overwrite = TRUE)
    cat(sprintf("  Uploaded %s to %s (%s)\n", fname, repo, tag))
  }, error = function(e) {
    cat(sprintf("  WARNING: Upload of %s failed: %s\n", fname, e$message))
  })
}

cat("\n=== COMPLETE ===\n")
