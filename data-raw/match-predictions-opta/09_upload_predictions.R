# 09_upload_predictions.R
# Validate match predictions output (LOCAL ONLY -- no upload here).
#
# Historically this step uploaded predictions.parquet/.csv to GitHub Releases
# directly via the gh CLI. Per ECOSYSTEM-FIX-PLAN.md PA5 (panna H-TORN), all
# release publishing for this pipeline now happens in ONE final gated step
# (13_publish_release_data.R) so predictions-latest and blog-latest either
# both advance together or neither does -- a mid-pipeline failure (e.g. the
# OOM-prone 10b export) can no longer leave predictions-latest ahead of a
# half-updated blog-latest. This step just validates step 07's output is
# present and sane before step 13 picks it up.

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")

parquet_path <- file.path(cache_dir, "predictions.parquet")
csv_path <- file.path(cache_dir, "predictions.csv")

# 2. Validate ----

message("\n=== Validating Predictions Output ===\n")

missing <- character(0)
if (!file.exists(parquet_path)) missing <- c(missing, "predictions.parquet")
if (!file.exists(csv_path)) missing <- c(missing, "predictions.csv")

if (length(missing) > 0) {
  stop(sprintf("Missing prediction files: %s\nRun step 07 first.",
               paste(missing, collapse = ", ")))
}

predictions <- arrow::read_parquet(parquet_path)
message(sprintf("  %d predictions across %d leagues",
                nrow(predictions), length(unique(predictions$league))))
message(sprintf("  Date range: %s to %s",
                min(predictions$match_date), max(predictions$match_date)))
message(sprintf("  Leagues: %s", paste(sort(unique(predictions$league)), collapse = ", ")))

if (exists("publish_files", envir = .GlobalEnv)) {
  publish_files$predictions_latest <<- c(publish_files$predictions_latest,
                                          parquet_path, csv_path)
} else {
  message("  (standalone run -- not registered for step-13 publish)")
}

message("\n========================================")
message("Predictions validated (publish deferred to step 13)")
message("========================================")
message(sprintf("  Matches: %d | Leagues: %d", nrow(predictions), length(unique(predictions$league))))
