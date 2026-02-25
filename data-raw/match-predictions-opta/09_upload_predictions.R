# 09_upload_predictions.R
# Upload match predictions to GitHub Releases
#
# Uploads predictions.parquet and predictions.csv to the
# predictions-latest release on peteowen1/pannadata using gh CLI.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
repo <- "peteowen1/pannadata"
tag <- "predictions-latest"

parquet_path <- file.path(cache_dir, "predictions.parquet")
csv_path <- file.path(cache_dir, "predictions.csv")

# 3. Validate ----

message("\n=== Uploading Predictions to GitHub ===\n")

missing <- character(0)
if (!file.exists(parquet_path)) missing <- c(missing, "predictions.parquet")
if (!file.exists(csv_path)) missing <- c(missing, "predictions.csv")

if (length(missing) > 0) {
  stop(sprintf("Missing prediction files: %s\nRun step 07 first.",
               paste(missing, collapse = ", ")))
}

# Quick summary of what we're uploading
predictions <- arrow::read_parquet(parquet_path)
message(sprintf("  %d predictions across %d leagues",
                nrow(predictions), length(unique(predictions$league))))
message(sprintf("  Date range: %s to %s",
                min(predictions$match_date), max(predictions$match_date)))
message(sprintf("  Leagues: %s", paste(sort(unique(predictions$league)), collapse = ", ")))

# 4. Ensure Release Exists ----

message(sprintf("\n  Checking release '%s' on %s...", tag, repo))

release_exists <- system2(
  "gh", c("release", "view", tag, "--repo", repo),
  stdout = FALSE, stderr = FALSE
)

if (release_exists != 0) {
  message("  Creating release...")
  system2("gh", c("release", "create", tag,
                   "--repo", repo,
                   "--title", shQuote("Match Predictions (Latest)"),
                   "--notes", shQuote("Auto-generated predictions from the Opta match prediction pipeline.")),
          stdout = TRUE, stderr = TRUE)
}

# 5. Upload ----

for (fpath in c(parquet_path, csv_path)) {
  fname <- basename(fpath)
  size_mb <- round(file.size(fpath) / (1024 * 1024), 2)
  message(sprintf("  Uploading %s (%.2f MB)...", fname, size_mb))
  result <- system2(
    "gh", c("release", "upload", tag, shQuote(fpath),
            "--repo", repo, "--clobber"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(sprintf("Failed to upload %s: %s", fname, paste(result, collapse = "\n")))
  }
}

# 6. Summary ----

message("\n========================================")
message("Predictions uploaded successfully!")
message("========================================")
message(sprintf("  Release: %s/%s/releases/tag/%s", "https://github.com", repo, tag))
message(sprintf("  Files: predictions.parquet, predictions.csv"))
message(sprintf("  Matches: %d | Leagues: %d", nrow(predictions), length(unique(predictions$league))))
