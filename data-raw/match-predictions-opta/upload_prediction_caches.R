# upload_prediction_caches.R
# Upload prediction pipeline caches to GitHub Releases
#
# Uploads pre-computed RAPM/Skills cache files to the predictions-cache
# release on peteowen1/pannadata. Run this manually after each RAPM or
# Skills pipeline run so the weekly GHA predictions workflow has fresh caches.

# 1. Configuration ----

repo <- "peteowen1/pannadata"
tag <- "predictions-cache"

cache_files <- list(
  list(
    path = file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds"),
    desc = "Opta RAPM seasonal ratings"
  ),
  list(
    path = file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds"),
    desc = "Skill-based seasonal ratings"
  ),
  list(
    path = file.path("data-raw", "cache-skills", "01_match_stats.rds"),
    desc = "Match stats for live skill estimation"
  ),
  list(
    path = file.path("data-raw", "cache-skills", "02b_decay_params.rds"),
    desc = "Decay params for skill estimation"
  ),
  list(
    path = file.path("data-raw", "cache-skills", "03_skill_spm.rds"),
    desc = "SPM model for skill-based predictions"
  )
)

# 2. Validate ----

message("\n=== Uploading Prediction Caches to GitHub ===\n")

# Check gh CLI
gh_check <- tryCatch(
  system2("gh", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(gh_check)) {
  stop("'gh' CLI is not installed or not on PATH. Install from https://cli.github.com/")
}

# Check which files exist
existing <- vapply(cache_files, function(f) file.exists(f$path), logical(1))
if (!any(existing)) {
  stop("No cache files found. Run the RAPM and/or Skills pipelines first.")
}

for (i in seq_along(cache_files)) {
  f <- cache_files[[i]]
  status <- if (existing[i]) {
    size_mb <- round(file.size(f$path) / (1024 * 1024), 2)
    sprintf("OK (%.1f MB)", size_mb)
  } else {
    "MISSING"
  }
  message(sprintf("  %-50s %s", f$desc, status))
}

if (!all(existing)) {
  missing_descs <- vapply(cache_files[!existing], `[[`, character(1), "desc")
  warning(sprintf("Missing files will be skipped: %s",
                  paste(missing_descs, collapse = ", ")),
          call. = FALSE, immediate. = TRUE)
}

# 3. Ensure Release Exists ----

message(sprintf("\n  Checking release '%s' on %s...", tag, repo))

release_check <- system2(
  "gh", c("release", "view", tag, "--repo", repo),
  stdout = TRUE, stderr = TRUE
)
release_status <- attr(release_check, "status")

if (!is.null(release_status) && release_status != 0) {
  stderr_text <- paste(release_check, collapse = "\n")
  if (grepl("auth|login|credential|forbidden|401|403", stderr_text, ignore.case = TRUE)) {
    stop(sprintf("gh authentication error: %s\nRun 'gh auth login' first.", stderr_text))
  }

  message("  Release not found. Creating...")
  create_result <- system2(
    "gh", c("release", "create", tag,
            "--repo", repo,
            "--title", shQuote("Prediction Pipeline Caches"),
            "--notes", shQuote("Pre-computed RAPM and Skills caches for the weekly predictions GHA workflow.")),
    stdout = TRUE, stderr = TRUE
  )
  create_status <- attr(create_result, "status")
  if (!is.null(create_status) && create_status != 0) {
    stop(sprintf("Failed to create release '%s': %s",
                 tag, paste(create_result, collapse = "\n")))
  }
}

# 4. Upload ----

uploaded <- 0L
for (i in seq_along(cache_files)) {
  if (!existing[i]) next
  f <- cache_files[[i]]
  fname <- basename(f$path)
  size_mb <- round(file.size(f$path) / (1024 * 1024), 2)
  message(sprintf("  Uploading %s (%.1f MB)...", fname, size_mb))

  result <- system2(
    "gh", c("release", "upload", tag, shQuote(f$path),
            "--repo", repo, "--clobber"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    warning(sprintf("Failed to upload %s: %s", fname, paste(result, collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
  } else {
    uploaded <- uploaded + 1L
  }
}

# 5. Summary ----

message("\n========================================")
message("Cache upload complete!")
message("========================================")
message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
message(sprintf("  Uploaded: %d/%d files", uploaded, sum(existing)))
