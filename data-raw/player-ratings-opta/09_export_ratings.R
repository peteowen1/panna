# 09_export_ratings.R
# Upload seasonal ratings to pannadata GitHub Releases for blog consumption
#
# Reads cached seasonal ratings from step 07 and uploads as parquet to the
# ratings-data release on peteowen1/pannadata. Downstream, build-blog-data.yml
# picks these up to produce panna_ratings.parquet for inthegame.

# 1. Setup ----

if (!requireNamespace("piggyback", quietly = TRUE)) {
  stop("Package 'piggyback' is required for export. Install with: install.packages('piggyback')")
}
if (!requireNamespace("arrow", quietly = TRUE)) {
  stop("Package 'arrow' is required for export. Install with: install.packages('arrow')")
}

# 2. Load Cached Ratings ----

ratings_file <- file.path(cache_dir, "07_seasonal_ratings.rds")
if (!file.exists(ratings_file)) {
  stop("No seasonal ratings cache found - run step 7 first")
}

seasonal_results <- readRDS(ratings_file)

if (is.null(seasonal_results$seasonal_xrapm) || nrow(seasonal_results$seasonal_xrapm) == 0) {
  stop("seasonal_xrapm is empty or NULL - cannot export. Check step 7 output.")
}

repo <- "peteowen1/pannadata"
tag <- "ratings-data"

# 3. Ensure Release Exists ----

release_ok <- tryCatch({
  piggyback::pb_list(repo = repo, tag = tag)
  TRUE
}, error = function(e) {
  if (grepl("not found|404|No GitHub release", e$message, ignore.case = TRUE)) {
    FALSE
  } else {
    stop(sprintf("Failed to check release '%s' on %s: %s", tag, repo, e$message))
  }
})

if (!release_ok) {
  message("Creating ratings-data release on pannadata...")
  piggyback::pb_new_release(repo = repo, tag = tag)
  Sys.sleep(3)
}

# 4. Upload files ----

upload_failures <- character(0)

# Upload seasonal_xrapm.parquet
tf_xrapm <- tempfile(fileext = ".parquet")
arrow::write_parquet(seasonal_results$seasonal_xrapm, tf_xrapm)
tryCatch({
  piggyback::pb_upload(tf_xrapm, repo = repo, tag = tag,
                       name = "seasonal_xrapm.parquet", overwrite = TRUE)
  message(sprintf("Uploaded seasonal_xrapm.parquet (%d rows)",
                  nrow(seasonal_results$seasonal_xrapm)))
}, error = function(e) {
  upload_failures <<- c(upload_failures, "seasonal_xrapm.parquet")
  warning(sprintf("Upload of seasonal_xrapm.parquet failed: %s", e$message), call. = FALSE)
})
unlink(tf_xrapm)

# Upload seasonal_spm.parquet
if (!is.null(seasonal_results$seasonal_spm)) {
  tf_spm <- tempfile(fileext = ".parquet")
  arrow::write_parquet(seasonal_results$seasonal_spm, tf_spm)
  tryCatch({
    piggyback::pb_upload(tf_spm, repo = repo, tag = tag,
                         name = "seasonal_spm.parquet", overwrite = TRUE)
    message(sprintf("Uploaded seasonal_spm.parquet (%d rows)",
                    nrow(seasonal_results$seasonal_spm)))
  }, error = function(e) {
    upload_failures <<- c(upload_failures, "seasonal_spm.parquet")
    warning(sprintf("Upload of seasonal_spm.parquet failed: %s", e$message), call. = FALSE)
  })
  unlink(tf_spm)
} else {
  warning("seasonal_spm not found in cache - re-run step 7 to generate both rating types")
}

if (length(upload_failures) > 0) {
  stop(sprintf("Failed to upload %d file(s): %s",
               length(upload_failures), paste(upload_failures, collapse = ", ")))
}
