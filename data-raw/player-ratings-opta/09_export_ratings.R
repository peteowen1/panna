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

repo <- "peteowen1/pannadata"
tag <- "ratings-data"

# 3. Ensure Release Exists ----

tryCatch(
  piggyback::pb_list(repo = repo, tag = tag),
  error = function(e) {
    message("Creating ratings-data release on pannadata...")
    piggyback::pb_new_release(repo = repo, tag = tag)
    Sys.sleep(3)
  }
)

# 4. Upload seasonal_xrapm.parquet ----

tf_xrapm <- tempfile(fileext = ".parquet")
arrow::write_parquet(seasonal_results$seasonal_xrapm, tf_xrapm)
piggyback::pb_upload(tf_xrapm, repo = repo, tag = tag,
                     name = "seasonal_xrapm.parquet")
message(sprintf("Uploaded seasonal_xrapm.parquet (%d rows)",
                nrow(seasonal_results$seasonal_xrapm)))
unlink(tf_xrapm)

# 5. Upload seasonal_spm.parquet ----

if (!is.null(seasonal_results$seasonal_spm)) {
  tf_spm <- tempfile(fileext = ".parquet")
  arrow::write_parquet(seasonal_results$seasonal_spm, tf_spm)
  piggyback::pb_upload(tf_spm, repo = repo, tag = tag,
                       name = "seasonal_spm.parquet")
  message(sprintf("Uploaded seasonal_spm.parquet (%d rows)",
                  nrow(seasonal_results$seasonal_spm)))
  unlink(tf_spm)
} else {
  warning("seasonal_spm not found in cache - re-run step 7 to generate both rating types")
}
