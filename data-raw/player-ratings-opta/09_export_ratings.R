# 09_export_ratings.R
# Upload seasonal ratings to pannadata GitHub Releases for blog consumption
#
# Reads cached seasonal ratings from step 07 and uploads as parquet to the
# ratings-data release on peteowen1/pannadata. Downstream, build-blog-data.yml
# picks these up to produce panna_ratings.parquet for inthegame.

# Both xRAPM and SPM are uploaded together via vb_publish() (ECOSYSTEM-FIX-PLAN.md
# PA6 / panna M-RATINGS-PAIR): the old two-independent-pb_upload-calls version
# could publish a version-skewed pair on ratings-data if the second upload
# failed after the first succeeded. vb_publish hashes both files first,
# uploads them, and gates bus_manifest.json on BOTH succeeding -- either both
# land or neither does, and consumers keep seeing the last consistent pair.

# 1. Setup ----

# Attach panna for the bare vb_publish() call below. Every step script runs in
# its own callr child (run_step_opta), which inherits NOTHING from the
# orchestrator's load_all() — steps 01-08 all carry this same header. Missing
# here since the callr isolation landed (panna#87): step 9 died with
# 'could not find function "vb_publish"' on every cloud run after 2026-06-11,
# masked by the swallowed-error bug until 2026-07-16 fixed the reporting.
devtools::load_all()

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

# 4. Upload files (both-or-neither via vb_publish) ----

if (is.null(seasonal_results$seasonal_spm)) {
  stop("seasonal_spm not found in cache - re-run step 7 to generate both rating types")
}

# vb_publish() uploads each path under its OWN basename() (no rename param),
# so the temp files must already be named seasonal_xrapm.parquet /
# seasonal_spm.parquet -- a plain tempfile(fileext=".parquet") would upload
# under a random name instead of the one consumers expect.
tf_dir   <- tempfile("ratings_export_")
dir.create(tf_dir)
tf_xrapm <- file.path(tf_dir, "seasonal_xrapm.parquet")
tf_spm   <- file.path(tf_dir, "seasonal_spm.parquet")
arrow::write_parquet(seasonal_results$seasonal_xrapm, tf_xrapm)
arrow::write_parquet(seasonal_results$seasonal_spm, tf_spm)

# vb_publish hashes both files, uploads them, verifies the live asset list,
# and only then writes bus_manifest.json -- any single upload failure aborts
# BEFORE the manifest, so ratings-data never advertises a version-skewed
# xRAPM/SPM pair (panna M-RATINGS-PAIR).
manifest <- vb_publish(
  c(tf_xrapm, tf_spm),
  repo = repo, tag = tag,
  rows = c(
    seasonal_xrapm.parquet = nrow(seasonal_results$seasonal_xrapm),
    seasonal_spm.parquet   = nrow(seasonal_results$seasonal_spm)
  )
)
unlink(tf_dir, recursive = TRUE)

message(sprintf("Uploaded seasonal_xrapm.parquet (%d rows) + seasonal_spm.parquet (%d rows) — generation %s",
                nrow(seasonal_results$seasonal_xrapm), nrow(seasonal_results$seasonal_spm),
                manifest$generation))
