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

if (is.null(seasonal_results$seasonal_rapm) || nrow(seasonal_results$seasonal_rapm) == 0) {
  stop("seasonal_rapm is empty or NULL - cannot export raw RAPM. Check step 7 output.")
}

rapm_file <- file.path(cache_dir, "04_rapm.rds")
if (!file.exists(rapm_file)) {
  stop("No pooled RAPM cache found (04_rapm.rds) - run step 4 first")
}
rapm_results <- readRDS(rapm_file)
if (is.null(rapm_results$ratings) || nrow(rapm_results$ratings) == 0) {
  stop("04_rapm.rds ratings is empty or NULL - cannot export pooled raw RAPM. Check step 4 output.")
}

repo <- "peteowen1/pannadata"
tag <- "ratings-data"

# 2b. Build Raw (Prior-Free) RAPM Exports ----
#
# panna#165 (Pete's transparency call): raw prior-free RAPM published
# alongside the shrunk xRAPM, clearly labelled so readers can see the
# un-shrunk signal the whole rating family is built on. Raw RAPM is noisy
# for low-minute players BY DESIGN -- that's the transparency point -- so no
# minimum-minutes filter is applied here; total_minutes rides along so
# consumers can filter sensibly themselves.
#
# Two grains, two new files (mirrors the existing seasonal_xrapm/seasonal_spm
# +  career_panna.parquet precedent of one file per grain rather than
# cramming career-level rows into the per-season table):
#   - seasonal_rapm_raw.parquet: one row per (player, season_end_year), from
#     seasonal_rapm (07_seasonal_ratings.rds -- the per-season base RAPM fit,
#     already cached alongside seasonal_xrapm/seasonal_spm).
#   - pooled_rapm_raw.parquet: one row per player, from the pooled
#     all-history base RAPM fit (04_rapm.rds$ratings -- a single ridge fit
#     over every splint, no season split, no SPM prior).
#
# Export-boundary conventions mirror 10_export_blog_data.R exactly: drop the
# synthetic player_id == "replacement" row (rapm_matrix.R's <200-min pool --
# a model artifact, not a coherent player rating). defense is positive=good
# since 2026-09-03 (extract_rapm_ratings() negates at extraction time), so no
# export-boundary flip is needed here any more -- df$defense already IS
# rapm_raw_defense. Column names are rapm_raw/rapm_raw_offense/
# rapm_raw_defense so they can't be confused with xRAPM (`xrapm`) or the
# career trait (`panna`).
.drop_replacement_row <- function(df) {
  df[!(df$player_id %in% "replacement" | df$player_name %in% "Replacement Level"), , drop = FALSE]
}

.raw_rapm_export <- function(df, extra_cols = character(0)) {
  df <- .drop_replacement_row(df)
  out <- data.frame(
    player_id = df$player_id,
    player_name = df$player_name,
    rapm_raw = round(df$rapm, 4),
    rapm_raw_offense = round(df$offense, 4),
    rapm_raw_defense = round(df$defense, 4),
    total_minutes = df$total_minutes,
    stringsAsFactors = FALSE
  )
  if (length(extra_cols) > 0) {
    out <- cbind(df[, extra_cols, drop = FALSE], out)
  }
  out
}

seasonal_rapm_raw <- .raw_rapm_export(seasonal_results$seasonal_rapm, extra_cols = "season_end_year")
pooled_rapm_raw <- .raw_rapm_export(rapm_results$ratings)

message(sprintf("  Raw RAPM (seasonal): %d player-seasons (replacement row dropped)",
                nrow(seasonal_rapm_raw)))
message(sprintf("  Raw RAPM (pooled all-history): %d players (replacement row dropped)",
                nrow(pooled_rapm_raw)))

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
tf_dir     <- tempfile("ratings_export_")
dir.create(tf_dir)
tf_xrapm   <- file.path(tf_dir, "seasonal_xrapm.parquet")
tf_spm     <- file.path(tf_dir, "seasonal_spm.parquet")
tf_rapm    <- file.path(tf_dir, "seasonal_rapm_raw.parquet")
tf_pooled  <- file.path(tf_dir, "pooled_rapm_raw.parquet")
arrow::write_parquet(seasonal_results$seasonal_xrapm, tf_xrapm)
arrow::write_parquet(seasonal_results$seasonal_spm, tf_spm)
arrow::write_parquet(seasonal_rapm_raw, tf_rapm)
arrow::write_parquet(pooled_rapm_raw, tf_pooled)

# vb_publish hashes all four files, uploads them, verifies the live asset
# list, and only then writes bus_manifest.json -- any single upload failure
# aborts BEFORE the manifest, so ratings-data never advertises a
# version-skewed set (panna M-RATINGS-PAIR, extended to the raw RAPM pair
# added by panna#165 -- the raw and shrunk ratings should always advance
# together too).
manifest <- vb_publish(
  c(tf_xrapm, tf_spm, tf_rapm, tf_pooled),
  repo = repo, tag = tag,
  rows = c(
    seasonal_xrapm.parquet    = nrow(seasonal_results$seasonal_xrapm),
    seasonal_spm.parquet      = nrow(seasonal_results$seasonal_spm),
    seasonal_rapm_raw.parquet = nrow(seasonal_rapm_raw),
    pooled_rapm_raw.parquet   = nrow(pooled_rapm_raw)
  )
)
unlink(tf_dir, recursive = TRUE)

message(sprintf(
  "Uploaded seasonal_xrapm.parquet (%d rows) + seasonal_spm.parquet (%d rows) + seasonal_rapm_raw.parquet (%d rows) + pooled_rapm_raw.parquet (%d rows) — generation %s",
  nrow(seasonal_results$seasonal_xrapm), nrow(seasonal_results$seasonal_spm),
  nrow(seasonal_rapm_raw), nrow(pooled_rapm_raw),
  manifest$generation))
