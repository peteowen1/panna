# bootstrap.R
# One-command setup for a fresh pannaverse clone
#
# Downloads all data, models, and caches needed to run any pipeline locally.
#
# Usage:
#   cd panna
#   Rscript data-raw/bootstrap.R            # Download everything
#   Rscript data-raw/bootstrap.R opta       # Opta data + models only
#   Rscript data-raw/bootstrap.R models     # Models only
#   Rscript data-raw/bootstrap.R caches     # Prediction caches only
#
# Prerequisites:
#   - gh CLI authenticated (gh auth login)
#   - piggyback R package installed

devtools::load_all()

args <- commandArgs(trailingOnly = TRUE)
what <- if (length(args) > 0) args[1] else "all"

valid_opts <- c("all", "opta", "models", "caches")
if (!what %in% valid_opts) {
  stop(sprintf("Invalid option '%s'. Choose from: %s", what, paste(valid_opts, collapse = ", ")))
}

pannadata_path <- file.path("..", "pannadata")
models_dir <- file.path(pannadata_path, "data", "opta", "models")
cache_opta <- file.path("data-raw", "cache-opta")
cache_skills <- file.path("data-raw", "cache-skills")

message("\n")
message(paste(rep("=", 60), collapse = ""))
message("  PANNA BOOTSTRAP")
message(sprintf("  Mode: %s", what))
message(paste(rep("=", 60), collapse = ""))

# ---- Step 1: Download Opta data ----
if (what %in% c("all", "opta")) {
  message("\n[1/3] Downloading Opta data from GitHub Releases...")
  tryCatch({
    # pb_download_source("opta") requires an "opta-parquet.tar.gz" archive
    # asset that no longer exists on opta-latest (individual consolidated
    # parquets only) -- pb_download_opta() is the incremental sync and works
    # from an empty directory too, so it doubles as the fresh-clone puller.
    pb_download_opta(dest = file.path(pannadata_path, "data", "opta"))
    message("  OK: Opta data downloaded")
  }, error = function(e) {
    message(sprintf("  WARN: Failed to download Opta data: %s", e$message))
    message("  You can retry with: panna::pb_download_opta()")
  })
}

# ---- Step 2: Download pre-trained models ----
if (what %in% c("all", "opta", "models")) {
  message("\n[2/3] Downloading pre-trained models (xG, xPass, EPV)...")
  dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

  model_files <- c("xg_model.rds", "xpass_model.rds", "epv_model.rds")
  failed_models <- character(0)
  for (mf in model_files) {
    dest <- file.path(models_dir, mf)
    if (file.exists(dest)) {
      message(sprintf("  SKIP: %s already exists", mf))
      next
    }
    tryCatch({
      tmp <- .pb_download_file(mf, repo = "peteowen1/pannadata",
                                tag = "epv-models",
                                label = paste("model:", mf))
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
      message(sprintf("  OK: %s", mf))
    }, error = function(e) {
      failed_models <<- c(failed_models, mf)
      message(sprintf("  FAIL: %s — %s", mf, e$message))
    })
  }
  if (length(failed_models) > 0) {
    message(sprintf("  WARNING: %d/%d models failed: %s",
                    length(failed_models), length(model_files),
                    paste(failed_models, collapse = ", ")))
    message("  Models are needed for xMetrics pipeline. Download manually from:")
    message("  gh release download epv-models -R peteowen1/pannadata -D pannadata/data/opta/models/")
  }
}

# ---- Step 3: Download prediction caches ----
if (what %in% c("all", "caches")) {
  message("\n[3/3] Downloading prediction caches...")
  dir.create(cache_opta, recursive = TRUE, showWarnings = FALSE)
  dir.create(cache_skills, recursive = TRUE, showWarnings = FALSE)

  cache_assets <- list(
    "07_seasonal_ratings.rds" = cache_opta,
    "06_seasonal_ratings.rds" = cache_skills,
    "01_match_stats.rds" = cache_skills,
    "02b_decay_params.rds" = cache_skills,
    "03_skill_spm.rds" = cache_skills
  )

  failed_caches <- character(0)
  for (asset_name in names(cache_assets)) {
    dest_dir <- cache_assets[[asset_name]]
    dest <- file.path(dest_dir, asset_name)
    if (file.exists(dest)) {
      message(sprintf("  SKIP: %s already exists", asset_name))
      next
    }
    tryCatch({
      tmp <- .pb_download_file(asset_name, repo = "peteowen1/pannadata",
                                tag = "predictions-cache",
                                label = paste("cache:", asset_name))
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
      message(sprintf("  OK: %s -> %s", asset_name, dest_dir))
    }, error = function(e) {
      failed_caches <<- c(failed_caches, asset_name)
      message(sprintf("  FAIL: %s — %s", asset_name, e$message))
    })
  }
  if (length(failed_caches) > 0) {
    message(sprintf("  WARNING: %d/%d caches failed: %s",
                    length(failed_caches), length(cache_assets),
                    paste(failed_caches, collapse = ", ")))
    message("  Run the RAPM + Skills pipelines locally to regenerate.")
  }
}

message("\n")
message(paste(rep("=", 60), collapse = ""))
message("  BOOTSTRAP COMPLETE")
message(paste(rep("=", 60), collapse = ""))
message("\nNext steps:")
message("  1. source('data-raw/epv/03_calculate_player_xmetrics.R')  # xMetrics")
message("  2. source('data-raw/player-ratings-opta/run_pipeline_opta.R')  # RAPM/SPM")
message("  3. source('data-raw/estimated-skills/run_skills_pipeline.R')  # Skills")
message("  4. source('data-raw/match-predictions-opta/run_predictions_opta.R')  # Predictions")
message("")
