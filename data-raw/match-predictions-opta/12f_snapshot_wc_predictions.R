# 12f_snapshot_wc_predictions.R
# Archive DATED snapshots of wc2026_predictions.parquet (per-match H/D/A probs)
# and wc2026_knockout_probs.parquet (pairwise knockout lookup), so a future
# as-at replay of a tournament can use the probabilities that were ACTUALLY
# published on a given date, not today's model re-stamped onto a past date
# (panna#177).
#
# Why this exists: step 07 re-predicts ALL matches, played and upcoming, on
# every run (07_predict_fixtures.R:29) with freshly re-fit goals/outcome models,
# and step 12 uploads the result to blog-latest, which is CLOBBERED every run.
# The knockout lookup (step 11) is rewritten the same way. This step writes
# wc2026_predictions_<DATE>.parquet / wc2026_knockout_probs_<DATE>.parquet to a
# SEPARATE release tag (wc2026-predictions-history) that nothing clobbers and
# the blog's R2 sync ignores. Mirrors 07b_snapshot_predictions.R (raw dated
# copies, no diff -- the dated files are the deliverable the replay consumes)
# and 12b/12c (same repo, WC2026 liveness gate, WC2026_NO_UPLOAD switch).
#
# Both files are FINAL by the end of step 12 (predictions written by step 12
# section 2, knockout probs by step 11 and only uploaded by step 12), so this
# runs after step 12. The knockout file is optional: step 11 is skipped once
# the tournament is over, and a missing file just skips that one snapshot.
#
# Idempotent within a day (--clobber). Sourced with local = TRUE from
# run_predictions_opta.R, so the body is wrapped in a function.

suppressPackageStartupMessages({ library(arrow) })

# (cache file stem, required?) -- predictions is the point of the step, the
# knockout lookup is "ideally" (panna#177) and absent when step 11 did not run.
.wc_prediction_artifacts <- list(
  list(stem = "wc2026_predictions",    required = TRUE),
  list(stem = "wc2026_knockout_probs", required = FALSE)
)

.snapshot_wc_predictions <- function() {
  cache_dir <- if (exists("cache_dir", inherits = TRUE)) {
    get("cache_dir", inherits = TRUE)
  } else file.path("data-raw", "cache-predictions-opta")
  repo <- "peteowen1/pannadata"
  tag  <- "wc2026-predictions-history"

  today      <- Sys.Date()
  snap_files <- character(0)
  for (a in .wc_prediction_artifacts) {
    src_path <- file.path(cache_dir, paste0(a$stem, ".parquet"))
    if (!file.exists(src_path)) {
      message(sprintf("  %s.parquet not in cache -- %s; skipping its snapshot", a$stem,
                      if (a$required) "run step 12 first" else "step 11 did not write it this run"))
      next
    }
    snap_name  <- sprintf("%s_%s.parquet", a$stem, today)
    snap_local <- file.path(cache_dir, snap_name)
    file.copy(src_path, snap_local, overwrite = TRUE)
    message(sprintf("  Snapshot written: %s (%d rows)", snap_name, nrow(read_parquet(src_path))))
    snap_files <- c(snap_files, snap_local)
  }
  if (length(snap_files) == 0L) return(invisible(NULL))

  no_upload <- isTRUE(Sys.getenv("WC2026_NO_UPLOAD", "") == "1")
  gh_ok <- !is.null(tryCatch(system2("gh", "--version", stdout = TRUE, stderr = TRUE),
                             error = function(e) NULL))
  gh_run <- function(args) system2("gh", args, stdout = TRUE, stderr = TRUE)
  gh_failed <- function(res) !is.null(attr(res, "status")) && attr(res, "status") != 0

  if (gh_ok && !no_upload) {
    rel <- gh_run(c("release", "view", tag, "--repo", repo))
    if (gh_failed(rel)) {
      crt <- gh_run(c("release", "create", tag, "--repo", repo,
               "--title", shQuote("WC2026 Predictions History"),
               "--notes", shQuote("Dated snapshots of wc2026_predictions and wc2026_knockout_probs for as-at replay fidelity (panna#177).")))
      if (gh_failed(crt))
        warning("Failed to create the ", tag, " release: ", paste(crt, collapse = "\n"),
                call. = FALSE, immediate. = TRUE)
    }
    for (f in snap_files) {
      res <- gh_run(c("release", "upload", tag, shQuote(f), "--repo", repo, "--clobber"))
      if (gh_failed(res)) {
        warning(sprintf("Failed to upload %s: %s", basename(f), paste(res, collapse = "\n")),
                call. = FALSE, immediate. = TRUE)
      } else {
        message(sprintf("  Uploaded %s", basename(f)))
      }
    }
  } else {
    message("  Skipped upload (gh unavailable or WC2026_NO_UPLOAD=1).")
  }

  message("\n=== WC 2026 predictions snapshot complete ===")
  invisible(NULL)
}

.snapshot_wc_predictions()
