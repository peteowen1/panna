# 07b_snapshot_predictions.R
# Archive a DATED snapshot of predictions.parquet, so a future as-at replay of
# a club league/cup can use the ACTUAL predictions that existed on a given
# historical date, rather than today's current model re-stamped onto a past
# date (panna#178 — same "exact-replay fidelity" gap the WC2026 snapshot
# steps, 12b/12c, close for minutes/team-strength).
#
# Why this exists: step 13 publishes predictions.parquet to predictions-latest,
# which is CLOBBERED every run — only the latest survives, so there is no
# history to replay against. This step writes predictions_<DATE>.parquet to a
# SEPARATE release tag (predictions-history) that nothing clobbers and the
# blog's normal R2 sync ignores. Each run a new dated file accumulates.
#
# Deliberately no diff output here (unlike 12b/12c) — predictions.parquet
# already carries per-match rows for every league/date, so there's no single
# natural join key to diff on across dates the way team/minutes tables have;
# the raw dated snapshots are the deliverable the as-at replay consumes.
#
# predictions.parquet is FINAL at step 7 (nothing downstream mutates it), so
# snapshotting the step-7 cache file here captures the authoritative value.
#
# Idempotent within a day (--clobber). Sourced with local = TRUE from
# run_predictions_opta.R, so the body is wrapped in a function.

suppressPackageStartupMessages({ library(arrow) })

.snapshot_predictions <- function() {
  cache_dir <- if (exists("cache_dir", inherits = TRUE)) {
    get("cache_dir", inherits = TRUE)
  } else file.path("data-raw", "cache-predictions-opta")
  repo <- "peteowen1/pannadata"
  tag  <- "predictions-history"

  src_path <- file.path(cache_dir, "predictions.parquet")
  if (!file.exists(src_path)) {
    message("  predictions.parquet not in cache — run step 07 first; skipping snapshot")
    return(invisible(NULL))
  }

  today      <- Sys.Date()
  snap_name  <- sprintf("predictions_%s.parquet", today)
  snap_local <- file.path(cache_dir, snap_name)
  file.copy(src_path, snap_local, overwrite = TRUE)

  curr_n <- nrow(read_parquet(src_path))
  message(sprintf("  Snapshot written: %s (%d rows)", snap_name, curr_n))

  no_upload <- isTRUE(Sys.getenv("WC2026_NO_UPLOAD", "") == "1")
  gh_ok <- !is.null(tryCatch(system2("gh", "--version", stdout = TRUE, stderr = TRUE),
                             error = function(e) NULL))
  gh_run <- function(args) system2("gh", args, stdout = TRUE, stderr = TRUE)
  gh_failed <- function(res) !is.null(attr(res, "status")) && attr(res, "status") != 0

  if (gh_ok && !no_upload) {
    rel <- gh_run(c("release", "view", tag, "--repo", repo))
    if (gh_failed(rel)) {
      crt <- gh_run(c("release", "create", tag, "--repo", repo,
               "--title", shQuote("Match Predictions History"),
               "--notes", shQuote("Dated snapshots of predictions.parquet for as-at replay fidelity (panna#178).")))
      if (gh_failed(crt))
        warning("Failed to create the ", tag, " release: ", paste(crt, collapse = "\n"),
                call. = FALSE, immediate. = TRUE)
    }
    res <- gh_run(c("release", "upload", tag, shQuote(snap_local), "--repo", repo, "--clobber"))
    if (gh_failed(res)) {
      warning(sprintf("Failed to upload %s: %s", basename(snap_local), paste(res, collapse = "\n")),
              call. = FALSE, immediate. = TRUE)
    } else {
      message(sprintf("  Uploaded %s", basename(snap_local)))
    }
  } else {
    message("  Skipped upload (gh unavailable or WC2026_NO_UPLOAD=1).")
  }

  message("\n=== Predictions snapshot complete ===")
  invisible(NULL)
}

.snapshot_predictions()
