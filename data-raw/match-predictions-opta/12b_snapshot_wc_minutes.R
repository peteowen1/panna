# 12b_snapshot_wc_minutes.R
# Archive a DATED snapshot of the WC2026 expected-minutes table and diff it
# against the previous snapshot, so we can watch projected-minutes drift across
# the group stage.
#
# Why this exists: step 12 uploads wc2026_squads.parquet to blog-latest, which
# is CLOBBERED every run — only the latest survives, so there is no history to
# diff. This step writes wc2026_squads_<DATE>.parquet to a SEPARATE release tag
# (wc2026-minutes-history) that nothing clobbers and the blog's R2 sync ignores
# (it only pulls wc2026_*.parquet from blog-latest, not from this tag). Each
# night a new dated file accumulates; the diff reports the biggest movers vs the
# most recent prior snapshot.
#
# Minutes (expected_minutes_norm) are FINAL at step 12 — pannadata's downstream
# enrich_squads_piero.R only adds the `piero` column, it never touches minutes —
# so snapshotting the step-12 cache file here captures the authoritative value.
#
# Idempotent within a day: re-running overwrites the same-dated snapshot
# (--clobber) and recomputes the diff against the prior date.
#
# Sourced with local = TRUE from run_predictions_opta.R, so the body is wrapped
# in a function (a top-level return() in a local-sourced file errors with "no
# function to return from").

suppressPackageStartupMessages({ library(data.table); library(arrow) })

.snapshot_wc_minutes <- function() {
  cache_dir <- if (exists("cache_dir", inherits = TRUE)) {
    get("cache_dir", inherits = TRUE)
  } else file.path("data-raw", "cache-predictions-opta")
  repo <- "peteowen1/pannadata"
  tag  <- "wc2026-minutes-history"

  squads_path <- file.path(cache_dir, "wc2026_squads.parquet")
  if (!file.exists(squads_path)) {
    message("  wc2026_squads.parquet not in cache — run step 12 first; skipping snapshot")
    return(invisible(NULL))
  }

  today    <- Sys.Date()
  key_cols <- c("team", "player_id")
  em_col   <- "expected_minutes_norm"

  curr <- as.data.table(read_parquet(squads_path))
  if (!em_col %in% names(curr)) {
    warning("wc2026_squads.parquet has no ", em_col, " column — cannot snapshot minutes",
            call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }
  keep <- intersect(c(key_cols, "player_name", "position", em_col, "is_starter_pred"),
                    names(curr))
  curr <- curr[, ..keep]

  snap_name  <- sprintf("wc2026_squads_%s.parquet", today)
  snap_local <- file.path(cache_dir, snap_name)
  write_parquet(curr, snap_local)
  message(sprintf("  Snapshot written: %s (%d players, %d squads)",
                  snap_name, nrow(curr), uniqueN(curr$team)))

  # --- gh availability (mirror step 12's guard) ------------------------------
  no_upload <- isTRUE(Sys.getenv("WC2026_NO_UPLOAD", "") == "1")
  gh_ok <- !is.null(tryCatch(system2("gh", "--version", stdout = TRUE, stderr = TRUE),
                             error = function(e) NULL))
  gh_run <- function(args) system2("gh", args, stdout = TRUE, stderr = TRUE)
  gh_failed <- function(res) !is.null(attr(res, "status")) && attr(res, "status") != 0

  # --- find + download the most recent PRIOR snapshot ------------------------
  prev <- NULL; prev_date <- NA
  if (gh_ok && !no_upload) {
    assets <- gh_run(c("release", "view", tag, "--repo", repo,
                       "--json", "assets", "--jq", ".assets[].name"))
    if (gh_failed(assets)) {
      message("  No ", tag, " release yet — this snapshot becomes the baseline")
      assets <- character(0)
    }
    snaps <- grep("^wc2026_squads_\\d{4}-\\d{2}-\\d{2}\\.parquet$", assets, value = TRUE)
    if (length(snaps) > 0) {
      dates <- as.Date(sub("^wc2026_squads_(\\d{4}-\\d{2}-\\d{2})\\.parquet$", "\\1", snaps))
      prior <- dates[dates < today]
      if (length(prior) > 0) {
        prev_date  <- max(prior)
        prev_name  <- sprintf("wc2026_squads_%s.parquet", prev_date)
        prev_local <- file.path(cache_dir, prev_name)
        dl <- gh_run(c("release", "download", tag, "--repo", repo,
                       "--pattern", prev_name, "--dir", cache_dir, "--clobber"))
        if (!gh_failed(dl) && file.exists(prev_local)) {
          prev <- as.data.table(read_parquet(prev_local))
          message(sprintf("  Prior snapshot: %s (%d players)", prev_name, nrow(prev)))
        } else {
          # Distinct from "no prior snapshot exists" (informational, below): a
          # prior snapshot IS listed on the release but the fetch failed, so we
          # silently lose tonight's drift diff — the whole point of this step.
          # Surface it as a real warning, not a soft message.
          warning("Could not download prior snapshot ", prev_name,
                  " (listed on the release but fetch failed) — drift diff skipped this run",
                  call. = FALSE, immediate. = TRUE)
        }
      } else {
        message("  No prior dated snapshot before ", today, " — baseline only, no diff")
      }
    }
  } else {
    message("  gh unavailable or WC2026_NO_UPLOAD=1 — snapshot kept local, no upload/diff")
  }

  # --- compute the diff ------------------------------------------------------
  diff_files <- character(0)
  if (!is.null(prev) && em_col %in% names(prev)) {
    diff_name  <- sprintf("wc2026_minutes_diff_%s.csv", today)
    diff_local <- file.path(cache_dir, diff_name)

    # is_starter_pred is intersect-guarded where the snapshot is built, so a
    # prior/current snapshot may legitimately lack it — default to NA so the
    # XI-change diff degrades gracefully instead of erroring on a missing column.
    if (!"is_starter_pred" %in% names(prev)) prev[, is_starter_pred := NA]
    if (!"is_starter_pred" %in% names(curr)) curr[, is_starter_pred := NA]
    pc <- prev[, c(key_cols, em_col, "is_starter_pred"), with = FALSE]
    setnames(pc, c(em_col, "is_starter_pred"), c("em_prev", "starter_prev"))
    cc <- curr[, c(key_cols, "player_name", "position", em_col, "is_starter_pred"),
               with = FALSE]
    setnames(cc, c(em_col, "is_starter_pred"), c("em_curr", "starter_curr"))

    d <- merge(cc, pc, by = key_cols, all = TRUE)
    d[, status := fifelse(is.na(em_prev), "added",
                  fifelse(is.na(em_curr), "dropped", "present"))]
    d[, delta := round(em_curr - em_prev, 1)]
    d[, xi_change := !is.na(starter_prev) & !is.na(starter_curr) &
                     starter_prev != starter_curr]
    setorder(d, -delta)

    write.csv(d, diff_local, row.names = FALSE)
    diff_files <- diff_local

    movers <- d[status == "present" & !is.na(delta)]
    message(sprintf("\n  === Minutes drift %s -> %s ===", prev_date, today))
    message(sprintf("  %d players present in both; %d added, %d dropped",
                    nrow(movers), sum(d$status == "added"), sum(d$status == "dropped")))
    message(sprintf("  Mean |delta| = %.1f   |delta|>=15 = %d   XI changes = %d",
                    mean(abs(movers$delta), na.rm = TRUE),
                    sum(abs(movers$delta) >= 15, na.rm = TRUE),
                    sum(d$xi_change, na.rm = TRUE)))
    fmt <- function(x) sprintf("    %+6.1f  %-22s %-14s (%s->%s)",
                               x$delta, x$player_name, x$team,
                               round(x$em_prev, 0), round(x$em_curr, 0))
    top_up   <- head(movers[order(-delta)], 8)
    top_down <- head(movers[order(delta)], 8)
    message("  Biggest risers:");  for (i in seq_len(nrow(top_up)))   message(fmt(top_up[i]))
    message("  Biggest fallers:"); for (i in seq_len(nrow(top_down))) message(fmt(top_down[i]))
  } else {
    message("  No diff computed (no comparable prior snapshot).")
  }

  # --- upload dated snapshot (+ diff) to the history tag ---------------------
  if (gh_ok && !no_upload) {
    rel <- gh_run(c("release", "view", tag, "--repo", repo))
    if (gh_failed(rel)) {
      crt <- gh_run(c("release", "create", tag, "--repo", repo,
               "--title", shQuote("WC2026 Minutes History"),
               "--notes", shQuote("Dated snapshots of wc2026 expected minutes for drift tracking.")))
      # Check create directly: if it fails (perms/race/network) every upload
      # below fails too, so surface the root cause once rather than N confusing
      # per-file upload warnings.
      if (gh_failed(crt))
        warning("Failed to create the ", tag, " release: ", paste(crt, collapse = "\n"),
                call. = FALSE, immediate. = TRUE)
    }
    for (f in c(snap_local, diff_files)) {
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

  message("\n=== WC 2026 minutes snapshot complete ===")
  invisible(NULL)
}

.snapshot_wc_minutes()
