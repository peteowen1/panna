# 12c_snapshot_wc_strength.R
# Archive a DATED snapshot of the WC2026 team-strength table (ELO + champion
# odds + rating categories) and diff it against the previous snapshot, so we can
# watch team ELO and p_champ drift across the tournament.
#
# Why this exists: step 12 uploads wc2026_team_strength.parquet to blog-latest,
# which is CLOBBERED every run — only the latest survives, so there is no history
# to diff (this is exactly why the kickoff ELO had to be reconstructed by replay).
# This step writes wc2026_team_strength_<DATE>.parquet to a SEPARATE release tag
# (wc2026-strength-history) that nothing clobbers and the blog's R2 sync ignores.
# Mirrors 12b_snapshot_wc_minutes.R exactly; the only differences are the source
# file, the key (team), and the diffed metrics (elo + p_champ).
#
# team_strength is FINAL at step 12 (downstream enrich_squads_piero.R only adds
# `piero` to the SQUADS file, never team_strength), so the step-12 cache file is
# the authoritative value.
#
# Idempotent within a day (--clobber). Sourced with local = TRUE from
# run_predictions_opta.R, so the body is wrapped in a function.

suppressPackageStartupMessages({ library(data.table); library(arrow) })

.snapshot_wc_strength <- function() {
  cache_dir <- if (exists("cache_dir", inherits = TRUE)) {
    get("cache_dir", inherits = TRUE)
  } else file.path("data-raw", "cache-predictions-opta")
  repo <- "peteowen1/pannadata"
  tag  <- "wc2026-strength-history"

  src_path <- file.path(cache_dir, "wc2026_team_strength.parquet")
  if (!file.exists(src_path)) {
    message("  wc2026_team_strength.parquet not in cache — run step 12 first; skipping snapshot")
    return(invisible(NULL))
  }

  today      <- Sys.Date()
  key_cols   <- "team"
  diff_cols  <- c("elo", "p_champ")        # the metrics that actually move
  keep_cols  <- c("team", "group", "elo", "p_champ", "panna", "epr", "psr", "bt")

  curr <- as.data.table(read_parquet(src_path))
  miss <- setdiff(diff_cols, names(curr))
  if (length(miss)) {
    warning("wc2026_team_strength.parquet missing ", paste(miss, collapse = ", "),
            " — cannot snapshot strength", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }
  curr <- curr[, intersect(keep_cols, names(curr)), with = FALSE]

  snap_name  <- sprintf("wc2026_team_strength_%s.parquet", today)
  snap_local <- file.path(cache_dir, snap_name)
  write_parquet(curr, snap_local)
  message(sprintf("  Snapshot written: %s (%d teams)", snap_name, nrow(curr)))

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
    snaps <- grep("^wc2026_team_strength_\\d{4}-\\d{2}-\\d{2}\\.parquet$", assets, value = TRUE)
    if (length(snaps) > 0) {
      dates <- as.Date(sub("^wc2026_team_strength_(\\d{4}-\\d{2}-\\d{2})\\.parquet$", "\\1", snaps))
      prior <- dates[dates < today]
      if (length(prior) > 0) {
        prev_date  <- max(prior)
        prev_name  <- sprintf("wc2026_team_strength_%s.parquet", prev_date)
        prev_local <- file.path(cache_dir, prev_name)
        dl <- gh_run(c("release", "download", tag, "--repo", repo,
                       "--pattern", prev_name, "--dir", cache_dir, "--clobber"))
        if (!gh_failed(dl) && file.exists(prev_local)) {
          prev <- as.data.table(read_parquet(prev_local))
          message(sprintf("  Prior snapshot: %s (%d teams)", prev_name, nrow(prev)))
        } else {
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

  # --- compute the diff (per-team elo + p_champ deltas) ----------------------
  diff_files <- character(0)
  if (!is.null(prev) && all(diff_cols %in% names(prev))) {
    diff_name  <- sprintf("wc2026_strength_diff_%s.csv", today)
    diff_local <- file.path(cache_dir, diff_name)

    pc <- prev[, c(key_cols, diff_cols), with = FALSE]
    setnames(pc, diff_cols, paste0(diff_cols, "_prev"))
    cc <- curr[, c(key_cols, "group", diff_cols), with = FALSE]
    setnames(cc, diff_cols, paste0(diff_cols, "_curr"))

    d <- merge(cc, pc, by = key_cols, all = TRUE)
    d[, elo_delta     := round(elo_curr - elo_prev, 1)]
    d[, p_champ_delta := round(p_champ_curr - p_champ_prev, 2)]
    setorder(d, -elo_delta)
    write.csv(d, diff_local, row.names = FALSE)
    diff_files <- diff_local

    mv <- d[!is.na(elo_delta)]
    message(sprintf("\n  === Strength drift %s -> %s ===", prev_date, today))
    message(sprintf("  %d teams compared | mean |elo delta| = %.1f | max = %.1f",
                    nrow(mv), mean(abs(mv$elo_delta), na.rm = TRUE),
                    max(abs(mv$elo_delta), na.rm = TRUE)))
    fmt <- function(x) sprintf("    elo %+6.1f  p_champ %+5.2f  %-22s", x$elo_delta, x$p_champ_delta, x$team)
    up   <- head(mv[order(-elo_delta)], 6); dn <- head(mv[order(elo_delta)], 6)
    message("  Biggest ELO risers:");  for (i in seq_len(nrow(up))) message(fmt(up[i]))
    message("  Biggest ELO fallers:"); for (i in seq_len(nrow(dn))) message(fmt(dn[i]))
  } else {
    message("  No diff computed (no comparable prior snapshot).")
  }

  # --- upload dated snapshot (+ diff) to the history tag ---------------------
  if (gh_ok && !no_upload) {
    rel <- gh_run(c("release", "view", tag, "--repo", repo))
    if (gh_failed(rel)) {
      crt <- gh_run(c("release", "create", tag, "--repo", repo,
               "--title", shQuote("WC2026 Team-Strength History"),
               "--notes", shQuote("Dated snapshots of wc2026 team strength (ELO + champion odds) for drift tracking.")))
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

  message("\n=== WC 2026 team-strength snapshot complete ===")
  invisible(NULL)
}

.snapshot_wc_strength()
