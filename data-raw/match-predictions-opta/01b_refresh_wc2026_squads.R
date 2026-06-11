# 01b_refresh_wc2026_squads.R
# Rebuild wc2026_announced_squads.parquet from the canonical Wikipedia squads
# page so the expected-minutes weights track the latest lineups (group-stage
# appearances accumulate daily during the tournament).
#
# Fallback: if the Wikipedia scrape errors (HTTP failure, or the parse
# sanity-check in scrape_wiki_squads.R fires), download the last known-good
# parquet from the predictions-cache release. That copy is refreshed only by
# a manual run of upload_prediction_caches.R, so its age is checked below.
# Step 02 only falls back to last-played-XI weighting if both paths fail —
# before 2026-06-11 the parquet was local-only (gitignored, on no release),
# so every GHA run silently shipped last-XI-weighted WC2026 team ratings.

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
squads_path <- file.path(cache_dir, "wc2026_announced_squads.parquet")

refresh_ok <- tryCatch({
  source("data-raw/match-predictions-opta/scrape_wiki_squads.R", local = new.env())
  TRUE
}, error = function(e) {
  warning(sprintf(
    "WC2026 squads wiki refresh FAILED (%s) — trying predictions-cache fallback",
    conditionMessage(e)), call. = FALSE, immediate. = TRUE)
  FALSE
})

if (!refresh_ok) {
  # tryCatch the whole fallback: system2() throws (rather than returning a
  # status) when the gh binary itself is missing, and this step must degrade
  # to a warning, not halt the pipeline.
  fallback_err <- tryCatch({
    res <- suppressWarnings(system2(
      "gh", c("release", "download", "predictions-cache",
              "--repo", "peteowen1/pannadata",
              "--pattern", "wc2026_announced_squads.parquet",
              "--dir", cache_dir, "--clobber"),
      stdout = TRUE, stderr = TRUE))
    status <- attr(res, "status")
    if (!is.null(status) && status != 0) paste(res, collapse = "\n") else NULL
  }, error = function(e) conditionMessage(e))

  if (is.null(fallback_err)) {
    message("  Downloaded fallback wc2026_announced_squads.parquet from predictions-cache")
    # gh release download stamps the file with NOW, so mtime says nothing
    # about data age — check the release asset's updatedAt instead (the
    # asset only refreshes when upload_prediction_caches.R is run manually).
    asset_upd <- tryCatch(suppressWarnings(system2(
      "gh", c("api", "repos/peteowen1/pannadata/releases/tags/predictions-cache",
              "--jq",
              ".assets[] | select(.name == \"wc2026_announced_squads.parquet\") | .updated_at"),
      stdout = TRUE, stderr = TRUE)), error = function(e) character(0))
    asset_upd <- asset_upd[nzchar(asset_upd)][1]
    if (!is.na(asset_upd) && length(asset_upd) == 1L) {
      age_days <- as.numeric(difftime(Sys.time(),
                                      as.POSIXct(asset_upd, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                                      units = "days"))
      if (is.finite(age_days) && age_days > 7) {
        warning(sprintf(paste(
          "Fallback wc2026_announced_squads.parquet is %.0f days old (asset updated %s).",
          "Expected-minutes weights are frozen at that date — rerun",
          "upload_prediction_caches.R after a successful scrape to refresh it."),
          age_days, asset_upd), call. = FALSE, immediate. = TRUE)
      } else {
        message(sprintf("  Fallback asset age: %.1f days (updated %s)", age_days, asset_upd))
      }
    }
  } else {
    warning(paste(
      "WC2026 squads fallback download FAILED too — step 02 will fall back to",
      "last-played-XI weighting for WC2026 fixtures:", fallback_err),
      call. = FALSE, immediate. = TRUE)
  }
}

if (file.exists(squads_path)) {
  message(sprintf("  wc2026_announced_squads.parquet present (%.0f KB, mtime %s)",
                  file.size(squads_path) / 1024,
                  format(file.mtime(squads_path))))
} else {
  warning(paste(
    "wc2026_announced_squads.parquet ABSENT after step 01b — step 02 will",
    "silently weight WC2026 teams by last-played XI"),
    call. = FALSE, immediate. = TRUE)
}
