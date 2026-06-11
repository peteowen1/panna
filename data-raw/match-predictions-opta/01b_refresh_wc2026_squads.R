# 01b_refresh_wc2026_squads.R
# Rebuild wc2026_announced_squads.parquet from the canonical Wikipedia squads
# page so the expected-minutes weights track the latest lineups (group-stage
# appearances accumulate daily during the tournament).
#
# Fallback: if the Wikipedia scrape (or name resolution) fails, download the
# last known-good parquet from the predictions-cache release. Step 02 only
# falls back to last-played-XI weighting if both paths fail — that fallback
# is silent in step 02, which is exactly the gap this step closes: before
# 2026-06-11 the parquet was local-only (gitignored, on no release), so every
# GHA run silently shipped last-XI-weighted WC2026 team ratings.

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
  res <- suppressWarnings(system2(
    "gh", c("release", "download", "predictions-cache",
            "--repo", "peteowen1/pannadata",
            "--pattern", "wc2026_announced_squads.parquet",
            "--dir", cache_dir, "--clobber"),
    stdout = TRUE, stderr = TRUE))
  status <- attr(res, "status")
  if (is.null(status) || status == 0) {
    message("  Downloaded fallback wc2026_announced_squads.parquet from predictions-cache")
  } else {
    warning(paste(
      "WC2026 squads fallback download FAILED too — step 02 will fall back to",
      "last-played-XI weighting for WC2026 fixtures:",
      paste(res, collapse = "\n")), call. = FALSE, immediate. = TRUE)
  }
}

if (file.exists(squads_path)) {
  message(sprintf("  wc2026_announced_squads.parquet present (%.0f KB, mtime %s)",
                  file.size(squads_path) / 1024,
                  format(file.mtime(squads_path))))
}
