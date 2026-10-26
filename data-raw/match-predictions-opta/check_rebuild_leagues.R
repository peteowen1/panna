# Fail a game-logs rebuild part that lacks a league the published season has.
#
# Called by the "Refuse a partial season" step of game-logs-rebuild.yml (issue
# #376). 10b SKIPS a league with "insufficient data" without writing a failure
# marker, so the marker check alone let AFCON 2023 drop out silently. This
# compares the leagues the part built against the published season's.
#
# Env:
#   SEASON, PART, NPARTS_PER_SEASON   the matrix cell
#   PUBLISHED_LOGS                    published game_logs_<season>.parquet
#                                     (missing file = nothing to compare)
#   FAILURE_MARKER                    10b's .partial_publish_failures (optional)
#   LEAGUE_ALLOWLIST                  space-separated "SEASON:LEAGUE" or "LEAGUE"
#                                     leagues allowed to be absent (optional)
# Exit status 1 when a league is missing, 0 otherwise.

devtools::load_all(quiet = TRUE)
source(file.path("data-raw", "pipeline_utils.R"))

season <- Sys.getenv("SEASON")
part <- as.integer(Sys.getenv("PART"))
nparts <- as.integer(Sys.getenv("NPARTS_PER_SEASON"))
published_file <- Sys.getenv("PUBLISHED_LOGS")
marker <- Sys.getenv("FAILURE_MARKER")
cache_dir <- file.path("data-raw", "cache-predictions-opta")

if (!nzchar(published_file) || !file.exists(published_file)) {
  cat(sprintf("no published copy of %s: nothing to compare the built leagues with\n", season))
  quit(status = 0)
}
published <- unique(arrow::read_parquet(published_file, col_select = "league")$league)

part_file <- file.path(cache_dir, sprintf("game_logs_%s__part%dof%d.parquet", season, part, nparts))
built <- if (file.exists(part_file)) unique(arrow::read_parquet(part_file, col_select = "league")$league) else character()

reported <- if (nzchar(marker) && file.exists(marker)) {
  vapply(strsplit(readLines(marker, warn = FALSE), "[[:space:]]+"),
         function(x) if (length(x) >= 2L) x[2L] else NA_character_, character(1))
} else character()

missing <- leagues_missing_from_rebuild(
  published, built,
  expected = blog_leagues_for_part(resolve_blog_leagues()$blog_leagues, part, nparts),
  reported = reported,
  allowed = league_allowlist_for_season(
    strsplit(Sys.getenv("LEAGUE_ALLOWLIST"), "[[:space:]]+")[[1]], season)
)

cat(sprintf("published %s leagues: %s\n", season, paste(sort(published), collapse = " ")))
cat(sprintf("built in part %d of %d:  %s\n", part, nparts, paste(sort(built), collapse = " ")))
if (length(missing) > 0L) {
  for (lg in missing) {
    cat(sprintf("::error title=League missing from rebuild %s::%s is in the published %s logs but part %d did not build it (skipped or failed)\n",
                season, lg, season, part))
  }
  quit(status = 1)
}
cat("every published league in this part's share was built\n")
