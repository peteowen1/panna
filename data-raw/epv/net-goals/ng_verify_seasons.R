# ng_verify_seasons.R -- do the published game logs actually conserve?
#
# Checks every game_logs_<season>.parquet against the FIXTURES, not against the
# backfill's own log. Those are different claims and this session separated them
# twice: a run can print "Game logs exported successfully!" over four seasons
# when eleven were asked for.
#
# The `age_h` column is the load-bearing part. Without it a season that has not
# been rebuilt yet is indistinguishable from one rebuilt and broken -- both show
# has_ng = FALSE, and the first version of this script reported six false
# failures on exactly that ambiguity.
#
# Expect max_err ~4e-4 (the parquet writer rounds to 4 decimals) and over_001 = 0.
#
#   powershell.exe -Command 'Rscript "data-raw/epv/net-goals/ng_verify_seasons.R"'

setwd("C:/dev/pannaverse/panna")
suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

# Verify every season the backfill has written SO FAR, against the fixtures
# rather than against the run log. "The workflow said success" and "the numbers
# are right" are different claims and tonight has already separated them twice.
CACHE <- "data-raw/cache-predictions-opta"
SEASONS <- c("2015-2016","2016-2017","2017-2018","2018-2019","2019-2020",
             "2020-2021","2021-2022","2022-2023","2023-2024","2024-2025",
             "2025-2026","2026-2027")

fx <- as.data.table(collect(arrow::open_dataset(
  "C:/dev/pannaverse/pannadata/data/opta/opta_fixtures.parquet") |>
  select(match_id, home_team_id, away_team_id, home_score, away_score)))
fx[, `:=`(hs = as.numeric(home_score), as_ = as.numeric(away_score))]
fx <- unique(fx[!is.na(hs)], by = "match_id")

out <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(CACHE, sprintf("game_logs_%s.parquet", s))
  if (!file.exists(f)) return(NULL)
  # The file's age is what separates "not rebuilt yet" from "rebuilt and
  # broken". Without it this check reports the same thing either way, which is
  # the exact ambiguity that let seven aborted seasons look like success.
  age_h <- round(as.numeric(difftime(Sys.time(), file.mtime(f), units = "hours")), 1)
  d <- as.data.table(arrow::read_parquet(f))
  if (!"net_goals" %in% names(d)) {
    return(data.table(season = s, rows = nrow(d), age_h = age_h, has_ng = FALSE,
                      state = if (age_h > 2) "STALE - not rebuilt yet" else "REBUILT BUT MISSING COLUMN"))
  }
  tt <- merge(d[!is.na(net_goals) & !is.na(team_id),
                .(v = sum(net_goals)), by = .(match_id, team_id)],
              fx, by = "match_id")
  tt <- tt[team_id == home_team_id | team_id == away_team_id]
  tt[, err := v - fifelse(team_id == home_team_id, hs - as_, as_ - hs)]
  data.table(season = s, rows = nrow(d), age_h = age_h, has_ng = TRUE,
             state = "rebuilt",
             has_recon = "ng_recon" %in% names(d),
             cover = round(100 * sum(!is.na(d$net_goals)) / nrow(d), 2),
             na_team = sum(is.na(d$team_id)),
             team_matches = nrow(tt),
             max_err = round(max(abs(tt$err)), 5),
             over_001 = sum(abs(tt$err) > 0.001))
}), fill = TRUE)

cat("==== seasons written so far ====\n")
cat("max_err = worst team-match distance from its own goal difference.\n")
cat("~4e-4 is the parquet's 4-decimal rounding. over_001 should be 0.\n\n")
print(out, row.names = FALSE)
cat("\nseasons verified:", nrow(out), "of", length(SEASONS), "\n")
if (nrow(out)) {
  # A stale file is not a failure; a rebuilt one missing the column is.
  bad <- out[(has_ng == TRUE & (over_001 > 0 | cover < 99)) |
             (has_ng == FALSE & age_h <= 2)]
  cat("seasons FAILING:", nrow(bad), "\n")
  if (nrow(bad)) print(bad, row.names = FALSE)
}
