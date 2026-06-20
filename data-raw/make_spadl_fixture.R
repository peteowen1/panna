# Generate real-match test fixtures: raw Opta events + SPADL conversion.
# Run from panna/:  Rscript data-raw/make_spadl_fixture.R
# Writes tests/testthat/fixtures/{opta_match_events_epl,spadl_match_epl}.parquet

devtools::load_all(".", quiet = TRUE)

season <- "2025-2026"

fx <- load_opta_fixtures("EPL", season = season, status = "Played", source = "local")
cat("Fixtures loaded:", nrow(fx), "played EPL matches\n")
stopifnot(nrow(fx) > 0)

date_col <- intersect(
  c("match_date", "date", "kickoff_time", "match_time", "datetime", "match_datetime"),
  names(fx)
)[1]
stopifnot(!is.na(date_col))
fx <- fx[order(fx[[date_col]], decreasing = TRUE), ]

ev <- data.table::as.data.table(
  load_opta_match_events("EPL", season = season, source = "local")
)
cat("Events loaded:", format(nrow(ev), big.mark = ","), "rows,",
    data.table::uniqueN(ev$match_id), "matches\n")
stopifnot(nrow(ev) > 0)

# Most recent played match that actually has events on disk
chosen <- NA_character_
for (mid in fx$match_id) {
  if (mid %in% ev$match_id) { chosen <- mid; break }
}
stopifnot(!is.na(chosen))

info <- fx[fx$match_id == chosen, ][1, ]
label_cols <- intersect(
  c("match_id", date_col, "home_team", "home_team_name", "away_team",
    "away_team_name", "home_score", "away_score"),
  names(fx)
)
cat("Chosen match:\n")
print(as.data.frame(info)[, label_cols])

m_ev <- ev[match_id == chosen]
spadl <- convert_opta_to_spadl(m_ev)

out_dir <- file.path("tests", "testthat", "fixtures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

arrow::write_parquet(m_ev, file.path(out_dir, "opta_match_events_epl.parquet"))
arrow::write_parquet(spadl, file.path(out_dir, "spadl_match_epl.parquet"))

cat("\nRaw events :", nrow(m_ev), "rows x", ncol(m_ev), "cols\n")
cat("SPADL      :", nrow(spadl), "rows x", ncol(spadl), "cols\n")
cat("SPADL columns:\n ", paste(names(spadl), collapse = ", "), "\n")
for (f in c("opta_match_events_epl.parquet", "spadl_match_epl.parquet")) {
  p <- file.path(out_dir, f)
  cat(sprintf("%-35s %.0f KB\n", f, file.size(p) / 1024))
}
