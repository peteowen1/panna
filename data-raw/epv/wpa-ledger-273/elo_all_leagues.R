# panna#273: pre-match Elo for EVERY league, not only the 14 competitions the match-prediction
# model covers. Builds fixture results for the extra leagues with the predictions pipeline's own
# step 01 (written to a scratch cache_dir, never the real one), stacks them on the existing
# 01_fixture_results.rds, and runs compute_match_elos() with step 03's exact settings.
# Output: $WPA273_DIR/match_elo_all.parquet (match_id, league, season, match_date, home_elo, away_elo, elo_diff),
# every value PRE-match (compute_match_elos() returns the rating before each update).
# Run from panna/ under PowerShell.
suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
scratch <- file.path(out, "fixture_extra"); dir.create(scratch, showWarnings = FALSE)

# The base leagues are rebuilt too, into scratch: the local pipeline cache can be weeks old
# (2026-11-10: built 2026-10-10, so Liverpool's last four matches were still "Fixture").
scratch_base <- file.path(out, "fixture_base"); dir.create(scratch_base, showWarnings = FALSE)
f_base <- file.path(scratch_base, "01_fixture_results.rds")
if (!file.exists(f_base)) {
  # The predictions run's own league list (run_predictions_opta.R), internationals included, so
  # national-team Elo matches the pipeline's.
  local({
    cache_dir <- scratch_base
    leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "NED", "POR", "TUR", "ENG2", "SCO", "UCL", "UEL", "UECL",
                 "WC", "EURO", "AFCON", "COPA", "GOLD", "ACUP", "GULF", "WCQ_UEFA", "WCQ_CONMEBOL", "WCQ_CAF",
                 "WCQ_AFC", "EUROQ", "AFCONQ", "ACUPQ", "NL", "INTL_FR")
    source("data-raw/match-predictions-opta/01_build_fixture_results.R", local = TRUE)
  })
}
base <- as.data.table(readRDS(f_base))
message(sprintf("existing fixture results: %d rows, leagues: %s", nrow(base), paste(sort(unique(base$league)), collapse = " ")))

extra <- c("BEL", "MEX", "SAU", "AUS", "MLS", "ARG", "BRA", "LIB", "SUD", "ACLE", "CAFCL", "CCC", "LGC", "CWC")
extra <- setdiff(extra, unique(base$league))
f_extra <- file.path(scratch, "01_fixture_results.rds")
if (!file.exists(f_extra)) {
  leagues <- extra; cache_dir <- scratch
  source("data-raw/match-predictions-opta/01_build_fixture_results.R", local = TRUE)
}
ext <- as.data.table(readRDS(f_extra))
message(sprintf("extra fixture results: %d rows, played %d, by league:", nrow(ext), ext[match_status == "Played", .N]))
print(ext[match_status == "Played", .N, by = league][order(league)])

allr <- rbindlist(list(base, ext[!match_id %in% base$match_id]), fill = TRUE)
played <- as.data.frame(allr[match_status == "Played"][order(match_date)])
message(sprintf("Elo over %d played matches, %d leagues", nrow(played), uniqueN(played$league)))
elo_result <- compute_match_elos(played, k = 20, home_advantage = 88, initial_elo = 1500,
                                 k_table = ELO_MATCH_TYPE_K, cross_conf_mult = ELO_CROSS_CONF_MULT,
                                 conf_priors = ELO_CONFEDERATION_PRIORS, use_venue_factor = TRUE)
pm <- as.data.table(elo_result$per_match)
pm <- unique(pm, by = "match_id")[as.data.table(played)[, .(match_id, league, season, match_date)], on = "match_id", nomatch = NULL]
message(sprintf("per-match Elo: %d matches, elo_diff non-NA %.1f%%", nrow(pm), 100 * mean(!is.na(pm$elo_diff))))
print(pm[, .(matches = .N, elo_cov = round(mean(!is.na(elo_diff)), 3), sd_diff = round(sd(elo_diff, na.rm = TRUE))), by = league][order(league)])

# Agreement with the pipeline's own Elo on the 14 shared competitions (should be close, not identical:
# the extra leagues add cross-league matches such as CWC and the Libertadores).
md <- as.data.table(readRDS("data-raw/cache-predictions-opta/04_match_dataset.rds"))[, .(match_id, elo_diff_04 = elo_diff)]
cmp <- pm[md, on = "match_id", nomatch = NULL]
message(sprintf("vs 04_match_dataset on %d shared matches: correlation %.4f, mean |diff| %.1f Elo points",
                nrow(cmp), cor(cmp$elo_diff, cmp$elo_diff_04, use = "complete.obs"), mean(abs(cmp$elo_diff - cmp$elo_diff_04), na.rm = TRUE)))
arrow::write_parquet(pm, file.path(out, "match_elo_all.parquet"))
message("wrote ", file.path(out, "match_elo_all.parquet"))
