# 09d_spmr.R
# SPMR = decay-weighted SPM (halflife 1 season), with its offensive and
# defensive halves OSPMR and DSPMR.
#
# Fills the one empty cell in the career / season / decayed matrix. Before this:
#
#   metric   career          season          decayed
#   RAPM     04_rapm (s2)    07_seasonal     09c_career_rapm
#   SPM      05_spm  (s2)    07_seasonal     THIS FILE
#   xRAPM    06_xrapm (s2)   07_seasonal     09_career_panna
#
# WHY IT IS NOT REDUNDANT WITH panna. xRAPM uses SPM as its prior, so the
# obvious objection is that panna already contains this information. Tested
# 2026-09-15 and falsified: predicting HELD-OUT 2026 seasonal RAPM defence from
# data <= 2025, decayed SPM scores 0.194 against panna-as-at's 0.179 and single
# season SPM's 0.177, and adding it on top of panna lifts adj R^2 from 0.0360 to
# 0.0617 (partial correlation +0.164 net of panna). Decaying SPM directly
# recovers something the prior route loses.
#
# Evidence: pannaverse docs/reviews/DEFENSIVE-RATING-INVESTIGATION-2026-09-15.md
# Registry:  pannaverse docs/reference/METRIC-DEFINITIONS.md
#
# OPEN: halflife_seasons = 1 was copied from panna's 365 days, not tuned. Tune it
# against the same holdout before treating the value as settled.
#
# Inputs:  cache-opta/07_seasonal_ratings.rds ($seasonal_spm)
# Output:  pannadata/data/opta/career_spm.parquet
#          (player_id, player_name, spmr, ospmr, dspmr, total_minutes,
#           n_seasons, ref_season, sign_convention). No upload.

library(arrow)
library(data.table)
devtools::load_all()

cache_opta <- file.path("data-raw", "cache-opta")
opta_dir   <- file.path("..", "pannadata", "data", "opta")

# Plain exists() so a driver's globalenv flag is visible through
# source(..., local = TRUE) -- same reason as 09c/09_career_panna.
halflife_seasons <- if (exists("spmr_halflife_seasons")) spmr_halflife_seasons else 1
ref_season       <- if (exists("spmr_ref_season")) spmr_ref_season else NULL

sr_path <- file.path(cache_opta, "07_seasonal_ratings.rds")
if (!file.exists(sr_path)) stop("Missing required input: ", sr_path)

cat("\n=== Loading seasonal SPM ===\n")
sr <- readRDS(sr_path)
if (is.null(sr$seasonal_spm)) {
  stop("seasonal_spm not found in 07_seasonal_ratings.rds - re-run step 7")
}
sspm <- as.data.table(sr$seasonal_spm)
cat(sprintf("  %s player-seasons | seasons %d-%d\n",
            format(nrow(sspm), big.mark = ","),
            min(sspm$season_end_year), max(sspm$season_end_year)))

# Coverage column for the events-present floor. seasonal_spm carries no box
# score, so join minutes-weighted touches per 90 from match stats. Without it,
# 18 player-seasons that have real minutes but essentially NO events (all 2024,
# all continental competitions, 0.43 touches per 90 against a population mean of
# 56.8) take the top five places on the SPMR leaderboard at +0.45 -- eight times
# the 99th percentile. A minutes floor cannot catch them; they clear 200 minutes
# comfortably. See the coverage note in fit_spmr().
ms_path <- file.path("data-raw", "cache-skills", "01_match_stats.rds")
if (file.exists(ms_path)) {
  msc <- as.data.table(readRDS(ms_path))
  msc[, .sey := as.integer(extract_season_end_year(season))]
  .mm <- as.numeric(msc$total_minutes); .mm[is.na(.mm)] <- 0
  .tt <- as.numeric(msc$touches_p90);   .tt[is.na(.tt)] <- 0
  msc[, `:=`(.mn = .mm, .tc = .tt)]
  cov <- msc[.mn > 0, .(touches_p90 = sum(.tc * .mn) / sum(.mn)),
             by = .(player_id, season_end_year = .sey)]
  sspm <- merge(sspm, cov, by = c("player_id", "season_end_year"), all.x = TRUE)
  cat(sprintf("  coverage joined for %.1f%% of player-seasons\n",
              100 * mean(!is.na(sspm$touches_p90))))
  rm(msc); gc(verbose = FALSE)
} else {
  cat("  WARNING: 01_match_stats.rds absent - coverage floor will NOT apply\n")
}

cat(sprintf("\n=== Fitting SPMR (halflife %s season(s)) ===\n", halflife_seasons))
spmr <- fit_spmr(sspm, ref_season = ref_season, halflife_seasons = halflife_seasons)

# Guard the artifact before it is written, not after it is read. career_rapm
# .parquet shipped untagged and inverted and nothing caught it for 12 days.
.assert_spmr_sign_convention(spmr)
stopifnot(nrow(spmr) > 1000,
          all(is.finite(spmr$spmr)),
          all(is.finite(spmr$ospmr)),
          all(is.finite(spmr$dspmr)))

cat(sprintf("\nSPMR: %s players | ref season %d | median %d season(s) each\n",
            format(nrow(spmr), big.mark = ","), spmr$ref_season[1],
            as.integer(median(spmr$n_seasons))))
cat(sprintf("  spmr  sd %.4f | range [%.3f, %.3f]\n",
            sd(spmr$spmr),  min(spmr$spmr),  max(spmr$spmr)))
cat(sprintf("  ospmr sd %.4f | dspmr sd %.4f\n", sd(spmr$ospmr), sd(spmr$dspmr)))

# Anchor: a DEFENSIVE rating must rate defensive positions above attacking ones.
# Cheap, and it is the check that would have caught panna#F1.
ms_path <- file.path("data-raw", "cache-skills", "01_match_stats.rds")
if (file.exists(ms_path)) {
  ms <- as.data.table(readRDS(ms_path))
  ms[, .sey := as.integer(extract_season_end_year(season))]
  ms[, .r8 := .role16_to_role8(classify_role(position, position_side))]
  .mm <- as.numeric(ms$total_minutes); .mm[is.na(.mm)] <- 0; ms[, .mn := .mm]
  rec <- ms[.sey == spmr$ref_season[1] & .mn > 0 & .r8 != "OTHER"]
  if (nrow(rec)) {
    rr <- rec[, .(m = sum(.mn)), by = .(player_id, .r8)]
    setorder(rr, player_id, -m, .r8)
    rr <- rr[, .SD[1L], by = player_id][, .(player_id, r8 = .r8)]
    chk <- merge(spmr, rr, by = "player_id")
    g <- chk[, .(n = .N, mean_d = mean(dspmr)), by = r8][order(-mean_d)]
    cat("\nANCHOR -- mean DSPMR by role (defensive roles should lead):\n")
    print(g, row.names = FALSE)
    defm <- mean(g[r8 %in% c("CB","FB","DM")]$mean_d)
    attm <- mean(g[r8 %in% c("W","AM","ST")]$mean_d)
    cat(sprintf("  defensive %+.5f vs attacking %+.5f -> %s\n", defm, attm,
                if (defm > attm) "positive=good OK" else "*** INVERTED ***"))
    if (!(defm > attm)) {
      stop("DSPMR anchor FAILED: attacking roles outrank defensive ones on a ",
           "defensive rating. Check 05_spm.R's defence sign before publishing.",
           call. = FALSE)
    }
  }
}

cat("\nTop 15 by SPMR:\n")
print(spmr[order(-spmr)][1:15, .(player_name,
      spmr = round(spmr, 4), ospmr = round(ospmr, 4), dspmr = round(dspmr, 4),
      seasons = n_seasons, mins = round(total_minutes))], row.names = FALSE)
cat("\nTop 15 by DSPMR (defensive):\n")
print(spmr[order(-dspmr)][1:15, .(player_name,
      dspmr = round(dspmr, 4), spmr = round(spmr, 4),
      seasons = n_seasons, mins = round(total_minutes))], row.names = FALSE)

out <- file.path(opta_dir, "career_spm.parquet")
arrow::write_parquet(as.data.frame(spmr), out)
cat(sprintf("\nWrote %s (%.1f MB)\n", out, file.info(out)$size / 1048576))
cat("(no upload -- local artifact; publish deliberately once reviewed)\n")
cat("=== COMPLETE ===\n")
