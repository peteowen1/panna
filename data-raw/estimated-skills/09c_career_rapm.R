# 09c_career_rapm.R
# Career-trait RAPM = decay-weighted multi-season RAPM (halflife 365d), fitted
# WITHOUT the skill-SPM prior.
#
# Completes the career / season / decayed matrix. Before this, "decayed" existed
# only for xRAPM:
#
#   metric   career          season          decayed
#   RAPM     04_rapm (s2)    07_seasonal     THIS FILE
#   SPM      05_spm  (s2)    07_seasonal     03_skill_spm (decay-weighted skills)
#   xRAPM    06_xrapm (s2)   07_seasonal     09_career_panna
#
# Relationship to 09_career_panna.R is exactly step 4's to step 6's in stage 2:
# same fit, same splints, same decay - one shrunk to an SPM prior, one not.
# fit_career_rapm() already defaults skill_spm = NULL, so this is a supported
# path that simply had no caller.
#
# Output columns are NOT named `panna`. fit_career_rapm() returns panna /
# panna_offense / panna_defense whatever the prior, and `panna` specifically
# means the career-trait xRAPM (see .claude/rules/career-rapm.md). A
# prior-free fit carrying that name would be a third thing called panna.
#
# Inputs: cache-opta/03_splints.rds, opta_fixtures.parquet (match dates for decay).
# Output: career_rapm.parquet (player_id, player_name, career_rapm,
# career_rapm_offense, career_rapm_defense, total_minutes). No upload.

library(arrow)
library(data.table)
devtools::load_all()

cache_opta <- file.path("data-raw", "cache-opta")
opta_dir   <- file.path("..", "pannadata", "data", "opta")

# Plain exists() so a driver's globalenv flag is visible through
# source(..., local = TRUE) -- see the note in 09_career_panna.R.
halflife <- if (exists("panna_halflife_days")) panna_halflife_days else 365L

splints_path <- file.path(cache_opta, "03_splints.rds")
fx_path      <- file.path(opta_dir, "opta_fixtures.parquet")
for (p in c(splints_path, fx_path)) {
  if (!file.exists(p)) stop("Missing required input: ", p)
}

cat("\n=== Loading inputs ===\n")
sd <- readRDS(splints_path)
sd <- filter_bad_xg_data(sd, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = FALSE)$splint_data
fixtures <- as.data.table(read_parquet(fx_path))[, .(match_id, match_date)]
cat(sprintf("  splint matches: %d | fixtures: %d\n",
            data.table::uniqueN(sd$splints$match_id), nrow(fixtures)))

cat(sprintf("\n=== Fitting career RAPM (halflife %dd, NO SPM prior) ===\n", halflife))
res <- fit_career_rapm(sd, fixtures, skill_spm = NULL, halflife_days = halflife,
                       min_minutes = MIN_MINUTES_RAPM_FIT, nfolds = 10)

rap <- res$ratings[, .(player_id, player_name,
                       career_rapm         = panna,
                       career_rapm_offense = panna_offense,
                       career_rapm_defense = panna_defense,
                       total_minutes)]

cat(sprintf("\nCareer RAPM: %d players | as-of %s\n",
            nrow(rap), as.character(res$reference_date)))
stopifnot(nrow(rap) > 1000, all(is.finite(rap$career_rapm)))

cat("Top 15:\n")
print(rap[order(-career_rapm)][1:15, .(player_name,
      rapm = round(career_rapm, 3), off = round(career_rapm_offense, 3),
      def = round(career_rapm_defense, 3), mins = round(total_minutes))],
      row.names = FALSE)

out <- file.path(opta_dir, "career_rapm.parquet")
arrow::write_parquet(as.data.frame(rap), out)
cat(sprintf("\nWrote %s (%.1f MB)\n", out, file.info(out)$size / 1048576))
cat("(no upload — this artifact is local only)\n")
cat("=== COMPLETE ===\n")
