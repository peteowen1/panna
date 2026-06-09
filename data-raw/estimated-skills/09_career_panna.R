# 09_career_panna.R
# Career-trait Panna = decay-weighted multi-season xRAPM (halflife 365d) shrunk to
# the career-trait skill-SPM prior. See CLAUDE_TODO_CAREER_PANNA.md.
#
# DISTINCT from per-season `xrapm` (player-ratings-opta seasonal export). One rating
# per player, as-of-now: "how good is this player / next-game impact".
#
# Inputs (both present after the skills pipeline): cache-opta/03_splints.rds (RAPM
# splints), cache-skills/03_skill_spm.rds (career-trait SPM prior), and
# opta_fixtures.parquet (exact match dates for the recency decay).
# Output: career_panna.parquet (player_id, player_name, panna, panna_offense,
# panna_defense, total_minutes), optionally uploaded to the ratings-data release.

library(arrow)
library(data.table)
devtools::load_all()

cache_opta   <- file.path("data-raw", "cache-opta")
cache_skills <- file.path("data-raw", "cache-skills")
opta_dir     <- file.path("..", "pannadata", "data", "opta")

halflife <- if (exists("panna_halflife_days")) panna_halflife_days else 365L
upload   <- if (exists("upload_career_panna", inherits = FALSE)) upload_career_panna else FALSE

splints_path <- file.path(cache_opta, "03_splints.rds")
spm_path     <- file.path(cache_skills, "03_skill_spm.rds")
fx_path      <- file.path(opta_dir, "opta_fixtures.parquet")
for (p in c(splints_path, spm_path, fx_path)) {
  if (!file.exists(p)) stop("Missing required input: ", p)
}

cat("\n=== Loading inputs ===\n")
sd <- readRDS(splints_path)
sd <- filter_bad_xg_data(sd, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = FALSE)$splint_data
skill_spm <- readRDS(spm_path)
fixtures  <- as.data.table(read_parquet(fx_path))[, .(match_id, match_date)]
cat(sprintf("  splint matches: %d | fixtures: %d | skill-SPM players: %d\n",
            data.table::uniqueN(sd$splints$match_id), nrow(fixtures),
            nrow(skill_spm$offense_spm_ratings)))

cat(sprintf("\n=== Fitting career Panna (halflife %dd) ===\n", halflife))
res <- fit_career_rapm(sd, fixtures, skill_spm = skill_spm, halflife_days = halflife,
                       min_minutes = MIN_MINUTES_RAPM_FIT, nfolds = 10)
panna <- res$ratings[, .(player_id, player_name, panna, panna_offense, panna_defense, total_minutes)]

cat(sprintf("\nCareer Panna: %d players | as-of %s\n", nrow(panna), as.character(res$reference_date)))
cat("Top 15:\n")
print(panna[order(-panna)][1:15, .(player_name, panna = round(panna, 3),
            off = round(panna_offense, 3), def = round(panna_defense, 3),
            mins = round(total_minutes))], row.names = FALSE)

out <- file.path(opta_dir, "career_panna.parquet")
arrow::write_parquet(as.data.frame(panna), out)
cat(sprintf("\nWrote %s (%.1f MB)\n", out, file.info(out)$size / 1048576))

if (isTRUE(upload)) {
  piggyback::pb_upload(out, repo = "peteowen1/pannadata", tag = "ratings-data",
                       name = "career_panna.parquet", overwrite = TRUE)
  cat("Uploaded career_panna.parquet to ratings-data release.\n")
} else {
  cat("(upload skipped — set upload_career_panna <- TRUE before sourcing to publish)\n")
}
cat("=== COMPLETE ===\n")
