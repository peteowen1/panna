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
n_matches_raw <- data.table::uniqueN(sd$splints$match_id)
cat(sprintf("  splint matches: %d | fixtures: %d\n", n_matches_raw, nrow(fixtures)))

# Prune splints older than PRUNE_YEARS, as 09b_career_panna_asof.R does.
#
# Principled, not just cheaper: at 8 years the decay weight is 0.5^8 = 0.004, so
# those splints contribute essentially nothing to a 365-day-halflife fit while
# still occupying the design matrix.
#
# It is also a memory bound. The unpruned sibling fit (step 9, career_panna)
# committed 71 GB on 2026-09-03 - resident stayed under 1 GB, but commit charge
# took a 31 GB box to 94% of its limit and collaterally killed a concurrent
# session's jobs. Commit headroom before this run was 63 GB, i.e. LESS than that
# peak, so running unpruned would have hit the ceiling again.
prune_years <- if (exists("career_rapm_prune_years")) career_rapm_prune_years else 8L

# match_date is CHARACTER ("2022-02-06Z"), not Date - arithmetic on it errors.
fixtures[, match_date_d := as.Date(substr(as.character(match_date), 1, 10))]

# Reference from the SPLINTS, not from the fixtures table. opta_fixtures runs to
# 2027-05-30 (scheduled future matches), so max(fixtures$match_date) would put
# the reference in the future and slide the whole 8-year window forward, keeping
# a different span than intended. The latest PLAYED match in the splint set is
# the honest "as of now".
ref_date <- max(fixtures[match_id %in% sd$splints$match_id, match_date_d], na.rm = TRUE)
keep_ids <- fixtures[!is.na(match_date_d) &
                     match_date_d > (ref_date - as.integer(prune_years) * 365L), match_id]
sd$splints <- sd$splints[sd$splints$match_id %in% keep_ids, ]
if (!is.null(sd$match_info)) {
  sd$match_info <- sd$match_info[sd$match_info$match_id %in% keep_ids, ]
}
n_matches <- data.table::uniqueN(sd$splints$match_id)
cat(sprintf("  pruned to >%dyr: %d -> %d matches (%.1f%% kept), ref %s\n",
            prune_years, n_matches_raw, n_matches,
            100 * n_matches / n_matches_raw, as.character(ref_date)))
stopifnot(n_matches > 1000)

cat(sprintf("\n=== Fitting career RAPM (halflife %dd, NO SPM prior) ===\n", halflife))
# THREE constraints, found the hard way (two earlier versions of this block
# each satisfied one and violated another):
#
#   1. fit_career_rapm() itself gates on `is.null(offense_prior) ||
#      is.null(defense_prior)` and then REQUIRES skill_spm if so
#      (career_rapm.R:75-78) - so offense_prior/defense_prior = NULL,NULL
#      reproduces the exact "Provide skill_spm" stop() this script exists to
#      avoid. NULL is not a valid "no prior" signal to THIS function.
#   2. But a non-NULL prior that matches ZERO of the design's players aborts
#      in R/rapm_model.R's D4 guard (FABLE-PRIOR-FIX-PLAN.md, citing the real
#      06_xrapm.R multi-target L3 incident): "A supplied-but-unmatched prior
#      is always a bug, never a valid zero-prior fallback." Ruled out
#      setNames(0, "placeholder").
#   3. Every value actually written must be 0, so the offset
#      `y_adjusted <- y - X %*% prior_vec` is a true no-op and the final
#      coefficient (gamma + prior_vec) reduces to gamma - algebra confirmed
#      independently in scratchpad/verify_zero_prior2.R.
#
# Resolution: a genuine all-zero vector named by the FULL player_id set in the
# cached splint data (sd$players$player_id - the players table `.fill_prior()`
# ultimately matches against, unfiltered by min_minutes so it is a superset of
# whatever prepare_rapm_data() selects internally). Non-NULL (satisfies 1),
# guaranteed to match every one of the design's players (satisfies 2), all
# zero (satisfies 3).
stopifnot("player_id" %in% names(sd$players))
all_pids <- unique(sd$players$player_id)
zero_prior <- stats::setNames(rep(0, length(all_pids)), all_pids)
cat(sprintf("  zero-prior vector: %s players (full cached set)\n",
            format(length(all_pids), big.mark = ",")))
res <- fit_career_rapm(sd, fixtures, skill_spm = NULL,
                       offense_prior = zero_prior, defense_prior = zero_prior,
                       halflife_days = halflife,
                       min_minutes = MIN_MINUTES_RAPM_FIT, nfolds = 10)

rap <- res$ratings[, .(player_id, player_name,
                       career_rapm         = panna,
                       career_rapm_offense = panna_offense,
                       career_rapm_defense = panna_defense,
                       total_minutes)]

# Report SAMPLE SIZE alongside fit quality. A prune that improves the fit while
# halving the effective sample has changed two things at once, and the second
# constrains everything measured afterwards - so both numbers travel together,
# never the fit quality alone.
cat(sprintf("\nCareer RAPM: %d players | as-of %s | from %d matches (pruned >%dyr)\n",
            nrow(rap), as.character(res$reference_date), n_matches, prune_years))
cat(sprintf("  rating sd %.4f | range [%.3f, %.3f] | median minutes %.0f\n",
            stats::sd(rap$career_rapm), min(rap$career_rapm), max(rap$career_rapm),
            stats::median(rap$total_minutes)))
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
