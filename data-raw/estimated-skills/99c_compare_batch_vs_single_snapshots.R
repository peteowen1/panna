# 99c_compare_batch_vs_single_snapshots.R
#
# panna#442 equivalence check: does the batch estimator
# (.estimate_prematch_skills_batch(as_of = TRUE), running sums) return the
# same skills as estimate_player_skills() (what 08b calls once per snapshot)
# under 08b's settings? Run this on the real caches BEFORE switching 08b over.
#
# Uses the same inputs 08b does: 01_match_stats.rds, 02b_decay_params.rds,
# min_weighted_90s = 3, and the position multipliers / prior centres computed
# once over the full data. Compares 3 snapshots (earliest 08b grid date, the
# middle one, the latest match date + 1) and prints, per date, the player-set
# difference, the primary_position mismatches and the largest skill difference
# over every stat. The two paths sum in a different order, so expect ~1e-12,
# not byte-identical output. It does NOT cover the role8 / GK lookups 08b adds
# after estimation: those read match_stats, not the skills table.
#
# Env: PANNA_CMP_CACHE_DIR (default data-raw/cache-skills),
#      PANNA_CMP_DATES (comma-separated YYYY-MM-DD, overrides the 3 defaults).
# Writes nothing. Memory: the single-date path copies match_stats once per
# date, same as 08b; run on a machine that can run 08b.

devtools::load_all()
library(data.table)

cache_dir <- Sys.getenv("PANNA_CMP_CACHE_DIR", file.path("data-raw", "cache-skills"))
ms <- readRDS(file.path(cache_dir, "01_match_stats.rds"))
setDT(ms)
if (!inherits(ms$match_date, "Date")) ms[, match_date := as.Date(match_date)]
dp_path <- file.path(cache_dir, "02b_decay_params.rds")
dp <- if (file.exists(dp_path)) readRDS(dp_path) else get_default_decay_params()

stat_cols <- .detect_skill_stat_cols(ms)
dp$position_multipliers <- compute_position_multipliers(ms, stat_cols)
w <- as.numeric(ms$total_minutes); w[is.na(w)] <- 0
dp$prior_centers <- vapply(stat_cols, function(sc) {
  v <- as.numeric(ms[[sc]]); v[is.na(v)] <- 0
  sum(v * w) / sum(w)
}, numeric(1))

dates <- Sys.getenv("PANNA_CMP_DATES", "")
dates <- if (nzchar(dates)) as.Date(strsplit(dates, ",")[[1]]) else {
  lo <- min(ms$match_date) + 365L; hi <- max(ms$match_date) + 1L
  c(lo, lo + as.integer(hi - lo) %/% 2L, hi)
}
cat("Comparing at:", format(dates), "\n")

batch <- .estimate_prematch_skills_batch(
  ms, ref_dates = dates, decay_params = dp, min_weighted_90s = 3,
  as_of = TRUE, verbose = FALSE)

ok <- TRUE
for (d in format(dates)) {
  single <- estimate_player_skills(ms, decay_params = dp, target_date = as.Date(d),
                                   min_weighted_90s = 3)
  b <- batch[[d]]
  m <- merge(single, b, by = "player_id", suffixes = c(".s", ".b"))
  pos_mis <- sum(xor(is.na(m$primary_position.s), is.na(m$primary_position.b)) |
                 (!is.na(m$primary_position.s) & !is.na(m$primary_position.b) &
                  m$primary_position.s != m$primary_position.b))
  sd_ <- vapply(stat_cols, function(sc) {
    x <- m[[paste0(sc, ".s")]]; y <- m[[paste0(sc, ".b")]]
    if (is.null(x) || is.null(y)) return(NA_real_)
    max(abs(x - y), na.rm = TRUE)
  }, numeric(1))
  worst <- names(which.max(sd_))
  cat(sprintf("%s: players single %d | batch %d | only-single %d | only-batch %d | position mismatches %d | name mismatches %d | max w90 diff %.2e | max skill diff %.2e (%s)\n",
              d, nrow(single), nrow(b),
              length(setdiff(single$player_id, b$player_id)),
              length(setdiff(b$player_id, single$player_id)),
              pos_mis, sum(m$player_name.s != m$player_name.b, na.rm = TRUE),
              max(abs(m$weighted_90s.s - m$weighted_90s.b)), max(sd_, na.rm = TRUE), worst))
  ok <- ok && nrow(single) == nrow(b) && pos_mis == 0 && max(sd_, na.rm = TRUE) < 1e-8
}
cat(if (ok) "\nEQUIVALENT (to 1e-8) on all dates.\n" else
    "\nDIFFERENCES FOUND -- do not switch 08b until explained.\n")
