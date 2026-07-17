# 07b. Build the within-position normalization artifact (per-role skill means)
# ===========================================================================
# Reproducible build of inst/extdata/position_role_means.csv — the per-position
# (broad GK/DEF/MID/FWD bucket) mean of every PSR/PSV skill feature. Subtracted
# at scoring (compute_player_psv/psr, position_means=) so a player is valued vs
# their ROLE, not vs all outfielders (BPM/VORP-style). See R/psr.R .player_role.
#
# Source population: the full box-score match_stats (cache-skills/01_match_stats),
# enriched with the per-match xMetrics (incl. the 5 duel WOE) so the artifact
# covers EXACTLY the scored feature set. Run AFTER step 03 (xmetrics) + step 01.
#   cd panna && Rscript data-raw/estimated-skills/07b_build_position_means.R

suppressMessages(devtools::load_all(".", quiet = TRUE))
library(data.table)

ms <- as.data.table(readRDS("data-raw/cache-skills/01_match_stats.rds"))
cli::cli_alert_info("Loaded match_stats: {format(nrow(ms), big.mark=',')} player-matches")
# Full history: compute_position_role_means keys by (season_end_year x role), so
# era drift is handled by per-era baselines (not a recent-window crop). Each
# game-log — current or historical — gets its own era's positional baseline.
ms <- enrich_match_stats_with_xmetrics(ms, verbose = TRUE, fail_if_missing_frac = 0.6)

# Scored feature set = union of stat_names across all trained coefficient sets.
# GK sets are gk_{psr,osr,dsr} — NOT gk_blend_* (those files never existed; the
# wrong names silently dropped every GK-only stat from this artifact until
# panna#144, because of the file.exists() guard below).
sets <- c("blend_psr","blend_osr","blend_dsr","gk_psr","gk_osr","gk_dsr",
          "psr","osr","dsr","gd_psr","gd_osr","gd_dsr")
missing_sets <- sets[!file.exists(system.file("extdata", paste0(sets, "_coefficients.csv"), package = "panna"))]
if (length(missing_sets)) cli::cli_abort("Coefficient set(s) not found (typo in `sets`?): {paste(missing_sets, collapse=', ')}")
skill_cols <- unique(unlist(lapply(sets, function(p){
  f <- system.file("extdata", paste0(p, "_coefficients.csv"), package = "panna")
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE)$stat_name else character(0)
})))
skill_cols <- intersect(skill_cols, names(ms))

pm <- compute_position_role_means(ms, skill_cols)
out <- file.path("inst", "extdata", "position_role_means.csv")
fwrite(pm, out)
cli::cli_alert_success("Saved {out}: {uniqueN(pm$role)} roles x {uniqueN(pm$stat_name)} stats = {nrow(pm)} rows")
cat("\nrole coverage (player-matches):\n")
ms[, .role := .player_role(ms)]
print(ms[, .(player_matches = .N), by = .role][order(-player_matches)])
