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
# Per-set scalar system.file: with a vector it silently DROPS missing entries
# (no "" placeholder), which would misalign a vectorized file.exists check.
missing_sets <- sets[!vapply(sets, function(p) {
  nzchar(system.file("extdata", paste0(p, "_coefficients.csv"), package = "panna"))
}, logical(1))]
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

# ---------------------------------------------------------------------------
# psv_match_reliability.csv — variance-decomposition reliability shrinkage
# (LIVE-PSV-UNBLOCK D1 v2, supersedes the v1 sd_match swap: the empirical gate
# rejected a raw sd-denominator swap because betas are calibrated to the OLD
# team-aggregated training sd, so swapping denominators re-weights features by
# sd_train/sd_match (up to 38x) and rare-but-stable-scale GK volume features
# AMPLIFIED instead of damping). v2 keeps standardization on the coefficient
# file's `sd` (unchanged, calibrated scale) and instead shrinks each stat's
# CONTRIBUTION by lambda_j = Var_between / (Var_between + Var_within) in
# [0, 1] -- the reliability of a SINGLE match as evidence of a persistent
# player level. Since lambda <= 1 always, nothing can amplify; noisy features
# (finishing spikes, GSAA, keeper sweeps) shrink hard, stable volume features
# (touches, passes) barely move. See R/psr.R calculate_psv(reliability=).
#
# Population: same enriched `ms` / `skill_cols` as position_role_means.csv
# above, split outfield/gk via .detect_gk_rows(). Per feature x is the raw
# single-match per-90 rate (ms's `_p90`/`_per90` columns), weight w =
# total_minutes/90 (mirrors 07_train's weighting and .weighted_sd_match()).
#
# Method of moments, only over players with >= 10 matches in the population
# (their within-player variance is estimable from real repetition):
#   var_i    = weighted per-player variance of x, denominator sum(w_i)
#              (.weighted_sd_match() convention)
#   n_eff_i  = sum(w_i)  (effective per-player sample size)
#   var_within  = weighted mean of var_i across players, weight = n_eff_i
#                 (pooled MSW-style estimator: more matches -> more DOF)
#   var_between = [UNWEIGHTED variance across players of their per-player
#                  weighted means] - var_within * mean(1 / n_eff_i), both the
#                  variance-of-means and the mean(1/n_eff_i) correction taken
#                  unweighted across players (each player is one draw from
#                  the population of true player levels: Var(mean_i) ~=
#                  var_between + var_within/n_eff_i per player, so weighting
#                  the LHS by n_eff_i while leaving the correction unweighted
#                  would double-count precision and suppress var_between for
#                  populations dominated by long-career low-variance players)
#   lambda = clamp(var_between / (var_between + var_within), 0, 1)
# If the attenuation-corrected var_between <= 0 (the raw signal doesn't clear
# the sampling-noise floor), lambda is set to 0 but the negative var_between
# is KEPT in the diagnostic column (no silent flooring of stored numbers).
# Too few estimable players (< 2) -> NA lambda (caller-side: treated the same
# as an absent stat, falls back to lambda = 1 with a warning).
is_gk <- .detect_gk_rows(ms)
model_pop <- data.table::fifelse(is_gk, "gk", "outfield")
w_all <- data.table::fifelse(is.na(ms$total_minutes), 0, as.numeric(ms$total_minutes) / 90)
player_id_all <- ms$player_id

.reliability_decompose <- function(x, w, player_id) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & !is.na(player_id)
  x <- x[ok]; w <- w[ok]; player_id <- player_id[ok]
  n_player_matches <- length(x)
  n_players <- data.table::uniqueN(player_id)
  sd_match <- unname(.weighted_sd_match(x, w)["sd_match"])

  if (n_players == 0L) {
    return(c(n_players = 0L, n_player_matches = 0L, sd_match = NA_real_,
             var_between = NA_real_, var_within = NA_real_, lambda = NA_real_))
  }

  dtp <- data.table::data.table(player_id = player_id, x = x, w = w)
  pp <- dtp[, .(n_i = .N, n_eff = sum(w), mean_i = sum(w * x) / sum(w)), by = player_id]
  # Per-player weighted variance, denominator sum(w_i) (.weighted_sd_match convention)
  var_by_player <- dtp[
    , .(var_i = { wm <- sum(w * x) / sum(w); sum(w * (x - wm)^2) / sum(w) }), by = player_id]
  pp <- merge(pp, var_by_player, by = "player_id")

  est <- pp[n_i >= 10]
  if (nrow(est) < 2) {
    return(c(n_players = n_players, n_player_matches = n_player_matches,
             sd_match = sd_match, var_between = NA_real_,
             var_within = NA_real_, lambda = NA_real_))
  }

  # Pooled within-player variance: weight by n_eff (more matches -> more DOF
  # -> more weight), the standard MSW-style pooled estimator.
  var_within <- sum(est$n_eff * est$var_i) / sum(est$n_eff)
  # Between-player variance: Var(mean_i) ~= var_between + var_within/n_eff_i
  # per player (each player is ONE draw from the population of true player
  # levels), so both the variance-of-means and its 1/n_eff correction must be
  # UNWEIGHTED across players -- weighting the variance-of-means by n_eff (as
  # v1 of this script did) systematically suppresses var_between whenever
  # long-career, low-variance veterans dominate the weight mass (touches/
  # passes), because it downweights the very players whose comparatively
  # different levels constitute the true between-player signal.
  grand_mean <- mean(est$mean_i)
  var_means_raw <- mean((est$mean_i - grand_mean)^2)
  atten <- var_within * mean(1 / est$n_eff)
  var_between <- var_means_raw - atten

  lambda <- if (var_between <= 0 || !is.finite(var_between)) {
    0
  } else {
    max(0, min(1, var_between / (var_between + var_within)))
  }

  c(n_players = n_players, n_player_matches = n_player_matches,
    sd_match = sd_match, var_between = var_between, var_within = var_within,
    lambda = lambda)
}

rel_rows <- rbindlist(lapply(c("outfield", "gk"), function(mdl) {
  idx <- model_pop == mdl
  w <- w_all[idx]
  pid <- player_id_all[idx]
  rbindlist(lapply(skill_cols, function(sc) {
    x <- as.numeric(ms[[sc]][idx])
    r <- .reliability_decompose(x, w, pid)
    data.table(model = mdl, stat_name = sc,
               n_players = as.integer(r["n_players"]),
               n_player_matches = as.integer(r["n_player_matches"]),
               sd_match = unname(r["sd_match"]), var_between = unname(r["var_between"]),
               var_within = unname(r["var_within"]), lambda = unname(r["lambda"]))
  }))
}))
rel_out <- file.path("inst", "extdata", "psv_match_reliability.csv")
fwrite(rel_rows, rel_out)
cli::cli_alert_success("Saved {rel_out}: {uniqueN(rel_rows$model)} models x {uniqueN(rel_rows$stat_name)} stats = {nrow(rel_rows)} rows")
cat("\nmodel population sizes (player-matches):\n")
print(data.table(model = c("outfield", "gk"), n = c(sum(!is_gk), sum(is_gk))))

cat("\nlambda summary (5 highest / 5 lowest per model):\n")
for (mdl in c("outfield", "gk")) {
  sub <- rel_rows[model == mdl & !is.na(lambda)][order(-lambda)]
  cat(sprintf("\n[%s] highest lambda:\n", mdl))
  print(head(sub[, .(stat_name, lambda, var_between, var_within, sd_match)], 5))
  cat(sprintf("\n[%s] lowest lambda:\n", mdl))
  print(tail(sub[, .(stat_name, lambda, var_between, var_within, sd_match)], 5))
}
