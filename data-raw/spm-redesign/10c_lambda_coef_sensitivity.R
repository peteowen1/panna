# 10c_lambda_coef_sensitivity.R
#
# Wave 2 coefficient-level lambda-sensitivity (BOX-SCORE-VALUE-SPM-REDESIGN.md
# sec 2.3.3 / kill criterion 5.4.2): Wave 1 established that the TARGETS'
# player ranks are lambda-stable (all 8 vintages Spearman ~0.93+); this
# script asks the question that actually gates per-game pricing -- are the
# fitted COEFFICIENTS (the prices) stable when the target is rebuilt at
# lambda in {1/4, 1/2, 1, 2, 4} x lambda.min?
#
# Method: swap the panel's offense/defense targets for each vintage's
# lambda-m ratings (cache-opta/rapm_window_lambda_grid.rds, built by
# 01_lambda_sensitivity_targets.R -- prior-free fit_rapm() fits), refit the
# S4a candidate on the FULL panel per lambda, and compare coefficient
# vectors against the x1 baseline: Spearman rank correlation (kill bar
# < 0.9), sign flips among coefficients nonzero in either fit, and top-20
# |coef| overlap.
#
# Output: data-raw/spm-redesign/wave2_lambda_coef_sensitivity.csv
#
# Run from panna/:
#   Rscript data-raw/spm-redesign/10c_lambda_coef_sensitivity.R

source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))
suppressMessages(library(data.table))

cache_dir <- file.path("data-raw", "cache-opta")
out_dir <- file.path("data-raw", "spm-redesign")

panel_bundle <- readRDS(file.path(cache_dir, "spm_panel.rds"))
panel <- panel_bundle$panel
attr(panel, "target_provenance") <- panel_bundle$target_provenance
grid <- readRDS(file.path(cache_dir, "rapm_window_lambda_grid.rds"))

mults <- c("x0.25", "x0.5", "x1", "x2", "x4")

swap_targets <- function(panel, mult) {
  out <- data.table::copy(panel)
  for (v in as.character(sort(unique(out$vintage_year)))) {
    entry <- grid[[v]]
    if (is.null(entry)) cli::cli_abort("No lambda-grid entry for vintage {v}.")
    r <- data.table::as.data.table(entry$fits[[mult]]$ratings)
    idx <- match(out[vintage_year == as.integer(v), player_id], r$player_id)
    out[vintage_year == as.integer(v), `:=`(
      offense_target = r$offense[idx],
      defense_target = r$defense[idx],
      rapm_target = r$rapm[idx]
    )]
  }
  # Same prior-free provenance: the grid fits are fit_rapm() (no prior),
  # only lambda differs (01_lambda_sensitivity_targets.R).
  attr(out, "target_provenance") <- attr(panel, "target_provenance")
  out
}

coef_vec <- function(fit) {
  cf <- as.matrix(stats::coef(fit, s = "lambda.min"))
  v <- cf[rownames(cf) != "(Intercept)", 1]
  v
}

fits_by_mult <- list()
for (m in mults) {
  cli::cli_h2(sprintf("Fitting S4a with %s targets", m))
  panel_m <- swap_targets(panel, m)
  fits_by_mult[[m]] <- run_candidate(panel_m, candidate_configs$S4a, seed = 1)
}

rows <- list()
for (target in c("offense", "defense")) {
  base <- coef_vec(fits_by_mult[["x1"]][[target]])
  for (m in mults) {
    v <- coef_vec(fits_by_mult[[m]][[target]])
    stopifnot(identical(names(v), names(base)))
    nz <- abs(v) > 1e-10 | abs(base) > 1e-10
    flips <- sum(sign(v[nz]) * sign(base[nz]) < 0)
    top_base <- names(sort(abs(base), decreasing = TRUE))[1:20]
    top_v <- names(sort(abs(v), decreasing = TRUE))[1:20]
    rows[[paste(target, m)]] <- data.table(
      target = target, mult = m,
      spearman_vs_x1 = stats::cor(v, base, method = "spearman"),
      spearman_nonzero = stats::cor(v[nz], base[nz], method = "spearman"),
      n_nonzero = sum(abs(v) > 1e-10), n_sign_flips = flips,
      top20_overlap = length(intersect(top_base, top_v)) / 20,
      scale_ratio = stats::sd(v) / stats::sd(base)
    )
  }
}
report <- rbindlist(rows)
fwrite(report, file.path(out_dir, "wave2_lambda_coef_sensitivity.csv"))
cli::cli_h1("Coefficient-level lambda sensitivity (S4a, vs x1 baseline)")
print(report)

worst <- report[mult != "x1", min(spearman_nonzero)]
if (worst < 0.9) {
  cli::cli_alert_danger(sprintf("KILL BAR (sec 5.4.2): min nonzero Spearman %.3f < 0.9 -- prices are regularisation artifacts at grid edges.", worst))
} else {
  cli::cli_alert_success(sprintf("Kill bar passed: min nonzero Spearman %.3f (>= 0.9).", worst))
}
