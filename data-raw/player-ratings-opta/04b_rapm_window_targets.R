# 04b_rapm_window_targets.R
#
# Builds windowed, prior-free RAPM targets for the SPM box-score-value
# redesign (pannaverse/docs/plans/BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.1).
#
# For each vintage year Y, fits a PRIOR-FREE ridge RAPM (fit_rapm(), alpha =
# 0) on the pooled splint design restricted to season_end_year in
# [Y - window_years, Y) -- a hard rolling window, not the full expanding
# history -- via the generalized .subset_rapm_data_expanding()/
# fit_expanding_pooled_rapm() (R/spm_asof.R, min_year argument). Mirrors the
# orchestration style of data-raw/estimated-skills/03_skill_spm.R sec 12
# (as-of fits): resumable per-Y, checkpointed after every year.
#
# BANNED as inputs to this script: cache-opta/06_xrapm.rds (xRAPM --
# fit_rapm_with_prior() embeds the SPM prior), career_panna.parquet /
# 09_career_panna.R output (embeds the skill-SPM prior), anything from
# extract_xrapm_ratings(). Only cache-opta/04_rapm.rds$rapm_data (fit_rapm()
# design matrix, no prior) and cache-opta/03_splints.rds (season map) are
# read. The output artifact is stamped target_provenance =
# "prior_free_rapm_window" (both top-level and per-vintage) -- this is the
# ONLY script that writes that stamp; assert_prior_free_target()
# (R/spm_asof.R) enforces it downstream.
#
# Run from panna/ (relative cache paths assume cwd = panna/, project
# convention -- see pannaverse/CLAUDE.md's wrong-cwd trap).

# 1. Setup ----

devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")

window_years <- if (exists("window_years", inherits = FALSE)) window_years else 5L
vintage_years <- if (exists("vintage_years", inherits = FALSE)) vintage_years else 2019:2026
force_rebuild <- if (exists("force_rebuild", inherits = FALSE)) force_rebuild else FALSE
window_nfolds <- if (exists("window_nfolds", inherits = FALSE)) window_nfolds else 5
window_lambda_formula <- if (exists("window_lambda_formula", inherits = FALSE)) {
  window_lambda_formula
} else {
  function(n) 16.67 * n^(-0.58)
}
window_seed <- if (exists("window_seed", inherits = FALSE)) window_seed else 20260721L

output_path <- if (exists("output_path", inherits = FALSE)) output_path else
  file.path(cache_dir, "rapm_window_targets.rds")

# 2. Resume ----

rapm_window_targets <- list()
if (isTRUE(force_rebuild) && file.exists(output_path)) {
  cat(sprintf("force_rebuild = TRUE: ignoring existing %s\n", basename(output_path)))
} else if (file.exists(output_path)) {
  rapm_window_targets <- readRDS(output_path)
  cat(sprintf("resume: %d vintage(s) already in %s\n",
              length(rapm_window_targets), basename(output_path)))
}

todo_years <- vintage_years[!as.character(vintage_years) %in% names(rapm_window_targets)]
cat(sprintf("%d vintage year(s) to fit: %s\n", length(todo_years),
            paste(todo_years, collapse = ", ")))

# 3. Load pooled RAPM design + season map ----

if (length(todo_years) > 0) {
  cat("\n=== Loading pooled RAPM design + season map ===\n")

  r4 <- readRDS(file.path(cache_dir, "04_rapm.rds"))
  pooled_rapm_data <- r4$rapm_data
  rm(r4); gc(verbose = FALSE)

  splints03 <- readRDS(file.path(cache_dir, "03_splints.rds"))
  splint_season_map <- splints03$splints[, c("splint_id", "season_end_year")]
  rm(splints03); gc(verbose = FALSE)

  # 4. Per-vintage windowed prior-free RAPM ----

  for (Y in todo_years) {
    min_year <- Y - window_years
    cat(sprintf("\n--- vintage %d (train on seasons [%d, %d)) ---\n", Y, min_year, Y))
    t0 <- Sys.time()

    fit <- fit_expanding_pooled_rapm(
      pooled_rapm_data, splint_season_map, cutoff_year = Y, min_year = min_year,
      lambda_formula = window_lambda_formula, nfolds = window_nfolds, seed = window_seed
    )
    if (is.null(fit)) next

    rapm_window_targets[[as.character(Y)]] <- list(
      ratings = fit$ratings,
      lambda_min = fit$lambda_min,
      n_obs = fit$n_obs,
      window = c(min_year, Y),
      target_provenance = "prior_free_rapm_window"
    )
    attr(rapm_window_targets, "target_provenance") <- "prior_free_rapm_window"
    saveRDS(rapm_window_targets, output_path)  # checkpoint after every vintage

    cat(sprintf("  vintage %d done in %.1f min (n_obs=%d, lambda.min=%.5f)\n",
                Y, as.numeric(difftime(Sys.time(), t0, units = "mins")),
                fit$n_obs, fit$lambda_min))
    gc(verbose = FALSE)
  }

  rm(pooled_rapm_data, splint_season_map); gc(verbose = FALSE)
} else {
  cat("\nNothing to do -- all requested vintages already cached.\n")
}

attr(rapm_window_targets, "target_provenance") <- "prior_free_rapm_window"
saveRDS(rapm_window_targets, output_path)

cat(sprintf("\nSaved %d windowed prior-free RAPM vintage(s) to %s\n",
            length(rapm_window_targets), output_path))
