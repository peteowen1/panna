# 03_attenuation_diagnostics.R
#
# Wave-1 shrinkage-attenuation diagnostics on the windowed prior-free RAPM
# TARGET, for the 2026 vintage (pannaverse/docs/plans/BOX-SCORE-VALUE-SPM-
# REDESIGN.md sec 2.3, sec 2.3.2 EIV candidate). This seeds the Wave-2
# errors-in-variables candidate -- no modeling decision is made here.
#
# Reliability estimate: r_hat_i = m_i / (m_i + m_0), m_i = window minutes
# (from rapm_window_lambda_grid.rds$window_minutes, the same windowed-weight
# derivation used in 01/02). m_0 (implied prior minutes at 0 the ridge penalty
# is equivalent to) is derived empirically: a near-unregularized reference fit
# (fixed_lambda = lambda.min / 50, far less shrinkage) stands in for the
# "true" unshrunk coefficient; |rating| shrinkage_i = |rapm_shrunk_i| /
# |rapm_unshrunk_i| is regressed on window minutes via nls to the ridge
# reliability form m_i / (m_i + m_0) (median-ratio fallback if nls doesn't
# converge). This is the "regress |rating| shrinkage vs minutes" proxy named
# in the BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.3.2 Wave-1 study brief.
#
# Run from panna/ (relative cache paths assume cwd = panna/).

# 1. Setup ----

devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")
if (!exists("vintage_year", inherits = FALSE)) vintage_year <- 2026L
if (!exists("unshrunk_lambda_frac", inherits = FALSE)) unshrunk_lambda_frac <- 1 / 50
if (!exists("eiv_floor", inherits = FALSE)) eiv_floor <- 0.4
if (!exists("minutes_bands", inherits = FALSE)) {
  minutes_bands <- c(0, 900, 1800, 3600, Inf)
  minutes_band_labels <- c("<900", "900-1800", "1800-3600", ">3600")
}

output_dir <- if (exists("output_dir", inherits = FALSE)) output_dir else
  file.path("data-raw", "spm-redesign")

grid_path <- file.path(cache_dir, "rapm_window_lambda_grid.rds")
if (!file.exists(grid_path)) {
  cli::cli_abort("Expected {.file {grid_path}} -- run 01_lambda_sensitivity_targets.R first.")
}
rapm_window_lambda_grid <- readRDS(grid_path)
entry <- rapm_window_lambda_grid[[as.character(vintage_year)]]
if (is.null(entry)) {
  cli::cli_abort("Vintage {vintage_year} not found in {.file {grid_path}}.")
}

shrunk <- entry$fits[["x1"]]$ratings  # lambda.min
lambda_min_Y <- entry$lambda_min_baseline
window_minutes <- entry$window_minutes

# 2. Near-unregularized reference fit (unshrunk stand-in) ----

unshrunk_path <- file.path(cache_dir, sprintf("rapm_unshrunk_ref_%d.rds", vintage_year))
if (file.exists(unshrunk_path)) {
  cat(sprintf("resume: reusing cached near-unregularized reference fit at %s\n", unshrunk_path))
  unshrunk <- readRDS(unshrunk_path)
} else {
  cat("\n=== Loading pooled RAPM design + season map (near-unregularized reference fit) ===\n")
  r4 <- readRDS(file.path(cache_dir, "04_rapm.rds"))
  pooled_rapm_data <- r4$rapm_data
  rm(r4); gc(verbose = FALSE)

  splints03 <- readRDS(file.path(cache_dir, "03_splints.rds"))
  splint_season_map <- splints03$splints[, c("splint_id", "season_end_year")]
  rm(splints03); gc(verbose = FALSE)

  rapm_sub <- .subset_rapm_data_expanding(pooled_rapm_data, splint_season_map,
                                          cutoff_year = vintage_year,
                                          min_year = vintage_year - 5L)
  rm(pooled_rapm_data, splint_season_map); gc(verbose = FALSE)

  unshrunk_lambda <- lambda_min_Y * unshrunk_lambda_frac
  cat(sprintf("Fitting near-unregularized reference at lambda = %.6f (x%.3g of lambda.min)\n",
              unshrunk_lambda, unshrunk_lambda_frac))
  model <- fit_rapm(rapm_sub, alpha = 0, use_weights = TRUE, penalize_covariates = FALSE,
                    parallel = FALSE, fixed_lambda = unshrunk_lambda)
  unshrunk <- extract_rapm_ratings(model, lambda = "min")[, c("player_id", "rapm", "offense", "defense")]
  saveRDS(unshrunk, unshrunk_path)
  rm(model, rapm_sub); gc(verbose = FALSE)
}

# 3. Per-player |rating| shrinkage vs window minutes -> m_0 ----

merged <- merge(shrunk[, c("player_id", "rapm")], unshrunk[, c("player_id", "rapm")],
                by = "player_id", suffixes = c("_shrunk", "_unshrunk"))
merged <- merge(merged, window_minutes, by = "player_id")
merged <- merged[is.finite(merged$rapm_unshrunk) & abs(merged$rapm_unshrunk) > 1e-6, ]
merged$shrinkage <- pmin(abs(merged$rapm_shrunk) / abs(merged$rapm_unshrunk), 1.5)

fit_m0 <- tryCatch({
  nls_fit <- stats::nls(shrinkage ~ window_minutes / (window_minutes + m0),
                        data = merged, start = list(m0 = stats::median(merged$window_minutes)))
  stats::coef(nls_fit)[["m0"]]
}, error = function(e) NA_real_)

if (is.na(fit_m0) || fit_m0 <= 0) {
  cat("nls fit for m0 did not converge -- falling back to median-ratio proxy.\n")
  z <- merged$window_minutes * (1 - merged$shrinkage) / pmax(merged$shrinkage, 1e-6)
  fit_m0 <- stats::median(z[is.finite(z) & z > 0])
}

cat(sprintf("\nImplied m0 (prior minutes at 0 the lambda.min ridge penalty is equivalent to): %.1f\n", fit_m0))

# 4. Reliability r_hat_i and capped-EIV candidate ----

window_minutes$r_hat <- window_minutes$window_minutes / (window_minutes$window_minutes + fit_m0)
scored <- merge(shrunk[, c("player_id", "rapm", "offense", "defense")], window_minutes, by = "player_id")
scored$r_hat_capped <- pmax(scored$r_hat, eiv_floor)
scored$eiv_rapm <- scored$rapm / scored$r_hat_capped
scored$eiv_offense <- scored$offense / scored$r_hat_capped
scored$eiv_defense <- scored$defense / scored$r_hat_capped

scored$minutes_band <- cut(scored$window_minutes, breaks = minutes_bands,
                           labels = minutes_band_labels, right = FALSE)

band_summary <- do.call(rbind, lapply(minutes_band_labels, function(b) {
  sub <- scored[scored$minutes_band == b & !is.na(scored$minutes_band), ]
  data.frame(
    minutes_band = b,
    n_players = nrow(sub),
    mean_window_minutes = mean(sub$window_minutes),
    mean_r_hat = mean(sub$r_hat),
    median_r_hat = stats::median(sub$r_hat),
    mean_rapm_shrunk = mean(sub$rapm),
    sd_rapm_shrunk = stats::sd(sub$rapm),
    mean_eiv_rapm = mean(sub$eiv_rapm),
    sd_eiv_rapm = stats::sd(sub$eiv_rapm)
  )
}))

cat("\n=== Reliability + capped-EIV candidate by window-minutes band (vintage ", vintage_year, ") ===\n", sep = "")
print(band_summary, row.names = FALSE)

cat(sprintf("\nOverall r_hat distribution: mean=%.3f median=%.3f p10=%.3f p90=%.3f\n",
            mean(scored$r_hat), stats::median(scored$r_hat),
            stats::quantile(scored$r_hat, 0.1), stats::quantile(scored$r_hat, 0.9)))
cat(sprintf("Capped-EIV rapm (y / max(r_hat, %.1f)) overall: mean=%.4f sd=%.4f (raw rapm: mean=%.4f sd=%.4f)\n",
            eiv_floor, mean(scored$eiv_rapm), stats::sd(scored$eiv_rapm),
            mean(scored$rapm), stats::sd(scored$rapm)))

# 5. Write outputs -- small aggregate table -> CSV in the study dir (tracked
# deliverable); ~22K-row player-level table -> parquet in cache_dir
# (gitignored, regenerable intermediate, not a tracked deliverable) ----

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(band_summary, file.path(output_dir, "attenuation_band_summary.csv"), row.names = FALSE)
player_scores_path <- file.path(cache_dir, "attenuation_player_scores.parquet")
arrow::write_parquet(
  scored[, c("player_id", "window_minutes", "r_hat", "minutes_band",
            "rapm", "offense", "defense", "eiv_rapm", "eiv_offense", "eiv_defense")],
  player_scores_path
)
cat(sprintf("\nWrote %s\n", file.path(output_dir, "attenuation_band_summary.csv")))
cat(sprintf("Wrote %s\n", player_scores_path))
cat(sprintf("m0 = %.1f -- see script header for derivation\n", fit_m0))
