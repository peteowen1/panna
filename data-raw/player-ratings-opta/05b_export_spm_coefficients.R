# 05b_export_spm_coefficients.R
# Export SPM's fitted glmnet coefficients to CSV for live per-match scoring
# parity with PSR/OSR/DSR (panna#173). See export_spm_coefficients_csv()'s
# docs (R/spm_model.R) for why no serve-time sd standardization is needed
# here, unlike PSR/OSR/DSR.
#
# Outputs:
#   inst/extdata/spm_coefficients.csv      (overall/margin SPM)
#   inst/extdata/spm_osr_coefficients.csv  (offensive SPM)
#   inst/extdata/spm_dsr_coefficients.csv  (defensive SPM)
#
# Reads the already-fitted, already-cached models from cache-opta/05_spm.rds
# -- does not refit anything.

devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")
extdata_dir <- file.path("inst", "extdata")
dir.create(extdata_dir, showWarnings = FALSE, recursive = TRUE)

spm_results <- readRDS(file.path(cache_dir, "05_spm.rds"))

cat("=== Exporting SPM coefficients (panna#173) ===\n\n")

exports <- list(
  list(model = spm_results$spm_glmnet, file = "spm_coefficients.csv", label = "overall"),
  list(model = spm_results$offense_spm_glmnet, file = "spm_osr_coefficients.csv", label = "offense"),
  list(model = spm_results$defense_spm_glmnet, file = "spm_dsr_coefficients.csv", label = "defense")
)

for (e in exports) {
  out_path <- file.path(extdata_dir, e$file)
  out <- export_spm_coefficients_csv(e$model, out_path)
  cat(sprintf("wrote %s: %d non-zero features (%s)\n", out_path, nrow(out), e$label))
}

cat("\nDone.\n")
