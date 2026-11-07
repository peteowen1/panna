# Shared by tests/testthat/test-psr.R. testthat loads helper-*.R before any test file.
# PSR/PSV test fixtures once carried only a few stats (e.g. goals_p90 + gsaa_per90), which broke on the
# 2026-11-07 retrain (panna#432): glmnet dropped goals_p90 from the blend and
# goal-diff models, leaving the outfield branch with "No matching skill columns".
# Add every stat any shipped coefficient file names (at 0), so each sub-model has
# columns to score whichever stats a retrain keeps.
.with_all_coef_cols <- function(dt) {
  files <- list.files(system.file("extdata", package = "panna"),
                      pattern = "_coefficients\\.csv$", full.names = TRUE)
  stats <- unique(unlist(lapply(files, function(f) utils::read.csv(f)$stat_name)))
  # Not the is_gk / is_df / is_mf / is_fw position flags: a 0 there would
  # override the position resolution these tests are about.
  stats <- stats[!grepl("^is_", stats)]
  stopifnot(length(stats) > 0)
  for (s in setdiff(stats, names(dt))) data.table::set(dt, j = s, value = 0)
  dt
}
