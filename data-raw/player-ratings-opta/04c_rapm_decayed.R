# 04c_rapm_decayed.R
#
# Recency-decayed RAPM target for skill-SPM (panna#257). `03_skill_spm.R`
# trained its O/D models against `04_rapm.rds` — an all-history POOLED RAPM
# fit with zero decay — while its own predictors (`02_skill_features.rds`)
# are decay-weighted (halflife ~231-347d). Target and features disagreed
# about what "now" means: a player's decayed feature snapshot reflects his
# recent form, but the model was taught to map that onto a RAPM value
# computed from his whole career, undiscounted. That is why a declined
# player (T. Müller) could rank 2nd on the target despite a decayed signal
# that had already fallen (panna#257).
#
# Fix: apply the SAME decay `fit_career_rapm()` (R/career_rapm.R) already
# uses for panna's own signal half -- `weight *= 0.5^(age_days/halflife_days)`,
# same default halflife (365d, tuned via optimize_panna_decay) -- to the RAPM
# fit that becomes skill-SPM's TRAINING TARGET. Same decay on both sides of
# the regression, not a new mechanism.
#
# Verified on the motivating case before this script existed (session
# scratch, 2026-09-16): Muller's offense value under this decay went
# 0.1873 (flat, rank 2 of 36,049) -> 0.1827 (decay-weighted, rank 6 of
# 36,049) -- moves the right direction. A harder 5-year window (the
# S6-panel approach) moves him further (rank ~15-25) but is a separate,
# already-built mechanism (04b_rapm_window_targets.R) serving a different
# consumer (seasonal xRAPM's S6 prior) -- kept distinct rather than merged,
# per Pete's call to keep this decay-only.
#
# Run from panna/ (relative cache paths assume cwd = panna/).

# 1. Setup ----

devtools::load_all()
library(arrow)
library(data.table)

cache_dir <- file.path("data-raw", "cache-opta")
opta_dir  <- file.path("..", "pannadata", "data", "opta")

halflife_days <- if (exists("skill_spm_target_halflife_days", inherits = FALSE)) {
  skill_spm_target_halflife_days
} else {
  365
}

output_path <- file.path(cache_dir, "04c_rapm_decayed.rds")

# 2. Load pooled RAPM design + match dates ----

cat("\n=== Loading pooled RAPM design ===\n")
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = TRUE)
splint_data <- filter_result$splint_data
rm(filter_result); gc(verbose = FALSE)

rapm_data <- prepare_rapm_data(splint_data, min_minutes = MIN_MINUTES_RAPM_FIT, include_covariates = TRUE)
rm(splint_data); gc(verbose = FALSE)

fixtures <- as.data.table(read_parquet(file.path(opta_dir, "opta_fixtures.parquet")))[, .(match_id, match_date)]
md <- unique(fixtures[!is.na(match_date), .(match_id, match_date = as.Date(match_date))], by = "match_id")

# 3. Apply the SAME decay fit_career_rapm() uses (R/career_rapm.R:90-100) ----

row_md <- data.table(match_id = rapm_data$row_data$match_id)
row_md[md, match_date := i.match_date, on = "match_id"]
reference_date <- max(row_md$match_date, na.rm = TRUE)
age_days <- as.numeric(reference_date - row_md$match_date)
n_missing <- sum(is.na(age_days))
if (n_missing > 0) age_days[is.na(age_days)] <- max(age_days, na.rm = TRUE)
decay <- 0.5 ^ (age_days / halflife_days)
rapm_data$weights <- rapm_data$weights * decay
cli::cli_alert_info(paste0(
  "Skill-SPM target decay: halflife {halflife_days}d | ref {as.character(reference_date)} | ",
  "weight x{round(min(decay), 4)}-{round(max(decay), 4)} | undated rows: {n_missing}"))

# 4. Fit RAPM on the decayed weights (same settings as 04_rapm.R for comparability) ----

n_obs <- sum(!is.na(rapm_data$y) & is.finite(rapm_data$y))
lambda_formula <- function(n) 16.67 * n^(-0.58)
lambda_seq <- lambda_formula(n_obs) * 2^seq(3, -3, by = -0.5)

cat("\n=== Fitting decayed RAPM ===\n")
model <- fit_rapm(
  rapm_data,
  alpha = 0,
  nfolds = 5,
  use_weights = TRUE,
  penalize_covariates = FALSE,
  parallel = FALSE,
  lambda_seq = lambda_seq
)

ratings <- extract_rapm_ratings(model)
cat("\nTop 20 players (decayed target):\n")
print(head(ratings[, c("player_name", "rapm", "offense", "defense", "total_minutes")], 20))

# 5. Save ----

rapm_decayed_results <- list(
  rapm_data = rapm_data,
  model = model,
  ratings = ratings,
  halflife_days = halflife_days,
  reference_date = reference_date,
  target_provenance = "decayed_rapm_hl365"
)
attr(rapm_decayed_results, "target_provenance") <- "decayed_rapm_hl365"

saveRDS(rapm_decayed_results, output_path)
stopifnot(nrow(ratings) >= 5000)  # standalone-safe check; validate_step_output()
                                   # needs pipeline_utils.R sourced by the driver
cat(sprintf("\nSaved to %s (%d players)\n", output_path, nrow(ratings)))
