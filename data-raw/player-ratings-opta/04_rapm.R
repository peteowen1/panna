# 04_rapm.R
# Fit base RAPM model on Opta splints
#
# Near-identical to FBref version. Uses SPADL-derived xG as target.
# Structure: 2 rows per splint (one per attacking perspective),
# target = xgf90, player columns = offense/defense.

# 1. Setup ----

library(dplyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-opta")

# panna#87 OOM mitigation (option A): env-gated CV skip. When OPTA_FIXED_LAMBDA
# is set (any non-empty value, e.g. "1" in the GHA workflow), skip cv.glmnet and
# fit each RAPM at the closed-form lambda = 16.67 * n_obs^-0.58 (R^2=0.96 vs
# lambda.min). cv.glmnet does 10 folds + a final fit = 11 dense refits per call,
# and this step runs ~4 such fits back-to-back — the spike that OOMs the 16GB
# runner. Default (unset) keeps the current cross-validated behaviour unchanged.
use_fixed_lambda <- nzchar(Sys.getenv("OPTA_FIXED_LAMBDA"))
# Sample-size lambda formula (same as data-raw/estimated-skills/09b_career_panna_asof.R).
lambda_formula <- function(n) 16.67 * n^(-0.58)
# n_obs for a fit = count of valid (non-NA, finite) responses, matching the
# count fit_rapm()/fit_rapm_with_prior() use internally.
.n_obs_valid <- function(rd) sum(!is.na(rd$y) & is.finite(rd$y))

# 2. Load Splint Data ----

cat("\n=== Loading Splint Data ===\n")
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

cat("Raw splints:", nrow(splint_data$splints), "\n")
cat("Raw players:", nrow(splint_data$players), "\n")

# Filter out league-seasons with bad xG data
# Opta uses SPADL-derived xG so ~25% zero-xG splints is normal (short splints
# without shots). Threshold of 30% avoids discarding valid league-seasons where
# zero-xG splints are inherent to the SPADL conversion process.
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = TRUE)
splint_data <- filter_result$splint_data

cat("\nAfter filtering:\n")
cat("  Splints:", nrow(splint_data$splints), "\n")
cat("  Players:", nrow(splint_data$players), "\n")

# Free memory
rm(filter_result); gc(verbose = FALSE)

# 3. Create RAPM Design Matrix ----

cat("\n=== Creating RAPM Design Matrix ===\n")

rapm_data <- prepare_rapm_data(
  splint_data,
  min_minutes = MIN_MINUTES_RAPM_FIT,
  include_covariates = TRUE
)

# Free memory
rm(splint_data); gc(verbose = FALSE)

cat("\nDesign matrix summary:\n")
cat("  Rows:", rapm_data$n_rows, "\n")
cat("  Players:", rapm_data$n_players, "\n")
cat("  Player columns:", rapm_data$n_players * 2, "(off + def)\n")
cat("  Covariates:", length(rapm_data$covariate_names), "\n")

if (length(rapm_data$covariate_names) > 0) {
  cat("\nCovariates included:\n")
  for (cov in rapm_data$covariate_names) {
    cat("  -", cov, "\n")
  }
}

if (!is.null(rapm_data$leagues)) {
  cat("\nLeagues:", paste(rapm_data$leagues, collapse = ", "), "\n")
}
if (!is.null(rapm_data$seasons)) {
  cat("Seasons:", paste(rapm_data$seasons, collapse = ", "), "\n")
}

# 4. Fit RAPM Model ----

cat("\n=== Fitting RAPM Model ===\n")

# panna#87: fixed-lambda mode skips CV (the 11-refit memory spike); else CV.
base_fixed_lambda <- if (use_fixed_lambda) lambda_formula(.n_obs_valid(rapm_data)) else NULL
if (use_fixed_lambda) {
  cli::cli_alert_info(
    "RAPM fixed-lambda mode (CV skipped), lambda={round(base_fixed_lambda, 5)} (n_obs={.n_obs_valid(rapm_data)})")
}

model <- fit_rapm(
  rapm_data,
  alpha = 0,           # Ridge regression
  nfolds = 5,          # panna#87: 10 -> 5 to halve the CV memory/time spike
  use_weights = TRUE,
  penalize_covariates = FALSE,
  parallel = FALSE,    # avoid 2x memory amplification from doParallel
                       # workers — OOM-killed step 4 on 7GB GHA runners
                       # at 664K obs x 38K cols on the v5 attempt.
  fixed_lambda = base_fixed_lambda  # panna#87: NULL = CV (default), else closed-form
)

# 5. Covariate Effects ----

cat("\n=== Covariate Effects ===\n")
cov_effects <- get_covariate_effects(model)
for (name in names(cov_effects)) {
  cat(sprintf("  %s: %.4f\n", name, cov_effects[name]))
}

# 6. Extract Player Ratings ----

cat("\n=== Player Ratings ===\n")
ratings <- extract_rapm_ratings(model)

cat("\nTop 20 players:\n")
print(
  ratings %>%
    head(20) %>%
    select(player_name, rapm, offense, defense, total_minutes)
)

# 7. Save Results ----

cat("\n=== Saving Results ===\n")

rapm_results <- list(
  rapm_data = rapm_data,
  model = model,
  ratings = ratings,
  covariate_effects = cov_effects
)

saveRDS(rapm_results, file.path(cache_dir, "04_rapm.rds"))
validate_step_output(ratings, step_name = "04_rapm: ratings",
                     min_rows = 100, warn_below = 5000)
cat("Saved to cache-opta/04_rapm.rds\n")

# 8. Multi-Target RAPM (optional) ----
# Fit RAPM on additional value metric targets if available on splints

use_multi_target <- if (exists("use_multi_target")) use_multi_target else TRUE

if (use_multi_target) {
  # Reload splints (freed earlier)
  splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

  value_targets <- c("epv", "wpa", "psv")
  available_targets <- character(0)
  for (tgt in value_targets) {
    home_col <- paste0(tgt, "_home")
    if (home_col %in% names(splint_data$splints)) {
      available_targets <- c(available_targets, tgt)
    }
  }

  if (length(available_targets) > 0) {
    cat("\n=== Multi-Target RAPM ===\n")
    cat("Available targets:", paste(available_targets, collapse = ", "), "\n")

    multi_target_results <- list()

    for (tgt in available_targets) {
      cat(sprintf("\n--- Fitting RAPM for target: %s ---\n", tgt))

      tryCatch({
        rapm_data_tgt <- prepare_rapm_data(
          splint_data,
          min_minutes = MIN_MINUTES_RAPM_FIT,
          target_type = tgt,
          include_covariates = TRUE
        )

        # panna#87: per-target fixed lambda from that target's own n_obs.
        tgt_fixed_lambda <- if (use_fixed_lambda) lambda_formula(.n_obs_valid(rapm_data_tgt)) else NULL
        if (use_fixed_lambda) {
          cli::cli_alert_info(
            "RAPM[{tgt}] fixed-lambda mode (CV skipped), lambda={round(tgt_fixed_lambda, 5)} (n_obs={.n_obs_valid(rapm_data_tgt)})")
        }

        model_tgt <- fit_rapm(
          rapm_data_tgt,
          alpha = 0,
          nfolds = 5,        # panna#87: 10 -> 5
          use_weights = TRUE,
          penalize_covariates = FALSE,
          parallel = FALSE,  # same reason as base RAPM above
          fixed_lambda = tgt_fixed_lambda  # panna#87
        )

        ratings_tgt <- extract_rapm_ratings(model_tgt)
        data.table::setnames(ratings_tgt, "rapm", paste0("rapm_", tgt),
                              skip_absent = TRUE)

        multi_target_results[[tgt]] <- list(
          rapm_data = rapm_data_tgt,
          model = model_tgt,
          ratings = ratings_tgt
        )

        cat(sprintf("  %s RAPM: %d players rated\n", toupper(tgt), nrow(ratings_tgt)))

      }, error = function(e) {
        cat(sprintf("  Skipping %s RAPM: %s\n", tgt, e$message))
      })
    }

    if (length(multi_target_results) > 0) {
      saveRDS(multi_target_results, file.path(cache_dir, "04_rapm_multi.rds"))
      cat("\nSaved multi-target RAPM to cache-opta/04_rapm_multi.rds\n")
    }

    rm(splint_data); gc(verbose = FALSE)
  } else {
    cat("\nNo value metric columns found on splints — skipping multi-target RAPM.\n")
    cat("Run EPV/WPA pipeline and step 03 with value metrics first.\n")
  }
}

message("\nRAPM complete!")
