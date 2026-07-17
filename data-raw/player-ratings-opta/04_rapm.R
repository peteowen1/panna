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

# panna#87: bracketed mini-CV. The closed-form lambda (career-fit calibration)
# was validated at current scale on 2026-07-08 and misplaces lambda ~4x
# (cv lambda.min 0.00122 vs formula 0.00489; player-coef cor 0.925, top-50
# overlap 34/50 — material for a published rating). But a SHORT 13-point CV
# grid bracketing the formula costs ~1 min and picks lambda FROM THE DATA —
# adapting to sample size, weights (n_eff/n_obs = 0.62 here), and design.
# The June OOM came from the default 100-lambda path x 10 folds, not CV per
# se. Formula's only job now: center the grid (proven to land the optimum
# interior). Validation harness: debug/validate_lambda_formula_at_scale.R.
# NB precompute glue values into plain variables: cli >= 3.4 treats a brace
# expression starting with a dot (e.g. {.n_obs_valid(x)}) as inline MARKUP
# and ERRORS ("Invalid cli literal: starts with a dot") — this exact line was
# the silent step-4 killer in runs 28890193113/28919371826/28920002141, only
# reached once the combine OOM was fixed.
n_obs_base <- .n_obs_valid(rapm_data)
lam_center_base <- lambda_formula(n_obs_base)
base_lambda_seq <- if (use_fixed_lambda) lam_center_base * 2^seq(3, -3, by = -0.5) else NULL
if (use_fixed_lambda) {
  cli::cli_alert_info(
    "RAPM mini-CV mode: 13-point lambda grid centered on {round(lam_center_base, 5)} (n_obs={n_obs_base})")
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
  lambda_seq = base_lambda_seq  # panna#87: NULL = default CV path, else mini-CV grid
)
if (use_fixed_lambda) {
  cli::cli_alert_info("RAPM mini-CV picked lambda.min={signif(model$lambda.min, 4)}")
}

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

# D6 (FABLE-PRIOR-FIX-PLAN.md): experimental gate, default FALSE -- the cloud
# pipeline never runs the multi-target (EPV/WPA) section until promotion.
# inherits = FALSE so a same-named object from an enclosing/parent scope
# (e.g. dplyr::sample_n-style collision) can't silently flip this on.
run_multi_target <- if (exists("run_multi_target", inherits = FALSE)) run_multi_target else FALSE

if (run_multi_target) {
  # Reload splints (freed earlier)
  splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

  # PSV removed from RAPM (FABLE-PRIOR-FIX-PLAN.md D3) -- no per-splint
  # box-score count cache exists, and PSV already has its own standalone
  # pipeline (R/psr.R). EPV/WPA now use true per-splint attribution (Step 3)
  # instead of the whole-match-value x duration-proration join.
  value_targets <- c("epv", "wpa")
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
        # FABLE-PRIOR-FIX-PLAN.md D2/Step 5: WPA MUST use mode = "net", never
        # "od". Empirically confirmed (not just the plan's -0.949 estimate on
        # the old proration bug): fitting a truly zero-sum target (Step 3's
        # per-splint WPA, wpa_home == -wpa_away exactly) in od mode drives
        # offense/defense to cor = -1.0000 EXACTLY -- a ridge fit on a
        # zero-sum target is symmetric under the (row, off/def, sign) swap,
        # so the unique ridge-regularized solution is a fixed point of that
        # symmetry (offense = -defense for every player). That would trip the
        # Step-1 D5 tripwire below unconditionally for every WPA fit,
        # aborting this whole script before ANY multi-target artifact (incl.
        # epv, already fit) gets saved -- the .check_degenerate_multi_target()
        # loop runs over every successfully-fit target before the one
        # saveRDS() call, uncaught by this tryCatch. xg/goals/epv keep "od"
        # (D1: not zero-sum between teams, an O/D split is meaningful).
        tgt_mode <- if (tgt == "wpa") "net" else "od"
        rapm_data_tgt <- prepare_rapm_data(
          splint_data,
          min_minutes = MIN_MINUTES_RAPM_FIT,
          target_type = tgt,
          include_covariates = TRUE,
          mode = tgt_mode
        )

        # panna#87: per-target mini-CV grid centered on that target's own
        # closed-form lambda (see the base-fit comment).
        n_obs_tgt <- .n_obs_valid(rapm_data_tgt)
        lam_center_tgt <- lambda_formula(n_obs_tgt)
        tgt_lambda_seq <- if (use_fixed_lambda) lam_center_tgt * 2^seq(3, -3, by = -0.5) else NULL
        if (use_fixed_lambda) {
          cli::cli_alert_info(
            "RAPM[{tgt}] mini-CV grid centered on {round(lam_center_tgt, 5)} (n_obs={n_obs_tgt})")
        }

        model_tgt <- fit_rapm(
          rapm_data_tgt,
          alpha = 0,
          nfolds = 5,        # panna#87: 10 -> 5
          use_weights = TRUE,
          penalize_covariates = FALSE,
          parallel = FALSE,  # same reason as base RAPM above
          lambda_seq = tgt_lambda_seq  # panna#87
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
      # D5 tripwire: abort loudly before writing anything the panna#87
      # heartbeat glob could upload, if any target's fit shows a known
      # degenerate-output signature (all-shrunk coefs / mirrored O-D).
      for (tgt in names(multi_target_results)) {
        .check_degenerate_multi_target(multi_target_results[[tgt]]$ratings, tgt)
      }

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
