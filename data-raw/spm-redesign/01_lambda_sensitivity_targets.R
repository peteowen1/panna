# 01_lambda_sensitivity_targets.R
#
# Wave-1 lambda-sensitivity study of the windowed prior-free RAPM TARGET
# (pannaverse/docs/plans/BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.3.3, sec 5.4
# kill criterion 2). For each vintage year Y, refits the 5-season window
# prior-free RAPM (same window as 04b_rapm_window_targets.R) at
# lambda in {1/4, 1/2, 1, 2, 4} x lambda.min(Y), using fit_rapm()'s
# fixed_lambda path (skips CV -- deterministic, fast). lambda.min(Y) is read
# from the already-built cache-opta/rapm_window_targets.rds rather than
# re-run via mini-CV.
#
# NOTE ON SCOPE: this is the TARGET-level lambda study only -- it measures
# whether the windowed prior-free RAPM ratings themselves are lambda-stable.
# SPM-COEFFICIENT-level lambda sensitivity (i.e. does the trained SPM model's
# prices change with lambda) is a separate Wave-2 study once the panel
# training machinery exists.
#
# Also computes per-vintage window minutes per player (needed for the
# >=900-minute restriction in 02_lambda_stability_report.R and the
# reliability bands in 03_attenuation_diagnostics.R), derived from the same
# windowed row/weight subset (weights = minutes / 90, R/rapm_matrix.R:837) --
# no separate minutes source is read.
#
# Run from panna/ (relative cache paths assume cwd = panna/).

# 1. Setup ----

devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")

window_years <- if (exists("window_years", inherits = FALSE)) window_years else 5L
vintage_years <- if (exists("vintage_years", inherits = FALSE)) vintage_years else 2019:2026
lambda_multiples <- if (exists("lambda_multiples", inherits = FALSE)) {
  lambda_multiples
} else {
  c(0.25, 0.5, 1, 2, 4)
}
force_rebuild <- if (exists("force_rebuild", inherits = FALSE)) force_rebuild else FALSE

output_path <- if (exists("output_path", inherits = FALSE)) output_path else
  file.path(cache_dir, "rapm_window_lambda_grid.rds")

# 2. Resume ----

rapm_window_lambda_grid <- list()
if (isTRUE(force_rebuild) && file.exists(output_path)) {
  cat(sprintf("force_rebuild = TRUE: ignoring existing %s\n", basename(output_path)))
} else if (file.exists(output_path)) {
  rapm_window_lambda_grid <- readRDS(output_path)
  cat(sprintf("resume: %d vintage(s) already in %s\n",
              length(rapm_window_lambda_grid), basename(output_path)))
}

mult_key <- function(m) sprintf("x%s", format(m, trim = TRUE))

vintage_needs_work <- function(Y) {
  entry <- rapm_window_lambda_grid[[as.character(Y)]]
  if (is.null(entry)) return(TRUE)
  missing_mult <- !all(vapply(lambda_multiples, function(m) mult_key(m) %in% names(entry$fits), logical(1)))
  is.null(entry$window_minutes) || missing_mult
}

todo_years <- vintage_years[vapply(vintage_years, vintage_needs_work, logical(1))]
cat(sprintf("%d vintage year(s) need lambda-grid work: %s\n", length(todo_years),
            paste(todo_years, collapse = ", ")))

# 3. Baseline lambda.min per vintage (from the already-built window-target cache) ----

if (length(todo_years) > 0) {
  window_targets_path <- file.path(cache_dir, "rapm_window_targets.rds")
  if (!file.exists(window_targets_path)) {
    cli::cli_abort("Expected {.file {window_targets_path}} (built by 04b_rapm_window_targets.R) -- run that first.")
  }
  rapm_window_targets <- readRDS(window_targets_path)

  cat("\n=== Loading pooled RAPM design + season map ===\n")
  r4 <- readRDS(file.path(cache_dir, "04_rapm.rds"))
  pooled_rapm_data <- r4$rapm_data
  rm(r4); gc(verbose = FALSE)

  splints03 <- readRDS(file.path(cache_dir, "03_splints.rds"))
  splint_season_map <- splints03$splints[, c("splint_id", "season_end_year")]
  rm(splints03); gc(verbose = FALSE)

  # 4. Per-vintage window minutes + per-lambda fixed-lambda refits ----

  for (Y in todo_years) {
    min_year <- Y - window_years
    lambda_min_Y <- rapm_window_targets[[as.character(Y)]]$lambda_min
    if (is.null(lambda_min_Y)) {
      cli::cli_warn("No lambda_min for vintage {Y} in rapm_window_targets.rds -- skipping.")
      next
    }
    cat(sprintf("\n--- vintage %d (window [%d, %d), lambda.min=%.5f) ---\n",
                Y, min_year, Y, lambda_min_Y))

    rapm_sub <- .subset_rapm_data_expanding(pooled_rapm_data, splint_season_map,
                                            cutoff_year = Y, min_year = min_year)

    off_cols <- paste0(rapm_sub$player_ids, "_off")
    off_idx <- match(off_cols, colnames(rapm_sub$X_full))
    window_minutes <- data.frame(
      player_id = rapm_sub$player_ids,
      window_minutes = as.numeric(Matrix::crossprod(rapm_sub$X_full[, off_idx, drop = FALSE],
                                                     rapm_sub$weights)) * 90
    )

    entry <- rapm_window_lambda_grid[[as.character(Y)]]
    if (is.null(entry)) {
      entry <- list(window = c(min_year, Y), lambda_min_baseline = lambda_min_Y,
                    n_obs = length(rapm_sub$y), window_minutes = window_minutes, fits = list())
    } else {
      entry$window_minutes <- window_minutes
    }

    for (m in lambda_multiples) {
      key <- mult_key(m)
      if (!force_rebuild && key %in% names(entry$fits)) next

      fixed_lambda <- lambda_min_Y * m
      t0 <- Sys.time()
      model <- fit_rapm(rapm_sub, alpha = 0, use_weights = TRUE,
                        penalize_covariates = FALSE, parallel = FALSE,
                        fixed_lambda = fixed_lambda)
      ratings <- extract_rapm_ratings(model, lambda = "min")

      entry$fits[[key]] <- list(
        lambda_multiple = m,
        lambda = fixed_lambda,
        ratings = ratings[, c("player_id", "rapm", "offense", "defense")]
      )

      rapm_window_lambda_grid[[as.character(Y)]] <- entry
      saveRDS(rapm_window_lambda_grid, output_path)  # checkpoint after every (Y, multiple)

      cat(sprintf("  lambda x%s (%.5f) done in %.1f s\n", format(m, trim = TRUE), fixed_lambda,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      rm(model); gc(verbose = FALSE)
    }
  }

  rm(pooled_rapm_data, splint_season_map); gc(verbose = FALSE)
} else {
  cat("\nNothing to do -- all requested (vintage, lambda-multiple) combinations already cached.\n")
}

cat(sprintf("\nSaved %d vintage(s) to %s\n", length(rapm_window_lambda_grid), output_path))
