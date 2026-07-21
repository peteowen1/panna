# 02_lambda_stability_report.R
#
# Wave-1 lambda-stability report on the windowed prior-free RAPM TARGET
# (pannaverse/docs/plans/BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.3.3, sec 5.4
# kill criterion 2). Reads data-raw/cache-opta/rapm_window_lambda_grid.rds
# (built by 01_lambda_sensitivity_targets.R) and, per vintage, reports vs the
# x1 (lambda.min) baseline:
#   (i)   Spearman rank correlation of rapm/offense/defense
#   (ii)  scale drift: sd(rating at lambda) / sd(rating at lambda.min)
#   (iii) top-50 overlap fraction (by rapm)
# restricted to players with >= 900 window minutes (derivable here, since
# 01_lambda_sensitivity_targets.R computes window_minutes from the same
# windowed weight subset).
#
# Kill bar (sec 5.4 #2): rapm rank Spearman < 0.9 anywhere in the grid ->
# the windowed target is a regularisation artifact, not a stable price
# signal -- FAIL that vintage.
#
# Run from panna/ (relative cache paths assume cwd = panna/).

# 1. Setup ----

devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")
if (!exists("min_window_minutes", inherits = FALSE)) min_window_minutes <- 900
if (!exists("top_n", inherits = FALSE)) top_n <- 50
if (!exists("spearman_bar", inherits = FALSE)) spearman_bar <- 0.9

output_dir <- if (exists("output_dir", inherits = FALSE)) output_dir else
  file.path("data-raw", "spm-redesign")
output_csv <- file.path(output_dir, "lambda_stability_report.csv")

grid_path <- file.path(cache_dir, "rapm_window_lambda_grid.rds")
if (!file.exists(grid_path)) {
  cli::cli_abort("Expected {.file {grid_path}} -- run 01_lambda_sensitivity_targets.R first.")
}
rapm_window_lambda_grid <- readRDS(grid_path)

# 2. Per-vintage, per-lambda-multiple comparison vs the x1 baseline ----

top50_overlap <- function(baseline, comparison, n = top_n) {
  base_top <- baseline$player_id[order(-baseline$rapm)][seq_len(min(n, nrow(baseline)))]
  comp_top <- comparison$player_id[order(-comparison$rapm)][seq_len(min(n, nrow(comparison)))]
  length(intersect(base_top, comp_top)) / n
}

rows <- list()

for (Y in names(rapm_window_lambda_grid)) {
  entry <- rapm_window_lambda_grid[[Y]]
  baseline <- entry$fits[["x1"]]$ratings
  if (is.null(baseline)) {
    cli::cli_warn("Vintage {Y}: no x1 (lambda.min) baseline fit -- skipping.")
    next
  }

  wm <- entry$window_minutes
  eligible_ids <- wm$player_id[wm$window_minutes >= min_window_minutes]
  baseline_elig <- baseline[baseline$player_id %in% eligible_ids, ]

  cat(sprintf("vintage %s: %d players total, %d with >= %d window minutes\n",
              Y, nrow(baseline), nrow(baseline_elig), min_window_minutes))

  for (key in names(entry$fits)) {
    comparison <- entry$fits[[key]]$ratings
    comparison_elig <- comparison[comparison$player_id %in% eligible_ids, ]

    merged <- merge(baseline_elig[, c("player_id", "rapm", "offense", "defense")],
                    comparison_elig[, c("player_id", "rapm", "offense", "defense")],
                    by = "player_id", suffixes = c("_base", "_cmp"))

    spearman_rapm <- stats::cor(merged$rapm_base, merged$rapm_cmp, method = "spearman")
    spearman_off <- stats::cor(merged$offense_base, merged$offense_cmp, method = "spearman")
    spearman_def <- stats::cor(merged$defense_base, merged$defense_cmp, method = "spearman")
    scale_drift <- stats::sd(merged$rapm_cmp) / stats::sd(merged$rapm_base)
    overlap <- top50_overlap(baseline_elig, comparison_elig)

    rows[[length(rows) + 1]] <- data.frame(
      vintage = as.integer(Y),
      lambda_multiple = entry$fits[[key]]$lambda_multiple,
      lambda = entry$fits[[key]]$lambda,
      n_players = nrow(merged),
      spearman_rapm = spearman_rapm,
      spearman_offense = spearman_off,
      spearman_defense = spearman_def,
      scale_drift = scale_drift,
      top50_overlap = overlap
    )
  }
}

report <- do.call(rbind, rows)
report <- report[order(report$vintage, report$lambda_multiple), ]

# 3. PASS/FAIL verdict per vintage vs the 0.9 rapm-Spearman bar ----

non_trivial <- report[report$lambda_multiple != 1, ]
verdict <- stats::aggregate(spearman_rapm ~ vintage, data = non_trivial, FUN = min)
names(verdict)[2] <- "min_spearman_rapm"
verdict$verdict <- ifelse(verdict$min_spearman_rapm >= spearman_bar, "PASS", "FAIL")

cat("\n=== Lambda-stability report (vs x1 = lambda.min baseline) ===\n")
print(report, row.names = FALSE)

cat("\n=== Per-vintage verdict (min rapm Spearman across non-baseline multiples, bar = ",
    spearman_bar, ") ===\n", sep = "")
print(verdict, row.names = FALSE)

overall <- if (all(verdict$verdict == "PASS")) "PASS" else "FAIL"
cat(sprintf("\nOVERALL VERDICT: %s\n", overall))
if (overall == "FAIL") {
  cat("Failing vintage(s): ", paste(verdict$vintage[verdict$verdict == "FAIL"], collapse = ", "), "\n")
}

# 4. Write CSV ----

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(report, output_csv, row.names = FALSE)
write.csv(verdict, file.path(output_dir, "lambda_stability_verdict.csv"), row.names = FALSE)
cat(sprintf("\nWrote %s\n", output_csv))
cat(sprintf("Wrote %s\n", file.path(output_dir, "lambda_stability_verdict.csv")))
