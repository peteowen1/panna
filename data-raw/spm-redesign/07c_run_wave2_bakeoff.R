# 07c_run_wave2_bakeoff.R
#
# Wave 2 bake-off driver (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.1/5.2 +
# runbook in pannaverse/docs/NEXT-STEPS.md 2026-07-21 evening block): runs
# ALL candidates S1-S5 through the 06c leak-free next-window gate on the
# FULL panel, then adds the panna#168 response-variance report -- per
# candidate, sd(prediction)/sd(next-window target) pooled and per vintage.
# The variance ratios are descriptive input to the #168 display-calibration
# decision (a compressed-but-well-ordered candidate can still win the gate;
# #168's knob 1 is a post-hoc display transform, not a gate criterion).
#
# Outputs (data-raw/spm-redesign/):
#   wave2_bakeoff_results.rds   full 06c results (per_vintage, pooled,
#                               pairs, bootstrap) keyed by candidate id
#   wave2_gate_table.csv        one row per candidate: pooled cor/wcor,
#                               bootstrap delta vs S0 + 95% CI, promotion
#                               verdict per sec 5.2 (CI > 0)
#   wave2_variance_report.csv   #168 report: sd ratios per candidate,
#                               pooled + per vintage (S0 included)
#
# Run from panna/ (relative cache paths):
#   Rscript data-raw/spm-redesign/07c_run_wave2_bakeoff.R

candidate_ids <- c("S1", "S2", "S3", "S4a", "S4b", "S5")

# 06c's main() runs on source (the skip guard is NOT set) and returns
# invisible(results); capture via source()$value.
results <- source(file.path("data-raw", "spm-redesign", "06c_eval_nextwindow.R"))$value

out_dir <- file.path("data-raw", "spm-redesign")

saveRDS(results, file.path(out_dir, "wave2_bakeoff_results.rds"))

# Gate table (sec 5.2: promotion iff paired-bootstrap 95% CI vs S0 > 0) ----

gate_rows <- lapply(names(results), function(cid) {
  res <- results[[cid]]
  data.table::data.table(
    candidate = cid,
    n_pairs = res$bootstrap$n_pairs,
    cor_pooled = res$pooled$cor_candidate,
    cor_s0 = res$pooled$cor_s0,
    wcor_pooled = res$pooled$wcor_candidate,
    wcor_s0 = res$pooled$wcor_s0,
    boot_delta = res$bootstrap$mean_delta,
    ci_lo = res$bootstrap$ci_lo,
    ci_hi = res$bootstrap$ci_hi,
    p_gt0 = res$bootstrap$p_gt0,
    clears_s0_ci = !is.na(res$bootstrap$ci_lo) & res$bootstrap$ci_lo > 0
  )
})
gate_table <- data.table::rbindlist(gate_rows)
data.table::fwrite(gate_table, file.path(out_dir, "wave2_gate_table.csv"))
cli::cli_h1("Wave 2 gate table (sec 5.2)")
print(gate_table)

# Response-variance report (panna#168) ----
#
# sd ratios computed on the SAME >=900-min pooled/per-vintage pairs the gate
# uses, so the compression numbers describe the population the metric would
# actually be displayed for. S0's ratio is computed from the same pairs
# (its predictions ride along in the pairs table).

variance_rows <- lapply(names(results), function(cid) {
  pairs <- results[[cid]]$pairs
  by_vintage <- pairs[, .(
    candidate = cid, scope = "vintage",
    sd_pred = stats::sd(candidate), sd_s0 = stats::sd(s0),
    sd_target = stats::sd(target_next),
    ratio_pred = stats::sd(candidate) / stats::sd(target_next),
    ratio_s0 = stats::sd(s0) / stats::sd(target_next),
    n = .N
  ), by = vintage]
  pooled <- data.table::data.table(
    candidate = cid, scope = "pooled", vintage = NA_character_,
    sd_pred = stats::sd(pairs$candidate), sd_s0 = stats::sd(pairs$s0),
    sd_target = stats::sd(pairs$target_next),
    ratio_pred = stats::sd(pairs$candidate) / stats::sd(pairs$target_next),
    ratio_s0 = stats::sd(pairs$s0) / stats::sd(pairs$target_next),
    n = nrow(pairs)
  )
  data.table::rbindlist(list(pooled, by_vintage), use.names = TRUE, fill = TRUE)
})
variance_report <- data.table::rbindlist(variance_rows)
data.table::fwrite(variance_report, file.path(out_dir, "wave2_variance_report.csv"))
cli::cli_h1("Response-variance report (panna#168)")
print(variance_report[scope == "pooled"])

cli::cli_alert_success("Bake-off artifacts written to {.path {out_dir}}")
