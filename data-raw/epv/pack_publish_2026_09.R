# Model-pack step 8: publish the new EPV and xGOT (EPV-RETRAIN-SCOPE.md).
# =============================================================================
# The published xG stays (it beat the retrain on 2026-27). The published WP stays
# too (2026-09-24, pack_gate_wp.R, Big 5 2024-25 held out): it scores the same on
# the new EPV as on the old (MSE 0.11048 -> 0.11047, ECE 0.0126 -> 0.0136), and
# the retrained WP is a trade rather than a win against it (MSE 0.1114, ECE
# 0.0104). Net goals never reads WP; game logs pass their own WP override. Worker JSONs on R2 are
# NOT touched here: the blog worker's validator checks live WPA against the
# published game logs, so they move with step 9's game-log rebuild.
#
# Rollback: C:/dev/_model-backups/2026-09-23/README.md (step 0 backup).
#
# Run from panna/:  Rscript data-raw/epv/pack_publish_2026_09.R            (dry run)
#                   PACK_PUBLISH=1 Rscript data-raw/epv/pack_publish_2026_09.R  (publish)
suppressMessages(devtools::load_all(quiet = TRUE))
P   <- "data-raw/cache/epv/pack-2026-09/"
BAK <- "C:/dev/_model-backups/2026-09-23/pannamodels-epv/"
new <- c(epv_model.rds  = paste0(P, "epv_model_pubv0.rds"),
         xgot_model.rds = paste0(P, "xgot_model.rds"))
stopifnot(all(file.exists(new)))

feats <- function(m) m$feature_names %||% m$panna_metadata$feature_cols %||% m$model$feature_names
cat("== input contract: new vs what is published now ==\n")
for (nm in names(new)) {
  a <- feats(readRDS(new[[nm]])); b <- feats(readRDS(paste0(BAK, nm)))
  if (!length(a) || !length(b)) stop(nm, ": could not read a feature list from one of the models; the contract check would compare nothing")
  cat(sprintf("%-15s new %2d features, published %2d, identical: %s\n", nm, length(a), length(b), identical(a, b)))
  # xGOT: predict_xgot() selects the model's own feature_cols by name, so a
  # model that drops an input still scores from the same R code. The retrain
  # dropped is_direct_freekick (2026-09-24). The blog worker pins all 18
  # (EXPECTED_FEATURES_FOOTBALL_XGOT), so its JSON and code move together in step 9.
  subset_ok <- nm == "xgot_model.rds" && all(a %in% b) && identical(b[b %in% a], a)
  if (!identical(a, b) && !subset_ok) stop(nm, ": the input contract changed; that needs a code lockstep, not this script")
  if (subset_ok && !identical(a, b)) cat("  xGOT drops:", setdiff(b, a), "(R scores by name; worker updates in step 9)\n")
}

stage <- file.path(tempdir(), "pack-2026-09"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, names(new))
stopifnot(all(file.copy(new, paths, overwrite = TRUE)))
print(data.frame(asset = names(new), md5 = unname(tools::md5sum(paths)), bytes = file.size(paths)))

dry <- !identical(Sys.getenv("PACK_PUBLISH"), "1")
for (dest in list(c("peteowen1/pannamodels", "epv"), c("peteowen1/pannadata", "models"))) {
  cat("\n== ", dest[1], "@", dest[2], if (dry) " (dry run)" else "", " ==\n", sep = "")
  vb_publish(paths, repo = dest[1], tag = dest[2], carry_forward = TRUE, dry_run = dry)
}
