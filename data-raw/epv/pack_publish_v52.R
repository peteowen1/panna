# xG v5.2 / xGOT v3.2 release (panna#277): v5.1 / v3.1 retrained without direct-from-corner
# shots (Opta q263, or a Corner-situation shot in the corner-flag box). Pete chose to publish
# without a gate (2026-10-06); the report card (xgv_14_direct_corner_report.R) is shown first.
# =============================================================================
#   xg_model_v5.rds   <- xg_model_v5_2.rds;  xg_model.rds is untouched (decision register D8)
#   xgot_model_v3.rds <- xgot_model_v3_2.rds; xgot_model.rds is untouched
# EPV and WP are NOT republished (Pete: EPV keeps its labels). Worker JSONs on R2 are not touched.
#
# Step 1 (always): download what is published now to C:/dev/_model-backups/2026-10-06/
#   with md5s, so a rollback is re-uploading those files.
# Step 2: dry run unless PACK_PUBLISH=1.
suppressMessages(devtools::load_all(quiet = TRUE))
new <- c(xg_model_v5.rds   = "data-raw/cache/epv/xg-vnext/xg_model_v5_2.rds",
         xgot_model_v3.rds = "data-raw/cache/epv/xg-vnext/xgot_model_v3_2.rds")
stopifnot(all(file.exists(new)))
DEST <- list(c("peteowen1/pannamodels", "epv"), c("peteowen1/pannadata", "models"))

# ---- 1. backup ---------------------------------------------------------------
BAK <- "C:/dev/_model-backups/2026-10-06"
for (d in DEST) {
  out <- file.path(BAK, paste0(basename(d[1]), "-", d[2])); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (a in names(new)) {
    f <- file.path(out, a)
    if (file.exists(f)) next
    st <- system2("gh", c("release", "download", d[2], "-R", d[1], "-p", a, "-D", shQuote(out)), stdout = TRUE, stderr = TRUE)
    if (!file.exists(f)) stop("backup failed: ", d[1], "@", d[2], " ", a, " (", tail(st, 1), ")")   # no backup, no publish
  }
  fs <- list.files(out, pattern = "[.]rds$", full.names = TRUE)
  md <- data.frame(file = basename(fs), md5 = unname(tools::md5sum(fs)), bytes = file.size(fs))
  write.csv(md, file.path(out, "md5.csv"), row.names = FALSE)
  for (f in fs) invisible(readRDS(f))   # every backup must load
  cat("backup", out, ":", nrow(md), "files\n"); print(md)
}
writeLines(c("# Model backups before the xG v5.2 release (2026-10-06): xG v5.1 / xGOT v3.1 as published 2026-09-29",
             "Taken by panna data-raw/epv/pack_publish_v52.R. Rollback: re-upload these files to the same",
             "release with vb_publish(..., carry_forward = TRUE). Not executed: each overwrites a live asset."),
           file.path(BAK, "README.md"))

# ---- 2. input contracts: same inputs as what each release holds now ------------
feats <- function(m) m$feature_names %||% m$panna_metadata$feature_cols %||% m$model$feature_names
for (d in DEST) for (nm in names(new)) {
  bak <- file.path(BAK, paste0(basename(d[1]), "-", d[2]), nm)
  a <- feats(readRDS(new[[nm]])); b <- feats(readRDS(bak))
  if (!length(a) || !length(b)) stop(nm, ": no feature list to compare")
  cat(sprintf("%-22s %-18s new %2d inputs, published %2d, identical: %s\n", paste0(d[1], "@", d[2]), nm, length(a), length(b), identical(a, b)))
  if (!identical(a, b)) stop(nm, ": the input contract changed; that needs a code lockstep, not this script")
  stopifnot(.needs_shot_context(readRDS(new[[nm]])))
}

# ---- 3. publish ----------------------------------------------------------------
stage <- file.path(tempdir(), "pack-v52"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, names(new))
stopifnot(all(file.copy(new, paths, overwrite = TRUE)))
print(data.frame(asset = names(new), md5 = unname(tools::md5sum(paths)), bytes = file.size(paths)))
dry <- !identical(Sys.getenv("PACK_PUBLISH"), "1")
for (d in DEST) {
  cat("\n== ", d[1], "@", d[2], if (dry) " (dry run)" else "", " ==\n", sep = "")
  vb_publish(paths, repo = d[1], tag = d[2], carry_forward = TRUE, dry_run = dry)
}
