# xG v5 release: publish the v5 pack (pannaverse docs/plans/XG-VNEXT-2026-09.md,
# "Release report card"). Pete's go 2026-09-28.
# =============================================================================
#   epv_model.rds     <- epv_model_v5v0.rds  (labels at the shot price on xG v5; same 14 inputs)
#   wp_model.rds      <- wp_v5/wp_model.rds  (live WP configuration, retrained on the v5 pack; same 8 inputs)
#   xg_model_v5.rds   NEW name; xg_model.rds is untouched (decision register D8)
#   xgot_model_v3.rds NEW name; xgot_model.rds is untouched
# Worker JSONs on R2 are NOT touched: the worker switches with the blog's shot tables.
#
# Step 1 (always): download what is published now to C:/dev/_model-backups/2026-09-28/
#   with md5s, so a rollback is re-uploading those files.
# Step 2: dry run unless PACK_PUBLISH=1.
suppressMessages(devtools::load_all(quiet = TRUE))
P <- "data-raw/cache/epv/pack-2026-09/"
new <- c(epv_model.rds     = paste0(P, "epv_model_v5v0.rds"),
         wp_model.rds      = paste0(P, "wp_v5/wp_model.rds"),
         xg_model_v5.rds   = "data-raw/cache/epv/xg-vnext/xg_model_v5.rds",
         xgot_model_v3.rds = "data-raw/cache/epv/xg-vnext/xgot_model_v3.rds")
stopifnot(all(file.exists(new)))
DEST <- list(c("peteowen1/pannamodels", "epv"), c("peteowen1/pannadata", "models"))

# ---- 1. backup ---------------------------------------------------------------
BAK <- "C:/dev/_model-backups/2026-09-28"
for (d in DEST) {
  out <- file.path(BAK, paste0(basename(d[1]), "-", d[2])); dir.create(out, recursive = TRUE, showWarnings = FALSE)
  for (a in c("epv_model.rds", "wp_model.rds", "xg_model.rds", "xgot_model.rds")) {
    f <- file.path(out, a)
    if (file.exists(f)) next
    st <- system2("gh", c("release", "download", d[2], "-R", d[1], "-p", a, "-D", shQuote(out)), stdout = TRUE, stderr = TRUE)
    # A failed download must stop the publish: step 3 overwrites both releases, and
    # a destination without its backup has no rollback (review finding).
    if (!file.exists(f)) stop("backup failed: ", d[1], "@", d[2], " ", a, " (", tail(st, 1), ")")
  }
  fs <- list.files(out, pattern = "[.]rds$", full.names = TRUE)
  md <- data.frame(file = basename(fs), md5 = unname(tools::md5sum(fs)), bytes = file.size(fs))
  write.csv(md, file.path(out, "md5.csv"), row.names = FALSE)
  for (f in fs) invisible(readRDS(f))   # every backup must load
  cat("backup", out, ":", nrow(md), "files\n")
}
writeLines(c("# Model backups before the xG v5 release (2026-09-28)",
             "Taken by panna data-raw/epv/pack_publish_v5.R. Rollback: re-upload these files to the same",
             "release with vb_publish(..., carry_forward = TRUE), then re-run 10b. Not executed: each overwrites a live asset.",
             "xg_model_v5.rds / xgot_model_v3.rds did not exist before: rolling back = deleting those two assets."),
           file.path(BAK, "README.md"))

# ---- 2. input contracts --------------------------------------------------------
feats <- function(m) m$feature_names %||% m$panna_metadata$feature_cols %||% m$model$feature_names
for (d in DEST) for (nm in c("epv_model.rds", "wp_model.rds")) {   # against what EACH release holds now
  bak <- file.path(BAK, paste0(basename(d[1]), "-", d[2]), nm)
  a <- feats(readRDS(new[[nm]])); b <- feats(readRDS(bak))
  if (!length(a) || !length(b)) stop(nm, ": no feature list to compare")
  cat(sprintf("%-22s %-14s new %2d inputs, published %2d, identical: %s\n", paste0(d[1], "@", d[2]), nm, length(a), length(b), identical(a, b)))
  if (!identical(a, b)) stop(nm, ": the input contract changed; that needs a code lockstep, not this script")
}
for (nm in c("xg_model_v5.rds", "xgot_model_v3.rds"))
  stopifnot(.needs_shot_context(readRDS(new[[nm]])))

# ---- 3. publish ----------------------------------------------------------------
stage <- file.path(tempdir(), "pack-v5"); dir.create(stage, showWarnings = FALSE)
paths <- file.path(stage, names(new))
stopifnot(all(file.copy(new, paths, overwrite = TRUE)))
print(data.frame(asset = names(new), md5 = unname(tools::md5sum(paths)), bytes = file.size(paths)))
dry <- !identical(Sys.getenv("PACK_PUBLISH"), "1")
for (d in DEST) {
  cat("\n== ", d[1], "@", d[2], if (dry) " (dry run)" else "", " ==\n", sep = "")
  vb_publish(paths, repo = d[1], tag = d[2], carry_forward = TRUE, dry_run = dry)
}
