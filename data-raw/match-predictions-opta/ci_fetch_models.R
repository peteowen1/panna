# ci_fetch_models.R
# Models the weekly predictions run (predictions-pipeline.yml) fetches from the pannamodels
# `epv` release before sourcing run_predictions_opta.R. Kept in a file, not in the workflow's
# `Rscript -e` block: R drops an -e expression past its length limit with only a WARNING, and
# the block is near that limit (panna#465 dry run 43378072288 ran nothing because of it).
#
# Sets in the global environment, read by 10b/10c via exists():
#   wp_structural_model_path  the WPA ledger's structural WP (panna#273). A failed download
#                             leaves the path missing and 10b says the ledger is OFF.
#   xg_model_override / xgot_model_override
#                             shot pricing for game logs, net goals and equity: xG v5.2 /
#                             xGOT v3.2 (panna#465, Pete 2026-11-13). Loaded by path and
#                             md5-checked, so a failed or wrong download stops the run instead
#                             of pricing this week's shots with the old xG.

# Three tries: with 20 rebuild jobs downloading at once, one structural-WP download
# failed outright (game-logs-rebuild run 43410608936, 2001-02 part 2) and turned the
# WPA ledger off for that job.
.gh_epv_download <- function(asset, dest, tries = 3L) {
  for (i in seq_len(tries)) {
    code <- system2("gh", c("release", "download", "epv", "-R", "peteowen1/pannamodels",
                            "-p", asset, "-O", dest, "--clobber"))
    if (identical(as.integer(code), 0L) && file.exists(dest)) return(0L)
    if (i < tries) { cat(asset, "download failed (exit", code, "), retry", i, "\n"); Sys.sleep(15 * i) }
  }
  code
}

wp_structural_model_path <- file.path(tempdir(), "wp_structural_model.rds")
.wps_dl <- .gh_epv_download("wp_structural_model.rds", wp_structural_model_path)
cat("wp_structural_model.rds download exit", .wps_dl, "| md5",
    if (file.exists(wp_structural_model_path)) unname(tools::md5sum(wp_structural_model_path)) else "none", "\n")

.sm_md5 <- c(xg_model_v5 = "a98df5d1", xgot_model_v3 = "4709ead1")  # MODELS.md rows 48-49
.sm_paths <- character(0)
for (.sm in names(.sm_md5)) {
  .sm_path <- file.path(tempdir(), paste0(.sm, ".rds"))
  .sm_dl <- .gh_epv_download(paste0(.sm, ".rds"), .sm_path)
  if (!identical(as.integer(.sm_dl), 0L) || !file.exists(.sm_path)) {
    stop(.sm, ".rds download failed (exit ", .sm_dl, ")")
  }
  .got <- substr(unname(tools::md5sum(.sm_path)), 1, 8)
  cat(.sm, ".rds md5", .got, "\n")
  if (!identical(.got, .sm_md5[[.sm]])) {
    stop(.sm, ".rds md5 ", .got, " but MODELS.md says ", .sm_md5[[.sm]])
  }
  .sm_paths[[.sm]] <- .sm_path
}
xg_model_override   <- load_xg_model(.sm_paths[["xg_model_v5"]])
xgot_model_override <- load_xgot_model(.sm_paths[["xgot_model_v3"]])
