# _xmetrics_export_common.R
# Shared consolidate+upload logic for 04_export_xmetrics.R (season-level) and
# 04b_export_xmetrics_bymatch.R (per-match) — factored out after a code review
# found the tempdir()-vs-cache-dir path bug had to be hand-fixed in both files
# (panna#126). A future fix to the consolidation/upload logic only needs to
# land here once.

#' Consolidate per-league/season xmetrics parquet files and upload to opta-latest
#'
#' @param subdir "xmetrics" or "xmetrics_bymatch" under opta_data_dir()
#' @param output_name Uploaded file name, e.g. "opta_xmetrics.parquet"
#' @param row_label Noun for the row-count summary message, e.g. "player-seasons"
#' @param upload Publish to the opta-latest release? TRUE keeps the CI
#'   behaviour. FALSE writes the consolidated parquet to the cache and stops,
#'   so the artifact can be inspected before it ships. Added 2026-09-03: with
#'   consolidation and publishing welded together there was no way to build the
#'   file and look at it, which blocked verifying a new xG model that was
#'   deliberately being held back from the release. Callers may pre-set
#'   XMETRICS_UPLOAD before sourcing 04/04b (the repo's config-override
#'   pattern).
#' @return The consolidated data.frame (invisibly used by callers for their own summary)
export_consolidated_xmetrics <- function(subdir, output_name, row_label,
                                         upload = TRUE) {
  root <- file.path(opta_data_dir(), subdir)
  if (!dir.exists(root)) {
    cli::cli_abort(c(
      "No {subdir} directory found at {.path {root}}.",
      "i" = "Run 03_calculate_player_xmetrics.R first."
    ))
  }

  parquet_files <- list.files(root, pattern = "\\.parquet$",
                              recursive = TRUE, full.names = TRUE)
  cli::cli_alert_info("Found {length(parquet_files)} {subdir} parquet files")
  if (length(parquet_files) == 0) {
    cli::cli_abort("No parquet files found under {.path {root}}")
  }

  all_data <- lapply(parquet_files, function(f) {
    tryCatch(arrow::read_parquet(f), error = function(e) {
      cli::cli_warn("Failed to read {.path {f}}: {conditionMessage(e)}")
      NULL
    })
  })
  all_data <- Filter(Negate(is.null), all_data)
  if (length(all_data) == 0) {
    cli::cli_abort(paste0(
      "No {subdir} parquet file could be read (all {length(parquet_files)} ",
      "failed to load — see warnings above)."))
  }

  # rbindlist(fill=TRUE): per-league-season files can carry different columns
  # across a ~22-season history (e.g. the 5 xDuel WOE columns added
  # 2026-06-25) — a plain rbind() errors on any schema mismatch instead of
  # NA-filling the older files' missing columns.
  combined <- as.data.frame(data.table::rbindlist(all_data, fill = TRUE, use.names = TRUE))

  cli::cli_alert_success("Combined {format(nrow(combined), big.mark=',')} {row_label} from {length(parquet_files)} files")

  # Add competition column (Opta league name) for remote query compatibility
  # query_remote_opta_parquet() filters on 'competition' using Opta league names.
  # Mapped once per DISTINCT league code, not once per row — to_opta_league()
  # falls through to a catalog download for unrecognized codes, so a per-row
  # call over a multi-million-row bymatch table is both slow and could hammer
  # the catalog endpoint.
  if ("league" %in% names(combined) && !"competition" %in% names(combined)) {
    codes <- unique(combined$league)
    code_map <- vapply(codes, function(lg) tryCatch(to_opta_league(lg), error = function(e) lg),
                       character(1))
    names(code_map) <- codes
    combined$competition <- unname(code_map[combined$league])
    cli::cli_alert_info("Added 'competition' column from league codes")
  }

  # 2. Write consolidated parquet ----
  # Written into the repo cache (not tempdir()) so the workflow's post-step
  # guard checking data-raw/cache/epv/<output_name> actually sees the file.
  cli::cli_h2("Step 2: Write consolidated parquet")
  cache_dir <- file.path("data-raw", "cache", "epv")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(cache_dir, output_name)
  arrow::write_parquet(combined, output_file)
  file_size <- file.size(output_file) / (1024 * 1024)
  cli::cli_alert_success("Written {.path {output_name}} ({round(file_size, 1)} MB)")

  # 3. Upload to GitHub Releases ----
  if (!isTRUE(upload)) {
    cli::cli_h2("Step 3: Upload SKIPPED (upload = FALSE)")
    cli::cli_alert_warning(c(
      "{.path {output_name}} written to {.path {cache_dir}} but NOT published."
    ))
    cli::cli_alert_info("Re-run with {.code upload = TRUE} to publish to opta-latest.")
    return(combined)
  }

  cli::cli_h2("Step 3: Upload to GitHub Releases")
  repo <- "peteowen1/pannadata"
  tag <- "opta-latest"

  if (!requireNamespace("piggyback", quietly = TRUE)) {
    cli::cli_abort("Package 'piggyback' is required. Install with: install.packages('piggyback')")
  }

  tryCatch({
    piggyback::pb_list(repo = repo, tag = tag)
  }, error = function(e) {
    cli::cli_alert_info("Creating new release: {tag}")
    piggyback::pb_new_release(repo = repo, tag = tag)
    Sys.sleep(3)
  })

  cli::cli_alert_info("Uploading to {repo} ({tag})...")
  piggyback::pb_upload(
    file = output_file,
    repo = repo,
    tag = tag,
    name = output_name,
    overwrite = TRUE
  )
  cli::cli_alert_success("Upload complete!")

  combined
}
