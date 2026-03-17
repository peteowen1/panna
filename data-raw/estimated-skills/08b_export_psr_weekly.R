# 08b_export_psr_weekly.R
# Compute and export weekly PSR snapshots for date-based player_psr() queries
#
# Generates skill estimates at weekly/monthly snapshot dates and applies PSR
# coefficients to produce opta_psr_weekly.parquet:
#   - Weekly snapshots for the last 2 years
#   - Monthly snapshots before that (back to 2015)
#
# Prerequisite: step 07 must have run (PSR coefficients in inst/extdata/)
#
# Outputs:
#   - pannadata/data/opta/opta_psr_weekly.parquet (uploaded to GitHub)

# 1. Setup ----

library(arrow)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache-skills")
opta_dir  <- opta_data_dir()
dir.create(opta_dir, showWarnings = FALSE, recursive = TRUE)

cat("\n")
cat(paste(rep("#", 70), collapse = ""), "\n")
cat("#  PSR WEEKLY SNAPSHOT EXPORT\n")
cat(paste(rep("#", 70), collapse = ""), "\n\n")

# 2. Check Prerequisites ----

coef_xg <- system.file("extdata", "psr_coefficients.csv", package = "panna")
coef_gd <- system.file("extdata", "gd_psr_coefficients.csv", package = "panna")
if (coef_xg == "" && coef_gd == "") {
  stop("PSR coefficients not found. Run step 07 (07_train_psr_model.R) first.")
}
psr_target <- if (coef_xg != "") "xg" else "goals"
if (psr_target == "goals") {
  cat("Note: xG coefficients not found, using goal-diff PSR instead.\n")
  cat("Re-run step 07 after fixing splint xG columns for xG-based PSR.\n\n")
}

slim_path <- file.path(cache_dir, "01_match_stats_slim.rds")
full_path  <- file.path(cache_dir, "01_match_stats.rds")
ms_path    <- if (file.exists(slim_path)) slim_path else full_path
if (!file.exists(ms_path)) {
  stop("Missing match stats cache. Run step 01 first.")
}

# 3. Load Data ----

cat("=== Loading Data ===\n")
cat(sprintf("  Match stats: %s\n", basename(ms_path)))
match_stats <- data.table::as.data.table(readRDS(ms_path))
if (!inherits(match_stats$match_date, "Date")) {
  match_stats[, match_date := as.Date(match_date)]
}
gc(verbose = FALSE)
cat(sprintf("  Rows: %s | Date range: %s to %s\n",
            format(nrow(match_stats), big.mark = ","),
            min(match_stats$match_date),
            max(match_stats$match_date)))

decay_params_path <- file.path(cache_dir, "02b_decay_params.rds")
decay_params <- if (file.exists(decay_params_path)) {
  cat("  Using optimized decay params\n")
  readRDS(decay_params_path)
} else {
  cat("  Using default decay params\n")
  get_default_decay_params()
}

# 4. Define Snapshot Dates ----

cat("\n=== Defining Snapshot Dates ===\n")

today       <- Sys.Date()
min_history <- min(match_stats$match_date) + 365L  # Need 1yr history minimum
cutoff_weekly <- today - 2L * 365L                  # 2 years back = weekly

# Weekly for last 2 years, monthly (every 4 weeks) before that
recent_weekly  <- seq(cutoff_weekly, today, by = "7 days")
older_monthly  <- seq(min_history, cutoff_weekly - 1L, by = "28 days")
snapshot_dates <- sort(unique(c(older_monthly, recent_weekly)))
snapshot_dates <- snapshot_dates[snapshot_dates >= min_history]

cat(sprintf("  Total snapshot dates: %d (%s to %s)\n",
            length(snapshot_dates),
            min(snapshot_dates),
            max(snapshot_dates)))
cat(sprintf("  Weekly (last 2yr): %d | Monthly (older): %d\n",
            length(recent_weekly),
            length(older_monthly[older_monthly >= min_history])))

# 5. Compute PSR at Each Snapshot Date ----

cat("\n=== Computing PSR Snapshots ===\n\n")

psr_list <- vector("list", length(snapshot_dates))
n_success <- 0L
start_time <- Sys.time()

for (i in seq_along(snapshot_dates)) {
  d <- snapshot_dates[i]

  if (i %% 50 == 0 || i == 1L) {
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    rate    <- if (i > 1) elapsed / (i - 1) else 0
    eta_min <- round(rate * (length(snapshot_dates) - i) / 60, 1)
    cat(sprintf("  [%d/%d] %s  (%.0fs elapsed, ~%.1f min remaining)\n",
                i, length(snapshot_dates), d, elapsed, eta_min))
  }

  skills <- tryCatch(
    estimate_player_skills_at_date(
      match_stats  = match_stats,
      decay_params = decay_params,
      date         = d,
      min_weighted_90s = 3
    ),
    error = function(e) NULL
  )
  if (is.null(skills) || nrow(skills) == 0) next

  psr <- tryCatch(
    compute_player_psr(skills, center = TRUE, target = psr_target),
    error = function(e) NULL
  )
  if (is.null(psr) || nrow(psr) == 0) next

  psr[, snapshot_date := d]
  psr_list[[i]] <- psr[, .(snapshot_date, player_id, player_name,
                            primary_position, psr, osr, dsr, weighted_90s)]
  n_success <- n_success + 1L
}

total_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("\nCompleted: %d / %d dates (%.0fs, %.1f sec/date)\n",
            n_success, length(snapshot_dates),
            total_secs, total_secs / max(n_success, 1)))

# 6. Combine and Export ----

cat("\n=== Exporting ===\n")

weekly_psr <- data.table::rbindlist(psr_list, fill = TRUE, use.names = TRUE)
cat(sprintf("  Total rows:    %s\n", format(nrow(weekly_psr), big.mark = ",")))
cat(sprintf("  Unique players: %s\n", format(data.table::uniqueN(weekly_psr$player_id), big.mark = ",")))
cat(sprintf("  Unique dates:   %d\n", data.table::uniqueN(weekly_psr$snapshot_date)))

out_path <- file.path(opta_dir, "opta_psr_weekly.parquet")
arrow::write_parquet(as.data.frame(weekly_psr), out_path)
mb <- round(file.info(out_path)$size / 1024^2, 1)
cat(sprintf("  Written: %s (%s MB)\n", out_path, mb))

# 7. Upload to GitHub Release ----

cat("\n=== Uploading to GitHub Release ===\n")

repo <- "peteowen1/pannadata"
tag  <- "opta-latest"

if (!requireNamespace("piggyback", quietly = TRUE)) {
  stop("Package 'piggyback' required for upload.")
}

tryCatch({
  piggyback::pb_upload(file = out_path, repo = repo, tag = tag, overwrite = TRUE)
  cat(sprintf("  Uploaded opta_psr_weekly.parquet to %s (%s)\n", repo, tag))
}, error = function(e) {
  stop(sprintf("Upload failed: %s. Weekly PSR not published.", e$message))
})

cat("\n=== COMPLETE ===\n")
cat(sprintf("  opta_psr_weekly.parquet uploaded (%s MB, %d dates)\n",
            mb, data.table::uniqueN(weekly_psr$snapshot_date)))
