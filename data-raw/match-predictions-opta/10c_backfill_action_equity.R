# 10c_backfill_action_equity.R
# Local backfill for per-season action_equity.parquet files.
#
# Runs 10c_export_equity.R over multiple historical seasons, producing
# action_equity_<season>.parquet for each. Designed to run locally.
#
# Usage (from panna/ directory):
#   Rscript data-raw/match-predictions-opta/10c_backfill_action_equity.R
#
# Or interactively:
#   devtools::load_all()
#   source("data-raw/match-predictions-opta/10c_backfill_action_equity.R")

# 1. Config ----

if (!"panna" %in% (.packages())) {
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(quiet = TRUE)
  } else {
    stop("devtools required. Run: install.packages('devtools')")
  }
}

# Override-safe config (same pattern as 10b_backfill_game_logs.R).
if (!exists("equity_seasons", inherits = FALSE)) {
  equity_seasons <- c(
    "2015-2016", "2016-2017", "2017-2018", "2018-2019",
    "2019-2020", "2020-2021", "2021-2022", "2022-2023",
    "2023-2024", "2024-2025", "2025-2026"
  )
}

if (!exists("force_rebuild", inherits = FALSE))    force_rebuild <- FALSE
if (!exists("upload_equity", inherits = FALSE))    upload_equity <- TRUE
if (!exists("parallel_workers", inherits = FALSE)) parallel_workers <- 1L

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

# 2. Filter already-built ----

if (!isTRUE(force_rebuild)) {
  existing <- vapply(equity_seasons, function(s) {
    file.exists(file.path(cache_dir, sprintf("action_equity_%s.parquet", s)))
  }, logical(1))
  if (any(existing)) {
    message(sprintf("Skipping already-built seasons: %s",
                    paste(equity_seasons[existing], collapse = ", ")))
    message("  (set force_rebuild <- TRUE to rebuild)")
    equity_seasons <- equity_seasons[!existing]
  }
}

if (length(equity_seasons) == 0) {
  message("\nAll seasons already built. Nothing to do.")
  return(invisible(NULL))
}

message(sprintf("\n=== Equity backfill plan ==="))
message(sprintf("  Seasons: %s", paste(equity_seasons, collapse = ", ")))
message(sprintf("  Upload:  %s", if (isTRUE(upload_equity)) "yes" else "no (dry run)"))
message(sprintf("  Workers: %d", parallel_workers))

# 3. Delegate to 10c ----

t0 <- Sys.time()

if (parallel_workers <= 1L) {
  source("data-raw/match-predictions-opta/10c_export_equity.R", local = FALSE)
} else {
  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("parallel_workers > 1 requires `future` + `future.apply`. ",
         "Install with: install.packages(c('future', 'future.apply'))")
  }
  message(sprintf("\n  Parallel mode: %d workers (multisession)", parallel_workers))

  future::plan(future::multisession, workers = parallel_workers)
  on.exit(future::plan(future::sequential), add = TRUE)

  worker_wd <- getwd()
  .run_one_equity_season <- function(s) {
    setwd(worker_wd)
    suppressMessages(devtools::load_all(".", quiet = TRUE))
    ge <- globalenv()
    assign("equity_seasons", s,     envir = ge)
    assign("upload_equity",  FALSE, envir = ge)
    assign("build_equity",   TRUE,  envir = ge)
    assign("cache_dir",
           file.path("data-raw", "cache-predictions-opta"),
           envir = ge)
    source("data-raw/match-predictions-opta/10c_export_equity.R", local = FALSE)
    p <- file.path(ge$cache_dir, sprintf("action_equity_%s.parquet", s))
    if (file.exists(p)) p else NULL
  }

  built <- future.apply::future_lapply(equity_seasons, .run_one_equity_season,
                                        future.seed = NULL)
  built <- Filter(Negate(is.null), built)
  message(sprintf("\n  Parallel build complete: %d/%d seasons produced",
                  length(built), length(equity_seasons)))

  if (isTRUE(upload_equity) && length(built) > 0) {
    build_equity <- FALSE
    source("data-raw/match-predictions-opta/10c_export_equity.R", local = FALSE)
  }
}

elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
message(sprintf("\nEquity backfill complete in %.1f min", elapsed))
