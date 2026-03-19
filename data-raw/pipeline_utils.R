# pipeline_utils.R
# Shared helper functions for pipeline runners (Opta and FBref)
#
# Source this file from run_pipeline_opta.R and run_pipeline.R to avoid
# duplicating the run_step() and check_critical_step() patterns.

#' Run a named pipeline step with timing and error handling
#'
#' Wraps a code block in tryCatch, prints status banners, and records
#' timing information. Skips if the step is disabled in run_steps.
#'
#' @param step_name Character name of the step (e.g., "load_data")
#' @param step_num Integer step number (e.g., 1)
#' @param code_block Function (thunk) to execute
#' @param run_steps Named list of step_XX_name = TRUE/FALSE flags
#' @return List with step, name, status, duration_secs, duration_formatted
run_step <- function(step_name, step_num, code_block, run_steps) {
  step_key <- sprintf("step_%02d_%s", step_num, step_name)
  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %d: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_num, step_name))
    return(NULL)
  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %d: %s",
                  format(Sys.time(), "%H:%M:%S"), step_num, step_name))
  message(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  start_time <- Sys.time()
  result <- tryCatch(
    withCallingHandlers({
      code_block()
      "SUCCESS"
    }, error = function(e) {
      # Capture traceback before stack unwinds
      message(sprintf("ERROR: %s", e$message))
      tb <- sys.calls()
      if (length(tb) > 2) {
        message("Traceback (most recent calls):")
        # Show last few calls (skip withCallingHandlers/tryCatch boilerplate)
        n <- min(length(tb), 10)
        for (i in seq(max(1, length(tb) - n + 1), length(tb))) {
          message(sprintf("  %s", deparse(tb[[i]], nlines = 1)))
        }
      }
    }),
    error = function(e) "FAILED"
  )
  end_time <- Sys.time()

  duration <- difftime(end_time, start_time, units = "secs")

  list(
    step = step_num,
    name = step_name,
    status = result,
    duration_secs = as.numeric(duration),
    duration_formatted = format_duration(as.numeric(duration))
  )
}


#' Check if a critical step failed and set pipeline_failed flag
#'
#' Inspects step_results for a failed step and halts downstream processing.
#'
#' @param step_num Integer step number to check
#' @param step_name Character name for error message
#' @param step_results List of step result lists
#' @return TRUE if the step failed, FALSE otherwise
check_critical_step <- function(step_num, step_name, step_results) {
  if (step_num > length(step_results)) return(FALSE)
  result <- step_results[[step_num]]
  if (!is.null(result) && result$status == "FAILED") {
    message(sprintf("\nCRITICAL: Step %d (%s) failed - halting downstream steps.",
                    step_num, step_name))
    return(TRUE)
  }
  FALSE
}


#' Print pipeline summary table
#'
#' @param step_results List of step result lists
#' @param pipeline_start POSIXct start time
#' @param pipeline_name Character name for the banner (e.g., "OPTA PIPELINE")
print_pipeline_summary <- function(step_results, pipeline_start, pipeline_name = "PIPELINE") {
  pipeline_end <- Sys.time()
  total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

  message("\n")
  message(paste(rep("=", 70), collapse = ""))
  message(paste(pipeline_name, "COMPLETE"))
  message(paste(rep("=", 70), collapse = ""))

  message("\nStep Summary:")
  message(sprintf("%-25s %-10s %s", "Step", "Status", "Duration"))
  message(paste(rep("-", 50), collapse = ""))

  for (result in step_results) {
    if (!is.null(result)) {
      message(sprintf("%-25s %-10s %s",
                      result$name,
                      result$status,
                      result$duration_formatted))
    }
  }

  message(paste(rep("-", 50), collapse = ""))
  message(sprintf("%-25s %-10s %s", "TOTAL", "", format_duration(as.numeric(total_duration))))
}


#' Handle force rebuild by clearing cache files
#'
#' @param force_rebuild_from Integer step to rebuild from (or NULL for no rebuild)
#' @param cache_dir Character path to cache directory
#' @param max_step Maximum step number (default 9)
handle_force_rebuild <- function(force_rebuild_from, cache_dir, max_step = 9) {
  if (is.null(force_rebuild_from) || force_rebuild_from < 1 || force_rebuild_from > max_step) {
    return(invisible(NULL))
  }

  cache_files <- list(
    "1" = c("01_raw_data.rds", "01_config.rds"),
    "2" = "02_processed_data.rds",
    "3" = "03_splints.rds",
    "4" = "04_rapm.rds",
    "5" = "05_spm.rds",
    "6" = "06_xrapm.rds",
    "7" = c("07_seasonal_ratings.rds", "seasonal_spm.csv", "seasonal_rapm.csv", "seasonal_xrapm.csv"),
    "8" = c("08_panna.rds", "panna_ratings.csv"),
    "9" = character(0)
  )

  files_to_delete <- unlist(cache_files[as.character(force_rebuild_from:max_step)])
  deleted <- 0
  for (f in files_to_delete) {
    fpath <- file.path(cache_dir, f)
    if (file.exists(fpath)) {
      file.remove(fpath)
      deleted <- deleted + 1
    }
  }
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %d onwards\n",
                  deleted, force_rebuild_from))
}
