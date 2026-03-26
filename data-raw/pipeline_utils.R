# pipeline_utils.R
# Shared helper functions for ALL pipeline runners (Opta, FBref, Skills, Predictions)
#
# Source this file from any run_*.R pipeline script to get:
#   run_step()              - execute a step with timing, error handling, and traceback
#   check_critical_step()   - halt downstream steps on failure
#   print_pipeline_summary() - print a formatted step summary table
#   handle_force_rebuild()  - clear cache files for a given pipeline
#   clear_cache_files()     - generic cache clearing with lettered step support

#' Run a named pipeline step with timing and error handling
#'
#' Wraps a code block in tryCatch, prints status banners, and records
#' timing information. Skips if the step is disabled in run_steps or
#' if pipeline_failed is TRUE.
#'
#' Supports both numeric (1, 2, 3) and lettered ("2b", "8b") step numbers.
#'
#' @param step_name Character name of the step (e.g., "load_data")
#' @param step_num Step number: integer (e.g., 1) or string (e.g., "2b")
#' @param code_block Function (thunk) to execute
#' @param run_steps Named list of step_XX_name = TRUE/FALSE flags
#' @param pipeline_failed Logical; if TRUE, step is skipped with "SKIPPED" status
#' @return List with step, name, status, duration_secs, duration_formatted;
#'         NULL if step is disabled in run_steps
run_step <- function(step_name, step_num, code_block, run_steps,
                     pipeline_failed = FALSE) {
  # Build step key: numeric -> "step_01_name", lettered -> "step_2b_name"
  step_label <- as.character(step_num)
  if (is.numeric(step_num)) {
    step_key <- sprintf("step_%02d_%s", step_num, step_name)
  } else {
    padded <- sub("^(\\d)([a-z])", "0\\1\\2", step_label)
    step_key <- sprintf("step_%s_%s", padded, step_name)
  }

  if (!isTRUE(run_steps[[step_key]])) {
    message(sprintf("\n[%s] Step %s: %s - SKIPPED",
                    format(Sys.time(), "%H:%M:%S"), step_label, step_name))
    return(NULL)
  }

  message(sprintf("\n%s", paste(rep("=", 70), collapse = "")))
  message(sprintf("[%s] Step %s: %s",
                  format(Sys.time(), "%H:%M:%S"), step_label, step_name))
  message(sprintf("%s\n", paste(rep("=", 70), collapse = "")))

  # Skip execution if a previous step failed
  if (isTRUE(pipeline_failed)) {
    message("  SKIPPED (previous step failed)")
    return(list(step = step_num, name = step_name, status = "SKIPPED",
                duration_secs = 0, duration_formatted = "0.0 seconds"))
  }

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


#' Check if a step result indicates failure
#'
#' Two modes:
#'   - 3-arg form (legacy): check_critical_step(step_num, step_name, step_results)
#'   - 1-arg form: check_critical_step(result) where result is a single step result
#'
#' @param result_or_num A step result list, or an integer step number (legacy form)
#' @param step_name Character name for error message (legacy form only)
#' @param step_results List of step results (legacy form only)
#' @return TRUE if the step failed, FALSE otherwise
check_critical_step <- function(result_or_num, step_name = NULL, step_results = NULL) {
  if (is.null(result_or_num)) return(FALSE)

  # 1-arg form: result_or_num is a step result list
  if (is.list(result_or_num)) {
    result <- result_or_num
    if (!is.null(result) && identical(result$status, "FAILED")) {
      message(sprintf("\nCRITICAL: Step %s (%s) failed - halting downstream steps.",
                      result$step, result$name))
      return(TRUE)
    }
    return(FALSE)
  }

  # Legacy 3-arg form: result_or_num is a step number
  step_num <- result_or_num
  if (step_num > length(step_results)) return(FALSE)
  result <- step_results[[step_num]]
  if (!is.null(result) && identical(result$status, "FAILED")) {
    message(sprintf("\nCRITICAL: Step %s (%s) failed - halting downstream steps.",
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
#' @param col_width Column width for step name (default 30)
print_pipeline_summary <- function(step_results, pipeline_start,
                                   pipeline_name = "PIPELINE", col_width = 30) {
  pipeline_end <- Sys.time()
  total_duration <- difftime(pipeline_end, pipeline_start, units = "secs")

  message("\n")
  message(paste(rep("=", 70), collapse = ""))
  message(paste(pipeline_name, "COMPLETE"))
  message(paste(rep("=", 70), collapse = ""))

  fmt <- sprintf("%%-%ds %%-10s %%s", col_width)
  message(sprintf("\n%s", sprintf(fmt, "Step", "Status", "Duration")))
  message(paste(rep("-", col_width + 20), collapse = ""))

  for (result in step_results) {
    if (!is.null(result)) {
      message(sprintf(fmt, result$name, result$status, result$duration_formatted))
    }
  }

  message(paste(rep("-", col_width + 20), collapse = ""))
  message(sprintf(fmt, "TOTAL", "", format_duration(as.numeric(total_duration))))
}


#' Clear cache files from a given step onwards
#'
#' Supports both numeric and lettered steps (e.g., "2b", "8b").
#'
#' @param force_rebuild_from Step to rebuild from (numeric or string), or NULL
#' @param cache_dir Character path to cache directory
#' @param cache_files Named list mapping step keys to character vectors of filenames
#' @param max_step Maximum numeric step number
clear_cache_files <- function(force_rebuild_from, cache_dir, cache_files, max_step) {
  if (is.null(force_rebuild_from)) return(invisible(NULL))

  rebuild_num <- as.numeric(sub("[a-z]+$", "", as.character(force_rebuild_from)))
  if (is.na(rebuild_num) || rebuild_num < 1 || rebuild_num > max_step) {
    warning(sprintf("Invalid force_rebuild_from value '%s' (must be 1-%d). Ignoring.",
                    force_rebuild_from, max_step))
    return(invisible(NULL))
  }

  # Build list of steps to clear: numeric steps >= rebuild + lettered steps
  steps_to_clear <- as.character(rebuild_num:max_step)
  fractional_steps <- setdiff(names(cache_files), as.character(1:max_step))
  for (fs in fractional_steps) {
    parent <- as.numeric(sub("[a-z]+$", "", fs))
    if (!is.na(parent) && parent >= rebuild_num) {
      steps_to_clear <- c(steps_to_clear, fs)
    }
  }

  files_to_delete <- unlist(cache_files[steps_to_clear])
  deleted <- 0
  for (f in files_to_delete) {
    fpath <- file.path(cache_dir, f)
    if (file.exists(fpath)) {
      if (file.remove(fpath)) {
        deleted <- deleted + 1
      } else {
        warning(sprintf("Could not delete cache file: %s", fpath))
      }
    }
  }
  message(sprintf("\n[Force rebuild] Cleared %d cache files from step %s onwards\n",
                  deleted, force_rebuild_from))
}


#' Handle force rebuild for Opta RAPM pipeline (legacy wrapper)
#'
#' @param force_rebuild_from Integer step to rebuild from (or NULL for no rebuild)
#' @param cache_dir Character path to cache directory
#' @param max_step Maximum step number (default 9)
handle_force_rebuild <- function(force_rebuild_from, cache_dir, max_step = 9) {
  opta_cache_files <- list(
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
  clear_cache_files(force_rebuild_from, cache_dir, opta_cache_files, max_step)
}
