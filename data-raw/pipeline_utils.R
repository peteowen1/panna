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


#' Validate pipeline step output before proceeding
#'
#' Checks that a data frame or RDS cache file has at least the expected
#' number of rows. Stops the pipeline with an informative error if
#' the output is empty or unexpectedly small.
#'
#' @param data Data frame to validate, or NULL to read from cache_path
#' @param cache_path Path to RDS cache file (used if data is NULL)
#' @param step_name Character label for error messages
#' @param min_rows Minimum expected rows (default 1). Set higher for
#'   known-large outputs (e.g., min_rows = 1000 for splints).
#' @param warn_below Optional threshold: warn (but don't stop) if rows
#'   are below this count. Useful for catching partial data loads.
validate_step_output <- function(data = NULL, cache_path = NULL,
                                 step_name = "step", min_rows = 1L,
                                 warn_below = NULL) {
  if (is.null(data) && !is.null(cache_path)) {
    if (!file.exists(cache_path)) {
      stop(sprintf("[%s] Expected cache file not found: %s", step_name, cache_path))
    }
    data <- readRDS(cache_path)
  }

  if (is.null(data)) {
    stop(sprintf("[%s] Output is NULL — step produced no data", step_name))
  }

  if (!is.data.frame(data) && is.list(data)) {
    # For list outputs (e.g., RAPM results), check the first data frame element
    df_elements <- Filter(is.data.frame, data)
    if (length(df_elements) > 0) {
      data <- df_elements[[1]]
    } else {
      message(sprintf("[%s] Output is a list (not a data frame); skipping row validation", step_name))
      return(invisible(TRUE))
    }
  }

  n <- nrow(data)
  if (is.null(n)) {
    message(sprintf("[%s] Output has no rows attribute; skipping row validation", step_name))
    return(invisible(TRUE))
  }

  if (n < min_rows) {
    stop(sprintf("[%s] Output has %d rows, expected at least %d — aborting to prevent downstream corruption",
                 step_name, n, min_rows))
  }

  if (!is.null(warn_below) && n < warn_below) {
    warning(sprintf("[%s] Output has only %d rows (expected ~%d+). Data may be incomplete.",
                    step_name, n, warn_below))
  }

  message(sprintf("[%s] Validated: %s rows", step_name, format(n, big.mark = ",")))
  invisible(TRUE)
}


#' Save cache file with metadata sidecar
#'
#' Saves an RDS file and writes a .meta.json sidecar with timestamp,
#' row count, and pipeline name. Consuming pipelines can use
#' \code{load_cache_with_meta()} to validate freshness.
#'
#' @param data Object to save (typically a data frame or list)
#' @param path Path for the RDS file
#' @param pipeline Character name of the producing pipeline
save_cache_with_meta <- function(data, path, pipeline = "unknown") {
  saveRDS(data, path)

  n_rows <- if (is.data.frame(data)) nrow(data)
            else if (is.list(data)) {
              dfs <- Filter(is.data.frame, data)
              if (length(dfs) > 0) nrow(dfs[[1]]) else NA
            } else NA

  meta <- list(
    written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    pipeline = pipeline,
    n_rows = n_rows,
    file = basename(path)
  )
  meta_path <- paste0(path, ".meta.json")
  writeLines(jsonlite::toJSON(meta, auto_unbox = TRUE, pretty = TRUE), meta_path)
  invisible(path)
}


#' Load cache file with freshness validation
#'
#' Reads an RDS cache file and optionally checks its metadata sidecar
#' to ensure the data isn't stale (older than \code{max_age_hours}).
#'
#' @param path Path to the RDS file
#' @param max_age_hours Maximum age in hours. NULL = no age check.
#'   Default 168 (1 week).
#' @param expected_pipeline If not NULL, warns if the producing pipeline
#'   doesn't match (catches accidental file overwrites).
#' @return The loaded R object
load_cache_with_meta <- function(path, max_age_hours = 168,
                                 expected_pipeline = NULL) {
  if (!file.exists(path)) {
    stop(sprintf("Cache file not found: %s", path))
  }

  meta_path <- paste0(path, ".meta.json")
  if (file.exists(meta_path)) {
    meta <- jsonlite::fromJSON(meta_path)

    if (!is.null(max_age_hours) && !is.null(meta$written_at)) {
      written <- as.POSIXct(meta$written_at, format = "%Y-%m-%dT%H:%M:%S%z")
      age_hours <- as.numeric(difftime(Sys.time(), written, units = "hours"))
      if (!is.na(age_hours) && age_hours > max_age_hours) {
        warning(sprintf("Cache %s is %.0f hours old (max: %d). Data may be stale.",
                        basename(path), age_hours, max_age_hours))
      }
    }

    if (!is.null(expected_pipeline) && !is.null(meta$pipeline)) {
      if (meta$pipeline != expected_pipeline) {
        warning(sprintf("Cache %s was written by '%s', expected '%s'.",
                        basename(path), meta$pipeline, expected_pipeline))
      }
    }

    message(sprintf("Loading %s (written: %s, rows: %s)",
                    basename(path),
                    if (!is.null(meta$written_at)) meta$written_at else "unknown",
                    if (!is.null(meta$n_rows)) format(meta$n_rows, big.mark = ",") else "unknown"))
  } else {
    message(sprintf("Loading %s (no metadata sidecar found)", basename(path)))
  }

  readRDS(path)
}


#' Retry a function with exponential backoff
#'
#' Useful for wrapping transient network operations (GitHub API, piggyback uploads).
#'
#' @param fn Function (thunk) to call — should take no arguments
#' @param max_retries Maximum number of retry attempts (default 3)
#' @param initial_delay_secs Initial delay before first retry (default 5)
#' @param label Human-readable label for log messages
#' @return The result of fn() if successful
retry_with_backoff <- function(fn, max_retries = 3L, initial_delay_secs = 5,
                               label = "operation") {
  last_error <- NULL
  for (attempt in seq_len(max_retries + 1L)) {
    result <- tryCatch(fn(), error = function(e) e)
    if (!inherits(result, "error")) return(result)
    last_error <- result
    if (attempt <= max_retries) {
      delay <- initial_delay_secs * (2 ^ (attempt - 1))
      message(sprintf("[Retry] %s failed (attempt %d/%d): %s. Retrying in %ds...",
                      label, attempt, max_retries + 1L, conditionMessage(last_error), delay))
      Sys.sleep(delay)
    }
  }
  stop(sprintf("%s failed after %d attempts: %s",
               label, max_retries + 1L, conditionMessage(last_error)), call. = FALSE)
}
