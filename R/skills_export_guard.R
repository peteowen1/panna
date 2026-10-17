# Pre-publish gate for the skills export (data-raw/estimated-skills/08_export_skills.R).
# Review R064: that step used to upload with no check at all, so a thin or
# half-empty rebuild replaced the live file on the blog.

#' Assert a skills/match-stats table is fit to publish
#'
#' Stops (before anything is uploaded) when the new table would be a regression
#' on what is live. Three checks:
#' \enumerate{
#'   \item no skill column (names matching `skill_pattern`) is 100% `NA`;
#'   \item row count is at least `min_row_frac` of the previously published
#'     table (skipped when `prev` is `NULL`);
#'   \item the seasons (`season_end_year`) and, where both tables carry one,
#'     the `league` values of the previous table are all still present (the new
#'     table may add, never drop; skipped when `prev` is `NULL`).
#' }
#'
#' @param new data.frame about to be published.
#' @param prev data.frame currently published, or `NULL` when it could not be
#'   read (the row-count and coverage checks are then skipped and say so).
#' @param min_row_frac minimum new/previous row ratio.
#' @param skill_pattern regex selecting the skill columns.
#' @param label name used in messages.
#' @return Invisibly, a list of what was checked (`skill_cols`, `n_new`,
#'   `n_prev`, `prev_checked`).
#' @noRd
assert_skills_export_ok <- function(new, prev = NULL, min_row_frac = 0.9,
                                    skill_pattern = "_p90$",
                                    label = "skills export") {
  problems <- character(0)

  skill_cols <- grep(skill_pattern, names(new), value = TRUE)
  if (length(skill_cols) == 0L) {
    problems <- c(problems, sprintf("no columns match '%s' (wrong table?)", skill_pattern))
  } else {
    all_na <- skill_cols[vapply(skill_cols, function(cn) all(is.na(new[[cn]])), logical(1))]
    if (length(all_na) > 0L) {
      problems <- c(problems, sprintf("%d skill column(s) are 100%% NA: %s",
                                      length(all_na), paste(utils::head(all_na, 10), collapse = ", ")))
    }
  }

  n_new <- nrow(new)
  n_prev <- NA_integer_
  prev_checked <- !is.null(prev)
  if (prev_checked) {
    n_prev <- nrow(prev)
    if (n_prev > 0L && n_new < min_row_frac * n_prev) {
      problems <- c(problems, sprintf(
        "%s rows is below %.0f%% of the %s rows currently published",
        format(n_new, big.mark = ","), min_row_frac * 100, format(n_prev, big.mark = ",")))
    }
    for (col in c("season_end_year", "league")) {
      if (col %in% names(new) && col %in% names(prev)) {
        lost <- setdiff(unique(prev[[col]][!is.na(prev[[col]])]),
                        unique(new[[col]][!is.na(new[[col]])]))
        if (length(lost) > 0L) {
          problems <- c(problems, sprintf("%s value(s) in the published table are missing from the new one: %s",
                                          col, paste(utils::head(sort(lost), 10), collapse = ", ")))
        }
      }
    }
  } else {
    message(sprintf("  [%s] previous published table not available: row-count and coverage checks SKIPPED",
                    label))
  }

  if (length(problems) > 0L) {
    stop(sprintf("%s refused, nothing uploaded:\n  - %s", label,
                 paste(problems, collapse = "\n  - ")), call. = FALSE)
  }
  message(sprintf("  [%s] pre-publish checks passed (%d skill columns, %s rows%s)",
                  label, length(skill_cols), format(n_new, big.mark = ","),
                  if (prev_checked) sprintf(", previous %s", format(n_prev, big.mark = ",")) else ""))
  invisible(list(skill_cols = skill_cols, n_new = n_new, n_prev = n_prev,
                 prev_checked = prev_checked))
}

#' Why a snapshot file would shrink what is published, if it would
#'
#' Compares the per-snapshot row counts of a file about to be published with
#' the published copy. A published snapshot date missing from the new file is
#' a problem, and so is one whose new row count is below `min_row_frac` of the
#' published count: the dates-only check passed a file with every date kept
#' but most of each date's rows gone.
#'
#' @param pub_dates Snapshot date of every row in the published file.
#' @param new_dates Snapshot date of every row in the new file.
#' @param min_row_frac Minimum new/published row ratio per snapshot date.
#' @return Character vector of problems, empty when the new file keeps
#'   everything.
#' @noRd
.snapshot_shrink_problems <- function(pub_dates, new_dates, min_row_frac = 0.9) {
  pub_n <- table(as.character(pub_dates))
  new_n <- table(as.character(new_dates))
  problems <- character(0)
  lost <- setdiff(names(pub_n), names(new_n))
  if (length(lost) > 0L) {
    problems <- c(problems, sprintf("%d of %d published snapshot dates are missing (e.g. %s)",
                                    length(lost), length(pub_n), sort(lost)[1]))
  }
  kept <- intersect(names(pub_n), names(new_n))
  ratio <- as.numeric(new_n[kept]) / as.numeric(pub_n[kept])
  shrunk <- kept[ratio < min_row_frac]
  if (length(shrunk) > 0L) {
    first <- sort(shrunk)[1]
    problems <- c(problems, sprintf(
      "%d snapshot date(s) have under %.0f%% of their published rows (e.g. %s: %d of %d)",
      length(shrunk), min_row_frac * 100, first, as.integer(new_n[first]), as.integer(pub_n[first])))
  }
  problems
}

#' Read the currently published copy of a release asset, or NULL
#'
#' Downloads to a temp dir (never over a local file). Any failure (offline, no
#' such asset, unreadable parquet) returns `NULL` so the caller can skip the
#' comparison and say so.
#'
#' `columns` reads only those columns (the ones present; the first column when
#' none are, so `nrow()` still holds). `assert_skills_export_ok()` reads only
#' the row count, `season_end_year` and `league` from `prev`, and reading all
#' 422 columns of `opta_match_stats.parquet` OOM-killed step 08 on the 16 GB
#' GHA runner (run 38836860556) with the new full-width table resident.
#' @noRd
read_published_asset <- function(file_name, repo = "peteowen1/pannadata",
                                 tag = "opta-latest", columns = NULL) {
  dest <- tempfile("published_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)
  tryCatch({
    piggyback::pb_download(file_name, repo = repo, tag = tag, dest = dest, show_progress = FALSE)
    path <- file.path(dest, file_name)
    if (!file.exists(path)) return(NULL)
    if (is.null(columns)) return(as.data.frame(arrow::read_parquet(path)))
    reader <- arrow::ParquetFileReader$create(path)
    avail <- reader$GetSchema()$names
    idx <- match(intersect(columns, avail), avail)
    if (length(idx) == 0L) idx <- 1L
    as.data.frame(reader$ReadTable(idx - 1L))   # 0-based column indices
  }, error = function(e) {
    message(sprintf("  could not read published %s: %s", file_name, conditionMessage(e)))
    NULL
  })
}
