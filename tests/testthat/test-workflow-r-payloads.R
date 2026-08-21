# Workflows run R through `Rscript -e "<payload>"`. The shell ends that string
# at the FIRST unescaped double quote, so a single `"` anywhere in the payload
# -- including inside an R comment, where it is harmless in a .R file --
# silently truncates the script and R dies with "unexpected end of input".
#
# That is not hypothetical. On 2026-08-21 a comment reading
#   # so a log line reading "DISABLING steps 11/12/12b/12c"
# cut predictions-pipeline.yml's payload from 97 lines to 74 and failed the
# run two minutes in. YAML validation passed, the R scripts themselves parsed,
# and the review missed it: nothing checked the payload the SHELL would build.
#
# So check that: reconstruct each payload the way the shell would, and parse it.

.workflow_r_payloads <- function(path) {
  yml <- yaml::yaml.load_file(path)
  out <- list()
  for (job in yml$jobs) {
    for (step in job$steps) {
      run <- step$run
      if (is.null(run) || !grepl('Rscript -e "', run, fixed = TRUE)) next
      # Everything after the opening quote, truncated at the next one -- which
      # is exactly what the shell passes to R.
      after <- sub('^.*?Rscript -e "', "", run)
      out[[length(out) + 1L]] <- list(
        step = if (is.null(step$name)) "(unnamed)" else step$name,
        payload = sub('".*$', "", after)
      )
    }
  }
  out
}

test_that("every Rscript -e payload in a workflow survives the shell and parses", {
  wf_dir <- testthat::test_path("..", "..", ".github", "workflows")
  skip_if_not(dir.exists(wf_dir), "workflows/ absent (installed package, not source tree)")
  skip_if_not_installed("yaml")

  files <- list.files(wf_dir, pattern = "[.]ya?ml$", full.names = TRUE)
  expect_gt(length(files), 0)

  for (f in files) {
    for (p in .workflow_r_payloads(f)) {
      err <- tryCatch({ parse(text = p$payload); NULL },
                      error = function(e) conditionMessage(e))
      expect_null(
        err,
        info = sprintf(
          paste0("%s / step '%s': the payload the SHELL builds does not parse (%s).
",
                 "  A stray double quote -- in the R code OR in a comment -- ends the ",
                 "`Rscript -e \"...\"` string early. Remove it; do not escape it."),
          basename(f), p$step, if (is.null(err)) "" else trimws(err)))
    }
  }
})
