# Regression test for panna#441: a smoke run (psr_smoke_n > 0) keeps only the
# newest snapshot dates, so 08b must never write its output over the local
# full-history opta_psr_weekly.parquet. The helper evaluates the real output-path
# lines from 08b_export_psr_weekly.R, not a copy of them, so it catches a regression
# in the script itself.

psr_out_name <- function(psr_smoke_n = NULL, psr_raw_mode = FALSE) {
  p <- testthat::test_path("..", "..", "data-raw", "estimated-skills", "08b_export_psr_weekly.R")
  src <- readLines(p, warn = FALSE)
  start <- grep("^\\.psr_smoke <- ", src)[1]
  end <- start - 1L + grep('else "opta_psr_weekly.parquet")', src[start:length(src)], fixed = TRUE)[1]
  stopifnot(!is.na(start), !is.na(end))
  env <- new.env(parent = baseenv())
  env$opta_dir <- "OPTA_DIR"
  env$psr_raw_mode <- psr_raw_mode
  if (!is.null(psr_smoke_n)) env$psr_smoke_n <- psr_smoke_n
  eval(parse(text = src[start:end]), envir = env)
  basename(env$out_path)
}

test_that("08b smoke run writes its own file, never the full-history opta_psr_weekly.parquet (panna#441)", {
  p <- testthat::test_path("..", "..", "data-raw", "estimated-skills", "08b_export_psr_weekly.R")
  skip_if_not(file.exists(p), "data-raw not available (R CMD check)")
  expect_equal(psr_out_name(), "opta_psr_weekly.parquet")
  expect_equal(psr_out_name(psr_smoke_n = 0), "opta_psr_weekly.parquet")
  expect_equal(psr_out_name(psr_smoke_n = 2), "opta_psr_weekly_SMOKE.parquet")
  expect_equal(psr_out_name(psr_smoke_n = 2, psr_raw_mode = TRUE), "opta_psr_weekly_SMOKE.parquet")
  expect_equal(psr_out_name(psr_raw_mode = TRUE), "opta_psr_weekly_RAW_uncalibrated.parquet")
})
