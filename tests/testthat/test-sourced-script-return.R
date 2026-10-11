# A top-level return() inside a source()d script does NOT stop the script: it
# only ends that one expression. Pipeline step scripts are sourced via
# run_step()/run_pipeline_step() as `function() source(file, local = TRUE)`
# (review R060/R068/R076), so early exits must use if/else or repeat+break.

run_like_run_step <- function(path) {
  out <- character(0)
  e <- new.env()
  e$log <- function(x) out <<- c(out, x)
  f <- function() source(path, local = e)
  f()
  out
}

test_that("top-level return() in a sourced script does not stop it (the bug)", {
  p <- tempfile(fileext = ".R")
  writeLines(c("if (TRUE) { log('hit'); return(invisible(NULL)) }",
               "log('after')"), p)
  expect_identical(run_like_run_step(p), c("hit", "after"))
})

test_that("repeat + break is a real early exit in a sourced script", {
  p <- tempfile(fileext = ".R")
  writeLines(c("repeat {",
               "  if (TRUE) { log('hit'); break }",
               "  log('after')",
               "  break",
               "}",
               "log('tail')"), p)
  expect_identical(run_like_run_step(p), c("hit", "tail"))
})

# Regression guard: no data-raw script may have a return() reachable at top
# level (through if/for/{ ), since it silently does nothing when sourced.
test_that("no data-raw script has a top-level return()", {
  root <- testthat::test_path("..", "..", "data-raw")
  skip_if_not(dir.exists(root), "data-raw not available (R CMD check)")
  has_top_return <- function(e) {
    if (!is.call(e)) return(FALSE)
    h <- e[[1]]
    if (identical(h, as.name("function"))) return(FALSE)
    if (identical(h, as.name("return"))) return(TRUE)
    if (identical(h, as.name("local"))) return(FALSE)  # return() is valid in local()
    for (a in as.list(e)[-1]) if (!missing(a) && has_top_return(a)) return(TRUE)
    FALSE
  }
  files <- list.files(root, "[.]R$", recursive = TRUE, full.names = TRUE)
  bad <- character(0)
  for (f in files) {
    ex <- tryCatch(parse(f, keep.source = TRUE), error = function(e) NULL)
    if (is.null(ex)) next
    sr <- attr(ex, "srcref")
    for (i in seq_along(ex)) {
      if (has_top_return(ex[[i]])) {
        bad <- c(bad, sprintf("%s:%d", basename(f), sr[[i]][1]))
      }
    }
  }
  expect_identical(bad, character(0))
})
