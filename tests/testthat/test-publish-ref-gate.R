# R026/R099: every dispatchable publishing workflow must switch publishing off
# on any ref other than main or dev, and the opta pipeline must serialise runs.
# Reads .github/, which R CMD check's tarball excludes, so this skips there
# (the source-tests CI job runs it).

wf_path <- function(f) testthat::test_path("..", "..", ".github", "workflows", f)

publishers <- c("psr-weekly-snapshot.yml", "epr-weekly-snapshot.yml",
                "opta-pipeline.yml", "predictions-pipeline.yml",
                "epv-pipeline.yml")

test_that("every publishing workflow defines the ref-based publish switch", {
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(wf_path(publishers[1])), ".github not available (R CMD check)")
  for (f in publishers) {
    wf <- yaml::read_yaml(wf_path(f))
    expr <- wf$env$PANNA_PUBLISH_DISABLED
    expect_true(is.character(expr), info = f)
    expect_match(expr, "refs/heads/main", fixed = TRUE, info = f)
    expect_match(expr, "refs/heads/dev", fixed = TRUE, info = f)
    expect_match(expr, "!=", fixed = TRUE, info = f)
  }
})

test_that("shell upload steps are gated on the publish switch", {
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(wf_path(publishers[1])), ".github not available (R CMD check)")
  # Steps whose whole job is to upload must carry the condition.
  must_gate <- list(
    "opta-pipeline.yml" = "Upload caches to predictions-cache release",
    "epv-pipeline.yml" = c("Upload models (immediately after training)",
                           "Upload trained models"),
    "predictions-pipeline.yml" = "Trigger blog data build"
  )
  for (f in names(must_gate)) {
    steps <- yaml::read_yaml(wf_path(f))$jobs[[1]]$steps
    for (nm in must_gate[[f]]) {
      st <- Filter(function(s) identical(s$name, nm), steps)
      expect_length(st, 1L)
      expect_match(st[[1]]$`if`, "PANNA_PUBLISH_DISABLED != 'true'",
                   fixed = TRUE, info = paste(f, nm))
    }
  }
})

test_that("opta-pipeline serialises runs and only uploads caches after success", {
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(wf_path("opta-pipeline.yml")), ".github not available (R CMD check)")
  wf <- yaml::read_yaml(wf_path("opta-pipeline.yml"))
  # main/dev share the group; other refs (dry builds) get their own queue.
  expect_match(wf$concurrency$group,
               "(github.ref == 'refs/heads/main' || github.ref == 'refs/heads/dev') && 'opta-pipeline'",
               fixed = TRUE)
  expect_false(isTRUE(wf$concurrency$`cancel-in-progress`))
  steps <- wf$jobs[[1]]$steps
  up <- Filter(function(s) identical(s$name, "Upload caches to predictions-cache release"), steps)[[1]]
  expect_match(up$`if`, "success()", fixed = TRUE)
  expect_false(grepl("always()", up$`if`, fixed = TRUE))
})

test_that("release-writing workflows share their queue only on main/dev", {
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(wf_path(publishers[1])), ".github not available (R CMD check)")
  # GitHub keeps one pending run per group, so a dry branch run in the shared group
  # would replace a pending publish. The shared name must sit behind the main/dev test.
  shared <- c("psr-weekly-snapshot.yml" = "panna-release-writer",
              "epr-weekly-snapshot.yml" = "panna-release-writer",
              "predictions-pipeline.yml" = "panna-release-writer",
              "opta-pipeline.yml" = "opta-pipeline",
              "career-panna.yml" = "opta-pipeline")
  for (f in names(shared)) {
    g <- yaml::read_yaml(wf_path(f))$concurrency$group
    expect_match(g, sprintf("(github.ref == 'refs/heads/main' || github.ref == 'refs/heads/dev') && '%s' || format('%s-dry-{0}', github.ref)",
                            shared[[f]], shared[[f]]), fixed = TRUE, info = f)
  }
})

test_that("psr skills rebuild runs even when an earlier step failed (R100)", {
  skip_if_not_installed("yaml")
  skip_if_not(file.exists(wf_path("psr-weekly-snapshot.yml")), ".github not available (R CMD check)")
  steps <- yaml::read_yaml(wf_path("psr-weekly-snapshot.yml"))$jobs[[1]]$steps
  st <- Filter(function(s) identical(s$name, "Rebuild and export the skill estimates"), steps)[[1]]
  expect_match(st$`if`, "!cancelled()", fixed = TRUE)
})

test_that("08b smoke-test guard uses plain exists() so the isolated driver can set it (R065)", {
  p <- testthat::test_path("..", "..", "data-raw", "estimated-skills", "08b_export_psr_weekly.R")
  skip_if_not(file.exists(p), "data-raw not available (R CMD check)")
  src <- readLines(p, warn = FALSE)
  expect_false(any(grepl('exists("psr_smoke_n", inherits = FALSE)', src, fixed = TRUE)))
  expect_true(any(grepl('exists("psr_smoke_n")', src, fixed = TRUE)))
})

test_that("step 13 publishes nothing when PANNA_PUBLISH_DISABLED is set, and publishes when it is not (R026)", {
  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "13_publish_release_data.R")
  skip_if_not(file.exists(script), "data-raw not available (R CMD check)")
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines("a,b", f)
  assign("publish_files", list(predictions_latest = f, blog_latest = f), envir = .GlobalEnv)
  withr::defer(rm("publish_files", envir = .GlobalEnv))

  run_step <- function(disabled) {
    calls <- character(0)
    env <- new.env(parent = globalenv())
    env$vb_publish <- function(paths, repo, tag, ...) {
      calls <<- c(calls, tag)
      list(generation = "g")
    }
    withr::with_envvar(c(PANNA_PUBLISH_DISABLED = disabled),
                       suppressMessages(source(script, local = env)))
    calls
  }
  expect_length(run_step("true"), 0L)
  expect_equal(run_step("false"), c("predictions-latest", "blog-latest"))
})
