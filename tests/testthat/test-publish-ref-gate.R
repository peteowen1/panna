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

test_that("step 02 refuses a publishing run without career_panna_asof.parquet, warns otherwise", {
  script <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                "02_player_ratings_to_team.R")
  skip_if_not(file.exists(script), "data-raw not available (R CMD check)")
  skip_if_not_installed("arrow")
  src <- readLines(script, warn = FALSE)
  i0 <- grep("^asof_path <- ", src)
  i1 <- grep("^# Validate required columns", src)
  expect_length(i0, 1L); expect_length(i1, 1L)
  block <- parse(text = src[i0:(i1 - 1L)])

  utils_env <- new.env(parent = globalenv())
  sys.source(file.path(testthat::test_path("..", ".."), "data-raw", "pipeline_utils.R"),
             envir = utils_env)

  run_block <- function(data_dir, run_steps) {
    env <- new.env(parent = environment())
    env$is_publish_run <- utils_env$is_publish_run
    env$opta_data_dir <- function() data_dir
    env$run_steps <- run_steps
    env$played <- data.frame(season_end_year = 2025L, match_date = as.Date("2024-08-17"))
    env$ratings <- data.frame(player_id = c("a", "b"), season_end_year = 2025L,
                              panna = c(9, 9), offense = c(9, 9), defense = c(9, 9))
    for (e in block) eval(e, env)
    env$ratings
  }

  empty_dir <- withr::local_tempdir()
  full_dir <- withr::local_tempdir()
  arrow::write_parquet(
    data.frame(player_id = "a", ref_date = as.Date("2024-08-01"), panna = 1,
               panna_offense = 0.5, panna_defense = 0.5,
               sign_convention = CAREER_PANNA_SIGN_CONVENTION),
    file.path(full_dir, "career_panna_asof.parquet"))
  publish <- list(step_13_publish_release_data = TRUE)
  local_run <- list(step_13_publish_release_data = FALSE, step_09_upload_predictions = FALSE)

  withr::with_envvar(c(PANNA_PUBLISH_DISABLED = "false"), {
    expect_error(run_block(empty_dir, publish), "Refusing to publish")
    expect_warning(run_block(empty_dir, local_run), "career_panna_asof.parquet not found")
    expect_silent(suppressMessages(r <- run_block(full_dir, publish)))
    expect_equal(r$panna, c(1, NA))   # real file: overridden, unmatched player is NA
  })
  withr::with_envvar(c(PANNA_PUBLISH_DISABLED = "true"),
                     expect_warning(run_block(empty_dir, publish), "not found"))
})
