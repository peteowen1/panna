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
