test_that("R038: reference checks that run zero facts warn instead of reporting success", {
  d <- withr::local_tempdir()
  expect_warning(res <- run_wc2026_reference_checks(d), "ran 0 of")
  expect_length(res, 0L)
})
