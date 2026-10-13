test_that("assert_step_output says why a check errored (R133)", {
  checks <- list(
    has_rows = function(d) nrow(d) > 0,
    bad_col = function(d) stop("column {x} missing"),
    false_check = function(d) FALSE
  )
  w <- tryCatch(assert_step_output(data.frame(a = 1), "s", checks),
                warning = function(w) conditionMessage(w))
  expect_match(w, "2 / 3 output assertions failed", fixed = TRUE)
  expect_match(w, "bad_col (check errored: column {x} missing)", fixed = TRUE)
  expect_match(w, "false_check", fixed = TRUE)
  expect_false(grepl("false_check (check errored", w, fixed = TRUE))
  expect_error(assert_step_output(data.frame(a = 1), "s", checks, severity = "abort"),
               "check errored", fixed = TRUE)
})
