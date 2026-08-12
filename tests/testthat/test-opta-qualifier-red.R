# Shared Opta dismissal test (R/utils.R::opta_qualifier_is_red)
#
# This was a byte-identical private copy in splint_creation.R and wp_model.R.
# The two consumers must agree on who was sent off: splint_creation drives
# minutes played, wp_model drives the win-probability man-count. If a new
# dismissal qualifier were added to one copy only, they would silently
# disagree about who was on the pitch.

test_that("straight red (33) and second yellow (32) both count", {
  expect_true(panna:::opta_qualifier_is_red('{"33":""}'))
  expect_true(panna:::opta_qualifier_is_red('{"32":""}'))
  expect_true(panna:::opta_qualifier_is_red('{"13":"1","33":""}'))
})

test_that("a plain yellow is not a dismissal", {
  expect_false(panna:::opta_qualifier_is_red('{"31":""}'))
  expect_false(panna:::opta_qualifier_is_red('{}'))
})

test_that("missing or unparseable qualifier JSON is not a dismissal", {
  # Deliberately FALSE, not an error: a card we cannot read is not evidence
  # of a dismissal, and erroring would drop a whole season over one bad event.
  expect_false(panna:::opta_qualifier_is_red(NA_character_))
  expect_false(panna:::opta_qualifier_is_red("not json at all"))
  expect_false(panna:::opta_qualifier_is_red(""))
})

test_that("both consumers call the shared helper, not a private copy", {
  src <- c(readLines(test_path("..", "..", "R", "splint_creation.R"),
                     warn = FALSE),
           readLines(test_path("..", "..", "R", "wp_model.R"), warn = FALSE))
  expect_false(any(grepl("detect_red_in_qj\\s*<-\\s*function", src)),
               info = "a private red-card parser has reappeared")
  expect_true(sum(grepl("opta_qualifier_is_red", src)) >= 2)
})
