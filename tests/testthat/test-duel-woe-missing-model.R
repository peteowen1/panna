# R108: a duel contest with no sub-model must give NA expectation/WOE, not a
# zero expectation that makes WOE equal raw wins.

.duel_woe_mocks <- function(env = parent.frame()) {
  prep <- function(...) data.table::data.table(
    player_id = c("p1", "p1", "p2"), team_id = "t1", won = c(1, 0, 1))
  testthat::local_mocked_bindings(
    .order_events = function(events) data.table::as.data.table(events),
    .finalize_duel = function(out) out,
    .predict_contest = function(sub, features) {
      if (is.null(sub)) rep(NA_real_, nrow(features)) else rep(0.25, nrow(features))
    },
    .DUEL_PREP_FUNS = list(aerial_win = prep, aerial_poss = prep, takeon = prep,
                           tackle_poss = prep, containment = prep),
    .env = environment(compute_duel_woe)
  )
}

test_that("compute_duel_woe: complete model gives won - exp and no warning", {
  .duel_woe_mocks()
  dm <- list(aerial_win = 1, aerial_poss = 1, takeon = 1, tackle_poss = 1, containment = 1)
  expect_no_warning(out <- compute_duel_woe(data.frame(x = 1), dm))
  p1 <- out[player_id == "p1"]
  expect_equal(p1$tackle_poss_won, 1)
  expect_equal(p1$tackle_poss_exp, 0.5)
  expect_equal(p1$tackle_poss_woe, 0.5)
})

test_that("compute_duel_woe: a missing sub-model warns and yields NA exp/woe, not woe == wins", {
  .duel_woe_mocks()
  dm <- list(aerial_win = 1, aerial_poss = 1, takeon = 1, tackle_poss = NULL, containment = 1)
  expect_warning(out <- compute_duel_woe(data.frame(x = 1), dm), "tackle_poss")
  expect_equal(out[player_id == "p1"]$tackle_poss_won, 1)   # raw wins still reported
  expect_true(all(is.na(out$tackle_poss_exp)))
  expect_true(all(is.na(out$tackle_poss_woe)))
  expect_false(anyNA(out$takeon_woe))                       # other contests untouched
})
