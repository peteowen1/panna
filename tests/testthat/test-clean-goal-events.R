# .clean_goal_events(): phantom 0:00 copies and goal-team credit (review R022).

mt <- data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A")

goals <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  x$event_type <- "goal"
  x
}

test_that("phantom 0:00 copies of timed goals are dropped, one per timed goal", {
  # Norwich 2-1 Sunderland shape: every goal also appears at 0:00.
  ev <- goals(match_id = "m1",
              team_id   = c("H", "H", "A", "H", "H", "A"),
              player_id = c("p1", "p2", "p3", "p1", "p2", "p3"),
              minute    = c(0, 0, 0, 31, 48, 86),
              second    = c(0, 0, 0, 0, 0, 0))
  shots <- data.frame(match_id = "m1", player_id = c("p1", "p2", "p3"),
                      team_id = c("H", "H", "A"), minute = c(30, 47, 85),
                      is_own_goal = FALSE)
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(nrow(out), 3L)
  expect_true(all(out$minute > 0))
  expect_equal(sum(out$team_id == "H"), 2L)
  expect_equal(attr(out, "goal_cleaning")[["phantoms_dropped"]], 3L)
})

test_that("a 0:00 goal with no timed copy by that player is kept", {
  ev <- goals(match_id = "m1", team_id = c("H", "A"), player_id = c("p1", "p9"),
              minute = c(0, 50), second = c(0, 10))
  shots <- data.frame(match_id = "m1", player_id = "p9", team_id = "A",
                      minute = 50, is_own_goal = FALSE)
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(nrow(out), 2L)
})

test_that("an own goal is credited to the opponent of the scorer's team", {
  # Fulham 1-1 Man Utd shape: Martinez (away) own goal logged under away.
  ev <- goals(match_id = "m1", team_id = c("A", "A"), player_id = c("pa", "martinez"),
              minute = c(20, 62), second = c(5, 40))
  shots <- data.frame(match_id = "m1", player_id = c("pa", "martinez"),
                      team_id = c("A", "A"), minute = c(20, 62),
                      is_own_goal = c(FALSE, TRUE))
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(out$team_id, c("A", "H"))
  expect_equal(out$is_own_goal, c(FALSE, TRUE))
  expect_equal(attr(out, "goal_cleaning")[["own_goals_flipped"]], 1L)
})

test_that("old-season minute offset (+1, second 0) still matches the own goal", {
  ev <- goals(match_id = "m1", team_id = "H", player_id = "pinnock",
              minute = 57, second = 0)
  shots <- data.frame(match_id = "m1", player_id = "pinnock", team_id = "H",
                      minute = 56, is_own_goal = TRUE)
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(out$team_id, "A")
})

test_that("a goal row under the wrong team takes the shooter's team", {
  ev <- goals(match_id = "m1", team_id = "A", player_id = "toure", minute = 70, second = 0)
  shots <- data.frame(match_id = "m1", player_id = "toure", team_id = "H",
                      minute = 68, is_own_goal = FALSE)
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(out$team_id, "H")
  expect_equal(attr(out, "goal_cleaning")[["wrong_team_fixed"]], 1L)
})

test_that("non-goal events and unmatched goals are left alone; row order kept", {
  ev <- rbind(
    goals(match_id = "m1", team_id = "H", player_id = "p1", minute = 10, second = 0),
    data.frame(match_id = "m1", team_id = "A", player_id = "p2", minute = 15,
               second = 0, event_type = "yellow_card"),
    goals(match_id = "m1", team_id = "A", player_id = "p3", minute = 80, second = 0)
  )
  shots <- data.frame(match_id = "m1", player_id = "zz", team_id = "H",
                      minute = 40, is_own_goal = FALSE)
  out <- .clean_goal_events(ev, shots, mt)
  expect_equal(out$event_type, c("goal", "yellow_card", "goal"))
  expect_equal(out$team_id, c("H", "A", "A"))
  expect_s3_class(out, "data.frame")
  expect_false(data.table::is.data.table(out))
})
