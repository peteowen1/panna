library(testthat)

# After-extra-time score + shootout winner (R/extra_time_scores.R, issue #411).
# Opta tags an own goal's team_id with the scorer's own team (qualifier 28).

.ev <- function(match_id, type_id, team_id, period_id, og = FALSE) {
  data.frame(match_id = match_id, type_id = type_id, team_id = team_id,
             period_id = period_id,
             qualifier_json = if (og) "{\"28\":null,\"56\":\"Center\"}" else "{\"56\":\"Center\"}",
             stringsAsFactors = FALSE)
}

.scores <- data.frame(
  match_id = c("final", "pens", "og_et", "reg", "scoreless", "noevents"),
  home_team_id = "H", away_team_id = "A",
  home_goals = c(0, 1, 1, 2, 0, 3), away_goals = c(0, 1, 1, 0, 0, 1),
  stringsAsFactors = FALSE
)

.events <- rbind(
  .ev("final", 1, "H", 1),                       # pass, not a goal
  .ev("final", 16, "H", 3),                      # extra-time winner for H
  .ev("pens", 16, "H", 1), .ev("pens", 16, "A", 2),
  .ev("pens", 1, "H", 3),                        # ET played, no goals
  .ev("pens", 16, "A", 5), .ev("pens", 16, "A", 5), .ev("pens", 16, "H", 5),
  .ev("og_et", 16, "H", 4, og = TRUE),           # H player scores for A
  .ev("reg", 16, "H", 1), .ev("reg", 16, "H", 2),
  .ev("scoreless", 1, "H", 1)
)

d <- derive_extra_time_scores(.events, .scores)
row <- function(id) d[d$match_id == id, ]

test_that("extra-time goals are added to the 90-minute score", {
  expect_equal(c(row("final")$home_goals_aet, row("final")$away_goals_aet), c(1, 0))
})

test_that("an extra-time own goal is credited to the opponent", {
  expect_equal(c(row("og_et")$home_goals_aet, row("og_et")$away_goals_aet), c(1, 2))
})

test_that("shootout winner is the side with more period-5 goals", {
  expect_equal(row("pens")$shootout_winner, "away")
  expect_equal(c(row("pens")$home_goals_aet, row("pens")$away_goals_aet), c(1, 1))
  expect_true(all(is.na(d$shootout_winner[d$match_id != "pens"])))
})

test_that("matches without extra time keep the 90-minute score", {
  expect_equal(c(row("reg")$home_goals_aet, row("reg")$away_goals_aet), c(2, 0))
  expect_equal(c(row("scoreless")$home_goals_aet, row("scoreless")$away_goals_aet), c(0, 0))
})

test_that("a match with no events at all is NA, not a copy of the 90-minute score", {
  expect_true(is.na(row("noevents")$home_goals_aet))
  expect_true(is.na(row("noevents")$away_goals_aet))
})

test_that("a pre-filtered events table needs events_match_ids to keep 0-0 matches", {
  goals_only <- .events[.events$type_id == 16 | .events$period_id >= 3, ]
  without <- derive_extra_time_scores(goals_only, .scores)
  expect_true(is.na(without$home_goals_aet[without$match_id == "scoreless"]))
  with <- derive_extra_time_scores(goals_only, .scores,
                                   events_match_ids = .events$match_id)
  expect_equal(with$home_goals_aet[with$match_id == "scoreless"], 0)
})

test_that("add_extra_time_scores skips domestic leagues and loads events per league season", {
  preds <- data.frame(
    match_id = c("final", "pens", "dom", "fix"),
    league = c("WC", "WC", "ENG", "WC"), season = "s",
    home_goals = c(0, 1, 2, NA), away_goals = c(0, 1, 1, NA),
    stringsAsFactors = FALSE)
  fx <- data.frame(match_id = preds$match_id, home_team_id = "H", away_team_id = "A",
                   stringsAsFactors = FALSE)
  calls <- character()
  out <- add_extra_time_scores(preds, fx, load_events = function(league, season) {
    calls <<- c(calls, paste(league, season))
    .events
  })
  expect_equal(calls, "WC s")
  expect_equal(out$home_goals_aet, c(1, 1, 2, NA))
  expect_equal(out$shootout_winner, c(NA, "away", NA, NA))
  expect_equal(out$home_goals, preds$home_goals)  # 90-minute columns untouched
})

test_that("a failed events load leaves aet NA with a warning", {
  preds <- data.frame(match_id = "final", league = "WC", season = "s",
                      home_goals = 0, away_goals = 0, stringsAsFactors = FALSE)
  fx <- data.frame(match_id = "final", home_team_id = "H", away_team_id = "A",
                   stringsAsFactors = FALSE)
  expect_warning(
    out <- add_extra_time_scores(preds, fx, load_events = function(league, season) stop("boom")),
    "boom")
  expect_true(is.na(out$home_goals_aet))
})
