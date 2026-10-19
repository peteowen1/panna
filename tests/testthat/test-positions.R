# position_roles(), position_slot_table(), position_match()

q <- function(...) {
  x <- list(...)
  paste0("{", paste(sprintf('"%s":"%s"', names(x), unlist(x)), collapse = ","), "}")
}

starter_ids <- paste0("p", 1:11)
ko_json <- q(`30` = paste(c(starter_ids, "s1", "s2", "s3", "s4"), collapse = ", "),
             `131` = paste(c(1:11, 0, 0, 0, 0), collapse = ", "), `130` = "8")

fixture_lineups <- function() {
  data.table::data.table(
    match_id = c(rep("m1", 15), rep("m2", 3)),
    team_id = "A",
    player_id = c(starter_ids, "s1", "s2", "s3", "s4", "q1", "q2", "q3"),
    position = c("Goalkeeper", "Defender", "", "Defender", "Defender",
                 "Defensive Midfielder", "Attacking Midfielder", "Defensive Midfielder",
                 "Striker", "Attacking Midfielder", "Attacking Midfielder",
                 rep("Substitute", 4),
                 "Defender", "", "Substitute"),
    position_side = c("Centre", "Right", "", "Centre/Right", "Left/Centre",
                      "Left/Centre", "Right", "Centre/Right", "Centre", "Centre", "Left",
                      rep(NA, 4),
                      "Left", "", NA),
    is_starter = c(rep(TRUE, 11), rep(FALSE, 4), TRUE, TRUE, FALSE),
    minutes_played = c(rep(90, 11), 30, 10, 0, 5, 90, 90, 20))
}

fixture_events <- function() {
  data.table::data.table(
    match_id = "m1", team_id = "A",
    player_id = c(NA, "s1", NA, "s2", NA, "s4"),
    type_id = c(34L, 19L, 40L, 19L, 40L, 19L),
    period_id = c(16L, 2L, 2L, 2L, 2L, 2L),
    minute = c(0, 60, 70, 80, 85, 85),
    second = c(0, 0, 0, 0, 10, 10),
    qualifier_json = c(ko_json, q(`145` = "2", `44` = "Defender"),
                       q(`30` = "p1, p2", `131` = "1, 2", `130` = "2"),
                       q(`145` = "7", `44` = "Midfielder", `292` = "9", `293` = "3"),
                       q(`30` = "p1, p2", `131` = "1, 2", `130` = "3"),
                       q(`145` = "4", `44` = "Defender")))
}

fixture_table <- data.table::data.table(
  formation = c(8L, 2L, 3L), slot = c(3L, 7L, 4L),
  role_full = c("LB", "RM", "LCB"), n = 100L, share = 0.9)

test_that("role16 from the shared map reproduces classify_role() exactly", {
  m <- panna:::POSITION_ROLE_MAP
  expect_identical(position_roles(m$position, m$position_side)$role16,
                   classify_role(m$position, m$position_side))
  expect_identical(nrow(m), nrow(unique(m, by = c("position", "position_side"))))
  expect_true(all(position_roles(m$position, m$position_side)$role8 %in% PSR_ROLE8_LEVELS))
})

test_that("position_roles keeps sides in role_full and mirrors them in role8", {
  r <- position_roles(c("Defender", "Defender", "Defender", "Goalkeeper", "Substitute", ""),
                      c("Left/Centre", "Centre/Right", "Left", NA, NA, "Left"))
  expect_identical(r$role_full, c("LCB", "RCB", "LB", "GK", NA, NA))
  expect_identical(r$role8, c("CB", "CB", "FB", "GK", NA, NA))
  expect_identical(r$role4, c("DEF", "DEF", "DEF", "GK", NA, NA))
})

test_that("position_match gives every route its source and position", {
  lu <- fixture_lineups()
  out <- position_match(lu, fixture_events(), slot_table = fixture_table)
  expect_identical(out$player_id, lu$player_id)          # input row order
  get <- function(pid, col) out[[col]][out$player_id == pid]
  # a labelled starter keeps Opta's own label
  expect_identical(get("p2", "match_role_full"), "RB")
  expect_identical(get("p2", "match_role_source"), "opta")
  # a sub into slot 2 with the kick-off shape unchanged inherits p2's label
  expect_identical(get("s1", "match_role_full"), "RB")
  expect_identical(get("s1", "match_role8"), "FB")
  expect_identical(get("s1", "match_role_source"), "slot")
  # Opta's own sub position wins over the slot; the slot's position is kept
  expect_identical(get("s2", "match_role_full"), "ST")
  expect_identical(get("s2", "match_role_source"), "opta_sub")
  # after a formation change the slot means something else: use the table
  expect_identical(get("s2", "match_formation"), 2L)
  expect_identical(get("s2", "match_slot_role_full"), "RM")
  expect_identical(get("s1", "match_slot_role_full"), "RB")
  # a change logged in the same second as the sub is the shape he joined
  expect_identical(get("s4", "match_formation"), 3L)
  expect_identical(get("s4", "match_role_full"), "LCB")
  # a blank-label starter is placed by his own kick-off slot
  expect_identical(get("p3", "match_slot"), 3L)
  expect_identical(get("p3", "match_role_full"), "LB")
  expect_identical(get("p3", "match_role_source"), "slot_table")
  # an unused sub has no position
  expect_identical(get("s3", "match_role_source"), "unused")
  expect_true(is.na(get("s3", "match_role8")))
  # no event data: labelled starter still opta, the rest none
  expect_identical(get("q1", "match_role_source"), "opta")
  expect_identical(get("q2", "match_role_source"), "none")
  expect_identical(get("q3", "match_role_source"), "none")
  expect_true(is.na(get("q3", "match_role8")))
})

test_that("Opta sub qualifiers decode to positions", {
  sp <- panna:::.sub_position_from_qualifiers(c("3", "3", "4", "4", "9", "9", "6", "7", "8", "10", "1", "0", NA),
                                              c("1", "2", "4", "3", "1", "3", "2", "3", "1", "3", "3", "0", NA))
  r <- position_roles(sp$position, sp$position_side)
  expect_identical(r$role_full, c("LB", "RB", "LCB", "CB", "LST", "ST", "RW", "CM", "LW", "AM", "GK", NA, NA))
  expect_identical(r$role8, c("FB", "FB", "CB", "CB", "ST", "ST", "W", "CM", "W", "AM", "GK", NA, NA))
})

test_that("a sub never inherits from a later formation change", {
  # the change at minute 70 must not reach s1, who came on at 60
  out <- position_match(fixture_lineups(), fixture_events(), slot_table = fixture_table)
  expect_identical(out$match_formation[out$player_id == "s1"], 8L)
})

test_that("position_slot_table learns the most common label per formation slot", {
  lu <- fixture_lineups()
  tb <- position_slot_table(lu, fixture_events())
  expect_identical(tb[formation == 8L & slot == 2L, role_full], "RB")
  expect_identical(tb[formation == 8L & slot == 2L, n], 1L)
  # the blank-label starter's slot has no labelled example
  expect_identical(nrow(tb[formation == 8L & slot == 3L]), 0L)
  # only kick-off shapes are learned from
  expect_identical(sort(unique(tb$formation)), 8L)
})

test_that("missing columns abort with the column named", {
  lu <- fixture_lineups()[, !"minutes_played"]
  expect_error(position_match(lu, fixture_events(), fixture_table), "minutes_played")
  ev <- fixture_events()[, !"qualifier_json"]
  expect_error(position_match(fixture_lineups(), ev, fixture_table), "qualifier_json")
})

asof_fixture <- function() {
  data.table::data.table(
    player_id = c("a", "a", "a", "a", "a", "a", "b"),
    match_date = as.Date(c("2020-01-01", "2020-01-08", "2020-01-08", "2020-01-15",
                           "2022-06-01", "2022-06-08", "2020-01-01")),
    minutes = c(90, 90, 30, 90, 90, 90, 90),
    match_role_full = c("LB", "LCB", "LCB", "RCB", "RCB", NA, "ST"),
    match_role_source = c("opta", "opta", "opta_sub", "opta", "opta", "none", "opta"))
}

test_that("position_asof only sees earlier dates", {
  out <- position_asof(asof_fixture())
  # first match: nothing earlier
  expect_identical(out$asof_role_source[1], "none")
  expect_true(is.na(out$asof_role8[1]))
  # both 2020-01-08 rows see only the 01-01 LB start, never each other
  expect_identical(out$asof_role_full[2:3], c("LB", "LB"))
  # 01-15: LB 90 v LCB 120 -> LCB; role8 FB 90 v CB 120 -> CB
  expect_identical(out$asof_role_full[4], "LCB")
  expect_identical(out$asof_role8[4], "CB")
  # 2022: the 365-day window is empty, so the whole earlier career votes
  expect_identical(out$asof_role_source[5], "all_prior")
  expect_identical(out$asof_role16[5], "CB")
  # 2022-06-08: window holds the 06-01 RCB start
  expect_identical(out$asof_role_source[6], "window")
  expect_identical(out$asof_role_full[6], "RCB")
  # another player's minutes never leak in
  expect_identical(out$asof_role_source[7], "none")
})

test_that("position_asof votes each grouping separately and breaks ties by level order", {
  d <- data.table::data.table(
    player_id = "c", match_date = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04")),
    minutes = c(60, 50, 50, 90), match_role_full = c("CM", "LM", "RM", NA),
    match_role_source = c("opta", "opta", "slot", "unused"))
  out <- position_asof(d)
  # role_full: CM 60 beats LM 50 and RM 50; role8: W 100 beats CM 60
  expect_identical(out$asof_role_full[4], "CM")
  expect_identical(out$asof_role8[4], "W")
  # at 01-03: CM 60 v LM 50 -> CM; on 01-02 only CM
  expect_identical(out$asof_role8[3], "CM")
  # a tie (LM 50 v RM 50 in role16 with CM 60 ahead) is not needed; test one directly
  t <- data.table::data.table(player_id = "d", match_date = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03")),
                              minutes = c(45, 45, 90), match_role_full = c("RB", "LB", NA),
                              match_role_source = c("opta", "opta", "none"))
  expect_identical(position_asof(t)$asof_role_full[3], "LB")   # LB precedes RB in level order
})

test_that("unused and filled-in rows never vote, and the row order is kept", {
  d <- asof_fixture()[c(7, 4, 1, 6, 2, 5, 3)]
  expect_identical(position_asof(d), position_asof(asof_fixture())[c(7, 4, 1, 6, 2, 5, 3)])
  expect_error(position_asof(d[, !"minutes"]), "minutes")
})

test_that("the position level orders match the constants they copy", {
  expect_identical(panna:::.POSITION_LEVELS$role8, PSR_ROLE8_LEVELS)
  expect_setequal(panna:::.POSITION_LEVELS$role16, unique(panna:::POSITION_ROLE_MAP$role16))
})

test_that("the window is the window_days before the match, edge included", {
  d <- data.table::data.table(
    player_id = "e",
    match_date = as.Date(c("2020-01-01", "2020-01-02", "2021-01-01", "2021-01-02")),
    minutes = c(90, 90, 10, 90), match_role_full = c("ST", "XX", "CB", NA),
    match_role_source = c("opta", "opta", "opta", "none"))
  out <- position_asof(d, window_days = 365L)
  # 2021-01-01 is 366 days after 2020-01-01: outside; 2020-01-02 (365 days) inside but
  # unmapped, so it never votes -> window empty -> whole career -> ST
  expect_identical(out$asof_role_source[3], "all_prior")
  expect_identical(out$asof_role_full[3], "ST")
  # 2021-01-02: the 10-minute CB start sits in the window, the ST start does not
  expect_identical(out$asof_role_full[4], "CB")
  expect_identical(position_asof(d, window_days = 366L)$asof_role_full[3], "ST")
  expect_identical(position_asof(d, window_days = 366L)$asof_role_source[3], "window")
})

season_fixture <- function() {
  data.table::data.table(
    player_id = c("a", "a", "a", "a", "a", "b"),
    season = c("2019-2020", "2019-2020", "2020", "2020-2021", "2020-2021", "2020"),
    minutes = c(90, 60, 90, 0, 90, 30),
    match_role_full = c("LCB", "LB", "RB", NA, NA, "ST"),
    match_role_source = c("opta", "opta", "opta_sub", "unused", "none", "slot"))
}

test_that("position_career votes over every match, per grouping", {
  out <- position_career(season_fixture())
  # a: LCB 90, LB 60, RB 90 -> LCB and RB tie on 90; LCB comes first in level order
  expect_identical(out$career_role_full[1:5], rep("LCB", 5))
  # role8: FB 150 beats CB 90 -- voted separately, not coarsened from LCB
  expect_identical(out$career_role8[1], "FB")
  expect_identical(out$career_role_source[6], "career")
  expect_identical(out$career_role_full[6], "ST")
})

test_that("position_season uses the season end year and falls back to career", {
  out <- position_season(season_fixture())
  # 2019-2020 and calendar 2020 share end year 2020: LCB 90, LB 60, RB 90 -> LCB
  expect_identical(out$season_role_full[1:3], rep("LCB", 3))
  expect_identical(out$season_role_source[1], "season")
  # 2020-2021 has no real position -> career
  expect_identical(out$season_role_source[4:5], c("career", "career"))
  expect_identical(out$season_role8[4], "FB")
  # row order kept on shuffled input
  sh <- season_fixture()[c(6, 3, 1, 5, 2, 4)]
  expect_identical(position_season(sh), position_season(season_fixture())[c(6, 3, 1, 5, 2, 4)])
  expect_error(position_season(season_fixture()[, !"season"]), "season")
})

test_that("season votes are per player, and missing player ids abort", {
  d <- data.table::data.table(player_id = c("a", "b", "a", "b"), season = "2020-2021",
                              minutes = 90, match_role_full = c("ST", "GK", "ST", "GK"),
                              match_role_source = "opta")
  expect_identical(position_season(d)$season_role_full, c("ST", "GK", "ST", "GK"))
  d$player_id[2] <- NA
  expect_error(position_season(d), "player_id")
  expect_error(position_career(d), "player_id")
  d$match_date <- as.Date("2020-09-01")
  expect_error(position_asof(d), "player_id")
})

test_that(".position_role4_any reads role4 codes, labels with side, and bare labels", {
  f <- panna:::.position_role4_any
  expect_identical(f(c("MID", "Defensive Midfielder", "Substitute", "Striker", "Attacking Midfielder", "", NA)),
                   c("MID", "MID", NA, "FWD", "MID", NA, NA))
  expect_identical(f(c("Attacking Midfielder", "Defender"), c("Left", "Centre")), c("FWD", "DEF"))
})

test_that("wide midfielders in a back three with no full-back slot become wing-backs", {
  tb <- data.table::data.table(
    formation = rep(c(17L, 11L, 8L), each = 4),
    slot = rep(1:4, 3),
    role_full = c("LCB", "CB", "RCB", "LM",    # 3-4-2-1: no full-back slot
                  "LCB", "CB", "RCB", "LWB",   # 5-4-1: wing-backs labelled already
                  "LCB", "RCB", "LB", "LM"),   # back four
    n = 10L, share = 1)
  expect_identical(panna:::.wingback_formations(tb), 17L)
  lu <- data.table::data.table(match_id = c("m", "m", "n", "o"), team_id = "A", player_id = c("p1", "p2", "p3", "p4"),
                               position = "Midfielder", position_side = c("Left", "Right", "Left", "Left"),
                               is_starter = TRUE, minutes_played = 90)
  ev <- data.table::data.table(match_id = c("m", "n", "o"), team_id = "A", player_id = NA, type_id = 34L,
                               period_id = 16L, minute = 0, second = 0,
                               qualifier_json = c(q(`30` = "p1, p2", `131` = "4, 5", `130` = "17"),
                                                  q(`30` = "p3", `131` = "4", `130` = "11"),
                                                  q(`30` = "p4", `131` = "4", `130` = "8")))
  out <- position_match(lu, ev, slot_table = tb)
  expect_identical(out$match_role_full, c("LWB", "RWB", "LM", "LM"))
  expect_identical(out$match_role8, c("FB", "FB", "W", "W"))
})
