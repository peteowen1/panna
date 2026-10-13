# Tests for splint creation functions

test_that("calculate_effective_minute uses continuous time with offset", {
  # Without first-half stoppage (default behavior)
  expect_equal(calculate_effective_minute(45, 3), 48)
  expect_equal(calculate_effective_minute(45, 0), 45)
  expect_equal(calculate_effective_minute(90, 11), 101)
  expect_equal(calculate_effective_minute(46, 0), 46)  # No offset when no stoppage

  # With 3 mins first-half stoppage - second half events get offset
  expect_equal(calculate_effective_minute(46, 0, first_half_stoppage = 3), 49)  # 46 + 3
  expect_equal(calculate_effective_minute(60, 0, first_half_stoppage = 3), 63)  # 60 + 3
  expect_equal(calculate_effective_minute(90, 0, first_half_stoppage = 3), 93)  # 90 + 3
  expect_equal(calculate_effective_minute(90, 11, first_half_stoppage = 3), 104) # 90 + 11 + 3

  # First half events NOT offset
  expect_equal(calculate_effective_minute(30, NA, first_half_stoppage = 3), 30)
  expect_equal(calculate_effective_minute(45, 3, first_half_stoppage = 3), 48)  # 45 + 3, no extra offset

  # Test vectorized with offset
  result <- calculate_effective_minute(c(30, 45, 46, 90), c(NA, 3L, NA, 11L), first_half_stoppage = 3)
  expect_equal(result, c(30, 48, 49, 104))
})

test_that("extract_player_timing_from_events cuts off_minute short at a red card (panna#141)", {
  # p1 = straight red (qualifier 33), p2 = second-yellow red (qualifier 32) --
  # the case the pre-fix `c("33", "14")` check silently missed, since a real
  # second yellow carries qualifier 32, never 14. p3 carries an unrelated
  # qualifier 14 (never a real red-card marker) and must play the full match.
  formation <- data.frame(
    match_id = "m1", type_id = 34L, period_id = 1L, team_id = "t1",
    player_id = NA_character_, minute = 0L,
    qualifier_json = '{"30":"p1,p2,p3","131":"1,2,3"}', stringsAsFactors = FALSE
  )
  cards <- data.frame(
    match_id = "m1", type_id = 17L, period_id = c(1L, 2L, 1L),
    team_id = "t1", player_id = c("p1", "p2", "p3"),
    minute = c(40L, 75L, 50L),
    qualifier_json = c('{"33":null}', '{"32":null}', '{"14":null}'),
    stringsAsFactors = FALSE
  )
  match_end <- data.frame(
    match_id = "m1", type_id = 30L, period_id = 2L, team_id = "t1",
    player_id = NA_character_, minute = 90L, qualifier_json = NA_character_,
    stringsAsFactors = FALSE
  )
  events <- rbind(formation, cards, match_end)

  result <- extract_player_timing_from_events(events)

  # Continuous clock (R009): the last first-half event is p3's card at 50
  # (period 1), so the first half ran 5 minutes over and every period-2 time
  # shifts by +5: p2's 75' red -> 80, the 90' final whistle -> 95.
  expect_equal(result$off_minute[result$player_id == "p1"], 40)
  expect_equal(result$off_minute[result$player_id == "p2"], 80)
  # p3's qualifier 14 is not a red-card marker -- plays to match end.
  expect_equal(result$off_minute[result$player_id == "p3"], 95)
})

test_that(".continuous_minute puts both halves (and extra time) on one clock (R009)", {
  cm <- panna:::.continuous_minute
  # first half ran to 48:30; second half restarts at 45 on Opta's clock
  expect_equal(cm(c(30, 47, 45, 46.5, 90), c(1L, 1L, 2L, 2L, 2L), 48.5), c(30, 47, 48.5, 50, 93.5))
  # period-1 and period-2 times at the same raw clock land in different places
  expect_true(cm(46.5, 2L, 48.5) > cm(46.5, 1L, 48.5))
  # extra time starts at raw 90 = the real second-half end (raw 94 -> 97.5),
  # so raw 95 is 5 minutes into extra time: 102.5
  expect_equal(cm(95, 3L, 48.5, 94), 102.5)
  # no first-half end: unchanged
  expect_equal(cm(50, 2L, NA), 50)
})

test_that(".attach_event_periods recovers the period of overlap-window events (R009)", {
  me <- data.frame(match_id = "m1", type_id = c(19L, 16L, 30L), period_id = c(2L, 1L, 1L),
                   minute = c(45L, 47L, 48L), second = c(0L, 10L, 30L), team_id = c("t1", "t2", "t1"))
  ev <- data.frame(match_id = "m1", event_type = c("substitution", "goal", "goal", "substitution"),
                   minute = c(45L, 47L, 20L, 70L), second = c(0L, 10L, 0L, 0L),
                   team_id = c("t1", "t2", "t1", "t2"))
  out <- panna:::.attach_event_periods(ev, me)
  # 45:00 sub is the half-time sub (period 2); 47:10 goal is first-half stoppage
  expect_equal(out$period_id, c(2L, 1L, 1L, 2L))
})

test_that("a half-time sub's on-minute follows the whole first half on the continuous clock (R009)", {
  formation <- data.frame(match_id = "m1", type_id = 34L, period_id = 1L, team_id = "t1",
    player_id = NA_character_, minute = 0L, second = 0L,
    qualifier_json = '{"30":"p1,p2","131":"1,0"}', stringsAsFactors = FALSE)
  ends <- data.frame(match_id = "m1", type_id = 30L, period_id = c(1L, 2L), team_id = "t1",
    player_id = NA_character_, minute = c(48L, 94L), second = c(30L, 0L), qualifier_json = NA_character_)
  subs <- data.frame(match_id = "m1", type_id = c(18L, 19L), period_id = 2L, team_id = "t1",
    player_id = c("p1", "p2"), minute = 45L, second = 0L, qualifier_json = NA_character_)
  tm <- extract_player_timing_from_events(rbind(formation, ends, subs))
  # first half ended 48:30, so the 45:00 half-time sub happens at 48.5 and the
  # 94:00 whistle at 97.5 -- never inside first-half stoppage
  expect_equal(tm$off_minute[tm$player_id == "p1"], 48.5)
  expect_equal(tm$on_minute[tm$player_id == "p2"], 48.5)
  expect_equal(tm$off_minute[tm$player_id == "p2"], 97.5)
})

test_that("create_all_splints reports failed matches and aborts when too many fail (R048)", {
  mk <- function(ids, league = "L1") list(
    lineups = data.frame(match_id = ids, player_id = "p"),
    shooting = data.frame(match_id = ids),
    results = data.frame(match_id = ids, league = league)
  )
  fake <- function(match_id, ...) {
    if (match_id %in% bad) stop("boom")
    list(splints = data.frame(match_id = match_id, splint_num = 1L, duration = 1),
         players = data.frame(match_id = match_id), match_info = data.frame(match_id = match_id))
  }
  testthat::local_mocked_bindings(create_match_splints_fast = fake)

  # 1 of 300 failing (0.33%) is tolerated but must be announced with its id
  bad <- "m1"
  ids <- paste0("m", 1:300)
  expect_message(
    out <- create_all_splints(mk(ids), verbose = FALSE, chunk_by = "none"),
    "m1"
  )
  expect_equal(nrow(out$splints), 299)

  # 1 of 10 (10%) is tolerated: a small cup is not stopped by one bad match
  out <- suppressMessages(create_all_splints(mk(paste0("m", 1:10)), verbose = FALSE, chunk_by = "none"))
  expect_equal(nrow(out$splints), 9)

  # 3 of 10 (30%, and more than 2) aborts
  bad <- c("m1", "m2", "m3")
  suppressMessages(expect_error(
    create_all_splints(mk(paste0("m", 1:10)), verbose = FALSE, chunk_by = "none"),
    "failed for 3 of 10"
  ))
})

test_that("create_all_splints aborts on NA league when chunking by league (R048)", {
  p <- list(
    lineups = data.frame(match_id = c("a", "b"), player_id = "p"),
    shooting = data.frame(match_id = c("a", "b")),
    results = data.frame(match_id = c("a", "b"), league = c("L1", NA))
  )
  expect_error(create_all_splints(p, verbose = FALSE, chunk_by = "league"), "NA league")
})
