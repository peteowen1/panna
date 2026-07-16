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

  expect_equal(result$off_minute[result$player_id == "p1"], 40)
  expect_equal(result$off_minute[result$player_id == "p2"], 75)
  # p3's qualifier 14 is not a red-card marker -- plays to match end.
  expect_equal(result$off_minute[result$player_id == "p3"], 90)
})
