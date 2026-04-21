# Tests for chain-derived player/period timing helpers added in the
# 2026-04-18 splint creation rebuild. These functions parse raw Opta
# event chains (type_id 30/34/18/19/17) into per-match end times and
# per-player on/off intervals at second precision.

test_that("extract_period_end_times returns empty df on empty/null input", {
  empty_in <- data.frame(
    match_id = character(0), type_id = integer(0), period_id = integer(0),
    minute = numeric(0), second = numeric(0)
  )
  out <- extract_period_end_times(empty_in)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_named(out, c("match_id", "first_half_end_time", "match_end_time"))

  out_null <- extract_period_end_times(NULL)
  expect_equal(nrow(out_null), 0)
})

test_that("extract_period_end_times pulls max time per (match, period_id)", {
  events <- data.frame(
    match_id = c("M1", "M1", "M1", "M1", "M2"),
    type_id  = c(30L,  30L,  30L,  30L,  30L),
    period_id = c(1L,  1L,  2L,  2L,  2L),
    minute  = c(45,  45,  93,  93,  92),
    second  = c(8,   17,  0,   30,  45)
  )
  out <- extract_period_end_times(events)
  expect_equal(nrow(out), 2)

  m1 <- out[out$match_id == "M1", ]
  expect_equal(m1$first_half_end_time, 45 + 17/60)  # max p1 second
  expect_equal(m1$match_end_time,      93 + 30/60)  # max p2 second

  m2 <- out[out$match_id == "M2", ]
  expect_true(is.na(m2$first_half_end_time))   # no p1 marker for M2
  expect_equal(m2$match_end_time, 92 + 45/60)
})

test_that("extract_period_end_times handles missing required columns", {
  bad <- data.frame(match_id = "M1", minute = 90)  # no type_id, period_id
  expect_warning(out <- extract_period_end_times(bad), "missing column")
  expect_equal(nrow(out), 0)
})


# Helper: build a synthetic Opta-style match with starting XI (type_id 34),
# subs (18/19), red card (17 + qualifier 33), and period-end markers (30).
.synthetic_match_events <- function(match_id = "TEST_MATCH") {
  starters_home <- paste0("HSTART", 1:11)
  bench_home    <- paste0("HBENCH", 1:7)
  starters_away <- paste0("ASTART", 1:11)
  bench_away    <- paste0("ABENCH", 1:7)

  # qualifier 30 = comma-separated player IDs; qualifier 131 = position
  # (1-11 for starters, 0 for bench).
  fmt_xi <- function(starters, bench) {
    ids <- c(starters, bench)
    pos <- c(1:11, rep(0, length(bench)))
    sprintf('{"30":"%s","131":"%s"}',
            paste(ids, collapse = ", "),
            paste(pos, collapse = ", "))
  }

  data.frame(
    match_id = match_id,
    type_id  = c(
      34L, 34L,                # 2 formation events (one per team)
      19L, 18L,                # sub at 60:30 (home brings on HBENCH1 for HSTART11)
      17L,                     # red card at 75:45 (away ASTART5)
      30L, 30L                 # period end markers
    ),
    period_id = c(1L, 1L,  2L, 2L,  2L,  1L, 2L),
    team_id   = c("HOME", "AWAY",  "HOME", "HOME",  "AWAY",  NA, NA),
    player_id = c("",  "",  "HBENCH1", "HSTART11",  "ASTART5",  "", ""),
    minute    = c(0,   0,   60,        60,           75,        45, 93),
    second    = c(0,   0,   30,        30,           45,        17, 30),
    qualifier_json = c(
      fmt_xi(starters_home, bench_home),
      fmt_xi(starters_away, bench_away),
      NA_character_, NA_character_,
      '{"33":"true"}',         # qualifier 33 = red card
      NA_character_, NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

test_that("extract_player_timing_from_events derives starters from type_id 34", {
  ev <- .synthetic_match_events()
  out <- extract_player_timing_from_events(ev)

  starters <- out[out$is_starter, ]
  bench_played <- out[!out$is_starter, ]

  # 22 starters (11 per team) — but HSTART11 was subbed off at 60:30 so still
  # appears as starter. ASTART5 got red carded but still a starter.
  expect_equal(sum(out$is_starter), 22)
  expect_true(all(starters$on_minute == 0))

  # Bench players who actually came on appear; bench-warmers are dropped.
  # Only HBENCH1 came on in our synthetic match.
  expect_equal(nrow(bench_played), 1)
  expect_equal(bench_played$player_id, "HBENCH1")
  expect_equal(bench_played$on_minute, 60 + 30/60)
})

test_that("extract_player_timing_from_events sets sub-off time for substituted starter", {
  ev <- .synthetic_match_events()
  out <- extract_player_timing_from_events(ev)
  hstart11 <- out[out$player_id == "HSTART11", ]
  expect_equal(nrow(hstart11), 1)
  expect_equal(hstart11$on_minute, 0)            # starter
  expect_equal(hstart11$off_minute, 60 + 30/60)  # sub-off time
})

test_that("extract_player_timing_from_events sets red-card off_minute", {
  ev <- .synthetic_match_events()
  out <- extract_player_timing_from_events(ev)
  red <- out[out$player_id == "ASTART5", ]
  expect_equal(nrow(red), 1)
  expect_equal(red$on_minute, 0)
  expect_equal(red$off_minute, 75 + 45/60)
})

test_that("extract_player_timing_from_events uses match_end for finishers", {
  ev <- .synthetic_match_events()
  out <- extract_player_timing_from_events(ev)
  # HSTART1 was not subbed off, not red-carded — should play to match end (93:30)
  finisher <- out[out$player_id == "HSTART1", ]
  expect_equal(finisher$on_minute, 0)
  expect_equal(finisher$off_minute, 93 + 30/60)  # type_id 30 period 2
})

test_that("extract_player_timing_from_events returns empty df on empty input", {
  empty_in <- data.frame(
    match_id = character(0), type_id = integer(0), period_id = integer(0),
    team_id = character(0), player_id = character(0),
    minute = numeric(0), qualifier_json = character(0)
  )
  out <- extract_player_timing_from_events(empty_in)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0)
  expect_true(all(c("match_id","player_id","team_id","is_starter",
                    "on_minute","off_minute") %in% names(out)))
})

test_that("extract_player_timing_from_events handles missing required columns", {
  bad <- data.frame(match_id = "M1", minute = 90, type_id = 34L)
  expect_warning(out <- extract_player_timing_from_events(bad), "missing column")
  expect_equal(nrow(out), 0)
})
