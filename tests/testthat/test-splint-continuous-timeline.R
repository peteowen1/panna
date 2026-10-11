# Review R009 / panna#300: Opta's period-2 clock restarts at 45:00 however long
# first-half stoppage ran, so clock 45:00-48:00 exists twice. The splint
# pipeline puts everything on one continuous timeline (period p >= 2 shifted
# by the cumulative overrun of earlier periods).

test_that("period_clock_map shifts later periods by cumulative overrun", {
  # 3 min first-half stoppage, 3 min second-half stoppage, extra time
  m <- period_clock_map(c(48, 93, 106, 121))
  expect_equal(m$offset, c(0, 3, 6, 7, 8))
  expect_equal(m$start[2:4], c(48, 96, 112))
  expect_equal(m$end, c(48, 96, 112, 128))

  # Period 2 only known: ET periods contribute nothing
  m2 <- period_clock_map(c(47.5, 92, NA, NA))
  expect_equal(m2$offset[2], 2.5)
  expect_true(is.na(m2$end[3]))

  # Unknown first-half end: no shift (nominal 45)
  expect_equal(period_clock_map(c(NA, 92, NA, NA))$offset[2], 0)
  # A period-1 end before 45:00 (bad data) never shifts backwards
  expect_equal(period_clock_map(c(44, 90, NA, NA))$offset[2], 0)
})

test_that("shift_to_continuous_clock moves period >= 2 and clamps the restart", {
  m <- period_clock_map(c(48 - 1 / 60, 94, NA, NA))
  x <- shift_to_continuous_clock(c(30, 46.5, 45, 46.5), c(1, 1, 2, 2), m)
  expect_equal(x[1:2], c(30, 46.5))
  expect_equal(x[3], 48 - 1 / 60)            # restart clock == first-half end
  expect_equal(x[4], 46.5 + 3 - 1 / 60)
  expect_equal(shift_to_continuous_clock(5, NA, m), 5)
})

# Synthetic match: 3 minutes of first-half stoppage (period 1 ends 48:00),
# a Southampton-style half-time sub (HSTART11 -> HBENCH1 at period 2, 45:00),
# full time at period-2 clock 94:00 (4 minutes of second-half stoppage).
.r009_match <- function() {
  fmt_xi <- function(starters, bench) {
    sprintf('{"30":"%s","131":"%s"}',
            paste(c(starters, bench), collapse = ", "),
            paste(c(1:11, rep(0, length(bench))), collapse = ", "))
  }
  hs <- paste0("HSTART", 1:11); as_ <- paste0("ASTART", 1:11)
  raw <- data.frame(
    match_id = "R009",
    type_id = c(34L, 34L, 18L, 19L, 30L, 30L),
    period_id = c(1L, 1L, 2L, 2L, 1L, 2L),
    team_id = c("HOME", "AWAY", "HOME", "HOME", NA, NA),
    player_id = c("", "", "HSTART11", "HBENCH1", "", ""),
    minute = c(0, 0, 45, 45, 48, 94),
    second = c(0, 0, 0, 0, 0, 0),
    qualifier_json = c(fmt_xi(hs, "HBENCH1"), fmt_xi(as_, "ABENCH1"),
                       NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  timing <- extract_player_timing_from_events(raw)
  nm <- c(hs, as_, "HBENCH1")
  tm <- timing[match(nm, timing$player_id), ]
  lineups <- data.frame(
    match_id = "R009", player_id = nm, player_name = nm,
    team = ifelse(grepl("^H", nm), "H", "A"),
    is_home = grepl("^H", nm),
    is_starter = nm != "HBENCH1", minutes = 90,
    on_minute = tm$on_minute, off_minute = tm$off_minute,
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    match_id = "R009", minute = 45, added_time = 0, event_type = "substitution",
    is_goal = FALSE, is_sub = TRUE, is_red_card = FALSE, is_home = TRUE,
    player_id = "HBENCH1", player_name = "HBENCH1", period_id = 2L,
    stringsAsFactors = FALSE
  )
  shooting <- data.frame(
    match_id = "R009", minute = c(46.5, 46.5), team = c("H", "A"),
    player_id = c("HSTART9", "ASTART9"), player_name = c("HSTART9", "ASTART9"),
    xg = c(0.1, 0.3), is_goal = FALSE, is_penalty = FALSE,
    period_id = c(1L, 2L), stringsAsFactors = FALSE
  )
  results <- data.frame(
    match_id = "R009", home_team = "H", away_team = "A",
    first_half_end_time = 48, match_end_time = 94,
    stringsAsFactors = FALSE
  )
  list(raw = raw, timing = timing, lineups = lineups, events = events,
       shooting = shooting, results = results)
}

test_that("player timing is on the continuous timeline", {
  m <- .r009_match()
  t <- m$timing
  sub_on <- t[t$player_id == "HBENCH1", ]
  sub_off <- t[t$player_id == "HSTART11", ]
  expect_equal(sub_on$on_minute, 48)      # period 2 restarts at the first-half end
  expect_equal(sub_off$off_minute, 48)
  # Finisher plays to the continuous full time: 94:00 + 3 min stoppage shift
  expect_equal(t$off_minute[t$player_id == "HSTART1"], 97)
})

test_that("shots at the same clock in period 1 and period 2 land in different splints", {
  m <- .r009_match()
  res <- create_match_splints_fast(
    "R009", m$events, m$lineups, m$shooting, m$results, min_splint_duration = 5)
  sp <- res$splints

  # Total splint time == real elapsed time: 48 min of period 1 + 49 of period 2
  expect_equal(sum(sp$duration), 97)
  expect_equal(max(sp$end_minute), 97)
  expect_true(48 %in% sp$start_minute)

  s1 <- sp$splint_num[sp$npxg_home > 0]   # period-1 shot, home xG 0.1
  s2 <- sp$splint_num[sp$npxg_away > 0]   # period-2 shot, away xG 0.3
  expect_length(s1, 1); expect_length(s2, 1)
  expect_false(s1 == s2)
  expect_equal(sp$npxg_home[s1], 0.1)
  expect_equal(sp$npxg_away[s2], 0.3)

  # Different lineups: the period-1 shot is played by the pre-sub XI, the
  # period-2 shot by the post-sub XI.
  pl <- res$players
  on_pitch <- function(sn) pl$player_id[pl$splint_num == sn & pl$share > 0.999]
  expect_true("HSTART11" %in% on_pitch(s1))
  expect_false("HBENCH1" %in% on_pitch(s1))
  expect_true("HBENCH1" %in% on_pitch(s2))
  expect_false("HSTART11" %in% on_pitch(s2))
})

test_that("splints record the per-period clock shift for EPV/WPA stream cuts", {
  m <- .r009_match()
  res <- create_match_splints_fast(
    "R009", m$events, m$lineups, m$shooting, m$results, min_splint_duration = 5)
  expect_equal(unique(res$splints$period_shift_2), 3)
  expect_equal(unique(res$splints$period_shift_3), 7)  # period 2 ends at 97, ET restarts at 90
})

test_that("add_value_metrics_to_splints cuts raw-clock streams on the continuous timeline", {
  m <- .r009_match()
  res <- create_match_splints_fast(
    "R009", m$events, m$lineups, m$shooting, m$results, min_splint_duration = 5)
  sp <- res$splints
  sp$splint_id <- paste0("R009_", sp$splint_num)
  pl <- res$players
  pl$splint_id <- paste0("R009_", pl$splint_num)
  sd <- list(splints = sp, players = pl)

  # Two WPA actions at the same raw clock (46:30 = 2790 s), one per period.
  wpa <- data.frame(
    match_id = "R009", period_id = c(1L, 2L), time_seconds = c(2790, 2790),
    wp_delta_home = c(0.10, 0.20)
  )
  out <- add_value_metrics_to_splints(sd, match_action_wpa = wpa)
  s <- out$splints
  expect_equal(sum(s$wpa_home), 0.30)
  expect_equal(sum(s$wpa_home[s$end_minute <= 48 + 1e-9]), 0.10)
  expect_equal(sum(s$wpa_home[s$start_minute >= 48 - 1e-9]), 0.20)
})

test_that("legacy period-less events and shots keep the raw clock", {
  m <- .r009_match()
  ev <- m$events; ev$period_id <- NULL
  sh <- m$shooting; sh$period_id <- NULL
  shift <- .apply_continuous_timeline(ev, sh, m$results)
  expect_false(shift$active)
  expect_equal(shift$shooting$minute, sh$minute)
})

test_that("assign_event_period_id looks the period up in raw events and falls back on the clock", {
  raw <- data.frame(
    match_id = "M", type_id = c(19L, 19L, 16L, 30L),
    period_id = c(1L, 2L, 2L, 1L), minute = c(46L, 45L, 94L, 48L),
    second = c(10L, 0L, 1L, 0L), player_id = c("P1", "P2", "P3", ""),
    stringsAsFactors = FALSE
  )
  ev <- data.frame(
    match_id = "M",
    event_type = c("substitution", "substitution", "goal", "goal", "goal"),
    minute = c(46L, 45L, 94L, 12L, 70L), second = c(10L, 0L, 1L, 0L, 0L),
    player_id = c(NA, NA, "P3", "X", "Y"),
    player_on_id = c("P1", "P2", NA, NA, NA),
    stringsAsFactors = FALSE
  )
  out <- assign_event_period_id(ev, raw)
  # 46:10 sub is a first-half-stoppage sub (period 1); 45:00 sub is period 2.
  expect_equal(out$period_id, c(1L, 2L, 2L, 1L, 2L))
  # No raw events: returned unchanged, no period_id
  expect_false("period_id" %in% names(assign_event_period_id(ev, NULL)))
})
