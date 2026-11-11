# Tests for the WPA net ledger (panna#273). Expected numbers are computed by hand
# in the comments, never from the function under test.
#
# Fixture: one match, H (home) 1-0 A. Four actions, WPA in the ACTING team's frame,
# not centred:
#   action 1  h1  H  pass  wp 0.54  wpa +0.10
#   action 2  a1  A  pass  wp 0.40  wpa +0.04   (home frame -0.04)
#   action 3  h2  H  pass  wp 0.62  wpa +0.10
#   action 4  h3  H  shot  wp 0.70  wpa +0.30   (the goal)
# Home-frame total 0.10 - 0.04 + 0.10 + 0.30 = 0.46 = result 1 - opening 0.54.
# Targets with the even start: H 1 - 0.5 = +0.5, A 0 - 0.5 = -0.5.
# Start term: H opens at 0.54, so +0.04 spread over its 11 players (90 min each):
# 0.04 / 11 each; A opens at 0.46, so -0.04 / 11 each.

wpa_fx_actions <- function() {
  data.frame(
    match_id = "m1", action_id = 1:4,
    team_id = c("H", "A", "H", "H"), player_id = c("h1", "a1", "h2", "h3"),
    action_type = c("pass", "pass", "pass", "shot"),
    result = rep("success", 4), period_id = 1L, time_seconds = c(10, 20, 30, 40),
    stringsAsFactors = FALSE)
}
wpa_fx_wp <- function() {
  data.frame(match_id = "m1", action_id = 1:4, team_id = c("H", "A", "H", "H"),
             wp = c(0.54, 0.40, 0.62, 0.70), wpa = c(0.10, 0.04, 0.10, 0.30),
             stringsAsFactors = FALSE)
}
wpa_fx_fixtures <- function() {
  data.frame(match_id = "m1", home_team_id = "H", away_team_id = "A",
             home_score = 1, away_score = 0, stringsAsFactors = FALSE)
}
wpa_fx_lineup <- function() {
  data.frame(
    match_id = "m1", player_id = c(paste0("h", 1:11), paste0("a", 1:11)),
    team_id = rep(c("H", "A"), each = 11),
    player_name = c(paste0("Home ", 1:11), paste0("Away ", 1:11)),
    match_date = "2026-01-01", is_starter = TRUE, minutes_played = 90,
    sub_on_minute = 0, sub_off_minute = 0, competition = "ENG", season = "2025-2026",
    stringsAsFactors = FALSE)
}

test_that("each team sums to its result minus 0.5, and the start term is spread by minutes", {
  pg <- wpa_build_ledger(wpa_fx_actions(), wpa_fx_wp(), wpa_fx_fixtures(), wpa_fx_lineup(),
                         verbose = FALSE)
  tot <- pg[, .(s = sum(wpa_neutral)), by = team_id]
  expect_equal(tot[team_id == "H", s], 0.5, tolerance = 1e-12)
  expect_equal(tot[team_id == "A", s], -0.5, tolerance = 1e-12)
  expect_equal(pg[team_id == "H", unique(round(wpa_start, 12))], round(0.04 / 11, 12))
  expect_equal(pg[team_id == "A", unique(round(wpa_start, 12))], round(-0.04 / 11, 12))
  # The parts add up to the ledger value on every row.
  expect_equal(pg$wpa_offence + pg$wpa_defence + pg$wpa_start, pg$wpa_neutral, tolerance = 1e-12)
  # torp's channels add up too; the start sits in `shared`, and `won` is only the named role.
  expect_equal(pg$wpa_own + pg$wpa_won + pg$wpa_shared, pg$wpa_neutral, tolerance = 1e-12)
  expect_equal(pg$wpa_won, if ("wpa_defender" %in% names(pg)) pg$wpa_defender else rep(0, nrow(pg)),
               tolerance = 1e-12)
  pools <- intersect(c("wpa_pool_off_spread", "wpa_pool_def_spread"), names(pg))
  expect_equal(pg$wpa_shared, rowSums(as.matrix(pg[, pools, with = FALSE])) + pg$wpa_start, tolerance = 1e-12)
  tg <- attr(pg, "targets")
  expect_equal(sort(tg$target), c(-0.5, 0.5))
  expect_equal(nrow(pg), 22L)
})

test_that("a draw sums to 0 for both teams, and start = 0.6 moves both targets", {
  fx <- wpa_fx_fixtures(); fx$away_score <- 1
  w <- wpa_fx_wp()
  # For a 1-1 the home total must be 0.5 - 0.54 = -0.04: make the goal row +0.30 -> -0.20.
  w$wpa[4] <- -0.20
  pg <- wpa_build_ledger(wpa_fx_actions(), w, fx, wpa_fx_lineup(), verbose = FALSE)
  expect_equal(pg[, sum(wpa_neutral), by = team_id]$V1, c(0, 0), tolerance = 1e-12)
  pg6 <- wpa_build_ledger(wpa_fx_actions(), w, fx, wpa_fx_lineup(), start = 0.6, verbose = FALSE)
  expect_equal(sort(pg6[, sum(wpa_neutral), by = team_id]$V1), c(-0.1, -0.1), tolerance = 1e-12)
})

test_that("centred WPA is refused rather than spread as a start term", {
  w <- wpa_fx_wp(); w$wpa <- w$wpa - mean(w$wpa)
  expect_error(wpa_build_ledger(wpa_fx_actions(), w, wpa_fx_fixtures(), wpa_fx_lineup(), verbose = FALSE),
               "centre = FALSE")
})

test_that("an action with no WPA is refused", {
  w <- wpa_fx_wp()[1:3, ]
  expect_error(wpa_build_ledger(wpa_fx_actions(), w, wpa_fx_fixtures(), wpa_fx_lineup(), verbose = FALSE),
               "no WPA")
})

test_that("xGOT and an old epv_delta on the actions change nothing (goals-unit rules stay off)", {
  base <- wpa_build_ledger(wpa_fx_actions(), wpa_fx_wp(), wpa_fx_fixtures(), wpa_fx_lineup(), verbose = FALSE)
  a <- wpa_fx_actions(); a$xgot <- c(NA, NA, NA, 0.6); a$epv <- c(0.1, 0.2, 0.3, 0.4); a$epv_delta <- 9
  alt <- wpa_build_ledger(a, wpa_fx_wp(), wpa_fx_fixtures(), wpa_fx_lineup(), verbose = FALSE)
  data.table::setorder(base, player_id); data.table::setorder(alt, player_id)
  expect_equal(alt$wpa_neutral, base$wpa_neutral, tolerance = 1e-12)
})

test_that("add_wp_vars(centre = FALSE) keeps the raw change; the default still centres", {
  d <- data.table::data.table(match_id = "m1", team_id = c("H", "A", "H"), is_home = c(1L, 0L, 1L),
                              wp_label = c(1, 0, 1), fake_wp = c(0.54, 0.40, 0.70))
  testthat::local_mocked_bindings(predict_wp = function(wp_model, dt, ...) dt$fake_wp)
  raw <- add_wp_vars(data.table::copy(d), wp_model = NULL, centre = FALSE)
  # 1 -> 2 flips possession: (1 - 0.40) - 0.54 = 0.06; 2 -> 3 flips: (1 - 0.70) - 0.40 = -0.10;
  # last row falls back to its label, same team: 1 - 0.70 = 0.30.
  expect_equal(raw$wpa, c(0.06, -0.10, 0.30), tolerance = 1e-12)
  cen <- add_wp_vars(data.table::copy(d), wp_model = NULL)
  expect_equal(sum(cen$wpa), 0, tolerance = 1e-12)
})
