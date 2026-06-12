test_that("name variants for the same player_id are merged, not split", {
  # Opta mixes name variants for one id across seasons ("L. Martínez" vs
  # "Lautaro Martínez"). build_team_expected_minutes() must canonicalise the
  # name per player_id BEFORE aggregating, otherwise the player's appearance
  # evidence splits into separate rows that are each shrunk toward zero.
  lineups <- data.table::data.table(
    team_name      = "Testland",
    match_id       = c("m1", "m2"),
    match_date     = c("2026-01-01", "2026-03-01"),
    player_id      = "p1",
    player_name    = c("L. Martinez", "Lautaro Martinez"),
    position       = "Striker",
    is_starter     = TRUE,
    minutes_played = 90L,
    sub_on_minute  = NA_real_,
    sub_off_minute = NA_real_,
    competition    = "Intl_Friendlies"
  )

  em <- build_team_expected_minutes("Testland", lineups,
                                    as_of = as.Date("2026-04-01"))

  p1 <- em[em$player_id == "p1", , drop = FALSE]
  expect_equal(nrow(p1), 1L)
  # Most recent variant wins
  expect_equal(p1$player_name, "Lautaro Martinez")
  # Evidence combined across both variants, not split
  expect_equal(p1$n_appear, 2L)
  expect_gt(p1$n_caps_weighted, 1.5)
  # Shrinkage applied to the COMBINED weight: with weight_total ~ 1.8 and
  # shrinkage_k = 3, EM ~ 90 * 1.8 / 4.8 ~ 33. A split row would cap at
  # ~90 * 0.95 / 3.95 ~ 21.7 — assert we are above what any single
  # variant's evidence could produce.
  expect_gt(p1$expected_minutes, 25)
})

# Shared fixture: pA starts every game, pB is an unused sub every game,
# pC is a one-cap wonder who started only the most recent game.
.em_fixture <- function() {
  one_match <- function(mid, date, rows) {
    data.table::data.table(
      team_name      = "Testland",
      match_id       = mid,
      match_date     = date,
      player_id      = rows$id,
      player_name    = rows$id,
      position       = rows$pos,
      is_starter     = rows$starter,
      minutes_played = rows$mins,
      sub_on_minute  = NA_real_,
      sub_off_minute = NA_real_,
      competition    = rows$comp
    )
  }
  m1 <- one_match("m1", "2026-02-01", data.frame(
    id = c("pA", "pB"), pos = c("Striker", "Substitute"),
    starter = c(TRUE, FALSE), mins = c(90L, 0L),
    comp = "Intl_Friendlies"))
  m2 <- one_match("m2", "2026-03-01", data.frame(
    id = c("pA", "pB", "pC"), pos = c("Striker", "Substitute", "Defender"),
    starter = c(TRUE, FALSE, TRUE), mins = c(90L, 0L, 90L),
    comp = "Intl_Friendlies"))
  data.table::rbindlist(list(m1, m2))
}

test_that("Beta prior stops one-cap players getting p_start = 1.00", {
  lineups <- .em_fixture()
  em <- build_team_expected_minutes("Testland", lineups,
                                    as_of = as.Date("2026-04-01"))
  pC <- em[em$player_id == "pC", , drop = FALSE]
  expect_lt(pC$p_start, 0.85)
  expect_gt(pC$p_start, 0.4)

  # Legacy behaviour recoverable with prob_prior_k = 0
  em0 <- build_team_expected_minutes("Testland", lineups,
                                     as_of = as.Date("2026-04-01"),
                                     prob_prior_k = 0)
  pC0 <- em0[em0$player_id == "pC", , drop = FALSE]
  expect_equal(pC0$p_start, 1)
  # and the no-bench-evidence 0/0 guard holds
  expect_equal(pC0$p_sub_given_bench, 0)
})

test_that("Beta prior damps the one-omission collapse", {
  lineups <- .em_fixture()
  # m3: pC is in the squad but an unused sub
  m3 <- data.table::data.table(
    team_name = "Testland", match_id = "m3", match_date = "2026-03-20",
    player_id = c("pA", "pB", "pC"), player_name = c("pA", "pB", "pC"),
    position = c("Striker", "Substitute", "Substitute"),
    is_starter = c(TRUE, FALSE, FALSE), minutes_played = c(90L, 0L, 0L),
    sub_on_minute = NA_real_, sub_off_minute = NA_real_,
    competition = "Intl_Friendlies")
  with_m3 <- data.table::rbindlist(list(lineups, m3))

  p_start_of <- function(lu, k) {
    em <- build_team_expected_minutes("Testland", lu,
                                      as_of = as.Date("2026-04-01"),
                                      prob_prior_k = k)
    em[em$player_id == "pC", "p_start"]
  }
  drop_prior  <- p_start_of(lineups, 3) - p_start_of(with_m3, 3)
  drop_legacy <- p_start_of(lineups, 0) - p_start_of(with_m3, 0)
  # Legacy: 1.00 -> ~0.5. Prior: ~0.7 -> ~0.56. Both drop, prior much less.
  expect_gt(drop_legacy, 0.4)
  expect_lt(drop_prior, drop_legacy / 2)
})

test_that("tournament_boost upweights in-tournament matches only", {
  lineups <- .em_fixture()
  # Same-day pair: pT starts a World Cup game, pF starts a friendly
  extra <- data.table::data.table(
    team_name = "Testland", match_id = c("wc1", "fr1"),
    match_date = "2026-03-15",
    player_id = c("pT", "pF"), player_name = c("pT", "pF"),
    position = "Defender", is_starter = TRUE, minutes_played = 90L,
    sub_on_minute = NA_real_, sub_off_minute = NA_real_,
    competition = c("World_Cup", "Intl_Friendlies"))
  lu <- data.table::rbindlist(list(lineups, extra))

  caps_of <- function(...) {
    em <- build_team_expected_minutes("Testland", lu,
                                      as_of = as.Date("2026-04-01"), ...)
    c(T = em[em$player_id == "pT", "n_caps_weighted"],
      F = em[em$player_id == "pF", "n_caps_weighted"])
  }
  off <- caps_of()
  expect_equal(off[["T"]], off[["F"]])
  on <- caps_of(tournament_boost = 4,
                tournament_start = as.Date("2026-03-01"))
  expect_equal(on[["T"]], 4 * on[["F"]], tolerance = 0.05)
  expect_equal(on[["F"]], off[["F"]])
})
