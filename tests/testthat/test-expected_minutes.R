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
