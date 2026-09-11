test_that(".assert_career_panna_sign_convention aborts on a missing sign_convention column", {
  cp <- data.frame(player_id = "p1", panna_defense = 0.1)
  expect_error(
    .assert_career_panna_sign_convention(cp, "test caller"),
    "no.*sign_convention.*column"
  )
})

test_that(".assert_career_panna_sign_convention aborts on a mismatched tag", {
  cp <- data.frame(player_id = "p1", panna_defense = 0.1,
                    sign_convention = "defense_negative_good")
  expect_error(
    .assert_career_panna_sign_convention(cp, "test caller"),
    "tagged.*expected"
  )
})

test_that(".assert_career_panna_sign_convention passes on a correctly-tagged file", {
  cp <- data.frame(player_id = "p1", panna_defense = 0.1,
                    sign_convention = CAREER_PANNA_SIGN_CONVENTION)
  expect_true(.assert_career_panna_sign_convention(cp, "test caller"))
})
