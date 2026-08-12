test_that("classify_role_group maps 16-role codes into 6 role groups correctly", {
  roles <- c("GK", "CB", "LB", "RB", "LWB", "RWB", "DM", "CM", "CAM", "LM", "RM", "LW", "RW", "CF", "LF", "RF", "UNK", NA)
  groups <- classify_role_group(roles)
  expect_equal(groups, c("GK", "CB", "FBWB", "FBWB", "FBWB", "FBWB", "DMCM", "DMCM", "AMWIDE", "AMWIDE", "AMWIDE", "AMWIDE", "AMWIDE", "CF", "CF", "CF", NA, NA))
})

test_that("calculate_spm_value scores mock match stats correctly", {
  mock_coefs <- list(
    offense = data.table::data.table(
      target = "offense",
      feature = c("(Intercept)", "goals_p90", "dev__CF__touches_p90"),
      role_group = c(NA, NA, "CF"),
      base_feature = c(NA, "goals_p90", "touches_p90"),
      is_deviation = c(FALSE, FALSE, TRUE),
      coef = c(0.1, 0.5, 0.01)
    ),
    defense = data.table::data.table(
      target = "defense",
      feature = c("(Intercept)", "tackles_won_p90"),
      role_group = c(NA, NA),
      base_feature = c(NA, "tackles_won_p90"),
      is_deviation = c(FALSE, FALSE),
      coef = c(-0.05, 0.2)
    )
  )

  match_df <- data.table::data.table(
    position_role = c("CF", "CB"),
    goals_p90 = c(1.0, 0.0),
    touches_p90 = c(50.0, 40.0),
    tackles_won_p90 = c(1.0, 3.0)
  )

  res <- calculate_spm_value(match_df, coefs = mock_coefs)
  expect_equal(nrow(res), 2)
  expect_named(res, c("spm_value_off", "spm_value_def", "spm_value"))

  # Row 1 (CF): off = 0.1 + 0.5*1.0 + 0.01*50 = 1.1; def = -0.05 + 0.2*1.0 = 0.15; net = 1.25
  expect_equal(res$spm_value_off[1], 1.1)
  expect_equal(res$spm_value_def[1], 0.15)
  expect_equal(res$spm_value[1], 1.25)

  # Row 2 (CB): off = 0.1 + 0; def = -0.05 + 0.2*3.0 = 0.55; net = 0.65
  expect_equal(res$spm_value_off[2], 0.1)
  expect_equal(res$spm_value_def[2], 0.55)
  expect_equal(res$spm_value[2], 0.65)
})

test_that("calculate_value_context_gap computes psv - spm_value correctly", {
  psv <- c(1.5, 0.8, -0.2)
  spm_val <- c(1.0, 1.0, -0.1)
  gap <- calculate_value_context_gap(psv, spm_val)
  expect_equal(gap, c(0.5, -0.2, -0.1))
})
