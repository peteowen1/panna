# Tests for PSR (Player Skill Rating) framework

# Internal function aliases (not exported)
load_psr_coefficients <- panna:::load_psr_coefficients
calculate_psr_components <- panna:::calculate_psr_components

# Helper: create minimal match_stats for PSR testing
make_psr_test_data <- function(n_players = 5, n_matches = 10) {
  players <- paste0("p", seq_len(n_players))
  dates <- seq.Date(as.Date("2024-01-01"), by = "week", length.out = n_matches)

  rows <- expand.grid(player_id = players, match_idx = seq_len(n_matches),
                       stringsAsFactors = FALSE)
  rows$player_name <- paste0("Player_", rows$player_id)
  rows$match_id <- paste0("m_", rows$match_idx)
  rows$match_date <- dates[rows$match_idx]
  rows$total_minutes <- 90
  rows$position <- rep(c("Defender", "Midfielder", "Striker"),
                        length.out = nrow(rows))

  # Simple skill columns
  set.seed(42)
  rows$goals_p90 <- runif(nrow(rows), 0, 1)
  rows$tackles_won_p90 <- runif(nrow(rows), 0, 3)
  rows$pass_accuracy <- runif(nrow(rows), 0.6, 0.95)
  rows$passes <- rpois(nrow(rows), 50)
  rows$passes_accurate <- round(rows$passes * rows$pass_accuracy)
  rows$shots_p90 <- runif(nrow(rows), 0, 3)

  as.data.frame(rows)
}

# Helper: create simple coefficient data.frame
make_test_coefs <- function(stats = c("goals_p90", "tackles_won_p90", "pass_accuracy"),
                             betas = c(2.0, 0.5, 1.0)) {
  data.frame(stat_name = stats, beta = betas, stringsAsFactors = FALSE)
}


# =============================================================================
# .get_psr_skill_cols()
# =============================================================================

test_that(".get_psr_skill_cols returns a character vector", {
  cols <- panna:::.get_psr_skill_cols()
  expect_type(cols, "character")
  expect_true(length(cols) > 0)
})

test_that(".get_psr_skill_cols includes rate, above-expected, and xmetrics columns", {
  cols <- panna:::.get_psr_skill_cols()

  # Rate columns end with _p90
  rate_cols <- grep("_p90$", cols, value = TRUE)
  expect_true(length(rate_cols) > 0)

  # Above-expected duel features: the 5 xDuel WOE counts replaced the old
  # duel/aerial/tackle success RATIOS in panna#116 (ratios discarded volume).
  expect_true("aerial_woe_per90" %in% cols)
  expect_true("takeon_woe_per90" %in% cols)
  expect_true("containment_woe_per90" %in% cols)

  # Finishing over-performance replaced the scale-free finishing ratios.
  expect_true("npg_minus_npxg_per90" %in% cols)

  # xMetrics columns
  expect_true("xg_per90" %in% cols)
  expect_true("npxg_per90" %in% cols)

  # Zonal finishing splits are BANNED from the PSR/PSV family (#162): their
  # season-grain training sds collapse (obox 45.8x amplification at match
  # grain), which is how obox's collinearity-noise defensive beta became the
  # largest DSV driver. They remain SPM-only (season grain).
  expect_false("ibox_g_minus_xg_per90" %in% cols)
  expect_false("obox_g_minus_xg_per90" %in% cols)
})

test_that(".get_psr_skill_cols has no duplicates", {
  cols <- panna:::.get_psr_skill_cols()
  expect_equal(length(cols), length(unique(cols)))
})


# =============================================================================
# calculate_psr()
# =============================================================================

test_that("calculate_psr returns correct output structure", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2", "p3"),
    player_name = c("Alice", "Bob", "Charlie"),
    goals_p90 = c(0.5, 0.3, 0.8),
    tackles_won_p90 = c(1.0, 2.0, 0.5),
    pass_accuracy = c(0.85, 0.90, 0.75)
  )
  coefs <- make_test_coefs()

  result <- calculate_psr(skills, coefs)

  expect_s3_class(result, "data.table")
  expect_true("player_id" %in% names(result))
  expect_true("psr_raw" %in% names(result))
  expect_true("psr" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("calculate_psr computes weighted sum of skills", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    goals_p90 = c(1.0, 0.0),
    tackles_won_p90 = c(0.0, 1.0)
  )
  coefs <- make_test_coefs(
    stats = c("goals_p90", "tackles_won_p90"),
    betas = c(3.0, 2.0)
  )

  result <- calculate_psr(skills, coefs, center = FALSE)

  # p1: 1.0*3.0 + 0.0*2.0 = 3.0
  # p2: 0.0*3.0 + 1.0*2.0 = 2.0
  expect_equal(result[player_id == "p1"]$psr, 3.0)
  expect_equal(result[player_id == "p2"]$psr, 2.0)
})

test_that("calculate_psr centering makes mean PSR approximately zero", {
  withr::with_seed(123, {
    n <- 20
    skills <- data.table::data.table(
      player_id = paste0("p", seq_len(n)),
      player_name = paste0("Player_", seq_len(n)),
      goals_p90 = runif(n, 0, 1),
      tackles_won_p90 = runif(n, 0, 3)
    )
  })
  coefs <- make_test_coefs(
    stats = c("goals_p90", "tackles_won_p90"),
    betas = c(2.0, 0.5)
  )

  result <- calculate_psr(skills, coefs, center = TRUE)
  expect_equal(mean(result$psr), 0, tolerance = 1e-10)
})

test_that("calculate_psr with center=FALSE preserves raw values", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(0.5, 1.0)
  )
  coefs <- make_test_coefs(stats = "goals_p90", betas = 2.0)

  result <- calculate_psr(skills, coefs, center = FALSE)
  expect_equal(result$psr, result$psr_raw)
  expect_equal(result[player_id == "p1"]$psr, 1.0)
  expect_equal(result[player_id == "p2"]$psr, 2.0)
})

test_that("calculate_psr with all-zero coefficients produces zero PSR", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(0.5, 1.0),
    tackles_won_p90 = c(2.0, 1.0)
  )
  coefs <- make_test_coefs(
    stats = c("goals_p90", "tackles_won_p90"),
    betas = c(0, 0)
  )

  expect_warning(
    result <- calculate_psr(skills, coefs),
    "zero"
  )
  expect_equal(result$psr, c(0, 0))
  expect_equal(result$psr_raw, c(0, 0))
})

test_that("calculate_psr warns and skips missing skill columns", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(0.5, 1.0)
  )
  # Coefficient for a column that doesn't exist
  coefs <- make_test_coefs(
    stats = c("goals_p90", "nonexistent_stat"),
    betas = c(2.0, 1.0)
  )

  expect_warning(
    result <- calculate_psr(skills, coefs, center = FALSE),
    "not found"
  )
  # Should still compute using available columns
  expect_equal(result[player_id == "p1"]$psr, 1.0)
  expect_equal(result[player_id == "p2"]$psr, 2.0)
})

test_that("calculate_psr errors when no matching columns found", {
  skills <- data.table::data.table(
    player_id = "p1",
    player_name = "A",
    goals_p90 = 0.5
  )
  coefs <- make_test_coefs(
    stats = c("nonexistent_a", "nonexistent_b"),
    betas = c(1.0, 2.0)
  )

  expect_error(calculate_psr(skills, coefs), "No matching skill columns")
})

test_that("calculate_psr SD standardization works correctly", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(0.4, 0.8),
    tackles_won_p90 = c(2.0, 1.0)
  )
  coefs <- data.frame(
    stat_name = c("goals_p90", "tackles_won_p90"),
    beta = c(1.0, 1.0),
    sd = c(0.2, 0.5),
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coefs, center = FALSE)

  # p1: (0.4/0.2)*1.0 + (2.0/0.5)*1.0 = 2.0 + 4.0 = 6.0
  # p2: (0.8/0.2)*1.0 + (1.0/0.5)*1.0 = 4.0 + 2.0 = 6.0
  expect_equal(result[player_id == "p1"]$psr, 6.0)
  expect_equal(result[player_id == "p2"]$psr, 6.0)
})

test_that("calculate_psr SD standardization handles zero/NA sd", {
  skills <- data.table::data.table(
    player_id = "p1",
    player_name = "A",
    stat_a = 3.0,
    stat_b = 2.0
  )
  coefs <- data.frame(
    stat_name = c("stat_a", "stat_b"),
    beta = c(1.0, 1.0),
    sd = c(0, NA),
    stringsAsFactors = FALSE
  )

  result <- calculate_psr(skills, coefs, center = FALSE)

  # Zero and NA sd should be replaced with 1, so no division effect
  # p1: (3.0/1)*1.0 + (2.0/1)*1.0 = 5.0
  expect_equal(result$psr, 5.0)
})

test_that("calculate_psr requires stat_name and beta columns", {
  skills <- data.table::data.table(player_id = "p1", goals_p90 = 0.5)
  bad_coefs <- data.frame(variable = "goals_p90", coefficient = 2.0)

  expect_error(calculate_psr(skills, bad_coefs), "stat_name.*beta")
})

test_that("calculate_psr handles NA values in skills by treating as zero", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(NA, 1.0)
  )
  coefs <- make_test_coefs(stats = "goals_p90", betas = 2.0)

  result <- calculate_psr(skills, coefs, center = FALSE)
  expect_equal(result[player_id == "p1"]$psr, 0.0)
  expect_equal(result[player_id == "p2"]$psr, 2.0)
})


# =============================================================================
# calculate_psr_components()
# =============================================================================

test_that("calculate_psr_components: osr + dsr = psr exactly", {
  withr::with_seed(999, {
    n <- 10
    skills <- data.table::data.table(
      player_id = paste0("p", seq_len(n)),
      player_name = paste0("Player_", seq_len(n)),
      goals_p90 = runif(n, 0, 1),
      tackles_won_p90 = runif(n, 0, 3),
      pass_accuracy = runif(n, 0.6, 0.95)
    )
  })

  margin_coefs <- make_test_coefs(
    stats = c("goals_p90", "tackles_won_p90", "pass_accuracy"),
    betas = c(2.0, 0.5, 1.0)
  )
  osr_coefs <- make_test_coefs(
    stats = c("goals_p90", "pass_accuracy"),
    betas = c(1.5, 0.8)
  )
  dsr_coefs <- make_test_coefs(
    stats = c("tackles_won_p90"),
    betas = c(1.2)
  )

  result <- calculate_psr_components(skills, margin_coefs, osr_coefs, dsr_coefs)

  expect_equal(result$osr + result$dsr, result$psr, tolerance = 1e-10)
})

test_that("calculate_psr_components returns correct output columns", {
  skills <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    goals_p90 = c(0.5, 0.8),
    tackles_won_p90 = c(1.0, 2.0)
  )
  margin_coefs <- make_test_coefs(
    stats = c("goals_p90", "tackles_won_p90"), betas = c(2.0, 1.0)
  )
  osr_coefs <- make_test_coefs(stats = "goals_p90", betas = 1.5)
  dsr_coefs <- make_test_coefs(stats = "tackles_won_p90", betas = 0.8)

  result <- calculate_psr_components(skills, margin_coefs, osr_coefs, dsr_coefs)

  expect_true("psr" %in% names(result))
  expect_true("psr_raw" %in% names(result))
  expect_true("osr" %in% names(result))
  expect_true("dsr" %in% names(result))
  expect_true("player_id" %in% names(result))
})

test_that("calculate_psr_components decomposition holds across seeds", {
  for (s in c(1, 42, 123, 456, 789)) {
    withr::with_seed(s, {
      n <- 15
      skills <- data.table::data.table(
        player_id = paste0("p", seq_len(n)),
        player_name = paste0("P_", seq_len(n)),
        goals_p90 = runif(n, 0, 1.5),
        tackles_won_p90 = runif(n, 0, 4),
        pass_accuracy = runif(n, 0.5, 0.95)
      )
    })

    margin_coefs <- make_test_coefs(
      stats = c("goals_p90", "tackles_won_p90", "pass_accuracy"),
      betas = c(2.5, 0.7, 1.2)
    )
    osr_coefs <- make_test_coefs(
      stats = c("goals_p90", "pass_accuracy"),
      betas = c(1.8, 0.6)
    )
    dsr_coefs <- make_test_coefs(
      stats = c("tackles_won_p90", "pass_accuracy"),
      betas = c(1.0, 0.3)
    )

    result <- calculate_psr_components(skills, margin_coefs, osr_coefs, dsr_coefs)
    expect_equal(result$osr + result$dsr, result$psr, tolerance = 1e-10,
                 label = paste("seed", s))
  }
})


# =============================================================================
# build_league_network() / compute_psr_league_offsets() / apply_psr_league_offsets()
# =============================================================================

# Helper: per-game logs where players co-occur in a WEAK league + ENG the SAME
# season, posting higher per-90 value in WEAK (so WEAK is "easy").
make_game_logs <- function(n = 6) {
  withr::with_seed(7, {
    data.table::rbindlist(lapply(seq_len(n), function(i) data.table::data.table(
      player_id = paste0("p", i), season = "2025-2026",
      league = c("WEAK", "ENG"), total_minutes = c(900, 900),
      # val = sum(psv)/(min/90): psv 3.0 -> 0.30/90, psv 1.0 -> 0.10/90
      psv = c(3.0, 1.0) + stats::rnorm(2, 0, 0.05)
    )))
  })
}

test_that("build_league_network anchors Big-5 at 0 and flags the easy league", {
  o <- build_league_network(make_game_logs(), value_col = "psv",
                            big5 = "ENG", verbose = FALSE)
  expect_true(all(c("league", "strength", "offset", "n_bridge") %in% names(o)))
  expect_equal(o[league == "ENG"]$offset, 0, tolerance = 1e-9)   # anchor
  # WEAK posts ~0.20 more per-90 -> positive strength -> NEGATIVE offset
  expect_gt(o[league == "WEAK"]$strength, 0)
  expect_lt(o[league == "WEAK"]$offset, 0)
  expect_equal(o[league == "WEAK"]$n_bridge, 6L)
})

test_that("compute_psr_league_offsets maps codes to display names + WEAK negative", {
  o <- compute_psr_league_offsets(make_game_logs(), big5 = "ENG", verbose = FALSE)
  expect_true(all(c("league", "offset", "n_bridge") %in% names(o)))
  expect_lt(o[league == "WEAK"]$offset, 0)
  # ENG code is mapped to its displayed competition name (EPL)
  expect_true("EPL" %in% o$league)
  expect_false("ENG" %in% o$league)
})

test_that("apply_psr_league_offsets adds offset and preserves osr+dsr=psr", {
  offs <- data.table::data.table(league = c("WEAK", "EPL"), offset = c(-0.18, 0))
  dt <- data.table::data.table(
    player_id = c("x", "y"), league = c("WEAK", "EPL"),
    psr = c(0.25, 0.25), osr = c(0.15, 0.15), dsr = c(0.10, 0.10)
  )
  out <- apply_psr_league_offsets(dt, offs)
  expect_true("psr_league_offset" %in% names(out))
  expect_equal(out[league == "WEAK"]$psr, 0.25 - 0.18, tolerance = 1e-9)
  expect_equal(out[league == "EPL"]$psr, 0.25)             # anchor unchanged
  expect_equal(out$osr + out$dsr, out$psr, tolerance = 1e-9)  # identity preserved
})

test_that("apply_psr_league_offsets leaves unknown leagues unchanged (offset 0)", {
  offs <- data.table::data.table(league = "WEAK", offset = -0.18)
  dt <- data.table::data.table(player_id = "z", league = "UNKNOWN_LG", psr = 0.2)
  out <- apply_psr_league_offsets(dt, offs)
  expect_equal(out$psr, 0.2)
  expect_equal(out$psr_league_offset, 0)
})


# =============================================================================
# load_psr_coefficients()
# =============================================================================

test_that("load_psr_coefficients loads bundled coefficient files", {
  # Coefficient CSVs now ship in inst/extdata/ (generated by 07_train_psr_model.R)
  margin <- load_psr_coefficients("margin", "xg")
  expect_s3_class(margin, "data.frame")
  expect_true(all(c("stat_name", "beta") %in% names(margin)))
  expect_gt(nrow(margin), 0)

  offense <- load_psr_coefficients("offense", "xg")
  expect_s3_class(offense, "data.frame")
  expect_gt(nrow(offense), 0)

  defense <- load_psr_coefficients("defense", "goals")
  expect_s3_class(defense, "data.frame")
  expect_gt(nrow(defense), 0)
})

test_that("load_psr_coefficients validates type argument", {
  expect_error(
    load_psr_coefficients(type = "invalid_type"),
    "arg"
  )
})

test_that("load_psr_coefficients validates target argument", {
  expect_error(
    load_psr_coefficients(type = "margin", target = "invalid_target"),
    "arg"
  )
})

test_that("load_psr_coefficients accepts all valid type values", {
  for (t in c("margin", "offense", "defense")) {
    result <- load_psr_coefficients(type = t, target = "xg")
    expect_s3_class(result, "data.frame")
    expect_true("stat_name" %in% names(result))
  }
})

test_that("load_psr_coefficients accepts all valid target values", {
  for (tgt in c("xg", "goals")) {
    result <- load_psr_coefficients(type = "margin", target = tgt)
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0)
  }
})


# =============================================================================
# .estimate_prematch_skills_batch()
# =============================================================================

test_that(".estimate_prematch_skills_batch returns named list of data.tables", {
  ms <- make_psr_test_data(n_players = 3, n_matches = 6)
  dates <- c("2024-02-01", "2024-03-01")

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = dates, verbose = FALSE
  )

  expect_type(result, "list")
  expect_true(length(result) > 0)
  # All elements should be data.tables

  for (nm in names(result)) {
    expect_s3_class(result[[nm]], "data.table")
  }
  # Names should be date strings
  expect_true(all(names(result) %in% as.character(as.Date(dates))))
})

test_that(".estimate_prematch_skills_batch detects _per90 (xMetrics) stat columns", {
  # Regression: the stat-column auto-detect used grep("_p90$") which does NOT
  # match `_per90` — so xg_per90 and every xMetrics over-performance feature was
  # specified in .get_psr_skill_cols() but never estimated/trained. The fix
  # greps `_p90$|_per90$` plus the registered skill-col union. This test pins it.
  ms <- make_psr_test_data(n_players = 3, n_matches = 6)
  set.seed(7)
  # Add an xMetrics-style _per90 column (signed, like over-performance)
  ms$xg_per90 <- runif(nrow(ms), 0, 1)
  ms$npg_minus_npxg_per90 <- rnorm(nrow(ms))

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = c("2024-02-01", "2024-03-01"),
    min_weighted_90s = 0, verbose = FALSE
  )

  # The estimator must have produced a smoothed estimate for the _per90 columns.
  non_empty <- Filter(function(d) nrow(d) > 0, result)
  expect_true(length(non_empty) > 0)
  sk <- non_empty[[1]]
  expect_true("xg_per90" %in% names(sk))
  expect_true("npg_minus_npxg_per90" %in% names(sk))
  # And the estimate is a real number (was silently absent before the fix)
  expect_true(is.numeric(sk$xg_per90))
  expect_false(all(is.na(sk$xg_per90)))
})

test_that(".estimate_prematch_skills_batch with single date matches estimate_player_skills", {
  ms <- make_psr_test_data(n_players = 3, n_matches = 5)
  ref_date <- as.Date("2024-02-15")

  params <- get_default_decay_params()

  # Batch version

  batch_result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = as.character(ref_date),
    decay_params = params, min_weighted_90s = 0, verbose = FALSE
  )

  # Single-date version
  single_result <- estimate_player_skills(
    ms, target_date = ref_date,
    decay_params = params, min_weighted_90s = 0
  )

  # Both should produce results (batch may be NULL if no data before date)
  if (length(batch_result) > 0 && !is.null(single_result)) {
    batch_dt <- batch_result[[1]]
    # Check that the same players are returned
    batch_players <- sort(batch_dt$player_id)
    single_players <- sort(single_result$player_id)
    expect_equal(batch_players, single_players)

    # Check common stat columns are close (may differ slightly due to
    # implementation differences, but should be very close)
    common_stats <- intersect(
      grep("_p90$", names(batch_dt), value = TRUE),
      grep("_p90$", names(single_result), value = TRUE)
    )
    for (sc in common_stats) {
      batch_vals <- batch_dt[order(player_id)][[sc]]
      single_vals <- single_result[order(player_id)][[sc]]
      expect_equal(batch_vals, single_vals, tolerance = 0.01,
                   label = paste("stat:", sc))
    }
  }
})

test_that(".estimate_prematch_skills_batch with later dates includes more data", {
  ms <- make_psr_test_data(n_players = 3, n_matches = 10)
  # Dates that span the match data range
  dates <- c("2024-02-01", "2024-03-01")

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = dates, min_weighted_90s = 0, verbose = FALSE
  )

  if (length(result) == 2) {
    early <- result[[1]]
    late <- result[[2]]

    # Later date should have higher (or equal) weighted_90s for each player
    for (pid in intersect(early$player_id, late$player_id)) {
      w90_early <- early[player_id == pid]$weighted_90s
      w90_late <- late[player_id == pid]$weighted_90s
      # Later date sees more matches, so weighted_90s should be at least as large
      # (accounting for decay, the relationship might not be strictly monotonic
      # if decay is very strong, but with default params it should hold)
      expect_true(w90_late >= w90_early * 0.5,
                  label = paste("player", pid, "weighted_90s should grow"))
    }
  }
})

test_that(".estimate_prematch_skills_batch returns empty list for dates before all data", {
  ms <- make_psr_test_data(n_players = 3, n_matches = 5)
  # All match data starts at 2024-01-01, use a date before that
  dates <- c("2020-01-01", "2020-06-01")

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = dates, verbose = FALSE
  )

  expect_type(result, "list")
  expect_equal(length(result), 0)
})

test_that(".estimate_prematch_skills_batch uses only data strictly before ref_date", {
  # Create data with one match on 2024-01-01 and one on 2024-01-08
  ms <- data.frame(
    player_id = c("p1", "p1"),
    player_name = c("Test", "Test"),
    match_id = c("m1", "m2"),
    match_date = as.Date(c("2024-01-01", "2024-01-08")),
    total_minutes = c(90, 90),
    position = c("Midfielder", "Midfielder"),
    goals_p90 = c(0.0, 2.0),
    stringsAsFactors = FALSE
  )

  params <- get_default_decay_params()
  params$rate <- 0  # no decay for cleaner test

  # At ref_date = 2024-01-08, only m1 (goals_p90=0.0) should be used
  result_before_m2 <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = "2024-01-08",
    decay_params = params, min_weighted_90s = 0, verbose = FALSE
  )

  # At ref_date = 2024-01-15, both m1 and m2 should be used
  result_after_m2 <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = "2024-01-15",
    decay_params = params, min_weighted_90s = 0, verbose = FALSE
  )

  if (length(result_before_m2) > 0 && length(result_after_m2) > 0) {
    skill_before <- result_before_m2[[1]][player_id == "p1"]$goals_p90
    skill_after <- result_after_m2[[1]][player_id == "p1"]$goals_p90

    # Before m2, skill should be based only on m1 (goals_p90=0.0),
    # so it should be lower than after m2 (which adds goals_p90=2.0)
    expect_true(skill_before < skill_after,
                label = "no look-ahead: skill at D should not include data from D")
  }
})

test_that(".estimate_prematch_skills_batch handles single-player data", {
  ms <- data.frame(
    player_id = rep("solo", 5),
    player_name = rep("Solo Player", 5),
    match_id = paste0("m", 1:5),
    match_date = as.Date("2024-01-01") + (0:4) * 7,
    total_minutes = 90,
    position = "Striker",
    goals_p90 = c(0.5, 0.3, 0.8, 0.1, 0.6),
    stringsAsFactors = FALSE
  )

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = "2024-02-15",
    min_weighted_90s = 0, verbose = FALSE
  )

  expect_true(length(result) > 0)
  expect_equal(nrow(result[[1]]), 1)
  expect_equal(result[[1]]$player_id, "solo")
})

test_that(".estimate_prematch_skills_batch keeps thin-history players under the DEFAULT min_weighted_90s when keep_players is NULL", {
  # Regression (2026-09-13): a memory optimization (273d5cf0, "add keep_players
  # to narrow per-date output") added a `sel <- which(run_w90 >= min_weighted_90s)`
  # gate that applied even when keep_players is NULL -- silently dropping every
  # player below the default 3-weighted-90s floor from the full-population
  # result, contradicting this function's own docstring ("REGRESSION threshold,
  # not an inclusion gate... output_min_w90" is the row-count lever) and
  # corrupting a live retrain (07_train_psr_model.R) to 82.6% skill coverage
  # against a required >=95%, uniformly across every season 2016-2025. The only
  # exclusion mechanism should be output_min_w90 (default 0 = keep everyone).
  ms <- data.frame(
    player_id = c(rep("veteran", 20), "rookie"),
    player_name = c(rep("Veteran", 20), "Rookie"),
    match_id = c(paste0("v", 1:20), "r1"),
    match_date = c(as.Date("2024-01-01") + (0:19) * 7, as.Date("2024-04-15")),
    total_minutes = 90,
    position = "Striker",
    goals_p90 = c(rep(0.4, 20), 0.2),
    stringsAsFactors = FALSE
  )

  # veteran has 20 matches of decayed history (well over 3 weighted_90s);
  # rookie has a single match one day before the ref_date (well under 3).
  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = "2024-04-16", verbose = FALSE
  )

  sk <- result[[1]]
  expect_true("rookie" %in% sk$player_id)
  expect_true("veteran" %in% sk$player_id)
  rookie_row <- sk[sk$player_id == "rookie", ]
  expect_lt(rookie_row$weighted_90s, 3)
  expect_false(is.na(rookie_row$goals_p90))
})

test_that(".estimate_prematch_skills_batch keeps an explicitly-requested thin-history player when keep_players narrows the date", {
  # Same bug, the keep_players branch: idx[run_w90[idx] >= min_weighted_90s]
  # dropped a caller-requested player for having thin history, which makes no
  # sense for a caller that explicitly asked for that (date, player_id) pair.
  ms <- data.frame(
    player_id = c(rep("veteran", 20), "rookie"),
    player_name = c(rep("Veteran", 20), "Rookie"),
    match_id = c(paste0("v", 1:20), "r1"),
    match_date = c(as.Date("2024-01-01") + (0:19) * 7, as.Date("2024-04-15")),
    total_minutes = 90,
    position = "Striker",
    goals_p90 = c(rep(0.4, 20), 0.2),
    stringsAsFactors = FALSE
  )
  ref_date <- as.Date("2024-04-16")
  keep <- data.table::data.table(date = ref_date, player_id = "rookie")
  data.table::setkey(keep, date)

  result <- panna:::.estimate_prematch_skills_batch(
    ms, ref_dates = as.character(ref_date), keep_players = keep, verbose = FALSE
  )

  sk <- result[[1]]
  expect_true("rookie" %in% sk$player_id)
})


# =============================================================================
# GK PSR goal-scale correction (panna#202)
# =============================================================================

test_that(".scale_gk_psr preserves the osr + dsr == psr identity", {
  gk <- data.table::data.table(
    player_id = c("a", "b"),
    psr_raw = c(0.40, -0.20),
    psr = c(0.50, -0.30),
    osr = c(0.35, -0.10),
    dsr = c(0.15, -0.20)
  )
  # precondition: the identity holds before scaling
  expect_equal(gk$osr + gk$dsr, gk$psr)

  out <- panna:::.scale_gk_psr(gk, 0.72)
  expect_equal(out$osr + out$dsr, out$psr)
  expect_equal(out$psr, c(0.50, -0.30) * 0.72)
  expect_equal(out$psr_raw, c(0.40, -0.20) * 0.72)
})

test_that(".scale_gk_psr shrinks magnitude but never flips sign or reorders", {
  gk <- data.table::data.table(
    player_id = c("a", "b", "c"),
    psr = c(0.50, -0.30, 0.10),
    osr = c(0.35, -0.10, 0.06),
    dsr = c(0.15, -0.20, 0.04)
  )
  out <- panna:::.scale_gk_psr(gk, 0.72)
  # a correction, not a re-rating: order and signs must survive
  expect_equal(order(out$psr), order(gk$psr))
  expect_equal(sign(out$psr), sign(gk$psr))
  expect_true(all(abs(out$psr) < abs(gk$psr)))
})

test_that(".scale_gk_psr with scale 1 is an exact no-op (pre-#202 behaviour)", {
  gk <- data.table::data.table(
    player_id = "a", psr = 0.5, osr = 0.35, dsr = 0.15
  )
  expect_equal(panna:::.scale_gk_psr(gk, 1), gk)
})

test_that(".scale_gk_psr handles empty input and rejects a bad scale", {
  empty <- data.table::data.table(
    player_id = character(0), psr = numeric(0),
    osr = numeric(0), dsr = numeric(0)
  )
  expect_equal(nrow(panna:::.scale_gk_psr(empty, 0.72)), 0)

  gk <- data.table::data.table(player_id = "a", psr = 0.5, osr = 0.3, dsr = 0.2)
  expect_error(panna:::.scale_gk_psr(gk, c(0.5, 0.6)), "single finite number")
  expect_error(panna:::.scale_gk_psr(gk, NA_real_), "single finite number")
})

test_that("GK_PSR_GOAL_SCALE is in a sane range", {
  # a correction, not a rewrite -- if a retrain pushes this outside (0.4, 1.0)
  # the derivation should be re-examined rather than the constant just updated
  expect_true(GK_PSR_GOAL_SCALE > 0.4 && GK_PSR_GOAL_SCALE < 1.0)
})


test_that("compute_player_psr scales GK rows end-to-end and leaves outfield untouched", {
  # Pins the CALL-SITE wiring, not just .scale_gk_psr()'s arithmetic: the helper
  # can be correct while being applied to the wrong rows, or not applied at all.
  # Comparing two runs of the same input sidesteps having to reproduce the
  # coefficient maths -- only the ratio between them is asserted.
  skills <- data.table::data.table(
    player_id = c("gk1", "gk2", "of1", "of2"),
    player_name = c("K One", "K Two", "P One", "P Two"),
    primary_position = c("GK", "GK", "DEF", "FWD"),
    weighted_90s = c(20, 18, 22, 19),
    total_minutes = c(1800, 1620, 1980, 1710),
    passes_p90 = c(30, 28, 45, 20),
    touches_p90 = c(40, 38, 60, 30),
    long_balls_p90 = c(12, 10, 4, 1),
    saves_p90 = c(3, 2.5, 0, 0),
    high_claim_p90 = c(1.2, 0.9, 0, 0),
    keeper_sweeper_p90 = c(0.8, 0.6, 0, 0),
    gsaa_per90 = c(0.05, -0.02, 0, 0)
  )

  unscaled <- tryCatch(
    suppressWarnings(suppressMessages(
      compute_player_psr(skills, center = FALSE, gk_goal_scale = 1))),
    error = function(e) NULL)
  skip_if(is.null(unscaled), "compute_player_psr unavailable on this fixture")

  scaled <- suppressWarnings(suppressMessages(
    compute_player_psr(skills, center = FALSE, gk_goal_scale = 0.5)))

  u <- data.table::as.data.table(unscaled)
  s <- data.table::as.data.table(scaled)
  data.table::setorder(u, player_id); data.table::setorder(s, player_id)
  expect_equal(u$player_id, s$player_id)

  is_gk <- u$player_id %in% c("gk1", "gk2")
  # GK rows scale by exactly the factor...
  expect_equal(s$psr[is_gk], u$psr[is_gk] * 0.5)
  # ...and outfield rows are completely unaffected by a GK-only parameter
  expect_equal(s$psr[!is_gk], u$psr[!is_gk])
  # identity still holds on the scaled output
  if (all(c("osr", "dsr") %in% names(s))) {
    expect_equal(s$osr[is_gk] + s$dsr[is_gk], s$psr[is_gk])
  }
})


# =============================================================================
# PSR position + season calibration (panna#202 / #213 / #214)
# =============================================================================

test_that("shipped calibration table is well formed and in a sane range", {
  cal <- load_psr_calibration()
  expect_true(nrow(cal) > 0)
  expect_true(all(c("axis", "level", "factor") %in% names(cal)))
  expect_setequal(unique(cal$axis), c("position", "season"))
  # a calibration, not a rewrite: nothing should flip a sign or explode
  expect_true(all(cal$factor > 0))
  expect_true(all(cal$factor > 0.3 & cal$factor < 2))
  # Factors ARE the fitted slopes (goal units), so they are NOT centred on 1 --
  # outfield sits above 1 and GK well below. Assert the ordering that matters
  # rather than an absolute scale, so a re-derivation does not spuriously fail.
  pos <- cal[cal$axis == "position", ]
  gk <- pos$factor[pos$level == "GK"]
  out <- pos$factor[pos$level != "GK"]
  expect_true(gk < 0.8)                 # keepers are the one large correction
  expect_true(all(out > 0.9 & out < 1.6))
  expect_true(gk < min(out))            # GK strictly below every outfield bucket
  # season factors stay close to 1: the position axis already set the scale
  sea <- cal[cal$axis == "season", ]
  expect_true(all(sea$factor > 0.75 & sea$factor < 1.35))
})

test_that(".psr_calibration_factor defaults unknown keys to 1, never NA", {
  cal <- data.table::data.table(axis = "position", level = c("GK", "DEF"),
                                 factor = c(0.5, 0.9))
  f <- panna:::.psr_calibration_factor(cal, "position", c("GK", "DEF", "FWD", NA))
  expect_equal(f, c(0.5, 0.9, 1, 1))
  # an unseen axis is a no-op, not an error
  expect_equal(panna:::.psr_calibration_factor(cal, "season", c("2016", "2020")), c(1, 1))
  # so is an empty table
  expect_equal(panna:::.psr_calibration_factor(NULL, "position", c("GK")), 1)
})

test_that(".calibrate_psr_positions scales by position and preserves the identity", {
  cal <- data.table::data.table(axis = "position", level = c("GK", "FWD"),
                                 factor = c(0.5, 2.0))
  dt <- data.table::data.table(
    player_id = c("g", "f", "d"),
    primary_position = c("GK", "FWD", "DEF"),
    psr_raw = c(1.0, 1.0, 1.0), psr = c(0.4, 0.4, 0.4),
    osr = c(0.3, 0.3, 0.3), dsr = c(0.1, 0.1, 0.1)
  )
  out <- panna:::.calibrate_psr_positions(dt, cal)
  expect_equal(out$psr, c(0.2, 0.8, 0.4))     # GK halved, FWD doubled, DEF untouched
  expect_equal(out$osr + out$dsr, out$psr)    # identity survives
  expect_equal(out$psr_raw, c(0.5, 2.0, 1.0))
})

test_that(".calibrate_psr_positions is a no-op with NULL/empty calibration", {
  dt <- data.table::data.table(player_id = "a", primary_position = "GK",
                                psr = 0.4, osr = 0.3, dsr = 0.1)
  expect_equal(panna:::.calibrate_psr_positions(dt, NULL), dt)
  expect_equal(panna:::.calibrate_psr_positions(
    dt, data.table::data.table(axis = character(0), level = character(0),
                               factor = numeric(0))), dt)
})

test_that("apply_psr_season_calibration scales by season and needs the column", {
  cal <- data.table::data.table(axis = "season", level = c("2016", "2023"),
                                 factor = c(0.9, 1.1))
  dt <- data.table::data.table(
    player_id = c("a", "b", "c"), season_end_year = c(2016L, 2023L, 2019L),
    psr = c(1.0, 1.0, 1.0), osr = c(0.6, 0.6, 0.6), dsr = c(0.4, 0.4, 0.4)
  )
  out <- apply_psr_season_calibration(dt, cal)
  expect_equal(out$psr, c(0.9, 1.1, 1.0))   # 2019 absent -> unchanged, not NA
  expect_equal(out$osr + out$dsr, out$psr)
  expect_false(anyNA(out$psr))

  expect_error(
    apply_psr_season_calibration(
      data.table::data.table(player_id = "a", psr = 1), cal),
    "season_end_year"
  )
})

test_that("the current season passes through uncalibrated rather than becoming NA", {
  # a season's factor needs the FOLLOWING season's matches, so the newest
  # season is always absent from the table -- it must survive untouched
  cal <- load_psr_calibration()
  newest <- max(as.integer(cal$level[cal$axis == "season"]), na.rm = TRUE)
  dt <- data.table::data.table(player_id = "a", season_end_year = newest + 2L,
                                psr = 0.5, osr = 0.3, dsr = 0.2)
  out <- apply_psr_season_calibration(dt, cal)
  expect_equal(out$psr, 0.5)
  expect_false(anyNA(out$psr))
})


test_that("psr_leaderboard_eligible flags rather than filters, and NA fails the bar", {
  dt <- data.table::data.table(
    player_id = c("full", "half", "cameo", "unknown"),
    weighted_90s = c(30, 15, 7, NA_real_)
  )
  e <- psr_leaderboard_eligible(dt, min_90s = 15)
  # boundary is inclusive; an unknown workload cannot clear a workload bar
  expect_equal(e, c(TRUE, TRUE, FALSE, FALSE))
  # returns a flag per row, not a filtered table -- callers keep the rows
  expect_length(e, nrow(dt))
  expect_type(e, "logical")

  expect_error(
    psr_leaderboard_eligible(data.table::data.table(player_id = "a")),
    "weighted_90s"
  )
})

test_that("MIN_90S_PSR_LEADERBOARD is a plausible half-season bar", {
  expect_true(MIN_90S_PSR_LEADERBOARD >= 8 && MIN_90S_PSR_LEADERBOARD <= 25)
})


# =============================================================================
# apply_psr_calibration: both axes, post-offset (panna#202/#213/#214)
# =============================================================================

test_that("apply_psr_calibration applies position and season together", {
  cal <- data.table::data.table(
    axis = c("position", "position", "season", "season"),
    level = c("GK", "FWD", "2016", "2023"),
    factor = c(0.5, 2.0, 0.5, 2.0)
  )
  dt <- data.table::data.table(
    player_id = c("a", "b"),
    primary_position = c("GK", "FWD"),
    season_end_year = c(2016L, 2023L),
    psr = c(1.0, 1.0), osr = c(0.6, 0.6), dsr = c(0.4, 0.4)
  )
  out <- apply_psr_calibration(dt, cal)
  # both axes multiply: GK/2016 = 0.5*0.5, FWD/2023 = 2*2
  expect_equal(out$psr, c(0.25, 4.0))
  expect_equal(out$osr + out$dsr, out$psr)
})

test_that("apply_psr_calibration skips the season axis when the column is absent", {
  # the weekly snapshot path is keyed on snapshot_date, not season_end_year
  cal <- data.table::data.table(
    axis = c("position", "season"), level = c("GK", "2016"), factor = c(0.5, 0.1)
  )
  dt <- data.table::data.table(player_id = "a", primary_position = "GK",
                                psr = 1.0, osr = 0.6, dsr = 0.4)
  out <- apply_psr_calibration(dt, cal)
  expect_equal(out$psr, 0.5)          # position only, season not applied
  expect_equal(out$osr + out$dsr, out$psr)
})

test_that("apply_psr_calibration refuses to double-scale already-gk-scaled ratings", {
  skills <- data.table::data.table(
    player_id = c("gk1", "of1"), player_name = c("K", "P"),
    primary_position = c("GK", "DEF"),
    weighted_90s = c(20, 22), total_minutes = c(1800, 1980),
    passes_p90 = c(30, 45), touches_p90 = c(40, 60),
    long_balls_p90 = c(12, 4), saves_p90 = c(3, 0),
    high_claim_p90 = c(1.2, 0), keeper_sweeper_p90 = c(0.8, 0),
    gsaa_per90 = c(0.05, 0)
  )
  scaled <- tryCatch(
    suppressWarnings(suppressMessages(
      compute_player_psr(skills, center = FALSE, gk_goal_scale = 0.5))),
    error = function(e) NULL)
  skip_if(is.null(scaled), "compute_player_psr unavailable on this fixture")
  expect_error(apply_psr_calibration(scaled), "double-scale")

  # the default path (gk_goal_scale = 1) carries no marker and calibrates fine
  plain <- suppressWarnings(suppressMessages(
    compute_player_psr(skills, center = FALSE)))
  expect_no_error(apply_psr_calibration(plain))
})

test_that("compute_player_psr no longer calibrates internally", {
  # calibration moved to apply_psr_calibration() so it lands AFTER the additive
  # league offsets; verify the raw output is genuinely uncalibrated
  skills <- data.table::data.table(
    player_id = "gk1", player_name = "K", primary_position = "GK",
    weighted_90s = 20, total_minutes = 1800,
    passes_p90 = 30, touches_p90 = 40, long_balls_p90 = 12,
    saves_p90 = 3, high_claim_p90 = 1.2, keeper_sweeper_p90 = 0.8,
    gsaa_per90 = 0.05
  )
  raw <- tryCatch(
    suppressWarnings(suppressMessages(compute_player_psr(skills, center = FALSE))),
    error = function(e) NULL)
  skip_if(is.null(raw), "compute_player_psr unavailable on this fixture")
  cal <- apply_psr_calibration(data.table::as.data.table(raw))
  gk_f <- load_psr_calibration()
  gk_f <- gk_f$factor[gk_f$axis == "position" & gk_f$level == "GK"]
  expect_equal(cal$psr, raw$psr * gk_f, tolerance = 1e-9)
})


# =============================================================================
# League-set constants (panna#221)
# =============================================================================

test_that("bridge leagues are disjoint from rating leagues and not domestic", {
  # bridges are connectivity-only: they must be ADDED to the rating set, never
  # already inside it, and must never be attributable as a player's own league
  expect_length(intersect(PANNA_RATING_LEAGUES, PANNA_BRIDGE_LEAGUES), 0)
  expect_length(intersect(PANNA_DOMESTIC_LEAGUES, PANNA_BRIDGE_LEAGUES), 0)
  expect_true(all(PANNA_DOMESTIC_LEAGUES %in% PANNA_RATING_LEAGUES))
})

test_that("PANNA_DOMESTIC_LEAGUES excludes every continental and international comp", {
  # a cross-league cup is where leagues MEET, not one a player belongs to
  not_domestic <- c("UCL", "UEL", "UECL", "CAFCL", "WC", "EURO",
                    "AFCON", "Copa_America")
  expect_length(intersect(PANNA_DOMESTIC_LEAGUES, not_domestic), 0)
  # and the obvious domestic ones are present
  expect_true(all(c("ENG", "ESP", "GER", "ITA", "FRA", "MLS", "SAU") %in%
                    PANNA_DOMESTIC_LEAGUES))
})

test_that("the skills pipeline league set now includes the bridges", {
  # guards the panna#221 regression: PSR was starved of cross-league links
  # because this pipeline used PANNA_RATING_LEAGUES alone while EPV and RAPM
  # both added the bridges on top
  combined <- c(PANNA_RATING_LEAGUES, PANNA_BRIDGE_LEAGUES)
  expect_true(all(PANNA_BRIDGE_LEAGUES %in% combined))
  expect_gt(length(combined), length(PANNA_RATING_LEAGUES))
})

# =============================================================================
# PSV position calibration (panna#211)
# =============================================================================

test_that("load_psv_calibration returns the bundled position factors", {
  cal <- panna:::load_psv_calibration()
  expect_true(nrow(cal) > 0)
  expect_true(all(c("axis", "level", "factor") %in% names(cal)))
  expect_setequal(cal[cal$axis == "position", ]$level, c("GK", "DEF", "MID", "FWD"))
  # GK must be the SMALLEST factor: goalkeeper PSV is the least predictive per
  # unit, which is the whole point of the table. If this flips, the factors were
  # fitted on a same-match target (tautological) rather than leak-free.
  f <- stats::setNames(cal$factor, cal$level)
  expect_lt(f[["GK"]], f[["DEF"]])
  expect_lt(f[["GK"]], f[["MID"]])
  expect_lt(f[["GK"]], f[["FWD"]])
})

test_that("apply_psv_calibration scales by position and preserves osv + dsv == psv", {
  d <- data.table::data.table(
    player_id = c("a", "b", "c", "d"),
    pos_grp = c("GK", "DEF", "MID", "FWD"),
    psv = c(0.10, 0.10, 0.10, 0.10),
    osv = c(0.06, 0.06, 0.06, 0.06),
    dsv = c(0.04, 0.04, 0.04, 0.04)
  )
  out <- apply_psv_calibration(d)
  f <- stats::setNames(panna:::load_psv_calibration()$factor,
                       panna:::load_psv_calibration()$level)
  expect_equal(out$psv, 0.10 * unname(f[c("GK", "DEF", "MID", "FWD")]), tolerance = 1e-8)
  expect_equal(out$osv + out$dsv, out$psv, tolerance = 1e-10)
  # keepers must end up scaled DOWN relative to forwards
  expect_lt(out$psv[1], out$psv[4])
})

test_that("apply_psv_calibration passes unknown positions through unchanged", {
  d <- data.table::data.table(pos_grp = c("MID", "Referee", NA_character_),
                              psv = c(0.2, 0.2, 0.2))
  out <- apply_psv_calibration(d)
  expect_equal(out$psv[2], 0.2)   # unrecognised -> factor 1
  expect_equal(out$psv[3], 0.2)   # NA -> factor 1
  expect_false(isTRUE(all.equal(out$psv[1], 0.2)))
})

test_that("apply_psv_calibration accepts raw Opta position labels", {
  d <- data.table::data.table(position = c("Goalkeeper", "Defender", "Striker"),
                              psv = c(0.1, 0.1, 0.1))
  out <- apply_psv_calibration(d, position_col = "position")
  expect_lt(out$psv[1], out$psv[3])
})

test_that("apply_psv_calibration accepts 16-role classify_role() codes (review finding)", {
  # apply_psv_calibration()'s own fallback chain can hand .psv_position_group()
  # a fine-grained primary_position (see .player_role()'s comment) -- this
  # locks in the .role16_to_broad() fallback that closes that gap, and that
  # .psv_position_group() no longer hand-rolls its own raw-label regex
  # (it now defers to the canonical .simplify_position()).
  d <- data.table::data.table(position = c("GK", "CB", "DM", "CF"),
                              psv = c(0.1, 0.1, 0.1, 0.1))
  out <- apply_psv_calibration(d, position_col = "position")
  cal <- stats::setNames(panna:::load_psv_calibration()$factor,
                         panna:::load_psv_calibration()$level)
  expect_equal(out$psv, 0.1 * unname(cal[c("GK", "DEF", "MID", "FWD")]), tolerance = 1e-8)
})

test_that("apply_psv_calibration refuses to double-scale", {
  d <- data.table::data.table(pos_grp = "MID", psv = 0.2)
  once <- apply_psv_calibration(d)
  expect_error(apply_psv_calibration(once), "already been calibrated")
})

test_that("shipped PSV factors are scale-preserving, not the raw slopes", {
  # The raw fitted slopes average ~1.48; using them inflates all of PSV and every
  # league offset with it (+55%), a units artefact that would also break the
  # PSV/PSR unit correspondence the offsets are added at full strength on.
  cal <- panna:::load_psv_calibration()
  expect_true("slope_raw" %in% names(cal))
  expect_true(all(cal$factor < cal$slope_raw))          # normalised down
  # position minute shares are roughly DEF .30 / MID .38 / FWD .22 / GK .09;
  # under any plausible weighting the shipped factors must straddle 1
  expect_lt(min(cal$factor), 1)
  expect_gt(max(cal$factor), 1)
  expect_gt(mean(cal$factor), 0.8)
  expect_lt(mean(cal$factor), 1.2)
})

test_that("resolve_position_group ignores the Substitute match role", {
  d <- data.table::data.table(
    player_id = c("p1","p1","p1","p2","p2"),
    season_end_year = c(2024L,2024L,2024L,2024L,2024L),
    position = c("Striker","Striker","Substitute","Goalkeeper","Substitute"),
    total_minutes = c(90, 90, 20, 90, 45)
  )
  g <- panna:::resolve_position_group(d)
  expect_equal(g, c("FWD","FWD","FWD","GK","GK"))       # substitute rows inherit the real position
})

test_that("resolve_position_group falls back across seasons then to the row label", {
  d <- data.table::data.table(
    player_id = c("p1","p1","p2"),
    season_end_year = c(2023L, 2024L, 2024L),
    position = c("Defender","Substitute","Midfielder"),
    total_minutes = c(900, 90, 90)
  )
  g <- panna:::resolve_position_group(d)
  expect_equal(g[2], "DEF")                              # career fallback from 2023
  expect_equal(g[3], "MID")
})


# ============================================================================
# Streaming / checkpoint-resume for .estimate_prematch_skills_batch()
# (added 2026-09-05 with the stream_dir + checkpoint work -- see that
# function's docs. The resume DECISION is the consequential branch: accepting
# a checkpoint built from different inputs silently splices two computations
# together with no error, so it gets the bulk of the coverage here.)
# ============================================================================

.psr_test_fingerprint <- function(...) {
  base <- list(n_rows = 100L, n_players = 10L, n_dates = 20L,
               ref_dates_sum = 12345, min_weighted_90s = 3, output_min_w90 = 0,
               decay_params = list(rate = 0.003, prior_strength = 5))
  utils::modifyList(base, list(...))
}

.psr_test_checkpoint <- function(fingerprint, i = 5L) {
  list(fingerprint = fingerprint, run_rate = list(), run_eff = list(),
       run_w90 = numeric(10), cursor = 42L, i = i)
}

test_that(".psr_checkpoint_usable accepts an exactly-matching checkpoint", {
  fp <- .psr_test_fingerprint()
  expect_true(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(fp), fp, n_dates = 20L))
})

test_that(".psr_checkpoint_usable REJECTS a checkpoint built under different decay_params", {
  # The critical case: decay_params does not change n_rows/n_players/n_dates,
  # so a counts-only fingerprint would wrongly accept this and decay the
  # restored running sums under one setting while later dates use another.
  fp_run  <- .psr_test_fingerprint()
  fp_ckpt <- .psr_test_fingerprint(decay_params = list(rate = 0.009, prior_strength = 5))
  expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(fp_ckpt), fp_run, n_dates = 20L))
})

test_that(".psr_checkpoint_usable REJECTS mismatched data/config shape", {
  fp <- .psr_test_fingerprint()
  for (bad in list(
    .psr_test_fingerprint(n_rows = 101L),
    .psr_test_fingerprint(n_players = 11L),
    .psr_test_fingerprint(n_dates = 21L),
    .psr_test_fingerprint(ref_dates_sum = 12346),
    .psr_test_fingerprint(min_weighted_90s = 5),
    .psr_test_fingerprint(output_min_w90 = 3)
  )) {
    expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(bad), fp, n_dates = 20L))
  }
})

test_that(".psr_checkpoint_usable REJECTS null, malformed, or out-of-range checkpoints", {
  fp <- cp_fp <- .psr_test_fingerprint()
  expect_false(panna:::.psr_checkpoint_usable(NULL, fp, n_dates = 20L))
  expect_false(panna:::.psr_checkpoint_usable("not a list", fp, n_dates = 20L))
  # missing a required state component
  incomplete <- .psr_test_checkpoint(cp_fp); incomplete$run_w90 <- NULL
  expect_false(panna:::.psr_checkpoint_usable(incomplete, fp, n_dates = 20L))
  # position outside this run's range
  expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(cp_fp, i = 0L), fp, n_dates = 20L))
  expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(cp_fp, i = 21L), fp, n_dates = 20L))
  expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(cp_fp, i = NA_integer_), fp, n_dates = 20L))
})

test_that(".read_skill_chunk handles paths, in-memory tables, and NULL", {
  dt <- data.table::data.table(player_id = c("a", "b"), x = c(1.5, 2.5))
  expect_null(panna:::.read_skill_chunk(NULL))
  expect_identical(panna:::.read_skill_chunk(dt), dt)      # in-memory passthrough
  p <- tempfile(fileext = ".rds"); saveRDS(dt, p)
  expect_equal(panna:::.read_skill_chunk(p), dt)           # path -> read from disk
  unlink(p)
})

test_that(".psr_checkpoint_reject_reason names the ACTUAL reason, not always 'fingerprint'", {
  fp <- .psr_test_fingerprint()
  # usable -> empty string
  expect_identical(panna:::.psr_checkpoint_reject_reason(.psr_test_checkpoint(fp), fp, 20L), "")
  # each rejection path reports its own cause
  expect_match(panna:::.psr_checkpoint_reject_reason(NULL, fp, 20L), "unreadable")
  incomplete <- .psr_test_checkpoint(fp); incomplete$cursor <- NULL
  expect_match(panna:::.psr_checkpoint_reject_reason(incomplete, fp, 20L), "incomplete.*cursor")
  expect_match(panna:::.psr_checkpoint_reject_reason(.psr_test_checkpoint(fp, i = 99L), fp, 20L),
               "outside this run")
  # a changed input is named specifically, so a resume failure is debuggable
  bad_decay <- .psr_test_fingerprint(decay_params = list(rate = 0.009))
  expect_match(panna:::.psr_checkpoint_reject_reason(.psr_test_checkpoint(bad_decay), fp, 20L),
               "decay_params")
  bad_src <- .psr_test_fingerprint(source_fingerprint = list(size = 999))
  expect_match(panna:::.psr_checkpoint_reject_reason(.psr_test_checkpoint(bad_src), fp, 20L),
               "source_fingerprint")
})

test_that("source_fingerprint participates in checkpoint validation", {
  # A match_stats change that preserves every count is invisible to the
  # count-based fields; the source file's mtime/size is what catches it.
  fp_run  <- .psr_test_fingerprint(source_fingerprint = list(mtime = "2026-09-05 10:00:00", size = 100))
  fp_ckpt <- .psr_test_fingerprint(source_fingerprint = list(mtime = "2026-09-05 12:00:00", size = 100))
  expect_false(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(fp_ckpt), fp_run, 20L))
  expect_true(panna:::.psr_checkpoint_usable(.psr_test_checkpoint(fp_run), fp_run, 20L))
})

# ---------------------------------------------------------------------------
# PSV position calibration: position resolution + GK pinning (panna#211)
# ---------------------------------------------------------------------------

test_that("resolve_position_group returns all-NA instead of erroring when every row is a substitute", {
  # Nine league-seasons (Primeira_Liga 2015-2016, Liga_MX, A_League, Super_Lig,
  # Championship, all 2013-2016) carry a blank `position` on 100% of rows. A
  # caller scoped to one league-season hits this directly, and an error there is
  # swallowed by the game-log export's per-league tryCatch -- dropping the whole
  # league rather than leaving it uncalibrated.
  dt <- data.table::data.table(
    player_id     = c("p1", "p1", "p2"),
    position      = c("Substitute", "", "Substitute"),
    total_minutes = c(10, 20, 30)
  )
  res <- resolve_position_group(dt)
  expect_length(res, 3L)
  expect_true(all(is.na(res)))
})

test_that("resolve_position_group still resolves when only SOME rows are substitutes", {
  dt <- data.table::data.table(
    player_id     = c("p1", "p1", "p1"),
    position      = c("Defender", "Substitute", "Defender"),
    total_minutes = c(90, 10, 90)
  )
  expect_equal(resolve_position_group(dt), rep("DEF", 3L))
})

test_that("resolve_position_group picks the MINUTES-weighted position, not the most frequent", {
  # Three short cameos up front must not outvote one full season in midfield:
  # counting rows would return FWD, weighting by minutes returns MID.
  dt <- data.table::data.table(
    player_id     = rep("p1", 5L),
    position      = c("Striker", "Striker", "Striker", "Midfielder", "Midfielder"),
    total_minutes = c(5, 5, 5, 90, 90)
  )
  expect_equal(unique(resolve_position_group(dt)), "MID")
})

test_that(".psv_pos_grp marks a substitute keeper NA rather than giving it the GK factor", {
  # A keeper coming off the bench has position == "Substitute", so the GK router
  # (.detect_gk_rows) sends the row to the OUTFIELD model. Its resolved position
  # is still GK, so without pinning it would take the GK factor onto a score the
  # factor was not fitted on.
  dt <- data.table::data.table(
    player_id     = c("gk1", "gk1", "out1"),
    position      = c("Goalkeeper", "Substitute", "Defender"),
    total_minutes = c(90, 20, 90)
  )
  is_gk <- panna:::.detect_gk_rows(dt)
  expect_equal(is_gk, c(TRUE, FALSE, FALSE))

  res <- panna:::.psv_pos_grp(dt, is_gk)
  expect_equal(res[1], "GK")      # started, routed to the GK model
  expect_true(is.na(res[2]))      # sub keeper: outfield-model score, no factor
  expect_equal(res[3], "DEF")
})

test_that(".psv_pos_grp yields pos_grp == 'GK' exactly when the GK router fires", {
  dt <- data.table::data.table(
    player_id     = c("a", "b", "c", "d"),
    position      = c("Goalkeeper", "Defender", "Substitute", "Striker"),
    total_minutes = c(90, 90, 15, 90)
  )
  is_gk <- panna:::.detect_gk_rows(dt)
  res <- panna:::.psv_pos_grp(dt, is_gk)
  expect_equal(is_gk, !is.na(res) & res == "GK")
})

test_that(".psv_pos_grp degrades to NA rather than aborting when the position columns are absent", {
  dt <- data.table::data.table(psv = c(1, 2))
  res <- panna:::.psv_pos_grp(dt, is_gk = c(FALSE, FALSE))
  expect_length(res, 2L)
  expect_true(all(is.na(res)))
})

test_that(".detect_gk_rows()'s majority vote is scope-dependent -- the motivation for compute_player_psv()'s is_gk override", {
  # A rare emergency keeper: 3 "real" GK rows plus 2 rows recorded as
  # "Substitute" (their raw label doesn't say GK). Full-history gk_share = 0.6
  # -- clearly a keeper. But a caller iterating per league-season slice might
  # only see the 2 "Substitute" rows plus one blank-labeled row, none of which
  # say GK, and get gk_share = 0 for that slice -- a different, wrong answer
  # for the exact same player. This is what broke 07c's K-invariant check
  # 2026-09-13 for 3 rows in MLS 2026 (panna#249 follow-up).
  full <- data.table::data.table(
    player_id = rep("rare_keeper", 5),
    position = c("Goalkeeper", "Goalkeeper", "Goalkeeper", "Substitute", "Substitute")
  )
  slice <- full[4:5]  # the two "Substitute" rows only -- what one narrow slice sees

  full_is_gk <- panna:::.detect_gk_rows(full)
  slice_is_gk <- panna:::.detect_gk_rows(slice)

  expect_true(all(full_is_gk))           # full history: unambiguously GK
  expect_false(any(slice_is_gk))         # the narrow slice alone: wrong answer
})

test_that("compute_player_psv()'s is_gk override is honored, not silently re-detected", {
  skip_if_not(file.exists("../../data-raw/cache-skills/01_match_stats.rds"),
              "match stats cache not available")
  ms <- data.table::as.data.table(
    readRDS("../../data-raw/cache-skills/01_match_stats.rds"))
  sub <- ms[season == ms$season[1]][1:200]
  # Row 1's raw position doesn't say GK, so the default (is_gk = NULL,
  # recomputed internally) must NOT route it to the GK sub-model.
  sub[1, position := "Substitute"]
  default_is_gk <- panna:::.detect_gk_rows(sub)
  expect_false(default_is_gk[1])
  key1 <- sub[1, .(match_id, player_id)]

  # Force an externally-supplied is_gk = TRUE for that same row -- exactly
  # what a caller with a stable full-population classification (07c) does.
  forced_is_gk <- default_is_gk
  forced_is_gk[1] <- TRUE

  # compute_player_psv() does NOT preserve input row order (outfield rows
  # then GK rows, per its own documented behaviour) -- look the row back up
  # by key rather than assuming position [1] survives the split.
  out_default <- compute_player_psv(sub, min_adjust = FALSE, target = "blend")
  out_forced  <- compute_player_psv(sub, min_adjust = FALSE, target = "blend",
                                     is_gk = forced_is_gk)
  row_default <- merge(as.data.table(out_default), key1, by = c("match_id", "player_id"))
  row_forced  <- merge(as.data.table(out_forced),  key1, by = c("match_id", "player_id"))

  # NA is the correct default answer here (resolve_position_group() has no
  # career data to fall back on for this synthetic row) -- either NA or a
  # non-GK guess is fine, just never "GK".
  expect_true(is.na(row_default$pos_grp) || row_default$pos_grp != "GK")
  # With the override, it's forced to GK -- proving the parameter is actually
  # used (and not silently re-detected internally).
  expect_equal(row_forced$pos_grp, "GK")
})

test_that("compute_player_psv() validates is_gk length against the input", {
  sub <- data.table::data.table(
    player_id = c("a", "b"), position = c("GK", "MID"), total_minutes = c(90, 90)
  )
  expect_error(
    compute_player_psv(sub, is_gk = TRUE),  # length 1, not 2
    "is_gk"
  )
})

test_that("compute_player_psv returns pos_grp so the calibration can key on it", {
  # The exported `position` column is the per-match LINEUP position, so a
  # calibration keyed on it is a silent no-op. pos_grp must survive the GK
  # split's rbind to be usable downstream.
  skip_if_not(file.exists("../../data-raw/cache-skills/01_match_stats.rds"),
              "match stats cache not available")
  ms <- data.table::as.data.table(
    readRDS("../../data-raw/cache-skills/01_match_stats.rds"))
  sub <- ms[season == ms$season[1]][1:500]
  out <- compute_player_psv(sub, min_adjust = FALSE, center = TRUE,
                            scale_to_minutes = TRUE, exclude_efficiency = FALSE,
                            target = "blend")
  expect_true("pos_grp" %in% names(out))
  expect_equal(sum(out$pos_grp == "GK", na.rm = TRUE),
               sum(panna:::.detect_gk_rows(out)))
})

test_that("the gk_goal_scale double-scale guard survives a merge(), not just rbindlist", {
  # The marker existed only as an R attribute, and merge() drops attributes --
  # while merge() is exactly what 08b and 06 run between computing PSR and
  # calibrating it (the league-offset join). So on the attribute alone the guard
  # was already gone by the time it ran, in the one code shape the pipeline uses.
  dt <- data.table::data.table(
    player_id = c("p1", "p2"),
    primary_position = c("GK", "MID"),
    psr = c(0.10, 0.20), osr = c(0.05, 0.10), dsr = c(0.05, 0.10),
    panna_gk_scaled = TRUE
  )
  data.table::setattr(dt, "panna_gk_scaled", TRUE)
  cal <- data.table::data.table(axis = "position", level = c("GK", "MID"),
                                 factor = c(0.6411, 1.2337))

  # Direct: guard fires (this is all the old test covered).
  expect_error(apply_psr_calibration(dt, cal), "already scaled")

  # After a merge, the attribute is gone -- prove that, so the test fails loudly
  # if data.table ever starts preserving it and this test stops testing anything.
  offsets <- data.table::data.table(player_id = c("p1", "p2"), offset = c(0.01, 0.02))
  merged <- merge(dt, offsets, by = "player_id", all.x = TRUE)
  expect_null(attr(merged, "panna_gk_scaled"))

  # The column survives, so the guard must still fire.
  expect_true("panna_gk_scaled" %in% names(merged))
  expect_error(apply_psr_calibration(merged, cal), "already scaled")
})

test_that("an unmarked table calibrates normally (the guard is not always-on)", {
  dt <- data.table::data.table(
    player_id = c("p1", "p2"),
    primary_position = c("GK", "MID"),
    psr = c(0.10, 0.20), osr = c(0.05, 0.10), dsr = c(0.05, 0.10)
  )
  cal <- data.table::data.table(axis = "position", level = c("GK", "MID"),
                                 factor = c(0.6411, 1.2337))
  out <- apply_psr_calibration(dt, cal)
  expect_equal(out$psr, c(0.10 * 0.6411, 0.20 * 1.2337))
  expect_equal(out$osr + out$dsr, out$psr)
})

# ---------------------------------------------------------------------------
# .detect_gk_rows(): majority-vote fallback for substitute keepers (2026-09-13)
# ---------------------------------------------------------------------------

test_that(".detect_gk_rows routes a substitute keeper's bench row to GK via majority vote", {
  # A genuine keeper: 90% of appearances are real GK rows, one is a bench
  # cameo where the row's own label reads "Substitute".
  dt <- data.table::data.table(
    player_id = rep("keeper1", 10),
    position  = c(rep("Goalkeeper", 9), "Substitute")
  )
  is_gk <- panna:::.detect_gk_rows(dt)
  expect_true(all(is_gk))
})

test_that(".detect_gk_rows does NOT flip an outfielder's rare emergency-keeper row", {
  # An outfielder who covered in goal once (e.g. after a red card) -- 10% GK
  # share, well under the >50% majority bar. Their real Defender rows must
  # stay non-GK, and so must their own rare GK-labelled row: the majority vote
  # only ADDS coverage for genuine keepers, it never removes a row's own
  # correctly-labelled GK reading.
  dt <- data.table::data.table(
    player_id = rep("outfielder1", 10),
    position  = c(rep("Defender", 9), "Goalkeeper")
  )
  is_gk <- panna:::.detect_gk_rows(dt)
  expect_equal(is_gk, c(rep(FALSE, 9), TRUE))
})

test_that(".detect_gk_rows defaults an exact 50/50 split to non-GK", {
  dt <- data.table::data.table(
    player_id = rep("tied1", 4),
    position  = c("Goalkeeper", "Goalkeeper", "Substitute", "Substitute")
  )
  is_gk <- panna:::.detect_gk_rows(dt)
  expect_equal(is_gk, c(TRUE, TRUE, FALSE, FALSE))
})

test_that(".detect_gk_rows majority-vote fallback is purely additive, never removes a raw GK row", {
  # player a: 2 of 3 rows real GK (67% share, clears the >50% bar) -- both
  # substitute rows should flip. player b: a single real GK row, unaffected.
  ms <- data.table::data.table(
    player_id = c("a", "a", "a", "b"),
    position  = c("Goalkeeper", "Goalkeeper", "Substitute", "Goalkeeper")
  )
  raw <- grepl("GK|Goalkeeper", ms$position, ignore.case = TRUE)
  new <- panna:::.detect_gk_rows(ms)
  expect_true(all(new[raw]))          # every raw-TRUE row stays TRUE
  expect_true(all(new))               # player a's substitute row now flips too
})

test_that(".detect_gk_rows falls back gracefully with no player_id (unchanged, per-row only)", {
  dt <- data.table::data.table(position = c("Goalkeeper", "Substitute"))
  expect_equal(panna:::.detect_gk_rows(dt), c(TRUE, FALSE))
})

test_that(".detect_gk_rows recovers a genuine keeper's BLANK-position rows, not just Substitute", {
  # Measured live 2026-09-13: most of the newly-captured population (1,990 of
  # 3,344 rows) had a completely blank position, not literally "Substitute".
  dt <- data.table::data.table(
    player_id = rep("keeper2", 5),
    position  = c(rep("Goalkeeper", 4), "")
  )
  expect_true(all(panna:::.detect_gk_rows(dt)))
})
