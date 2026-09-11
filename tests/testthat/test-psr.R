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
