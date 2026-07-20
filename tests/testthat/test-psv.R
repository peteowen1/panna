# Tests for calculate_psv() and calculate_psv_components()

test_that("calculate_psv applies coefficients to raw stats", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(1, 0),
    tackles_p90 = c(2, 5),
    minutes_played = c(90, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.5, 0.1),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)

  expect_true("psv" %in% names(result))
  expect_true("psv_raw" %in% names(result))

  # Alice: 1*0.5 + 2*0.1 = 0.7
  # Bob:   0*0.5 + 5*0.1 = 0.5
  expect_equal(result$psv[result$player_id == "p1"], 0.7)
  expect_equal(result$psv[result$player_id == "p2"], 0.5)
})

test_that("calculate_psv minutes adjustment works", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 1),
    minutes_played = c(45, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = TRUE, center = FALSE)

  # Alice: goals_p90=2, mins=45, adjusted = 2 / (45/90) = 4
  # Bob:   goals_p90=1, mins=90, adjusted = 1 / (90/90) = 1
  expect_equal(result$psv[result$player_id == "p1"], 4.0)
  expect_equal(result$psv[result$player_id == "p2"], 1.0)
})

test_that("calculate_psv centering within round", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("A", "B", "C", "D"),
    match_id = c("m1", "m1", "m2", "m2"),
    season = c("2024", "2024", "2024", "2024"),
    round = c(1, 1, 2, 2),
    goals_p90 = c(2, 4, 10, 20),
    minutes_played = c(90, 90, 90, 90),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE)

  # Round 1: mean=3, A=2-3=-1, B=4-3=1
  # Round 2: mean=15, C=10-15=-5, D=20-15=5
  expect_equal(result$psv[result$player_id == "p1"], -1)
  expect_equal(result$psv[result$player_id == "p2"], 1)
  expect_equal(result$psv[result$player_id == "p3"], -5)
  expect_equal(result$psv[result$player_id == "p4"], 5)
})

test_that("calculate_psv standardizes with sd column", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 2,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    sd = 2.0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  # 2 / 2.0 * 1.0 = 1.0
  expect_equal(result$psv, 1.0)
})

test_that("calculate_psv excludes efficiency stats by default", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 1,
    pass_accuracy = 0.85,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = c("goals_p90", "pass_accuracy"),
    beta = c(1.0, 2.0),
    stringsAsFactors = FALSE
  )

  # With efficiency exclusion (default): only goals_p90 used
  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                           exclude_efficiency = TRUE)
  expect_equal(result$psv, 1.0)

  # Without efficiency exclusion: both used
  result2 <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                            exclude_efficiency = FALSE)
  expect_equal(result2$psv, 1.0 + 0.85 * 2.0)
})

test_that("calculate_psv_components ensures osv + dsv = psv", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 1),
    tackles_p90 = c(1, 4),
    minutes_played = c(90, 90),
    stringsAsFactors = FALSE
  )

  margin_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.5, 0.1),
    stringsAsFactors = FALSE
  )
  osr_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.6, 0.0),
    stringsAsFactors = FALSE
  )
  dsr_coef <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"),
    beta = c(0.0, 0.15),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv_components(stats, margin_coef, osr_coef, dsr_coef,
                                      min_adjust = FALSE, center = FALSE)

  expect_true("osv" %in% names(result))
  expect_true("dsv" %in% names(result))

  # osv + dsv must equal psv
  expect_equal(result$osv + result$dsv, result$psv)
})

test_that("calculate_psv scale_to_minutes makes PSV additive over minutes", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Starter", "Cameo"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 2),
    minutes_played = c(90, 45),
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  # Per-90 (default): identical rate regardless of minutes
  per90 <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                          scale_to_minutes = FALSE)
  expect_equal(per90$psv[per90$player_id == "p1"], 2)
  expect_equal(per90$psv[per90$player_id == "p2"], 2)

  # Minutes-scaled: value at the level of minutes played (per90 * mins/90)
  scaled <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                           scale_to_minutes = TRUE)
  expect_equal(scaled$psv[scaled$player_id == "p1"], 2 * (90 / 90))  # 2
  expect_equal(scaled$psv[scaled$player_id == "p2"], 2 * (45 / 90))  # 1
})

test_that("scale_to_minutes preserves osv + dsv = psv", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("Alice", "Bob"),
    match_id = c("m1", "m1"),
    goals_p90 = c(2, 1),
    tackles_p90 = c(1, 4),
    minutes_played = c(80, 30),
    stringsAsFactors = FALSE
  )
  margin_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                            beta = c(0.5, 0.1), stringsAsFactors = FALSE)
  osr_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                         beta = c(0.6, 0.0), stringsAsFactors = FALSE)
  dsr_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                         beta = c(0.0, 0.15), stringsAsFactors = FALSE)

  result <- calculate_psv_components(stats, margin_coef, osr_coef, dsr_coef,
                                      min_adjust = FALSE, center = FALSE,
                                      scale_to_minutes = TRUE)
  expect_equal(result$osv + result$dsv, result$psv)
})

test_that("calculate_psv handles zero coefficients gracefully", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    goals_p90 = 1,
    minutes_played = 90,
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "goals_p90",
    beta = 0,
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  expect_equal(result$psv, 0)
})

test_that("calculate_psv errors on no matching columns", {
  stats <- data.frame(
    player_id = "p1",
    player_name = "Alice",
    match_id = "m1",
    stringsAsFactors = FALSE
  )

  coef_df <- data.frame(
    stat_name = "nonexistent_stat",
    beta = 1.0,
    stringsAsFactors = FALSE
  )

  expect_error(calculate_psv(stats, coef_df), "No matching stat columns")
})

test_that("compute_player_psv routes keepers through the GK model (gsaa drives DSV)", {
  # Regression: compute_player_psv had NO test and originally applied the
  # outfield model to everyone — keepers scored as bad outfielders, no GSAA
  # credit. It now splits GK vs outfield and scores keepers with the gk_ model
  # (gsaa_per90 loads on GK defensive value, +0.025). This pins both that the
  # split happens and that a mixed frame keeps every row.
  gk <- data.frame(
    player_id = paste0("gk", 1:4),
    player_name = paste0("Keeper", 1:4),
    match_id = paste0("m", 1:4),
    primary_position = "GK",
    total_minutes = 90,
    saves_p90 = 3,                       # identical GK action stat
    gsaa_per90 = c(2, 2, -2, -2),        # only GSAA varies
    stringsAsFactors = FALSE
  )
  outfield <- data.frame(
    player_id = "of1", player_name = "Striker", match_id = "m5",
    primary_position = "Striker", total_minutes = 90,
    # stats with non-zero outfield coefficients (goals_p90 is zeroed by glmnet —
    # finishing enters via over-performance, so use shot-volume stats here)
    shots_p90 = 3, shots_obox_p90 = 1, pen_area_entries_p90 = 4,
    saves_p90 = 0, gsaa_per90 = 0,
    stringsAsFactors = FALSE
  )
  dt <- data.table::rbindlist(list(gk, outfield), fill = TRUE)

  res <- data.table::as.data.table(
    suppressWarnings(compute_player_psv(
      dt, min_adjust = FALSE, center = TRUE,
      scale_to_minutes = FALSE, exclude_efficiency = FALSE, target = "blend")))

  # Mixed frame: every row preserved (outfield + GK both scored, none dropped)
  expect_equal(nrow(res), 5L)
  expect_true(all(c("psv", "osv", "dsv") %in% names(res)))
  expect_false(any(is.na(res$dsv)))

  # GSAA routed to GK defensive value: high-GSAA keepers out-score low-GSAA ones.
  # (Only possible if keepers went through the gk_ model — the outfield DSR has
  # no gsaa coefficient.)
  res[, is_high_gsaa := player_id %in% c("gk1", "gk2")]
  gk_rows <- res[grepl("^gk", player_id)]
  expect_gt(mean(gk_rows[is_high_gsaa == TRUE]$dsv),
            mean(gk_rows[is_high_gsaa == FALSE]$dsv))
})


# =============================================================================
# reliability shrinkage (LIVE-PSV-UNBLOCK D1 v2, #158 Rec 2)
# =============================================================================

test_that("calculate_psv reliability=NULL is bit-identical to the pre-shrinkage path", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = "goals_p90", beta = 1.0, sd = 2.0, stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  # No reliability arg passed at all -- default NULL, standardized only: 2/2.0*1.0 = 1.0
  expect_equal(result$psv, 1.0)

  result_explicit_null <- calculate_psv(stats, coef_df, min_adjust = FALSE,
                                         center = FALSE, reliability = NULL)
  expect_identical(result$psv, result_explicit_null$psv)
})

test_that("calculate_psv reliability lambda scales the standardized contribution", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = "goals_p90", beta = 1.0, sd = 2.0, stringsAsFactors = FALSE
  )
  # Standardized (unshrunk) contribution: 2 / 2.0 * 1.0 = 1.0
  base <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  expect_equal(base$psv, 1.0)

  # lambda = 1 -- shrinkage is a no-op, but supplying `reliability` at all
  # (even with lambda = 1) turns on the GD-unit display scale -- NOT
  # identical to the unscaled base anymore (LIVE-PSV-UNBLOCK D1-v2 FINAL).
  lam1 <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                         reliability = data.frame(stat_name = "goals_p90", lambda = 1.0))
  expect_equal(lam1$psv, PSV_RELIABILITY_GD_SCALE * base$psv)

  # lambda = 0.5 -- halves the (unscaled) contribution, then the scale is
  # applied exactly once on top: 0.5 * PSV_RELIABILITY_GD_SCALE * base.
  lam_half <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                             reliability = data.frame(stat_name = "goals_p90", lambda = 0.5))
  expect_equal(lam_half$psv, 0.5 * PSV_RELIABILITY_GD_SCALE * base$psv)

  # lambda = 0 -- zeroes the contribution regardless of scale (0 * scale = 0)
  lam0 <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                         reliability = data.frame(stat_name = "goals_p90", lambda = 0))
  expect_equal(lam0$psv, 0)
})

test_that("calculate_psv applies PSV_RELIABILITY_GD_SCALE exactly once", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = "goals_p90", beta = 1.0, sd = 2.0, stringsAsFactors = FALSE
  )
  # Unscaled/unshrunk standardized contribution: 2 / 2.0 * 1.0 = 1.0
  base <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE)
  expect_equal(base$psv, 1.0)

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                           reliability = data.frame(stat_name = "goals_p90", lambda = 0.5))
  # A lambda = 0.5 feature contributes 0.5 * PSV_RELIABILITY_GD_SCALE * base --
  # if the scale were applied twice (e.g. once per lambda multiply and again
  # globally) this would instead be 0.5 * PSV_RELIABILITY_GD_SCALE^2 * base.
  expect_equal(result$psv, 0.5 * PSV_RELIABILITY_GD_SCALE * base$psv)
  once <- 0.5 * PSV_RELIABILITY_GD_SCALE * base$psv
  twice <- 0.5 * PSV_RELIABILITY_GD_SCALE^2 * base$psv
  expect_false(isTRUE(all.equal(result$psv, twice)))
  expect_equal(result$psv, once)
})

test_that("load_psv_match_reliability returns a complete two-population artifact", {
  rel <- load_psv_match_reliability()
  expect_true(all(c("model", "stat_name", "lambda") %in% names(rel)))
  expect_setequal(unique(rel$model), c("outfield", "gk"))
  expect_true(all(rel$lambda >= 0 & rel$lambda <= 1, na.rm = TRUE))
})

test_that("calculate_psv reliability lookup is by stat_name, not row order", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, tackles_p90 = 4, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"), beta = c(1.0, 1.0),
    sd = c(2.0, 4.0), stringsAsFactors = FALSE
  )
  # Rows deliberately in the REVERSE of coef_df/stat_cols order, with
  # DIFFERENT lambdas so positional indexing would give a different answer.
  reliability <- data.frame(
    stat_name = c("tackles_p90", "goals_p90"), lambda = c(0.25, 1.0),
    stringsAsFactors = FALSE
  )

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                           reliability = reliability)
  # goals_p90: 2/2.0*1.0 * 1.0 = 1.0; tackles_p90: 4/4.0*1.0 * 0.25 = 0.25
  # positional indexing would swap the lambdas and give 1*0.25 + 1*1.0 = 1.25 instead
  # (then the GD-unit display scale applies once on top, since reliability is supplied)
  expect_equal(result$psv, 1.25 * PSV_RELIABILITY_GD_SCALE)
})

test_that("calculate_psv falls back to lambda = 1 with a warning for stats absent from reliability", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, tackles_p90 = 4, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = c("goals_p90", "tackles_p90"), beta = c(1.0, 1.0),
    sd = c(2.0, 4.0), stringsAsFactors = FALSE
  )
  # reliability only covers goals_p90 -- tackles_p90 must fall back to lambda = 1
  reliability <- data.frame(stat_name = "goals_p90", lambda = 0.5, stringsAsFactors = FALSE)

  expect_warning(
    result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                             reliability = reliability),
    "tackles_p90"
  )
  # goals_p90: 2/2.0*1.0*0.5 = 0.5; tackles_p90: 4/4.0*1.0*1.0 = 1.0 (lambda fallback)
  # (then the GD-unit display scale applies once on top, since reliability is supplied)
  expect_equal(result$psv, (0.5 + 1.0) * PSV_RELIABILITY_GD_SCALE)
})

test_that("calculate_psv falls back to lambda = 1 with a warning for NA lambda values", {
  # A stat present in the table but with an NA lambda (e.g. too few players to
  # estimate the variance decomposition) must be treated the same as absent.
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, minutes_played = 90, stringsAsFactors = FALSE
  )
  coef_df <- data.frame(
    stat_name = "goals_p90", beta = 1.0, sd = 2.0, stringsAsFactors = FALSE
  )
  reliability <- data.frame(stat_name = "goals_p90", lambda = NA_real_,
                             stringsAsFactors = FALSE)

  expect_warning(
    result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = FALSE,
                             reliability = reliability),
    "goals_p90"
  )
  # (the GD-unit display scale applies once on top, since reliability is supplied)
  expect_equal(result$psv, 1.0 * PSV_RELIABILITY_GD_SCALE)
})

test_that("calculate_psv_components threads reliability through margin/osr/dsr", {
  stats <- data.frame(
    player_id = "p1", player_name = "Alice", match_id = "m1",
    goals_p90 = 2, minutes_played = 90, stringsAsFactors = FALSE
  )
  margin_coef <- data.frame(stat_name = "goals_p90", beta = 1.0, sd = 2.0,
                             stringsAsFactors = FALSE)
  osr_coef <- data.frame(stat_name = "goals_p90", beta = 1.0, sd = 2.0,
                          stringsAsFactors = FALSE)
  dsr_coef <- data.frame(stat_name = "goals_p90", beta = 0.0, sd = 2.0,
                          stringsAsFactors = FALSE)
  reliability <- data.frame(stat_name = "goals_p90", lambda = 0.5,
                             stringsAsFactors = FALSE)

  result <- calculate_psv_components(stats, margin_coef, osr_coef, dsr_coef,
                                      min_adjust = FALSE, center = FALSE,
                                      reliability = reliability)
  # margin/osr both shrunk by lambda = 0.5: 2/2.0*1.0*0.5 = 0.5, then the
  # GD-unit display scale applies once on top (reliability is supplied).
  expect_equal(result$psv, 0.5 * PSV_RELIABILITY_GD_SCALE)
  # osv + dsv = psv still reconciles exactly under the scale: it's a linear
  # multiplier applied identically inside each of the three calculate_psv
  # calls (margin/osr/dsr), so it factors out of the additive delta shift.
  expect_equal(result$osv + result$dsv, result$psv)
})

# =============================================================================
# minutes-weighted round centring (LIVE-PSV-UNBLOCK 2026-07-20, task 2)
# =============================================================================

test_that("center_weights = 'none' (default) is bit-identical to the pre-existing centering path", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("A", "B", "C", "D"),
    match_id = c("m1", "m1", "m2", "m2"),
    season = c("2024", "2024", "2024", "2024"),
    round = c(1, 1, 1, 1),
    goals_p90 = c(2, 4, 10, 20),
    minutes_played = c(90, 10, 90, 45),
    stringsAsFactors = FALSE
  )
  coef_df <- data.frame(stat_name = "goals_p90", beta = 1.0, stringsAsFactors = FALSE)

  no_arg <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE)
  explicit_none <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                                  center_weights = "none")
  expect_identical(no_arg$psv, explicit_none$psv)

  # Plain row mean = mean(2, 4, 10, 20) = 9, unaffected by minutes_played
  expect_equal(explicit_none$psv, c(2, 4, 10, 20) - 9)
})

test_that("center_weights = 'minutes' makes the minutes-scaled group sum exactly zero", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3", "p4", "p5"),
    player_name = c("A", "B", "C", "D", "E"),
    match_id = c("m1", "m1", "m2", "m2", "m3"),
    season = c("2024", "2024", "2024", "2024", "2024"),
    round = c(1, 1, 1, 1, 1),
    goals_p90 = c(2, 4, 10, 1, 6),
    minutes_played = c(90, 15, 60, 90, 30),  # deliberately lopsided
    stringsAsFactors = FALSE
  )
  coef_df <- data.frame(stat_name = "goals_p90", beta = 1.0, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                           scale_to_minutes = TRUE, center_weights = "minutes")

  # Sum of minutes-scaled psv within the round must be ~0 (float tolerance)
  expect_equal(sum(result$psv), 0, tolerance = 1e-10)

  # Cross-check against the closed-form weighted mean
  w <- stats$minutes_played / 90
  wmean <- sum(stats$goals_p90 * w) / sum(w)
  expected <- (stats$goals_p90 - wmean) * w
  expect_equal(result$psv, expected, tolerance = 1e-10)

  # Plain (unweighted) centering on the SAME data does NOT zero-sum after
  # scaling — demonstrates the property is specific to weighted centering.
  unweighted <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                               scale_to_minutes = TRUE, center_weights = "none")
  expect_false(isTRUE(all.equal(sum(unweighted$psv), 0)))
})

test_that("center_weights = 'minutes' centers each (season, round) group independently", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    player_name = c("A", "B", "C", "D"),
    match_id = c("m1", "m1", "m2", "m2"),
    season = c("2024", "2024", "2024", "2024"),
    round = c(1, 1, 2, 2),
    goals_p90 = c(2, 4, 10, 20),
    minutes_played = c(90, 30, 60, 90),
    stringsAsFactors = FALSE
  )
  coef_df <- data.frame(stat_name = "goals_p90", beta = 1.0, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                           scale_to_minutes = TRUE, center_weights = "minutes")

  r1 <- result[result$round == 1, ]
  r2 <- result[result$round == 2, ]
  expect_equal(sum(r1$psv), 0, tolerance = 1e-10)
  expect_equal(sum(r2$psv), 0, tolerance = 1e-10)
})

test_that("center_weights = 'minutes' falls back to the plain mean when all weights are zero", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    match_id = c("m1", "m1"),
    season = c("2024", "2024"),
    round = c(1, 1),
    goals_p90 = c(2, 6),
    minutes_played = c(0, NA),  # every row's weight resolves to 0
    stringsAsFactors = FALSE
  )
  coef_df <- data.frame(stat_name = "goals_p90", beta = 1.0, stringsAsFactors = FALSE)

  result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                           center_weights = "minutes")
  # Falls back to plain mean = 4, no division-by-zero NaN
  expect_false(any(is.na(result$psv)))
  expect_equal(result$psv, c(2, 6) - 4)
})

test_that("center_weights = 'minutes' warns and no-ops the weighting when no minutes column exists", {
  stats <- data.frame(
    player_id = c("p1", "p2"),
    player_name = c("A", "B"),
    match_id = c("m1", "m1"),
    season = c("2024", "2024"),
    round = c(1, 1),
    goals_p90 = c(2, 6),
    stringsAsFactors = FALSE
  )
  coef_df <- data.frame(stat_name = "goals_p90", beta = 1.0, stringsAsFactors = FALSE)

  expect_warning(
    result <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                             center_weights = "minutes"),
    "no minutes column"
  )
  # Falls back to unweighted (equal-weight) centering, same as center_weights = "none"
  baseline <- calculate_psv(stats, coef_df, min_adjust = FALSE, center = TRUE,
                             center_weights = "none")
  expect_equal(result$psv, baseline$psv)
})

test_that("calculate_psv_components preserves osv + dsv = psv under minutes-weighted centering", {
  stats <- data.frame(
    player_id = c("p1", "p2", "p3"),
    player_name = c("Alice", "Bob", "Cara"),
    match_id = c("m1", "m1", "m2"),
    season = c("2024", "2024", "2024"),
    round = c(1, 1, 1),
    goals_p90 = c(2, 1, 5),
    tackles_p90 = c(1, 4, 2),
    minutes_played = c(80, 30, 90),
    stringsAsFactors = FALSE
  )
  margin_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                            beta = c(0.5, 0.1), stringsAsFactors = FALSE)
  osr_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                         beta = c(0.6, 0.0), stringsAsFactors = FALSE)
  dsr_coef <- data.frame(stat_name = c("goals_p90", "tackles_p90"),
                         beta = c(0.0, 0.15), stringsAsFactors = FALSE)

  result <- calculate_psv_components(stats, margin_coef, osr_coef, dsr_coef,
                                      min_adjust = FALSE, center = TRUE,
                                      scale_to_minutes = TRUE,
                                      center_weights = "minutes")
  expect_equal(result$osv + result$dsv, result$psv, tolerance = 1e-10)
  expect_equal(sum(result$psv), 0, tolerance = 1e-10)
})

test_that("compute_player_psv threads center_weights through the GK/outfield split", {
  gk <- data.frame(
    player_id = c("gk1", "gk2"), player_name = c("Keeper1", "Keeper2"),
    match_id = c("m1", "m2"), primary_position = "GK",
    total_minutes = c(90, 30), season = "2024", round = 1,
    saves_p90 = c(3, 5), gsaa_per90 = c(1, -1), stringsAsFactors = FALSE
  )
  outfield <- data.frame(
    player_id = c("of1", "of2"), player_name = c("Striker1", "Striker2"),
    match_id = c("m3", "m4"), primary_position = "Striker",
    total_minutes = c(90, 60), season = "2024", round = 1,
    shots_p90 = c(3, 1), shots_obox_p90 = c(1, 0), pen_area_entries_p90 = c(4, 2),
    saves_p90 = 0, gsaa_per90 = 0, stringsAsFactors = FALSE
  )
  dt <- data.table::rbindlist(list(gk, outfield), fill = TRUE)

  res <- data.table::as.data.table(suppressWarnings(compute_player_psv(
    dt, min_adjust = FALSE, center = TRUE, scale_to_minutes = TRUE,
    exclude_efficiency = FALSE, target = "blend", center_weights = "minutes")))

  # Each sub-population (GK, outfield) is centered — and therefore zero-sums —
  # independently, same as the unweighted split already does today.
  expect_equal(sum(res[grepl("^gk", player_id)]$psv), 0, tolerance = 1e-10)
  expect_equal(sum(res[grepl("^of", player_id)]$psv), 0, tolerance = 1e-10)
})

test_that("compute_player_psv routes reliability by model (outfield vs gk)", {
  gk <- data.frame(
    player_id = "gk1", player_name = "Keeper", match_id = "m1",
    primary_position = "GK", total_minutes = 90,
    saves_p90 = 3, gsaa_per90 = 2, stringsAsFactors = FALSE
  )
  outfield <- data.frame(
    player_id = "of1", player_name = "Striker", match_id = "m2",
    primary_position = "Striker", total_minutes = 90,
    shots_p90 = 3, shots_obox_p90 = 1, pen_area_entries_p90 = 4,
    saves_p90 = 0, gsaa_per90 = 0, stringsAsFactors = FALSE
  )
  dt <- data.table::rbindlist(list(gk, outfield), fill = TRUE)

  # A deliberately extreme lambda for saves_p90 in the "gk" bucket only -- if
  # compute_player_psv routed the wrong bucket to the keeper row, this would
  # either error (unmatched stat) or leave the GK score unchanged.
  reliability <- data.table::data.table(
    model = c("gk", "outfield"),
    stat_name = c("saves_p90", "saves_p90"),
    lambda = c(0.1, 1.0)
  )

  res_no_rel <- data.table::as.data.table(suppressWarnings(compute_player_psv(
    dt, min_adjust = FALSE, center = FALSE, scale_to_minutes = FALSE,
    exclude_efficiency = FALSE, target = "blend")))
  res_rel <- data.table::as.data.table(suppressWarnings(compute_player_psv(
    dt, min_adjust = FALSE, center = FALSE, scale_to_minutes = FALSE,
    exclude_efficiency = FALSE, target = "blend", reliability = reliability)))

  gk_no_rel  <- res_no_rel[player_id == "gk1"]$psv
  gk_rel     <- res_rel[player_id == "gk1"]$psv
  of_no_rel  <- res_no_rel[player_id == "of1"]$psv
  of_rel     <- res_rel[player_id == "of1"]$psv

  # GK score shrinks when the gk-bucket lambda = 0.1 is applied.
  expect_false(isTRUE(all.equal(gk_no_rel, gk_rel)))
  # Outfield striker never has a nonzero saves_p90 coefficient, so an
  # outfield-bucket lambda = 1.0 for saves_p90 leaves the SHRINKAGE unchanged
  # (no stat of theirs is actually damped) -- but supplying `reliability` at
  # all still turns on the GD-unit display scale (LIVE-PSV-UNBLOCK D1-v2
  # FINAL), so of_rel is the scaled version of of_no_rel, not identical to it.
  expect_equal(of_rel, of_no_rel * PSV_RELIABILITY_GD_SCALE)
})
