# Tests for Step 3 (FABLE-PRIOR-FIX-PLAN.md): true per-splint EPV/WPA
# attribution in add_value_metrics_to_splints(), replacing the whole-match
# join + duration-proration (the C1 degeneracy: whole_match_value * 90 /
# match_duration cannot vary within a lineup). Deliberately self-contained
# fixtures (not shared with other test-rapm-*.R files), per the convention
# in test-rapm-tripwires.R / test-rapm-net-mode.R.

# ===========================================================================
# Fixture: 1 match, 3 splints (0-45, 45-90, 90-120), 4 players (2 home,
# 2 away). Splint 3 deliberately has NO stream actions (empty-splint test).
# Actions are distributed UNEVENLY across splints 1 and 2 on purpose -- the
# old whole-match proration would have forced an identical per-90 value in
# every splint of the match; true per-splint attribution must not.
# ===========================================================================

.svt_splints <- data.frame(
  splint_id = c("m1_1", "m1_2", "m1_3"),
  match_id = c("m1", "m1", "m1"),
  splint_num = c(1L, 2L, 3L),
  start_minute = c(0, 45, 90),
  end_minute = c(45, 90, 120),
  duration = c(45, 45, 30),
  gf_home = c(0, 1, 0),
  ga_home = c(0, 0, 0),
  avg_min = c(22.5, 67.5, 105),
  n_players_home = c(11L, 11L, 11L),
  n_players_away = c(11L, 11L, 11L),
  stringsAsFactors = FALSE
)

.svt_players <- data.frame(
  splint_id = rep(c("m1_1", "m1_2", "m1_3"), each = 4),
  match_id = "m1",
  player_id = rep(c("p1", "p2", "p3", "p4"), 3),
  player_name = rep(c("Player One", "Player Two", "Player Three", "Player Four"), 3),
  is_home = rep(c(TRUE, TRUE, FALSE, FALSE), 3),
  stringsAsFactors = FALSE
)

.svt_splint_data <- list(splints = .svt_splints, players = .svt_players)

# time_seconds is match-cumulative (minute*60 + second, continuous across
# periods -- Issue #94 / epv_features.R), matching the continuous
# start_minute/end_minute domain splints use.
.svt_player_action_epv <- data.frame(
  match_id = c("m1", "m1", "m1", "m1"),
  period_id = c(1L, 1L, 2L, 2L),
  time_seconds = c(600, 1200, 3600, 5100),   # 10, 20, 60, 85 minutes
  team_id = c("t1", "t2", "t1", "t2"),
  player_id = c("p1", "p3", "p2", "p4"),
  credit = c(0.5, -0.2, 0.9, -0.1),
  stringsAsFactors = FALSE
)

.svt_match_action_wpa <- data.frame(
  match_id = c("m1", "m1", "m1"),
  period_id = c(1L, 1L, 2L),
  time_seconds = c(600, 1800, 3600),          # 10, 30, 60 minutes
  wp_delta_home = c(0.03, -0.01, 0.05),
  stringsAsFactors = FALSE
)


# ===========================================================================
# EPV: per-splint per-team attribution (D1)
# ===========================================================================

test_that("add_value_metrics_to_splints computes true per-splint EPV sums", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          player_action_epv = .svt_player_action_epv)
  splints <- result$splints

  expect_equal(splints$epv_home[splints$splint_id == "m1_1"], 0.5)
  expect_equal(splints$epv_away[splints$splint_id == "m1_1"], -0.2)
  expect_equal(splints$epv_home[splints$splint_id == "m1_2"], 0.9)
  expect_equal(splints$epv_away[splints$splint_id == "m1_2"], -0.1)
})

test_that("per-splint EPV sums reconcile with per-match team totals", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          player_action_epv = .svt_player_action_epv)
  splints <- result$splints

  home_total <- sum(.svt_player_action_epv$credit[.svt_player_action_epv$player_id %in% c("p1", "p2")])
  away_total <- sum(.svt_player_action_epv$credit[.svt_player_action_epv$player_id %in% c("p3", "p4")])

  expect_equal(sum(splints$epv_home), home_total)
  expect_equal(sum(splints$epv_away), away_total)
})

test_that("a splint with no stream actions gets 0, not NA (EPV)", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          player_action_epv = .svt_player_action_epv)
  splints <- result$splints
  empty_row <- splints[splints$splint_id == "m1_3", ]

  expect_false(is.na(empty_row$epv_home))
  expect_false(is.na(empty_row$epv_away))
  expect_equal(empty_row$epv_home, 0)
  expect_equal(empty_row$epv_away, 0)
})

test_that("EPV boundary tie-break matches the xG findInterval convention (left-closed)", {
  # An action AT exactly the splint-2 start timestamp (45 min = 2700s)
  # belongs to splint 2 (the splint STARTING there), not splint 1 --
  # calculate_splint_npxgd_fast()'s findInterval(shot_minutes,
  # boundaries$start_minute) convention (R/splint_creation.R).
  tie_action <- data.frame(
    match_id = "m1", period_id = 2L, time_seconds = 2700, team_id = "t1",
    player_id = "p1", credit = 1.0, stringsAsFactors = FALSE
  )
  result <- add_value_metrics_to_splints(.svt_splint_data, player_action_epv = tie_action)
  splints <- result$splints

  expect_equal(splints$epv_home[splints$splint_id == "m1_1"], 0)
  expect_equal(splints$epv_home[splints$splint_id == "m1_2"], 1.0)
})

test_that("player_action_epv missing required columns aborts", {
  bad_stream <- data.frame(match_id = "m1", time_seconds = 10, stringsAsFactors = FALSE)
  expect_error(
    add_value_metrics_to_splints(.svt_splint_data, player_action_epv = bad_stream),
    "missing column"
  )
})


# ===========================================================================
# F1 (FABLE-PRIOR-FIX-PLAN.md review): a stream row whose (match_id,
# player_id) isn't in the splint players table's is_home lookup gets
# is_home = NA from the side_map join, and the aggregation silently drops it
# via `!is.na(is_home)` -- discarding real EPV credit with no signal unless
# guarded. Small unmatched share -> warn only, credit correctly excluded from
# the aggregate. Large unmatched share (>1% of rows OR >1% of |credit|) ->
# abort (an ID coverage gap, not incidental noise).
# ===========================================================================

test_that("EPV stream rows unmatched to the splint players table warn (small share) and are excluded from the aggregate", {
  n_known <- 199
  known_players <- rep(c("p1", "p2", "p3", "p4"), length.out = n_known)
  known_teams <- ifelse(known_players %in% c("p1", "p2"), "t1", "t2")
  time_secs <- seq(60, 5340, length.out = n_known)  # 1..89 minutes -- splints 1 & 2

  known_rows <- data.frame(
    match_id = "m1",
    period_id = ifelse(time_secs <= 2700, 1L, 2L),
    time_seconds = time_secs,
    team_id = known_teams,
    player_id = known_players,
    credit = 0.01,
    stringsAsFactors = FALSE
  )
  unknown_row <- data.frame(
    match_id = "m1", period_id = 1L, time_seconds = 300, team_id = "t1",
    player_id = "unknown_px", credit = 0.01, stringsAsFactors = FALSE
  )
  stream <- rbind(known_rows, unknown_row)

  # 1 unmatched row of 200 (0.5%) and 0.01 of 2.00 total |credit| (0.5%) --
  # both under the 1% abort threshold, so this should warn, not abort.
  expect_warning(
    result <- add_value_metrics_to_splints(.svt_splint_data, player_action_epv = stream),
    "no matching"
  )

  total_credit_all <- sum(stream$credit)
  dropped_credit <- unknown_row$credit
  expect_equal(
    sum(result$splints$epv_home) + sum(result$splints$epv_away),
    total_credit_all - dropped_credit
  )
})

test_that("EPV stream rows unmatched to the splint players table abort when the unmatched share exceeds 1%", {
  bad_row <- data.frame(
    match_id = "m1", period_id = 1L, time_seconds = 300, team_id = "t1",
    player_id = "unknown_px", credit = 0.5, stringsAsFactors = FALSE
  )
  stream <- rbind(.svt_player_action_epv, bad_row)  # 1 of 5 rows (20%) unmatched

  expect_error(
    add_value_metrics_to_splints(.svt_splint_data, player_action_epv = stream),
    "coverage gap"
  )
})


# ===========================================================================
# WPA: per-splint home-POV sum, EXACTLY zero-sum (D2)
# ===========================================================================

test_that("add_value_metrics_to_splints computes true per-splint WPA sums", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          match_action_wpa = .svt_match_action_wpa)
  splints <- result$splints

  expect_equal(splints$wpa_home[splints$splint_id == "m1_1"], 0.03 - 0.01)
  expect_equal(splints$wpa_home[splints$splint_id == "m1_2"], 0.05)
})

test_that("wpa_home + wpa_away == 0 exactly on synthetic data", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          match_action_wpa = .svt_match_action_wpa)
  splints <- result$splints

  expect_equal(splints$wpa_home + splints$wpa_away, rep(0, nrow(splints)))
  expect_equal(max(abs(splints$wpa_home + splints$wpa_away)), 0)
})

test_that("a splint with no stream actions gets 0, not NA (WPA)", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          match_action_wpa = .svt_match_action_wpa)
  empty_row <- result$splints[result$splints$splint_id == "m1_3", ]

  expect_false(is.na(empty_row$wpa_home))
  expect_false(is.na(empty_row$wpa_away))
  expect_equal(empty_row$wpa_home, 0)
  expect_equal(empty_row$wpa_away, 0)
})

test_that("the Step 4 net-mode design matrix accepts Step 3's WPA splint output end-to-end", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          match_action_wpa = .svt_match_action_wpa)

  # Step 4's zero-sum tripwire (.build_rapm_row_data, mode = "net") must NOT
  # abort -- it fires unless target_home + target_away is ~0 everywhere.
  design <- expect_no_error(
    create_rapm_design_matrix(result, min_minutes = 0, target_type = "wpa", mode = "net")
  )

  expect_equal(design$mode, "net")
  expect_equal(design$n_rows, nrow(.svt_splints))
  # One signed column per player (+1 replacement), not the od 2x layout.
  expect_equal(ncol(design$X_players), design$n_players + 1L)
})

test_that("match_action_wpa missing required columns aborts", {
  bad_stream <- data.frame(match_id = "m1", time_seconds = 10, stringsAsFactors = FALSE)
  expect_error(
    add_value_metrics_to_splints(.svt_splint_data, match_action_wpa = bad_stream),
    "missing column"
  )
})


# ===========================================================================
# C1-signature test: the whole point of Step 3. Per-splint targets must VARY
# between splints of the SAME match when actions are distributed unevenly --
# the old whole-match-value * 90 / duration proration made this exactly
# constant per 90 for every splint in a match (duration cancels).
# ===========================================================================

test_that("C1 signature: per-splint EPV target_per_90 varies within a match (old proration made it constant)", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          player_action_epv = .svt_player_action_epv)

  rd <- .build_rapm_row_data(result$splints, "epv")$row_data
  home_rows <- rd[rd$home_away == "home" & rd$match_id == "m1", ]

  expect_gt(var(home_rows$target_per_90), 0)
  # Concretely: splint 1 = 0.5*90/45 = 1.0, splint 2 = 0.9*90/45 = 1.8 --
  # different values, not the single constant the old proration produced.
  expect_false(isTRUE(all.equal(home_rows$target_per_90[1], home_rows$target_per_90[2])))
})

test_that("C1 signature: per-splint WPA target_per_90 varies within a match", {
  result <- add_value_metrics_to_splints(.svt_splint_data,
                                          match_action_wpa = .svt_match_action_wpa)

  rd <- .build_rapm_row_data(result$splints, "wpa")$row_data
  home_rows <- rd[rd$home_away == "home" & rd$match_id == "m1", ]

  expect_gt(var(home_rows$target_per_90), 0)
})


# ===========================================================================
# D3: psv removed from RAPM entirely (target_map, target_type enums). PSV's
# own standalone pipeline (calculate_psv() etc.) is untouched -- covered by
# test-psv.R / test-psr.R, not here.
# ===========================================================================

test_that("psv is absent from create_rapm_design_matrix's target_type enum", {
  valid_types <- as.character(formals(create_rapm_design_matrix)$target_type)[-1]
  expect_false("psv" %in% valid_types)
  expect_true("epv" %in% valid_types)
  expect_true("wpa" %in% valid_types)
})

test_that("psv is absent from prepare_rapm_data's target_type enum", {
  valid_types <- as.character(formals(prepare_rapm_data)$target_type)[-1]
  expect_false("psv" %in% valid_types)
})

test_that("target_type = 'psv' is rejected by create_rapm_design_matrix", {
  expect_error(
    create_rapm_design_matrix(.svt_splint_data, target_type = "psv"),
    "should be one of"
  )
})

test_that(".build_rapm_row_data has no 'psv' entry in target_map (unknown target_type)", {
  splints_with_psv_cols <- .svt_splints
  splints_with_psv_cols$psv_home <- 0
  splints_with_psv_cols$psv_away <- 0
  expect_error(
    .build_rapm_row_data(splints_with_psv_cols, "psv"),
    "Unknown target_type"
  )
})


# ===========================================================================
# NULL inputs / PSV pass-through regression guards
# ===========================================================================

test_that("add_value_metrics_to_splints with all-NULL streams leaves splints unchanged", {
  result <- add_value_metrics_to_splints(.svt_splint_data)
  expect_false("epv_home" %in% names(result$splints))
  expect_false("wpa_home" %in% names(result$splints))
  expect_false("psv_home" %in% names(result$splints))
  expect_equal(nrow(result$splints), nrow(.svt_splints))
})

test_that("add_value_metrics_to_splints still supports the psv whole-match-proration path", {
  player_game_psv <- data.frame(
    player_id = c("p1", "p2", "p3", "p4"),
    match_id = rep("m1", 4),
    psv = c(0.4, 0.2, -0.1, 0.3),
    stringsAsFactors = FALSE
  )
  result <- add_value_metrics_to_splints(.svt_splint_data, player_game_psv = player_game_psv)
  splints <- result$splints

  expect_true("psv_home" %in% names(splints))
  expect_true("psv_away" %in% names(splints))
  # Whole-match value 0.6 (p1+p2), prorated by duration/match_duration
  # (45/120 for splint 1) -- proration is UNCHANGED for psv (D3).
  match_dur <- sum(.svt_splints$duration)
  expect_equal(splints$psv_home[splints$splint_id == "m1_1"], 0.6 * 45 / match_dur)
})
