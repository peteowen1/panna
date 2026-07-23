# Tests for xGOT (post-shot xG): placement features, the on-target/window
# filter, and the finishing decomposition in aggregate_player_xmetrics().

test_that(".create_placement_features builds expected columns and propagates NA", {
  pf <- .create_placement_features(c(45.2, 50, NA), c(2, 20, NA))
  expect_setequal(names(pf),
                  c("gm_y", "gm_z", "dist_to_near_post", "dist_to_top_corner"))
  expect_equal(pf$dist_to_near_post[1], 0)    # exactly on the left post
  expect_equal(pf$dist_to_near_post[2], 4.8)  # centre: min(|50-45.2|,|50-54.8|)
  # NA in -> NA out (must never become 0): missing coords propagate to derived
  expect_true(all(is.na(unlist(pf[3, ]))))
})

test_that("prepare_shots_for_xgot keeps only on-target, 2021+, coord-bearing shots", {
  shots <- data.frame(
    match_id = "m", event_id = 1:6, player_id = "p", player_name = "P",
    x = 80, y = 50,
    type_id = c(13L, 15L, 16L, 15L, 16L, 15L),  # miss, saved, goal, saved, goal, saved
    is_goal = c(0L, 0L, 1L, 0L, 1L, 0L),
    season  = c("2024-2025", "2024-2025", "2024-2025",
                "2019-2020", "2024-2025", "2024-2025"),
    goalmouth_y = c(50, 49, 51, 48, 50, NA),    # row 6 has no coords
    goalmouth_z = c(10, 12, 3, 20, 5, NA),
    body_part = "RightFoot", situation = "OpenPlay", big_chance = 0L,
    stringsAsFactors = FALSE
  )
  feat <- suppressWarnings(prepare_shots_for_xgot(shots))
  # kept: rows 2,3,5 (on-target + 2021+ + coords); dropped: 1 (miss),
  # 4 (pre-2021 window), 6 (no coords)
  expect_equal(nrow(feat), 3)
  expect_equal(attr(feat, "placement_cols"),
               c("gm_y", "gm_z", "dist_to_near_post", "dist_to_top_corner"))
  expect_true(all(c("gm_y", "dist_to_near_post", "is_goal") %in% names(feat)))
})

test_that("prepare_shots_for_xgot drops own goals (type-16 goal at own end) from training", {
  shots <- data.frame(
    match_id = "m", event_id = 1:3, player_id = "p", player_name = "P",
    x = c(80, 5, 82), y = 50,
    type_id = c(16L, 16L, 15L),        # goal, OWN goal (x=5), saved
    is_goal = c(1L, 1L, 0L),
    season  = "2024-2025",
    goalmouth_y = c(50, 51, 49), goalmouth_z = c(10, 3, 12),
    body_part = "RightFoot", situation = "OpenPlay", big_chance = 0L,
    stringsAsFactors = FALSE
  )
  feat <- suppressMessages(suppressWarnings(prepare_shots_for_xgot(shots)))
  expect_equal(nrow(feat), 2)                    # own goal excluded
  expect_false(2L %in% feat$event_id)
  expect_equal(attr(feat, "placement_cols"),
               c("gm_y", "gm_z", "dist_to_near_post", "dist_to_top_corner"))
  expect_true(all(c("gm_y", "dist_to_near_post", "is_goal") %in% names(feat)))
})

test_that("prepare_shots_for_xgot excludes blocked shots (q82) from training (#176)", {
  shots <- data.frame(
    match_id = "m", event_id = 1:3, player_id = "p", player_name = "P",
    x = 80, y = 50,
    type_id = c(15L, 15L, 16L),        # saved, BLOCKED (saved bucket), goal
    is_goal = c(0L, 0L, 1L),
    season  = "2024-2025",
    goalmouth_y = c(49, 50, 51), goalmouth_z = c(12, 19, 3),  # row 2 = placeholder z
    is_blocked = c(FALSE, TRUE, FALSE),
    body_part = "RightFoot", situation = "OpenPlay", big_chance = 0L,
    stringsAsFactors = FALSE
  )
  feat <- suppressMessages(suppressWarnings(prepare_shots_for_xgot(shots)))
  expect_equal(nrow(feat), 2)          # blocked shot excluded
  expect_false(2L %in% feat$event_id)
})

test_that("aggregate_player_xmetrics emits a consistent finishing decomposition", {
  spadl <- data.frame(
    match_id = "m1", player_id = "A", player_name = "Pl A", team_id = "T",
    action_type = "shot",
    result = c("success", "fail", "fail"),
    opta_type_id = c(16L, 13L, 15L),            # goal, miss, saved
    xg   = c(0.2, 0.1, 0.3),
    xgot = c(0.7, 0.0, 0.25),                   # on-target pred / off=0 / saved pred
    shot_on_target = c(TRUE, FALSE, TRUE),
    is_penalty = 0L, stringsAsFactors = FALSE
  )
  lineups <- data.frame(
    match_id = "m1", player_id = "A", player_name = "Pl A", team_id = "T",
    minutes_played = 90, team_name = "Team", stringsAsFactors = FALSE
  )
  r <- suppressMessages(aggregate_player_xmetrics(spadl, lineups))
  # on-target = saved(15) + goal(16); the miss (13) must NOT count
  expect_equal(r$shots_on_target, 2)
  expect_equal(r$xgot_placement, 0.45)          # (0.7-0.2) + (0.25-0.3)
  expect_equal(r$targeting, -0.1)               # off-target miss: 0 - 0.1
  expect_equal(r$placement_added, r$xgot_placement + r$targeting)
})

test_that("aggregate_player_xmetrics excludes penalties from the xGOT decomposition", {
  spadl <- data.frame(
    match_id = "m1", player_id = "A", player_name = "Pl A", team_id = "T",
    action_type = "shot",
    result = c("success", "success"),
    opta_type_id = c(16L, 16L),                 # both goals
    xg   = c(0.2, 0.76), xgot = c(0.7, 0.9),
    shot_on_target = c(TRUE, TRUE),
    is_penalty = c(0L, 1L),                     # second shot is a penalty
    stringsAsFactors = FALSE
  )
  lineups <- data.frame(
    match_id = "m1", player_id = "A", player_name = "Pl A", team_id = "T",
    minutes_played = 90, team_name = "Team", stringsAsFactors = FALSE
  )
  r <- suppressMessages(aggregate_player_xmetrics(spadl, lineups))
  expect_equal(r$shots, 2)            # both shots still count in box-score
  expect_equal(r$goals, 2)
  expect_equal(r$n_xgot_shots, 1)     # only the non-penalty shot
  expect_equal(r$xgot_placement, 0.5) # (0.7-0.2) — penalty excluded
  expect_equal(r$xgot, 0.7)
})

test_that("predict_xgot aborts when a model feature column is missing", {
  fake <- list(panna_metadata = list(
    feature_cols = c("gm_y", "gm_z", "dist_to_near_post", "dist_to_top_corner")))
  df <- data.frame(gm_y = 50, gm_z = 5, dist_to_near_post = 4)  # missing dist_to_top_corner
  expect_error(predict_xgot(fake, df), "missing feature")
})

test_that("add_xgot_to_spadl assigns xgot correctly (realign, 0/NA matrix, own-goal, dedup)", {
  # Mock the model prediction so we test the assignment logic deterministically.
  testthat::local_mocked_bindings(
    predict_xgot = function(xgot_model, shot_features) rep(0.5, nrow(shot_features))
  )
  spadl <- data.frame(
    match_id = "m", original_event_id = 1:6, action_type = "shot",
    start_x = c(85, 85, 85, 85, 3, 85),         # event 5 = own-goal origin (x < 50)
    start_y = 50, bodypart = "foot_right", stringsAsFactors = FALSE
  )
  # lookup: shuffled order, event 1 DUPLICATED (dedup test), events 4 & 6 absent
  lookup <- data.frame(
    match_id = "m",
    event_id = c(3, 1, 2, 5, 1),
    type_id  = c(16L, 16L, 15L, 16L, 16L),      # goal/goal/saved/goal/(dup)
    goalmouth_y = c(50, 51, NA, 50, 51),        # event 2 on-target but NO coords
    goalmouth_z = c(5, 3, NA, 5, 3),
    situation = "OpenPlay", stringsAsFactors = FALSE
  )
  r <- suppressWarnings(suppressMessages(add_xgot_to_spadl(spadl, list(), lookup)))
  expect_equal(nrow(r), 6)                       # dedup -> no row inflation/crash
  expect_equal(r$xgot[1], 0.5)                   # on-target + coords -> pred
  expect_true(is.na(r$xgot[2]))                  # on-target, no coords -> NA
  expect_equal(r$xgot[3], 0.5)                   # on-target + coords -> pred
  expect_true(is.na(r$xgot[4]))                  # unmatched -> NA
  expect_true(is.na(r$xgot[5]))                  # own-goal (type 16, x<50) -> NA
  expect_true(is.na(r$xgot[6]))                  # unmatched -> NA
  expect_equal(r$shot_on_target, c(TRUE, TRUE, TRUE, NA, TRUE, NA))
})

test_that("add_xgot_to_spadl prefers the is_own_goal qualifier over position (#148)", {
  testthat::local_mocked_bindings(
    predict_xgot = function(xgot_model, shot_features) rep(0.5, nrow(shot_features))
  )
  spadl <- data.frame(
    match_id = "m", original_event_id = 1:3, action_type = "shot",
    start_x = c(30, 85, 55),                     # 1 = legit long-range goal
    start_y = 50, bodypart = "foot_right",
    is_own_goal = c(FALSE, FALSE, TRUE),         # 3 = own goal logged past halfway
    stringsAsFactors = FALSE
  )
  lookup <- data.frame(
    match_id = "m", event_id = 1:3, type_id = 16L,   # all goals
    goalmouth_y = 50, goalmouth_z = 5, is_blocked = FALSE,
    situation = "OpenPlay", stringsAsFactors = FALSE
  )
  # Qualifier branch must not fire the missing-column fallback warning.
  expect_no_warning(r <- suppressMessages(add_xgot_to_spadl(spadl, list(), lookup)))
  expect_equal(r$xgot[1], 0.5)     # x<50 but NOT an own goal -> scored, not NA
  expect_equal(r$xgot[2], 0.5)
  expect_true(is.na(r$xgot[3]))    # own goal past halfway -> NA via qualifier
})

test_that("add_xgot_to_spadl excludes blocked shots (q82) from on-target (#176)", {
  testthat::local_mocked_bindings(
    predict_xgot = function(xgot_model, shot_features) rep(0.5, nrow(shot_features))
  )
  spadl <- data.frame(
    match_id = "m", original_event_id = 1:2, action_type = "shot",
    start_x = 85, start_y = 50, bodypart = "foot_right",
    stringsAsFactors = FALSE
  )
  lookup <- data.frame(
    match_id = "m", event_id = 1:2, type_id = 15L,      # both "Attempt Saved"
    goalmouth_y = c(49, 50), goalmouth_z = c(12, 19),   # 2 = placeholder z
    is_blocked = c(FALSE, TRUE),
    situation = "OpenPlay", stringsAsFactors = FALSE
  )
  r <- suppressWarnings(suppressMessages(add_xgot_to_spadl(spadl, list(), lookup)))
  expect_equal(r$xgot[1], 0.5)          # real save -> scored
  expect_equal(r$xgot[2], 0)            # blocked -> off-target, cannot score
  expect_equal(r$shot_on_target, c(TRUE, FALSE))
})

test_that("add_xgot_to_spadl falls back positionally for NA is_own_goal rows", {
  testthat::local_mocked_bindings(
    predict_xgot = function(xgot_model, shot_features) rep(0.5, nrow(shot_features))
  )
  # Mixed-vintage cache shape: rbindlist(fill=TRUE) leaves NA on old chunks.
  spadl <- data.frame(
    match_id = "m", original_event_id = 1:3, action_type = "shot",
    start_x = c(3, 85, 85), start_y = 50, bodypart = "foot_right",
    is_own_goal = c(NA, NA, FALSE),
    stringsAsFactors = FALSE
  )
  lookup <- data.frame(
    match_id = "m", event_id = 1:3, type_id = 16L,
    goalmouth_y = 50, goalmouth_z = 5, is_blocked = FALSE,
    situation = "OpenPlay", stringsAsFactors = FALSE
  )
  expect_warning(
    r <- suppressMessages(add_xgot_to_spadl(spadl, list(), lookup)),
    "NA is_own_goal"
  )
  expect_true(is.na(r$xgot[1]))    # NA marker + own-end goal -> positional NA
  expect_equal(r$xgot[2], 0.5)     # NA marker, normal goal -> scored
  expect_equal(r$xgot[3], 0.5)     # populated FALSE -> scored
})
