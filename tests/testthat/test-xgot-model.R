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
