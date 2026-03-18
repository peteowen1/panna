# Tests for possession chain analytics and visualization

# Internal function aliases (not exported)
summarize_match_chains <- panna:::summarize_match_chains
summarize_player_chains <- panna:::summarize_player_chains
draw_pitch <- panna:::draw_pitch
plot_chain <- panna:::plot_chain
plot_match_chains <- panna:::plot_match_chains

# Helper: create minimal SPADL-like data with chain-relevant columns
create_test_spadl <- function() {
  data.frame(
    match_id = rep("m1", 15),
    action_id = 1:15,
    period_id = rep(1L, 15),
    time_seconds = c(10, 12, 14, 16, 18,   # chain 1: team A (5 actions)
                     20, 22, 24,             # chain 2: team B (3 actions)
                     55, 57, 59, 61,         # chain 3: team A (4 actions)
                     63, 65, 67),            # chain 4: team B (3 actions)
    team_id = c(rep("A", 5), rep("B", 3), rep("A", 4), rep("B", 3)),
    player_id = c("p1", "p2", "p3", "p2", "p1",
                  "p4", "p5", "p4",
                  "p1", "p3", "p2", "p1",
                  "p5", "p4", "p5"),
    player_name = c("P1", "P2", "P3", "P2", "P1",
                    "P4", "P5", "P4",
                    "P1", "P3", "P2", "P1",
                    "P5", "P4", "P5"),
    action_type = c("pass", "pass", "pass", "dribble", "shot",
                    "pass", "pass", "shot",
                    "pass", "pass", "pass", "shot",
                    "pass", "pass", "clearance"),
    result = c("success", "success", "success", "success", "success",  # goal
               "success", "success", "fail",                             # missed shot
               "success", "success", "success", "fail",                  # saved shot
               "success", "success", "success"),
    start_x = c(30, 45, 55, 65, 85,
                20, 35, 80,
                25, 50, 70, 88,
                15, 30, 40),
    start_y = c(50, 55, 48, 52, 50,
                50, 45, 48,
                60, 55, 50, 48,
                40, 45, 50),
    end_x = c(45, 55, 65, 75, 100,
              35, 80, 100,
              50, 70, 88, 100,
              30, 40, 50),
    end_y = c(55, 48, 52, 50, 50,
              45, 48, 50,
              55, 50, 48, 50,
              45, 50, 55),
    bodypart = rep("foot_right", 15),
    xg = c(rep(0, 4), 0.35,
           rep(0, 2), 0.12,
           rep(0, 3), 0.45,
           rep(0, 3)),
    stringsAsFactors = FALSE
  )
}


test_that("create_possession_chains assigns chain_id correctly", {
  spadl <- create_test_spadl()
  result <- create_possession_chains(spadl)

  expect_true("chain_id" %in% names(result))
  expect_true("chain_team_id" %in% names(result))
  expect_true("action_in_chain" %in% names(result))
  expect_true("chain_start_time" %in% names(result))


  # Should have 4 chains (team changes at rows 6, 9, 13)
  expect_equal(max(result$chain_id), 4)

  # Chain 1 = team A, 5 actions
  chain1 <- result[result$chain_id == 1, ]
  expect_equal(nrow(chain1), 5)
  expect_equal(unique(chain1$team_id), "A")

  # action_in_chain should be sequential
  expect_equal(chain1$action_in_chain, 1:5)
})


test_that("classify_chain_outcomes identifies goals and shots", {
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  outcomes <- classify_chain_outcomes(chains)

  expect_true(all(c("match_id", "chain_id", "outcome", "ends_in_goal") %in% names(outcomes)))

  # Chain 1 ends in goal (shot + success)
  expect_equal(outcomes$outcome[outcomes$chain_id == 1], "goal")
  expect_equal(outcomes$ends_in_goal[outcomes$chain_id == 1], 1L)

  # Chain 2 ends in shot (shot + fail)
  expect_equal(outcomes$outcome[outcomes$chain_id == 2], "shot")

  # Chain 4 is not a shot
  expect_false(outcomes$outcome[outcomes$chain_id == 4] %in% c("goal", "shot"))
})


test_that("summarize_match_chains produces correct match-team aggregation", {
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  outcomes <- classify_chain_outcomes(chains)
  labeled <- label_actions_with_outcomes(chains, outcomes)

  result <- summarize_match_chains(labeled)

  expect_true(all(c("match_id", "team_id", "total_chains", "chains_with_shot",
                     "chains_with_goal", "avg_chain_length", "avg_chain_duration",
                     "territory_pct", "chain_xg", "possession_pct") %in% names(result)))

  # Team A has 2 chains, Team B has 2 chains
  team_a <- result[result$team_id == "A", ]
  team_b <- result[result$team_id == "B", ]
  expect_equal(team_a$total_chains, 2)
  expect_equal(team_b$total_chains, 2)

  # Possession should sum to 100%
  expect_equal(sum(result$possession_pct), 100)

  # Team A: 1 goal chain, 1 shot chain (both count as chains_with_shot)
  expect_equal(team_a$chains_with_goal, 1)
})


test_that("summarize_player_chains tracks per-player contributions", {
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  outcomes <- classify_chain_outcomes(chains)
  labeled <- label_actions_with_outcomes(chains, outcomes)

  result <- summarize_player_chains(labeled)

  expect_true(all(c("match_id", "player_id", "player_name", "team_id",
                     "chains_involved", "chain_starts", "chain_finishes",
                     "progressive_chains", "key_chain_actions") %in% names(result)))

  # Player p1 is in chains 1 and 3 (both team A chains)
  p1 <- result[result$player_id == "p1", ]
  expect_equal(p1$chains_involved, 2)

  # Player p1 starts chain 1 (first action) and chain 3 (first action)
  expect_equal(p1$chain_starts, 2)
})


test_that("summarize_match_chains errors without chain_id", {
  spadl <- create_test_spadl()
  expect_error(summarize_match_chains(spadl), "chain_id")
})


test_that("summarize_player_chains errors without chain_id", {
  spadl <- create_test_spadl()
  expect_error(summarize_player_chains(spadl), "chain_id")
})


test_that("draw_pitch returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  p <- draw_pitch()
  expect_s3_class(p, "gg")
})


test_that("draw_pitch accepts background parameter", {
  skip_if_not_installed("ggplot2")
  p_white <- draw_pitch("white")
  p_green <- draw_pitch("green")
  expect_s3_class(p_white, "gg")
  expect_s3_class(p_green, "gg")
})


test_that("plot_chain creates visualization for a chain", {
  skip_if_not_installed("ggplot2")
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)

  p <- plot_chain(chains, target_match_id = "m1", target_chain_id = 1)
  expect_s3_class(p, "gg")
})


test_that("plot_chain errors for nonexistent chain", {
  skip_if_not_installed("ggplot2")
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  expect_error(plot_chain(chains, "m1", 999), "No actions found")
})


test_that("plot_match_chains creates match visualization", {
  skip_if_not_installed("ggplot2")
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  outcomes <- classify_chain_outcomes(chains)
  labeled <- label_actions_with_outcomes(chains, outcomes)

  p <- plot_match_chains(labeled, target_match_id = "m1")
  expect_s3_class(p, "gg")
})


test_that("plot_match_chains filters by team_id", {
  skip_if_not_installed("ggplot2")
  spadl <- create_test_spadl()
  chains <- create_possession_chains(spadl)
  outcomes <- classify_chain_outcomes(chains)
  labeled <- label_actions_with_outcomes(chains, outcomes)

  p <- plot_match_chains(labeled, target_match_id = "m1", target_team_id = "A")
  expect_s3_class(p, "gg")
})
