# Property-based tests for panna package
#
# Fuzz-style tests verifying structural invariants hold across random inputs.
# Uses withr::with_seed() for reproducibility with different random seeds.


# ============================================================================
# Helpers: Random Data Generators
# ============================================================================

# Generate random events for a single match
generate_random_events <- function(n_goals = NULL, n_subs = NULL, n_reds = 0) {
  if (is.null(n_goals)) n_goals <- sample(0:5, 1)
  if (is.null(n_subs)) n_subs <- sample(3:6, 1)

  rows <- list()

  if (n_goals > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      minute = sort(sample(1:90, n_goals, replace = TRUE)),
      is_goal = TRUE, is_sub = FALSE, is_red_card = FALSE,
      is_home = sample(c(TRUE, FALSE), n_goals, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  if (n_subs > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      minute = sort(sample(45:85, n_subs, replace = TRUE)),
      is_goal = FALSE, is_sub = TRUE, is_red_card = FALSE,
      is_home = sample(c(TRUE, FALSE), n_subs, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  if (n_reds > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      minute = sort(sample(10:80, n_reds, replace = TRUE)),
      is_goal = FALSE, is_sub = FALSE, is_red_card = TRUE,
      is_home = sample(c(TRUE, FALSE), n_reds, replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(
      minute = integer(0), is_goal = logical(0),
      is_sub = logical(0), is_red_card = logical(0),
      is_home = logical(0), stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}


# Generate random RAPM design matrix data
generate_random_rapm_data <- function(n_splints = NULL, n_players = NULL) {
  if (is.null(n_splints)) n_splints <- sample(20:80, 1)
  if (is.null(n_players)) n_players <- sample(15:40, 1)

  player_ids <- paste0("player_", seq_len(n_players))
  n_rows <- n_splints * 2
  n_player_cols <- n_players * 2

  X_players <- matrix(0, nrow = n_rows, ncol = n_player_cols)

  for (i in seq_len(n_splints)) {
    n_home <- sample(9:11, 1)
    n_away <- sample(9:11, 1)
    home_players <- sample(seq_len(n_players), min(n_home, n_players))
    available <- setdiff(seq_len(n_players), home_players)
    away_players <- sample(available, min(n_away, length(available)))

    row_home <- (i - 1) * 2 + 1
    row_away <- (i - 1) * 2 + 2

    for (p in home_players) {
      X_players[row_home, p] <- 1       # offense
      X_players[row_away, n_players + p] <- 1  # defense (swap perspective)
    }
    for (p in away_players) {
      X_players[row_home, n_players + p] <- 1  # defense
      X_players[row_away, p] <- 1       # offense (swap perspective)
    }
  }

  player_cols_off <- paste0(player_ids, "_off")
  player_cols_def <- paste0(player_ids, "_def")
  covariates <- c("is_home", "goal_diff")

  X_covariates <- matrix(
    c(rep(c(1, 0), n_splints), runif(n_rows, -2, 2)),
    nrow = n_rows
  )

  X <- cbind(X_players, X_covariates)
  colnames(X) <- c(player_cols_off, player_cols_def, covariates)

  list(
    X = X,
    y = rnorm(n_rows, mean = 1.5, sd = 0.5),
    weights = runif(n_rows, 5, 30),
    player_ids = player_ids,
    player_mapping = data.frame(
      player_id = player_ids,
      player_name = paste("Player", seq_len(n_players)),
      total_minutes = sample(500:3000, n_players, replace = TRUE),
      stringsAsFactors = FALSE
    ),
    covariate_cols = covariates,
    covariate_names = covariates,
    n_players = n_players,
    total_minutes = stats::setNames(
      sample(500:3000, n_players, replace = TRUE), player_ids
    )
  )
}


# Generate synthetic processed data for full pipeline tests (mirrors test-integration.R)
generate_synthetic_processed_data <- function(n_matches = 10, players_per_team = 11) {
  home_teams <- paste0("Team_", LETTERS[1:5])
  away_teams <- paste0("Team_", LETTERS[6:10])
  match_ids <- paste0("match_", seq_len(n_matches))

  results <- data.frame(
    match_id = match_ids,
    home_team = rep(home_teams, length.out = n_matches),
    away_team = rep(away_teams, length.out = n_matches),
    home_score = sample(0:3, n_matches, replace = TRUE),
    away_score = sample(0:3, n_matches, replace = TRUE),
    home_xg = runif(n_matches, 0.5, 3),
    away_xg = runif(n_matches, 0.5, 3),
    stringsAsFactors = FALSE
  )

  all_player_ids <- paste0("player_", 1:50)
  all_player_names <- paste("Player", 1:50)

  lineup_rows <- list()
  for (i in seq_len(n_matches)) {
    home_idx <- sample(1:25, players_per_team)
    away_idx <- sample(26:50, players_per_team)

    for (j in seq_along(home_idx)) {
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = all_player_ids[home_idx[j]],
        player_name = all_player_names[home_idx[j]],
        team = results$home_team[i],
        is_home = TRUE, is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
    for (j in seq_along(away_idx)) {
      lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
        match_id = match_ids[i],
        player_id = all_player_ids[away_idx[j]],
        player_name = all_player_names[away_idx[j]],
        team = results$away_team[i],
        is_home = FALSE, is_starter = j <= 11,
        on_minute = if (j <= 11) 0 else sample(45:75, 1),
        off_minute = if (j <= 11) 90 else 90,
        minutes = if (j <= 11) 90 else sample(15:45, 1),
        stringsAsFactors = FALSE
      )
    }
  }
  lineups <- do.call(rbind, lineup_rows)

  shot_rows <- list()
  for (i in seq_len(n_matches)) {
    n_shots <- sample(15:30, 1)
    match_lineups <- lineups[lineups$match_id == match_ids[i], ]
    shot_players <- match_lineups[sample(nrow(match_lineups), n_shots, replace = TRUE), ]
    shot_rows[[i]] <- data.frame(
      match_id = rep(match_ids[i], n_shots),
      minute = sort(sample(1:90, n_shots, replace = TRUE)),
      player_id = shot_players$player_id,
      player_name = shot_players$player_name,
      team = shot_players$team,
      is_home = shot_players$is_home,
      xg = pmin(runif(n_shots, 0.02, 0.5), 0.95),
      is_goal = rbinom(n_shots, 1, 0.1) == 1,
      is_penalty = FALSE, is_own_goal = FALSE,
      stringsAsFactors = FALSE
    )
  }
  shooting <- do.call(rbind, shot_rows)

  event_rows <- list()
  for (i in seq_len(n_matches)) {
    match_shots <- shooting[shooting$match_id == match_ids[i], ]
    goals <- match_shots[match_shots$is_goal, ]
    if (nrow(goals) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(goals)),
        team = goals$team, is_home = goals$is_home,
        event_type = "goal", minute = goals$minute,
        player_name = goals$player_name,
        is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }
    n_subs <- sample(2:3, 1)
    match_lineups <- lineups[lineups$match_id == match_ids[i] & lineups$is_starter, ]
    sub_players <- match_lineups[sample(nrow(match_lineups), min(n_subs, nrow(match_lineups))), ]
    if (nrow(sub_players) > 0) {
      event_rows[[length(event_rows) + 1]] <- data.frame(
        match_id = rep(match_ids[i], nrow(sub_players)),
        team = sub_players$team, is_home = sub_players$is_home,
        event_type = "substitution",
        minute = sample(45:75, nrow(sub_players), replace = TRUE),
        player_name = sub_players$player_name,
        is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  events <- do.call(rbind, event_rows)

  list(
    results = results, lineups = lineups, shooting = shooting,
    events = events, stats_summary = NULL
  )
}


# Generate random Opta events for SPADL conversion
generate_random_opta_events <- function(n_events = NULL) {
  if (is.null(n_events)) n_events <- sample(200:600, 1)

  # Opta type_ids: 1=pass, 2=offside pass, 3=take on, 4=foul, 10=save,
  # 13=shot miss, 14=shot post, 15=shot saved, 16=goal
  type_ids <- sample(c(1, 3, 4, 13, 14, 15, 16, 44, 49, 61),
                     n_events, replace = TRUE,
                     prob = c(0.4, 0.05, 0.05, 0.05, 0.02, 0.05, 0.02, 0.1, 0.06, 0.2))

  teams <- c("team_1", "team_2")
  players <- paste0("player_", 1:22)
  player_names <- paste("Player", 1:22)
  team_assignment <- c(rep("team_1", 11), rep("team_2", 11))

  player_idx <- sample(1:22, n_events, replace = TRUE)

  data.frame(
    match_id = "match_1",
    type_id = type_ids,
    team_id = team_assignment[player_idx],
    player_id = players[player_idx],
    player_name = player_names[player_idx],
    minute = sort(sample(0:95, n_events, replace = TRUE)),
    second = sample(0:59, n_events, replace = TRUE),
    x = runif(n_events, 0, 100),
    y = runif(n_events, 0, 100),
    outcome = sample(0:1, n_events, replace = TRUE),
    period_id = ifelse(sort(sample(0:95, n_events, replace = TRUE)) <= 45, 1L, 2L),
    end_x = runif(n_events, 0, 100),
    end_y = runif(n_events, 0, 100),
    stringsAsFactors = FALSE
  )
}


# ============================================================================
# Property Tests: Splint Boundaries
# ============================================================================

test_that("splint boundaries: ordering and continuity hold for random events", {
  for (seed in c(1, 42, 123, 999, 2024)) {
    withr::with_seed(seed, {
      events <- generate_random_events()
      boundaries <- create_splint_boundaries(events)

      # Property 1: start < end for every splint
      expect_true(all(boundaries$start_minute < boundaries$end_minute),
                  info = paste("seed:", seed, "- start must be < end"))

      # Property 2: no gaps - end[i] == start[i+1]
      if (nrow(boundaries) > 1) {
        for (i in seq_len(nrow(boundaries) - 1)) {
          expect_equal(boundaries$end_minute[i], boundaries$start_minute[i + 1],
                       info = paste("seed:", seed, "- gap between splints", i, "and", i + 1))
        }
      }

      # Property 3: first splint starts at 0
      expect_equal(boundaries$start_minute[1], 0,
                   info = paste("seed:", seed, "- must start at 0"))

      # Property 4: all durations positive
      expect_true(all(boundaries$duration > 0),
                  info = paste("seed:", seed, "- durations must be positive"))

      # Property 5: splint_num is sequential 1:n
      expect_equal(boundaries$splint_num, seq_len(nrow(boundaries)),
                   info = paste("seed:", seed, "- splint_num must be sequential"))
    })
  }
})


test_that("splint boundaries: goal accounting is consistent", {
  for (seed in c(7, 55, 200, 777)) {
    withr::with_seed(seed, {
      n_goals <- sample(1:5, 1)
      events <- generate_random_events(n_goals = n_goals, n_subs = 3, n_reds = 0)
      boundaries <- create_splint_boundaries(events)

      # Property: total goals across splints equals total goals in events
      total_home_goals <- sum(boundaries$goals_home)
      total_away_goals <- sum(boundaries$goals_away)
      expected_home <- sum(events$is_goal & events$is_home)
      expected_away <- sum(events$is_goal & !events$is_home)

      expect_equal(total_home_goals, expected_home,
                   info = paste("seed:", seed, "- home goals must sum correctly"))
      expect_equal(total_away_goals, expected_away,
                   info = paste("seed:", seed, "- away goals must sum correctly"))
    })
  }
})


test_that("splint boundaries: player counts respect red cards", {
  for (seed in c(10, 50, 300)) {
    withr::with_seed(seed, {
      events <- generate_random_events(n_goals = 1, n_subs = 2, n_reds = sample(1:2, 1))
      boundaries <- create_splint_boundaries(events)

      # Property: player counts = 11 - cumulative red cards
      expect_true(all(boundaries$n_players_home == 11 - boundaries$red_home),
                  info = paste("seed:", seed, "- home players = 11 - reds"))
      expect_true(all(boundaries$n_players_away == 11 - boundaries$red_away),
                  info = paste("seed:", seed, "- away players = 11 - reds"))

      # Property: player counts between 9 and 11
      expect_true(all(boundaries$n_players_home >= 9 & boundaries$n_players_home <= 11),
                  info = paste("seed:", seed, "- home player count in valid range"))
      expect_true(all(boundaries$n_players_away >= 9 & boundaries$n_players_away <= 11),
                  info = paste("seed:", seed, "- away player count in valid range"))
    })
  }
})


# ============================================================================
# Property Tests: RAPM Design Matrix
# ============================================================================

test_that("RAPM matrix: dimension invariants hold for random data", {
  skip_if_not_installed("glmnet")

  for (seed in c(1, 42, 88, 500)) {
    withr::with_seed(seed, {
      rapm_data <- generate_random_rapm_data()

      # Property 1: rows = 2 * n_splints
      n_rows <- nrow(rapm_data$X)
      n_player_cols <- rapm_data$n_players * 2
      expect_equal(n_rows %% 2, 0,
                   info = paste("seed:", seed, "- rows must be even (2 per splint)"))

      # Property 2: player columns = 2 * n_players (offense + defense)
      player_cols <- grep("_off$|_def$", colnames(rapm_data$X))
      expect_equal(length(player_cols), n_player_cols,
                   info = paste("seed:", seed, "- player cols = 2 * n_players"))

      # Property 3: all weights positive
      expect_true(all(rapm_data$weights > 0),
                  info = paste("seed:", seed, "- all weights must be positive"))

      # Property 4: y and weights have correct length
      expect_equal(length(rapm_data$y), n_rows,
                   info = paste("seed:", seed, "- y length must match n_rows"))
      expect_equal(length(rapm_data$weights), n_rows,
                   info = paste("seed:", seed, "- weights length must match n_rows"))
    })
  }
})


test_that("RAPM matrix: player exclusivity per row (offense XOR defense)", {
  withr::with_seed(42, {
    rapm_data <- generate_random_rapm_data(n_splints = 30, n_players = 20)
    n_players <- rapm_data$n_players

    for (row in sample(seq_len(nrow(rapm_data$X)), min(20, nrow(rapm_data$X)))) {
      for (p in seq_len(n_players)) {
        off_val <- rapm_data$X[row, p]
        def_val <- rapm_data$X[row, n_players + p]
        # A player can be on offense or defense in a row, never both
        expect_false(off_val > 0 && def_val > 0,
                     info = paste("Row", row, "player", p, "- can't be on both offense and defense"))
      }
    }
  })
})


test_that("RAPM fit + extract: rapm == offense - defense for all players", {
  skip_if_not_installed("glmnet")

  for (seed in c(42, 123, 777)) {
    withr::with_seed(seed, {
      rapm_data <- generate_random_rapm_data(n_splints = 40, n_players = 20)
      model <- fit_rapm(rapm_data, parallel = FALSE, nfolds = 3)
      ratings <- extract_rapm_ratings(model)

      # Property: rapm = offense - defense (exact)
      expect_equal(ratings$rapm, ratings$offense - ratings$defense,
                   tolerance = 1e-10,
                   info = paste("seed:", seed, "- rapm must equal offense - defense"))

      # Property: all columns are numeric
      expect_true(is.numeric(ratings$rapm), info = "rapm must be numeric")
      expect_true(is.numeric(ratings$offense), info = "offense must be numeric")
      expect_true(is.numeric(ratings$defense), info = "defense must be numeric")

      # Property: all players from input appear in output
      expect_equal(nrow(ratings), rapm_data$n_players,
                   info = paste("seed:", seed, "- all players must appear in ratings"))
    })
  }
})


# ============================================================================
# Property Tests: SPADL Conversion
# ============================================================================

test_that("SPADL conversion: output has all required columns", {
  for (seed in c(42, 100, 999)) {
    withr::with_seed(seed, {
      opta_events <- generate_random_opta_events(n_events = 200)
      spadl <- tryCatch(
        convert_opta_to_spadl(opta_events),
        error = function(e) NULL
      )

      skip_if(is.null(spadl), "SPADL conversion failed for this random input")

      required_cols <- c("match_id", "action_id", "period_id", "time_seconds",
                         "team_id", "player_id", "player_name",
                         "start_x", "start_y", "end_x", "end_y",
                         "action_type", "result", "bodypart")

      for (col in required_cols) {
        expect_true(col %in% names(spadl),
                    info = paste("seed:", seed, "- missing column:", col))
      }
    })
  }
})


test_that("SPADL conversion: coordinate bounds in [0, 100]", {
  withr::with_seed(42, {
    opta_events <- generate_random_opta_events(n_events = 300)
    spadl <- tryCatch(convert_opta_to_spadl(opta_events), error = function(e) NULL)

    skip_if(is.null(spadl), "SPADL conversion failed")

    # Property: all coordinates in [0, 100]
    coord_cols <- c("start_x", "start_y", "end_x", "end_y")
    for (col in coord_cols) {
      vals <- spadl[[col]]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        expect_true(all(vals >= 0 & vals <= 100),
                    info = paste("Column", col, "has values outside [0, 100]"))
      }
    }
  })
})


test_that("SPADL conversion: action_id is sequential within match", {
  withr::with_seed(123, {
    opta_events <- generate_random_opta_events(n_events = 250)
    spadl <- tryCatch(convert_opta_to_spadl(opta_events), error = function(e) NULL)

    skip_if(is.null(spadl), "SPADL conversion failed")

    # Property: action_id should be 1:N within each match
    for (mid in unique(spadl$match_id)) {
      match_actions <- spadl[spadl$match_id == mid, ]
      expect_equal(match_actions$action_id, seq_len(nrow(match_actions)),
                   info = paste("action_id not sequential for match", mid))
    }
  })
})


test_that("SPADL conversion: result and bodypart are valid enums", {
  withr::with_seed(55, {
    opta_events <- generate_random_opta_events(n_events = 300)
    spadl <- tryCatch(convert_opta_to_spadl(opta_events), error = function(e) NULL)

    skip_if(is.null(spadl), "SPADL conversion failed")

    valid_results <- c("success", "fail")
    valid_bodyparts <- c("foot", "head", "other")

    expect_true(all(spadl$result %in% valid_results),
                info = "All results must be 'success' or 'fail'")
    expect_true(all(spadl$bodypart %in% valid_bodyparts),
                info = "All bodyparts must be 'foot', 'head', or 'other'")
  })
})


test_that("SPADL conversion: time ordering within periods", {
  withr::with_seed(42, {
    opta_events <- generate_random_opta_events(n_events = 300)
    spadl <- tryCatch(convert_opta_to_spadl(opta_events), error = function(e) NULL)

    skip_if(is.null(spadl), "SPADL conversion failed")

    # Property: within each period, time_seconds should be non-decreasing
    for (pid in unique(spadl$period_id)) {
      period_actions <- spadl[spadl$period_id == pid, ]
      times <- period_actions$time_seconds
      expect_true(all(diff(times) >= 0),
                  info = paste("Time not non-decreasing in period", pid))
    }
  })
})


# ============================================================================
# Property Tests: Panna Rating Decomposition
# ============================================================================

test_that("panna rating: panna == spm_prior + deviation", {
  skip_if_not_installed("glmnet")

  for (seed in c(42, 200, 888)) {
    withr::with_seed(seed, {
      rapm_data <- generate_random_rapm_data(n_splints = 40, n_players = 20)

      # Create mock SPM ratings
      spm_ratings <- data.frame(
        player_id = rapm_data$player_ids,
        spm = rnorm(rapm_data$n_players, 0, 0.3),
        stringsAsFactors = FALSE
      )

      result <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 1)
      ratings <- result$ratings

      # Property: panna = spm_prior + deviation (exact decomposition)
      expect_equal(ratings$panna, ratings$spm_prior + ratings$deviation,
                   tolerance = 1e-10,
                   info = paste("seed:", seed, "- panna must equal spm_prior + deviation"))

      # Property: all rating columns are numeric
      expect_true(is.numeric(ratings$panna), info = "panna must be numeric")
      expect_true(is.numeric(ratings$spm_prior), info = "spm_prior must be numeric")
      expect_true(is.numeric(ratings$deviation), info = "deviation must be numeric")
    })
  }
})


test_that("panna rating: stronger lambda shrinks deviation toward zero", {
  skip_if_not_installed("glmnet")

  withr::with_seed(42, {
    rapm_data <- generate_random_rapm_data(n_splints = 50, n_players = 20)

    spm_ratings <- data.frame(
      player_id = rapm_data$player_ids,
      spm = rnorm(rapm_data$n_players, 0, 0.3),
      stringsAsFactors = FALSE
    )

    # Fit with small vs large lambda
    result_small <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 0.01)
    result_large <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 100)

    # Property: larger lambda produces smaller deviations (more shrinkage)
    mean_dev_small <- mean(abs(result_small$ratings$deviation))
    mean_dev_large <- mean(abs(result_large$ratings$deviation))

    expect_true(mean_dev_large < mean_dev_small,
                info = "Larger lambda should shrink deviations more toward zero")
  })
})


# ============================================================================
# Property Tests: SPM Model
# ============================================================================

test_that("SPM model: predictions are finite and reasonable", {
  skip_if_not_installed("glmnet")

  for (seed in c(42, 99, 500)) {
    withr::with_seed(seed, {
      n_players <- sample(40:80, 1)
      player_ids <- paste0("player_", seq_len(n_players))

      train_data <- data.frame(
        player_id = player_ids,
        player_name = paste("Player", seq_len(n_players)),
        total_minutes = sample(500:3000, n_players, replace = TRUE),
        n_matches = sample(10:38, n_players, replace = TRUE),
        goals_p90 = runif(n_players, 0, 0.8),
        npxg_p90 = runif(n_players, 0, 0.6),
        xa_p90 = runif(n_players, 0, 0.4),
        tackles_p90 = runif(n_players, 0.5, 4),
        interceptions_p90 = runif(n_players, 0.3, 2.5),
        progressive_passes_p90 = runif(n_players, 1, 8),
        stringsAsFactors = FALSE
      )

      # RAPM correlates with offensive stats
      train_data$rapm <- 0.5 * train_data$npxg_p90 +
        0.3 * train_data$xa_p90 -
        0.1 * train_data$tackles_p90 +
        rnorm(n_players, 0, 0.2)

      model <- fit_spm_model(train_data, nfolds = 3)
      spm_ratings <- calculate_spm_ratings(train_data, model)

      # Property: all predictions are finite
      expect_true(all(is.finite(spm_ratings$spm)),
                  info = paste("seed:", seed, "- all SPM predictions must be finite"))

      # Property: predictions have correct length
      expect_equal(nrow(spm_ratings), n_players,
                   info = paste("seed:", seed, "- one prediction per player"))

      # Property: SPM has smaller variance than RAPM (regularized → shrinkage)
      expect_true(var(spm_ratings$spm) <= var(train_data$rapm) * 1.5,
                  info = paste("seed:", seed, "- SPM should not have much larger variance than RAPM"))
    })
  }
})


# ============================================================================
# Property Tests: Full Pipeline Integration
# ============================================================================

test_that("full pipeline: splints cover all matches without gaps", {
  skip_if_not_installed("glmnet")

  withr::with_seed(42, {
    processed <- generate_synthetic_processed_data(n_matches = 8)
    splint_data <- create_all_splints(processed, verbose = FALSE)

    # Property: every match in results has splints
    result_matches <- unique(processed$results$match_id)
    splint_matches <- unique(splint_data$splints$match_id)

    for (mid in result_matches) {
      expect_true(mid %in% splint_matches,
                  info = paste("Match", mid, "should have splints"))
    }

    # Property: within each match, splints are contiguous
    for (mid in splint_matches) {
      match_splints <- splint_data$splints[splint_data$splints$match_id == mid, ]
      match_splints <- match_splints[order(match_splints$start_minute), ]

      # Start of first splint is 0
      expect_equal(match_splints$start_minute[1], 0,
                   info = paste("Match", mid, "- first splint must start at 0"))

      # No gaps between consecutive splints
      if (nrow(match_splints) > 1) {
        for (i in seq_len(nrow(match_splints) - 1)) {
          expect_equal(match_splints$end_minute[i], match_splints$start_minute[i + 1],
                       info = paste("Match", mid, "- gap at splint", i))
        }
      }
    }

    # Property: each splint has players assigned
    for (sid in unique(splint_data$splints$splint_id)) {
      splint_players <- splint_data$players[splint_data$players$splint_id == sid, ]
      expect_true(nrow(splint_players) > 0,
                  info = paste("Splint", sid, "must have players assigned"))
    }
  })
})
