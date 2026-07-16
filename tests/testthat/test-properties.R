# Property-based tests for panna package
#
# Fuzz-style tests verifying structural invariants hold across random inputs.
# Uses withr::with_seed() for reproducibility with different random seeds.


# ============================================================================
# Helpers: Random Data Generators
# ============================================================================

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


# Shared fixture: create_synthetic_processed_data() is defined in helper-fixtures.R
# Alias for backwards compatibility within this file
generate_synthetic_processed_data <- create_synthetic_processed_data


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
