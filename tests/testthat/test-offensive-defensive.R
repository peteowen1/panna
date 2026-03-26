# Tests for offensive_defensive.R
# Covers: calculate_od_panna, split_od_contributions,
#         categorize_player_profile, get_top_offensive, get_top_defensive

# Helper: create mock rapm_data with X_od matrix for O/D split
create_od_rapm_data <- function(n_splints = 30, n_players = 5) {
  set.seed(42)
  player_ids <- paste0("player_", seq_len(n_players))

  # X_od has 2*n_players columns (offense + defense)
  X_od <- matrix(0, nrow = n_splints, ncol = 2 * n_players)
  for (i in seq_len(n_splints)) {
    # Random home/away players
    home_players <- sample(seq_len(n_players), 2)
    away_players <- sample(seq_len(n_players), 2)
    X_od[i, home_players] <- 1                    # offense
    X_od[i, n_players + away_players] <- -1        # defense
  }
  colnames(X_od) <- c(paste0(player_ids, "_off"), paste0(player_ids, "_def"))

  y <- rnorm(n_splints, mean = 0, sd = 0.3)

  mapping <- data.frame(
    player_id = player_ids,
    player_name = paste("Player", seq_len(n_players)),
    stringsAsFactors = FALSE
  )

  list(
    X_od = X_od,
    y = y,
    n_players = n_players,
    player_ids = player_ids,
    player_mapping = mapping
  )
}


# ===========================================================================
# categorize_player_profile
# ===========================================================================

test_that("categorize_player_profile classifies boundary values correctly", {
  # o_pct > 0.7 -> "Offensive"
  expect_equal(categorize_player_profile(0.8, 0.2), "Offensive")

  # o_pct > 0.55 -> "Balanced-Offensive"
  expect_equal(categorize_player_profile(0.6, 0.4), "Balanced-Offensive")

  # o_pct > 0.45 -> "Balanced"
  expect_equal(categorize_player_profile(0.5, 0.5), "Balanced")

  # o_pct > 0.3 -> "Balanced-Defensive"
  expect_equal(categorize_player_profile(0.35, 0.65), "Balanced-Defensive")

  # o_pct <= 0.3 -> "Defensive"
  expect_equal(categorize_player_profile(0.2, 0.8), "Defensive")
})

test_that("categorize_player_profile handles vectorized input", {
  result <- categorize_player_profile(
    c(0.8, 0.6, 0.5, 0.35, 0.2),
    c(0.2, 0.4, 0.5, 0.65, 0.8)
  )
  expect_equal(result, c("Offensive", "Balanced-Offensive", "Balanced",
                          "Balanced-Defensive", "Defensive"))
})


# ===========================================================================
# get_top_offensive / get_top_defensive
# ===========================================================================

test_that("get_top_offensive returns top n by o_panna", {
  ratings <- data.frame(
    player_id = paste0("p", 1:5),
    o_panna = c(0.5, 0.1, 0.8, 0.3, 0.6),
    d_panna = c(0.1, 0.4, 0.2, 0.5, 0.3),
    panna = c(0.6, 0.5, 1.0, 0.8, 0.9)
  )
  top <- get_top_offensive(ratings, n = 3)
  expect_equal(nrow(top), 3)
  expect_equal(top$player_id[1], "p3")  # highest o_panna
})

test_that("get_top_defensive returns top n by d_panna", {
  ratings <- data.frame(
    player_id = paste0("p", 1:5),
    o_panna = c(0.5, 0.1, 0.8, 0.3, 0.6),
    d_panna = c(0.1, 0.4, 0.2, 0.5, 0.3),
    panna = c(0.6, 0.5, 1.0, 0.8, 0.9)
  )
  top <- get_top_defensive(ratings, n = 3)
  expect_equal(nrow(top), 3)
  expect_equal(top$player_id[1], "p4")  # highest d_panna
})

test_that("get_top_offensive errors on missing column", {
  ratings <- data.frame(player_id = "p1", panna = 0.5)
  expect_error(get_top_offensive(ratings), "o_panna")
})

test_that("get_top_defensive errors on missing column", {
  ratings <- data.frame(player_id = "p1", panna = 0.5)
  expect_error(get_top_defensive(ratings), "d_panna")
})


# ===========================================================================
# split_od_contributions
# ===========================================================================

test_that("split_od_contributions produces o_panna + d_panna = panna", {
  panna_ratings <- data.frame(
    player_id = paste0("p", 1:4),
    panna = c(0.8, -0.2, 0.5, 0.0)
  )
  features <- data.frame(
    player_id = paste0("p", 1:4),
    xg_p90 = c(0.5, 0.1, 0.3, 0.2),  # offensive
    shots_p90 = c(3.0, 0.5, 2.0, 1.0),  # offensive
    tackles_p90 = c(1.0, 3.0, 2.0, 1.5),  # defensive
    interceptions_p90 = c(0.5, 2.5, 1.5, 1.0)  # defensive
  )

  result <- split_od_contributions(panna_ratings, features)

  expect_true("o_panna" %in% names(result))
  expect_true("d_panna" %in% names(result))
  # o_panna + d_panna should approximately equal panna
  expect_equal(result$o_panna + result$d_panna, result$panna, tolerance = 1e-10)
})

test_that("split_od_contributions warns on missing feature columns", {
  panna_ratings <- data.frame(player_id = "p1", panna = 0.5)
  features <- data.frame(player_id = "p1", some_stat = 1.0)

  expect_warning(
    result <- split_od_contributions(panna_ratings, features),
    "Cannot split"
  )
  # Should return original ratings unchanged
  expect_false("o_panna" %in% names(result))
})


# ===========================================================================
# calculate_od_panna
# ===========================================================================

test_that("calculate_od_panna errors without X_od", {
  bad_data <- list(X = matrix(1, 10, 5), y = rnorm(10))
  expect_error(calculate_od_panna(bad_data, data.frame()), "separate_od")
})

test_that("calculate_od_panna produces ratings with o_spm priors", {
  rapm_data <- create_od_rapm_data()
  spm_ratings <- data.frame(
    player_id = rapm_data$player_ids,
    o_spm = runif(rapm_data$n_players, -0.5, 0.5),
    d_spm = runif(rapm_data$n_players, -0.5, 0.5)
  )

  result <- calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)

  expect_true(is.list(result))
  expect_true("ratings" %in% names(result))
  expect_true(all(c("o_panna", "d_panna", "panna") %in% names(result$ratings)))
  expect_equal(nrow(result$ratings), rapm_data$n_players)

  # panna should equal o_panna + d_panna
  expect_equal(result$ratings$o_panna + result$ratings$d_panna,
               result$ratings$panna, tolerance = 1e-10)
})

test_that("calculate_od_panna works with spm (not o_spm) priors", {
  rapm_data <- create_od_rapm_data()
  spm_ratings <- data.frame(
    player_id = rapm_data$player_ids,
    spm = runif(rapm_data$n_players, -0.5, 0.5)
  )

  result <- calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)

  expect_true(is.list(result))
  expect_equal(nrow(result$ratings), rapm_data$n_players)
  # o_panna + d_panna = panna
  expect_equal(result$ratings$o_panna + result$ratings$d_panna,
               result$ratings$panna, tolerance = 1e-10)
})

test_that("calculate_od_panna handles unmatched player IDs gracefully", {
  rapm_data <- create_od_rapm_data()
  # SPM ratings for different players
  spm_ratings <- data.frame(
    player_id = paste0("other_", 1:3),
    o_spm = c(0.1, 0.2, 0.3),
    d_spm = c(0.3, 0.2, 0.1)
  )

  # Should not error — unmatched priors stay at 0
  result <- calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)
  expect_equal(nrow(result$ratings), rapm_data$n_players)
})


# ===========================================================================
# prepare_od_scatter_data
# ===========================================================================

test_that("prepare_od_scatter_data adds profile and label columns", {
  ratings <- data.frame(
    player_id = paste0("p", 1:3),
    player_name = c("Alice", "Bob", "Carol"),
    o_panna = c(0.8, 0.3, 0.5),
    d_panna = c(0.2, 0.5, 0.5),
    panna = c(1.0, 0.8, 1.0)
  )

  result <- prepare_od_scatter_data(ratings)
  expect_true("profile" %in% names(result))
  expect_true("label" %in% names(result))
  expect_equal(result$label, c("Alice", "Bob", "Carol"))
})

test_that("prepare_od_scatter_data errors on missing columns", {
  ratings <- data.frame(player_id = "p1", panna = 0.5)
  expect_error(prepare_od_scatter_data(ratings), "o_panna")
})
