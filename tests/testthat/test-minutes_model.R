test_that(".decay_past_sum matches the brute-force definition", {
  set.seed(1)
  x <- rpois(20, 50)
  d <- cumsum(sample(1:60, 20, replace = TRUE))
  got <- panna:::.decay_past_sum(x, d, half_life = 365)
  want <- vapply(seq_along(x), function(i) {
    if (i == 1L) return(0)
    j <- seq_len(i - 1L)
    sum(x[j] * 0.5 ^ ((d[i] - d[j]) / 365))
  }, numeric(1))
  expect_equal(got, want, tolerance = 1e-10)
})

test_that("query_minutes_features computes tournament + decay-prior features", {
  # pA: starts all 3 games. pB: bench in m1/m2, starts the WC game m3.
  mk <- function(mid, date, comp, rows) {
    data.table::data.table(
      team_name = "Testland", match_id = mid, match_date = date,
      competition = comp,
      player_id = rows$id, player_name = rows$id,
      position = rows$pos, position_side = "Centre",
      is_starter = rows$starter, minutes_played = rows$mins)
  }
  lineups <- data.table::rbindlist(list(
    mk("m1", "2026-03-01", "UEFA_WC_Qualifiers",
       data.frame(id = c("pA", "pB"), pos = c("Striker", "Substitute"),
                  starter = c(TRUE, FALSE), mins = c(90L, 0L))),
    mk("m2", "2026-03-25", "UEFA_WC_Qualifiers",
       data.frame(id = c("pA", "pB"), pos = c("Striker", "Substitute"),
                  starter = c(TRUE, FALSE), mins = c(90L, 20L))),
    mk("m3", "2026-06-11", "World_Cup",
       data.frame(id = c("pA", "pB"), pos = c("Striker", "Defender"),
                  starter = c(TRUE, TRUE), mins = c(90L, 75L)))
  ))
  cache <- prepare_minutes_cache(lineups,
                                 intl_comps = c("UEFA_WC_Qualifiers", "World_Cup"),
                                 ratings_path = tempfile(), verbose = FALSE)

  feats <- query_minutes_features(
    cache, c("pA", "pB", "pNEW"), "Testland",
    as_of_date = as.Date("2026-06-16"),
    tournament_start = as.Date("2026-06-11"))

  fB <- feats[feats$player_id == "pB", ]
  expect_equal(fB$tourn_mins_sofar, 75)
  expect_equal(fB$tourn_starts_sofar, 1)
  expect_equal(fB$started_prev_team_match, 1L)
  expect_equal(fB$mins_prev_team_match, 75)

  # p_start_decay: brute-force from the same definition (k = 3, hl = 365,
  # base = mean(is_starter) over intl rows = 4/6)
  as_of <- as.integer(as.Date("2026-06-16"))
  d <- as.integer(as.Date(c("2026-03-01", "2026-03-25", "2026-06-11")))
  w <- 0.5 ^ ((as_of - d) / 365)
  base <- 4 / 6
  expect_equal(fB$p_start_decay,
               (sum(w * c(0, 0, 1)) + 3 * base) / (sum(w) + 3),
               tolerance = 1e-10)
  expect_equal(fB$caps_decay, sum(w), tolerance = 1e-10)

  # pA's tournament features only count the WC window
  fA <- feats[feats$player_id == "pA", ]
  expect_equal(fA$tourn_mins_sofar, 90)
  expect_equal(fA$started_prev_team_match, 1L)

  # Unknown player: zeros + prior base rate, not NA
  fN <- feats[feats$player_id == "pNEW", ]
  expect_equal(fN$caps_decay, 0)
  expect_equal(fN$p_start_decay, base)
  expect_equal(fN$tourn_mins_sofar, 0)

  # Without tournament_start the tournament accumulators stay 0
  feats0 <- query_minutes_features(cache, "pB", "Testland",
                                   as_of_date = as.Date("2026-06-16"))
  expect_equal(feats0$tourn_mins_sofar, 0)
})
