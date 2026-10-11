# aggregate_lineup_ratings(): per-row asof_* ratings replace the season lookup (review R005/R017).

mk_lineups <- function(asof_epr = NULL) {
  lu <- data.frame(
    match_id = "m1", team_id = rep(c("H", "A"), each = 3),
    team_name = rep(c("Home FC", "Away FC"), each = 3),
    team_position = rep(c("home", "away"), each = 3),
    player_id = paste0("p", 1:6), player_name = paste0("P", 1:6),
    position = rep(c("Goalkeeper", "Defender", "Striker"), 2),
    is_starter = TRUE, stringsAsFactors = FALSE)
  if (!is.null(asof_epr)) lu$asof_epr <- asof_epr
  lu
}
ratings <- data.frame(
  player_id = paste0("p", 1:6), player_name = paste0("P", 1:6), season_end_year = 2025,
  panna = 0.1, offense = 0.05, defense = 0.05, spm = 0.02,
  epr = 1, epr_offensive = 0.5, epr_defensive = 0.5)

test_that("without asof columns the season lookup is used", {
  out <- aggregate_lineup_ratings(mk_lineups(), ratings, season_end_year = 2025)
  expect_equal(out$home_sum_epr, 3)
  expect_equal(out$away_sum_epr, 3)
})

test_that("asof_epr replaces the season value row by row", {
  out <- aggregate_lineup_ratings(mk_lineups(asof_epr = c(0.1, 0.2, 0.3, 0, 0, 0)),
                                  ratings, season_end_year = 2025)
  expect_equal(out$home_sum_epr, 0.6)
  expect_equal(out$away_sum_epr, 0)
  expect_false(any(grepl("^asof_", names(out))))
})

test_that("an NA asof value is imputed, not reverted to the season value", {
  out <- aggregate_lineup_ratings(mk_lineups(asof_epr = c(0.3, 0.3, NA, 0.3, 0.3, 0.3)),
                                  ratings, season_end_year = 2025)
  # Not 0.3 + 0.3 + 1 (season value back); the NA row gets the shrunk team mean.
  # team mean of rated = 0.3, coverage 1 -> shrink 1/(1+0.6) = 0.625 -> 0.1875
  expect_equal(out$home_sum_epr, 0.3 + 0.3 + 0.3 * 0.625)
  expect_equal(out$home_n_rated_players, 3)   # rated status is still panna-based
})
