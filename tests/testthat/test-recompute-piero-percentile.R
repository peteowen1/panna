# panna#507: piero_percentile must be a season-wide rank. 10b ranks over the
# leagues one run builds, and the rebuild builds a season in parts by league.

test_that("recompute_piero_percentile ranks over the whole table, not per part", {
  part_a <- data.table::data.table(
    player_id = c("a", "a", "b", "c"), league = "AAA",
    piero = c(1, 1, 5, -2))
  part_b <- data.table::data.table(
    player_id = c("d", "e", "f"), league = "BBB",
    piero = c(10, 0, 3))
  # What 10b writes for each part: a rank within the part.
  for (p in list(part_a, part_b)) {
    p[, piero_percentile := NA_real_]
  }
  recompute_piero_percentile(part_a)
  recompute_piero_percentile(part_b)
  expect_equal(unique(part_b[player_id == "d"]$piero_percentile), 100)  # top of its part

  season <- data.table::rbindlist(list(part_a, part_b))
  recompute_piero_percentile(season)
  # totals: a=2, b=5, c=-2, d=10, e=0, f=3 -> ranks c1 e2 a3 f4 b5 d6, of 6
  got <- unique(season[, .(player_id, piero_percentile)])
  expect_equal(got[order(player_id)]$piero_percentile,
               round(100 * c(3, 5, 1, 6, 2, 4) / 6, 1))
  # One value per player, even when a player appears in two parts.
  expect_equal(nrow(got), 6L)
})

test_that("a player who played in two parts gets one percentile", {
  d <- data.table::data.table(
    player_id = c("a", "a", "b", "c"), league = c("AAA", "BBB", "AAA", "BBB"),
    piero = c(1, 2, 0.5, 10), piero_percentile = c(100, 50, 20, 99))
  recompute_piero_percentile(d)
  expect_equal(length(unique(d[player_id == "a"]$piero_percentile)), 1L)
  expect_equal(unique(d[player_id == "a"]$piero_percentile), round(100 * 2 / 3, 1))
})

test_that("a player with no piero in any game is NA and not ranked", {
  d <- data.table::data.table(
    player_id = c("a", "b", "c"), piero = c(NA_real_, 1, 2),
    piero_percentile = 0)
  recompute_piero_percentile(d)
  expect_true(is.na(d[player_id == "a"]$piero_percentile))
  expect_equal(d[player_id == "b"]$piero_percentile, 50)
  expect_equal(d[player_id == "c"]$piero_percentile, 100)
})

test_that("a table without the columns is returned untouched", {
  d <- data.table::data.table(player_id = "a", x = 1)
  expect_identical(names(recompute_piero_percentile(d)), c("player_id", "x"))
})
