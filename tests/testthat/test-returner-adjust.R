# panna#497: returning-player adjustment with no cliffs (rust rises with club matches missed, drains with minutes).

test_that("returner_adjusted_panna keeps 1 - 0.25 * rust of the lead over replacement", {
  out <- returner_adjusted_panna(c(0.12, 0.12, NA, 0.12), -0.11, c(1, 0, 1, NA))
  expect_equal(out[1], -0.11 + 0.23 * 0.75)
  expect_equal(out[2], 0.12)
  expect_true(is.na(out[3]))
  expect_equal(out[4], 0.12)                                   # NA rust counts as 0
  expect_equal(returner_adjusted_panna(0.05, -0.1, 0.5, max_discount = 0), 0.05)
  expect_error(returner_adjusted_panna(0.1, -0.1, c(1, 0)))
})

# One club "C" plays every 7 days through 2026. Player p plays the first 9 matches, then is out.
club <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = 7)
mk_lineups <- function(p_dates, p_minutes = 90, p_team = "C") {
  mate <- data.frame(player_id = "mate", team_id = "C", match_id = paste0("m", seq_along(club)),
                     match_date = club, minutes_played = 90)
  pl <- data.frame(player_id = "p", team_id = p_team, match_id = paste0("m", match(p_dates, club)),
                   match_date = p_dates, minutes_played = p_minutes)
  rbind(mate, pl)
}
s_curve <- function(m) { z <- (m / 4)^6; z / (1 + z) }

test_that("rust while still out follows the S-curve of club matches missed (no step)", {
  lu <- mk_lineups(club[1:9])
  r <- sapply(10:20, function(j) returner_rust(lu, club[j] + 1)[player_id == "p", rust])
  expect_equal(r, s_curve(1:11), tolerance = 1e-12)            # club played 1..11 matches since his last
  expect_true(all(diff(r) > 0))                                # rises smoothly
  expect_lt(returner_rust(lu, club[10] + 1)[player_id == "p", rust], 0.001)
  expect_equal(returner_rust(lu, club[10] + 1)[player_id == "mate", rust], 0)
})

test_that("rust drains linearly with minutes back and is 0 after 900", {
  back <- club[21:40]                                          # back after missing 11 club matches
  lu <- mk_lineups(c(club[1:9], back))
  r0 <- s_curve(11)
  r5 <- returner_rust(lu, back[5] + 1, drain_minutes = 900)[player_id == "p"]
  expect_equal(r5$rust, r0 - 450 / 900, tolerance = 1e-12)
  expect_equal(r5$club_matches_missed, 11)
  expect_equal(r5$minutes_since_return, 450)
  expect_equal(returner_rust(lu, back[10] + 1, drain_minutes = 900)[player_id == "p", rust], 0)
  expect_equal(returner_rust(lu, back[10] + 1)[player_id == "p", rust], r0 - 900 / 1800, tolerance = 1e-12)  # default drain 1,800
})

test_that("the summer break adds no rust: the club played no matches", {
  lu <- mk_lineups(club[1:9])
  lu <- lu[!(lu$player_id == "mate" & lu$match_date > club[9] & lu$match_date < as.Date("2026-06-01")), ]
  expect_lt(returner_rust(lu, as.Date("2026-05-30"))[player_id == "p", rust], 1e-9)
})

test_that("corrupt minutes and duplicated rows do not drain rust early; rows on/after as_of are ignored", {
  back <- club[21:40]
  lu <- mk_lineups(c(club[1:9], back))
  lu$minutes_played[lu$player_id == "p" & lu$match_date == back[1]] <- 99999   # capped at 120
  expect_equal(returner_rust(lu, back[5] + 1, drain_minutes = 900)[player_id == "p", rust], s_curve(11) - (120 + 4 * 90) / 900, tolerance = 1e-12)
  lu2 <- mk_lineups(c(club[1:9], back))
  lu2 <- rbind(lu2, lu2[lu2$player_id == "p" & lu2$match_date == back[2], ])
  expect_equal(returner_rust(lu2, back[5] + 1, drain_minutes = 900)[player_id == "p", rust], s_curve(11) - 450 / 900, tolerance = 1e-12)
  expect_equal(returner_rust(lu, back[1])[player_id == "p", minutes_since_return], 0)   # the day of the return: still out
})

test_that("an unused-substitute game counts bench_weight of a missed match (Pete 2026-11-18: 0.75)", {
  lu <- mk_lineups(club[1:9])
  # p sits on the bench unused for the club's next 6 matches (0-minute rows), then is out of the squad for 2
  bench_rows <- data.frame(player_id = "p", team_id = "C", match_id = paste0("m", 10:15), match_date = club[10:15], minutes_played = 0)
  lu <- rbind(lu, bench_rows)
  m_eff <- 2 + 0.75 * 6                                         # 2 not in the squad + 6 on the bench at 0.75
  expect_equal(returner_rust(lu, club[17] + 1)[player_id == "p", rust], s_curve(m_eff), tolerance = 1e-12)
  expect_equal(returner_rust(lu, club[17] + 1, bench_weight = 1)[player_id == "p", rust], s_curve(8), tolerance = 1e-12)
  expect_equal(returner_rust(lu, club[17] + 1, bench_weight = 0)[player_id == "p", rust], s_curve(2), tolerance = 1e-12)
})
