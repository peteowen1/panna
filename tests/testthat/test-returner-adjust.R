# panna#497: returning-player adjustment and the return rule.

test_that("returner_adjusted_panna keeps 85% of the lead over replacement for returners only", {
  out <- returner_adjusted_panna(c(0.12, 0.12, NA), -0.11, c(TRUE, FALSE, TRUE))
  expect_equal(out[1], -0.11 + 0.23 * 0.85)
  expect_equal(out[2], 0.12)
  expect_true(is.na(out[3]))
  expect_equal(returner_adjusted_panna(0.05, -0.1, TRUE, keep = 1), 0.05)   # keep = 1 is the identity
  expect_error(returner_adjusted_panna(0.1, -0.1, c(TRUE, FALSE)))
})

# One club "C" plays every 7 days from 2026-01-01. Player p is ever-present until 2026-02-26, then out.
mk_lineups <- function(p_dates, p_minutes = 90) {
  club <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = 7)
  mate <- data.frame(player_id = "mate", team_id = "C", match_id = paste0("m", seq_along(club)),
                     match_date = club, minutes_played = 90)
  pl <- data.frame(player_id = "p", team_id = "C", match_id = paste0("m", match(p_dates, club)),
                   match_date = p_dates, minutes_played = p_minutes)
  rbind(mate, pl)
}
club <- seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = 7)

test_that("a player out 60+ days while his club played 8+ matches is returning now", {
  lu <- mk_lineups(club[1:9])                       # last game 2026-02-26
  s <- returner_status(lu, as.Date("2026-05-15"))   # 78 days out, club played 11 since
  expect_true(s[player_id == "p", returning])
  expect_equal(s[player_id == "p", days_out], 78)
  expect_false(s[player_id == "mate", returning])
  s2 <- returner_status(lu, as.Date("2026-04-10"))  # 43 days out: not yet
  expect_false(s2[player_id == "p", returning])
})

test_that("a returner stays returning for 900 minutes, then not", {
  back <- club[club >= as.Date("2026-06-04")]       # back after a gap from 2026-02-26
  lu <- mk_lineups(c(club[1:9], back))
  s <- returner_status(lu, back[5] + 1)              # 5 games = 450 minutes since return
  expect_true(s[player_id == "p", returning])
  expect_equal(s[player_id == "p", minutes_since_return], 450)
  expect_equal(s[player_id == "p", days_out], as.numeric(as.Date("2026-06-04") - club[9]))
  s2 <- returner_status(lu, back[10] + 1)            # 900 minutes since return
  expect_false(s2[player_id == "p", returning])
})

test_that("a gap only counts if his club kept playing (8+ matches)", {
  lu <- mk_lineups(club[1:9])
  lu <- lu[!(lu$player_id == "mate" & lu$match_date > club[9] & lu$match_date < as.Date("2026-06-01")), ]
  # club played nothing from 2026-03-05 to 2026-05-28 (an off-season), so p is not counted as out
  expect_false(returner_status(lu, as.Date("2026-05-15"))[player_id == "p", returning])
})

test_that("returner_status is as-at: rows on or after as_of are ignored", {
  lu <- mk_lineups(c(club[1:9], club[25]))           # comes back on club[25]
  s <- returner_status(lu, club[25])                 # the day of the return: still out
  expect_true(s[player_id == "p", returning])
  expect_equal(s[player_id == "p", minutes_since_return], 0)
})
