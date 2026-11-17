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

test_that("a corrupt minutes value or a duplicated row does not end the window early (review of #497)", {
  back <- club[club >= as.Date("2026-06-04")]
  lu <- mk_lineups(c(club[1:9], back))
  lu$minutes_played[lu$player_id == "p" & lu$match_date == back[1]] <- 99999     # corrupt value: capped at 120
  s <- returner_status(lu, back[5] + 1)
  expect_true(s[player_id == "p", returning])
  expect_equal(s[player_id == "p", minutes_since_return], 120 + 4 * 90)
  lu2 <- mk_lineups(c(club[1:9], back))
  lu2 <- rbind(lu2, lu2[lu2$player_id == "p" & lu2$match_date == back[2], ])     # same (match, player) twice
  expect_equal(returner_status(lu2, back[5] + 1)[player_id == "p", minutes_since_return], 450)
})

test_that("a transfer debut after 60+ days is not a return; coming back to his own club is (as the validity tests)", {
  lu <- mk_lineups(club[1:9])                                  # p plays for C until 2026-02-26
  newc <- data.frame(player_id = "q", team_id = "N", match_id = paste0("n", seq_along(club)),
                     match_date = club, minutes_played = 90)   # club N also plays weekly
  deb <- data.frame(player_id = "p", team_id = "N", match_id = paste0("n", 23), match_date = club[23], minutes_played = 90)
  s <- returner_status(rbind(lu, newc, deb), club[23] + 1)     # debut for N on club[23]
  expect_false(s[player_id == "p", returning])
  expect_true(is.na(s[player_id == "p", days_out]))
  back <- data.frame(player_id = "p", team_id = "C", match_id = paste0("m", 23), match_date = club[23], minutes_played = 90)
  s2 <- returner_status(rbind(lu, back), club[23] + 1)         # back at C instead
  expect_true(s2[player_id == "p", returning])
})
