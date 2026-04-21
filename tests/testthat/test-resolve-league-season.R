# Tests for resolve_league_season() — the league/season routing helper shared
# by 10b and 10c blog exports.

test_that("resolve_league_season returns domestic season unchanged for non-tournament leagues", {
  expect_equal(resolve_league_season("ENG",  "2013-2014"), "2013-2014")
  expect_equal(resolve_league_season("ESP",  "2023-2024"), "2023-2024")
  expect_equal(resolve_league_season("UCL",  "2013-2014"), "2013-2014")
  expect_equal(resolve_league_season("UECL", "2022-2023"), "2022-2023")
})

test_that("resolve_league_season maps summer tournament to preceding domestic season", {
  mock_avail <- c("2018 Russia", "2014 Brazil", "2010 South Africa", "2006 Germany")
  # Stub list_opta_seasons so the test doesn't hit the network/catalog.
  testthat::local_mocked_bindings(list_opta_seasons = function(...) mock_avail)

  expect_equal(resolve_league_season("WC", "2013-2014"), "2014 Brazil")
  expect_equal(resolve_league_season("WC", "2017-2018"), "2018 Russia")
  expect_equal(resolve_league_season("WC", "2009-2010"), "2010 South Africa")
})

test_that("resolve_league_season handles bare-year tournament (EURO 2020 pan-European)", {
  mock_avail <- c("2024 Germany", "2020", "2016 France", "2012 Poland-Ukraine")
  testthat::local_mocked_bindings(list_opta_seasons = function(...) mock_avail)

  # EURO 2020 is labelled just "2020" with no country because it was pan-European.
  expect_equal(resolve_league_season("EURO", "2019-2020"), "2020")
  expect_equal(resolve_league_season("EURO", "2023-2024"), "2024 Germany")
})

test_that("resolve_league_season returns NULL when no tournament runs that year", {
  mock_avail <- c("2018 Russia", "2014 Brazil")
  testthat::local_mocked_bindings(list_opta_seasons = function(...) mock_avail)

  expect_null(resolve_league_season("WC", "2018-2019"))
  expect_null(resolve_league_season("WC", "2019-2020"))
})

test_that("resolve_league_season rejects prefix collisions on the year match", {
  # "2024 Germany" should not match t_year = 202 (prefix) or 20 (shorter prefix).
  mock_avail <- c("2024 Germany", "2020")
  testthat::local_mocked_bindings(list_opta_seasons = function(...) mock_avail)

  # Synthetic domestic season strings that resolve to 202 or 20 (shouldn't happen
  # in practice, but exercises the regex anchor).
  expect_null(resolve_league_season("EURO", "0201-0202"))   # t_year = 202
  expect_null(resolve_league_season("EURO", "0019-0020"))   # t_year = 20
})

test_that("resolve_league_season honours custom tournament_leagues", {
  mock_avail <- c("2022 Poland")
  testthat::local_mocked_bindings(list_opta_seasons = function(...) mock_avail)

  # Treat UCL as a tournament — should try to remap.
  expect_equal(
    resolve_league_season("UCL", "2021-2022", tournament_leagues = c("UCL")),
    "2022 Poland"
  )
  # With default tournament_leagues, UCL passes through unchanged.
  expect_equal(resolve_league_season("UCL", "2021-2022"), "2021-2022")
})

test_that("resolve_league_season returns NULL for malformed domestic_season", {
  testthat::local_mocked_bindings(list_opta_seasons = function(...) c("2020"))

  # No trailing 4-digit year → can't extract t_year → NULL.
  expect_null(resolve_league_season("WC", "not-a-season"))
  expect_null(resolve_league_season("WC", ""))
})
