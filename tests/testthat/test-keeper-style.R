# Keeper distribution style (panna#429): club habit vs keeper's own effect, fitted as-of.

fit_keeper_style_asof <- panna:::fit_keeper_style_asof
add_kstyle <- panna:::.add_keeper_style_features

# Synthetic league: 24 clubs, 5 seasons, one starting keeper per club-season.
# value = 15 + club habit + keeper effect + noise. A third of keepers change
# club each season, which is what lets the model separate the two.
make_keeper_league <- function(seed = 1, n_clubs = 24, seasons = 2021:2025, n_matches = 30, last_shift = 0) {
  set.seed(seed)
  club_habit <- stats::setNames(stats::rnorm(n_clubs, 0, 5), sprintf("c%02d", seq_len(n_clubs)))
  n_keep <- n_clubs + 10L
  keeper_eff <- stats::setNames(stats::rnorm(n_keep, 0, 2), sprintf("k%02d", seq_len(n_keep)))
  at_club <- stats::setNames(names(keeper_eff)[seq_len(n_clubs)], names(club_habit))
  rows <- list()
  for (y in seasons) {
    if (y > seasons[1]) {
      movers <- sample(names(at_club), round(n_clubs / 3))
      at_club[movers] <- at_club[sample(movers)]               # swap keepers between these clubs
      bench <- setdiff(names(keeper_eff), at_club)
      new_in <- sample(names(at_club), 2)
      at_club[new_in] <- sample(bench, 2)                       # two new keepers join
    }
    for (cl in names(at_club)) {
      kp <- at_club[[cl]]
      # Optional style change in the last season for the first half of the clubs (a new coach).
      sh <- if (y == max(seasons) && match(cl, names(club_habit)) <= n_clubs / 2) last_shift else 0
      rows[[length(rows) + 1L]] <- data.table::data.table(
        match_id = paste(y, cl, seq_len(n_matches)), player_id = kp, team_id = cl,
        match_date = as.Date(sprintf("%d-08-01", y - 1L)) + 7L * seq_len(n_matches),
        league = "L1", season_end_year = y, position = "Goalkeeper", total_minutes = 90,
        long_balls_p90 = 15 + club_habit[[cl]] + sh + keeper_eff[[kp]] + stats::rnorm(n_matches, 0, 6))
    }
  }
  dt <- data.table::rbindlist(rows)
  # A few outfield rows, which must be left alone.
  dt <- rbind(dt, data.table::data.table(match_id = "o1", player_id = "o1", team_id = "c01",
                                         match_date = as.Date("2024-09-01"), league = "L1",
                                         season_end_year = 2025L, position = "Defender", total_minutes = 90,
                                         long_balls_p90 = 3))
  list(dt = dt, club_habit = club_habit, keeper_eff = keeper_eff)
}

test_that("as-of club style recovers the club habit, and the adjustment recovers the keeper (panna#429)", {
  sim <- make_keeper_league()
  st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", lambda = 0.00127, verbose = FALSE)
  expect_setequal(names(st), c("stat", "season_end_year", "team_id", "club_style"))
  # The first season has no earlier data, so no style for it.
  expect_false(2021L %in% st$season_end_year)
  last <- st[season_end_year == 2025L]
  expect_gt(stats::cor(last$club_style, sim$club_habit[last$team_id]), 0.8)

  dt <- add_kstyle(data.table::copy(sim$dt), st, verbose = FALSE)
  k <- dt[position == "Goalkeeper" & season_end_year >= 2022L,
          .(raw = mean(long_balls_p90), adj = mean(long_balls_kadj_p90)), by = player_id]
  truth <- sim$keeper_eff[k$player_id]
  # The adjusted value tracks the keeper's own effect better than the raw rate.
  expect_gt(stats::cor(k$adj, truth), stats::cor(k$raw, truth) + 0.2)
})

test_that("club style for season Y uses only seasons before Y", {
  sim <- make_keeper_league(seed = 2)
  a <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", verbose = FALSE)
  changed <- data.table::copy(sim$dt)
  changed[season_end_year >= 2024L, long_balls_p90 := long_balls_p90 + 50]  # rewrite the future
  b <- fit_keeper_style_asof(changed, stats = "long_balls_p90", verbose = FALSE)
  for (y in 2022:2024) {
    ay <- a[season_end_year == y][order(team_id)]; by <- b[season_end_year == y][order(team_id)]
    expect_equal(ay$club_style, by$club_style, tolerance = 1e-8, info = paste("season", y))
  }
  # ...and a later season does see the change.
  expect_false(isTRUE(all.equal(a[season_end_year == 2025L]$club_style, b[season_end_year == 2025L]$club_style)))
})

test_that(".add_keeper_style_features is additive and leaves outfield and first-season rows raw", {
  sim <- make_keeper_league(seed = 3)
  st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", verbose = FALSE)
  before <- data.table::copy(sim$dt)
  dt <- add_kstyle(data.table::copy(sim$dt), st, verbose = FALSE)
  expect_true("long_balls_kadj_p90" %in% names(dt))
  expect_equal(dt$long_balls_p90, before$long_balls_p90)            # raw column untouched
  expect_equal(dt[position == "Defender", long_balls_kadj_p90], 3)  # outfield keeps raw
  first <- dt[season_end_year == 2021L]
  expect_equal(first$long_balls_kadj_p90, first$long_balls_p90)     # no earlier season -> raw
  later <- dt[position == "Goalkeeper" & season_end_year == 2025L]
  expect_true(all(later$long_balls_kadj_p90 != later$long_balls_p90))
})

test_that("the in-season club term uses only earlier dates of the same season", {
  sim <- make_keeper_league(seed = 6)
  st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", verbose = FALSE)
  a <- add_kstyle(data.table::copy(sim$dt), st, verbose = FALSE)
  cut <- as.Date("2025-01-01")
  changed <- data.table::copy(sim$dt)
  changed[season_end_year == 2025L & match_date > cut, long_balls_p90 := long_balls_p90 + 40]
  b <- add_kstyle(changed, st, verbose = FALSE)   # same style table: only the in-season term can react
  sel <- a$season_end_year == 2025L & a$match_date <= cut & a$position == "Goalkeeper"
  expect_gt(sum(sel), 100)
  expect_equal(a$long_balls_kadj_p90[sel], b$long_balls_kadj_p90[sel], tolerance = 1e-10)
  # The first match of each club-season has no earlier matches, so no in-season shift.
  first <- a[position == "Goalkeeper" & season_end_year == 2025L, .SD[which.min(match_date)], by = team_id]
  off <- add_kstyle(data.table::copy(sim$dt), st, in_season_k = Inf, verbose = FALSE)
  first_off <- off[first[, .(match_id)], on = "match_id"]
  expect_equal(first$long_balls_kadj_p90, first_off$long_balls_kadj_p90, tolerance = 1e-10)
})

test_that("the in-season term removes a club's style change that earlier seasons could not see", {
  sim <- make_keeper_league(seed = 7, last_shift = 8)   # half the clubs go 8 long balls longer in 2025
  st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", verbose = FALSE)
  on  <- add_kstyle(data.table::copy(sim$dt), st, verbose = FALSE)
  off <- add_kstyle(data.table::copy(sim$dt), st, in_season_k = Inf, verbose = FALSE)
  shifted <- names(sim$club_habit)[1:12]
  late <- function(x) x[position == "Goalkeeper" & season_end_year == 2025L & match_date > as.Date("2024-12-01")]
  gap <- function(x) late(x)[, mean(long_balls_kadj_p90[team_id %in% shifted]) - mean(long_balls_kadj_p90[!team_id %in% shifted])]
  expect_gt(gap(off), 6)        # prior seasons cannot see the change
  expect_lt(abs(gap(on)), 3)    # the in-season term takes most of it out
})

test_that("the in-season term asks for match_date instead of guessing", {
  sim <- make_keeper_league(seed = 8)
  st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90", verbose = FALSE)
  expect_error(add_kstyle(data.table::copy(sim$dt)[, !"match_date"], st, verbose = FALSE), "match_date")
  expect_no_error(add_kstyle(data.table::copy(sim$dt)[, !"match_date"], st, in_season_k = Inf, verbose = FALSE))
})

test_that("the verbose messages run (the pipeline calls both functions with verbose = TRUE)", {
  sim <- make_keeper_league(seed = 5)
  expect_message(st <- fit_keeper_style_asof(sim$dt, stats = "long_balls_p90"), "keeper style long_balls_p90")
  expect_message(add_kstyle(data.table::copy(sim$dt), st), "long_balls_kadj_p90: club style applied")
})

test_that("fit_keeper_style_asof stops on missing inputs instead of returning nothing", {
  sim <- make_keeper_league(seed = 4)
  expect_error(fit_keeper_style_asof(sim$dt[, !"team_id"], stats = "long_balls_p90", verbose = FALSE), "team_id")
  expect_error(fit_keeper_style_asof(sim$dt, stats = "not_a_stat", verbose = FALSE), "None of the keeper")
  expect_error(fit_keeper_style_asof(sim$dt[position != "Goalkeeper"], stats = "long_balls_p90", verbose = FALSE),
               "No keeper rows")
})
