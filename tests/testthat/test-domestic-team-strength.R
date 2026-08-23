# Tests for 12d_export_domestic_team_strength.R (panna#193).
#
# Domestic sibling of 12_export_wc2026_blog.R section 5: Tiento for every
# club in the current fixture window (domestic-league AND cup-only), not just
# the WC2026 48. Two things are genuinely new versus the WC exporter and get
# the most coverage here:
#   1. Elo seeding for teams with no tracked history (promoted from a
#      division panna doesn't scrape) -- the measured relegated-cohort rule.
#   2. Classifying every team as domestic-league vs cup-only, pinning a
#      dual-labelled team (domestic + continental cup this season) to its
#      domestic league.
#
# `.mocked_bindings` style: local_no_reload() stubs devtools::load_all()
# before sourcing, same helper test-wc2026-export.R and
# test-match-features-export.R already use (the script's own load_all() call
# would otherwise reload the package namespace mid-suite).

local_no_reload <- function(env = parent.frame()) {
  if (!requireNamespace("devtools", quietly = TRUE)) return(invisible(NULL))
  testthat::local_mocked_bindings(
    load_all = function(...) invisible(NULL), .package = "devtools", .env = env)
}

.dts_script <- function() {
  testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                      "12d_export_domestic_team_strength.R")
}

# --- Source-only helper: get the pure functions without any I/O fixtures ---
# `env` defaults to the CALLER's frame (the enclosing test_that() block) so
# local_no_reload()'s withr::defer un-mocks devtools::load_all() when that
# frame exits -- same lifetime local_no_reload() gets when test files call it
# directly, per test-wc2026-export.R's own comment on why this matters
# (load_all() reloading the package mid-suite breaks every later test file).
.dts_source_only <- function(env = parent.frame()) {
  script <- .dts_script()
  testthat::skip_if_not(file.exists(script), "12d script not found")
  local_no_reload(env)
  e <- new.env(parent = globalenv())
  assign("DOMESTIC_TEAM_STRENGTH_SOURCE_ONLY", TRUE, envir = e)
  source(script, local = e)
  e
}

# ============================================================
# .classify_team_leagues()
# ============================================================

test_that("dual-labelled team is pinned to its domestic league, not its cup", {
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league  = c("ENG", "ENG", "UCL"),
    season_end_year = c(2027, 2027, 2027),
    home_team = c("TeamA", "TeamB", "TeamA"),
    away_team = c("TeamB", "TeamA", "CupOnlyClub"),
    match_status = "Played"
  )
  out <- e$.classify_team_leagues(fr, domestic_codes = "ENG", cup_codes = "UCL")
  a <- out[team == "TeamA"]
  expect_equal(a$league, "ENG")
  expect_true(a$is_domestic_league)
  cup <- out[team == "CupOnlyClub"]
  expect_equal(cup$league, "UCL")
  expect_false(cup$is_domestic_league)
})

test_that("current season is picked PER LEAGUE, not globally", {
  # ENG's latest season_end_year (2028) is ahead of SCO's (2027) -- SCO's
  # roster must use ITS OWN latest season, not get dragged to 2028 and come
  # back empty.
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league  = c("ENG", "ENG", "SCO"),
    season_end_year = c(2027, 2028, 2027),
    home_team = c("TeamA", "TeamA", "ScoX"),
    away_team = c("TeamB", "TeamB", "ScoY"),
    match_status = "Played"
  )
  out <- e$.classify_team_leagues(fr, domestic_codes = c("ENG", "SCO"), cup_codes = "UCL")
  expect_equal(out[team == "TeamA"]$cur_sey, 2028)
  expect_equal(out[team == "ScoX"]$cur_sey, 2027)
  expect_true(all(c("ScoX", "ScoY") %in% out$team))
})

test_that("a team appearing in NO scope league is simply absent from the output", {
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league  = c("ENG", "WC"),
    season_end_year = c(2027, 2027),
    home_team = c("TeamA", "France"),
    away_team = c("TeamB", "Brazil"),
    match_status = "Played"
  )
  out <- e$.classify_team_leagues(fr, domestic_codes = "ENG", cup_codes = "UCL")
  expect_false(any(c("France", "Brazil") %in% out$team))
})

# ============================================================
# .relegated_cohort()
# ============================================================

test_that("relegated cohort is last season's teams minus this season's", {
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league = "ENG",
    season_end_year = c(2026, 2026, 2026, 2027, 2027),
    home_team = c("TeamA", "TeamB", "OldBad", "TeamA", "TeamB"),
    away_team = c("TeamB", "OldBad", "TeamA", "TeamB", "TeamA"),
    match_status = "Played"
  )
  out <- e$.relegated_cohort(fr, "ENG", cur_sey = 2027)
  expect_setequal(out, "OldBad")
})

test_that("no prior season at all -> empty relegated cohort (not an error)", {
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league = "ENG", season_end_year = 2027,
    home_team = "TeamA", away_team = "TeamB", match_status = "Played"
  )
  out <- e$.relegated_cohort(fr, "ENG", cur_sey = 2027)
  expect_equal(out, character(0))
})

# ============================================================
# .seed_missing_elo() -- the measured rule
# ============================================================

test_that("a promoted team is seeded at the relegated cohort's mean final Elo", {
  e <- .dts_source_only()
  fr <- data.table::data.table(
    league = c("ENG", "ENG", "ENG"),
    season_end_year = c(2026, 2026, 2027),
    home_team = c("TeamA", "OldBad", "TeamA"),
    away_team = c("OldBad", "TeamA", "TeamB"),
    match_status = "Played"
  )
  team_league <- data.table::data.table(
    team = c("TeamA", "OldBad", "NewClub"),
    league = "ENG", is_domestic_league = TRUE, cur_sey = 2027
  )
  final_elos <- c(TeamA = 1550, OldBad = 1420)  # NewClub absent -- no history
  out <- e$.seed_missing_elo(final_elos, team_league, fr)

  nc <- out[team == "NewClub"]
  expect_true(nc$elo_seeded)
  expect_equal(nc$seed_method, "relegated_cohort")
  expect_equal(nc$seed_n, 1L)
  expect_equal(nc$elo, 1420)  # mean of a single-team cohort is that team's own Elo

  earned <- out[team == "TeamA"]
  expect_false(earned$elo_seeded)
  expect_equal(earned$seed_method, "earned")
  expect_equal(earned$elo, 1550)
})

test_that("mean of a MULTI-team relegated cohort, not just the first", {
  e <- .dts_source_only()
  team_league <- data.table::data.table(
    team = "NewClub", league = "ENG", is_domestic_league = TRUE, cur_sey = 2027
  )
  fr <- data.table::data.table(
    league = "ENG", season_end_year = 2026,
    home_team = c("Bad1", "Bad2"), away_team = c("Bad2", "Bad1"),
    match_status = "Played"
  )
  final_elos <- c(Bad1 = 1400, Bad2 = 1500)
  out <- e$.seed_missing_elo(final_elos, team_league, fr)
  expect_equal(out$elo, 1450)  # mean(1400, 1500)
  expect_equal(out$seed_n, 2L)
})

test_that("no identifiable relegated cohort falls back to the league mean, and says so", {
  e <- .dts_source_only()
  # No prior ENG season at all in fr -- .relegated_cohort() returns empty.
  fr <- data.table::data.table(
    league = "ENG", season_end_year = 2027,
    home_team = "PeerA", away_team = "PeerB", match_status = "Played"
  )
  team_league <- data.table::data.table(
    team = c("PeerA", "PeerB", "NewClub"),
    league = "ENG", is_domestic_league = TRUE, cur_sey = 2027
  )
  final_elos <- c(PeerA = 1600, PeerB = 1400)  # NewClub absent
  expect_message(
    out <- e$.seed_missing_elo(final_elos, team_league, fr),
    "no identifiable relegated cohort"
  )
  nc <- out[team == "NewClub"]
  expect_equal(nc$seed_method, "league_mean_fallback")
  expect_equal(nc$elo, 1500)  # mean(1600, 1400)
})

test_that("a cup-only team with no history falls back to the cup's peer mean (no relegation concept)", {
  e <- .dts_source_only()
  team_league <- data.table::data.table(
    team = c("CupPeer", "CupNewbie"),
    league = "UCL", is_domestic_league = FALSE, cur_sey = 2027
  )
  fr <- data.table::data.table(
    league = "UCL", season_end_year = 2027,
    home_team = "CupPeer", away_team = "SomeoneElse", match_status = "Played"
  )
  final_elos <- c(CupPeer = 1650)  # CupNewbie absent
  out <- e$.seed_missing_elo(final_elos, team_league, fr)
  newbie <- out[team == "CupNewbie"]
  expect_true(newbie$elo_seeded)
  expect_equal(newbie$seed_method, "league_mean_fallback_cup")
  expect_equal(newbie$elo, 1650)
})

test_that("no cohort AND no peers leaves Elo NA with a loud warning (never silently 1500)", {
  e <- .dts_source_only()
  team_league <- data.table::data.table(
    team = "Lonely", league = "ZZZ", is_domestic_league = TRUE, cur_sey = 2027
  )
  fr <- data.table::data.table(
    league = character(0), season_end_year = numeric(0),
    home_team = character(0), away_team = character(0), match_status = character(0)
  )
  final_elos <- c(SomeoneUnrelated = 1500)
  expect_message(
    out <- e$.seed_missing_elo(final_elos, team_league, fr),
    "no relegated cohort AND no league-mean peers"
  )
  expect_true(is.na(out$elo))
  expect_true(out$elo_seeded)
  # Never falls back to the bare ELO_INITIAL default -- confirms it stayed NA,
  # not silently 1500.
  expect_false(isTRUE(out$elo == 1500))
})

# ============================================================
# .compute_tiento() -- global z-score pool
# ============================================================

test_that("tiento is a weighted sum of GLOBAL z-scores, and ranks correctly", {
  e <- .dts_source_only()
  dt <- data.table::data.table(
    team = c("Strong", "Mid", "Weak"),
    panna = c(2, 0, -2), epr = c(1, 0, -1), elo = c(1600, 1500, 1400), psr = c(0.5, 0, -0.5)
  )
  out <- e$.compute_tiento(dt, weights = c(panna = 0.4, epr = 0.2, elo = 0.3, psr = 0.1))
  # Strong is above the pool mean on every metric -> highest tiento, rank 1.
  expect_equal(out[team == "Strong"]$rank_tiento, 1L)
  expect_equal(out[team == "Weak"]$rank_tiento, 3L)
  expect_true(out[team == "Strong"]$tiento > out[team == "Mid"]$tiento)
  expect_true(out[team == "Mid"]$tiento > out[team == "Weak"]$tiento)
  # Mid sits exactly at the pool mean on every metric -> z=0 on all four -> tiento 0.
  expect_equal(out[team == "Mid"]$tiento, 0)
})

test_that("a constant column contributes zero (no NaN from sd=0), not an error", {
  e <- .dts_source_only()
  dt <- data.table::data.table(
    team = c("A", "B"), panna = c(1, 1), epr = c(1, -1), elo = c(1500, 1500), psr = c(0, 0)
  )
  out <- e$.compute_tiento(dt, weights = c(panna = 0.4, epr = 0.2, elo = 0.3, psr = 0.1))
  expect_false(anyNA(out$tiento))
})

# ============================================================
# .validate_domestic_strength() -- mutation-tested guards
# ============================================================

.dts_good_strength <- function() {
  data.table::data.table(
    team = c("A", "B", "C"),
    league = c("ENG", "ENG", "UCL"),
    panna = c(1, 0.5, -0.5), epr = c(1, 0.5, -0.5), psr = c(1, 0.5, -0.5),
    elo = c(1600, 1500, 1400),
    tiento = c(2, 0, -2),
    squad_n = c(20L, 18L, 22L)
  )
}

test_that("guard: every team must have a league", {
  e <- .dts_source_only()
  good <- .dts_good_strength()
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))

  bad <- data.table::copy(good)
  bad$league[2] <- NA_character_
  expect_error(e$.validate_domestic_strength(bad, min_squad_n = 5L), "no league")

  # Revert: passes again.
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))
})

test_that("guard: every team must have non-NA tiento", {
  e <- .dts_source_only()
  good <- .dts_good_strength()
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))

  bad <- data.table::copy(good)
  bad$tiento[1] <- NA_real_
  expect_error(e$.validate_domestic_strength(bad, min_squad_n = 5L), "NA tiento")

  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))
})

test_that("guard: elo must be non-NA (post-seeding) for every team", {
  e <- .dts_source_only()
  good <- .dts_good_strength()
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))

  bad <- data.table::copy(good)
  bad$elo[3] <- NA_real_
  expect_error(e$.validate_domestic_strength(bad, min_squad_n = 5L), "elo missing")

  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))
})

test_that("guard: a 2-player squad is a broken join, not a small squad", {
  e <- .dts_source_only()
  good <- .dts_good_strength()
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))

  bad <- data.table::copy(good)
  bad$squad_n[1] <- 2L
  expect_error(e$.validate_domestic_strength(bad, min_squad_n = 5L), "found no lineups for them")

  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))
})

test_that("guard: panna/epr/psr zero (or all-NA) for >=50% of teams refuses to publish", {
  e <- .dts_source_only()
  good <- .dts_good_strength()
  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))

  bad <- data.table::copy(good)
  bad$panna <- c(0, 0, -0.5)  # 2/3 zero
  expect_error(e$.validate_domestic_strength(bad, min_squad_n = 5L), "rating-join failure")

  # And the ALL-NA variant (a metric nobody joined at all) must not crash on
  # NaN from an empty na.rm mean -- it must still refuse, not error out with
  # "missing value where TRUE/FALSE needed" or silently pass.
  bad_na <- data.table::copy(good)
  bad_na$epr <- NA_real_
  expect_error(e$.validate_domestic_strength(bad_na, min_squad_n = 5L), "rating-join failure")

  expect_no_error(suppressMessages(e$.validate_domestic_strength(good, min_squad_n = 5L)))
})

# ============================================================
# End-to-end: source() the whole script against a small real fixture set
# ============================================================

.dts_full_fixture <- function(cache_dir, opta_dir, skills_dir) {
  dir.create(opta_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(skills_dir, recursive = TRUE, showWarnings = FALSE)

  # --- fixture_results: ENG (2 seasons, 1 relegation), SCO (1 season, no
  # prior season at all), UCL (cup-only + one dual-labelled team: TeamA) ---
  mk <- function(id, date, league, season, sey, home, away, hg, ag, status = "Played") {
    data.frame(match_id = id, match_date = as.character(date), match_status = status,
              league = league, season = season, season_end_year = sey,
              home_team = home, away_team = away,
              home_team_id = paste0("id_", home), away_team_id = paste0("id_", away),
              home_goals = hg, away_goals = ag, home_xg = NA_real_, away_xg = NA_real_,
              result = if (is.na(hg)) NA_character_ else if (hg > ag) "H" else if (hg == ag) "D" else "A",
              is_neutral_venue = 0L, stringsAsFactors = FALSE)
  }
  fr <- rbind(
    # ENG season 2025-2026 (sey=2026): TeamA/TeamB/TeamC/OldBad all played.
    mk("m1", "2025-09-01", "ENG", "2025-2026", 2026, "TeamA", "TeamB", 2, 1),
    mk("m2", "2025-09-08", "ENG", "2025-2026", 2026, "TeamB", "OldBad", 0, 2),
    mk("m3", "2025-09-15", "ENG", "2025-2026", 2026, "OldBad", "TeamC", 1, 1),
    mk("m4", "2025-09-22", "ENG", "2025-2026", 2026, "TeamC", "TeamA", 0, 3),
    # ENG season 2026-2027 (sey=2027): OldBad absent (relegated), NewClub
    # present only via an upcoming fixture (zero played history anywhere).
    mk("m6", "2026-08-20", "ENG", "2026-2027", 2027, "TeamA", "TeamB", 1, 1),
    mk("m7", "2026-08-27", "ENG", "2026-2027", 2027, "TeamB", "TeamC", 2, 0),
    mk("m8", "2026-09-03", "ENG", "2026-2027", 2027, "TeamC", "TeamA", 1, 2),
    mk("m5", "2026-10-05", "ENG", "2026-2027", 2027, "NewClub", "TeamA", NA, NA, status = "Fixture"),
    # SCO: only ONE tracked season -- no prior season exists at all, so
    # ScoTeamZ (promoted, zero history) must hit the league-mean fallback.
    mk("m12", "2026-08-10", "SCO", "2026-2027", 2027, "ScoTeamX", "ScoTeamY", 1, 0),
    mk("m13", "2026-08-24", "SCO", "2026-2027", 2027, "ScoTeamZ", "ScoTeamX", NA, NA, status = "Fixture"),
    # UCL: TeamA is dual-labelled (also ENG) -- must pin to ENG. CupOnlyClub2
    # has real UCL history (earned Elo); CupOnlyClub is cup-only with zero
    # history (must hit the cup's league-mean fallback, i.e. CupOnlyClub2).
    mk("m9", "2026-09-10", "UCL", "2026-2027", 2027, "TeamA", "CupOnlyClub2", 2, 0),
    mk("m11", "2026-10-01", "UCL", "2026-2027", 2027, "CupOnlyClub", "CupOnlyClub2", NA, NA, status = "Fixture")
  )
  saveRDS(fr, file.path(cache_dir, "01_fixture_results.rds"))

  # --- opta_lineups.parquet: 4 players per team, one match each ---
  teams <- c("TeamA", "TeamB", "TeamC", "NewClub", "ScoTeamX", "ScoTeamY",
            "ScoTeamZ", "CupOnlyClub", "CupOnlyClub2")
  lu_rows <- list()
  pid <- 1L
  for (tm in teams) {
    for (j in 1:4) {
      lu_rows[[length(lu_rows) + 1L]] <- data.frame(
        team_id = paste0("id_", tm), team_name = tm,
        match_id = paste0("lu_", tm), match_date = "2026-09-01T00:00:00Z",
        player_id = sprintf("p%03d", pid), player_name = sprintf("Player %d", pid),
        position = c("Goalkeeper", "Centre Back", "Central Midfielder", "Striker")[j],
        is_starter = TRUE, minutes_played = 90L, competition = "Domestic",
        stringsAsFactors = FALSE)
      pid <- pid + 1L
    }
  }
  lu <- do.call(rbind, lu_rows)
  arrow::write_parquet(lu, file.path(opta_dir, "opta_lineups.parquet"))

  # --- career_panna.parquet / opta_epr_weekly.parquet / seasonal PSR: every
  # player gets a distinct, nonzero rating so no zero-tripwire guard fires. ---
  player_ids <- unique(lu$player_id)
  n <- length(player_ids)
  cp <- data.frame(player_id = player_ids,
                   panna = seq(0.1, by = 0.05, length.out = n),
                   panna_offense = seq(0.2, by = 0.05, length.out = n),
                   panna_defense = seq(-0.1, by = -0.02, length.out = n),
                   total_minutes = 900, stringsAsFactors = FALSE)
  arrow::write_parquet(cp, file.path(opta_dir, "career_panna.parquet"))

  epr <- data.frame(player_id = player_ids, snapshot_date = "2026-09-15",
                    epr = seq(0.05, by = 0.02, length.out = n), stringsAsFactors = FALSE)
  arrow::write_parquet(epr, file.path(opta_dir, "opta_epr_weekly.parquet"))

  seasonal_psr <- data.frame(player_id = player_ids, season_end_year = 2027,
                             psr = seq(0.15, by = 0.03, length.out = n), stringsAsFactors = FALSE)
  saveRDS(list(seasonal_psr = seasonal_psr), file.path(skills_dir, "06_seasonal_ratings.rds"))

  invisible(NULL)
}

.run_12d <- function(cache_dir, opta_dir, skills_dir, publish = TRUE) {
  script <- .dts_script()
  testthat::skip_if_not(file.exists(script), "12d script not found")
  if (publish) {
    assign("publish_files",
          list(predictions_latest = character(0), blog_latest = character(0)),
          envir = globalenv())
  } else if (exists("publish_files", envir = globalenv())) {
    rm("publish_files", envir = globalenv())
  }
  old_opta_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(opta_dir)
  on.exit({
    if (!is.null(old_opta_dir)) opta_data_dir(old_opta_dir)
  }, add = TRUE)

  # Same "runner inside a closure reaching globalenv" trick 04b's test uses --
  # required for the script's `publish_files$... <<-` to find the accumulator.
  runner <- function() source(script, local = TRUE)
  environment(runner) <- list2env(
    list(cache_dir = cache_dir, script = script,
        domestic_codes = c("ENG", "SCO"), cup_codes = "UCL",
        as_of_domestic = as.Date("2026-10-15"),
        MIN_PLAUSIBLE_SQUAD_N = 3L,
        skills_cache_dir = skills_dir,
        opta_cache_dir = file.path(skills_dir, "..", "cache-opta")),
    parent = globalenv())
  runner()
  invisible(NULL)
}

test_that("12d exports Tiento for every team, seeds Elo correctly, and pins dual labels", {
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  .run_12d(cache_dir, opta_dir, skills_dir)

  out <- arrow::read_parquet(file.path(cache_dir, "team_strength.parquet"))
  expect_equal(nrow(out), 9L)
  expect_setequal(out$team, c("TeamA", "TeamB", "TeamC", "NewClub",
                              "ScoTeamX", "ScoTeamY", "ScoTeamZ",
                              "CupOnlyClub", "CupOnlyClub2"))

  # Dual-labelled team pinned to its domestic league.
  a <- out[out$team == "TeamA", ]
  expect_equal(a$league, "ENG")
  expect_true(a$is_domestic_league)

  # Cup-only team correctly flagged.
  cup <- out[out$team == "CupOnlyClub2", ]
  expect_equal(cup$league, "UCL")
  expect_false(cup$is_domestic_league)

  # Elo seeding fired for exactly the three teams with no tracked history.
  seeded <- out[out$elo_seeded, ]
  expect_setequal(seeded$team, c("NewClub", "ScoTeamZ", "CupOnlyClub"))
  expect_equal(seeded$seed_method[seeded$team == "NewClub"], "relegated_cohort")
  expect_equal(seeded$seed_method[seeded$team == "ScoTeamZ"], "league_mean_fallback")
  expect_equal(seeded$seed_method[seeded$team == "CupOnlyClub"], "league_mean_fallback_cup")
  # Every non-seeded team is "earned".
  expect_true(all(out$seed_method[!out$elo_seeded] == "earned"))

  # No NA anywhere in the columns a publish depends on.
  expect_false(anyNA(out$elo))
  expect_false(anyNA(out$tiento))
  expect_false(anyNA(out$league))

  # Squad size sane (4 players each in the fixture).
  expect_true(all(out$squad_n == 4L))

  # build_id stamped identically across the one file this step writes.
  expect_true(all(nzchar(out$build_id)))
})

test_that("12d registers the parquet + CSV for blog-latest publish, only at the very end", {
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  .run_12d(cache_dir, opta_dir, skills_dir)
  pf <- get("publish_files", envir = globalenv())
  expect_length(pf$blog_latest, 2L)
  expect_true(any(grepl("team_strength[.]parquet$", pf$blog_latest)))
  expect_true(any(grepl("team_strength[.]csv$", pf$blog_latest)))
  # It is blog data, not a predictions-pipeline diagnostic.
  expect_length(pf$predictions_latest, 0L)
})

test_that("12d aborts rather than shipping a team with an impossible squad (integration-level guard)", {
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  # Break the join for one team: drop its lineup rows entirely (opta_lineups
  # drift / team_name mismatch), same failure shape the guard exists to catch.
  lu <- arrow::read_parquet(file.path(opta_dir, "opta_lineups.parquet"), mmap = FALSE)
  lu <- lu[lu$team_name != "TeamB", ]
  arrow::write_parquet(lu, file.path(opta_dir, "opta_lineups.parquet"))

  expect_error(.run_12d(cache_dir, opta_dir, skills_dir, publish = FALSE),
              "found no lineups for them")
})


# --- Elo-constant drift guard -----------------------------------------------
# 12d re-derives final_elos by duplicating 03_team_rolling_features.R's
# compute_match_elos() call, because step 3 keeps only the per-match features it
# derives and never persists the team-state vector a relegated cohort needs.
#
# That duplication is unavoidable today, but "MUST be kept in sync by hand" is
# exactly the instruction that does not get followed -- this repo has already
# been bitten by a vendored file drifting, a header comment claiming coverage it
# no longer had, and a `keep` vector nothing read. If step 3's constants are
# retuned and 12d's literals are not, domestic Tiento silently uses a DIFFERENT
# Elo from the one the predictions use, and nothing fails.
#
# So assert it instead of asking for it.

test_that("12d's Elo literals still match 03_team_rolling_features.R", {
  step3 <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                               "03_team_rolling_features.R")
  step12d <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                 "12d_export_domestic_team_strength.R")
  skip_if_not(file.exists(step3) && file.exists(step12d), "pipeline scripts not present")

  # regmatches rather than a sub() backreference: this file is written through
  # shells that mangle backslash escapes, and a broken backreference silently
  # returns a control character that compares unequal to everything -- which
  # would make this guard fail for the wrong reason.
  grab <- function(path, pattern) {
    ln <- grep(pattern, readLines(path, warn = FALSE), value = TRUE)
    if (!length(ln)) return(NA_character_)
    m <- regmatches(ln[1], regexpr("[0-9]+", ln[1]))
    if (!length(m)) NA_character_ else m
  }

  expect_identical(grab(step3, "^ELO_K[[:space:]]*<-"),        "20")
  expect_identical(grab(step3, "^ELO_HOME_ADV[[:space:]]*<-"), "88")
  expect_identical(grab(step3, "^ELO_INITIAL[[:space:]]*<-"),  "1500")

  src <- readLines(step12d, warn = FALSE)
  call_line <- grep("k = .*home_advantage = .*initial_elo = ", src, value = TRUE)
  expect_length(call_line, 1L)
  # If this fails, step 3 was retuned and 12d was not. Update 12d's literals to
  # match -- do NOT relax this test. A mismatch means domestic Tiento is built
  # from a different Elo than the predictions use.
  expect_match(call_line, "k = 20", fixed = TRUE)
  expect_match(call_line, "home_advantage = 88", fixed = TRUE)
  expect_match(call_line, "initial_elo = 1500", fixed = TRUE)

  # use_venue_factor too: it has already been flipped once (FALSE -> TRUE), so
  # it is demonstrably a thing that changes. Both sites hardcode TRUE today.
  # Match the ARGUMENT, not any mention -- step 3 also names it in a comment
  # recording the FALSE -> TRUE change, which is itself the evidence that this
  # value moves.
  arg_of <- function(lines) {
    hit <- grep("^[[:space:]]*use_venue_factor[[:space:]]*=", lines, value = TRUE)
    if (!length(hit)) return(NA_character_)
    trimws(sub(".*=", "", hit[1]))
  }
  expect_identical(arg_of(readLines(step3, warn = FALSE)), arg_of(src))
})

test_that("12d asks build_team_expected_minutes for the same window the WC path does", {
  # The WC squads are built with lookback_days = 1095L; the library default is
  # 730L. Taking the default here would make a domestic Tiento mean something
  # different from a WC Tiento while the header claimed they matched -- which
  # is what review found on the first version.
  sq <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                            "announced_squads.R")
  step12d <- testthat::test_path("..", "..", "data-raw", "match-predictions-opta",
                                 "12d_export_domestic_team_strength.R")
  skip_if_not(file.exists(sq) && file.exists(step12d), "pipeline scripts not present")
  wc_windows <- unique(trimws(grep("lookback_days", readLines(sq, warn = FALSE), value = TRUE)))
  expect_true(all(grepl("1095", wc_windows)))
  expect_true(any(grepl("lookback_days = 1095L",
                        readLines(step12d, warn = FALSE), fixed = TRUE)))
})


# ---------------------------------------------------------------------------
# Regression tests for the 2026-08-23 team_name-join bug (found on the first
# live run of 12d). Opta reuses club names across competitions and countries,
# so a name-keyed squad join is wrong in BOTH directions at once:
#   too much -- "Liverpool" matched Liverpool FC + Liverpool FC Women (WSL) +
#     Liverpool FC Montevideo, giving a 168-player squad against a real 69,
#     about a quarter of it the women's team. Arsenal and Everton the same.
#   too little -- fixtures spell clubs "Le Mans FC" / "SV 07 Elversberg" where
#     opta_lineups says "Le Mans" / "Elversberg", so those squads came back
#     EMPTY and the min-squad guard blocked the publish.
# Both are fixed by keying the join on team_id. The min-squad guard only ever
# caught the second kind; the first inflates squad_n and looks healthier than
# a correct squad, which is why it needs a test rather than a threshold.
# ---------------------------------------------------------------------------

test_that("a second club sharing a team NAME does not merge into the squad", {
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  lu_path <- file.path(opta_dir, "opta_lineups.parquet")
  lu <- as.data.frame(arrow::read_parquet(lu_path, mmap = FALSE))
  # The "TeamA Women" case: same team_name, different club, different id.
  intruder <- lu[lu$team_id == "id_TeamA", ]
  intruder$team_id    <- "id_TeamA_WOMEN"
  intruder$match_id   <- paste0(intruder$match_id, "_w")
  intruder$player_id  <- paste0(intruder$player_id, "_w")
  arrow::write_parquet(rbind(lu, intruder), lu_path)

  .run_12d(cache_dir, opta_dir, skills_dir)
  out <- as.data.frame(arrow::read_parquet(file.path(cache_dir, "team_strength.parquet"),
                                           mmap = FALSE))
  # 4, not 8: the intruder's players must not be in TeamA's squad.
  expect_identical(out$squad_n[out$team == "TeamA"], 4L)
})

test_that("a club spelled differently in lineups than in fixtures still resolves", {
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  lu_path <- file.path(opta_dir, "opta_lineups.parquet")
  lu <- as.data.frame(arrow::read_parquet(lu_path, mmap = FALSE))
  # Fixtures call it "TeamC"; lineups call it "TeamC FC" -- the Le Mans shape.
  lu$team_name[lu$team_id == "id_TeamC"] <- "TeamC FC"
  arrow::write_parquet(lu, lu_path)

  .run_12d(cache_dir, opta_dir, skills_dir)
  out <- as.data.frame(arrow::read_parquet(file.path(cache_dir, "team_strength.parquet"),
                                           mmap = FALSE))
  # Full squad despite the spelling difference; pre-fix this was 0 and the
  # min-squad guard aborted the whole step.
  expect_identical(out$squad_n[out$team == "TeamC"], 4L)
})

test_that("the name-fallback path refuses to merge two clubs into one squad", {
  # The one route by which a merge can still happen: a team whose fixture rows
  # carry no team_id falls back to the name join. That path must abort, not
  # publish a merged squad.
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  fr <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
  fr$home_team_id[fr$home_team == "TeamA"] <- NA_character_
  fr$away_team_id[fr$away_team == "TeamA"] <- NA_character_
  saveRDS(fr, file.path(cache_dir, "01_fixture_results.rds"))

  lu_path <- file.path(opta_dir, "opta_lineups.parquet")
  lu <- as.data.frame(arrow::read_parquet(lu_path, mmap = FALSE))
  intruder <- lu[lu$team_id == "id_TeamA", ]
  intruder$team_id   <- "id_TeamA_WOMEN"
  intruder$match_id  <- paste0(intruder$match_id, "_w")
  intruder$player_id <- paste0(intruder$player_id, "_w")
  arrow::write_parquet(rbind(lu, intruder), lu_path)

  expect_error(.run_12d(cache_dir, opta_dir, skills_dir),
               "distinct clubs sharing a name")
})


test_that("an unresolved (present-but-unmatched) team_id does not fall back to a name match against a same-named club", {
  # Code-review finding on the team_id fix: a team_id that EXISTS on the
  # fixture side but is absent from opta_lineups.parquet (e.g. this club's
  # matches haven't been scraped yet -- 01_fixture_results.R's own "silent
  # split-identity risk") must NOT fall back to a plain team_name join. If it
  # did, and another real club shares the name, the wrong club's players
  # would be silently priced in -- and the >1-team_id structural guard cannot
  # catch it, because only ONE id is present in that slice. The safe behavior
  # is to treat it as squad_n = 0 and let the min-squad guard abort.
  skip_if_not_installed("arrow")
  local_no_reload()
  cache_dir <- withr::local_tempdir()
  opta_dir <- withr::local_tempdir()
  skills_dir <- withr::local_tempdir()
  .dts_full_fixture(cache_dir, opta_dir, skills_dir)
  on.exit(suppressWarnings(rm("publish_files", envir = globalenv())), add = TRUE)

  # Give TeamB a fixture-side id that opta_lineups has never heard of.
  fr <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
  fr$home_team_id[fr$home_team == "TeamB"] <- "id_TeamB_NOT_IN_LINEUPS"
  fr$away_team_id[fr$away_team == "TeamB"] <- "id_TeamB_NOT_IN_LINEUPS"
  saveRDS(fr, file.path(cache_dir, "01_fixture_results.rds"))

  # A DIFFERENT real club that happens to share TeamB's name -- the trap a
  # name-join fallback would fall into.
  lu_path <- file.path(opta_dir, "opta_lineups.parquet")
  lu <- as.data.frame(arrow::read_parquet(lu_path, mmap = FALSE))
  intruder <- lu[lu$team_id == "id_TeamC", ]
  intruder$team_id    <- "id_TeamB_IMPOSTOR"
  intruder$team_name  <- "TeamB"
  intruder$match_id   <- paste0(intruder$match_id, "_impostor")
  intruder$player_id  <- paste0(intruder$player_id, "_impostor")
  arrow::write_parquet(rbind(lu, intruder), lu_path)

  # squad_n < MIN_PLAUSIBLE_SQUAD_N (3L in this harness) must abort, NOT
  # silently publish the impostor's 4-player squad under TeamB.
  expect_error(.run_12d(cache_dir, opta_dir, skills_dir), "found no lineups for them")
})
