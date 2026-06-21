# Tests for Opta loader functions

# -- to_opta_league --

test_that("to_opta_league resolves direct Opta codes", {
  expect_equal(to_opta_league("EPL"), "EPL")
  expect_equal(to_opta_league("La_Liga"), "La_Liga")
  expect_equal(to_opta_league("Bundesliga"), "Bundesliga")
  expect_equal(to_opta_league("UCL"), "UCL")
})

test_that("to_opta_league resolves panna aliases", {
  expect_equal(to_opta_league("ENG"), "EPL")
  expect_equal(to_opta_league("ESP"), "La_Liga")
  expect_equal(to_opta_league("GER"), "Bundesliga")
  expect_equal(to_opta_league("ITA"), "Serie_A")
  expect_equal(to_opta_league("FRA"), "Ligue_1")
})

test_that("to_opta_league is case-insensitive for panna aliases", {
  expect_equal(to_opta_league("eng"), "EPL")
  expect_equal(to_opta_league("Eng"), "EPL")
  expect_equal(to_opta_league("esp"), "La_Liga")
  expect_equal(to_opta_league("ger"), "Bundesliga")
})

test_that("to_opta_league is case-insensitive for Opta codes", {
  expect_equal(to_opta_league("epl"), "EPL")
  expect_equal(to_opta_league("Epl"), "EPL")
  expect_equal(to_opta_league("la_liga"), "La_Liga")
  expect_equal(to_opta_league("BUNDESLIGA"), "Bundesliga")
  expect_equal(to_opta_league("ucl"), "UCL")
})

test_that("to_opta_league errors on invalid format", {
  expect_error(to_opta_league("123!"), "Unknown league code")
  expect_error(to_opta_league("  "), "Unknown league code")
})

test_that("to_opta_league warns on unknown but valid-looking codes when catalog unavailable", {
  # Mock catalog as unavailable to test offline fallback path
  local_mocked_bindings(download_opta_catalog = function(...) stop("offline"))
  # Deliberately-unmapped code: matches the valid-code regex but isn't in
  # OPTA_LEAGUES. (Was "MLS" until MLS was added as a real league — don't use a
  # real code here, or this test re-breaks when that league gets mapped.)
  expect_warning(to_opta_league("ZZ_Unmapped_League"), "not in hardcoded mappings")
})


# -- validate_parquet_file --

test_that("validate_parquet_file returns FALSE for missing file", {
  expect_false(validate_parquet_file("/nonexistent/path/file.parquet"))
})

test_that("validate_parquet_file returns FALSE for empty file", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  file.create(tmp)
  expect_false(validate_parquet_file(tmp))
})

test_that("validate_parquet_file returns FALSE for truncated file", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  # Write only the header magic, not footer
  con <- file(tmp, "wb")
  writeBin(charToRaw("PAR1"), con)
  writeBin(raw(100), con)
  close(con)
  expect_false(validate_parquet_file(tmp))
})

test_that("validate_parquet_file returns TRUE for valid parquet magic", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  con <- file(tmp, "wb")
  writeBin(charToRaw("PAR1"), con)
  writeBin(raw(100), con)  # some content
  writeBin(charToRaw("PAR1"), con)
  close(con)
  expect_true(validate_parquet_file(tmp))
})


# -- list_opta_leagues local with mock directory --

test_that("list_opta_leagues local returns correct structure from filesystem", {
  tmp <- withr::local_tempdir()
  ps_dir <- file.path(tmp, "player_stats", "EPL")
  dir.create(ps_dir, recursive = TRUE)
  file.create(file.path(ps_dir, "2023-2024.parquet"))
  file.create(file.path(ps_dir, "2024-2025.parquet"))

  # Use the setter function to point opta_data_dir at tmp
  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  result <- list_opta_leagues(source = "local")
  expect_true(is.data.frame(result))
  expect_true("code" %in% names(result))
  expect_true("n_seasons" %in% names(result))
  expect_equal(nrow(result), 1)
  expect_equal(result$code, "EPL")
  expect_equal(result$n_seasons, 2)
  expect_equal(result$panna_alias, "ENG")
})


# -- suggest_opta_seasons local --

test_that("suggest_opta_seasons returns seasons from local filesystem", {
  tmp <- withr::local_tempdir()
  # Default table_type is "match_events"
  me_dir <- file.path(tmp, "match_events", "EPL")
  dir.create(me_dir, recursive = TRUE)
  file.create(file.path(me_dir, "2023-2024.parquet"))
  file.create(file.path(me_dir, "2024-2025.parquet"))

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  seasons <- suggest_opta_seasons("EPL")
  expect_true(is.character(seasons))
  expect_equal(length(seasons), 2)
  expect_equal(seasons[1], "2024-2025")
  expect_equal(seasons[2], "2023-2024")
})

test_that("suggest_opta_seasons returns empty for nonexistent league", {
  tmp <- withr::local_tempdir()
  me_dir <- file.path(tmp, "match_events")
  dir.create(me_dir, recursive = TRUE)

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  # Mock catalog as unavailable to test offline fallback path
  local_mocked_bindings(download_opta_catalog = function(...) stop("offline"))
  expect_warning(
    seasons <- suggest_opta_seasons("NonExistent"),
    "not in hardcoded mappings"
  )
  expect_equal(length(seasons), 0)
})


# -- load_opta_skills --

test_that("load_opta_skills local reads parquet file", {
  tmp <- withr::local_tempdir()

  # Create a minimal skills parquet
  skills_df <- data.frame(
    player_id = c("p1", "p2", "p3"),
    player_name = c("Player A", "Player B", "Player C"),
    primary_position = c("Striker", "Midfielder", "Defender"),
    season_end_year = c(2025, 2025, 2024),
    weighted_90s = c(10.5, 15.2, 8.1),
    total_minutes = c(945, 1368, 729),
    goals_p90 = c(0.5, 0.2, 0.05),
    tackles_won_p90 = c(0.3, 1.5, 2.8)
  )
  arrow::write_parquet(skills_df, file.path(tmp, "opta_skills.parquet"))

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  result <- load_opta_skills(source = "local")
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true("player_name" %in% names(result))
  expect_true("goals_p90" %in% names(result))
})

test_that("load_opta_skills filters by season", {
  tmp <- withr::local_tempdir()

  skills_df <- data.frame(
    player_id = c("p1", "p1"),
    player_name = c("Player A", "Player A"),
    primary_position = c("Striker", "Striker"),
    season_end_year = c(2024, 2025),
    weighted_90s = c(8.0, 12.0),
    total_minutes = c(720, 1080),
    goals_p90 = c(0.4, 0.6)
  )
  arrow::write_parquet(skills_df, file.path(tmp, "opta_skills.parquet"))

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  result <- load_opta_skills(season = 2025, source = "local")
  expect_equal(nrow(result), 1)
  expect_equal(result$season_end_year, 2025)
})

test_that("load_opta_skills selects columns", {
  tmp <- withr::local_tempdir()

  skills_df <- data.frame(
    player_id = "p1",
    player_name = "Player A",
    season_end_year = 2025,
    goals_p90 = 0.5,
    tackles_won_p90 = 1.2
  )
  arrow::write_parquet(skills_df, file.path(tmp, "opta_skills.parquet"))

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  result <- load_opta_skills(columns = c("player_name", "goals_p90"), source = "local")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("player_name", "goals_p90"))
})

test_that("load_opta_skills errors for missing local file", {
  tmp <- withr::local_tempdir()

  old_dir <- tryCatch(opta_data_dir(), error = function(e) NULL)
  opta_data_dir(tmp)
  withr::defer({
    if (!is.null(old_dir)) {
      opta_data_dir(old_dir)
    } else {
      rm("opta_dir", envir = panna:::.opta_env, inherits = FALSE)
    }
  })

  expect_error(load_opta_skills(source = "local"), "not found")
})


# --- download_opta_catalog TTL freshness check -----------------------------

test_that("download_opta_catalog treats stale local file as expired", {
  # Simulates the scenario where a scrape refreshed the remote catalog but
  # the local copy is older than the TTL — stale local should be bypassed.

  tmp_dir <- withr::local_tempdir()
  local_path <- file.path(tmp_dir, "opta-catalog.json")

  # Plant a local catalog that's technically valid but 48 hours old.
  stale_catalog <- list(
    competitions = list(EPL = list(seasons = c("2018-2019"))),
    panna_aliases = list(ENG = "EPL")
  )
  jsonlite::write_json(stale_catalog, local_path, auto_unbox = TRUE)
  # Back-date mtime 48 hours.
  Sys.setFileTime(local_path, Sys.time() - as.difftime(48, units = "hours"))

  # Fresh catalog that the "download" branch would return.
  fresh_catalog <- list(
    competitions = list(EPL = list(seasons = c("2018-2019", "2024-2025"))),
    panna_aliases = list(ENG = "EPL")
  )

  # Stub opta_data_dir to our tmp path; stub pb_download to install fresh.
  local_mocked_bindings(
    opta_data_dir = function(...) tmp_dir,
    .package = "panna"
  )
  local_mocked_bindings(
    pb_download = function(file, repo, tag, dest, overwrite, ...) {
      jsonlite::write_json(fresh_catalog, file.path(dest, file), auto_unbox = TRUE)
      invisible(NULL)
    },
    .package = "piggyback"
  )

  # Clear any session cache so the test is deterministic.
  .opta_remote_env <- asNamespace("panna")$.opta_remote_env
  if (exists("opta_catalog", envir = .opta_remote_env)) {
    rm("opta_catalog", envir = .opta_remote_env)
  }

  # TTL 6h: 48h-old local is stale → should return fresh catalog.
  result <- download_opta_catalog(max_age_hours = 6)
  expect_equal(unlist(result$competitions$EPL$seasons),
               c("2018-2019", "2024-2025"))

  # After download, local file's mtime should have been refreshed too
  # (so next session doesn't re-download on the next call).
  new_mtime_age_hours <- as.numeric(
    difftime(Sys.time(), file.info(local_path)$mtime, units = "hours")
  )
  expect_lt(new_mtime_age_hours, 1)
})

test_that("download_opta_catalog accepts fresh local file within TTL", {

  tmp_dir <- withr::local_tempdir()
  local_path <- file.path(tmp_dir, "opta-catalog.json")

  fresh_catalog <- list(
    competitions = list(EPL = list(seasons = c("2024-2025"))),
    panna_aliases = list(ENG = "EPL")
  )
  jsonlite::write_json(fresh_catalog, local_path, auto_unbox = TRUE)
  # mtime is "now" — well within the 6h default TTL.

  local_mocked_bindings(
    opta_data_dir = function(...) tmp_dir,
    .package = "panna"
  )

  # Clear cache so it's forced to hit the local-file path.
  .opta_remote_env <- asNamespace("panna")$.opta_remote_env
  if (exists("opta_catalog", envir = .opta_remote_env)) {
    rm("opta_catalog", envir = .opta_remote_env)
  }

  # Would fail if the "download" path ran (no pb_download stub here).
  result <- download_opta_catalog(max_age_hours = 6)
  expect_equal(unlist(result$competitions$EPL$seasons), "2024-2025")
})

test_that("enrich_match_stats_with_xmetrics surfaces gaps and fail-fasts", {
  # Missing join keys -> returns input unchanged with a warning (no silent drop).
  ms_nokey <- data.frame(player_id = "p1", goals_p90 = 1, stringsAsFactors = FALSE)
  expect_warning(
    out <- enrich_match_stats_with_xmetrics(ms_nokey, verbose = FALSE),
    "missing league/season/match_id/player_id"
  )
  expect_equal(nrow(out), 1L)

  # A league-season with no local xmetrics_bymatch file => total miss. With a
  # finite fail_if_missing_frac the helper must STOP rather than silently train
  # an xG-blind model (the regression this guard prevents). Library default
  # (Inf) only warns.
  ms <- data.frame(
    player_id = "p1", player_name = "P", match_id = "m1",
    league = "__nonexistent_league__", season = "1900-1901",
    goals_p90 = 1, stringsAsFactors = FALSE
  )
  expect_error(
    suppressWarnings(
      enrich_match_stats_with_xmetrics(ms, verbose = FALSE,
                                       fail_if_missing_frac = 0.5)),
    "Refusing to proceed|No per-match xMetrics"
  )
  # Inf default: warns and returns unchanged (no xG columns added).
  out2 <- suppressWarnings(
    enrich_match_stats_with_xmetrics(ms, verbose = FALSE))
  expect_false("xg_per90" %in% names(out2))
})
