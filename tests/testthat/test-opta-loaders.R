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
  expect_error(to_opta_league("123!"), "Invalid league code")
  expect_error(to_opta_league("  "), "Invalid league code")
})

test_that("to_opta_league warns on unknown but valid-looking codes", {
  expect_warning(to_opta_league("MLS"), "not in hardcoded mappings")
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
