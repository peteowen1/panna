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

test_that("to_opta_league errors on invalid format", {
  expect_error(to_opta_league("123!"), "Invalid league code")
  expect_error(to_opta_league("  "), "Invalid league code")
})

test_that("to_opta_league warns on unknown but valid-looking codes", {
  expect_warning(to_opta_league("MLS"), "not in hardcoded mappings")
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
    if (!is.null(old_dir)) opta_data_dir(old_dir)
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
    if (!is.null(old_dir)) opta_data_dir(old_dir)
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
    if (!is.null(old_dir)) opta_data_dir(old_dir)
  })

  expect_warning(
    seasons <- suggest_opta_seasons("NonExistent"),
    "not in hardcoded mappings"
  )
  expect_equal(length(seasons), 0)
})
