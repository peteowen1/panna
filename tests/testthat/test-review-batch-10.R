# Review batch 10 (R129, R130, R135): make silent failures loud.

test_that("R129: remote load_opta_table aborts (vb_error_absent) on a season with 0 rows", {
  local_mocked_bindings(
    query_remote_opta_parquet = function(...) data.frame(match_id = character(0)),
    to_opta_league = function(league) league
  )
  expect_error(
    panna:::load_opta_table("lineups", "EPL", "1999-2000", NULL, source = "remote"),
    class = "vb_error_absent"
  )
})

test_that("R129: remote load_opta_table still returns rows when the season exists", {
  local_mocked_bindings(
    query_remote_opta_parquet = function(...) data.frame(match_id = c("a", "b")),
    to_opta_league = function(league) league
  )
  out <- suppressMessages(
    panna:::load_opta_table("lineups", "EPL", "2024-2025", NULL, source = "remote")
  )
  expect_equal(nrow(out), 2L)
})

test_that("R130: opta_data_dir does not cache a not-yet-existing fallback path", {
  env <- panna:::.opta_env
  had <- exists("opta_dir", envir = env, inherits = FALSE)
  old <- if (had) get("opta_dir", envir = env) else NULL
  if (had) rm("opta_dir", envir = env)
  withr::defer({
    if (exists("opta_dir", envir = env, inherits = FALSE)) rm("opta_dir", envir = env)
    if (had) assign("opta_dir", old, envir = env)
  })

  missing_base <- file.path(withr::local_tempdir(), "no_such_pannadata")
  local_mocked_bindings(pannadata_dir = function(...) missing_base)
  first <- opta_data_dir()
  expect_false(dir.exists(first))
  expect_false(exists("opta_dir", envir = env, inherits = FALSE))

  # A later call must see a changed base dir, not a pinned stale one.
  real_base <- withr::local_tempdir()
  dir.create(file.path(real_base, "opta"))
  local_mocked_bindings(pannadata_dir = function(...) real_base)
  expect_equal(opta_data_dir(),
               normalizePath(file.path(real_base, "opta")))
})

test_that("R135: canonical-id map groups accented and plain spellings of a name", {
  skip_if_not_installed("stringi")
  lu <- data.table::data.table(
    player_id   = c(rep("main", 20), rep("alt", 1), "other"),
    player_name = c(rep("José Muñoz", 20), "Jose Munoz", "Ana-Lee Smith"),
    team_name   = c(rep("Club A", 20), "Club A", "Club B")
  )
  canon <- panna:::build_player_id_canonical_map(lu)
  expect_equal(canon[player_id == "alt", canonical_id], "main")
  # Hyphens must survive normalisation (the old regex stripped them).
  expect_equal(canon[player_id == "other", canonical_id], "other")
})
