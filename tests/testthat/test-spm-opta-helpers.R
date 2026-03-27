# Tests for spm_opta.R shared helpers
# Covers: .ensure_player_id, .rename_opta_columns, .clean_numeric_na


# ===========================================================================
# .ensure_player_id
# ===========================================================================

test_that(".ensure_player_id creates player_id from player_name when missing", {
  dt <- data.table::data.table(
    player_name = c("Mohamed Salah", "Erling Haaland")
  )

  result <- .ensure_player_id(dt, "test")

  expect_true("player_id" %in% names(result))
  expect_equal(nrow(result), 2)
  # player_id should be derived from clean_player_name
  expect_equal(result$player_id, clean_player_name(c("Mohamed Salah", "Erling Haaland")))
})

test_that(".ensure_player_id preserves existing player_id", {
  dt <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("Mohamed Salah", "Erling Haaland")
  )

  result <- .ensure_player_id(dt, "test")

  expect_equal(result$player_id, c("p1", "p2"))
})

test_that(".ensure_player_id fills NA player_ids from player_name", {
  dt <- data.table::data.table(
    player_id = c("p1", NA_character_),
    player_name = c("Mohamed Salah", "Erling Haaland")
  )

  expect_warning(
    result <- .ensure_player_id(dt, "test_fn"),
    "1/2 rows have NA"
  )

  expect_equal(result$player_id[1], "p1")
  expect_equal(result$player_id[2], clean_player_name("Erling Haaland"))
})

test_that(".ensure_player_id no warning when all player_ids present", {
  dt <- data.table::data.table(
    player_id = c("p1", "p2"),
    player_name = c("A", "B")
  )

  expect_no_warning(.ensure_player_id(dt, "test"))
})


# ===========================================================================
# .rename_opta_columns
# ===========================================================================

test_that(".rename_opta_columns renames matching columns", {
  # Get a few known mappings from the real mapping
  mapping <- .get_opta_col_mapping()
  # Find a mapping where panna_name != opta_name
  diff_idx <- which(names(mapping) != unname(mapping))
  if (length(diff_idx) == 0) skip("No differing column names in mapping")

  opta_name <- unname(mapping[diff_idx[1]])
  panna_name <- names(mapping)[diff_idx[1]]

  dt <- data.table::data.table(x = 1:3)
  data.table::set(dt, j = opta_name, value = c(10, 20, 30))

  result <- .rename_opta_columns(dt)

  expect_true(panna_name %in% names(dt))
})

test_that(".rename_opta_columns returns matched columns", {
  dt <- data.table::data.table(
    minsPlayed = c(90, 45),
    player_name = c("A", "B")
  )

  result <- .rename_opta_columns(dt)

  # Should return a named vector of matched columns
  expect_true(is.character(result))
})

test_that(".rename_opta_columns handles no matching columns", {
  dt <- data.table::data.table(
    completely_unknown_col = c(1, 2)
  )

  result <- .rename_opta_columns(dt)

  # Should return empty (no matches)
  expect_equal(length(result), 0)
})


# ===========================================================================
# .clean_numeric_na
# ===========================================================================

test_that(".clean_numeric_na replaces NA with 0", {
  df <- data.frame(
    name = c("a", "b", "c"),
    goals = c(1, NA, 3),
    assists = c(NA, 2, NA)
  )

  result <- .clean_numeric_na(df, check_inf = FALSE)

  expect_equal(result$goals, c(1, 0, 3))
  expect_equal(result$assists, c(0, 2, 0))
  # Non-numeric columns untouched
  expect_equal(result$name, c("a", "b", "c"))
})

test_that(".clean_numeric_na replaces Inf when check_inf = TRUE", {
  df <- data.frame(
    x = c(1, Inf, -Inf, NA),
    y = c(NA, 2, 3, 4)
  )

  result <- .clean_numeric_na(df, check_inf = TRUE)

  expect_equal(result$x, c(1, 0, 0, 0))
  expect_equal(result$y, c(0, 2, 3, 4))
})

test_that(".clean_numeric_na does NOT replace Inf when check_inf = FALSE", {
  df <- data.frame(x = c(1, Inf, NA))

  result <- .clean_numeric_na(df, check_inf = FALSE)

  expect_equal(result$x[1], 1)
  expect_true(is.infinite(result$x[2]))  # Inf preserved
  expect_equal(result$x[3], 0)           # NA replaced
})

test_that(".clean_numeric_na returns unchanged df with no NAs", {
  df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
  result <- .clean_numeric_na(df)

  expect_equal(result, df)
})

test_that(".clean_numeric_na returns unchanged df with no numeric cols", {
  df <- data.frame(a = c("x", "y"), b = c("z", "w"))
  result <- .clean_numeric_na(df)

  expect_equal(result, df)
})

test_that(".clean_numeric_na warns when >5% of cells affected", {
  # 6 NAs out of 8 numeric cells = 75%
  df <- data.frame(
    x = c(NA, NA, NA, NA),
    y = c(NA, NA, 1, 2)
  )

  # cli::cli_warn uses rlang conditions
  expect_condition(
    .clean_numeric_na(df),
    class = "warning"
  )
})

test_that(".clean_numeric_na handles NaN (treated as NA)", {
  df <- data.frame(x = c(1, NaN, 3))

  result <- .clean_numeric_na(df, check_inf = FALSE)

  # NaN is NA in R: is.na(NaN) == TRUE
  expect_equal(result$x, c(1, 0, 3))
})
