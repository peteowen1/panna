# Tests for R/spadl_conversion.R qualifier parsing (H2-OG-WP, H2-PEN)

test_that("parse_opta_qualifiers anchors qualifier keys and ignores matching VALUES", {
  dt <- data.table::data.table(
    qualifier_json = c(
      '{"9":null}',                 # qualifier 9 (penalty) as a KEY -> TRUE
      '{"55":"9"}',                 # "9" only as a qualifier VALUE -> FALSE
      '{"108":null,"55":"9","9":"1"}', # both -- key 9 present -> TRUE
      '{"28":null}',                 # qualifier 28 (own goal) as a KEY -> TRUE
      '{"9":"28"}',                  # qualifier 9 as a KEY (value "28") -> is_penalty TRUE, is_own_goal FALSE ("28" is only a value here)
      NA_character_
    )
  )

  result <- parse_opta_qualifiers(dt)

  expect_equal(result$is_penalty, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_equal(result$is_own_goal, c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE))
})

test_that("convert_opta_to_spadl flips no attribution itself but correctly tags is_own_goal/is_penalty per event (H2-OG-WP / H2-PEN)", {
  # Row 2: t2's player scores an OWN GOAL (qualifier 28).
  # Row 3: t1's player scores a PENALTY (qualifier 9 as a KEY) -- also carries
  #        a related-event qualifier (55) whose VALUE is "9", which an
  #        unanchored `grepl('"9"', ...)` would also match.
  events <- data.frame(
    match_id    = rep("m1", 4),
    event_id    = 1:4,
    type_id     = c(1L, 16L, 16L, 1L),
    team_id     = c("t1", "t2", "t1", "t1"),
    player_id   = c("p1", "p2", "p3", "p1"),
    player_name = c("A", "B", "C", "A"),
    minute      = c(1L, 10L, 20L, 30L),
    second      = c(0L, 0L, 0L, 0L),
    x           = c(50, 3, 78, 50),
    y           = c(50, 50, 50, 50),
    end_x       = c(60, 3, 100, 60),
    end_y       = c(50, 50, 50, 50),
    outcome     = c(1L, 1L, 1L, 1L),
    period_id   = c(1L, 1L, 1L, 1L),
    qualifier_json = c(
      '{"1":null}',
      '{"28":null}',
      '{"9":null,"55":"9"}',
      '{"1":null}'
    ),
    stringsAsFactors = FALSE
  )

  spadl <- convert_opta_to_spadl(events)

  expect_true(all(c("is_own_goal", "is_penalty") %in% names(spadl)))

  og_row <- spadl[spadl$original_event_id == 2, ]
  expect_equal(nrow(og_row), 1L)
  expect_true(og_row$is_own_goal)
  expect_false(og_row$is_penalty)
  expect_equal(og_row$team_id, "t2")  # scorer's own team, per Opta convention

  pen_row <- spadl[spadl$original_event_id == 3, ]
  expect_equal(nrow(pen_row), 1L)
  expect_true(pen_row$is_penalty)
  expect_false(pen_row$is_own_goal)

  other_rows <- spadl[spadl$original_event_id %in% c(1, 4), ]
  expect_false(any(other_rows$is_own_goal))
  expect_false(any(other_rows$is_penalty))
})
