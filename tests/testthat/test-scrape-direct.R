# Tests for direct FBref scraping functions

test_that("get_fbref_headers returns valid headers", {
  headers <- get_fbref_headers()

  expect_type(headers, "character")
  expect_true("User-Agent" %in% names(headers))
  expect_true("Accept" %in% names(headers))
  expect_true("sec-ch-ua" %in% names(headers))
})


test_that("make_match_filename creates valid filenames", {
  # Function now only takes fbref_id (hierarchical structure uses dir path for league/season)
  filename <- make_match_filename("abc12345")

  expect_equal(filename, "abc12345.rds")

  # Handles special characters
  filename_special <- make_match_filename("abc/123")
  expect_false(grepl("/", filename_special))
})


test_that("cache directory functions work", {
  # Get base cache dir (now uses pannadata_dir which defaults differently)
  cache_dir <- get_fbref_match_cache_dir(create = FALSE)
  expect_type(cache_dir, "character")

  # Get table-specific cache dir - hierarchical structure
  cache_dir_summary <- get_fbref_match_cache_dir("summary", "ENG", "2024-2025", create = FALSE)
  expect_true(grepl("summary", cache_dir_summary))
  expect_true(grepl("ENG", cache_dir_summary))
  expect_true(grepl("2024-2025", cache_dir_summary))
})


test_that("save and load match table work correctly", {
  skip_on_cran()

  # Create test data
  test_data <- data.frame(
    player = c("Test Player 1", "Test Player 2"),
    goals = c(1, 0),
    stringsAsFactors = FALSE
  )

  # Save - hierarchical structure: {table_type}/{league}/{season}/{id}.rds
  save_match_table(test_data, "TEST", "2024-2025", "test1234", "summary")

  # Load
  loaded <- load_match_table("TEST", "2024-2025", "test1234", "summary")

  expect_equal(test_data, loaded)

  # Check cache detection - requires metadata with tables_available field
  # For now just check that nonexistent returns FALSE
  expect_false(is_match_cached("TEST", "2024-2025", "nonexist", "summary"))

  # Cleanup - remove the whole test directory structure
  cache_dir <- get_fbref_match_cache_dir("summary", "TEST", "2024-2025", create = FALSE)
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
  }
  # Also try to clean up parent directories if empty
  parent_dir <- dirname(cache_dir)
  if (dir.exists(parent_dir) && length(list.files(parent_dir)) == 0) {
    unlink(parent_dir, recursive = TRUE)
  }
})


test_that("list_cached_matches returns correct structure", {
  # Should return empty data frame when no cache
  cached <- list_cached_matches("nonexistent_type")

  expect_s3_class(cached, "data.frame")
  expect_equal(names(cached), c("league", "season", "fbref_id"))
  expect_equal(nrow(cached), 0)
})


test_that("fetch_match_page validates URL", {
  expect_error(
    fetch_match_page("https://google.com"),
    "Invalid FBref match URL"
  )
})


test_that("scrape_fbref_matches validates inputs", {
  expect_error(
    scrape_fbref_matches(character(0), "ENG", "2024-2025"),
    "No match URLs provided"
  )

  expect_error(
    scrape_fbref_matches("https://fbref.com/en/matches/abc123/test"),
    "league and season are required"
  )
})


# Tests for extract_match_metadata() with mock HTML
test_that("extract_match_metadata extracts basic fields from mock HTML", {
  # Create minimal mock HTML with scorebox
  mock_html <- '
  <html>
  <div class="scorebox">
    <strong><a href="/en/squads/abc12345/Team-A">Team A</a></strong>
    <strong><a href="/en/squads/def67890/Team-B">Team B</a></strong>
    <div class="score">2</div>
    <div class="score">1</div>
  </div>
  <table id="stats_11111111_summary"><tbody></tbody></table>
  <table id="stats_22222222_summary"><tbody></tbody></table>
  </html>
  '

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Team-A-Team-B-January-15-2025-Test-League"

  metadata <- extract_match_metadata(page, url)

  expect_s3_class(metadata, "data.frame")
  expect_equal(nrow(metadata), 1)
  expect_equal(metadata$fbref_id, "abc12345")
  expect_equal(metadata$home_team, "Team A")
  expect_equal(metadata$away_team, "Team B")
  expect_equal(metadata$home_score, 2)
  expect_equal(metadata$away_score, 1)
})


test_that("extract_match_metadata extracts extended fields from mock HTML", {
  # Create mock HTML with extended metadata
  # Note: Player IDs must be valid 8-character hex strings (a-f, 0-9)
  mock_html <- '
  <html>
  <div class="scorebox">
    <div class="scorebox_team" id="sb_team_0">
      <strong><a href="/en/squads/abc12345/Team-A">Team A</a></strong>
      <div class="datapoint"><strong>Manager</strong>: John Smith</div>
      <div class="datapoint"><strong>Captain</strong>: <a href="/en/players/aabbcc11/Player-A">Player A</a></div>
    </div>
    <div class="scorebox_team" id="sb_team_1">
      <strong><a href="/en/squads/def67890/Team-B">Team B</a></strong>
      <div class="datapoint"><strong>Manager</strong>: Jane Doe</div>
      <div class="datapoint"><strong>Captain</strong>: <a href="/en/players/ddeeff22/Player-B">Player B</a></div>
    </div>
    <div class="score">2</div>
    <div class="score">1</div>
    <div class="scorebox_meta">
      <div><strong><small>Venue</small></strong>: <small>Test Stadium, Test City</small></div>
      <div><strong><small>Attendance</small></strong>: <small>50,000</small></div>
      <div><strong><small>Officials</small></strong>: <small>
        <span>Mike Ref (Referee)</span> ·
        <span>Jim AR (AR1)</span> ·
        <span>Bob AR (AR2)</span>
      </small></div>
      <div><span class="venuetime" data-venue-time="15:00" data-venue-epoch="1700000000">3:00 PM</span></div>
    </div>
  </div>
  <div class="lineup" id="a"><table><tr><th>Team A (4-3-3)</th></tr></table></div>
  <div class="lineup" id="b"><table><tr><th>Team B (4-4-2)</th></tr></table></div>
  <table id="stats_11111111_summary"><tbody></tbody></table>
  <table id="stats_22222222_summary"><tbody></tbody></table>
  </html>
  '

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Team-A-Team-B-January-15-2025-Test-League"

  metadata <- extract_match_metadata(page, url)

  # Test new fields
  expect_equal(metadata$home_manager, "John Smith")
  expect_equal(metadata$away_manager, "Jane Doe")
  expect_equal(metadata$home_captain, "Player A")
  expect_equal(metadata$away_captain, "Player B")
  expect_equal(metadata$home_captain_id, "aabbcc11")
  expect_equal(metadata$away_captain_id, "ddeeff22")
  expect_equal(metadata$venue, "Test Stadium, Test City")
  expect_equal(metadata$attendance, 50000L)
  expect_equal(metadata$referee, "Mike Ref")
  expect_equal(metadata$ar1, "Jim AR")
  expect_equal(metadata$ar2, "Bob AR")
  expect_equal(metadata$kickoff_time, "15:00")
  expect_equal(metadata$kickoff_epoch, 1700000000L)
  expect_equal(metadata$home_formation, "4-3-3")
  expect_equal(metadata$away_formation, "4-4-2")
})


# Tests for scrape_match_events()
test_that("scrape_match_events extracts goals from mock HTML", {
  # Create mock HTML with events
  # Note: Player IDs must be valid 8-character hex strings (a-f, 0-9)
  mock_html <- '
  <html>
  <div id="events_wrap">
    <div class="event a">
      <div>19\' <br/><small><span>1:0</span></small></div>
      <div>
        <div class="event_icon goal"></div>
        <div>
          <div><a href="/en/players/aa111111/Goal-Scorer">Goal Scorer</a></div>
          <small>Assist: <a href="/en/players/bb222222/Assist-Man">Assist Man</a></small>
        </div>
      </div>
    </div>
    <div class="event b">
      <div>35\' <br/><small><span>1:1</span></small></div>
      <div>
        <div class="event_icon goal"></div>
        <div><div><a href="/en/players/cc333333/Away-Scorer">Away Scorer</a></div></div>
      </div>
    </div>
  </div>
  </html>
  '

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Test-Match"

  events <- scrape_match_events(page, url)

  expect_s3_class(events, "data.frame")
  expect_equal(nrow(events), 2)
  expect_equal(events$fbref_id[1], "abc12345")

  # First goal (home)
  expect_equal(events$minute[1], 19L)
  expect_equal(events$event_type[1], "goal")
  expect_true(events$is_home[1])
  expect_equal(events$player[1], "Goal Scorer")
  expect_equal(events$player_id[1], "aa111111")
  expect_equal(events$secondary_player[1], "Assist Man")
  expect_equal(events$secondary_player_id[1], "bb222222")
  expect_equal(events$score_home[1], 1L)
  expect_equal(events$score_away[1], 0L)

  # Second goal (away)
  expect_equal(events$minute[2], 35L)
  expect_equal(events$event_type[2], "goal")
  expect_false(events$is_home[2])
  expect_equal(events$player[2], "Away Scorer")
  expect_equal(events$score_home[2], 1L)
  expect_equal(events$score_away[2], 1L)
})


test_that("scrape_match_events handles cards and substitutions", {
  # Note: Player IDs must be valid 8-character hex strings (a-f, 0-9)
  mock_html <- '
  <html>
  <div id="events_wrap">
    <div class="event a">
      <div>45+2\'</div>
      <div>
        <div class="event_icon yellow_card"></div>
        <div><div><a href="/en/players/ca1d1111/Yellow-Player">Yellow Player</a></div></div>
      </div>
    </div>
    <div class="event b">
      <div>60\'</div>
      <div>
        <div class="event_icon substitute_in"></div>
        <div>
          <div><a href="/en/players/a1b11111/Sub-On-Player">Sub On Player</a></div>
          <small>for <a href="/en/players/a1b22222/Sub-Off-Player">Sub Off Player</a></small>
        </div>
      </div>
    </div>
    <div class="event a">
      <div>78\'</div>
      <div>
        <div class="event_icon yellow_red_card"></div>
        <div><div><a href="/en/players/ed111111/Sent-Off-Player">Sent Off Player</a></div></div>
      </div>
    </div>
  </div>
  </html>
  '

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Test-Match"

  events <- scrape_match_events(page, url)

  expect_equal(nrow(events), 3)

  # Yellow card with added time
  expect_equal(events$minute[1], 45L)
  expect_equal(events$added_time[1], 2L)
  expect_equal(events$event_type[1], "yellow_card")
  expect_equal(events$player[1], "Yellow Player")

  # Substitution
  expect_equal(events$minute[2], 60L)
  expect_equal(events$event_type[2], "sub_on")
  expect_equal(events$player[2], "Sub On Player")
  expect_equal(events$secondary_player[2], "Sub Off Player")
  expect_equal(events$secondary_player_id[2], "a1b22222")

  # Second yellow (yellow_red_card)
  expect_equal(events$minute[3], 78L)
  expect_equal(events$event_type[3], "yellow_red_card")
  expect_equal(events$player[3], "Sent Off Player")
})


test_that("scrape_match_events returns NULL for page without events", {
  mock_html <- '<html><div id="content">No events here</div></html>'

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Test-Match"

  events <- scrape_match_events(page, url)

  expect_null(events)
})


test_that("scrape_match_events handles penalty goals", {
  # Note: Player IDs must be valid 8-character hex strings (a-f, 0-9)
  mock_html <- '
  <html>
  <div id="events_wrap">
    <div class="event a">
      <div>85\' <br/><small><span>2:1</span></small></div>
      <div>
        <div class="event_icon penalty_goal"></div>
        <div><div><a href="/en/players/ae111111/Penalty-Taker">Penalty Taker</a></div></div>
      </div>
    </div>
  </div>
  </html>
  '

  page <- rvest::read_html(mock_html)
  url <- "https://fbref.com/en/matches/abc12345/Test-Match"

  events <- scrape_match_events(page, url)

  expect_equal(nrow(events), 1)
  expect_equal(events$event_type[1], "penalty_goal")
  expect_equal(events$player[1], "Penalty Taker")
})


# Integration tests (skip by default, require network)
test_that("fetch_match_page works with real URL", {
  skip_on_cran()
  skip_if_offline()
  skip("Manual test - requires network access to FBref")

  url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"
  page <- fetch_match_page(url)

  expect_false(is.null(page))
  expect_s3_class(page, "xml_document")
})


test_that("full scrape workflow works", {
  skip_on_cran()
  skip_if_offline()
  skip("Manual test - requires network access to FBref")

  url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"

  data <- scrape_fbref_matches(
    url,
    league = "ESP",
    season = "2025-2026",
    delay = 5,
    use_cache = FALSE,
    verbose = FALSE
  )

  expect_type(data, "list")
  expect_true("metadata" %in% names(data))
  expect_true("summary" %in% names(data))
  expect_true("events" %in% names(data))

  # Check metadata (basic fields)
  expect_s3_class(data$metadata, "data.frame")
  expect_equal(data$metadata$fbref_id, "12c8079e")

  # Check metadata (extended fields)
  expect_true("home_manager" %in% names(data$metadata))
  expect_true("away_manager" %in% names(data$metadata))
  expect_true("home_captain" %in% names(data$metadata))
  expect_true("venue" %in% names(data$metadata))
  expect_true("attendance" %in% names(data$metadata))
  expect_true("referee" %in% names(data$metadata))
  expect_true("home_formation" %in% names(data$metadata))
  expect_true("away_formation" %in% names(data$metadata))

  # Check summary stats
  if (!is.null(data$summary)) {
    expect_s3_class(data$summary, "data.frame")
    expect_true("player" %in% names(data$summary))
    expect_true("team" %in% names(data$summary))
  }

  # Check events
  if (!is.null(data$events)) {
    expect_s3_class(data$events, "data.frame")
    expect_true("minute" %in% names(data$events))
    expect_true("event_type" %in% names(data$events))
    expect_true("player" %in% names(data$events))
    expect_true("is_home" %in% names(data$events))
  }
})
