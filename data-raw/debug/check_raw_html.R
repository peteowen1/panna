# Check what HTML we're actually getting

devtools::load_all()

# Reset session first
reset_fbref_session()

url <- "https://fbref.com/en/matches/74125d47/Arsenal-Manchester-United-September-3-2023-Premier-League"

cat("Making direct request...\n")
Sys.sleep(2)  # Small delay after reset

response <- httr::GET(
  url,
  httr::add_headers(.headers = get_fbref_headers()),
  httr::timeout(30),
  handle = get_fbref_session()
)

cat("Status code:", httr::status_code(response), "\n")
cat("Final URL:", response$url, "\n")

# Get HTML content
html <- httr::content(response, "text", encoding = "UTF-8")

# Check the title
title_match <- regmatches(html, regexpr("<title>[^<]+</title>", html))
cat("Page title:", title_match, "\n")

# Check for key elements
cat("\nChecking for key elements:\n")
cat("  scorebox:", grepl('class="scorebox"', html), "\n")
cat("  events_wrap:", grepl('id="events_wrap"', html), "\n")
cat("  stats_ tables:", grepl('id="stats_', html), "\n")

# If we got scorebox, test the parsing
if (grepl('class="scorebox"', html)) {
  cat("\n=== TESTING PARSING ===\n")
  page <- rvest::read_html(html)

  metadata <- extract_match_metadata(page, url)
  cat("Home team:", metadata$home_team, "\n")
  cat("Away team:", metadata$away_team, "\n")
  cat("Home manager:", metadata$home_manager, "\n")
  cat("Venue:", metadata$venue, "\n")

  events <- scrape_match_events(page, url)
  if (!is.null(events)) {
    cat("Events found:", nrow(events), "\n")
    cat("\nFirst 5 events:\n")
    print(head(events[, c("minute", "added_time", "event_type", "player")], 5))
    cat("\nEvents from minute 89+:\n")
    late_events <- events[events$minute >= 89, c("minute", "added_time", "event_type", "player")]
    print(late_events)
  }
} else {
  cat("\n*** Did not get match page - got redirected ***\n")
  # Show first bit of body
  body_start <- regmatches(html, regexpr("<body[^>]*>", html))
  cat("Body tag:", body_start, "\n")
}
