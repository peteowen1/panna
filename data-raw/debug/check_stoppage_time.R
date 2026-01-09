# Check if FBref has explicit stoppage time data in HTML
setwd("C:/Users/peteo/OneDrive/Documents/pannaverse/panna")
library(rvest)
library(httr)
devtools::load_all()

# Fetch a real match page
url <- "https://fbref.com/en/matches/74125d47/Arsenal-Manchester-United-September-3-2023-Premier-League"
page <- fetch_match_page(url)

# Look for "stoppage" or "added" or "injury time" text
html_text <- as.character(page)

# Check for various stoppage time indicators
cat("=== Searching for stoppage time indicators ===\n\n")

# Search for "stoppage"
if (grepl("stoppage", html_text, ignore.case = TRUE)) {
  cat("Found 'stoppage' in HTML\n")
  matches <- gregexpr("stoppage", html_text, ignore.case = TRUE)
  cat("Occurrences:", length(unlist(matches)), "\n")
}

# Search for "added time" or "additional time"
if (grepl("added time|additional time", html_text, ignore.case = TRUE)) {
  cat("Found 'added time' or 'additional time' in HTML\n")
}

# Search for "injury time"
if (grepl("injury time", html_text, ignore.case = TRUE)) {
  cat("Found 'injury time' in HTML\n")
}

# Look for event headers like "Half Time" or "Full Time"
cat("\n=== Event Headers ===\n")
event_headers <- html_nodes(page, ".event_header")
for (h in event_headers) {
  cat(html_text(h), "\n")
}

# Look at the events timeline for any time-related patterns
cat("\n=== Sample Events with Minutes ===\n")
events <- html_nodes(page, "#events_wrap .event")
for (i in seq_len(min(5, length(events)))) {
  event_html <- as.character(events[i])
  # Extract the minute portion
  minute_div <- html_node(events[i], "div:first-child")
  if (!is.na(minute_div)) {
    cat("Event", i, "minute text:", html_text(minute_div), "\n")
  }
}

# Check if there's any explicit halftime stoppage info
cat("\n=== Looking for explicit stoppage time announcements ===\n")

# Look for patterns like "45+3" in the HTML
plus_times <- regmatches(html_text, gregexpr("[0-9]+\\+[0-9]+", html_text))[[1]]
if (length(plus_times) > 0) {
  cat("Found added time patterns:", unique(plus_times), "\n")
}

# Look at match info box
cat("\n=== Match Info/Scorebox Meta ===\n")
meta <- html_nodes(page, ".scorebox_meta")
if (length(meta) > 0) {
  cat(html_text(meta), "\n")
}
