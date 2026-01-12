# Debug why minutes aren't always extracted

devtools::load_all()
reset_fbref_session()

url <- "https://fbref.com/en/matches/74125d47/Arsenal-Manchester-United-September-3-2023-Premier-League"

cat("Fetching page...\n")
page <- fetch_match_page(url)

events_wrap <- rvest::html_node(page, "#events_wrap")
event_nodes <- rvest::html_nodes(events_wrap, ".event")

cat("Found", length(event_nodes), "events\n\n")

# Check a few events in detail
cat("=== DETAILED BYTE ANALYSIS ===\n\n")

for (i in c(1, 4, 16)) {  # One that fails, one that works, one with added time
  if (i > length(event_nodes)) next

  event_node <- event_nodes[[i]]
  first_div <- rvest::html_node(event_node, "div")
  minute_text <- rvest::html_text(first_div)

  cat("Event", i, ":\n")
  cat("  Raw text:", minute_text, "\n")

  # Show character codes for first 20 chars
  chars <- strsplit(substr(minute_text, 1, 30), "")[[1]]
  cat("  Char codes: ")
  for (ch in chars) {
    code <- as.integer(charToRaw(ch))
    if (length(code) == 1) {
      cat(sprintf("%02x ", code))
    } else {
      cat(sprintf("[%s] ", paste(sprintf("%02x", code), collapse="")))
    }
  }
  cat("\n")

  # Try different regex patterns
  patterns <- list(
    "current" = "([0-9]+)\\+?([0-9]*)['\\u2019]",
    "rsquor_literal" = "([0-9]+)\\+?([0-9]*)&rsquor;",
    "any_after_num" = "([0-9]+)\\+?([0-9]*)[^0-9]"
  )

  for (name in names(patterns)) {
    pat <- patterns[[name]]
    match <- regmatches(minute_text, regexpr(pat, minute_text))
    cat(sprintf("  Pattern %-20s: %s\n", name, if (length(match) > 0) match else "NO MATCH"))
  }
  cat("\n")
}

# Check if it's the right quote character
cat("=== CHECKING QUOTE CHARACTER ===\n")
# The Unicode right single quote is U+2019
cat("U+2019 (right single quote):", "\u2019", "- bytes:", paste(sprintf("%02x", as.integer(charToRaw("\u2019"))), collapse=" "), "\n")
cat("Regular apostrophe:", "'", "- bytes:", paste(sprintf("%02x", as.integer(charToRaw("'"))), collapse=" "), "\n")
