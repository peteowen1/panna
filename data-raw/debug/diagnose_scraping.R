# Diagnostic script - see what's actually on the FBref page

devtools::load_all()

# Try an older match that definitely has full stats
url <- "https://fbref.com/en/matches/867aeb40/Arsenal-Manchester-United-September-3-2023-Premier-League"

cat("Fetching page...\n")
page <- fetch_match_page(url)

if (is.null(page)) {
  cat("ERROR: Failed to fetch page\n")
  quit()
}

cat("Page fetched successfully\n\n")

# Check for scorebox
cat("=== SCOREBOX ===\n")
scorebox <- rvest::html_node(page, ".scorebox")
if (is.na(scorebox)) {
  cat("NO SCOREBOX FOUND\n")
} else {
  cat("Scorebox found\n")
  # Team names
  teams <- rvest::html_text(rvest::html_nodes(scorebox, "strong a"))
  cat("Teams found:", length(teams), "-", paste(teams, collapse = " vs "), "\n")

  # Scores
  scores <- rvest::html_text(rvest::html_nodes(scorebox, ".score"))
  cat("Scores found:", length(scores), "-", paste(scores, collapse = " - "), "\n")
}

# Check for team nodes
cat("\n=== TEAM NODES ===\n")
home_node <- rvest::html_node(page, "#sb_team_0")
away_node <- rvest::html_node(page, "#sb_team_1")
cat("Home team node (#sb_team_0):", if (is.na(home_node)) "NOT FOUND" else "FOUND", "\n")
cat("Away team node (#sb_team_1):", if (is.na(away_node)) "NOT FOUND" else "FOUND", "\n")

if (!is.na(home_node)) {
  datapoints <- rvest::html_nodes(home_node, ".datapoint")
  cat("Home datapoints:", length(datapoints), "\n")
  for (dp in datapoints) {
    cat("  -", trimws(rvest::html_text(dp)), "\n")
  }
}

# Check scorebox_meta
cat("\n=== SCOREBOX META ===\n")
meta <- rvest::html_node(page, ".scorebox_meta")
if (is.na(meta)) {
  cat("NO SCOREBOX_META FOUND\n")
} else {
  cat("Scorebox meta found\n")
  divs <- rvest::html_nodes(meta, "div")
  cat("Divs in meta:", length(divs), "\n")
  for (i in seq_along(divs)) {
    text <- trimws(rvest::html_text(divs[[i]]))
    if (nchar(text) > 0 && nchar(text) < 200) {
      cat("  ", i, ":", substr(text, 1, 100), "\n")
    }
  }
}

# Check for lineup/formation
cat("\n=== LINEUPS ===\n")
lineup_a <- rvest::html_node(page, ".lineup#a")
lineup_b <- rvest::html_node(page, ".lineup#b")
cat("Home lineup (.lineup#a):", if (is.na(lineup_a)) "NOT FOUND" else "FOUND", "\n")
cat("Away lineup (.lineup#b):", if (is.na(lineup_b)) "NOT FOUND" else "FOUND", "\n")

if (!is.na(lineup_a)) {
  header <- rvest::html_node(lineup_a, "th")
  if (!is.na(header)) {
    cat("Home lineup header:", rvest::html_text(header), "\n")
  }
}

# Check for events
cat("\n=== EVENTS ===\n")
events_wrap <- rvest::html_node(page, "#events_wrap")
if (is.na(events_wrap)) {
  cat("NO EVENTS_WRAP FOUND\n")
} else {
  cat("Events wrap found\n")
  events <- rvest::html_nodes(events_wrap, ".event")
  cat("Event nodes:", length(events), "\n")

  if (length(events) > 0) {
    cat("First 3 events:\n")
    for (i in seq_len(min(3, length(events)))) {
      event_class <- rvest::html_attr(events[[i]], "class")
      icon <- rvest::html_node(events[[i]], ".event_icon")
      icon_class <- if (!is.na(icon)) rvest::html_attr(icon, "class") else "no icon"
      text <- substr(trimws(rvest::html_text(events[[i]])), 1, 80)
      cat("  ", i, ": class=", event_class, " icon=", icon_class, "\n")
      cat("      text:", text, "\n")
    }
  }
}

# Check for tables
cat("\n=== TABLES ===\n")
tables <- rvest::html_nodes(page, "table")
table_ids <- rvest::html_attr(tables, "id")
table_ids <- table_ids[!is.na(table_ids)]
cat("Tables with IDs:", length(table_ids), "\n")
if (length(table_ids) > 0) {
  cat("First 10 table IDs:\n")
  for (id in head(table_ids, 10)) {
    cat("  -", id, "\n")
  }
}

# Now test the actual extraction function
cat("\n=== TESTING extract_match_metadata() ===\n")
metadata <- extract_match_metadata(page, url)
cat("Returned columns:", paste(names(metadata), collapse = ", "), "\n\n")
cat("Values:\n")
for (col in names(metadata)) {
  val <- metadata[[col]]
  cat(sprintf("  %s: %s\n", col, if (is.na(val)) "NA" else as.character(val)))
}

cat("\n=== TESTING scrape_match_events() ===\n")
events <- scrape_match_events(page, url)
if (is.null(events)) {
  cat("Events returned NULL\n")
} else {
  cat("Events rows:", nrow(events), "\n")
  if (nrow(events) > 0) {
    cat("First 3 events:\n")
    print(head(events, 3))
  }
}

cat("\n=== DONE ===\n")
