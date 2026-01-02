# Test FBref API call
# Testing direct fetch of match page

library(httr)
library(rvest)

# Target URL from browser
url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"
url <- "https://fbref.com/en/matches/74a8527b/Levante-Real-Madrid-September-23-2025-La-Liga"

cat("=== Method 1: httr with browser headers ===\n")

# Set headers to mimic browser request
headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
  "Accept-Language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "sec-ch-ua" = '"Google Chrome";v="143", "Chromium";v="143"',
  "sec-ch-ua-mobile" = "?0",
  "sec-ch-ua-platform" = '"Windows"',
  "sec-fetch-dest" = "document",
  "sec-fetch-mode" = "navigate",
  "sec-fetch-site" = "same-origin",
  "Referer" = "https://fbref.com/en/comps/12/schedule/La-Liga-Scores-and-Fixtures"
)

cat("Fetching URL:", url, "\n\n")

# Make request
response <- GET(
  url,
  add_headers(.headers = headers),
  timeout(30)
)

cat("Status Code:", status_code(response), "\n")
cat("Content Type:", headers(response)$`content-type`, "\n")
cat("Content Length:", length(content(response, "raw")), "bytes\n\n")

# Parse HTML content
html_content <- content(response, "text", encoding = "UTF-8")
page <- read_html(html_content)

# Extract page title
title <- page %>% html_node("title") %>% html_text()
cat("Page Title:", title, "\n\n")

# Look for match info
scorebox <- page %>% html_node(".scorebox")
if (!is.na(scorebox)) {
  cat("Found scorebox element\n")

  # Team names
  teams <- scorebox %>% html_nodes("strong a") %>% html_text()
  cat("Teams:", paste(teams, collapse = " vs "), "\n")

  # Scores
  scores <- scorebox %>% html_nodes(".score") %>% html_text()
  cat("Scores:", paste(scores, collapse = " - "), "\n")
}

# Look for lineup tables
lineup_tables <- page %>% html_nodes("table")
cat("\nFound", length(lineup_tables), "tables on page\n")

# List table IDs
table_ids <- page %>% html_nodes("table") %>% html_attr("id")
table_ids <- table_ids[!is.na(table_ids)]
cat("Table IDs found:\n")
print(table_ids)

# Try to get summary stats table
cat("\n--- Looking for player stats ---\n")
stats_tables <- page %>% html_nodes("table.stats_table")
cat("Found", length(stats_tables), "stats tables\n")

if (length(stats_tables) > 0) {
  # Get first stats table (player summary)
  first_table <- stats_tables[[1]]
  table_id <- html_attr(first_table, "id")
  cat("First stats table ID:", table_id, "\n")

  # Try to parse it - need to handle multi-row headers
  df <- html_table(first_table, fill = TRUE, header = TRUE)
  cat("Raw dimensions:", nrow(df), "rows x", ncol(df), "cols\n\n")

  # The table has 2 header rows, first row is actual data after headers
  # Let's look at the raw structure
  cat("First 5 rows of raw table:\n")
  print(df[1:min(5, nrow(df)), 1:min(10, ncol(df))])
}

# Parse summary table properly
cat("\n--- Parsing Player Summary Stats ---\n")
summary_table <- page %>% html_node("#stats_9024a00a_summary")

if (!is.na(summary_table)) {
  # Get header rows
  header_rows <- summary_table %>% html_nodes("thead tr")
  cat("Header rows:", length(header_rows), "\n")

  # Get second header row (actual column names)
  col_names <- header_rows[[2]] %>% html_nodes("th") %>% html_text()
  cat("Column names:", paste(head(col_names, 15), collapse = ", "), "...\n\n")

  # Get data rows
  data_rows <- summary_table %>% html_nodes("tbody tr")
  cat("Data rows:", length(data_rows), "\n")

  # Parse into data frame
  parse_row <- function(row) {
    cells <- row %>% html_nodes("th, td") %>% html_text()
    return(cells)
  }

  row_data <- lapply(data_rows, parse_row)
  df <- do.call(rbind, row_data)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- col_names[1:ncol(df)]

  cat("\nParsed player data (first 5 rows, key columns):\n")
  key_cols <- c("Player", "Pos", "Min", "Gls", "Ast", "Sh", "SoT", "Touches", "Tkl", "Int")
  available_cols <- key_cols[key_cols %in% names(df)]
  print(df[1:min(5, nrow(df)), available_cols])

  cat("\nAll columns available:\n")
  print(names(df))
}

# ============================================
cat("\n\n=== Method 2: worldfootballR approach ===\n")

# Try using worldfootballR's internal method
library(worldfootballR)

cat("Testing fb_match_summary for the same match...\n")

# The match URL
match_url <- "https://fbref.com/en/matches/12c8079e/Girona-Rayo-Vallecano-August-15-2025-La-Liga"

result <- tryCatch({
  data <- fb_match_summary(match_url)
  cat("Success! Got", nrow(data), "rows\n")
  if (nrow(data) > 0) {
    cat("Columns:", paste(names(data), collapse = ", "), "\n")
    print(head(data))
  }
  data
}, error = function(e) {
  cat("Error:", e$message, "\n")
  NULL
})

# Also try getting match lineups
cat("\n--- Testing fb_match_lineups ---\n")
lineups <- tryCatch({
  data <- fb_match_lineups(match_url)
  cat("Success! Got", nrow(data), "rows\n")
  if (nrow(data) > 0) {
    cat("Columns:", paste(head(names(data), 10), collapse = ", "), "...\n")
  }
  data
}, error = function(e) {
  cat("Error:", e$message, "\n")
  NULL
})
