# test_cup_fixtures.R
# Quick test to verify cup competition fixtures scraping works
# Run this in RStudio after waiting a few minutes (rate limit cooldown)

devtools::load_all()

# Test URL generation
cat("=== URL Generation Test ===\n\n")
test_comps <- c("FA_CUP", "UCL", "UEL", "COPA_DEL_REY", "DFB_POKAL")
for (comp in test_comps) {
  url <- build_fixtures_url(comp, "2024-2025")
  cat(sprintf("%s:\n  %s\n", comp, url))
}

# Test one fixtures fetch
cat("\n=== Fixtures Fetch Test ===\n\n")
cat("Testing FA Cup 2024-2025...\n")
Sys.sleep(4)

fixtures <- tryCatch({
  scrape_fixtures("FA_CUP", "2024-2025", completed_only = TRUE)
}, error = function(e) {
  cat("Error:", e$message, "\n")
  NULL
})

if (!is.null(fixtures) && nrow(fixtures) > 0) {
  cat(sprintf("SUCCESS! Found %d matches\n\n", nrow(fixtures)))
  cat("Sample matches:\n")
  print(head(fixtures[, c("home_team", "away_team", "date")], 10))
} else {
  cat("No fixtures returned (may be rate limited)\n")
}

# Test Champions League
cat("\n\nTesting Champions League 2024-2025...\n")
Sys.sleep(4)

ucl_fixtures <- tryCatch({
  scrape_fixtures("UCL", "2024-2025", completed_only = TRUE)
}, error = function(e) {
  cat("Error:", e$message, "\n")
  NULL
})

if (!is.null(ucl_fixtures) && nrow(ucl_fixtures) > 0) {
  cat(sprintf("SUCCESS! Found %d matches\n\n", nrow(ucl_fixtures)))
  cat("Sample matches:\n")
  print(head(ucl_fixtures[, c("home_team", "away_team", "date")], 10))
} else {
  cat("No fixtures returned (may be rate limited)\n")
}
