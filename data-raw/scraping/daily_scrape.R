# daily_scrape.R
# Incremental daily scrape for GitHub Actions
#
# - Runs incrementally (only fetches new/missing matches)
# - Covers all competitions (leagues, cups, national teams)
# - Designed for automated daily runs via GitHub Actions
#
# Run from panna directory: Rscript data-raw/scraping/daily_scrape.R

devtools::load_all()

# 1. Configuration ----

# Incremental mode - only fetch matches not in cache
FORCE_RESCRAPE <- FALSE

# Delay between requests (respect FBref rate limits)
DELAY <- 4

# Table types to scrape
TABLE_TYPES <- c("summary", "passing", "passing_types", "defense",
                 "possession", "misc", "keeper", "shots", "events", "metadata")

# Current season (most matches happen here)
CURRENT_SEASON <- "2025-2026"

# 2. Competitions ----

# Club competitions: Big 5 leagues + European cups + domestic cups
CLUB_COMPS <- c(
  list_competitions("league"),    # ENG, ESP, GER, ITA, FRA
  list_competitions("european"),  # UCL, UEL
  list_competitions("cup")        # FA Cup, Copa del Rey, DFB Pokal, etc.
)

# National team competitions
NATIONAL_COMPS <- list_competitions("national_team")  # WC, EURO, Nations League, etc.

# Determine current national team season
get_current_national_season <- function() {
  year <- as.integer(format(Sys.Date(), "%Y"))
  month <- as.integer(format(Sys.Date(), "%m"))

  # If before July, use previous year as start
  if (month < 7) {
    paste0(year - 1, "-", year)
  } else {
    paste0(year, "-", year + 1)
  }
}

# 3. Main Script ----

cat("\n", strrep("=", 60), "\n")
cat("PANNA DAILY SCRAPE\n")
cat(strrep("=", 60), "\n\n")

cat("Mode: INCREMENTAL (using cache)\n")
cat("Delay:", DELAY, "seconds\n")
cat("Current season:", CURRENT_SEASON, "\n")
cat("Club competitions:", paste(CLUB_COMPS, collapse = ", "), "\n")
cat("National competitions:", paste(NATIONAL_COMPS, collapse = ", "), "\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

total_scraped <- 0

# 4. Club Competitions ----
cat("*** CLUB COMPETITIONS ***\n")

for (comp in CLUB_COMPS) {
  cat("\n", strrep("-", 40), "\n")
  cat(comp, "- Season:", CURRENT_SEASON, "\n")
  cat(strrep("-", 40), "\n")

  n <- tryCatch(
    scrape_comp_season(comp, CURRENT_SEASON, TABLE_TYPES, DELAY,
                       force_rescrape = FORCE_RESCRAPE, max_matches = Inf),
    error = function(e) {
      cat("  Error:", e$message, "\n")
      0
    }
  )

  total_scraped <- total_scraped + n
}

# 5. National Team Competitions ----
cat("\n\n*** NATIONAL TEAM COMPETITIONS ***\n")

for (comp in NATIONAL_COMPS) {
  cat("\n", strrep("-", 40), "\n")
  cat(comp, "\n")
  cat(strrep("-", 40), "\n")

  # Get appropriate seasons for this competition
  if (is_tournament_competition(comp) && comp != "NATIONS_LEAGUE") {
    # Tournaments: get years since 2020 (recent tournaments only)
    comp_seasons <- tryCatch(
      get_tournament_years(comp),
      error = function(e) character(0)
    )
    # Filter to recent years only
    comp_seasons <- comp_seasons[as.numeric(comp_seasons) >= 2020]
  } else {
    # Nations League uses regular season format
    comp_seasons <- get_current_national_season()
  }

  if (length(comp_seasons) == 0) {
    cat("  No active seasons\n")
    next
  }

  for (season in comp_seasons) {
    cat("  Season:", season, "\n")

    n <- tryCatch(
      scrape_comp_season(comp, season, TABLE_TYPES, DELAY,
                         force_rescrape = FORCE_RESCRAPE, max_matches = Inf),
      error = function(e) {
        cat("    Error:", e$message, "\n")
        0
      }
    )

    total_scraped <- total_scraped + n
  }
}

# 6. Summary ----
cat("\n\n", strrep("=", 60), "\n")
cat("DAILY SCRAPE COMPLETE\n")
cat(strrep("=", 60), "\n")
cat("\nNew matches scraped:", total_scraped, "\n")
cat("Finished:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Show cache status for key tables
cat("\nCache status:\n")
for (table_type in c("metadata", "summary", "events")) {
  cached <- tryCatch(
    list_cached_matches(table_type),
    error = function(e) data.frame()
  )
  cat(sprintf("  %s: %d matches\n", table_type, nrow(cached)))
}

cat("\nNext: Upload to GitHub Releases via piggyback\n")
