# Full scrape script for panna
#
# Scrapes ALL competitions (clubs, cups, national teams) from FBref.
# Uses cache to skip matches already scraped (unless FORCE_RESCRAPE = TRUE).
#
# setwd("C:/Users/peteo/OneDrive/Documents/pannaverse/panna")
devtools::load_all()

# ============================================================================
# Configuration
# ============================================================================

# Set to TRUE to rescrape everything (ignores cache)
# Set to FALSE to only scrape new/missing matches (uses cache)
FORCE_RESCRAPE <- TRUE

# Delay between requests (minimum 3 seconds, 5 recommended)
DELAY <- 5

# Batch control: set to Inf for all matches, or a number to limit per session
MAX_MATCHES_PER_SESSION <- Inf

# Table types to scrape
TABLE_TYPES <- c("summary", "passing", "passing_types", "defense",
                 "possession", "misc", "keeper", "shots", "events", "metadata")

# Competitions to scrape (all by default)
CLUB_COMPS <- c(
 list_competitions("league"),   # Big 5 leagues
  list_competitions("european"), # UCL, UEL
  list_competitions("cup")       # Domestic cups (FA Cup, EFL Cup, etc.)
)

NATIONAL_COMPS <- list_competitions("national_team")  # WC, EURO, etc.

# Seasons
SEASONS <- get_seasons_since(2017)


# ============================================================================
# Helper Functions
# ============================================================================

#' Get match URLs from cached metadata
get_cached_match_urls <- function(league, season) {
  cache_dir <- file.path(
    "C:/Users/peteo/OneDrive/Documents/pannaverse/pannadata/data/metadata",
    league, season
  )

  if (!dir.exists(cache_dir)) return(character(0))

  files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) == 0) return(character(0))

  urls <- vapply(files, function(f) {
    meta <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(meta) && "match_url" %in% names(meta)) {
      return(meta$match_url)
    }
    NA_character_
  }, character(1), USE.NAMES = FALSE)

  urls[!is.na(urls)]
}


#' Scrape a competition-season (fetches fixtures if not cached)
scrape_comp_season <- function(comp, season, table_types, delay, force_rescrape) {

  cat(sprintf("\n%s %s\n", comp, season))
  cat(strrep("-", 40), "\n")

  # Check cache first
  cached_urls <- get_cached_match_urls(comp, season)

  if (length(cached_urls) > 0 && !force_rescrape) {
    # Check if all table types are cached for these matches
    # For simplicity, just check if metadata exists (assume others do too)
    cat(sprintf("  Cache: %d matches found\n", length(cached_urls)))

    # Still call scrape_fbref_matches - it will skip cached tables
    tryCatch({
      scrape_fbref_matches(
        match_urls = cached_urls,
        league = comp,
        season = season,
        table_types = table_types,
        delay = delay,
        use_cache = TRUE,
        verbose = TRUE
      )
    }, error = function(e) {
      cat("  ERROR:", conditionMessage(e), "\n")
    })

    return(invisible(NULL))
  }

  # No cache - fetch fixtures from FBref
  cat("  Fetching fixtures from FBref...\n")
  Sys.sleep(delay)

  fixtures <- tryCatch(
    scrape_fixtures(comp, season, completed_only = TRUE),
    error = function(e) {
      cat("  ERROR fetching fixtures:", conditionMessage(e), "\n")
      NULL
    }
  )

  if (is.null(fixtures) || nrow(fixtures) == 0) {
    cat("  No fixtures found\n")
    return(invisible(NULL))
  }

  urls <- fixtures$match_url
  cat(sprintf("  Found %d matches\n", length(urls)))

  # Scrape matches
  tryCatch({
    scrape_fbref_matches(
      match_urls = urls,
      league = comp,
      season = season,
      table_types = table_types,
      delay = delay,
      use_cache = !force_rescrape,
      verbose = TRUE
    )
  }, error = function(e) {
    cat("  ERROR:", conditionMessage(e), "\n")
  })

  invisible(NULL)
}


# ============================================================================
# Main Script
# ============================================================================

cat("\n", strrep("=", 60), "\n")
cat("PANNA SCRAPE ALL\n")
cat(strrep("=", 60), "\n\n")

cat("Mode:", ifelse(FORCE_RESCRAPE, "FORCE RESCRAPE (ignoring cache)", "INCREMENTAL (using cache)"), "\n")
cat("Delay:", DELAY, "seconds\n")
cat("Club competitions:", paste(CLUB_COMPS, collapse = ", "), "\n")
cat("National competitions:", paste(NATIONAL_COMPS, collapse = ", "), "\n")
cat("Seasons:", paste(range(SEASONS), collapse = " to "), "\n\n")

total_scraped <- 0


# ============================================================
# PART 1: Club competitions (leagues + cups)
# ============================================================
cat("*** CLUB COMPETITIONS ***\n")

for (comp in CLUB_COMPS) {
  cat("\n", strrep("=", 50), "\n")
  cat(comp, "\n")
  cat(strrep("=", 50), "\n")

  for (season in SEASONS) {
    scrape_comp_season(comp, season, TABLE_TYPES, DELAY, FORCE_RESCRAPE)
  }
}


# ============================================================
# PART 2: National team competitions
# ============================================================
cat("\n\n*** NATIONAL TEAM COMPETITIONS ***\n")

for (comp in NATIONAL_COMPS) {
  cat("\n", strrep("=", 50), "\n")
  cat(comp, "\n")
  cat(strrep("=", 50), "\n")

  # Get appropriate seasons for this competition
  if (is_tournament_competition(comp) && comp != "NATIONS_LEAGUE") {
    comp_seasons <- tryCatch(
      get_tournament_years(comp),
      error = function(e) character(0)
    )
  } else {
    # Nations League uses regular season format
    comp_seasons <- get_seasons_since(2018)
  }

  if (length(comp_seasons) == 0) {
    cat("  No seasons configured\n")
    next
  }

  for (season in comp_seasons) {
    scrape_comp_season(comp, season, TABLE_TYPES, DELAY, FORCE_RESCRAPE)
  }
}


# ============================================================
# Summary
# ============================================================
cat("\n\n", strrep("=", 60), "\n")
cat("SCRAPING COMPLETE\n")
cat(strrep("=", 60), "\n")

# Show cache status
cat("\nCache status:\n")
for (table_type in c("metadata", "summary", "events")) {
  cached <- tryCatch(
    list_cached_matches(table_type),
    error = function(e) data.frame()
  )
  cat(sprintf("  %s: %d matches\n", table_type, nrow(cached)))
}

cat("\nTo load data:\n")
cat("  summary <- aggregate_cached_matches('summary', league='ENG')\n")
cat("  events <- aggregate_cached_matches('events', league='ENG')\n")
