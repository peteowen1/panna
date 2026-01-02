# 08_scrape_fbref_matches.R
#
# Scrapes match-level data from FBref for Big 5 leagues, domestic cups, and
# UEFA club competitions since 2017-2018.
# Uses direct HTTP requests with browser headers (bypasses worldfootballR).
#
# This script is designed to be run incrementally - it will skip matches
# that have already been cached.
#
# IMPORTANT: This is a large scraping job. At 4 seconds per match, scraping
# all competitions from scratch takes many hours. Run in batches!

library(dplyr)
devtools::load_all()

# ============================================================================
# Configuration
# ============================================================================

# Competitions to scrape (adjust as needed)
# Options: get_big5_codes(), get_domestic_cup_codes(), get_uefa_cup_codes()
LEAGUES <- get_all_competition_codes()

# Seasons to scrape (2017-2018 onwards)
SEASONS <- get_seasons_since(2017)

# Delay between requests (integer seconds) - be polite!
DELAY <- 5

# Table types to scrape per match (all available)
TABLE_TYPES <- c("summary", "passing", "passing_types", "defense",
                 "possession", "misc", "keeper", "shots", "metadata")

# How many matches to scrape per session (set to Inf for all)
# Useful for running in batches
MAX_MATCHES_PER_SESSION <- Inf

# Whether to save fixtures to cache
CACHE_FIXTURES <- TRUE


# ============================================================================
# Helper Functions
# ============================================================================

#' Get fixtures cache path
get_fixtures_cache_path <- function(league, season) {
  cache_dir <- file.path("data", "fbref_matches", "fixtures")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  file.path(cache_dir, paste0(league, "_", season, "_fixtures.rds"))
}


#' Load or scrape fixtures for a league-season
get_fixtures <- function(league, season, use_cache = TRUE) {
  cache_path <- get_fixtures_cache_path(league, season)

  # Check cache
 if (use_cache && file.exists(cache_path)) {
    progress_msg(sprintf("Loading cached fixtures: %s %s", league, season))
    return(readRDS(cache_path))
  }

  # Scrape fixtures
  fixtures <- scrape_fixtures(league, season, completed_only = TRUE)

  # Cache if successful
  if (!is.null(fixtures) && nrow(fixtures) > 0 && CACHE_FIXTURES) {
    saveRDS(fixtures, cache_path)
  }

  # Rate limit after fetching fixtures page (with jitter)
  Sys.sleep(add_delay_jitter(DELAY))

  fixtures
}


#' Get matches that haven't been fully scraped yet
#' Uses batch lookup for speed
get_uncached_matches <- function(fixtures, league, season) {
  if (is.null(fixtures) || nrow(fixtures) == 0) {
    return(character(0))
  }

  # Extract fbref_ids from URLs
  fbref_ids <- regmatches(fixtures$match_url, regexpr("[a-f0-9]{8}", fixtures$match_url))

  # Get all cached IDs for this league-season (fast batch lookup)
  cached_ids <- get_cached_match_ids(league, season)

  # Return URLs for uncached matches
  fixtures$match_url[!fbref_ids %in% cached_ids]
}


#' Print scraping summary
print_summary <- function(all_fixtures) {
  cat("\n")
  cat("=" , rep("=", 60), "\n", sep = "")
  cat("SCRAPING SUMMARY\n")
  cat("=", rep("=", 60), "\n", sep = "")

  for (league in names(all_fixtures)) {
    cat(sprintf("\n%s:\n", league))
    league_data <- all_fixtures[[league]]

    for (season in names(league_data)) {
      fixtures <- league_data[[season]]
      if (!is.null(fixtures)) {
        n_total <- nrow(fixtures)
        n_uncached <- length(get_uncached_matches(fixtures, league, season))
        cat(sprintf("  %s: %d matches (%d to scrape)\n",
                    season, n_total, n_uncached))
      }
    }
  }
  cat("\n")
}


# ============================================================================
# Main Scraping Logic
# ============================================================================

cat("FBref Match Scraper\n")
cat("===================\n\n")
cat("Leagues:", paste(LEAGUES, collapse = ", "), "\n")
cat("Seasons:", paste(range(SEASONS), collapse = " to "), "\n")
cat("Delay:", DELAY, "seconds\n")
cat("Max matches per session:", MAX_MATCHES_PER_SESSION, "\n\n")


# Step 1: Collect all fixtures
# ----------------------------
cat("Step 1: Collecting fixtures for all league-seasons...\n\n")

all_fixtures <- list()

for (league in LEAGUES) {
  all_fixtures[[league]] <- list()

  for (season in SEASONS) {
    fixtures <- tryCatch({
      get_fixtures(league, season, use_cache = CACHE_FIXTURES)
    }, error = function(e) {
      warning(sprintf("Failed to get fixtures for %s %s: %s",
                      league, season, e$message))
      NULL
    })

    all_fixtures[[league]][[season]] <- fixtures
  }
}

# Print summary
print_summary(all_fixtures)


# Step 2: Build scraping queue
# ----------------------------
cat("Step 2: Building scraping queue...\n\n")

scrape_queue <- data.frame(
  league = character(0),
  season = character(0),
  match_url = character(0),
  stringsAsFactors = FALSE
)

for (league in LEAGUES) {
  for (season in SEASONS) {
    fixtures <- all_fixtures[[league]][[season]]
    uncached <- get_uncached_matches(fixtures, league, season)

    if (length(uncached) > 0) {
      scrape_queue <- rbind(scrape_queue, data.frame(
        league = league,
        season = season,
        match_url = uncached,
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat(sprintf("Total matches to scrape: %d\n", nrow(scrape_queue)))

if (nrow(scrape_queue) == 0) {
  cat("\nAll matches already cached! Nothing to do.\n")
} else {
  # Limit to max per session
  if (is.finite(MAX_MATCHES_PER_SESSION) && nrow(scrape_queue) > MAX_MATCHES_PER_SESSION) {
    cat(sprintf("Limiting to %d matches this session\n", MAX_MATCHES_PER_SESSION))
    scrape_queue <- scrape_queue[1:MAX_MATCHES_PER_SESSION, ]
  }

  estimated_time <- nrow(scrape_queue) * DELAY / 60
  cat(sprintf("Estimated time: %.1f minutes\n\n", estimated_time))


  # Step 3: Scrape matches
  # ----------------------
  cat("Step 3: Scraping matches...\n\n")

  # Group by league-season for efficient scraping
  queue_split <- split(scrape_queue, paste(scrape_queue$league, scrape_queue$season))

  total_scraped <- 0
  total_failed <- 0

  for (group_name in names(queue_split)) {
    group <- queue_split[[group_name]]
    league <- group$league[1]
    season <- group$season[1]

    cat(sprintf("\n--- %s %s (%d matches) ---\n", league, season, nrow(group)))

    result <- tryCatch({
      scrape_fbref_matches(
        match_urls = group$match_url,
        league = league,
        season = season,
        table_types = TABLE_TYPES,
        delay = DELAY,
        use_cache = TRUE,
        verbose = TRUE
      )
    }, error = function(e) {
      warning(sprintf("Error scraping %s %s: %s", league, season, e$message))
      NULL
    })

    if (!is.null(result) && !is.null(result$metadata)) {
      total_scraped <- total_scraped + nrow(result$metadata)
    }
  }

  cat("\n")
  cat("=", rep("=", 60), "\n", sep = "")
  cat(sprintf("COMPLETE: Scraped %d matches\n", total_scraped))
  cat("=", rep("=", 60), "\n", sep = "")
}


# Step 4: Show cache status
# -------------------------
cat("\n\nCache Status:\n")
cat("-------------\n")

for (table_type in TABLE_TYPES) {
  cached <- list_cached_matches(table_type)
  cat(sprintf("%s: %d matches cached\n", table_type, nrow(cached)))
}

cat("\nTo load cached data:\n")
cat("  summary_data <- aggregate_cached_matches('summary')\n")
cat("  shots_data <- aggregate_cached_matches('shots')\n")
