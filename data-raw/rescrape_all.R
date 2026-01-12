# Full scrape script for panna
#
# Scrapes ALL competitions (clubs, cups, national teams) from FBref.
# Uses cache to skip matches already scraped (unless FORCE_RESCRAPE = TRUE).
#
# setwd("C:/Users/peteo/OneDrive/Documents/pannaverse/panna")
devtools::load_all()

# Configuration ----

# Set to TRUE to rescrape everything (ignores cache)
# Set to FALSE to only scrape new/missing matches (uses cache)
FORCE_RESCRAPE <-  TRUE

# Delay between requests (minimum 3 seconds, 5 recommended)
DELAY <- 4

# Batch control: set to Inf for unlimited
MAX_MATCHES_PER_SEASON <- Inf    # Limit per competition-season (e.g., 10 means 10 from ENG 2017-2018, 10 from ENG 2018-2019, etc.)
MAX_MATCHES_TOTAL <- Inf          # Limit for entire session (stops script after this many total matches)

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


# Main Script ----

cat("\n", strrep("=", 60), "\n")
cat("PANNA SCRAPE ALL\n")
cat(strrep("=", 60), "\n\n")

cat("Mode:", ifelse(FORCE_RESCRAPE, "FORCE RESCRAPE (ignoring cache)", "INCREMENTAL (using cache)"), "\n")
cat("Delay:", DELAY, "seconds\n")
cat("Max per season:", if (is.infinite(MAX_MATCHES_PER_SEASON)) "Unlimited" else MAX_MATCHES_PER_SEASON, "\n")
cat("Max total:", if (is.infinite(MAX_MATCHES_TOTAL)) "Unlimited" else MAX_MATCHES_TOTAL, "\n")
cat("Club competitions:", paste(CLUB_COMPS, collapse = ", "), "\n")
cat("National competitions:", paste(NATIONAL_COMPS, collapse = ", "), "\n")
cat("Seasons:", paste(range(SEASONS), collapse = " to "), "\n\n")

total_scraped <- 0
session_limit_reached <- FALSE


# Club Competitions ----
cat("*** CLUB COMPETITIONS ***\n")

for (comp in CLUB_COMPS) {
  if (session_limit_reached) break

  cat("\n", strrep("=", 50), "\n")
  cat(comp, "\n")
  cat(strrep("=", 50), "\n")

  for (season in SEASONS) {
    if (session_limit_reached) break

    # Calculate how many matches we can still scrape
    remaining <- MAX_MATCHES_TOTAL - total_scraped
    max_this_call <- min(MAX_MATCHES_PER_SEASON, remaining)

    n <- scrape_comp_season(comp, season, TABLE_TYPES, DELAY, FORCE_RESCRAPE, max_this_call)
    total_scraped <- total_scraped + n

    if (total_scraped >= MAX_MATCHES_TOTAL) {
      cat("\n*** SESSION LIMIT REACHED:", total_scraped, "matches ***\n")
      session_limit_reached <- TRUE
    }
  }
}


# National Team Competitions ----
if (!session_limit_reached) {
  cat("\n\n*** NATIONAL TEAM COMPETITIONS ***\n")

  for (comp in NATIONAL_COMPS) {
    if (session_limit_reached) break

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
      if (session_limit_reached) break

      # Calculate how many matches we can still scrape
      remaining <- MAX_MATCHES_TOTAL - total_scraped
      max_this_call <- min(MAX_MATCHES_PER_SEASON, remaining)

      n <- scrape_comp_season(comp, season, TABLE_TYPES, DELAY, FORCE_RESCRAPE, max_this_call)
      total_scraped <- total_scraped + n

      if (total_scraped >= MAX_MATCHES_TOTAL) {
        cat("\n*** SESSION LIMIT REACHED:", total_scraped, "matches ***\n")
        session_limit_reached <- TRUE
      }
    }
  }
}


# Summary ----
cat("\n\n", strrep("=", 60), "\n")
cat("SCRAPING COMPLETE\n")
cat(strrep("=", 60), "\n")
cat("\nSession total:", total_scraped, "matches scraped\n")

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
