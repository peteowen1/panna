# Understat competition metadata
#
# Central storage for Understat competition slugs and metadata.
# Used by scraping functions to map league codes to Understat URLs.
# This is the SINGLE SOURCE OF TRUTH for Understat competition metadata.

#' Understat competition metadata
#'
#' A data frame containing Understat competition slugs and metadata for all
#' supported competitions. Understat covers Big 5 European leagues plus Russia.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{code}{Short code used in file paths (e.g., "ENG", "RUS")}
#'   \item{name}{Full competition name}
#'   \item{understat_slug}{URL slug for Understat URLs (e.g., "EPL", "La_liga")}
#'   \item{season_format}{How seasons are formatted: "YYYY" (Understat uses single year)}
#' }
#'
#' @export
#' @examples
#' understat_competitions
#' understat_competitions[understat_competitions$code == "ENG", ]
#' get_understat_slug("ENG")
understat_competitions <- data.frame(
code = c(
    # Big 5 leagues
    "ENG", "ESP", "GER", "ITA", "FRA",
    # Russia (Understat-exclusive, not on FBref)
    "RUS"
  ),
  name = c(
    # Big 5 leagues
    "Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1",
    # Russia
    "Russian Premier League"
  ),
  understat_slug = c(
    # Big 5 leagues
    "EPL", "La_liga", "Bundesliga", "Serie_A", "Ligue_1",
    # Russia
    "RFPL"
  ),
  season_format = rep("YYYY", 6),
  stringsAsFactors = FALSE
)


#' Get Understat URL slug
#'
#' Looks up the Understat URL slug for a given league code.
#'
#' @param code Character, the league code (e.g., "ENG", "RUS")
#'
#' @return Character, the Understat URL slug
#' @export
#'
#' @examples
#' get_understat_slug("ENG")  # "EPL"
#' get_understat_slug("RUS")  # "RFPL"
get_understat_slug <- function(code) {
  idx <- match(code, understat_competitions$code)
  if (is.na(idx)) {
    stop("Unknown Understat competition code: ", code,
         "\nValid codes: ", paste(understat_competitions$code, collapse = ", "))
  }
  understat_competitions$understat_slug[idx]
}


#' Get Understat league URL
#'
#' Constructs the Understat league page URL for a competition and season.
#'
#' @param code Character, the league code (e.g., "ENG", "RUS")
#' @param season Character or numeric, the season year (e.g., "2024" or 2024)
#'
#' @return Character, the full Understat league URL
#' @export
#'
#' @examples
#' get_understat_league_url("ENG", 2024)
#' get_understat_league_url("ESP", "2023")
get_understat_league_url <- function(code, season) {
  slug <- get_understat_slug(code)

  # Convert season to year if in YYYY-YYYY format
season_year <- if (grepl("-", as.character(season))) {
    substr(season, 1, 4)
  } else {
    as.character(season)
  }

  sprintf("https://understat.com/league/%s/%s", slug, season_year)
}


#' Get Understat match URL
#'
#' Constructs the Understat match page URL for a given match ID.
#'
#' @param understat_id Numeric or character, the Understat match ID
#'
#' @return Character, the full Understat match URL
#' @export
#'
#' @examples
#' get_understat_match_url(28988)
get_understat_match_url <- function(understat_id) {
  sprintf("https://understat.com/match/%s", understat_id)
}


#' Convert FBref season to Understat season
#'
#' Converts FBref's YYYY-YYYY format to Understat's YYYY format.
#' Takes the start year of the season.
#'
#' @param fbref_season Character, season in FBref format (e.g., "2023-2024")
#'
#' @return Character, season in Understat format (e.g., "2023")
#' @export
#'
#' @examples
#' fbref_to_understat_season("2023-2024")  # "2023"
#' fbref_to_understat_season("2024")       # "2024" (already correct)
fbref_to_understat_season <- function(fbref_season) {
  if (grepl("-", fbref_season)) {
    substr(fbref_season, 1, 4)
  } else {
    fbref_season
  }
}


#' Convert Understat season to FBref season
#'
#' Converts Understat's YYYY format to FBref's YYYY-YYYY format.
#'
#' @param understat_season Character or numeric, season in Understat format (e.g., "2023" or 2023)
#'
#' @return Character, season in FBref format (e.g., "2023-2024")
#' @export
#'
#' @examples
#' understat_to_fbref_season(2023)    # "2023-2024"
#' understat_to_fbref_season("2024")  # "2024-2025"
understat_to_fbref_season <- function(understat_season) {
  year <- as.integer(understat_season)
  paste0(year, "-", year + 1)
}


#' Get available Understat seasons
#'
#' Returns a vector of available seasons for Understat data.
#' Understat has data from 2014 onwards.
#'
#' @param start_year First season year (default 2014)
#'
#' @return Character vector of season years
#' @export
#'
#' @examples
#' get_understat_seasons()
#' get_understat_seasons(2020)
get_understat_seasons <- function(start_year = 2014) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_month <- as.numeric(format(Sys.Date(), "%m"))

  # If we're past July, include current season
  end_year <- if (current_month >= 7) current_year else current_year - 1

  as.character(start_year:end_year)
}


#' List Understat competitions
#'
#' Returns all available Understat competition codes.
#'
#' @return Character vector of competition codes
#' @export
#'
#' @examples
#' list_understat_competitions()
list_understat_competitions <- function() {
  understat_competitions$code
}


#' Check if league is available on Understat
#'
#' @param code Character, the league code
#'
#' @return Logical, TRUE if league is available on Understat
#' @export
#'
#' @examples
#' is_understat_league("ENG")  # TRUE
#' is_understat_league("POR")
is_understat_league <- function(code) {
  code %in% understat_competitions$code
}


#' Map Understat league name to our code
#'
#' Converts Understat's league name (e.g., "EPL", "La liga") to our
#' standardized code (e.g., "ENG", "ESP").
#'
#' @param understat_league Character, the league name from Understat metadata
#'
#' @return Character, our league code, or NA if not found
#' @export
#'
#' @examples
#' understat_league_to_code("EPL")      # "ENG"
#' understat_league_to_code("La liga")  # "ESP"
#' understat_league_to_code("Serie A")  # "ITA"
understat_league_to_code <- function(understat_league) {
  # Understat uses these names in their metadata
  # Need to map to our codes
  mapping <- c(
    "EPL" = "ENG",
    "La liga" = "ESP",
    "Bundesliga" = "GER",
    "Serie A" = "ITA",
    "Ligue 1" = "FRA",
    "RFPL" = "RUS"
  )

  code <- mapping[understat_league]
  if (is.na(code)) {
    warning("Unknown Understat league: ", understat_league)
  }
  unname(code)
}
