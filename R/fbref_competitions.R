# FBref competition metadata
#
# Central storage for FBref competition IDs and metadata.
# Used by scraping functions to map league codes to FBref URLs.
# This is the SINGLE SOURCE OF TRUTH for competition metadata.

#' FBref competition metadata
#'
#' A data frame containing FBref competition IDs and metadata for all
#' supported competitions.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{code}{Short code used in file paths (e.g., "ENG", "WC")}
#'   \item{fbref_id}{FBref competition ID for URL construction}
#'   \item{name}{Full competition name}
#'   \item{url_slug}{URL-friendly name for FBref URLs (e.g., "Premier-League")}
#'   \item{type}{Competition type: "league", "cup", "european", "national_team"}
#'   \item{season_format}{How seasons are formatted: "YYYY-YYYY" or "YYYY"}
#' }
#'
#' @export
#' @examples
#' fbref_competitions
#' fbref_competitions[fbref_competitions$type == "national_team", ]
#' get_fbref_comp_id("WC")
fbref_competitions <- data.frame(
  code = c(
    # Big 5 leagues
    "ENG", "ESP", "GER", "ITA", "FRA",
    # European club competitions
    "UCL", "UEL",
    # Domestic cups
    "FA_CUP", "EFL_CUP", "COPA_DEL_REY", "DFB_POKAL", "COPPA_ITALIA", "COUPE_DE_FRANCE",
    # National team competitions
    "WC", "EURO", "COPA_AMERICA", "AFCON", "NATIONS_LEAGUE", "GOLD_CUP", "ASIAN_CUP"
  ),
  fbref_id = c(
    # Big 5 leagues
    9, 12, 20, 11, 13,
    # European club competitions
    8, 19,
    # Domestic cups (FA Cup, EFL Cup, Copa del Rey, DFB-Pokal, Coppa Italia, Coupe de France)
    514, 690, 569, 521, 529, 518,
    # National team competitions
    1, 676, 685, 656, 677, 681, 664
  ),
  name = c(
    # Big 5 leagues
    "Premier League", "La Liga", "Bundesliga", "Serie A", "Ligue 1",
    # European club competitions
    "UEFA Champions League", "UEFA Europa League",
    # Domestic cups
    "FA Cup", "EFL Cup", "Copa del Rey", "DFB-Pokal", "Coppa Italia", "Coupe de France",
    # National team competitions
    "FIFA World Cup", "UEFA European Championship", "Copa America",
    "Africa Cup of Nations", "UEFA Nations League", "CONCACAF Gold Cup", "AFC Asian Cup"
  ),
  url_slug = c(
    # Big 5 leagues
    "Premier-League", "La-Liga", "Bundesliga", "Serie-A", "Ligue-1",
    # European club competitions
    "Champions-League", "Europa-League",
    # Domestic cups
    "FA-Cup", "EFL-Cup", "Copa-del-Rey", "DFB-Pokal", "Coppa-Italia", "Coupe-de-France",
    # National team competitions
    "World-Cup", "European-Championship", "Copa-America",
    "Africa-Cup-of-Nations", "Nations-League", "Gold-Cup", "Asian-Cup"
  ),
  type = c(
    # Big 5 leagues
    rep("league", 5),
    # European club competitions
    rep("european", 2),
    # Domestic cups
    rep("cup", 6),
    # National team competitions
    rep("national_team", 7)
  ),
  season_format = c(
    # Big 5 leagues - YYYY-YYYY
    rep("YYYY-YYYY", 5),
    # European club competitions - YYYY-YYYY
    rep("YYYY-YYYY", 2),
    # Domestic cups - YYYY-YYYY
    rep("YYYY-YYYY", 6),
    # National team competitions - YYYY (except Nations League)
    "YYYY", "YYYY", "YYYY", "YYYY", "YYYY-YYYY", "YYYY", "YYYY"
  ),
  stringsAsFactors = FALSE
)


#' Get FBref competition ID
#'
#' Looks up the FBref competition ID for a given league code.
#'
#' @param code Character, the league code (e.g., "ENG", "WC")
#'
#' @return Integer, the FBref competition ID
#' @export
#'
#' @examples
#' get_fbref_comp_id("ENG")  # 9
#' get_fbref_comp_id("WC")   # 1
get_fbref_comp_id <- function(code) {
  idx <- match(code, fbref_competitions$code)
  if (is.na(idx)) {
    stop("Unknown competition code: ", code)
  }
  fbref_competitions$fbref_id[idx]
}


#' Get FBref schedule URL
#'
#' Constructs the FBref fixtures/schedule URL for a competition and season.
#'
#' @param code Character, the league code (e.g., "ENG", "WC")
#' @param season Character, the season (e.g., "2023-2024" or "2024")
#'
#' @return Character, the full FBref schedule URL
#' @export
#'
#' @examples
#' get_fbref_schedule_url("ENG", "2023-2024")
#' get_fbref_schedule_url("WC", "2022")
get_fbref_schedule_url <- function(code, season) {
  idx <- match(code, fbref_competitions$code)
  if (is.na(idx)) {
    stop("Unknown competition code: ", code)
  }

  comp_id <- fbref_competitions$fbref_id[idx]
  url_slug <- fbref_competitions$url_slug[idx]

  # For tournament competitions, extract year if season format given
  if (fbref_competitions$season_format[idx] == "YYYY" && grepl("-", season)) {
    season <- substr(season, 1, 4)
  }

  sprintf(
    "https://fbref.com/en/comps/%d/%s/schedule/%s-%s-Scores-and-Fixtures",
    comp_id, season, season, url_slug
  )
}


#' Check if competition uses tournament (year-only) format
#'
#' @param code Character, the competition code
#'
#' @return Logical, TRUE if competition uses year-only seasons
#' @export
#'
#' @examples
#' is_tournament_competition("WC")    # TRUE
#' is_tournament_competition("ENG")   # FALSE
is_tournament_competition <- function(code) {
  idx <- match(code, fbref_competitions$code)
  if (is.na(idx)) {
    stop("Unknown competition code: ", code)
  }
  fbref_competitions$season_format[idx] == "YYYY"
}


#' Get all seasons since a start year
#'
#' Returns a vector of season strings from start_year to current season.
#'
#' @param start_year First season start year (default 2017)
#'
#' @return Character vector of season strings (e.g., "2017-2018", "2018-2019", ...)
#' @export
#'
#' @examples
#' get_seasons_since(2020)
#' get_seasons_since()  # From 2017
get_seasons_since <- function(start_year = 2017) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  current_month <- as.numeric(format(Sys.Date(), "%m"))

  # If we're past July, include current season
 end_year <- if (current_month >= 7) current_year else current_year - 1

  years <- start_year:end_year
  paste0(years, "-", years + 1)
}


#' Get tournament years with available data
#'
#' Returns years when tournaments were held (with FBref data available).
#'
#' @param code Tournament code (e.g., "WC", "EURO", "COPA_AMERICA")
#'
#' @return Character vector of years
#' @export
#'
#' @examples
#' get_tournament_years("WC")
#' get_tournament_years("EURO")
get_tournament_years <- function(code) {
  # Tournament years with FBref detailed data (2018+)
  tournaments <- list(
    WC = c("2018", "2022", "2026"),
    EURO = c("2020", "2024"),
    COPA_AMERICA = c("2019", "2021", "2024"),
    AFCON = c("2019", "2021", "2023"),
    GOLD_CUP = c("2019", "2021", "2023"),
    ASIAN_CUP = c("2019", "2023")
  )

  if (!code %in% names(tournaments)) {
    stop("Unknown tournament: ", code,
         ". Valid options: ", paste(names(tournaments), collapse = ", "),
         "\nNote: Nations League uses season format, use get_seasons_since()")
  }

  tournaments[[code]]
}


#' List competitions by type
#'
#' Returns competition codes filtered by type.
#'
#' @param type Character, one of "league", "cup", "european", "national_team", or "all"
#'
#' @return Character vector of competition codes
#' @export
#'
#' @examples
#' list_competitions("league")
#' list_competitions("national_team")
list_competitions <- function(type = "all") {
  if (type == "all") {
    return(fbref_competitions$code)
  }
  fbref_competitions$code[fbref_competitions$type == type]
}
