# Weather Data Functions
#
# Fetches historical and forecast weather for match venues using the
# Open-Meteo API (free, no API key required). Weather features can be
# used in match prediction and xG models.
#
# Data source hierarchy: Cricinfo > Cricsheet (mirrored from bouncer)
# Weather source: Open-Meteo archive + forecast APIs

#' Geocode a Venue
#'
#' Looks up latitude/longitude for a venue name using a built-in reference
#' table of major football stadiums. Falls back to Nominatim (OpenStreetMap)
#' for unknown venues.
#'
#' @param venue Character. Venue/stadium name.
#' @param country Character. Country hint for disambiguation (optional).
#' @param use_nominatim Logical. If TRUE, tries Nominatim API for unknown venues.
#'
#' @return Named list with `lat`, `lon`, `source` ("reference" or "nominatim")
#' @keywords internal
geocode_venue <- function(venue, country = NULL, use_nominatim = TRUE) {
  if (is.na(venue) || venue == "") return(list(lat = NA_real_, lon = NA_real_, source = "missing"))

  # Check built-in reference first
  ref <- get_venue_coordinates()
  venue_lower <- tolower(trimws(venue))

  match_idx <- which(tolower(ref$venue) == venue_lower)
  if (length(match_idx) == 0) {
    # Try partial match
    match_idx <- which(vapply(tolower(ref$venue), function(v) grepl(v, venue_lower, fixed = TRUE), logical(1)))
  }
  if (length(match_idx) == 0) {
    match_idx <- which(vapply(venue_lower, function(v) grepl(v, tolower(ref$venue), fixed = TRUE), logical(1)))
  }

  if (length(match_idx) > 0) {
    row <- ref[match_idx[1], ]
    return(list(lat = row$lat, lon = row$lon, source = "reference"))
  }

  # Fallback to Nominatim
  if (use_nominatim) {
    result <- tryCatch({
      query <- if (!is.null(country)) paste(venue, country, sep = ", ") else venue
      url <- paste0("https://nominatim.openstreetmap.org/search?q=",
                     utils::URLencode(query), "&format=json&limit=1")
      resp <- httr2::request(url) |>
        httr2::req_headers(`User-Agent` = "panna-r-package/0.2.0") |>
        httr2::req_retry(max_tries = 2) |>
        httr2::req_perform()
      Sys.sleep(1.1)  # Nominatim rate limit: always sleep after request
      data <- httr2::resp_body_json(resp)
      if (length(data) > 0) {
        list(lat = as.numeric(data[[1]]$lat), lon = as.numeric(data[[1]]$lon), source = "nominatim")
      } else {
        list(lat = NA_real_, lon = NA_real_, source = "not_found")
      }
    }, error = function(e) {
      list(lat = NA_real_, lon = NA_real_, source = "error")
    })
    return(result)
  }

  list(lat = NA_real_, lon = NA_real_, source = "not_found")
}


#' Get Venue Coordinates Reference
#'
#' Returns the built-in reference table of major football stadium coordinates.
#'
#' @return Data frame with venue, lat, lon, country columns
#' @keywords internal
get_venue_coordinates <- function() {
  ref_path <- system.file("extdata", "venue_coordinates.csv", package = "panna")
  if (ref_path == "") {
    # Fallback for devtools::load_all()
    ref_path <- file.path("inst", "extdata", "venue_coordinates.csv")
  }
  if (file.exists(ref_path)) {
    utils::read.csv(ref_path, stringsAsFactors = FALSE)
  } else {
    data.frame(venue = character(), lat = numeric(), lon = numeric(),
               country = character(), stringsAsFactors = FALSE)
  }
}


#' Fetch Weather from Open-Meteo
#'
#' Fetches daily weather data for a location and date range from the
#' Open-Meteo archive API (historical) or forecast API (future).
#'
#' @param lat Numeric. Latitude.
#' @param lon Numeric. Longitude.
#' @param start_date Character or Date. Start date (YYYY-MM-DD).
#' @param end_date Character or Date. End date (YYYY-MM-DD).
#'
#' @return Data frame with date, temperature_max, temperature_min,
#'   precipitation, wind_speed_max, humidity_mean columns
#' @keywords internal
fetch_weather <- function(lat, lon, start_date, end_date) {
  start_date <- as.character(as.Date(start_date))
  end_date <- as.character(as.Date(end_date))

  # Choose API based on date
  today <- Sys.Date()
  is_forecast <- as.Date(end_date) > today - 5

  if (is_forecast) {
    base_url <- "https://api.open-meteo.com/v1/forecast"
  } else {
    base_url <- "https://archive-api.open-meteo.com/v1/archive"
  }

  url <- paste0(base_url,
    "?latitude=", lat,
    "&longitude=", lon,
    "&start_date=", start_date,
    "&end_date=", end_date,
    "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,",
    "wind_speed_10m_max,relative_humidity_2m_mean",
    "&timezone=UTC"
  )

  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_retry(max_tries = 3, backoff = function(i) 2^i) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
  }, error = function(e) {
    cli::cli_warn("Weather API failed for ({lat}, {lon}): {e$message}")
    return(NULL)
  })

  if (is.null(resp)) return(data.frame())

  data <- httr2::resp_body_json(resp)

  if (is.null(data$daily) || length(data$daily$time) == 0) {
    return(data.frame())
  }

  d <- data$daily
  data.frame(
    date = as.Date(unlist(d$time)),
    temperature_max = as.numeric(unlist(d$temperature_2m_max)),
    temperature_min = as.numeric(unlist(d$temperature_2m_min)),
    precipitation = as.numeric(unlist(d$precipitation_sum)),
    wind_speed_max = as.numeric(unlist(d$wind_speed_10m_max)),
    humidity_mean = as.numeric(unlist(d$relative_humidity_2m_mean)),
    stringsAsFactors = FALSE
  )
}


#' Add Weather Features to Match Data
#'
#' Joins weather data to a match data frame. Fetches weather for each
#' unique venue+date combination, then merges back.
#'
#' @param matches Data frame with at least `date`, `venue` (or `stadium`),
#'   and optionally `country` columns.
#' @param venue_col Character. Name of the venue column. Default "venue".
#' @param date_col Character. Name of the date column. Default "date".
#' @param country_col Character. Name of the country column (optional).
#'
#' @return The input data frame with added weather columns:
#'   `temp_avg`, `precipitation_total`, `wind_avg`, `humidity_avg`,
#'   `is_rain`, `log_precip`, `log_wind`
#' @export
add_weather_features <- function(matches, venue_col = "venue",
                                  date_col = "date",
                                  country_col = NULL) {

  if (nrow(matches) == 0) return(matches)
  if (!venue_col %in% names(matches)) {
    cli::cli_warn("Column '{venue_col}' not found, skipping weather features")
    return(matches)
  }

  venues <- unique(matches[[venue_col]])
  venues <- venues[!is.na(venues) & venues != ""]

  if (length(venues) == 0) {
    cli::cli_warn("No venue data available for weather lookup")
    return(add_empty_weather_cols(matches))
  }

  cli::cli_alert_info("Fetching weather for {length(venues)} venues...")

  # Geocode all venues
  coords <- lapply(venues, function(v) {
    country <- if (!is.null(country_col) && country_col %in% names(matches)) {
      matches[[country_col]][matches[[venue_col]] == v][1]
    } else {
      NULL
    }
    geocode_venue(v, country = country)
  })
  names(coords) <- venues

  # Fetch weather per venue
  date_range <- range(as.Date(matches[[date_col]]), na.rm = TRUE)
  weather_list <- list()

  for (v in venues) {
    coord <- coords[[v]]
    if (is.na(coord$lat) || is.na(coord$lon)) next

    w <- fetch_weather(coord$lat, coord$lon, date_range[1], date_range[2])
    if (nrow(w) > 0) {
      w$venue <- v
      weather_list[[v]] <- w
    }
  }

  if (length(weather_list) == 0) {
    cli::cli_warn("No weather data retrieved for any venue")
    return(add_empty_weather_cols(matches))
  }

  weather_df <- do.call(rbind, weather_list)
  rownames(weather_df) <- NULL

  # Compute features
  weather_df$temp_avg <- (weather_df$temperature_max + weather_df$temperature_min) / 2
  weather_df$precipitation_total <- weather_df$precipitation
  weather_df$wind_avg <- weather_df$wind_speed_max
  weather_df$humidity_avg <- weather_df$humidity_mean
  weather_df$is_rain <- as.integer(!is.na(weather_df$precipitation) & weather_df$precipitation > 1)

  # Select weather columns for join
  weather_merge <- weather_df[, c("venue", "date", "temp_avg", "precipitation_total",
                                    "wind_avg", "humidity_avg", "is_rain")]

  # Join via match() to preserve row order (merge() reorders)
  match_key <- paste(matches[[venue_col]], as.character(as.Date(matches[[date_col]])))
  weather_key <- paste(weather_merge$venue, as.character(as.Date(weather_merge$date)))
  idx <- match(match_key, weather_key)

  for (col in c("temp_avg", "precipitation_total", "wind_avg", "humidity_avg", "is_rain")) {
    matches[[col]] <- weather_merge[[col]][idx]
  }
  result <- matches

  # Impute missing + log transforms
  result <- impute_weather(result)

  geocoded <- sum(!is.na(result$temp_avg))
  coverage <- round(geocoded / nrow(result) * 100, 1)
  cli::cli_alert_success("Weather coverage: {geocoded}/{nrow(result)} matches ({coverage}%)")

  return(result)
}


#' Impute Missing Weather Values
#'
#' Applies median imputation for missing continuous weather values and
#' adds log-transformed features.
#'
#' @param df Data frame with weather columns
#' @return Data frame with imputed values and log transforms
#' @keywords internal
impute_weather <- function(df) {
  # Median imputation for continuous
  for (col in c("temp_avg", "wind_avg", "humidity_avg")) {
    if (col %in% names(df)) {
      med <- stats::median(df[[col]], na.rm = TRUE)
      if (is.na(med)) med <- switch(col, temp_avg = 15, wind_avg = 10, humidity_avg = 65)
      df[[col]][is.na(df[[col]])] <- med
    }
  }

  # Zero imputation for precipitation
  if ("precipitation_total" %in% names(df)) {
    df$precipitation_total[is.na(df$precipitation_total)] <- 0
  }
  if ("is_rain" %in% names(df)) {
    df$is_rain[is.na(df$is_rain)] <- 0L
  }

  # Log transforms
  if ("precipitation_total" %in% names(df)) {
    df$log_precip <- log1p(df$precipitation_total)
  }
  if ("wind_avg" %in% names(df)) {
    df$log_wind <- log1p(df$wind_avg)
  }

  return(df)
}


#' Add Empty Weather Columns
#' @keywords internal
add_empty_weather_cols <- function(df) {
  df$temp_avg <- NA_real_
  df$precipitation_total <- NA_real_
  df$wind_avg <- NA_real_
  df$humidity_avg <- NA_real_
  df$is_rain <- NA_integer_
  df$log_precip <- NA_real_
  df$log_wind <- NA_real_
  impute_weather(df)
}
