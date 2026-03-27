# Fetch Weather from Open-Meteo

Fetches daily weather data for a location and date range from the
Open-Meteo archive API (historical) or forecast API (future).

## Usage

``` r
fetch_weather(lat, lon, start_date, end_date)
```

## Arguments

- lat:

  Numeric. Latitude.

- lon:

  Numeric. Longitude.

- start_date:

  Character or Date. Start date (YYYY-MM-DD).

- end_date:

  Character or Date. End date (YYYY-MM-DD).

## Value

Data frame with date, temperature_max, temperature_min, precipitation,
wind_speed_max, humidity_mean columns
