# Add Weather Features to Match Data

Joins weather data to a match data frame. Fetches weather for each
unique venue+date combination, then merges back.

## Usage

``` r
add_weather_features(
  matches,
  venue_col = "venue",
  date_col = "date",
  country_col = NULL
)
```

## Arguments

- matches:

  Data frame with at least `date`, `venue` (or `stadium`), and
  optionally `country` columns.

- venue_col:

  Character. Name of the venue column. Default "venue".

- date_col:

  Character. Name of the date column. Default "date".

- country_col:

  Character. Name of the country column (optional).

## Value

The input data frame with added weather columns: `temp_avg`,
`precipitation_total`, `wind_avg`, `humidity_avg`, `is_rain`,
`log_precip`, `log_wind`
