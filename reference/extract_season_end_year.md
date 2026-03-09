# Extract season end year from a season string

Handles both "YYYY-YYYY" format (returns second year) and tournament
"YYYY Country" format (returns the year).

## Usage

``` r
extract_season_end_year(season)
```

## Arguments

- season:

  Season string (e.g., "2023-2024" or "2018 Russia")

## Value

Numeric end year, or NA_real\_ if unparseable
