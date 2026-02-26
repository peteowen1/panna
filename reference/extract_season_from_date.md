# Extract Season from Match Date

Determines the season string (e.g., "2024-2025") from a match date.
Assumes seasons run August to May: Aug-Dec = first year, Jan-May =
second year.

## Usage

``` r
extract_season_from_date(date)
```

## Arguments

- date:

  Date or character date string (YYYY-MM-DD format)

## Value

Character season string (e.g., "2024-2025")
