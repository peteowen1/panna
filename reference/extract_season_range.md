# Extract season range from a data frame

Extracts min/max season_end_year from a data frame. All data from
pannadata will have this column.

## Usage

``` r
extract_season_range(data, data_name = "data")
```

## Arguments

- data:

  Data frame to extract season range from

- data_name:

  Name of the data for reporting (unused, kept for compatibility)

## Value

Named list with min_season, max_season, and n_seasons
