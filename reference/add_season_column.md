# Add season_end_year column to a data frame based on match_url lookup

Joins season information from match results to any data frame with
match_url.

## Usage

``` r
add_season_column(data, match_season_lookup)
```

## Arguments

- data:

  Data frame to add season to

- match_season_lookup:

  Named vector mapping match_url to season_end_year

## Value

Data frame with season_end_year column added
