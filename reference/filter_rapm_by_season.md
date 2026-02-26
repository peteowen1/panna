# Filter RAPM data to specific seasons

Subsets RAPM data to include only splints from specified seasons.

## Usage

``` r
filter_rapm_by_season(rapm_data, seasons, match_info)
```

## Arguments

- rapm_data:

  List from prepare_rapm_data

- seasons:

  Character vector of seasons to include

- match_info:

  Match info data frame with season column

## Value

Filtered rapm_data
