# List cached matches

Scans the hierarchical directory structure:
`{table_type}/{league}/{season}/{id}.rds`

## Usage

``` r
list_cached_matches(table_type = "metadata", league = NULL, season = NULL)
```

## Arguments

- table_type:

  Table type to check

- league:

  Optional league filter

- season:

  Optional season filter

## Value

Data frame with league, season, fbref_id columns
