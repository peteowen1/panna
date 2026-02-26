# Load match table from cache

Loads from hierarchical path: `{table_type}/{league}/{season}/{id}.rds`

## Usage

``` r
load_match_table(league, season, fbref_id, table_type)
```

## Arguments

- league:

  League code

- season:

  Season string

- fbref_id:

  FBref match ID

- table_type:

  Type of table

## Value

Data frame or NULL if not cached
