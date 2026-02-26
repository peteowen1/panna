# Save match table to cache

Saves to hierarchical path: `{table_type}/{league}/{season}/{id}.rds`

## Usage

``` r
save_match_table(data, league, season, fbref_id, table_type)
```

## Arguments

- data:

  Data frame to save

- league:

  League code

- season:

  Season string

- fbref_id:

  FBref match ID

- table_type:

  Type of table

## Value

Invisible path to saved file
