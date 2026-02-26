# Get cached match IDs for a league-season (fast batch version)

Returns all fully-cached match IDs for a league-season. A match is
considered cached if:

1.  All 9 table type files exist (fast path for Big 5 leagues), OR

2.  The metadata has `tables_available` field and all those tables exist

## Usage

``` r
get_cached_fbref_ids(league, season)
```

## Arguments

- league:

  League code

- season:

  Season string

## Value

Character vector of cached fbref_ids

## Details

Uses hierarchical path: `{table_type}/{league}/{season}/{id}.rds`
