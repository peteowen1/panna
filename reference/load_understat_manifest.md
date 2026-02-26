# Load Understat manifest from parquet file

The manifest tracks all scraped match IDs with their league and season.

## Usage

``` r
load_understat_manifest(path)
```

## Arguments

- path:

  Path to manifest parquet file

## Value

Data frame with columns: match_id, league, season, scraped_at
