# Process advanced match stats

Cleans advanced player stats data. Uses data.table for speed with large
datasets.

## Usage

``` r
process_advanced_stats(stats, results, stat_type = "summary")
```

## Arguments

- stats:

  Data frame of advanced match stats

- results:

  Processed match results for match IDs

- stat_type:

  The type of stats being processed

## Value

Cleaned data frame with advanced stats
