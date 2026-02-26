# Aggregate a single stat table by player_id

Internal helper to aggregate match-level stats to player totals.

## Usage

``` r
.aggregate_stat_table(stats_df, col_mapping)
```

## Arguments

- stats_df:

  Data frame with player stats

- col_mapping:

  Named vector mapping output col names to input col names

## Value

Data frame aggregated by player_id with renamed columns
