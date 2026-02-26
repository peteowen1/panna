# Calculate per-90 rates for player stats

Internal helper to add per-90 stats to player data frame.

## Usage

``` r
.calculate_per90_rates(player_stats, mins_per_90)
```

## Arguments

- player_stats:

  Data frame with aggregated player stats

- mins_per_90:

  Numeric vector of minutes/90 for each player

## Value

player_stats with per-90 columns added
