# Calculate derived features (ratios and success rates)

Internal helper to add derived features to player data frame.

## Usage

``` r
.calculate_derived_features(player_stats, mins_per_90)
```

## Arguments

- player_stats:

  Data frame with aggregated player stats

- mins_per_90:

  Numeric vector of minutes/90 for each player

## Value

player_stats with derived feature columns added
