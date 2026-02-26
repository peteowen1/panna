# Convert statistics to per-100-sequences rate

Normalizes counting statistics to a per-100-sequences basis, similar to
basketball's per-100-possessions.

## Usage

``` r
calculate_per_100_sequences(player_stats, team_sequences, stat_cols = NULL)
```

## Arguments

- player_stats:

  Data frame of player match stats

- team_sequences:

  Data frame of team sequences per match

- stat_cols:

  Character vector of columns to convert

## Value

Data frame with rate statistics
