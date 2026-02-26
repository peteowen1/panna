# Apply Bayesian padding to statistics

Regresses statistics toward population mean for players with few games.
This prevents extreme ratings from small sample sizes.

## Usage

``` r
apply_bayesian_padding(
  player_stats,
  stat_cols,
  min_games = 10,
  weight_col = "n_games"
)
```

## Arguments

- player_stats:

  Data frame with player statistics

- stat_cols:

  Character vector of columns to pad

- min_games:

  Games required for full weight (default 10)

- weight_col:

  Column containing games/minutes played

## Value

Data frame with Bayesian-padded statistics
