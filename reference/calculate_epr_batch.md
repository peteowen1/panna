# Calculate EPR at multiple dates (batch version)

Efficiently computes EPR ratings at multiple reference dates using the
cumsum trick for O(N + D \* players) instead of O(N \* D) complexity.

## Usage

``` r
calculate_epr_batch(player_game_epv, ref_dates, ...)
```

## Arguments

- player_game_epv:

  Per-game EPV data.

- ref_dates:

  Character or Date vector of reference dates.

- ...:

  Additional parameters passed to
  [`calculate_epr`](https://peteowen1.github.io/panna/reference/calculate_epr.md).

## Value

A data.table with columns from `calculate_epr` plus `ref_date`.

## See also

Other epr:
[`PANNA_EPR_WEIGHT`](https://peteowen1.github.io/panna/reference/PANNA_EPR_WEIGHT.md),
[`PANNA_PSR_WEIGHT`](https://peteowen1.github.io/panna/reference/PANNA_PSR_WEIGHT.md),
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md),
[`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md),
[`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md),
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md),
[`player_value()`](https://peteowen1.github.io/panna/reference/player_value.md)
