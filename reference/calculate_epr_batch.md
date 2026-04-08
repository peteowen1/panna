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
