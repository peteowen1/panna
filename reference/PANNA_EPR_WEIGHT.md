# EPR weight in combined Panna Value rating

Fraction of the combined rating attributed to EPR (play-by-play
EPV-based). The remaining `1 - PANNA_EPR_WEIGHT` goes to PSR
(stat-based). Analogous to torpverse's `TORP_EPR_WEIGHT = 0.5`.

## Usage

``` r
PANNA_EPR_WEIGHT
```

## Format

Numeric value: 0.5

## See also

Other epr:
[`PANNA_PSR_WEIGHT`](https://peteowen1.github.io/panna/reference/PANNA_PSR_WEIGHT.md),
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md),
[`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md),
[`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md),
[`calculate_epr_batch()`](https://peteowen1.github.io/panna/reference/calculate_epr_batch.md),
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md),
[`player_value()`](https://peteowen1.github.io/panna/reference/player_value.md)
