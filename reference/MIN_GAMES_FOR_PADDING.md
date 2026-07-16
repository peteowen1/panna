# Default minimum games for Bayesian padding full weight

Number of games required for a player's statistics to receive full
weight in Bayesian padding calculations. Players with fewer games are
regressed toward population mean.

## Usage

``` r
MIN_GAMES_FOR_PADDING
```

## Format

Integer value: 10

## See also

Other constants:
[`BETA_PRIOR_ALPHA`](https://peteowen1.github.io/panna/reference/BETA_PRIOR_ALPHA.md),
[`CHAIN_TIME_GAP_SECONDS`](https://peteowen1.github.io/panna/reference/CHAIN_TIME_GAP_SECONDS.md),
[`CONFIDENCE_LEVEL`](https://peteowen1.github.io/panna/reference/CONFIDENCE_LEVEL.md),
[`HALFTIME_MINUTE`](https://peteowen1.github.io/panna/reference/HALFTIME_MINUTE.md),
[`MINUTES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MINUTES_PER_MATCH.md),
[`MIN_MINUTES_FEATURES`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_FEATURES.md),
[`MIN_MINUTES_RAPM`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_RAPM.md),
[`MIN_MINUTES_SPM`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_SPM.md),
[`MIN_SEQUENCES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MIN_SEQUENCES_PER_MATCH.md),
[`MIN_SHOTS_FOR_FINISHING`](https://peteowen1.github.io/panna/reference/MIN_SHOTS_FOR_FINISHING.md),
[`MIN_WEIGHT_DURATION`](https://peteowen1.github.io/panna/reference/MIN_WEIGHT_DURATION.md),
[`PENALTY_SHOOTOUT_CONVERSION`](https://peteowen1.github.io/panna/reference/PENALTY_SHOOTOUT_CONVERSION.md),
[`PENALTY_XG`](https://peteowen1.github.io/panna/reference/PENALTY_XG.md),
[`PLAYERS_PER_TEAM`](https://peteowen1.github.io/panna/reference/PLAYERS_PER_TEAM.md),
[`SIX_YARD_X_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_X_MIN.md),
[`SIX_YARD_Y_MAX`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MAX.md),
[`SIX_YARD_Y_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MIN.md),
[`TOUCHES_PER_SEQUENCE`](https://peteowen1.github.io/panna/reference/TOUCHES_PER_SEQUENCE.md),
[`XG_MAX`](https://peteowen1.github.io/panna/reference/XG_MAX.md),
[`XG_MIN`](https://peteowen1.github.io/panna/reference/XG_MIN.md)

## Examples

``` r
MIN_GAMES_FOR_PADDING
#> [1] 10
```
