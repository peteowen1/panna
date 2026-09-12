# Minimum weighted 90s to qualify for a PSR leaderboard (panna#215)

PSR rankings apply no minutes floor, so short partial seasons sit
alongside full campaigns: **23 of the outfield top 100 have fewer than
15 weighted nineties and 11 fewer than 10** — F. Totti at 7.0, M.
Pinilla at 5.5, J. Pastore at 9.7. This was the dominant driver of
leaderboard noise, bigger than the cross-league offsets it is often
blamed on (weak leagues are actually *under*-represented in the top 100:
91 of 100 are Big-5).

## Usage

``` r
MIN_90S_PSR_LEADERBOARD
```

## Format

Numeric value: 15

## Details

15 removes every sub-threshold entry currently in the outfield top 100;
10 would remove 11 of 23. Chosen at 15 as roughly half a domestic
season, the conventional qualification bar.

This is a DISPLAY threshold, not a rating filter — a low-minutes rating
is still the best estimate available for that player, it is just too
noisy to rank against a full season. Use
[`psr_leaderboard_eligible`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md)
and keep the rows.

## See also

Other constants:
[`BETA_PRIOR_ALPHA`](https://peteowen1.github.io/panna/reference/BETA_PRIOR_ALPHA.md),
[`CHAIN_TIME_GAP_SECONDS`](https://peteowen1.github.io/panna/reference/CHAIN_TIME_GAP_SECONDS.md),
[`CONFIDENCE_LEVEL`](https://peteowen1.github.io/panna/reference/CONFIDENCE_LEVEL.md),
[`HALFTIME_MINUTE`](https://peteowen1.github.io/panna/reference/HALFTIME_MINUTE.md),
[`MINUTES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MINUTES_PER_MATCH.md),
[`MIN_GAMES_FOR_PADDING`](https://peteowen1.github.io/panna/reference/MIN_GAMES_FOR_PADDING.md),
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
MIN_90S_PSR_LEADERBOARD
#> [1] 15
```
