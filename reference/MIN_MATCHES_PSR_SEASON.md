# Minimum matches in a season for it to enter a cross-season PSR display

Opta box-score coverage ramps up over its first seasons: the lineup rows
exist for every match, but the per-player stats PSR is built from are
populated for only a fraction of them. Counting matches with real player
stats (\>= 10 minutes played) over the rating and bridge competitions in
`opta_player_stats.parquet` (measured 2026-10-18):

## Usage

``` r
MIN_MATCHES_PSR_SEASON
```

## Format

Integer value: 4000

## Details

|        |         |            |
|--------|---------|------------|
| season | matches | with stats |
| 2013   | 5,396   | 499        |
| 2014   | 6,183   | 822        |
| 2015   | 6,547   | 2,663      |
| 2016   | 6,353   | 5,456      |
| 2017   | 6,542   | 6,031      |

So the thin seasons are genuinely thin Opta data, not a pipeline filter
(the raw feed has all 376 EPL matches for 2014, but only 45 carry player
stats). A season with a handful of European matches puts that one club
at the top of any cross-season table – 2014 was swept by Arsenal. Every
season from 2016 on clears 5,400; 2015 (2,663) and earlier sit below
1,000-2,700. 4,000 splits the two groups with room either side.

A DISPLAY threshold, like
[`MIN_90S_PSR_LEADERBOARD`](https://peteowen1.github.io/panna/reference/MIN_90S_PSR_LEADERBOARD.md):
the ratings are kept, only flagged. See
[`psr_season_covered`](https://peteowen1.github.io/panna/reference/psr_season_covered.md).

## See also

Other constants:
[`BETA_PRIOR_ALPHA`](https://peteowen1.github.io/panna/reference/BETA_PRIOR_ALPHA.md),
[`CHAIN_TIME_GAP_SECONDS`](https://peteowen1.github.io/panna/reference/CHAIN_TIME_GAP_SECONDS.md),
[`CONFIDENCE_LEVEL`](https://peteowen1.github.io/panna/reference/CONFIDENCE_LEVEL.md),
[`HALFTIME_MINUTE`](https://peteowen1.github.io/panna/reference/HALFTIME_MINUTE.md),
[`MINUTES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MINUTES_PER_MATCH.md),
[`MIN_90S_PSR_LEADERBOARD`](https://peteowen1.github.io/panna/reference/MIN_90S_PSR_LEADERBOARD.md),
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
MIN_MATCHES_PSR_SEASON
#> [1] 4000
```
