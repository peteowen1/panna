# Default penalty kick xG value

xG override for penalty kicks, applied in
[`add_xg_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xg_to_spadl.md)
to shots flagged `is_penalty` (Opta qualifier 9). The xG model is
trained with penalties excluded (`exclude_penalties = TRUE`), so without
this override a penalty scores like a contested ~12m open-play shot
(~0.23).

## Usage

``` r
PENALTY_XG
```

## Format

Numeric value: 0.7694

## Details

**Re-derived 2026-09-03 on the full corpus: 0.80 -\> 0.7694.** The
previous value came from ENG 2021-24 only, 251/306 = 0.82, and was
rounded to 0.80 as "a robust central value". Measured across every
league and season in `opta_shot_events.parquet` the rate is **39,916 /
51,881 = 0.7694**, 95% CI **0.7657 to 0.7730** – 170x the sample, and
**0.80 falls outside the interval**, so the old value overrated every
penalty by about 4%. (Square brackets around the interval would be
parsed by roxygen as a link.)

It is stable enough to stay a single constant rather than becoming a
model feature: by year 0.751-0.806 with no trend (2014-2026), by league
0.74-0.81 across the twelve highest-volume competitions. Penalties
remain EXCLUDED from xG training (`exclude_penalties = TRUE`) – every
penalty is taken from the same spot, so there is nothing for the
geometry features to learn, and a measured constant is the right shape
for it.

Worth noting the old 0.80 exactly matched Opta's own penalty xG, which
is what a copied constant looks like rather than a measured one.

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
[`PLAYERS_PER_TEAM`](https://peteowen1.github.io/panna/reference/PLAYERS_PER_TEAM.md),
[`SIX_YARD_X_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_X_MIN.md),
[`SIX_YARD_Y_MAX`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MAX.md),
[`SIX_YARD_Y_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MIN.md),
[`TOUCHES_PER_SEQUENCE`](https://peteowen1.github.io/panna/reference/TOUCHES_PER_SEQUENCE.md),
[`XG_MAX`](https://peteowen1.github.io/panna/reference/XG_MAX.md),
[`XG_MIN`](https://peteowen1.github.io/panna/reference/XG_MIN.md)

## Examples

``` r
PENALTY_XG
#> [1] 0.7694
```
