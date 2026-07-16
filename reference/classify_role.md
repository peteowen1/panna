# Map raw Opta (position, side) pair to a 16-role taxonomy

Granular roles capture position-specific sub patterns the broad
GK/DEF/MID/FWD bucketing washes out (e.g. RBs are subbed off 18% more
often than CBs; AMs come off twice as often as CBs).

## Usage

``` r
classify_role(position, side)
```

## Arguments

- position:

  Character vector of `position` column from lineups.

- side:

  Character vector of `position_side` column.

## Value

Character vector of role codes: GK, CB, LB, RB, LWB, RWB, DM, CM, LM,
RM, CAM, LW, RW, CF, LF, RF (or `"UNK"` for blanks / "Substitute" /
unrecognized).

## See also

Other expected minutes:
[`build_team_expected_minutes()`](https://peteowen1.github.io/panna/reference/build_team_expected_minutes.md),
[`predict_minutes()`](https://peteowen1.github.io/panna/reference/predict_minutes.md),
[`prepare_minutes_cache()`](https://peteowen1.github.io/panna/reference/prepare_minutes_cache.md),
[`query_minutes_features()`](https://peteowen1.github.io/panna/reference/query_minutes_features.md)
