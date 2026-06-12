# Empirical penalty-shootout conversion rate

Per-kick conversion probability in a penalty shootout, measured from
local Opta data: 900 goals / 1200 shootout kicks = 0.75 across 116
shootouts (cross-validates the literature consensus of ~0.75-0.76).
Distinct from `PENALTY_XG` (in-run penalty xG): shootout kicks are a
different, higher-pressure context, even though the rates happen to be
close. Default conversion rate for
[`shootout_win_prob`](https://peteowen1.github.io/panna/reference/shootout_win_prob.md).

## Usage

``` r
PENALTY_SHOOTOUT_CONVERSION
```

## Format

Numeric value: 0.75

## Examples

``` r
PENALTY_SHOOTOUT_CONVERSION
#> [1] 0.75
```
