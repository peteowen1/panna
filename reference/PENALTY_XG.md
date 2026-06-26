# Default penalty kick xG value

xG override for penalty kicks, applied in
[`add_xg_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xg_to_spadl.md)
to shots flagged `is_penalty` (Opta qualifier 9). The xG model is
trained with penalties excluded (`exclude_penalties = TRUE`), so without
this override a penalty scores like a contested ~12m open-play shot
(~0.23). Empirical: ENG 2021-24 = 251/306 = 0.82 (thin, seasonal range
0.74-0.90); long-run top-flight ~0.78. 0.80 is a robust central value.

## Usage

``` r
PENALTY_XG
```

## Format

Numeric value: 0.80

## Examples

``` r
PENALTY_XG
#> [1] 0.8
```
