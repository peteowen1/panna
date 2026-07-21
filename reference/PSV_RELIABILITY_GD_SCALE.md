# Reliability-shrunk PSV display scale ("expected GD contribution per 90")

Multiplies the lambda-priced display PSV
(`calculate_psv(reliability = )`) so its units become "expected
goal-difference contribution per 90". Derived 2026-07-20
(LIVE-PSV-UNBLOCK D1-v2 FINAL) by regressing match goal difference on
minutes-weighted team sums of lambda-priced player PSV (13,548 matches,
R-squared = 0.31, t ~= 59) – the slope makes summed player PSVs predict
match GD with slope 1. ONE global constant for BOTH the outfield and GK
populations: the GK-specific GD coefficient (c ~= 25, fit by the same
regression as a separate GK term) was REJECTED as pricing the \#159
team-context leak in the GK reliability artifact, not genuine keeper
skill – GKs use this same constant until \#159 retrains. Re-derive via
`data-raw/estimated-skills/07d_derive_psv_gd_scale.R` after any retrain
of `psv_match_reliability.csv` (07b) or the PSR/PSV coefficients
(07_train_psr_model.R), then update this value by hand.

## Usage

``` r
PSV_RELIABILITY_GD_SCALE
```

## Format

Numeric value: 5.134

## See also

Other psr:
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
