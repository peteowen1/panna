# Canonical SPM-Opta predictor selection

The ONE place the Opta SPM feature set is defined, shared by
[`fit_spm_opta()`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md)
(glmnet half) and passed explicitly to
[`fit_spm_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_xgb.md)
by `05_spm.R` (XGBoost half) so the two halves of the shipped 50/50
blend can never train on divergent feature sets — the failure mode
behind the "SPM was xG-blind" bug, where the enrichment join was dead
code because the fit-time grep didn't match the joined column names.

## Usage

``` r
.spm_opta_predictor_cols(data)
```

## Arguments

- data:

  Data frame of candidate features

## Value

Character vector of predictor column names present in `data`

## Details

Selection: all per-90 rates, BOTH suffix spellings (`_p90` box-score,
`_per90` xMetrics model outputs) + the `_xmetrics`-suffixed pair + the
kept efficiency ratios + position dummies. The ratios with a direct
above-expected replacement were REMOVED (mirrors the PSR/PSV redesign,
panna#116): duel/aerial/tackle success -\> the five \*\_woe_per90
counts; goals_per_shot / big_chance_conversion / headed_goal_rate /
ibox/obox_goal_rate / penalty_conversion -\> the finishing
over-performance family (scale-free ratios discard volume: 1/1 ==
10/10). Ratios WITHOUT a modeled replacement (zone pass accuracies, bad
touches, 50/50s, possession control) are kept.
