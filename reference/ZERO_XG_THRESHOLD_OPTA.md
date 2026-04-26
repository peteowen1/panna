# Zero-xG threshold for Opta pipeline data quality filter

Maximum percentage of zero-xG splints allowed before a match is flagged
as bad data. Opta data via SPADL conversion naturally has ~25% zero-xG
splints, so the threshold is set higher than FBref. Used in
[`filter_bad_xg_data()`](https://peteowen1.github.io/panna/reference/filter_bad_xg_data.md).
Raised from 30 → 50 on 2026-04-18: with second-precision splint creation
and 5-min boundary-merge minimum, the per-splint zero-xG baseline rose
(shorter splints naturally have fewer shots). 50% only catches genuine
bad-data league-seasons rather than penalising fine-grained splits.

## Usage

``` r
ZERO_XG_THRESHOLD_OPTA
```

## Format

Integer value: 50
