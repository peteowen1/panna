# Zero-xG threshold for Opta pipeline data quality filter

Maximum percentage of zero-xG splints allowed before a match is flagged
as bad data. Opta data via SPADL conversion naturally has ~25% zero-xG
splints, so the threshold is set higher than FBref. Used in
[`filter_bad_xg_data()`](https://peteowen1.github.io/panna/reference/filter_bad_xg_data.md).

## Usage

``` r
ZERO_XG_THRESHOLD_OPTA
```

## Format

Integer value: 30
