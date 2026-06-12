# International blend weight

Weight on the international-specialist model when predicting
international (national-team) matches; the remainder is on the pooled
(all-data) model. The prediction is
`w * international + (1 - w) * pooled`.

## Usage

``` r
MATCH_INTL_BLEND_WEIGHT
```

## Format

Numeric value: 0.5

## Details

A blend-weight sweep on held-out international games found accuracy
improves monotonically toward `w = 1` (pure specialist), but only by
~0.6\\ The default 0.5 trades that small edge for robustness against the
smaller-sample specialist model misbehaving on out-of-distribution
squads.
