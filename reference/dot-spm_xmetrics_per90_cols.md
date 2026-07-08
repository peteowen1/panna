# Canonical list of above-expected xMetrics columns SPM enrichment can produce

The full set of `*_per90` column names
[`.aggregate_xmetrics_for_spm()`](https://peteowen1.github.io/panna/reference/dot-aggregate_xmetrics_for_spm.md)
MAY produce, independent of which are derivable from a given xmetrics
vintage. Callers that need to guarantee a fixed column set on the output
data frame (e.g. so a model's `predictor_cols` always resolves, even for
a season/subset with zero coverage of some columns) should ensure
exactly this list exists, defaulting missing ones to 0 — see
[`.spm_opta_predictor_cols()`](https://peteowen1.github.io/panna/reference/dot-spm_opta_predictor_cols.md)
for why 0 is the correct fallback (population mean for mean-zero
above-expected metrics).

## Usage

``` r
.spm_xmetrics_per90_cols()
```

## Value

Character vector of canonical `*_per90` column names
