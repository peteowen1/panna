# WC2026 Reference Facts – Domain Assertions on Pipeline Outputs

WC2026 Reference Facts – Domain Assertions on Pipeline Outputs

## Usage

``` r
WC2026_REFERENCE_FACTS
```

## Format

Named list of `list(fact, check_team_strength)` where `fact` is a
one-line rationale string and `check_team_strength` is a function
applied to `wc2026_team_strength.parquet` returning TRUE iff the fact
holds.
