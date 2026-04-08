# Get efficiency stat columns to exclude from PSV

Returns the subset of
[`.get_psr_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_psr_skill_cols.md)
that are efficiency/ratio stats. These are excluded from PSV because
they are ratios (not additive counts) and are redundant when their
numerator and denominator are already included as rate stats.

## Usage

``` r
.get_psr_efficiency_cols()
```

## Value

Character vector
