# Classify stats into rate vs efficiency categories

Per-90 rate stats are weighted by minutes; efficiency stats (success
rates, conversion rates) are weighted by their denominator (attempts).
This function returns the classification and the denominator column for
efficiency stats.

## Usage

``` r
.classify_skill_stats()
```

## Value

A data.table with columns: stat, category, denominator
