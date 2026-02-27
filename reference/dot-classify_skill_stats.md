# Get efficiency stat denominator mapping

Returns a named list mapping efficiency stat column names to their
denominator column names. Stats not in this list are treated as per-90
rate stats (weighted by minutes).

## Usage

``` r
.classify_skill_stats()
```

## Value

A named list mapping efficiency stat names to denominator column names
