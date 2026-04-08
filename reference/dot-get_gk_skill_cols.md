# Get GK-specific PSR skill feature column names

Returns the per-90 rate and efficiency columns used for the GK
sub-model. GK model uses goal differential (not xG diff) as target, so
GK action stats like save percentage can have meaningful signal.

## Usage

``` r
.get_gk_skill_cols()
```

## Value

Character vector of column names
