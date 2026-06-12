# Decay-weighted strictly-past sum for one ordered series

`S_i = sum_{j < i} value_j * 0.5 ^ ((date_i - date_j) / half_life)` via
the incremental recurrence
`S_i = (S_{i-1} + value_{i-1}) * q ^ (date_i - date_{i-1})`, so it runs
in O(n) per player.

## Usage

``` r
.decay_past_sum(value, date_int, half_life)
```
