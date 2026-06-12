# Test whether period_id values are penalty-shootout periods

Shootout kicks are recorded as goals (`type_id == 16`) at minute 120 but
are not open play: they must be excluded from match scores, SPADL, EPV
and WPA. A match decided on penalties is a draw in open play (its WP
label is 0.5). Any `period_id >= 5` is treated as shootout — no
legitimate open-play period exceeds 4 (covers the standard 5 and a stray
16 some feeds emit).

## Usage

``` r
is_shootout_period(period_id)
```

## Arguments

- period_id:

  Integer vector of Opta period identifiers.

## Value

Logical vector, `TRUE` where the period is a shootout period.
