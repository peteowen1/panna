# Per-match period ends on the raw Opta clock

First-half end from the period-1 type 30 marker (else the last period-1
event), second-half end from the period-2 marker (else the last period-2
event).

## Usage

``` r
.raw_period_ends(dt)
```

## Arguments

- dt:

  data.table with match_id, type_id, period_id, eff_minute.

## Value

data.table match_id, fh_end, sh_end_raw.
