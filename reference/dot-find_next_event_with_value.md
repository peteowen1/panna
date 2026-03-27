# Find the next event with extra columns via non-equi rolling join

Like
[`.find_next_event()`](https://peteowen1.github.io/panna/reference/dot-find_next_event.md)
but also carries through an extra numeric column (e.g., shot_xg) from
the events table.

## Usage

``` r
.find_next_event_with_value(dt, events_dt, extra_col)
```

## Arguments

- dt:

  data.table of SPADL actions

- events_dt:

  data.table of target events. Must have columns: match_id, period_id,
  event_time, event_team, plus `extra_col`

- extra_col:

  Name of the additional column to carry through

## Value

dt with next_event_team and next\_{extra_col} columns added
