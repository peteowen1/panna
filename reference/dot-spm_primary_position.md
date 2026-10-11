# Primary position per player: modal position over the games they STARTED

Opta's `position` is the role in that match, and every bench appearance
(used or not) reads `"Substitute"`. Counting those made "Substitute" the
modal label for 53\\ got no position dummy, and half the keepers who
started in goal (21 of 42 EPL, 22 of 44 La Liga) were not flagged
`is_gk` (review R010). This counts only rows with minutes played and a
real position. A player who only ever came off the bench gets no row (NA
after the join), as before.

## Usage

``` r
.spm_primary_position(dt, minutes_col)
```

## Arguments

- dt:

  data.table with `player_id` and `position`.

- minutes_col:

  Candidate minutes column names, first present wins (rows with no
  minutes are unused-bench rows). If none is present, every row counts.

## Value

data.table with `player_id`, `primary_position`.
