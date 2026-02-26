# Resolve substitute positions using each player's modal starting position

Players listed as "Substitute" get their most common starting position
from other matches. Players who only ever appear as Substitute are left
as NA.

## Usage

``` r
.resolve_positions(dt)
```

## Arguments

- dt:

  A data.table with player_id and position columns.

## Value

The input data.table with an added `pos_group` column.
