# Play type for a net goals payment

Labels one payment row of the ledger (after
[`ng_spread_pools()`](https://peteowen1.github.io/panna/reference/ng_spread_pools.md))
by its role and play type. Role decides first, because one play type
pays different people: on a shot, the shooter is paid for the strike and
the keeper named on the finish. These are the columns of the "Where Net
Goals Come From" artifact
(`data-raw/epv/net-goals/build_net_goals_artifacts.R`).

## Usage

``` r
.ng_play_type(play_type, role)
```

## Arguments

- play_type, role:

  Columns of the payment table.

## Value

Character vector of category labels.
