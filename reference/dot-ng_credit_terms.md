# Split each action's value into credit and blame

The **margin convention**. Subdivides every row's `value_home` among
recipients, allocating it ONCE and splitting it across both sides.
[`.ng_credit_terms_team()`](https://peteowen1.github.io/panna/reference/dot-ng_credit_terms_team.md)
is the other convention, which books it twice. Subdivides every row's
`value_home` among recipients. The terms for a row always sum to that
row's `value_home`, so no rule can change a match total – only who is
paid.

## Usage

``` r
.ng_credit_terms(dt, shares)
```

## Arguments

- dt:

  Actions with `value_home`, `action_type`, `result`, recipient columns,
  and optionally `true_possession_change` / `true_next_player` from
  [`ng_build_adjacency()`](https://peteowen1.github.io/panna/reference/ng_build_adjacency.md)
  and `xpass` from
  [`add_xpass_to_spadl()`](https://peteowen1.github.io/panna/reference/add_xpass_to_spadl.md).

- shares:

  Output of
  [`ng_shares()`](https://peteowen1.github.io/panna/reference/ng_shares.md).

## Value

A long data.table of payments with `role` and `play_type` tags.

## Details

Panna's row shape differs from torp's in one way worth stating, because
it changes where the decision term lives.
[`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md)
overrides a shot's EPV with its xG and then recalculates the preceding
row's delta to target that xG. So the value of working the ball into a
shot worth 0.30 instead of 0.05 – the decision term, averaging +0.0716
goals a shot – is already paid on the pass before it. The shot row
carries only `1 - xG` on a goal or `-xG` on a miss, which is the
surprise. This function therefore splits surprises; it does not
reconstruct a decision term that is already allocated elsewhere.
