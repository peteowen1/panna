# Mirror match rows (swap home/away perspective)

Produces the home/away-swapped version of a set of match rows. Every
`home_*` column is exchanged with its `away_*` partner; signed
"home-perspective" columns (`*_diff`, `diff_*`, `home_field`,
`pred_goal_diff`) are negated; and the result/outcome labels are
flipped. Symmetric quantities (`pred_total_goals`, league dummies,
`is_neutral_venue`, `match_month`, ...) are left unchanged.

## Usage

``` r
mirror_match_rows(df)
```

## Arguments

- df:

  A data.frame (or data.table) of match rows. Any subset of the standard
  match-dataset columns is accepted; only the columns that are present
  are transformed.

## Value

A data.frame of identical shape and column order with the home/away
perspective swapped.

## Details

Used in two places:

- Steps 05/06 – append `rbind(train, mirror_match_rows(train))` so the
  goals and outcome models train on both orientations.

- Step 07 – predict each fixture in both orientations and average,
  giving a prediction invariant to which team is listed as home.
