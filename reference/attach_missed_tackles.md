# Turn Missed Tackles Into Duel Blame

Opta type 83 is a defender's missed tackle: the attacking team keeps the
ball (panna#532, decided 2026-11-27). Kept as an action it would hand
the defending team the ball for one row, so the passer just before it is
blamed for a turnover that never happened. Instead the defender is
attached as the duel loser (`opponent_player_id`, flagged
`opponent_from_83`) of the attacker's next action, and the type-83 row
is dropped. The blame itself is applied in
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md).

## Usage

``` r
attach_missed_tackles(dt, window = 3, look_ahead = 5L)
```

## Arguments

- dt:

  SPADL data.table after
  [`merge_duel_rows()`](https://peteowen1.github.io/panna/reference/merge_duel_rows.md).

- window:

  Seconds to look ahead for the attacker's next action.

- look_ahead:

  Rows to look ahead.

## Value

`dt` without type-83 rows, with `opponent_from_83`.
