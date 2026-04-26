# Aggregate Player EPV Per Game

Like
[`aggregate_player_epv`](https://peteowen1.github.io/panna/reference/aggregate_player_epv.md)
but groups by `(player_id, match_id)` to produce one row per player per
match. Includes offensive/defensive decomposition, per-90 rates, and
optional position-centering.

## Usage

``` r
aggregate_player_game_epv(
  spadl_with_epv,
  lineups = NULL,
  position_center = FALSE
)
```

## Arguments

- spadl_with_epv:

  SPADL actions with EPV and credit columns from
  [`assign_epv_credit`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md).

- lineups:

  Optional lineup data with `player_id`, `match_id`, `minutes_played`,
  and optionally `position`.

- position_center:

  Logical; subtract position-group mean per season to produce `epv_adj`
  columns. Requires lineups with `position`. Default `FALSE`.

## Value

A data.table with one row per player per match:

- player_id, player_name, team_id, match_id:

  Identifiers

- n_actions:

  Number of SPADL actions by this player in this match

- epv_total:

  Total EPV = actor + receiver + duel_blame

- epv_offensive:

  Offensive EPV = passing + shooting + dribbling + aerial + keeping +
  receiver credit

- epv_defensive:

  Defensive EPV = defending + duel_blame

- epv_as_actor, epv_as_receiver, epv_duel_blame:

  Credit source breakdown

- epv_passing:

  Outfield passing + ball touches

- epv_shooting:

  Shot credit (xG-weighted)

- epv_dribbling:

  Ground take-on attempts

- epv_aerial:

  Aerial duel credit (winner + / loser via duel_blame)

- epv_keeping:

  Keeper pick-up, claim, punch (distribution/handling)

- epv_defending:

  Tackles, interceptions, clearances, ball recoveries, keeper saves,
  fouls won, dispossessed events

- minutes_played:

  Minutes played (if lineups provided)

- epv_p90, epv_offensive_p90, ...:

  Per-90 rates (if lineups provided)

- epv_adj:

  Position-centered EPV (if `position_center = TRUE`)
