# Each shooter's earlier foot shots, for the weak-foot input

For every (player, match): how many foot shots the player took in
EARLIER matches (by match date; same-day matches excluded) and how many
of those were right-footed. `foot_share` for a shot is then the share of
those earlier foot shots taken with the foot used for this shot.

## Usage

``` r
.shot_foot_history(shot_events)
```

## Arguments

- shot_events:

  Opta shot events with `player_id`, `match_id`, `body_part` (RightFoot
  / LeftFoot / Head / ...) and `match_date`.

## Value

data.table: `player_id`, `match_id`, `r_prev`, `n_prev`.
