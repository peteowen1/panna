# Derive Expected Assists (xA) from SPADL Actions

For each shot in SPADL data, checks if the preceding action was a
completed pass by a teammate. If so, credits the passer with the shot's
xG as xA.

## Usage

``` r
derive_xa(spadl_actions)
```

## Arguments

- spadl_actions:

  SPADL actions data frame with xG column already added. Must contain:
  action_id, action_type, result, team_id, player_id, player_name,
  match_id, xg.

## Value

The same SPADL data frame with additional columns:

- xa: Expected assists value (shot's xG credited to passer)

- is_key_pass: Whether this pass led to a shot

- is_assist: Whether this pass led to a goal
