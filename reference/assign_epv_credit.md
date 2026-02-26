# Assign EPV Credit with Turnover Handling

Assigns EPV credit/blame to players. For turnovers (possession changes),
splits the value between the player who lost the ball and who gained it.
For merged duels (Aerials, Take On vs Tackle), assigns zero-sum credit
where winner gain = loser loss.

## Usage

``` r
assign_epv_credit(spadl_with_epv, xpass_model = NULL)
```

## Arguments

- spadl_with_epv:

  SPADL actions with EPV values from calculate_action_epv()

- xpass_model:

  Optional xPass model for pass difficulty weighting

## Value

SPADL actions with credit columns added:

- player_credit: EPV credit to acting player

- receiver_credit: EPV credit to receiver (for turnovers, this is
  positive)

- opponent_credit: EPV credit to duel loser (negative, via
  opponent_player_id)

- xpass: Pass completion probability (for passes)
