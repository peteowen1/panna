# Adjust Player EPV Credits for Opponent Strength

Residual-based opponent adjustment. For each match, computes how much a
team over/underperformed their season average, then attributes that
residual to the opponent they faced. Uses decay-weighted rolling
profiles with Bayesian shrinkage toward league average (0).

## Usage

``` r
adjust_epv_for_opponents(
  player_match,
  credit_col = "total_credit",
  lambda_decay = EPV_OPP_LAMBDA_DECAY,
  prior_games = EPV_OPP_PRIOR_GAMES
)
```

## Arguments

- player_match:

  data.table with columns: player_id, match_id, team_id, match_date,
  minutes_played, total_credit (or column specified by credit_col)

- credit_col:

  Name of the credit column to use for team totals. Default
  "total_credit".

- lambda_decay:

  Exponential decay rate for opponent profiles. Default
  EPV_OPP_LAMBDA_DECAY (0.003, ~231-day half-life).

- prior_games:

  Pseudo-games for Bayesian shrinkage toward league avg. Default
  EPV_OPP_PRIOR_GAMES (2).

## Value

Same data.table with opp_adjustment and player_opp_adj columns added

## Details

Adjustment is distributed to players by minutes share (proportional to
playing time within the match).
