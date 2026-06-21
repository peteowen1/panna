# Update Elo Ratings After a Match

Updates Elo ratings for home and away teams based on match result. Uses
standard Elo formula with configurable K-factor and home advantage.

## Usage

``` r
update_elo(
  home_elo,
  away_elo,
  home_goals,
  away_goals,
  k = 20,
  home_advantage = 88,
  update_mode = c("outcome", "margin_sqrt"),
  home_xg = NA_real_,
  away_xg = NA_real_,
  margin_slope = 1.66,
  blend_w = 0.5
)
```

## Arguments

- home_elo:

  Current home team Elo

- away_elo:

  Current away team Elo

- home_goals:

  Goals scored by home team

- away_goals:

  Goals scored by away team

- k:

  K-factor controlling update magnitude (default 20)

- home_advantage:

  Home advantage in Elo points (default 88)

- update_mode:

  "outcome" (default) = W/D/L surprise x goal-difference multiplier, the
  v6 production form. "margin_sqrt" = update toward a blended goals/xG
  margin, sqrt-dampened (the xG-Elo form).

- home_xg, away_xg:

  Expected goals per team (margin_sqrt mode only). When either is NA the
  target falls back to actual goal difference (~35% of matches have no
  shot data, so xG is unavailable for them).

- margin_slope:

  Expected goal-margin per 400 Elo of gap, used as the reference the
  result is judged against (margin_sqrt mode). Default 1.66.

- blend_w:

  Weight on actual goal diff vs xG diff in the target margin: perf =
  blend_w\*GD + (1-blend_w)\*xGD (margin_sqrt mode). Default 0.5.

## Value

Named list with new_home_elo, new_away_elo
