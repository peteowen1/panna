# Convert BT ratings to a match probability

Convert BT ratings to a match probability

## Usage

``` r
bt_match_prob(r_home, r_away, home_adv = 0, nu = 0.5)
```

## Arguments

- r_home:

  Rating of home team

- r_away:

  Rating of away team

- home_adv:

  Home-advantage param (log-odds). Pass 0 for neutral venue.

- nu:

  Draw-frequency param.

## Value

Named vector with `prob_H`, `prob_D`, `prob_A`.
