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

## See also

Other world cup simulation:
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md),
[`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md),
[`fit_bt_ratings()`](https://peteowen1.github.io/panna/reference/fit_bt_ratings.md),
[`match_is_international()`](https://peteowen1.github.io/panna/reference/match_is_international.md),
[`mirror_match_rows()`](https://peteowen1.github.io/panna/reference/mirror_match_rows.md),
[`run_wc2026_reference_checks()`](https://peteowen1.github.io/panna/reference/run_wc2026_reference_checks.md),
[`simulate_world_cup()`](https://peteowen1.github.io/panna/reference/simulate_world_cup.md)
