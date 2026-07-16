# Classify competitions as international vs domestic

Classify competitions as international vs domestic

## Usage

``` r
match_is_international(league)
```

## Arguments

- league:

  Character vector of competition codes.

## Value

Logical vector – `TRUE` for international (national-team) competitions,
`FALSE` for domestic club competitions.

## See also

Other world cup simulation:
[`bt_match_prob()`](https://peteowen1.github.io/panna/reference/bt_match_prob.md),
[`build_knockout_lookup()`](https://peteowen1.github.io/panna/reference/build_knockout_lookup.md),
[`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md),
[`fit_bt_ratings()`](https://peteowen1.github.io/panna/reference/fit_bt_ratings.md),
[`mirror_match_rows()`](https://peteowen1.github.io/panna/reference/mirror_match_rows.md),
[`run_wc2026_reference_checks()`](https://peteowen1.github.io/panna/reference/run_wc2026_reference_checks.md),
[`simulate_world_cup()`](https://peteowen1.github.io/panna/reference/simulate_world_cup.md)
