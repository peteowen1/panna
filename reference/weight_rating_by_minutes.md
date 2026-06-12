# Build a minutes-weighted team rating

Build a minutes-weighted team rating

## Usage

``` r
weight_rating_by_minutes(team_em, ratings, rating_col = "panna")
```

## Arguments

- team_em:

  Output of
  [`build_team_expected_minutes()`](https://peteowen1.github.io/panna/reference/build_team_expected_minutes.md).

- ratings:

  Data frame with player_id + a numeric rating column.

- rating_col:

  Name of the rating column. Default `"panna"`.

## Value

Single numeric – `sum(rating * expected_minutes_norm) / 990`. Equivalent
to "what's the average panna rating of who'll be on the pitch."
