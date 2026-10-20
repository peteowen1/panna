# Minutes to scale a PSV league offset by

Returns, per row, the number of minutes `calculate_psv` actually scaled
that row's PSV by (its `psv_minutes` column, carried through
[`build_player_game_ratings`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md)),
divided by 90 and floored at 0. Rows with no `psv_minutes` (no PSV row
at all, so `psv` is NA and the offset cannot reach a value) fall back to
`fallback_minutes`, which keeps the published `psv_league_offset` column
unchanged for them. When the `psv_minutes` column is absent altogether,
everything falls back, as before panna#164.

## Usage

``` r
.psv_offset_mins_scale(psv_minutes, fallback_minutes)
```

## Arguments

- psv_minutes:

  Numeric vector (or `NULL`) of minutes PSV used.

- fallback_minutes:

  Numeric vector of lineup-derived minutes (`total_minutes`), same
  length.

## Value

Numeric vector of `minutes / 90`, never NA or negative.
