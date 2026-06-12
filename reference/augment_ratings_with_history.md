# Augment a ratings table with time-decayed historical fallback

For each player_id, finds their MOST recent non-zero rated season. If
that season is older than `current_sey`, applies an exponential decay
`decay_factor ^ years_gap` to the rating and emits a synthetic row under
`current_sey`. This lets unrated-in-current-season players (who moved to
a non-covered league like Saudi PL / MLS / Liga MX) still contribute a
sensible non-zero panna estimate when their lineup is aggregated.

## Usage

``` r
augment_ratings_with_history(
  ratings,
  current_sey,
  decay_factor = 0.85,
  max_years_back = 5L
)
```

## Arguments

- ratings:

  Data.table with at least `player_id`, `season_end_year`, `panna`,
  `offense`, `defense` (and optionally `spm`).

- current_sey:

  The season we want imputed rows for (e.g., 2026).

- decay_factor:

  Per-year decay. Default 0.85 (15% decline per year away from a
  player's last-rated season – accounts for both ageing and uncertainty
  about their current form).

- max_years_back:

  Cap how far back to look. Default 5 years. Beyond that the decay is so
  heavy the imputation is near-zero anyway.

## Value

The input `ratings` with extra synthetic rows for `current_sey` covering
players who weren't already rated there.
