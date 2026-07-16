# Extract Period End Times from Raw Opta Match Events

Opta marks the actual final whistle of each period with `type_id == 30`
events that carry second-level timing. This function extracts the
maximum (minute + second/60) for each (match_id, period_id) so splint
creation can use the real period boundaries instead of guessing with a
+0.5 buffer off the last gameplay event.

## Usage

``` r
extract_period_end_times(match_events)
```

## Arguments

- match_events:

  Raw Opta match-events data frame with columns `match_id`, `type_id`,
  `period_id`, `minute`, and (optionally) `second`.

## Value

Data frame with columns `match_id`, `first_half_end_time`,
`match_end_time`. Matches without markers are omitted.

## See also

Other rapm:
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
