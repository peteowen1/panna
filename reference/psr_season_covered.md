# Is a player-season in a season with enough Opta coverage? (panna#221)

Returns a logical vector, one per row – a flag, not a filter. Opta's
first seasons are sparse (2014: 822 matches with player stats against
~6,000 in a normal season), so one well-covered club tops any
cross-season table. Pair with
[`psr_leaderboard_eligible`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
which accepts this gate through its `season_coverage` argument so
consumers need not know about it.

## Usage

``` r
psr_season_covered(
  psr_dt,
  season_coverage,
  min_matches = MIN_MATCHES_PSR_SEASON
)
```

## Arguments

- psr_dt:

  Table containing `season_end_year`.

- season_coverage:

  Table from
  [`psr_season_coverage`](https://peteowen1.github.io/panna/reference/psr_season_coverage.md).

- min_matches:

  Minimum matches; defaults to
  [`MIN_MATCHES_PSR_SEASON`](https://peteowen1.github.io/panna/reference/MIN_MATCHES_PSR_SEASON.md).

## Value

Logical vector.

## Details

The latest season in `season_coverage` always passes: it can be in
progress and so legitimately short, and the ramp-up seasons this gate
exists for are never the latest. A season absent from the table, or an
`NA` season, is NOT covered – unknown coverage cannot clear a coverage
bar. An empty `season_coverage`, or one ending before the latest season
in `psr_dt`, is an error: either means it was not built from the full
match-stats table.

## See also

Other psr:
[`PSV_RELIABILITY_GD_SCALE`](https://peteowen1.github.io/panna/reference/PSV_RELIABILITY_GD_SCALE.md),
[`apply_psr_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_calibration.md),
[`apply_psr_season_calibration()`](https://peteowen1.github.io/panna/reference/apply_psr_season_calibration.md),
[`apply_psv_calibration()`](https://peteowen1.github.io/panna/reference/apply_psv_calibration.md),
[`apply_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/apply_psv_opponent_adjustment.md),
[`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md),
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md),
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md),
[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md),
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psr_calibration()`](https://peteowen1.github.io/panna/reference/load_psr_calibration.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psr_season_coverage()`](https://peteowen1.github.io/panna/reference/psr_season_coverage.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
