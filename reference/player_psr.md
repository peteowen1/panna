# Get Player Skill Ratings

Returns a ranked PSR leaderboard using pre-computed weekly snapshots.
Snaps to the nearest weekly date at or before `date`. Players under
`min_90s` weighted nineties are left off the ranking (a name lookup via
`player` is exempt).

## Usage

``` r
player_psr(
  date = NULL,
  player = NULL,
  n = 50,
  min_90s = MIN_90S_PSR_LEADERBOARD,
  position = NULL,
  target = c("xg", "goals"),
  source = c("remote", "local")
)
```

## Arguments

- date:

  Date to query as a `Date` or `"YYYY-MM-DD"` string. Defaults to the
  latest available snapshot.

- player:

  Optional player name filter (partial match, case-insensitive). E.g.,
  `"Salah"` matches "Mohamed Salah".

- n:

  Number of top players to show (default 50, NULL for all).

- min_90s:

  Minimum weighted 90s to appear in the leaderboard (default
  [`MIN_90S_PSR_LEADERBOARD`](https://peteowen1.github.io/panna/reference/MIN_90S_PSR_LEADERBOARD.md),
  panna#215). Applied through
  [`psr_leaderboard_eligible`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md)
  before ranking. Not applied when `player` is given, so a single-player
  lookup still shows a short season's rating. Use `0` to rank every row.

- position:

  Filter by position group: `"GK"`, `"DEF"`, `"MID"`, `"FWD"`, or NULL
  for all.

- target:

  One of `"xg"` (default, xG differential) or `"goals"` (goal
  differential). Note: weekly snapshots are xG-based; `"goals"`
  recomputes from skills on-demand (slower).

- source:

  Data source: `"remote"` (default, GitHub Releases) or `"local"`.

## Value

A data.table with columns: `snapshot_date`, `player_name`,
`primary_position`, `psr`, `osr`, `dsr`, `weighted_90s`.

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
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psr_season_coverage()`](https://peteowen1.github.io/panna/reference/psr_season_coverage.md),
[`psr_season_covered()`](https://peteowen1.github.io/panna/reference/psr_season_covered.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Latest PSR leaderboard
player_psr()

# As of a specific date
player_psr(date = "2026-03-18")

# Look up a specific player
player_psr(date = "2026-03-18", player = "Salah")

# Top midfielders
player_psr(position = "MID")
} # }
```
