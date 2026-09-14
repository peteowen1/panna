# Compute PSV from bundled coefficient files

Convenience wrapper that loads pre-trained coefficients and calls
[`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md).

## Usage

``` r
compute_player_psv(
  player_match_stats,
  min_adjust = TRUE,
  center = TRUE,
  target = c("xg", "goals", "blend"),
  scale_to_minutes = FALSE,
  exclude_efficiency = TRUE,
  position_means = NULL,
  reliability = NULL,
  center_weights = c("none", "minutes"),
  is_gk = NULL,
  .pos_grp_override = NULL,
  role_override = NULL
)
```

## Arguments

- player_match_stats:

  Per-game player stats (one row per player per match).

- min_adjust:

  Logical. Minutes-adjust raw counts. Default `TRUE`.

- center:

  Logical. Center within each round. Default `TRUE`.

- target:

  One of `"xg"` (default, xG differential), `"goals"` (goal
  differential), or `"blend"` (alpha\*xG + (1-alpha)\*goals — the
  displayed value model; falls back to `"xg"` until the blend is
  trained).

- scale_to_minutes:

  Logical. Multiply the per-90 PSV by `minutes_played / 90` so values
  are additive over games (like EPV). Default `FALSE`. See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).

- exclude_efficiency:

  Logical. Exclude efficiency/ratio stats. Default `TRUE`. Set `FALSE`
  to score with the full trained coefficient vector (the form used for
  the displayed blog PSV). See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).

- position_means:

  Optional pre-computed position-mean lookup table used to center skill
  columns before scoring (see
  [`compute_player_psr`](https://peteowen1.github.io/panna/reference/compute_player_psr.md)).
  If `NULL`, no cross-position centering is applied.

- reliability:

  Optional per-match reliability lookup table (see
  [`load_psv_match_reliability`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md)),
  columns `model`, `stat_name`, `lambda`. Filtered to the
  `"outfield"`/`"gk"` subset for each scoring branch and passed to
  [`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md)/[`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md).
  `NULL` (default) applies no shrinkage – unchanged behaviour.

- center_weights:

  One of `"none"` (default) or `"minutes"`; passed through to
  [`calculate_psv_components`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md)/
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
  for BOTH the outfield and GK branches (each sub-population is centered
  – weighted or not – separately, same as today). See
  [`calculate_psv`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
  for the zero-sum property.

- is_gk:

  Optional logical vector, one per row of `player_match_stats`, marking
  rows to route to the GK sub-model. `NULL` (default) computes it fresh
  via
  [`.detect_gk_rows`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md)
  on `player_match_stats` – unchanged behaviour for single-call use.
  Pass this explicitly when scoring REPEATED SLICES of a larger
  population (looping per league, season, or date):
  [`.detect_gk_rows()`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md)'s
  majority vote is NOT scope-invariant, so the same player can get a
  different (even internally inconsistent) classification depending on
  how much of their history the current slice contains – confirmed
  2026-09-13 (panna#249 follow-up) for rare emergency keepers with few
  total career rows. Compute
  [`.detect_gk_rows()`](https://peteowen1.github.io/panna/reference/dot-detect_gk_rows.md)
  ONCE on the full population and subset it alongside each slice
  instead.

- .pos_grp_override:

  Optional character vector, one per row of `player_match_stats`,
  supplying the `pos_grp` calibration bucket directly instead of
  resolving it internally via
  [`.psv_pos_grp`](https://peteowen1.github.io/panna/reference/dot-psv_pos_grp.md).
  Same scope-narrowness motivation as `is_gk`:
  [`resolve_position_group`](https://peteowen1.github.io/panna/reference/resolve_position_group.md)'s
  season- and career-modal fallback tiers can only see the rows handed
  to THIS call, so a caller scoring one league-season at a time (10b)
  narrows the "career" tier to that single league's own history – a
  player whose `position` is blank across that whole league-season
  resolves to `NA` (scored uncalibrated, factor 1) even when their wider
  career makes the bucket obvious. Resolve once on the full population
  and pass it here. Named with a dot prefix deliberately: a parameter
  called `pos_grp` would be shadowed by the column of the same name
  inside `dt[...]`.

## Value

A data.table with `psv`, `osv`, `dsv` columns.

## Details

The underlying coefficient CSVs' `sd` column is the TEAM-SUM training
sd, not a player-population sd – this is deliberate, not a bug. See
[`calculate_psr`](https://peteowen1.github.io/panna/reference/calculate_psr.md)'s
`coef_df` docs for the full derivation (panna#167).

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
[`default_stat_rating_params()`](https://peteowen1.github.io/panna/reference/default_stat_rating_params.md),
[`fit_psv_opponent_adjustment()`](https://peteowen1.github.io/panna/reference/fit_psv_opponent_adjustment.md),
[`load_opta_psr_weekly()`](https://peteowen1.github.io/panna/reference/load_opta_psr_weekly.md),
[`load_psr_calibration()`](https://peteowen1.github.io/panna/reference/load_psr_calibration.md),
[`load_psv_match_reliability()`](https://peteowen1.github.io/panna/reference/load_psv_match_reliability.md),
[`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md),
[`psr_leaderboard_eligible()`](https://peteowen1.github.io/panna/reference/psr_leaderboard_eligible.md),
[`psv_opponent`](https://peteowen1.github.io/panna/reference/psv_opponent.md),
[`soccer_position_map()`](https://peteowen1.github.io/panna/reference/soccer_position_map.md),
[`soccer_stat_rating_definitions()`](https://peteowen1.github.io/panna/reference/soccer_stat_rating_definitions.md),
[`stat_rating_names()`](https://peteowen1.github.io/panna/reference/stat_rating_names.md)
