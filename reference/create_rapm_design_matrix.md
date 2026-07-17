# Create RAPM design matrix (new structure)

Creates the design matrix with 2 rows per splint (one per team
perspective):

- Target: xgf90 or gf90 (xG or goals FOR per 90 from each team's
  perspective)

- Covariates: gd, gf, ga, avg_min, home_away

- Player columns: playerX_off (attacking), playerX_def (defending)

- Replacement columns: replacement_off, replacement_def for low-minute
  players

## Usage

``` r
create_rapm_design_matrix(
  splint_data,
  min_minutes = 90,
  target_type = c("xg", "goals", "epv", "wpa"),
  min_duration = 1,
  mode = c("od", "net")
)
```

## Arguments

- splint_data:

  Combined splint data from create_all_splints

- min_minutes:

  Minimum total minutes for player inclusion

- target_type:

  Type of target variable: `"xg"` for non-penalty xG (default),
  `"goals"` for actual goals, `"epv"` for Expected Possession Value,
  `"wpa"` for Win Probability Added. Requires corresponding home/away
  columns on splints (e.g., `epv_home`, `epv_away`). PSV was removed
  from RAPM (FABLE-PRIOR-FIX-PLAN.md D3) – it has its own standalone
  pipeline
  ([`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md)).

- min_duration:

  Minimum splint duration in minutes (default 1.0). Splints shorter than
  this are dropped to avoid per-90 inflation artefacts on stoppage-time
  fragments. Set to 0 to keep all splints. Note: with chain-derived
  splint creation (`create_splint_boundaries_fast`, default
  `min_splint_duration = 5`), the upstream pipeline already enforces a
  5-min minimum so this secondary filter rarely fires.

- mode:

  Design matrix mode. `"od"` (default) is the production layout: 2 rows
  per splint, `_off`/`_def` player columns – byte-identical to before
  this parameter existed. `"net"` builds 1 row per splint and 1 signed
  (`_net`) column per player (+home, -away), for zero-sum targets like
  WPA where an offense/defense split is mechanically unidentified
  (FABLE-PRIOR-FIX-PLAN.md D2).

## Value

List with design matrix components

## See also

Other rapm:
[`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md),
[`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md),
[`extract_player_timing_from_events()`](https://peteowen1.github.io/panna/reference/extract_player_timing_from_events.md),
[`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md),
[`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md),
[`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
