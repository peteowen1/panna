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
  target_type = c("xg", "goals", "epv", "wpa", "psv")
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
  `"wpa"` for Win Probability Added, `"psv"` for Player Stat Value.
  Requires corresponding home/away columns on splints (e.g., `epv_home`,
  `epv_away`).

## Value

List with design matrix components
