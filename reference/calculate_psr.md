# Calculate Player Skill Ratings (PSR)

Computes PSR for each player by applying pre-trained glmnet coefficients
to individual player skill values. PSR represents each player's
predicted contribution to xG/goal differential based on their skill
profile.

## Usage

``` r
calculate_psr(skills, coef_df, center = TRUE)
```

## Arguments

- skills:

  A data.table/data.frame with player skill estimates, containing
  identity columns (`player_id`, `player_name`) and numeric skill
  columns matching the `stat_name` values in `coef_df`.

- coef_df:

  A data.frame with columns `stat_name` and `beta`. If an `sd` column is
  present, each skill is divided by its SD before multiplying by beta
  (i.e. the coefficients are on the standardized scale).

- center:

  Logical. If TRUE (default), subtract the league mean so PSR =
  contribution above average player.

## Value

A data.table with identity columns plus `psr_raw` and `psr`.
