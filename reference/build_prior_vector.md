# Build prior vector for RAPM from SPM predictions

Creates a named prior vector aligned with player IDs from SPM rating
predictions. This is a vectorized helper used by xRAPM and seasonal
ratings to build priors from SPM predictions without manual for-loops.

## Usage

``` r
build_prior_vector(spm_data, spm_col, player_mapping, default = 0)
```

## Arguments

- spm_data:

  Data frame with player_name and the SPM column to use

- spm_col:

  Name of the column containing SPM predictions

- player_mapping:

  Data frame with player_id and player_name from RAPM

- default:

  Value for players without SPM prediction (default 0)

## Value

Named vector of priors keyed by player_id

## Examples

``` r
if (FALSE) { # \dontrun{
offense_prior <- build_prior_vector(
  spm_data = offense_spm_ratings,
  spm_col = "offense_spm",
  player_mapping = rapm_data$player_mapping
)
} # }
```
