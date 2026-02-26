# Create SPM prior vector for RAPM

Creates a prior vector aligned with RAPM player IDs.

## Usage

``` r
create_spm_prior(spm_predictions, player_mapping, default_prior = 0)
```

## Arguments

- spm_predictions:

  Named vector or data frame of SPM predictions

- player_mapping:

  Data frame with player_id and player_name

- default_prior:

  Value for players without SPM prediction

## Value

Named vector of priors (keyed by player_id)
