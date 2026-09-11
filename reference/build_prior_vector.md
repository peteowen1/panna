# Build prior vector for RAPM from SPM predictions

Creates a named prior vector aligned with player IDs from SPM rating
predictions. This is a vectorized helper used by xRAPM and seasonal
ratings to build priors from SPM predictions without manual for-loops.

## Usage

``` r
build_prior_vector(
  spm_data,
  spm_col,
  player_mapping,
  default = 0,
  negate = FALSE
)
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

- negate:

  Negate the matched values before returning (default FALSE). Sign
  convention (Pete, 2026-09-04):
  [`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)'s
  internal fitting math (`y_adjusted <- y - X %*% prior_vec`,
  `beta_final <- gamma + prior_vec`) needs `defense_prior` on the RAW
  internal scale (bad = positive) – unaffected by the extraction-time
  sign flip in
  [`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md)/[`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md).
  But `defense_spm` (the SPM column this is normally called with for a
  defense prior) is trained against the PUBLISHED `defense` column
  (positive = good) via `05_spm.R`'s
  `defense_train <- spm_train_data %>% mutate(rapm = defense)`, so it
  comes out on the FLIPPED scale. Every defense-prior call site must
  pass `negate = TRUE` to convert back to the raw scale
  [`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
  expects. Safe with the default=0 used by every caller (0 negates to
  0); if a caller ever passes a nonzero `default`, revisit whether it
  also needs negating.

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
defense_prior <- build_prior_vector(
  spm_data = defense_spm_ratings,
  spm_col = "defense_spm",
  player_mapping = rapm_data$player_mapping,
  negate = TRUE
)
} # }
```
