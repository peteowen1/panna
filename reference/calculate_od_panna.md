# Calculate O-panna and D-panna

Separates panna rating into offensive and defensive components.

## Usage

``` r
calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)
```

## Arguments

- rapm_data:

  RAPM data prepared with separate_od = TRUE

- spm_ratings:

  SPM ratings (or separate O-SPM/D-SPM)

- lambda_prior:

  Regularization strength

## Value

List with separated offensive and defensive ratings

## Examples

``` r
if (FALSE) { # \dontrun{
rapm_data <- create_rapm_design_matrix(splints, separate_od = TRUE)
od_result <- calculate_od_panna(rapm_data, spm_ratings, lambda_prior = 1)
head(od_result$ratings[, c("player_name", "o_panna", "d_panna", "panna")])
} # }
```
