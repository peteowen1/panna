# Build RAPM row data from valid splints

Creates 2 rows per splint (home attacking, away attacking) with game
state covariates and target variable.

## Usage

``` r
.build_rapm_row_data(valid_splints, target_type)
```

## Arguments

- valid_splints:

  Data frame of splints with duration \> 0

- target_type:

  "xg" or "goals"

## Value

List with row_data data.frame and target_per90_name string
