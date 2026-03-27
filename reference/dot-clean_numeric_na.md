# Replace NA/Inf with 0 in numeric columns and log summary

Replace NA/Inf with 0 in numeric columns and log summary

## Usage

``` r
.clean_numeric_na(df, check_inf = TRUE)
```

## Arguments

- df:

  data.frame with numeric columns

- check_inf:

  Whether to also replace Inf values (default TRUE)

## Value

df with NAs/Inf replaced
