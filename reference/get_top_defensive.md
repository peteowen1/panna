# Get top defensive players

Returns players with highest defensive contribution.

## Usage

``` r
get_top_defensive(ratings, n = 10)
```

## Arguments

- ratings:

  O/D ratings data frame

- n:

  Number of players

## Value

Data frame of top defensive players

## Examples

``` r
if (FALSE) { # \dontrun{
od_result <- calculate_od_panna(rapm_data, spm_ratings)
top_defenders <- get_top_defensive(od_result$ratings, n = 20)
} # }
```
