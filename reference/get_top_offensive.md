# Get top offensive players

Returns players with highest offensive contribution.

## Usage

``` r
get_top_offensive(ratings, n = 10)
```

## Arguments

- ratings:

  O/D ratings data frame

- n:

  Number of players

## Value

Data frame of top offensive players

## Examples

``` r
if (FALSE) { # \dontrun{
od_result <- calculate_od_panna(rapm_data, spm_ratings)
top_attackers <- get_top_offensive(od_result$ratings, n = 20)
} # }
```
