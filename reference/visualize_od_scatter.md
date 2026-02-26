# Visualize O/D scatter

Creates scatter plot of offensive vs defensive ratings. Requires
ggplot2.

## Usage

``` r
visualize_od_scatter(ratings, highlight_top = 10)
```

## Arguments

- ratings:

  O/D ratings data frame

- highlight_top:

  Number of top players to label

## Value

ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
od_result <- calculate_od_panna(rapm_data, spm_ratings)
visualize_od_scatter(od_result$ratings, highlight_top = 15)
} # }
```
