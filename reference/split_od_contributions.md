# Split O/D contributions from existing ratings

Approximates O/D split from overall panna and feature data.

## Usage

``` r
split_od_contributions(panna_ratings, player_features)
```

## Arguments

- panna_ratings:

  Overall panna ratings

- player_features:

  Player features with offensive/defensive stats

## Value

Data frame with estimated O-panna and D-panna

## Examples

``` r
if (FALSE) { # \dontrun{
features <- create_player_feature_matrix(processed_data)
od_ratings <- split_od_contributions(panna_ratings, features)
head(od_ratings[, c("player_name", "panna", "o_panna", "d_panna")])
} # }
```
