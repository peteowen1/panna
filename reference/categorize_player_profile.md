# Categorize player profile

Classifies players based on their O/D rating balance.

## Usage

``` r
categorize_player_profile(o_rating, d_rating)
```

## Arguments

- o_rating:

  Offensive rating

- d_rating:

  Defensive rating

## Value

Character vector of player types

## Examples

``` r
if (FALSE) { # \dontrun{
categorize_player_profile(0.8, 0.2)
categorize_player_profile(c(0.6, 0.3, 0.5), c(0.4, 0.7, 0.5))
} # }
```
