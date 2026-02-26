# Rank players by panna rating

Returns ranked list of players.

## Usage

``` r
rank_players_panna(ratings, top_n = NULL, position = NULL)
```

## Arguments

- ratings:

  Data frame of panna ratings

- top_n:

  Number of top players to show

- position:

  Optional position filter

## Value

Data frame of ranked players

## Examples

``` r
if (FALSE) { # \dontrun{
ratings <- get_panna_ratings(panna_model)
top_20 <- rank_players_panna(ratings, top_n = 20)
forwards <- rank_players_panna(ratings, top_n = 10, position = "FW")
} # }
```
