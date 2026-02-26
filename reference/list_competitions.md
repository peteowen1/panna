# List competitions by type

Returns competition codes filtered by type.

## Usage

``` r
list_competitions(type = "all")
```

## Arguments

- type:

  Character, one of "league", "cup", "european", "national_team", or
  "all"

## Value

Character vector of competition codes

## Examples

``` r
list_competitions("league")
#> [1] "ENG" "ESP" "GER" "ITA" "FRA"
list_competitions("national_team")
#> [1] "WC"             "EURO"           "COPA_AMERICA"   "AFCON"         
#> [5] "NATIONS_LEAGUE" "GOLD_CUP"       "ASIAN_CUP"     
```
