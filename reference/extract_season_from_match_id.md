# Extract season from match_id

Extracts the season string from a match_id. Match IDs have format
"2017-2018_20170915_TeamA_TeamB".

## Usage

``` r
extract_season_from_match_id(match_id)
```

## Arguments

- match_id:

  Character vector of match IDs

## Value

Character vector of season strings (e.g., "2017-2018")

## Examples

``` r
if (FALSE) { # \dontrun{
extract_season_from_match_id("2017-2018_20170915_Bournemouth_Brighton")
# Returns "2017-2018"
} # }
```
