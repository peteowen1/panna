# Extract season end year from match_id

Extracts the season end year as a numeric value from a match_id. Match
IDs have format "2017-2018_20170915_TeamA_TeamB".

## Usage

``` r
extract_season_end_year_from_match_id(match_id)
```

## Arguments

- match_id:

  Character vector of match IDs

## Value

Numeric vector of season end years (e.g., 2018)

## Examples

``` r
if (FALSE) { # \dontrun{
extract_season_end_year_from_match_id("2017-2018_20170915_Bournemouth_Brighton")
# Returns 2018
} # }
```
