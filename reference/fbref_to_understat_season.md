# Convert FBref season to Understat season

Converts FBref's YYYY-YYYY format to Understat's YYYY format. Takes the
start year of the season.

## Usage

``` r
fbref_to_understat_season(fbref_season)
```

## Arguments

- fbref_season:

  Character, season in FBref format (e.g., "2023-2024")

## Value

Character, season in Understat format (e.g., "2023")

## Examples

``` r
if (FALSE) { # \dontrun{
fbref_to_understat_season("2024")       # "2024" (already correct)
} # }
```
