# Convert Understat season to FBref season

Converts Understat's YYYY format to FBref's YYYY-YYYY format.

## Usage

``` r
understat_to_fbref_season(understat_season)
```

## Arguments

- understat_season:

  Character or numeric, season in Understat format (e.g., "2023" or
  2023)

## Value

Character, season in FBref format (e.g., "2023-2024")

## Examples

``` r
if (FALSE) { # \dontrun{
understat_to_fbref_season("2024")  # "2024-2025"
} # }
```
