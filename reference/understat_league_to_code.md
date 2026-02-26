# Map Understat league name to our code

Converts Understat's league name (e.g., "EPL", "La liga") to our
standardized code (e.g., "ENG", "ESP").

## Usage

``` r
understat_league_to_code(understat_league)
```

## Arguments

- understat_league:

  Character, the league name from Understat metadata

## Value

Character, our league code, or NA if not found

## Examples

``` r
if (FALSE) { # \dontrun{
understat_league_to_code("La liga")  # "ESP"
understat_league_to_code("Serie A")  # "ITA"
} # }
```
