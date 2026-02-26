# Understat competition metadata

A data frame containing Understat competition slugs and metadata for all
supported competitions. Understat covers Big 5 European leagues plus
Russia.

## Usage

``` r
understat_competitions
```

## Format

A data frame with columns:

- code:

  Short code used in file paths (e.g., "ENG", "RUS")

- name:

  Full competition name

- understat_slug:

  URL slug for Understat URLs (e.g., "EPL", "La_liga")

- season_format:

  How seasons are formatted: "YYYY" (Understat uses single year)

## Examples

``` r
understat_competitions
#>   code                   name understat_slug season_format
#> 1  ENG         Premier League            EPL          YYYY
#> 2  ESP                La Liga        La_liga          YYYY
#> 3  GER             Bundesliga     Bundesliga          YYYY
#> 4  ITA                Serie A        Serie_A          YYYY
#> 5  FRA                Ligue 1        Ligue_1          YYYY
#> 6  RUS Russian Premier League           RFPL          YYYY
understat_competitions[understat_competitions$code == "ENG", ]
#>   code           name understat_slug season_format
#> 1  ENG Premier League            EPL          YYYY
```
