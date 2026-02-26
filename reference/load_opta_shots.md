# Load Opta Shot Data

Loads aggregated shot statistics per player per match from Opta data.
Includes shot locations, body parts, and big chance metrics.

## Usage

``` r
load_opta_shots(
  league,
  season = NULL,
  columns = NULL,
  source = c("remote", "local")
)
```

## Arguments

- league:

  League code. Accepts panna format (ENG, ESP, GER, ITA, FRA) or Opta
  format (EPL, La_Liga, Bundesliga, Serie_A, Ligue_1).

- season:

  Optional season filter (e.g., "2021-2022"). If NULL, loads all
  seasons.

- columns:

  Optional character vector of columns to select. If NULL, selects all.

- source:

  Data source: "remote" (default) downloads from GitHub releases,
  "local" loads from local files (requires prior
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)).

## Value

Data frame of shot statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all EPL shot data
epl_shots <- load_opta_shots("ENG")

# Load specific season
shots_2122 <- load_opta_shots("ENG", season = "2021-2022")
} # }
```
