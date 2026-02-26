# Load Opta Player Stats

Loads player-level match statistics from Opta/TheAnalyst data. Contains
263 columns including Opta-exclusive stats like progressiveCarries,
possWonDef3rd, touchesInOppBox, bigChanceCreated, etc.

## Usage

``` r
load_opta_stats(
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

Data frame of player statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL data (downloads automatically from GitHub)
epl <- load_opta_stats("ENG")

# Load from local files (requires pb_download_opta() first)
epl_local <- load_opta_stats("ENG", season = "2021-2022", source = "local")

# Load specific columns only
epl_basic <- load_opta_stats("ENG", columns = c(
  "match_id", "player_name", "team_name", "minsPlayed",
  "goals", "totalScoringAtt", "progressiveCarries"
))

# Load all Big 5 leagues
big5 <- data.table::rbindlist(lapply(c("ENG", "ESP", "GER", "ITA", "FRA"), load_opta_stats))
} # }
```
