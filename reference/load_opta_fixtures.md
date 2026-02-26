# Load Opta Fixture Data

Loads fixture data including both played and upcoming matches from Opta.
Fixtures are saved by the scraper alongside match data and include match
status (Played, Fixture, Postponed).

## Usage

``` r
load_opta_fixtures(
  league,
  season = NULL,
  columns = NULL,
  status = NULL,
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

- status:

  Optional match status filter (e.g., "Fixture", "Played", "Postponed").
  If NULL (default), returns all statuses.

- source:

  Data source: "remote" (default) downloads from GitHub releases,
  "local" loads from local files (requires prior
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)).

## Value

Data frame of fixtures with columns:

- match_id: Match identifier

- match_date: Scheduled/played date

- match_status: Fixture, Played, or Postponed

- home_team, away_team: Team names

- home_team_id, away_team_id: Team IDs

- competition: Opta league code

- season: Season identifier

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all EPL fixtures
epl_fix <- load_opta_fixtures("ENG", season = "2024-2025")

# Load only upcoming matches
upcoming <- load_opta_fixtures("ENG", season = "2024-2025", status = "Fixture")
} # }
```
