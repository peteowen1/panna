# Load Opta Lineup Data

Loads lineup information including starting XI, positions, and minutes
played. Useful for assigning players to time periods in RAPM
calculations.

## Usage

``` r
load_opta_lineups(
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

Data frame of lineup data with columns:

- match_id, match_date: Match identifiers

- player_id, player_name: Player info

- team_id, team_name: Team info

- team_position: home or away

- position: Goalkeeper, Defender, Midfielder, etc.

- position_side: Left, Right, Centre

- formation_place: 1-11 for starters

- shirt_number: Jersey number

- is_starter: TRUE if player started

- minutes_played: Total minutes played

- sub_on_minute: Minute substituted on (0 if started)

- sub_off_minute: Minute substituted off (0 if played full match)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL lineups
epl_lineups <- load_opta_lineups("ENG", season = "2024-2025")

# Find starters with most minutes
starters <- epl_lineups |>
  dplyr::filter(is_starter) |>
  dplyr::group_by(player_name, team_name) |>
  dplyr::summarise(total_mins = sum(minutes_played))
} # }
```
